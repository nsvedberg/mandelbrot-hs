{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Complex
import Data.Functor
import Data.Time.Clock
import Data.Time.Format
import Data.Word
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import SDL hiding (get)
import qualified Data.Vector.Storable.Mutable as V
import qualified SDL.Internal.Types
import qualified SDL.Raw as Raw

-- The type of fractal that is currently being shown to the user.
data FractalType = Mandelbrot | JuliaSet
  deriving (Eq, Ord)

-- The area of the fractal that is visible to the user.
data Viewport = Viewport
  { viewportCenter :: V2 Float
  , viewportWidth :: Float
  , viewportHeight :: Float
  }

-- Get the X and Y limits of a viewport.
viewportLimits :: Viewport -> V4 Float
viewportLimits vp = V4 left bot right top
  where left = x - width/2
        right = x + width/2
        bot = y - height/2
        top = y + height/2
        (V2 x y) = viewportCenter vp
        width = viewportWidth vp
        height = viewportHeight vp

-- Scale a viewport by a scale factor.
scaleViewport :: Viewport -> Float -> Viewport
scaleViewport vp s = vp { viewportWidth = w*s, viewportHeight = h*s}
  where w = viewportWidth vp
        h = viewportHeight vp

-- An aggregation of state for our application.
data AppState = AppState
  { appFractal :: FractalType
  , appShouldUpdate :: Bool
  , appShouldQuit :: Bool
  , appShouldShowInfo :: Bool
  , appWinWidth :: Int
  , appWinHeight :: Int
  , appMaxIter :: Int
  , appViewport :: Viewport
  , appInfoTexture :: Maybe Texture
  , appFractalTexture :: Maybe Texture
  }

-- The default state of our application.
defaultAppState :: AppState
defaultAppState =
  AppState
    { appFractal = Mandelbrot
    , appShouldUpdate = True
    , appShouldQuit = False
    , appShouldShowInfo = False
    , appWinWidth = 1200
    , appWinHeight = 800
    , appMaxIter = 100
    , appViewport = Viewport { viewportCenter = V2 0 0
                             , viewportWidth = 3.36
                             , viewportHeight = 2.24 }
    , appInfoTexture = Nothing
    , appFractalTexture = Nothing
    }

-- This is a monad "transformer" which combines the functionallity of an IO
-- monad and a State monad into one type.
--
-- Functions you may find useful:
--
-- get :: Monad m => StateT s m s
--    Fetch the current value of the state within the monad.
--
-- put :: Monad m => s -> StateT s m ()
--    `put s` sets the state within the monad to `s`.
--
-- modify :: Monad m => (s -> s) -> StateT s m ()
--    `modify f` is an action that updates the state to the result of applying
--    `f` to the current state.
--
-- liftIO :: IO a -> m a
--    Lift a computation from the IO monad. This allows us to run IO
--    computations in any monadic stack, so long as it supports these kinds of
--    operations (i.e. IO is the base monad for the stack).
--
-- https://en.wikibooks.org/wiki/Haskell/Monad_transformers
-- https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-State-Lazy.html
-- https://hackage.haskell.org/package/base-4.19.1.0/docs/Control-Monad-IO-Class.html#v:liftIO
type AppMonad = StateT AppState IO

-- Entry point.
main :: IO ()
main = do

  -- Initialize SDL.
  initializeAll

  -- Create a new SDL window with default settings.
  let winWidth = fromIntegral $ appWinWidth defaultAppState
      winHeight = fromIntegral $ appWinHeight defaultAppState

  -- TODO: Allow the window to be resizable.
  --
  -- In theory, the window being resizable should be fine, because the width
  -- and the height of the window are being kept track of by our application.
  -- However, on my machine, the program crashes immediately when the window is
  -- resized.
  --
  -- This is almost certainly related to the unsafe portion of the render
  -- function. It probably has something to do with the pitch, and being
  -- aligned to a certain byte multiple. However, I don't have time to debug
  -- this now.
  window <- createWindow
    "Mandlebrot Set"
    defaultWindow
      { windowInitialSize = V2 winWidth winHeight
   -- , windowResizable = True
      }

  -- Create a renderer from that window.
  renderer <- createRenderer window (-1) defaultRenderer

  -- Create a texture for the info screen.
  infoSurface <- loadBMP "help-info.bmp"
  infoTexture <- createTextureFromSurface renderer infoSurface

  -- Create a texture to draw the fractal to.
  fractalTexture <- createTexture
    renderer
    RGB888
    TextureAccessStreaming (V2 winWidth winHeight)

  -- Handle events from the user.
  _ <- runStateT
    (run window renderer)
    defaultAppState { appInfoTexture = Just infoTexture
                    , appFractalTexture = Just fractalTexture }

  -- No more events, clean up our window.
  destroyWindow window

-- The main loop for the app.
run :: Window -> Renderer -> AppMonad ()
run window renderer = do

  -- Wait for an event to happen.
  (Event _timestamp payload) <- waitEvent

  case payload of
    KeyboardEvent (KeyboardEventData { keyboardEventKeyMotion = Pressed
                                     , keyboardEventKeysym = sym }) ->
      case (keysymKeycode sym) of
        -- Screenshot when S is pressed.
        KeycodeS -> saveRender window renderer
        -- Change modes with number keys.
        Keycode1 -> do modify (\s -> s {appFractal = Mandelbrot})
                       modify (\s -> s {appShouldUpdate = True})
        Keycode2 -> do modify (\s -> s {appFractal = JuliaSet})
                       modify (\s -> s {appShouldUpdate = True})
        -- Zoom in and out with the plus and minus buttons.
        code | code==KeycodeEquals || code==KeycodeMinus -> do
          let scale = case code of
                KeycodeEquals -> 3/4
                KeycodeMinus -> 4/3

          vp <- gets appViewport

          modify (\s -> s { appViewport = scaleViewport vp scale
                          , appShouldUpdate = True
                          })
        -- Show the help screen when ? is pressed.
        KeycodeSlash -> modify (\s -> s { appShouldShowInfo = True })
        -- Quit when the user presses Q.
        KeycodeQ -> modify (\s -> s { appShouldQuit = True})
        -- Ignore other keycodes.
        _ -> return ()

    KeyboardEvent (KeyboardEventData { keyboardEventKeyMotion = Released
                                     , keyboardEventKeysym = sym}) ->
      case (keysymKeycode sym) of
        -- Quit showing the help screen when ? is released.
        KeycodeSlash -> modify (\s -> s { appShouldShowInfo = False })
        -- Ignore other keycodes.
        _ -> return ()

    -- If the user clicks a point on the screen, move the viewport to that point.
    MouseButtonEvent (MouseButtonEventData { mouseButtonEventButton = ButtonLeft
                                           , mouseButtonEventMotion = Pressed
                                           , mouseButtonEventPos = pos
                                           }) -> do
      winWidth <- gets appWinWidth
      winHeight <- gets appWinHeight

      vp@(Viewport { viewportCenter = V2 centerX centerY
                   , viewportWidth = vpWidth
                   , viewportHeight = vpHeight
                   }) <- gets appViewport

      -- The amount that the center is moved should be relative to the size of
      -- the viewport.
      let (P (V2 clickX clickY)) = pos
          relativeX = fromIntegral clickX / fromIntegral winWidth - 0.5
          relativeY = fromIntegral clickY / fromIntegral winHeight - 0.5
          offsetX = relativeX*vpWidth
          offsetY = relativeY*vpHeight
          newCenter = V2 (centerX + offsetX) (centerY + offsetY)

      modify (\s -> s { appViewport = vp { viewportCenter = newCenter }
                      , appShouldUpdate = True
                      })

    WindowResizedEvent (WindowResizedEventData
      { windowResizedEventSize = V2 width height }) ->
        modify (\s -> s { appWinWidth = fromIntegral width
                        , appWinHeight = fromIntegral height
                        , appShouldUpdate = True})

    -- Quit when the user closes the window.
    WindowClosedEvent _ -> modify (\s -> s { appShouldQuit = True})

    _ -> return ()

  -- Render the current fractal that is selected.
  renderAll renderer

  -- If nothing happened, keep running.
  shouldQuit <- gets appShouldQuit
  unless shouldQuit (run window renderer)

-- Write what's on screen to a file in the `screenshots/` directory.
saveRender :: Window -> Renderer -> AppMonad ()
saveRender window renderer = do

  -- Generate the filename from the current date and time.
  currentTime <- liftIO getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" currentTime
      filename = "screenshots/" ++ formattedTime ++ ".bmp"
  filenameC <- liftIO (newCString filename)

  -- Since the functions we need form SDL, namely RenderReadPixels[1] and
  -- SaveBMP[2], are not part of the "regular" bindings in our Haskell SDL2
  -- library, we need to use the "raw" functions.
  --
  -- This involves using the Haskell <-> C FFI interface.
  --
  -- [1]: https://wiki.libsdl.org/SDL2/SDL_RenderReadPixels
  -- [2]: https://wiki.libsdl.org/SDL2/SDL_SaveBMP

  -- Expose the internal "raw" types for our window and renderer.
  let (SDL.Internal.Types.Window rawWindow) = window
  let (SDL.Internal.Types.Renderer rawRenderer) = renderer

  -- Get the raw pixel format of the window.
  pixFormat <- Raw.getWindowPixelFormat rawWindow

  -- Get the dimensions of the window.
  winWidth <- gets (fromIntegral . appWinWidth)
  winHeight <- gets (fromIntegral . appWinHeight)

  -- Create an RGB surface for our screenshot.
  surface <-
    Raw.createRGBSurface
      0          -- According to the documentation: "the flags are unused and
                 -- should be set to 0".
      winWidth   -- The width of the surface.
      winHeight  -- The height of the surface.
      32         -- The depth of the surface.
      0x00ff0000 -- The red mask of the surface.
      0x0000ff00 -- The green mask of the surface.
      0x000000ff -- The blue mask of the surface.
      0xff000000 -- The alpha mask of the surface.

  pixels <- Raw.surfacePixels <$> liftIO (peek surface)

  -- Copy the pixels out of the window and into our screenshot surface.
  _ <-
    Raw.renderReadPixels
      rawRenderer  -- The rendering context.
      nullPtr      -- The area to read from (NULL for the entire rendering area).
      pixFormat    -- The pixel format of the pixel data.
      pixels       -- The pixels to copy.
      (winWidth*4) -- The pitch (i.e. the amount of bytes in a row of pixels).

  -- Save the surface to a file.
  _ <- Raw.saveBMP surface filenameC

  liftIO $ putStrLn $ "Saved screenshot to " ++ filename

-- Render the entire scene.
renderAll :: Renderer -> AppMonad()
renderAll renderer = do

  -- Add the fractal texture to the renderer.
  fractalTexture <- gets appFractalTexture
  shouldUpdate <- gets appShouldUpdate
  case fractalTexture of
    Just tex -> do
      -- If the fractal texture is stale, update it.
      when shouldUpdate $ updateFractalTexture renderer tex
      copy renderer tex Nothing Nothing
    Nothing -> return ()

  -- If the user is pressing the ? key, show the info page.
  shouldShowInfo <- gets appShouldShowInfo
  infoTexture <- gets appInfoTexture

  case infoTexture of
    Just tex -> when shouldShowInfo $ copy renderer tex Nothing Nothing
    Nothing -> return ()

  -- Display everything which was rendered.
  present renderer

-- Render the fractal chosen by the user.
updateFractalTexture :: Renderer -> Texture -> AppMonad ()
updateFractalTexture renderer texture = do

  -- Get some parameters.
  maxIter <- gets appMaxIter
  limits <- gets (viewportLimits . appViewport)
  winWidth <- gets (fromIntegral . appWinWidth)
  winHeight <- gets (fromIntegral . appWinHeight)

  fractalType <- gets appFractal

  let func = case fractalType of
        Mandelbrot -> mandelbrot
        JuliaSet -> julia

  -- Get access to the raw bytes in the texture.
  (ptr, pitch) <- lockTexture texture Nothing
  fptr <- liftIO $ newForeignPtr_ (castPtr ptr :: Ptr Word8)
  let vec = V.unsafeFromForeignPtr0 fptr (fromIntegral $ winWidth * pitch)

      -- For on-screen coordinates x and y scaled to the provided limits, get the
      -- value of the fractal.
      value x y = heatMapColor $ fromIntegral m / fromIntegral maxIter
        where
          V4 xMin yMin xMax yMax = limits
          fx = fromIntegral x
          fy = fromIntegral y
          -- The real part of the complex number.
          a = ((fx * (xMax - xMin)) / ((fromIntegral winWidth - 1) - xMin)) + xMin
          -- The imaginary part of the complex number.
          b = ((fy * (yMax - yMin)) / ((fromIntegral winHeight - 1) - yMin)) + yMin
          -- The value of the fractal.
          m = func maxIter (a :+ b)

      -- Write an SDL vector3 to the given address.
      writeVal addr (V3 w1 w2 w3) =
        V.write vec addr w3
          >> V.write vec (addr + 1) w2
          >> V.write vec (addr + 2) w1

  -- For each x and y location, generate the value and write it to the texture.
  sequence_ $
    [ writeVal (fromIntegral (y * pitch + x * 4)) $ value x y
      | x <- [0 .. winWidth - 1],
        y <- [0 .. winHeight - 1]
    ]

  -- We're done accessing the raw bytes in the texture.
  unlockTexture texture

  modify (\s -> s {appShouldUpdate = False})

-- Generate a color, given a value between 0.0 and 1.0.
--
-- You can tune the parameters in this function, I've chosen ones that I think
-- result in a pretty image.
--
-- By the way, this function uses cosine because it's the first thing I thought
-- of, but linear interpolation may be simpler and faster.
heatMapColor :: Float -> V3 Word8
heatMapColor x = V3 r b g
  where
    r = round $ max 0 $ 255 * cos ((3 * pi / 2) * (x ** s - rp))
    g = round $ max 0 $ 255 * cos ((3 * pi / 2) * (x ** s - gp))
    b = round $ max 0 $ 255 * cos ((3 * pi / 2) * (x ** s - bp))
    -- The center of the red peak.
    rp = 10 / 15
    -- The center of the green peak.
    gp = 4 / 15
    -- The center of the blue peak.
    bp = 7 / 15
    -- Exponential factor, changing this will affect how "saturated" the
    -- resulting image is.
    s = 0.8

-- Helper function for the mandelbrot and julia set fractals.
helper :: Int -> Int -> Complex Float -> Complex Float -> Int
helper maxIter n z c =
  if n >= maxIter || magnitude z > 2
    then n
    else helper maxIter (n + 1) (z * z + c) c

-- Evaluate the mandelbrot fractal at one point.
mandelbrot :: Int -> Complex Float -> Int
mandelbrot maxIter c = helper maxIter 0 0 c

-- Evaluate the julia set at one point with a predefined constant.
julia :: Int -> Complex Float -> Int
julia maxIter z = helper maxIter 0 z c
  where c = (-0.4) :+ 0.6
