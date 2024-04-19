{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Complex
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

-- An aggregation of state for our application.
data AppState = AppState
  { appFractal :: FractalType,
    appShouldUpdate :: Bool,
    appScale :: Float,
    appWinWidth :: Int,
    appWinHeight :: Int,
    appMaxIter :: Int
  }

-- The default state of our application.
defaultAppState :: AppState
defaultAppState =
  AppState
    { appFractal = Mandelbrot,
      appShouldUpdate = True,
      appScale = 1.0,
      appWinWidth = 1200,
      appWinHeight = 800,
      appMaxIter = 100
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

defaultLimits :: V4 Float
defaultLimits = V4 (-2.00) (-1.12) 1.36 1.12

-- Entry point.
main :: IO ()
main = do

  -- Initialize SDL.
  initializeAll

  -- Create a new SDL window with default settings.
  let winWidth = fromIntegral $ appWinWidth defaultAppState
      winHeight = fromIntegral $ appWinHeight defaultAppState
  window <- createWindow
    "Mandlebrot Set"
    defaultWindow { windowInitialSize = V2 winWidth winHeight }

  -- Create a renderer from that window.
  renderer <- createRenderer window (-1) defaultRenderer

  -- Handle events from the user.
  _ <- runStateT (run window renderer) defaultAppState

  -- No more events, clean up our window.
  destroyWindow window

-- The main loop for the app.
run :: Window -> Renderer -> AppMonad ()
run window renderer = do

  -- Wait for an event to happen.
  (Event _timestamp payload) <- waitEvent

  -- If the user pressed the Q key or closed the window, we should quit.
  let shouldQuit = case payload of
        KeyboardEvent keyboardEvent ->
          keyboardEventKeyMotion keyboardEvent == Pressed
            && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
        WindowClosedEvent _ -> True
        _ -> False

  -- Screenshot when S is pressed.
  case payload of
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed
          && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeS ->
          saveRender window renderer
    _ -> return ()

  -- Change modes with number keys.
  case payload of
    KeyboardEvent event | keyboardEventKeyMotion event == Pressed -> do
      oldFractal <- gets appFractal

      case keysymKeycode (keyboardEventKeysym event) of
        Keycode1 -> modify (\s -> s {appFractal = Mandelbrot})
        Keycode2 -> modify (\s -> s {appFractal = JuliaSet})
        _ -> return()

      newFractal <- gets appFractal
      when (oldFractal /= newFractal) $ modify (\s -> s {appShouldUpdate = True})

    _ -> return ()

  -- Zoom in and out with the up and down arrows.
  case payload of
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed
          && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeUp ->
          do
            scale <- gets appScale
            modify (\s -> s {appScale = scale - 0.1})
            modify (\s -> s {appShouldUpdate = True})
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed
          && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeDown ->
          do
            scale <- gets appScale
            modify (\s -> s {appScale = scale + 0.1})
            modify (\s -> s {appShouldUpdate = True})
    _ -> return ()

  -- Render the current fractal that is selected.
  shouldUpdate <- gets appShouldUpdate

  when shouldUpdate $
    do
      mode <- gets appFractal
      maxIter <- gets appMaxIter
      case mode of
        Mandelbrot -> renderFractal mandelbrot renderer maxIter
        JuliaSet -> renderFractal julia renderer maxIter
      modify (\s -> s {appShouldUpdate = False})

  -- If nothing happened, keep running.
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

-- Render the fractal given the callback function
renderFractal :: (Int -> Complex Float -> Int) -> Renderer -> Int -> AppMonad ()
renderFractal callback renderer maxIter = do

  -- Get some parameters.
  scale <- gets appScale
  let limits = (* scale) <$> defaultLimits

  winWidth <- gets (fromIntegral . appWinWidth)
  winHeight <- gets (fromIntegral . appWinHeight)

  -- Clear the screen.
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  -- Create a texture to draw to.
  texture <- createTexture renderer RGB888 TextureAccessStreaming (V2 winWidth winHeight)

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
          -- The value of the mandelbrot fractal.
          m = callback maxIter (a :+ b)

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

  -- Copy the texture to the renderer and display it on the screen.
  copy renderer texture Nothing Nothing
  present renderer

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
