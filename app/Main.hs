{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Complex
import Data.Time.Clock
import Data.Time.Format
import qualified Data.Vector.Storable.Mutable as V
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import SDL hiding (get)
import qualified SDL.Internal.Types
import qualified SDL.Raw as Raw

data FractalType = Mandelbrot | JuliaSet

data AppState = AppState
  { appStateFractal :: FractalType,
    appStateShouldUpdate :: Bool,
    appStateScale :: Float
  }

defaultAppState :: AppState
defaultAppState =
  AppState
    { appStateFractal = Mandelbrot,
      appStateShouldUpdate = True,
      appStateScale = 1.0
    }

type AppMonad = StateT AppState IO

winWidth :: CInt
winWidth = 1200

winHeight :: CInt
winHeight = 800

defaultLimits :: V4 Float
defaultLimits = V4 (-2.00) (-1.12) 1.36 1.12

-- Entry point.
main :: IO ()
main = do
  -- Initialize SDL.
  initializeAll

  -- Create a new SDL window with default settings.
  window <-
    createWindow
      "Mandlebrot Set"
      defaultWindow
        { windowInitialSize = V2 winWidth winHeight
        }

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

  -- Change modes with number keys
  -- There is definitely a cleaner way to do this Haskell just doesn't have finally blocks
  case payload of
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed
          && keysymKeycode (keyboardEventKeysym keyboardEvent) == Keycode1 ->
          do
            modify (\s -> s {appStateFractal = Mandelbrot})
            modify (\s -> s {appStateShouldUpdate = True})
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed
          && keysymKeycode (keyboardEventKeysym keyboardEvent) == Keycode2 ->
          do
            modify (\s -> s {appStateFractal = JuliaSet})
            modify (\s -> s {appStateShouldUpdate = True})
    _ -> return ()

  -- Zoom in and out with UP/DOWN ARROW
  case payload of
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed
          && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeUp ->
          do
            scale <- gets appStateScale
            modify (\s -> s {appStateScale = scale - 0.1})
            modify (\s -> s {appStateShouldUpdate = True})
    KeyboardEvent keyboardEvent
      | keyboardEventKeyMotion keyboardEvent == Pressed
          && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeDown ->
          do
            scale <- gets appStateScale
            modify (\s -> s {appStateScale = scale + 0.1})
            modify (\s -> s {appStateShouldUpdate = True})
    _ -> return ()

  -- Render the current mode selected
  shouldUpdate <- gets appStateShouldUpdate
  when shouldUpdate $
    do
      mode <- gets appStateFractal
      case mode of
        Mandelbrot -> renderFractal mandelbrot renderer 100
        JuliaSet -> renderFractal julia renderer 100
      modify (\s -> s {appStateShouldUpdate = False})

  -- If nothing happened, keep running.
  unless shouldQuit (run window renderer)

-- Write what's on screen to file
saveRender :: Window -> Renderer -> AppMonad ()
saveRender window renderer = do
  currentTime <- liftIO getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d-%H:%M:%S" currentTime
      filename = "screenshots/" ++ formattedTime ++ ".png"
  filenameC <- liftIO (newCString filename)

  let (SDL.Internal.Types.Renderer rawRenderer) = renderer
  let (SDL.Internal.Types.Window rawWindow) = window

  pixFormat <- Raw.getWindowPixelFormat rawWindow

  surface <-
    Raw.createRGBSurface
      0
      winWidth
      winHeight
      32
      0x00ff0000
      0x0000ff00
      0x000000ff
      0xff000000

  pixels <- Raw.surfacePixels <$> liftIO (peek surface)

  _ <-
    Raw.renderReadPixels
      rawRenderer
      nullPtr
      pixFormat
      pixels
      4800

  _ <- Raw.saveBMP surface filenameC

  liftIO $ putStrLn $ "Saved screenshot to " ++ filename

-- Render the fractal given the callback function
renderFractal :: (Int -> Complex Float -> Int) -> Renderer -> Int -> AppMonad ()
renderFractal callback renderer maxIter = do
  scale <- gets appStateScale
  let limits = (* scale) <$> defaultLimits

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
      -- value of the mandelbrot function.
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

-- Generate a color given a value between 0.0 and 1.0.
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

-- Evaluate the mandelbrot fractal at one point.
mandelbrot :: Int -> Complex Float -> Int
mandelbrot maxIter c = helper 0 0
  where
    helper n z =
      if n >= maxIter || magnitude z > 2
        then n
        else helper (n + 1) (z * z + c)

-- Evaluate the julia set at one with a constant
julia :: Int -> Complex Float -> Int
julia maxIter = helper 0
  where
    constant = (-0.4) :+ 0.6
    helper n z =
      if n >= maxIter || magnitude z > 2
        then n
        else helper (n + 1) (z * z + constant)
