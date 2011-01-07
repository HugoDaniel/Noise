import Control.Monad ( when )
import Data.Bits ( (.&.) )
import Foreign ( withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Maybe

-- Noisy stuff
import Math.Noise.Modules.Perlin
-- import Math.Noise.Modules.ScaleBias
-- import Math.Noise.Modules.Abs
-- import Math.Noise.Modules.Billow
-- import Math.Noise.Modules.Select
import Math.Noise.NoiseModule
-- import Gradient
-- import Noise.Test

-- Vector stuff
import Data.Vector.Storable
import Foreign.ForeignPtr

import TexNoiseVect
import Criterion.Main


main2 :: IO()
main2 = defaultMain [
             bench "perlin 512x512" $ benchmark 512
           , bench "perlin 1024x1024" $ benchmark 1024
           , bench "perlin 2048x2048" $ benchmark 2048
           ]

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   case _args of
     ["show"] -> showTex 
     ["512"] -> defaultMain [ bench "perlin 512x512" $ benchmark 1024 ]
     ["1024"] -> defaultMain [ bench "perlin 1024x1024" $ benchmark 1024 ]
     _ -> defaultMain [ bench "perlin 256x256" $ benchmarkAndProfile 256 
                      , bench "perlin 512x512" $ benchmarkAndProfile 512
                      , bench "perlin 1024x1024" $ benchmarkAndProfile 1024
                      -- , bench "perlin 2048x2048" $ benchmarkAndProfile 2048
                      ]

showTex = do 
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow ""
   -- we have to do this *after* createWindow, otherwise we have no OpenGL context
   version <- get (majorMinor glVersion)
   when (version == (1,0)) $ do
      putStrLn "This program demonstrates a feature which is not in OpenGL Version 1.0."
      putStrLn "If your implementation of OpenGL Version 1.0 has the right extensions,"
      putStrLn "you may be able to modify this program to make it run."
      exitFailure
   texNames <- myInit
   reshapeCallback $= Just reshape
   displayCallback $= display texNames
   keyboardMouseCallback $= Just keyboard
   mainLoop

