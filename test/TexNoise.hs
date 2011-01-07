import Control.Monad ( when )
import Data.Bits ( (.&.) )
import Foreign ( withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Maybe

-- Noisy stuff
import Noise.Modules.Perlin
import Noise.Modules.ScaleBias
import Noise.Modules.Abs
import Noise.Modules.Billow
import Noise.Modules.Select
import Noise.NoiseModule
import Gradient
import Noise.Test

-- Vector stuff
import Data.Vector.Storable
import Foreign.ForeignPtr

-- Create checkerboard image
checkImageSize :: TextureSize2D
checkImageSize = TextureSize2D 512 512

withNoiseImage :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withNoiseImage (TextureSize2D w h) n f act =
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c = floor $ (\v ->  (v + 1.0) * 127.5 ) $ noiseClampedVal
                   boundBottomX :: Double
                   boundBottomX = 0.0
                   boundBottomY :: Double
                   boundBottomY = 0.0
                   boundUpperX :: Double
                   boundUpperX = 10.0
                   boundUpperY :: Double
                   boundUpperY = 10.0
                   xsize = w
                   ysize = h
                   xIncrement :: Double
                   xIncrement = (boundUpperX - boundBottomX) / (fromIntegral xsize)
                   yIncrement :: Double
                   yIncrement = (boundUpperY - boundBottomY) / (fromIntegral ysize)
                   xPos x = ((fromIntegral x) * xIncrement)  +  boundBottomX
                   yPos y = ((fromIntegral y) * yIncrement)  +  boundBottomY

                   noiseF :: NoiseModule
                   -- noiseF = gen perlin { perlinFrequency = 0.6, perlinOctaveCount = 5 }
                   noiseF = gen billow { billowFrequency = 0.6, billowOctaves = 5 }

                   -- Actual noise computation, getValue returns Maybe Double
                   noiseValue = fromMaybe (-1.0) $ getValue noiseF (xPos i, yPos j, 2.123)
                   -- Make sure the noiseValue is in the [-1.0, 1.0] range
                   noiseClampedVal = if noiseValue > 1.0 
                                        then 1.0
                                        else if noiseValue < (-1.0) then (-1.0)
                                                                    else noiseValue
                                        
                                                         
               ] $
   act . PixelData RGBA UnsignedByte


withCheckImage :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withCheckImage (TextureSize2D w h) n f act =
   withArray [ f c |
               i <- [ 0 .. w - 1 ],
               j <- [ 0 .. h - 1 ],
               let c | (i .&. n) == (j .&. n) = 0
                     | otherwise              = 255 ] $
   act . PixelData RGBA UnsignedByte

myInit :: IO (TextureObject, TextureObject)
myInit = do
   clearColor $= Color4 0 0 0 0
   shadeModel $= Flat
   depthFunc $= Just Less
   rowAlignment Unpack $= 1

   [texName0, texName1] <- genObjectNames 2
   textureBinding Texture2D $= Just texName0
   textureWrapMode Texture2D S $= (Repeated, Clamp)
   textureWrapMode Texture2D T $= (Repeated, Clamp)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   withNoiseImage checkImageSize 0x18 (\c -> Color4 c c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0

   textureBinding Texture2D $= Just texName1
   textureWrapMode Texture2D S $= (Repeated, Clamp)
   textureWrapMode Texture2D T $= (Repeated, Clamp)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   textureFunction $= Decal
   withNoiseImage checkImageSize 0x10 (\c -> Color4 c 0 0 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0
   texture Texture2D $= Enabled
   return (texName0, texName1)

display ::  (TextureObject, TextureObject) -> DisplayCallback
display (texName0, texName1) = do
   clear [ ColorBuffer, DepthBuffer ]
   -- resolve overloading, not needed in "real" programs
   let texCoord2f = texCoord :: TexCoord2 GLfloat -> IO ()
       vertex3f = vertex :: Vertex3 GLfloat -> IO ()
   textureBinding Texture2D $= Just texName0
   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 (-2.0)    (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 (-2.0)      1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   0.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   0.0     (-1.0)   0.0     )
   textureBinding Texture2D $= Just texName1
   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   2.41421   1.0  (-1.41421))
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   2.41421 (-1.0) (-1.41421))
   flush
   exitWith ExitSuccess

reshape :: ReshapeCallback
reshape size@(Size w h) = do
   viewport $= (Position 0 0, size)
   matrixMode $= Projection
   loadIdentity
   perspective 60 (fromIntegral w / fromIntegral h) 1 30
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-3.6 :: GLfloat))

keyboard :: KeyboardMouseCallback
keyboard (Char '\27') Down _ _ = exitWith ExitSuccess
keyboard _            _    _ _ = return ()

main :: IO ()
main = do
   (progName, _args) <- getArgsAndInitialize
   initialDisplayMode $= [ SingleBuffered, RGBMode, WithDepthBuffer ]
   initialWindowSize $= Size 250 250
   initialWindowPosition $= Position 100 100
   createWindow progName
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

