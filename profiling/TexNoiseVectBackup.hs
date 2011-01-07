{-# LANGUAGE BangPatterns #-}
module TexNoiseVect where 
import Control.Monad ( when )
import Data.Bits ( (.&.) )
import Foreign ( withArray )
import System.Exit ( exitFailure, exitWith, ExitCode(ExitSuccess) )
import Graphics.UI.GLUT
import Maybe

-- Noisy stuff
import Math.Noise.Modules.Perlin
{-
import Noise.Modules.ScaleBias
import Noise.Modules.Abs
import Noise.Modules.Billow
import Noise.Modules.Select
-}
import Math.Noise.NoiseModule
import Math.Noise.NoiseGen
-- import Gradient

-- Vector stuff
import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr

-- Parallel stuff
import Control.Parallel
--import Control.Monad.Parallel

-- Create checkerboard image
checkImageSize :: TextureSize2D
checkImageSize = TextureSize2D 1024 1024

defColor = const Color4 0 0 0 0

noiseParGen :: GLsizei -> GLsizei -> (GLubyte -> Color4 (GLubyte) ) -> Int -> Int -> V.Vector (Color4 GLubyte)

noiseParGen w h f total !tnum | tnum == total = genFunc total 
                              -- | otherwise = (V.++) gen1 $ par gen1 gen2 
                              | otherwise = par gen1 (seq gen2 (gen1 V.++ gen2))
    where 
          !ammount = floor $ ((fromIntegral w) * (fromIntegral h)) / (fromIntegral total)
          genFunc !num = V.generate ammount (vecFunc w h f  ((num - 1) *ammount) )
          gen1 = genFunc tnum 
          gen2 = noiseParGen w h f total (tnum + 1)

noiseParGen4 :: GLsizei -> GLsizei -> (GLubyte -> Color4 (GLubyte) ) -> Int -> Int -> Int -> IO (V.Vector (Color4 GLubyte))
noiseParGen4 w h f numt proc1 proc2 = do  
    -- v1 <- forkExec $ genFunc proc1
    -- let v2 = genFunc proc2
    --return $ v1 V.++ v2
    return $ (genFunc 1) V.++ (genFunc 2) V.++ (genFunc 3) V.++ (genFunc 4)
    where 
          ammount = floor $ ((fromIntegral w) * (fromIntegral h)) / (fromIntegral numt)
          genFunc num = V.generate ammount (vecFunc w h f  ((num - 1) *ammount) )

noiseParGenAll w h f numt = return $ 
  foldr (V.++) V.empty lst
  where 
        ammount = floor $ ((fromIntegral w) * (fromIntegral h)) / (fromIntegral numt)
        genFunc num = V.generate ammount (vecFunc w h f  ((num - 1) *ammount) )
        lst = map genFunc [1..numt]

noiseGen2 w h f = V.generate ((fromIntegral w) * (fromIntegral h)) (vecFunc w h f 0)

vecFunc w h f baseIndx indx = f c 
 where c = floor $ (\v ->  (v + 1.0) * 127.5 ) $ noiseClampedVal
       realIndx = (fromIntegral baseIndx) + (fromIntegral indx)
       (i,j) = (realIndx `mod` w, floor $ (/) (fromIntegral realIndx) (fromIntegral w) )
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
       noiseF = gen perlin{ perlinFrequency = 0.6, perlinOctaveCount = 8}

       -- Actual noise computation, getValue returns Maybe Double
       noiseValue = fromMaybe (-1.0) $ getValue noiseF (xPos i, yPos j, 2.123)
       noiseClampedVal = min 1.0 . max (-1.0) $ noiseValue      

noiseGen1 w h f = (V.fromList [ f c |
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
            ])

withParNoiseVec :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withParNoiseVec (TextureSize2D w h) n f act =    
  V.unsafeWith (noiseParGen (fromIntegral w) (fromIntegral h) f 4 1) (act . PixelData RGBA UnsignedByte)

withNoiseVec :: TextureSize2D -> GLsizei -> (GLubyte -> (Color4 GLubyte))
               -> (PixelData (Color4 GLubyte) -> IO ()) -> IO ()
withNoiseVec (TextureSize2D w h) n f act =
   V.unsafeWith (noiseGen2 (fromIntegral w) (fromIntegral h) f) (act . PixelData RGBA UnsignedByte)

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

benchmark :: Int -> IO()
benchmark n = withNoiseVec (TextureSize2D (fromIntegral n) (fromIntegral n) ) 0x18 (\c -> Color4 c c c 255) $
                texImage2D Nothing NoProxy 0  RGBA' (TextureSize2D (fromIntegral n) (fromIntegral n) ) 0

benchmarkAndProfile :: Int -> IO()
benchmarkAndProfile n = withNoiseVec (TextureSize2D (fromIntegral n) (fromIntegral n) ) 0x18 (\c -> Color4 c c c 255) $
                texImage2D Nothing NoProxy 0  RGBA' (TextureSize2D (fromIntegral n) (fromIntegral n) ) 0

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
   withParNoiseVec checkImageSize 0x18 (\c -> Color4 c c c 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0

{-
   textureBinding Texture2D $= Just texName1
   textureWrapMode Texture2D S $= (Repeated, Clamp)
   textureWrapMode Texture2D T $= (Repeated, Clamp)
   textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
   textureFunction $= Decal
   withParNoiseVec checkImageSize 0x10 (\c -> Color4 c 0 0 255) $
      texImage2D Nothing NoProxy 0  RGBA' checkImageSize 0
-}
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
   textureBinding Texture2D $= Just texName0
   renderPrimitive Quads $ do
      texCoord2f (TexCoord2 0 0); vertex3f (Vertex3   1.0     (-1.0)   0.0     )
      texCoord2f (TexCoord2 0 1); vertex3f (Vertex3   1.0       1.0    0.0     )
      texCoord2f (TexCoord2 1 1); vertex3f (Vertex3   2.41421   1.0  (-1.41421))
      texCoord2f (TexCoord2 1 0); vertex3f (Vertex3   2.41421 (-1.0) (-1.41421))
   flush
   -- exitWith ExitSuccess

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
