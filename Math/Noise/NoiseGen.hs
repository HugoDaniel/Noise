{-# LANGUAGE ForeignFunctionInterface #-}
module Math.Noise.NoiseGen where
import Data.Int
import Data.Bits
import Data.Array.Unboxed
import Math.Noise.Interpolation
import Math.Noise.VectorTable
import Data.Fixed (mod')
import qualified Data.Vector.Unboxed as Vector
import Foreign
import Foreign.C.Types

foreign import ccall "noisegen.h gradientCoherentNoise"
  c_gradientNoise :: CDouble -> CDouble -> CDouble -> CInt -> CDouble

-- CONSTANTS
--xNoiseGen :: Int32
xNoiseGen = 1619

--yNoiseGen :: Int32
yNoiseGen = 31337

--zNoiseGen :: Int32
zNoiseGen = 6971

--seedNoiseGen :: Int32
seedNoiseGen = 1013

shiftNoiseGen :: Int
shiftNoiseGen = 8 

-- | Generates a gradient-coherent-noise value from the coordinates of a 3D value
{-
gradientCoherentNoise3D :: Double -> Double -> Double -> Int -> NoiseQuality -> Double
gradientCoherentNoise3D x y z seed quality = noiseCube x y z quality gradNoise
  where gradNoise i0 i1 i2 = gradientNoise3D x y z (toInt i0) (toInt i1) (toInt i2) seed
        toInt = fromIntegral . floor 
-}

gradientCoherentNoise3D :: Double -> Double -> Double -> Int -> Double
gradientCoherentNoise3D x y z seed = realToFrac $ c_gradientNoise (realToFrac x) (realToFrac y) (realToFrac z) (fromIntegral seed)

{-
  where x0 = if x > 0.0 then floor x
                        else round (x - 1)
        x1 = x0 + 1
        y0 = if y > 0.0 then floor y
                        else round (y -1)
        y1 = y0 + 1
        z0 = if z > 1.0 then floor z
                        else round (z - 1)
        z1 = z0 + 1

        (xs,ys,zs) = case quality of
                           Fast -> (x - fromIntegral x0 
                                   ,y - fromIntegral y0
                                   ,z - fromIntegral z0)
                           Standard -> (scurve3 (x - fromIntegral x0)
                                       ,scurve3 (y - fromIntegral y0)
                                       ,scurve3 (z - fromIntegral z0) )
                           Best -> (scurve5 (x - fromIntegral x0)
                                   ,scurve5 (y - fromIntegral y0)
                                   ,scurve5 (z - fromIntegral z0) )

        noiseFunc = gradientNoise3D x y z seed
        n0a = noiseFunc x0 y0 z0 
        n1a = noiseFunc x1 y0 z0 
        ix0a = linear n0a n1a xs
        n0b = noiseFunc x0 y1 z0 
        n1b = noiseFunc x1 y1 z0 
        ix1a = linear n0b n1b xs
        iy0 = linear ix0a ix1a ys
        n0c = noiseFunc x0 y0 z1 
        n1c = noiseFunc x1 y0 z1 
        ix0b = linear n0c n1c xs
        n0d = noiseFunc x0 y1 z1 
        n1d = noiseFunc x1 y1 z1 
        ix1b = linear n0d n1d xs
        iy1 = linear ix0b ix1b ys

        final = linear iy0 iy1 zs
-}

{-
        (a0,a1,b0,b1,c0,c1,d0,d1) = fastGradientNoise x y z seed x0 y0 z0
        ix0a = linear a0 a1 xs
        ix1a = linear b0 b1 xs
        iy0 = linear ix0a ix1a ys
        ix0b = linear c0 c1 xs
        ix1b = linear d0 d1 xs
        iy1 = linear ix0b ix1b ys
        final = linear iy0 iy1 zs
-}



{- | Generates a gradient-noise value from the coordinates of a 3D input value and
the integer coordinates of a nearby 3D value -}
gradientNoise3D :: Double -> Double -> Double -> Int -> Int -> Int -> Int -> Double
gradientNoise3D !fx !fy !fz !seed ix iy iz = 
  (xvGrad * xvPoint) +
  (yvGrad * yvPoint) +
  (zvGrad * zvPoint) * 2.12
  where icalc = xNoiseGen * ix
              + yNoiseGen * iy
              + zNoiseGen * iz
              + seedNoiseGen * seed
                                       
        index = shiftL ( (xor (fromIntegral icalc) (shiftR icalc shiftNoiseGen)) .&. 0xff) 2 -- gen some quick random shiznit

        xvGrad :: Double
        xvGrad = staticTable Vector.! index 
        yvGrad :: Double
        yvGrad = staticTable Vector.! (index + 1)
        zvGrad :: Double
        zvGrad = staticTable Vector.! (index + 2)

        xvPoint :: Double
        xvPoint = fx - (fromIntegral ix)
        yvPoint :: Double
        yvPoint = fy - (fromIntegral iy)
        zvPoint :: Double
        zvPoint = fz - (fromIntegral iz)

{-
intValueNoise3D :: Int -> Int -> Int -> Int -> Int
intValueNoise3D x y z seed = 
  fromIntegral $ (n * (n * n * 60493 + 19990303) + 1376312589) .&. 0x7fffffff
  where nx :: Int32
        nx = fromIntegral x
        ny :: Int32
        ny = fromIntegral y
        nz :: Int32
        nz = fromIntegral z
        nseed :: Int32
        nseed = fromIntegral seed
        baseVal = (xNoiseGen * nx
                 + yNoiseGen * ny
                 + zNoiseGen * nz
                 + seedNoiseGen * nseed) .&. 0x7fffffff
        n = xor (shiftR baseVal 13) baseVal


-- | Generates a value-coherent-noise value from the coordinates of a 3D input
valueCoherentNoise3D :: Double -> Double -> Double -> Int -> NoiseQuality -> Double
valueCoherentNoise3D x y z seed quality = noiseCube x y z quality valNoise
  where valNoise i0 i1 i2 = valueNoise3D (toInt i0) (toInt i1) (toInt i2) seed
        toInt = fromIntegral . floor 


-- | Generates a value-noise value from the coordinates of a 3D input value
valueNoise3D :: Int -> Int -> Int -> Int -> Double
valueNoise3D x y z seed = 
  let noiseCalc = (fromIntegral $ intValueNoise3D x y z seed) in
    1.0 - ( noiseCalc / 1073741824.0 )
    -}
