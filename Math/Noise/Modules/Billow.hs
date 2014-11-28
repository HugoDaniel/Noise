{-# LANGUAGE ForeignFunctionInterface #-}
module Math.Noise.Modules.Billow where
import Math.Noise.NoiseGen
import Math.Noise.NoiseModule
import Foreign
import Foreign.C.Types

foreign import ccall "billow.h billowGen"
  c_billow :: CDouble -> CDouble -> CDouble 
           -> CDouble -> CDouble -> CUInt 
           -> CDouble -> CUInt 
           -> CDouble

-- | Billow noise value is just a special case of perlin noise, it shares
-- all of its input
data Billow = Billow { billowFrequency
                     , billowLacunarity
                     , billowPersistence :: Double
                     , billowOctaves
                     , billowSeed 
                     , billowMaxOctave :: Int
                     }

-- | Billow data type with default values
billow :: Billow
billow = Billow { billowFrequency = 1.0
                , billowLacunarity = 2.0
                , billowPersistence = 0.5
                , billowOctaves = 6
                , billowSeed = 125
                , billowMaxOctave = 30
                }

instance NoiseClass Billow where
  getNoiseValue (Billow { billowFrequency = freq 
                        , billowLacunarity = lac
                        , billowOctaves = octaveCount
                        , billowPersistence = pers
                        , billowSeed = seed
                        , billowMaxOctave = maxoctv
                        } ) _ (x,y,z) =
    Just . toDouble $ c_billow (realToFrac x) (realToFrac y) (realToFrac z) 
                               (realToFrac freq) (realToFrac lac) 
                               (fromIntegral octaveCount) (realToFrac pers) 
                               (fromIntegral seed)
        where toDouble (CDouble d) = d

