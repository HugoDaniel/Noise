{-# LANGUAGE ForeignFunctionInterface #-}
module Billow where
import Math.Noise.NoiseGen
import Math.Noise.NoiseModule
import Foreign
import Foreign.C.Types

{-
foreign import ccall "billow.h billowGen"
  c_billow :: CDouble -> CDouble -> CDouble 
           -> CDouble -> CDouble -> CInt 
           -> CDouble -> CInt 
           -> CDouble
-}

data Billow = Billow { billowFrequency
                     , billowLacunarity
                     , billowPersistence :: Double
                     , billowOctaves
                     , billowSeed 
                     , billowMaxOctave :: Int
                     }

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
    Just . (+) 0.5 . value $ 
      foldr octaveFunc (0.0, 1.0, ix, iy, iz) [0..octaveCount]
    where ix = x * freq
          iy = y * freq
          iz = z * freq
          fromDouble = fromInteger . floor
          value (v,_,_,_,_) = v
          signal sx sy sz octv = 2.0 * (abs $ gradientCoherentNoise3D sx sy sz (octaveSeed octv)) - 1.0
          octaveSeed octv = seed + octv
          octaveFunc curOctave (value, curPersistence, ox, oy, oz) =
                ( value + ( (signal ox oy oz curOctave) * curPersistence)
                , curPersistence * pers
                , ox * lac
                , oy * lac
                , oz * lac
                )
