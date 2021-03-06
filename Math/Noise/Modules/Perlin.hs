{-# LANGUAGE ForeignFunctionInterface #-}
module Math.Noise.Modules.Perlin where
import Data.Default
import Math.Noise.NoiseGen
import Math.Noise.NoiseModule
import Data.Bits
import Foreign
import Foreign.C.Types
import Data.Word

foreign import ccall "perlin.h perlinGen"
  c_perlin :: CDouble -> CDouble -> CDouble 
           -> CDouble -> CDouble -> CUInt 
           -> CDouble -> CUInt 
           -> CDouble


data Perlin = Perlin { perlinFrequency :: Double 
                     -- ^ Frequency of the first octave
		             , perlinLacunarity :: Double 
                     -- ^ Frequency multiplier between successive octaves
		             , perlinOctaves :: Word32
                     -- ^ Total number of octaves that generate the Perlin noise
		             , perlinPersistence :: Double 
                     -- ^ Persistence of the Perlin noise
		             , perlinSeed :: Word32
 		             } deriving (Show, Eq)

perlin :: Perlin 
perlin = Perlin { perlinFrequency = 1.0
		        , perlinLacunarity = 2.0
		        , perlinOctaves = 6
		        , perlinPersistence = 0.5
		        , perlinSeed = 123 
		        }

-- TODO: make sure the number of octaves are within range...
-- getValue :: Perlin -> (Double, Double, Double) -> Double
instance NoiseClass Perlin where
  getNoiseValue (Perlin { perlinFrequency = freq
		                , perlinLacunarity = lac
		   	            , perlinOctaves = octaveCount
		   	            , perlinPersistence = p
		   	            , perlinSeed = seed } ) _ (x,y,z) = 
    Just . toDouble $ c_perlin (CDouble x) (CDouble y) (CDouble z) 
                               (CDouble freq) (CDouble lac) 
                               (CUInt octaveCount) (CDouble p) 
                               (CUInt seed)
        where toDouble (CDouble d) = d

{-
    Just $ value $ V.foldr' octaveFunc (0.0, 1.0, x*freq, y*freq, z*freq) (V.generate (octaveCount-1) id)
    where value (v,_,_,_,_) = v
  	  signal sx sy sz octv = gradientCoherentNoise3D sx sy sz (seed + octv) 
	  octaveFunc curOctave (value, curPersistence, ox, oy, oz) = 
	    ( value + ( (signal ox oy oz (curOctave+1)) * curPersistence)
	    , curPersistence * p
	    , ox * lac
	    , oy * lac
	    , oz * lac
	    )
-}
