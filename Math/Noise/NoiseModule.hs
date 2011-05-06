{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving,
             ExistentialQuantification, MultiParamTypeClasses #-}
module Math.Noise.NoiseModule where
import Control.Applicative
import Data.Maybe
import Data.Typeable

class NoiseClass n where
  getNoiseValue :: n -> [NoiseModule] -> (Double, Double, Double) -> Maybe Double
  getValue :: n -> (Double, Double, Double) -> Maybe Double
  getValue n i = getNoiseValue n [] i
  gen :: n -> NoiseModule
  gen n = NoiseModule (sanitize n) []
  sanitize :: n -> n
  sanitize = id 

data NoiseModule = forall n. (NoiseClass n) => NoiseModule { noiseFunc :: n
					                                       , sources :: [NoiseModule]
					                                       }

instance NoiseClass NoiseModule where
  getNoiseValue (NoiseModule n srcs ) src2 (x, y, z) = getNoiseValue n (src2 ++ srcs) (x, y, z)
  gen = id

data ZeroNoise = ZeroNoise
instance NoiseClass ZeroNoise where
  getNoiseValue _ _ _ = Just 0.0

data OneNoise = OneNoise
instance NoiseClass OneNoise where
  getNoiseValue _ _ _ = Just 1.0

instance NoiseClass (Maybe a) where
  getNoiseValue _ _ _ = Nothing

zero :: NoiseModule
zero = NoiseModule { noiseFunc = ZeroNoise , sources = [] }

one :: NoiseModule
one = NoiseModule { noiseFunc = OneNoise, sources = [] }

isSourceOf :: (NoiseClass a, NoiseClass b) => a -> b -> NoiseModule 
isSourceOf s1 n = NoiseModule { noiseFunc = n, sources = [gen s1] } 

andModule :: (NoiseClass a, NoiseClass b) => a -> b -> NoiseModule
andModule s1 s2 = NoiseModule Nothing [gen s1, gen s2]

areSourcesOf :: (NoiseClass a) => NoiseModule -> a -> NoiseModule
areSourcesOf mod n = setSrcs . gen $ n 
  where 
    setSrcs (NoiseModule f _) = NoiseModule f (fetchSrcs mod [])
    fetchSrcs :: NoiseModule -> [NoiseModule] -> [NoiseModule]
    fetchSrcs (NoiseModule nfunc [s1, s2] ) accum = fetchSrcs s1 (gen s2 : accum)
    fetchSrcs (NoiseModule nfunc []) accum = (gen nfunc) : accum

-- | creates a NoiseModule from a 1D function, the created module does not require sources 
{-
generator1D :: (Double->Double) -> NoiseModule
generator1D f = 
-}


