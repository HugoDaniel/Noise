module Math.Noise.Modules.Abs where
import Math.Noise.NoiseModule
import Control.Applicative

data Absolute = Absolute

absolute = Absolute

instance NoiseClass Absolute where
--  getSourceCount = const 1
  getNoiseValue Absolute [src] (x,y,z) = abs <$> getValue src (x,y,z)
  getNoiseValue a [] i = Nothing
  getNoiseValue _ _ _ = Nothing
