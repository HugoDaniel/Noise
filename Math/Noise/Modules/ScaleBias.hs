module Math.Noise.Modules.ScaleBias where
import Math.Noise.NoiseModule

-- | ScaleBias can scale or shift a noise value, the formula that this module
-- applies to the noise value is: noise * scale + bias
data ScaleBias = ScaleBias { bias
                           , scale :: Double
                           }

-- | ScaleBias default values (bias = 0.0, scale = 1.0)
scalebias :: ScaleBias 
scalebias = ScaleBias { bias = 0.0, scale = 1.0 }

instance NoiseClass ScaleBias where
  getNoiseValue (ScaleBias {bias = bias, scale = scale}) [src] (x,y,z) =
    fmap sbfunc $ getValue src (x, y, z) 
    where sbfunc a = a * scale + bias
  getNoiseValue _ _ _ = Nothing

