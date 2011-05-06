module Math.Noise.Modules.ScaleBias where
import Math.Noise.NoiseModule
import Control.Applicative

data ScaleBias = ScaleBias { bias
			   , scale :: Double
			   }

scalebias :: ScaleBias 
scalebias = ScaleBias { bias = 0.0, scale = 1.0 }

instance NoiseClass ScaleBias where
--  getSourceCount = const 1
  getNoiseValue (ScaleBias {bias = bias, scale= scale}) [src] (x,y,z) =
    sbfunc <$> getValue src (x, y, z) 
    where sbfunc a = a * scale + bias

  getNoiseValue s [] (x,y,z) = Nothing

