module Math.Noise.Modules.Select where
import Math.Noise.NoiseGen
import Math.Noise.NoiseModule
import Math.Noise.Interpolation
import Data.Maybe
import Control.Applicative

data Select = Select { edgeFallOff 
		             , lowerBound 
		             , upperBound :: Double
		             , controller :: NoiseModule
		             }
-- | Select module default values: edgeFallOff = 0.0, lowerBound = -1.0, upperBound = 1.0, controller = first noise module
select = Select { edgeFallOff = 0.0
                , lowerBound = -1.0
                , upperBound = 1.0
                , controller = one 
                }
-- | Adjusts select values so that they are correct (i.e. lowerBound < upperBound, etcc...)
sanitizeSelect (Select { edgeFallOff = falloff, lowerBound = l, upperBound = u, controller = c } ) = Select { edgeFallOff = newFalloff, lowerBound = chooseLower, upperBound = chooseUpper, controller = c }
  where 
    chooseLower | l > u  = u
                | u > l  = l
                | u == l = -1.0
    chooseUpper | l > u = l
                | u > l = u
                | u == l = 1.0
    -- make sure that the edge falloff curves do not overlap
    chooseFalloff f up low | f > ( (up - low) / 2) = (up - low) / 2
                           | otherwise = f
    newFalloff = chooseFalloff falloff chooseUpper chooseLower

instance NoiseClass Select where
    sanitize = sanitizeSelect
    getNoiseValue (Select falloff low up ctrl) [src1,src2] pt 
        | up > low = selectFunc
        | otherwise = Nothing
    where 
        controlVal = fromMaybe 0.0 $ getValue ctrl pt
        src1Val = getValue src1 pt
        src2Val = getValue src2 pt
        lowerCurve1 = low - falloff
        upperCurve1 = low + falloff
        alpha1 = scurve3 $ (controlVal - lowerCurve1) / (upperCurve1 - lowerCurve1)
        lowerCurve2 = up - falloff
        upperCurve2 = up + falloff
        alpha2 = scurve3 $ (controlVal - lowerCurve2) / (upperCurve2 - lowerCurve2)
        maybeLinear (Just a) (Just b) alph = Just $ linear a b alph
        maybeLinear _ _ alph = Nothing
        selectFunc | falloff > 0.0 = selectFallOff
                   | otherwise     = selectNoFallOff
        selectFallOff | controlVal < (low - falloff) = getValue src1 pt
                      | controlVal < (low + falloff) = maybeLinear src1Val src2Val alpha1 
                      | controlVal < (up - falloff) = src2Val
                      | controlVal < (up + falloff) = maybeLinear src2Val src1Val alpha2
                      | otherwise = src1Val
        selectNoFallOff | controlVal < low || controlVal > up = src1Val 
                        | otherwise = src2Val
    getNoiseValue _ _ _ = Nothing

