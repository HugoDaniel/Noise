module Math.Noise.Interpolation where

{- | Performs cubic interpolation between two values bound between two other values
 - The alpha value should range from 0.0 to 1.0. 
 - If the alpha value is 0.0, this function returns the first value.
 - If the alpha vlaue is 1.0, this function returns the second value. 
 -}
cubic :: Double -> Double -> Double -> Double -> Double -> Double
cubic beforeFirstValue firstValue secondValue afterSecondValue alpha = 
  (p * alpha * alpha * alpha) + (q * alpha * alpha) + (r * alpha) + firstValue
  where p = (afterSecondValue - secondValue) - (beforeFirstValue - firstValue)
        q = (beforeFirstValue- firstValue) - p
        r = secondValue - beforeFirstValue

{- | Performs linear interpolation between two values.
 - The alpha value should range from 0.0 to 1.0.
 - If the alpha value is 0.0, this function returns the first value.
 - If the alpha value is 1.0, this function returns the second value.
 -}
linear :: Double -> Double -> Double -> Double 
{-# INLINE linear #-}
linear firstValue secondValue alpha = 
  ((1.0 - alpha) * firstValue) + (alpha * secondValue)


{- | Maps a value onto a cubic S-curve
 - the derivative of a cubic S-curve is zero at 0.0 and "value" at 1.0 -}
scurve3 :: Double -> Double
{-# INLINE scurve3 #-}
scurve3 value = value^3

{- | Maps a value onto a quintic S-curve.
 - The first derivative is zero at 0.0 and "value" at 1.0
 - The second derivative is zero at 0.0 and "value" at 1.0 -}
scurve5 :: Double -> Double
{-# INLINE scurve5 #-}
scurve5 value = (6.0 * value^5) - (15.0 * value^4) + (10.0 * value^3)

