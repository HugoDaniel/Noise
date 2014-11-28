#include "billow.h"
#include "../noisegen.h"

__ATTR_PURE double billowGen( const double x 
                            , const double y
                            , const double z
                            , const double freq
                            , const double lac
                            , const unsigned int octaveCount
                            , const double persistence
                            , const unsigned int seed
                            )
{
  int currentOctv;
  double value = 0.0;
  double curPersistence = 1.0;
  double sigx = x*freq;
  double sigy = y*freq;
  double sigz = z*freq;
  for(currentOctv = 1; currentOctv <= octaveCount; currentOctv++)
  {
        value += gradientCoherentNoise(sigx, sigy, sigz, seed + currentOctv) 
               * curPersistence;
        sigx = sigx * lac;
        sigy = sigy * lac;
        sigz = sigz * lac;
        curPersistence = curPersistence * persistence;
  } 
  value += 0.5;
  return value;
}

/* The code above replaces the following haskell code:

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

*/
