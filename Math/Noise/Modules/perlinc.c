#include "perlin.h"
#include "../noisegen.h"

__ATTR_PURE double perlinGen( const double x 
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
  return value;

/* The code above replaces the following haskell code:
Just $ value $ V.foldr' octaveFunc (0.0, 1.0, x*freq, y*freq, z*freq) (V.generate (octaveCount-1) id)
     where value (v,_,_,_,_) = v
           signal sx sy sz octv = gradientCoherentNoise3D sx sy sz (seed + octv) q
           octaveFunc curOctave (value, curPersistence, ox, oy, oz) =
             ( value + ( (signal ox oy oz (curOctave+1)) * curPersistence)
             , curPersistence * p
             , ox * lac
             , oy * lac
             , oz * lac
             )
*/

}
