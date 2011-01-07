module Math.Noise.Test where
import Math.Noise.Modules.Perlin
import Math.Noise.Modules.ScaleBias
import Math.Noise.Modules.Abs
import Math.Noise.Modules.Billow
import Math.Noise.Modules.Select
import Math.Noise.NoiseModule

test = getValue myNoise (-0.1, -0.5, 0.76)

myNoise2 = perlin { perlinFrequency = 0.123 } `isSourceOf` scalebias { scale = 10.0 } `isSourceOf` absolute 
myNoise = perlin `isSourceOf` scalebias { scale = 1.0 } `isSourceOf` absolute 

tNoise = perlin { perlinFrequency = 0.123 } `andModule` billow { billowOctaves = 12 } `andModule` myNoise `areSourcesOf` select { upperBound = 0.7, lowerBound = 0.2 }

{-
n1 `isSourceOf` n2 `isSourceOf` select `with` 


n1 `isSourceOf` select `togetherWith` n2 

(n1, n2) -> NoiseModule N [n1, n2]
(NoiseModule N [n1, n2], n3) -> NoiseModule N [NoiseModule N [n1, n2], n3]
(NoiseModule N [NoiseModule N [n1, n2], n3], n4) -> NoiseModule N [NoiseModule N [NoiseModule N [n1, n2], n3], n4]


[NoiseModule N [NoiseModule N [n1, n2], n3], n4]
areSourcesOf :: NoiseModule -> a -> NoiseModule
areSourcesOf mod n = NoiseModule n (getSrcs 
  where 
    fetchSrcs :: NoiseModule -> [NoiseModule] -> [NoiseModule]
    fetchSrcs (NoiseModule nfunc [s1,s2] ) accum = 
      case nfunc of 
        NoiseModule Nothing srcs -> map ((flip fetchSrcs) accum) srcs
        NoiseModule f [] -> (f:accum)
        Nothing -> (fetchSrcs s1 []) : (fetchSrcs s2 []) : accum
                           


(n1, n2) -> NoiseModule N [NoiseModule N [NoiseModule n1 []] , NoiseModule n2 [] ]
 (NoiseModule N [NoiseModule N [NoiseModule n1 []] , NoiseModule n2 [] ])


(NoiseModule N [NoiseModule n1 [] , NoiseModule n2 [] ], n3) -> NoiseModule N [ NoiseModule (NoiseModule N [NoiseModule n1 [] , NoiseModule n2 [] ]) [], NoiseModule n3 [] ]

NoiseModule N [ NoiseModule (NoiseModule N [ NoiseModule (NoiseModule N [NoiseModule n1 [] , NoiseModule n2 [] ]) [], NoiseModule n3 [] ]) [], NoiseModule n4 [] ]


-}
