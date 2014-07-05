module Poisson
( poissonPMF
) where

import Factorials

poissonPMF :: Float -> Int -> Float
poissonPMF k lambda    = (numerator / denominator) * exp ((-1) * (fromIntegral lambda))
    where  numerator   = fromIntegral lambda ** k 
           denominator = factorial k

