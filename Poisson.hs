module Poisson
( poissonPMF
) where

import IncompleteGamma
import Factorials

poissonPMF :: Float -> Int -> Float
poissonPMF k lambda    = (numerator / denominator) * exp ((-1) * (fromIntegral lambda))
    where  numerator   = fromIntegral lambda ** k 
           denominator = factorial k

poissonCMF :: Float -> Int -> Float
poissonCMF k lambda =  numerator / denominator
    where numerator = gammaLowerIncomplete (fromIntegral (truncate (k + 1))) (fromIntegral lambda)
          denominator = factorial (fromIntegral (truncate k))
