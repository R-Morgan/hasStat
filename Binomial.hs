module Binomial
(binomialCoef
) where

import Data.List
import Factorials

binomialCoef :: Int -> Int -> Float
binomialCoef x k    =
    let numerator   = fromIntegral $ factorialFall x k
        denominator = fromIntegral $ factorial k 
    in  numerator / denominator
