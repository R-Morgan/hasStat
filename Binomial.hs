module Binomial
( binomialCoef
, binomialPDF
, binomialCMF
) where

import Data.List
import Factorials

binomialCoef :: (Fractional a) => Integer -> Integer -> a 
binomialCoef k n    =
    let numerator   = fromIntegral $ factorialFall n k
        denominator = fromIntegral $ factorial k 
    in  numerator / denominator

binomialPDF :: (Num a, Fractional a) => Integer -> Integer -> a -> a 
binomialPDF k n p   = (binomialCoef k n) * (p^k) * (1 - p)^(n-k)

binomialCMF :: (Num a, Fractional a) => Integer -> Integer -> a -> a 
binomialCMF k n p   = sum [binomialPDF ks n p | ks <- [0..k-1]]
