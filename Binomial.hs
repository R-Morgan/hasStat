module Binomial
( binomialCoef
, binomialPDF
, binomialCDF
) where

import Data.List
import Factorials

binomialCoef :: Int -> Int -> Float
binomialCoef k n    =
    let numerator   = fromIntegral $ factorialFall n k
        denominator = fromIntegral $ factorial k 
    in  numerator / denominator

binomialPDF :: Int -> Int -> Float -> Float
binomialPDF k n p   = (binomialCoef k n) * (p^k) * (1 - p)^(n-k)

binomialCMF :: Int -> Int -> Float -> Float
binomialCMF k n p   = sum [binomialPDF ks n p | ks <- [0..k-1]]
