module Binomial
( binomialCoef
, binomialPMF
, binomialCMF
, tuplePMF
, binomialLL
, binomialLLRec
) where

import Data.List
import Factorials

binomialCoef :: (Fractional a) => Integer -> Integer -> a 
binomialCoef k n    =
    let numerator   = fromIntegral $ factorialFall n k
        denominator = fromIntegral $ factorial k 
    in  numerator / denominator

binomialPMF :: (Num a, Fractional a) => Integer -> Integer -> a -> a 
binomialPMF k n p   = (binomialCoef k n) * (p^k) * (1 - p)^(n-k)

binomialCMF :: (Num a, Fractional a) => Integer -> Integer -> a -> a 
binomialCMF k n p   = sum [binomialPMF ks n p | ks <- [0..k-1]]

tuplePMF :: (Num a, Fractional a) => (Integer, Integer, a) -> a
tuplePMF (k, n, p) = binomialPMF k n p

binomialLL :: (Num a, Floating a) => a -> a -> a -> a
binomialLL k n p = k * log p + (n - k) * log (1-p)

binomialLLRec :: (Num a, Floating a) => [a] -> [a] -> [a] -> [a]
binomialLLRec [] [] [] = []
binomialLLRec (k:ks) (n:ns) (p:ps) = binomialLL k n p : binomialLLRec ks ns ps
