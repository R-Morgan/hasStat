module Gamma
(
gammaFunc
, gammaPDF
) where

gammaFunc :: (Num a, Floating a) => a -> Int -> a
gammaFunc t n =
    (exp(-gamma*t)/t) * product [(1 + (t/(fromIntegral ns)))**(-1) * (exp(t/(fromIntegral ns))) | ns <- [1..n]]
    where gamma = 0.577216

gammaPDF :: (Num a, Floating a) => a -> a -> a -> a
gammaPDF x k theta = let gam       = gammaFunc k 10000
                         numerator =(x ** (k-1)) * exp (-(x / theta))
                         denominator = (theta ** k) * gam
                     in  numerator / denominator

