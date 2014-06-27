module Multinomial
(
multinomialPMF
) where

import Factorials

multinomialPMF :: (Num a, Floating a) => [a] -> Int -> [a] -> Float
multinomialPMF xs n ps = (factorial n) / (product $ map factorial xs) * product [p ^ x | x <- xs, p <- ps]
