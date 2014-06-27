module Multinomial
(
multinomialPMF
) where

import Factorials

multinomialPMF :: (Num a, Floating a) => [Int] -> Int -> [a] -> a
multinomialPMF xs n ps = (fromIntegral $ factorial n) / (fromIntegral $ product $ map factorial xs) * product [p ^ x | (x, p) <- zip xs ps]
