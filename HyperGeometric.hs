module HyperGeometric 
(
 hyperGeomFirstOrd
) where

import Factorials

hyperGeomFirstOrd :: (Num a, Enum a, Floating a, Ord a) => a -> a -> a -> a
hyperGeomFirstOrd a b x = sum [(pocahammer a k / pocahammer b k) * (x ** k) * (1 / factorial k) | k <- [0..10]]

