module IncompleteGamma
(
gammaLowerIncomplete
) where

import HyperGeometric

gammaLowerIncomplete :: (Num a, Ord a, Enum a, Floating a) => a -> a -> a
gammaLowerIncomplete a x = (a ** (-1)) * (x ** a) * exp (-x) * hyperGeomFirstOrd 1 (1 + a) x


