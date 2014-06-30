module Factorials
(factorial
, factorialFall
, pocahammer
) where

import Gamma

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialFall :: Integer -> Integer -> Integer
factorialFall x 0 = 1
factorialFall x 1 = x
factorialFall x k = (x - k + 1) * factorialFall x (k-1)

factorialRise :: Integer -> Integer -> Integer
factorialRise x 0 = 1
factorialRise x 1 = x
factorialRise x k = (x + k - 1) * factorialRise x (k-1)

pocahammer :: (Num a, Floating a, Eq a) => a -> a -> a
pocahammer x 0 = 1
pocahammer x 1 = x
pocahammer x n = (gammaFunc (x + n) 10000) / gammaFunc x 10000
