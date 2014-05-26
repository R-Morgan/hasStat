module Factorials
(factorial
, factorialFall
) where

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialFall :: Int -> Int -> Int
factorialFall x 0 = 1
factorialFall x 1 = x
factorialFall x k = (x - k + 1) * factorialFall x (k-1)

factorialRise :: Int -> Int -> Int
factorialRise x 0 = 1
factorialRise x 1 = x
factorialRise x k = (x + k - 1) * factorialRise x (k-1)
