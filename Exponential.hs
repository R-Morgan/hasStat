module Exponential
(
exponentialPDF
) where

exponentialPDF :: (Num a, Floating a) => a -> a -> a
exponentialPDF x lambda = lambda * exp (-1 * lambda * x)

