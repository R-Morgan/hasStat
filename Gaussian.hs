module Gaussian
( standardNormPDF
, genNormPDF
) where

import Dispersion

standardNormPDF :: (Num a, Floating a) => a -> a
standardNormPDF x = exp (-((1/2) * x^2)) / (sqrt (2 * pi))
          
genNormPDF :: (Num a, Floating a) => a -> a -> a -> a
genNormPDF x mu sigma = (1 / (sigma * sqrt 2*pi)) * exp(-((x - mu)^2 / 2*sigma^2))
