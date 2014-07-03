module Gaussian
( standardNormPDF
, generalNormPDF
, normGenerator
) where

import Dispersion
import StatUtils

standardNormPDF :: (Num a, Floating a) => a -> a
standardNormPDF x = exp (-((1/2) * x^2)) / (sqrt (2 * pi))
          
generalNormPDF :: (Num a, Floating a) => a -> a -> a -> a
generalNormPDF x mu sigma = constant * (e ** exponent)
    where constant = (1 / (sigma * sqrt (2 * pi)))
          e        = exp 1
          exponent = -(((x - mu)**2) / (2 * sigma**2))
      
