module Gaussian
( standardNormPDF
, generalNormPDF
, normGenerator
) where

import StatUtils

-- PDF for normal distribution with mean 0 and standard dev 1. 
-- Variables:
--            x: Independent variable value
standardNormPDF :: (Num a, Floating a) => a -> a
standardNormPDF x = exp (-((1/2) * x^2)) / (sqrt (2 * pi))
          

-- General densitiy function for normal distribution.
-- Parameters:
--             x     = Independent variable value
--             mu    = estimate of population mean
--             sigma = estimate of population variance
generalNormPDF :: (Num a, Floating a, Eq a) => a -> a -> a -> a
generalNormPDF x mu 0     = 0
generalNormPDF x mu sigma = constant * (e ** exponent)
    where constant = (1 / (sigma * sqrt (2 * pi)))
          e        = exp 1
          exponent = -(((x - mu)**2) / (2 * sigma**2))
      
-- Function generates a tuple of normally distributed pseudorandom variables.
-- The arguments serve in the generation of uniform PRVs. The current uniform
-- generation process is a linear congruential generator, normalised by the
-- constant 2^31; thus, the periodicity of this generator is short. Currently,
-- this is not meant for critical uses.
-- Parameters:
--             seedA, seedB = Integer values used in generation of uniform vals 
normGenerator :: (Num a, Floating a, Eq a) =>  Int -> Int -> (a, a)
normGenerator seedA seedB = (x, y) 
    where x = sqrt ((-2) * (log $ lcValGen seedA)) * cos (2 * pi * (lcValGen seedB))
          y = sqrt ((-2) * (log $ lcValGen seedA)) * sin (2 * pi * (lcValGen seedB))
