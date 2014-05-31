module Gaussian
( standardNormPDF
) where

import Dispersion

standardNormPDF :: (Num a, Floating a) => a -> a
standardNormPDF x = e**((-1/2) * x^2) / sqrt 2*pi
    where e = exp(1)
