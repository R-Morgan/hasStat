module Exponential
( exponentialPDF
, exponentialCDF
, exponentialQuantile
, exponentialValGen
) where

import StatUtils

exponentialPDF :: (Num a, Floating a) => a -> a -> a
exponentialPDF x lambda = lambda * exp (-1 * lambda * x)

exponentialCDF :: (Num a, Floating a) => a -> a -> a
exponentialCDF x lambda = 1 - exp(-1 * lambda * x)

exponentialQuantile :: (Num a, Floating a) => a -> a -> a
exponentialQuantile x lambda = (-1 * log (1-x)) / lambda 

exponentialValGen :: (Num a, Floating a) => a -> a -> a
exponentialValGen u lambda = exponentialQuantile u lambda  -- u ~ Unif(0, 1)

