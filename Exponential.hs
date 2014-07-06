module Exponential
( exponentialPDF
, exponentialAltPDF
, exponentialCDF
, exponentialQuantile
, exponentialValGen
) where

import StatUtils

exponentialPDF :: (Num a, Ord a, Floating a) => a -> a -> a
exponentialPDF x lambda 
    | x      <= 0 = 0
    | lambda <= 0 = 0 
    | otherwise = lambda * exp (-1 * lambda * x)

exponentialAltPDF :: (Num a, Ord a, Floating a) => a -> a -> a
exponentialAltPDF x beta
    | x      <= 0 = 0
    | beta   <= 0 = 0 
    | otherwise = (1 / beta) * exp (-1 * (x / beta))

exponentialCDF :: (Num a, Floating a) => a -> a -> a
exponentialCDF x lambda = 1 - exp(-1 * lambda * x)

exponentialQuantile :: (Num a, Floating a) => a -> a -> a
exponentialQuantile x lambda = (-1 * log (1-x)) / lambda 

exponentialValGen :: (Num a, Floating a) => a -> a -> a
exponentialValGen u lambda = exponentialQuantile u lambda  -- u ~ Unif(0, 1)

