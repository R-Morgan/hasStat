module Means
( ostTest
, tstTest
) where

import Dispersion

ostTest :: (Fractional a, Floating a) => [a] -> a -> a
ostTest vals testmean =
    let mx = mean vals
        s  = standardDev vals
        sample = fromIntegral((length vals) - 1)
    in (mx - testmean) / (s / sqrt sample)

tstTest :: (Fractional a, Floating a) => [a] -> [a] -> a
tstTest xvals yvals =
    let d  = [a - b | (a, b) <- zip xvals yvals]
        ds = sum d
    in  ds / ((standardDev d) / sqrt (fromIntegral(length xvals)))



