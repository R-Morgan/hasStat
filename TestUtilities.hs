module TestUtilities
( sequencer 
, tmpA
, tmpB
) where

sequencer :: Int -> Int -> [Float] 
sequencer n k = [(fromIntegral x) / (fromIntegral k) | x <- [0..n]]

tmpA :: (Num a, Floating a) => [a]
tmpA = [0.05, 0.05, 0.1, 0.2, 0.15, 0.25, 0.1, 0.025, 0.025, 0.05] -- proportion of 'words'

tmpB :: (Num a, Floating a) => [a]
tmpB = [a * b | (a, b) <- zip tmpA (replicate 10 10000)]            -- actual counts

