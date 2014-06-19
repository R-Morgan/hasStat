sequencer :: Int -> Int -> [Float] 
sequencer n k = [(fromIntegral x) / (fromIntegral k) | x <- [0..n]]

