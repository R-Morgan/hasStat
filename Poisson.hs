
poissonPMF :: Int -> Int -> Float
poissonPMF k lambda = lambda^k * exp(-lambda) / factorial k
