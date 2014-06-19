gammaFunc :: Float -> Int -> Float
gammaFunc t n =
    (exp(-gamma*t)/t) * product [(1 + (t/(fromIntegral ns)))**(-1) * (exp(t/(fromIntegral ns))) | ns <- [1..n]]
    where gamma = 0.577216
