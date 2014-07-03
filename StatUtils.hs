module StatUtils
( dropKth
, doAndGlue
, lcIntGen
, lcIterIntGen
, lcValGen
, lcGen
) where

dropKth :: Int -> [a] -> [a]
dropKth 0 xs = xs
dropKth 1 xs = tail xs
dropKth n xs = let (a, b) = splitAt n xs
               in init a ++ b
               
doAndGlue :: Int -> ([a] -> a) ->  [a] -> [a]
doAndGlue _ _ [] = []
doAndGlue 1 f xs = let tmpList = tail xs
                       tmpVal  = f tmpList -- scalar
                       reRunList = [tmpVal] ++ tmpList
                   in  doAndGlue (1 + 1) f reRunList
doAndGlue n f xs
    | n > 1 && n < length xs = let tmpList = dropKth n xs
                                   tmpVal  = f tmpList
                                   (aList, bList)  = splitAt n xs
                                   reRunList =  (init aList) ++ [tmpVal] ++ bList
                               in doAndGlue (n + 1) f reRunList
    | n == length xs = init xs ++ [f $ init xs]
    | otherwise = doAndGlue (n + 1) f [] 

-- linear congruential generator
lcIntGen :: (Num a, Integral a) => a -> a 
lcIntGen seed = let m        = 2^31
                    increment  = 12345
                    multiplier = 110515245
                in  (fromIntegral $ (multiplier * seed + increment) `mod` m) 

lcIterIntGen :: (Num a, Integral a) => a -> a -> [a]
lcIterIntGen seed 0          = []
lcIterIntGen seed iterations = let tmpVal = lcIntGen seed 
                               in tmpVal : lcIterIntGen tmpVal (iterations - 1)

lcValGen :: (Num a, Floating a) => Int -> a
lcValGen seed = (fromIntegral $ lcIntGen seed) / 2**31 

lcGen :: (Num a, Floating a) => Int -> Int -> [a] 
lcGen seed n = [fromIntegral randInt / 2**31 | randInt <- lcIterIntGen seed n]

 
