module StatUtils
( dropKth
, doAndGlue
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

