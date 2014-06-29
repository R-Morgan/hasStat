module StatUtils
(dropKth
) where

dropKth :: Int -> [a] -> [a]
dropKth 0 xs = xs
dropKth 1 xs = tail xs
dropKth n xs = let (a, b) = splitAt n xs
               in init a ++ b
               
doAndGlue :: Int -> ([a] -> a) ->  [a] -> [a]
doAndGlue 0 f xs = xs
doAndGlue 1 f xs = hd ++ [f hd] where hd = init xs
-- doAndGlue n f xs = 
-- ++ t where t = tail xs
