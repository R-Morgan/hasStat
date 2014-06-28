module StatUtils
(dropKth
) where

dropKth :: Int -> [a] -> [a]
dropKth 0 xs = xs
dropKth 1 xs = tail xs
dropKth n xs = let (a, b) = splitAt n xs
               in init a ++ b
