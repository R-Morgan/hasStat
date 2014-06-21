module Bernoulli
( bernoulliPMF
) where

bernoulliPMF :: Int -> Float -> Float
bernoulliPMF k p = p^k * (1-p)^(1-k) 

--Function to calculate product of probabilities for each document which
--are each labeled either 0 or 1. Elements of the list comprehension are
--the probability of the element of the list being either 1 or 0, given
--p, which is the probability of getting a 1 or 0

bernoulliProd :: [Int] -> Float -> Float
bernoulliProd ks p = product [bernoulliPMF k p | k <- ks] 
