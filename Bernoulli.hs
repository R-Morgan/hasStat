module Bernoulli
( bernoulliPMF
) where

bernoulliPMF :: Int -> Float -> Float
bernoulliPMF k p = p^k * (1-p)^(1-k) 

