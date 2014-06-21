module Dirichlet
( dirichletPDF
) where

import Beta

dirichletPDF :: [Float] -> [Float] -> Float
dirichletPDF xs alphas = (multinomialBetaFunc alphas) * product [x**(alpha - 1) | x <- xs, alpha <- alphas] 
