module Dirichlet
( dirichletPDF
) where

import Beta

dirichletPDF :: [Float] -> [Float] -> Float
dirichletPDF xs alphas = (1 / (multinomialBeta alphas)) * (product [x**(alpha - 1) | (x, alpha) <- zip xs alphas])
