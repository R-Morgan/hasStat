module Beta
( betaFunc
, betaPDF
, incompleteBeta
--, betaCDF
, multinomialBeta
) where 

import Gamma
import Factorials

-- Type declaration for BetaParams (alpha and beta). Not sure if this is the
-- best use of the typing system or if it really gets me anything.
-- 
-- Parameters:
--            alpha : continuous shape parameter on interval (0,1)
--            beta  : continuous shape parameter 
data BetaParams = BetaParams Float Float deriving(Show)

-- Beta function implemented via ratio of gamma functions. Used in computation
-- of the Beta Distribution's PDF.
betaFunc :: BetaParams -> Float
betaFunc (BetaParams alpha beta) = 
    ((gammaFunc alpha 10000) * (gammaFunc beta 10000)) / gammaFunc (alpha + beta) 10000

-- Beta  probability distribution function
-- Parameters:
--            x : Value IV at which to calculate density 
betaPDF :: Float ->  BetaParams -> Float
betaPDF x (BetaParams alpha beta) = (1/(betaFunc (BetaParams alpha beta))) * 
                                    x ** (alpha - 1) * (1-x) ** (beta - 1)

-- Incomplete beta function, used in computation of the cdf 
-- Parameters:
--            x : Value IV at which to calculate cumulative density, given
--                BetaParams
incompleteBeta :: Float -> BetaParams -> Float
incompleteBeta x (BetaParams alpha beta) =
    constant * sum [(pocahammer (1 - beta) n) / ((factorial n) * (alpha + n)) * 
                    x ** n | n <- [0..20]] where constant  = (x ** alpha)

-- BetaCDF or Regularized incomplete beta function.
-- Parameters:
--            x : Value IV at which to calculate cumulative density, given
--                BetaParams

betaCDF :: Float -> BetaParams -> Float
betaCDF x (BetaParams alpha beta) = numerator / denominator
    where numerator   = incompleteBeta x (BetaParams alpha beta) 
          denominator = betaFunc (BetaParams alpha beta)    

-- Multinomial Beta function
-- Parameters:
--            x: 
multinomialBeta :: [Float] -> Float
multinomialBeta x = numerator / denominator
    where numerator   = product [gammaFunc alpha 10000 | alpha <- x] 
          denominator = gammaFunc (sum [alpha | alpha <- x]) 10000
