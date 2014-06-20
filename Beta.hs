import Gamma
import Factorials

data BetaParams = BetaParams Float Float deriving(Show)

betaFunc :: BetaParams -> Float
betaFunc (BetaParams alpha beta) = ((gammaFunc alpha 10000) * (gammaFunc beta 10000)) / gammaFunc (alpha + beta) 10000

betaPDF :: Float ->  BetaParams -> Float
betaPDF x (BetaParams alpha beta) = (1/(betaFunc (BetaParams alpha beta))) * x**(alpha - 1) * (1-x)**(beta - 1)

incompleteBeta :: Float -> BetaParams -> Float
incompleteBeta x (BetaParams alpha beta) =
   x**alpha * sum [((pocahammer (1-beta) (fromIntegral n)) / ((fromIntegral (factorial n)) * (alpha + (fromIntegral n)))) * x^n | n <- [0..20]] 


