import Gamma
import Factorials

data BetaParams = BetaParams Float Float deriving(Show)

betaFunc :: BetaParams -> Float
betaFunc (BetaParams alpha beta) = ((gammaFunc alpha 10000) * (gammaFunc beta 10000)) / gammaFunc (alpha + beta) 10000

