module DTS.InferenceAnalyzer(
    volume
)where

import qualified DTS.NaturalLanguageInference as NLI

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

main :: IO ()
main = 
    print $ volume 1000.00