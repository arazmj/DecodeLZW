-- Area of each rectangle in the standart input strem
module Main where

import Text.Printf

main :: IO()
main = interact (showResults . map area . readTestCases)

readTestCases :: String -> [(Double,Double,Double,Double)]
readTestCases = pairUp . (map read) . words

pairUp :: [Double] -> [(Double,Double,Double,Double)]

pairUp (0.0:0.0:0.0:0.0:_) = []
pairUp (n:m:o:p:rest) = (n,m,o,p) : (pairUp rest)
pairUp (_) = []

showResults ::  [Double] -> String
showResults = unlines . (map format)

format :: Double -> String
format a = printf "%.2f" a

area :: (Double, Double, Double, Double) -> Double
area (x,y,z,w) = abs ((x-z) * (y-w))
