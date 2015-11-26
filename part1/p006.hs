module Main where

main :: IO ()
main = print $ squareOfSum n - sumOfSquares n where n = 100

squareOfSum :: Int -> Int
squareOfSum n = m * m where m = sum [1..n]

sumOfSquares :: Int -> Int
sumOfSquares n = sum [x * x | x <- [1..n]]
