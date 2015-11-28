module Main where

main :: IO ()
main = print solution

bigNumber :: Integer
bigNumber = 2 ^ (1000 :: Integer)

solution :: Integer
solution = trivialSolution

trivialSolution :: Integer
trivialSolution = sumDigits 0 bigNumber where
  sumDigits summ num
    | num < 10  = summ + num
    | otherwise = sumDigits (summ + num `mod` 10) (num `div` 10)
