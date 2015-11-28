module Main where

import Data.List (maximumBy)
import Data.Ord (comparing)

main :: IO ()
main = print solution

solution :: (Int, Int)
solution = maximumBy (comparing fst) [(length $ collatzSequence i, i) | i <- [1..1000000]]

collatzSequence :: Int -> [Int]
collatzSequence = generate [] where
  generate xs 1 = xs
  generate xs x = generate (c : xs) c where c = collatz x

collatz :: Int -> Int
collatz n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise  = 3 * n + 1
