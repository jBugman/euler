module Main where

import Shared (primes)

main :: IO ()
main = print solution

solution :: Int
solution = head $ dropWhile (\x -> 500 > numDivisors x) [triangle i | i <- [1..]]

numDivisors :: Int -> Int
numDivisors x = product [p + 1 | p <- powers] where
  powers = factorAndCount x primes [] 0 where
    factorAndCount 1 _ xs _ = 1 : xs
    factorAndCount y ps xs n
      | y `mod` p == 0 = factorAndCount (y `div` p) ps xs (n + 1)
      | otherwise = n : factorAndCount y (tail ps) xs 0
      where p = head ps

triangle :: Int -> Int
triangle n = n * (n + 1) `div` 2
