module Main where

import Shared (factors)

main :: IO ()
main = print $ solution 10001

solution :: Int -> Int
solution n = head $ drop n primes

primes :: [Int]
primes = filter (\x -> length (factors x) == 1) [1..]
