module Main where

import Shared (primes)

main :: IO ()
main = print $ solution 10001

solution :: Int -> Int
solution n = head $ drop n primes
