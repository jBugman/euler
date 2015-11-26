module Main where

import Shared (primes)

main :: IO ()
main = print $ sum $ takeWhile (< 2000000) primes
