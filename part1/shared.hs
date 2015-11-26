module Shared where

factors :: Int -> [Int]
factors 1 = [1]
factors n = factors' n 2
  where
    factors' 1 _ = []
    factors' m x
      | m `mod` x == 0 = x : factors' (m `div` x) x
      | otherwise      = factors' m (x + 1)

primes :: [Int]
primes = [2] ++ filter primal [3..]

primal :: Int -> Bool
primal x = x `mod` 2 == 1 && not (any (\n -> x `mod` n == 0) [3,5..(intSqrt x)])

intSqrt :: Int -> Int
intSqrt x = floor . sqrt $ (fromIntegral x :: Float)
