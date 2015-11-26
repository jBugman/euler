module Shared where

factors :: Int -> [Int]
factors 1 = [1]
factors n = factors' n 2
  where
    factors' 1 _ = []
    factors' m x
      | m `mod` x == 0 = x : factors' (m `div` x) x
      | otherwise      = factors' m (x + 1)