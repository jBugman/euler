module Main where

main :: IO ()
main = print $ solution 10001

solution :: Int -> Int
solution n = head $ drop n primes

primes :: [Int]
primes = filter (\x -> length (factors x) == 1) [1..]

factors :: Int -> [Int]
factors 1 = [1]
factors n = factors' n 2
  where
    factors' 1 _ = []
    factors' m x
      | m `mod` x == 0 = x : factors' (m `div` x) x
      | otherwise      = factors' m (x + 1)
