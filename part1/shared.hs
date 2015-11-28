module Shared where

import qualified Data.Text    as Text
import qualified Data.Text.IO as Text

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

-- Using Binet's formula approximation: F(n) = round( Phi^n / √5 ) provided n ≥ 0
fibonacci :: Int -> Integer
fibonacci n = round $ (phi ^^ n) / sqrt5 where
  phi = 1.61803398875 :: Double
  sqrt5 = 2.236067977 :: Double

readLines :: String -> IO [String]
readLines fileName = do
  linesAsText <- fmap Text.lines $ Text.readFile fileName
  return $ map Text.unpack linesAsText
