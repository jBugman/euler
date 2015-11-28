module Main where

main :: IO ()
main = print $ solution (20 :: Integer)

-- Solition is the middle number in 2Nth row of Pascals' triangle by definition
solution :: Integral a => a -> a
solution x = nCr n r where
  r = (n + 1) `div` 2
  n = x * 2

-- nCr = n! / (r! * (n - r)!)
nCr :: Integral a => a -> a -> a
nCr n r = product [1..n] `div` (product [1..r] * product[1..(n - r)])
