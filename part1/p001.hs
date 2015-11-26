module Main where

main :: IO ()
main = print $ solution 1000 3 5

solution :: Integer -> Integer -> Integer -> Integer
solution x y z = sum factors where
  factors = filter factorYZ numbers
  factorYZ n = isFactor y n || isFactor z n
  isFactor n m = mod m n == 0
  numbers = [1..(x - 1)]
