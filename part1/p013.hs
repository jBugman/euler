module Main where

import Data.List.Split (chunksOf)
import Data.List (transpose)
import Shared (readLines)

main :: IO ()
main = do
  numbers <- readLines "p013.txt"
  print $ take 10 $ summ numbers

summ :: [String] -> String
summ numbers = concatMap show $ carry [] sumDigits 0 where
  sumDigits = map sum digits
  digits = map toDigits $ transpose ns
  ns = map reverse numbers

carry :: [Int] -> [Int] -> Int -> [Int]
carry rs [] 0 = rs
carry rs [] c = c : rs
carry rs (d:ds) c = carry (mod (d+c) 10 : rs) ds (div (d+c) 10)

toDigits :: String -> [Int]
toDigits s = map read $ chunksOf 1 s
