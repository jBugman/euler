module Main where

import Shared (readLines)

type Pyramid = [[Int]]

main :: IO ()
main = do
  rows <- readLines "p018.txt"
  -- let pyramid = parse ["3", "7 4", "2 4 6", "8 5 9 3"]
  let pyramid = parse rows
  print $ maximum $ reduce pyramid

parse :: [String] -> [[Int]]
parse = map (map read . words)

reduce :: Pyramid -> [Int]
reduce [] = []
reduce [[apex]] = [apex]
reduce xs = reduce ls ++ reduce rs where (ls, rs) = split xs

split :: Pyramid -> (Pyramid, Pyramid)
split ([apex]:[left, right]:rest) = ([apex + left] : map init rest, [apex + right] : map tail rest)
split _ = ([], []) -- non-reachable branch
