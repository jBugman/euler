module Main where

import Data.List (find)

main :: IO ()
main = print $ solution 20

divisableUpTo :: Int -> Int -> Bool
divisableUpTo x y = all (\z -> y `mod` z == 0) [2..x]

solution :: Int -> Maybe Int
solution n = find (divisableUpTo n) [1..]
