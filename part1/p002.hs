module Main where

import Shared (fibonacci)

main :: IO ()
main = print solution

solution :: Integer
solution = sum $ takeWhile (<4000000) $ filter (\n -> n `mod` 2 == 0) $ map fibonacci [2..]
