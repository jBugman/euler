module Main where

import Data.List (find)

main :: IO ()
main = print $ find productOfThreeDigits palindromes

palindrome :: Int -> Int -> Int -> Int
palindrome x y z = 100000 * x + 10000 * y + 1000 * z + 100 * z + 10 * y + x

palindromes :: [Int]
palindromes = [palindrome x y z | x <- [9,8..1], y <- [9,8..0], z <- [9,8..0]]

productOfThreeDigits :: Int -> Bool
productOfThreeDigits x = any (\(n, m) -> n * m == x) [(n, m) | n <- [999,998..100], m <- [999,998..100]]
