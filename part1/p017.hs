module Main where

import Data.Char (isLetter)

main :: IO ()
main = print solution

solution :: Int
solution = length $ filter isLetter $ concatMap spell [1..1000]

spell :: Int -> String
spell 1  = "one"
spell 2  = "two"
spell 3  = "three"
spell 4  = "four"
spell 5  = "five"
spell 6  = "six"
spell 7  = "seven"
spell 8  = "eight"
spell 9  = "nine"
spell 10 = "ten"
spell 11 = "eleven"
spell 12 = "twelve"
spell 13 = "thirteen"
spell 14 = "fourteen"
spell 15 = "fifteen"
spell 16 = "sixteen"
spell 17 = "seventeen"
spell 18 = "eighteen"
spell 19 = "nineteen"
spell 20 = "twenty"
spell 30 = "thirty"
spell 40 = "forty"
spell 50 = "fifty"
spell 60 = "sixty"
spell 70 = "seventy"
spell 80 = "eighty"
spell 90 = "ninety"
spell x
  | x `mod` 1000 == 0 = spell (x `div` 1000) ++ " thousand"
  | x `mod` 100 == 0  = spell (x `div` 100) ++ " hundred"
  | x < 100           = spell (x `div` 10 * 10) ++ "-" ++ spell (x `mod` 10)
  | x < 1000          = spell (x `div` 100) ++ " hundred and " ++ spell (x `mod` 100)
  | otherwise         = spell (x `div` 1000) ++ " thousand, " ++ spell (x `mod` 1000)
