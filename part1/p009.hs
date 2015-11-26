module Main where

main :: IO ()
main = print solution

type ReversedTriplet = (Int, Int, Int)

solution :: Int
solution = x * y * z where (x, y, z) = thetriplet 1000

thetriplet :: Int -> ReversedTriplet
thetriplet n = head $ filter pythagorean (possibleAddends n)

possibleAddends :: Int -> [ReversedTriplet]
possibleAddends n = (concat . concat) [[[(x, y, z) | x <- [y..n], x + y + z == n] | y <- [z..n]] | z <- [1..n]]

pythagorean :: ReversedTriplet -> Bool
pythagorean (z, y, x) = x * x + y * y == z * z
