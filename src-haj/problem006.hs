-- Project Euler
--
-- Problem 006
--
-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 102 = 385
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)2 = 55^2 = 3025
-- Hence the difference between the sum of the squares of the 
-- first ten natural numbers and the square of the sum is 3025 - 385 = 2640.
--
-- Find the difference between the sum of the squares of the 
-- first one hundred natural numbers and the square of the sum.

-- Usage:
-- $ ./problem006 100
-- <Result Here>

module Main where
import System.Environment (getArgs)

findDifference :: Int -> Int
findDifference n = (sum [x | x <- [1..n]]) ^ 2 - sum [x * x | x <- [1..n]]


main = do i <- getArgs
          print (findDifference $ read $ head $ i)
