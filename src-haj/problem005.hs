-- Project Euler
--
-- Problem 005
--
-- 2520 is the smallest number that can be divided by each of the numbers
-- from 1 to 10 without any remainder.
-- What is the smallest positive number that is evenly divisible 
-- by all of the numbers from 1 to 20?

-- Usage:
-- $ ./problem005 20
-- <Result Here>

module Main where
import System.Environment (getArgs)

isEvenlyDivisible :: Int -> Int -> Bool
isEvenlyDivisible d n = length [x | x <- [1..d], n `mod` x == 0] == d

main = do i <- getArgs
          print (head $ filter (isEvenlyDivisible $ read $ head $ i) [x | x <- [1..]])
