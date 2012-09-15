-- Project Euler
--
-- Problem 007
--
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13,
-- we can see that the 6th prime is 13.
--
-- What is the 10001st prime number?

-- Usage:
-- $ ./problem007 10001
-- <Result Here>

module Main where
import System.Environment (getArgs)

findNthPrime :: Int -> Int
findNthPrime x = (filter (isPrime) [y | y <- [2..]]) !! x

isPrime :: Int -> Bool
isPrime x = length [y | y <- [2..x], x `mod` y == 0] == 1

main = do i <- getArgs
          print (findNthPrime $ read $ head $ i)
