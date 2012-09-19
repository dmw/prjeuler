-- Project Euler
--
-- Problem 010
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--
-- Usage:
-- $ ./problem009 2000000
-- <Result Here>

module Main where
import System.Environment (getArgs)

findSum :: Integer -> Integer
findSum limit = sum $ primes $ limit

primes :: Integer -> [Integer]
primes n = sieve [2..n]

sieve :: [Integer] -> [Integer]
sieve []     = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = do i <- getArgs
          print (findSum $ read $ head $ i)
