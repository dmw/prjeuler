-- Project Euler
--
-- Problem 004
--
-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of 
-- two 2-digit numbers is 9009 = 91 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

-- Usage:
-- $ ./problem004 3
-- Where you are giving as argument the number of digits of your palindrome
-- <Result Here>

module Main where
import System.Environment (getArgs)

findLargestPalindrome :: Integer -> Integer
findLargestPalindrome z = maximum ( 
                            filter (isPalindrome) [ x * y
                                                  | x <- reverse [100..(10^z - 1)]
                                                  , y <- reverse [100..(10^z - 1)]])

isPalindrome :: Integer -> Bool
isPalindrome x = (show x) == reverse (show x)

main = do x <- getArgs
          print ( findLargestPalindrome $ read $ head x)
