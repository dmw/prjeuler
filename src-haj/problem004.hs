-- Project Euler
--
-- Problem 004
--
-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 99.
-- Find the largest palindrome made from the product of two 3-digit numbers.

-- Usage:
-- $ ./problem004 3
-- Where you are giving as argument the number of digits of your palindrome
-- <Result Here>

module Main where
import System.Environment (getArgs)

findLargestPalindrome :: Integer -> Integer
findLargestPalindrome x = fLPAux (10 ^ x - 1) (10 ^ x - 1) x

fLPAux :: Integer -> Integer -> Integer -> Integer
fLPAux x 0 n = fLPAux (x - 1) (10 ^ n - 1) n
fLPAux 0 y n = 0
fLPAux x y n
    | isPalindrome $ show $ z   = z
    | otherwise                 = fLPAux x (y - 1) n
    where z = x * y

isPalindrome :: String -> Bool
isPalindrome xs = xs == reverse xs

main = do x <- getArgs       
          print ( findLargestPalindrome $ read $ head x)
