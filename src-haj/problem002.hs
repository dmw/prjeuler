-- Project Euler
--
-- Problem 002
--
-- Each new term in the Fibonacci sequence is generated 
-- by adding the previous two terms. By starting with 1 and 2, 
-- the first 10 terms will be:
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
-- By considering the terms in the Fibonacci sequence whose values 
-- do not exceed four million, find the sum of the even-valued terms.

-- Usage:
-- $ ./problem002
-- Enter Value: 
-- 4000000           # <-- Value entered for the problem
-- <Result Here>

module Main where

myAdd :: Int -> Int -> Int -> Int -> Int -> Int
myAdd x  y  z  acc l
  |  (x >= l )             = acc
  |  (x `mod` 2 == 0)      = myAdd (x + y) x y (x + acc) l
  |  otherwise             = myAdd (x + y) x y acc       l 

main = do putStrLn "Enter Value: "
          l <- readLn          
          print ( myAdd 1 0 0 0 l)           