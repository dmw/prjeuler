-- Project Euler
--
-- Problem 001
--
-- If we list all the natural numbers below 10 that are multiples of 3 or 5, 
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.

-- Usage:
-- $ ./problem001
-- Enter Value: 
-- 1000           # <-- Value entered for the problem
-- <Result Here>

module Main where

myAdd :: Int -> Int -> Int
myAdd x acc 
  |   x == 0                                                       = acc
  | ( x > 0 )  && ( ( x `mod` 3 == 0 ) ||  ( x `mod` 5 == 0 ) )    = myAdd ( x - 1 ) ( acc + x )
  | ( x > 0 )                                                      = myAdd ( x - 1 ) ( acc  )
  |   x <  0                                                       = 0

main = do putStrLn "Enter Value: "
          x <- readLn          
          print ( myAdd ( x - 1)  0 )           