-- Project Euler
--
-- Problem 009
--
-- A Pythagorean triplet is a set of three natural numbers, 
-- a < b < c, for which,
--
-- a^2 + b^2 = c^2
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.

-- Usage:
-- $ ./problem009 1000
-- <Result Here>

module Main where
import System.Environment (getArgs)

findProduct :: Integer -> Integer
findProduct p = head $ [ x * y * z | x <- [1..(p-2)], y <- [1..(p-2)]
                       , z <- [1..(p-2)], x^2 + y^2 == z^2, x + y + z == p]

main = do i <- getArgs
          print (findProduct $ read $ head $ i)
