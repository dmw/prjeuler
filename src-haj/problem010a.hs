-- Project Euler
--
-- Problem 010 (Alternative Solution)
--
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
-- Find the sum of all the primes below two million.
--
-- Usage:
-- $ ./problem010a 2000000
-- <Result Here>

-- ALTERNATIVE SOLUTION PARSING A LIST OF PRIMES, PREVIOUSLY
-- COMPUTED IN A TEXT FILE

module Main where
import System.Environment (getArgs)
import System.IO

findSum :: [Integer] -> Integer -> Integer
findSum xs limit = sum $ [x | x <- xs, x < limit]

getPrimeList :: String -> [Integer]
getPrimeList s = map (read) $ words s

main =  do
        handle <- openFile "../data/primes2.txt" ReadMode
        contents <- hGetContents handle
        i <- getArgs
        print $ findSum (getPrimeList $ contents) (read $ head $ i)
        hClose handle
