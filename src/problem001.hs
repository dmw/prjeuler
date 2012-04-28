

module Main (main) where

import System.Environment (getArgs)

canApply :: [Int] -> Int -> Bool
canApply xs n = 0 < length (filter (\ x -> n `mod` x == 0 ) xs)

sumBelow :: Int -> [Int] -> Int
sumBelow n xs = sum (filter (canApply xs) [1..n])

main :: IO ()
main = do r <- getArgs
          print $ sumBelow (read $ head r) (fmap read $ tail r)
