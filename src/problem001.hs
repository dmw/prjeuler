----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  https://github.com/dmw/prjeuler
-- Repository  :  https://github.com/dmw/prjeuler
--
-- Project Euler, Problem 001.
--
-- If we list all the natural numbers below 10 that are multiples
-- of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.
-----------------------------------------------------------------------------


module Main (main) where

import System.Environment (getArgs)

-- | Checks if the given list xs can be divided by the given
-- integer n.
canApply :: [Int]       -- ^ List to Check.
            -> Int      -- ^ Number to be used.
            -> Bool     -- ^ The Returning Check
canApply xs n = 0 < length (filter (\ x -> n `mod` x == 0 ) xs)

-- | Checks the sum of the given list xs filtered with canApply
-- for all numbers between 1 and n.
sumBelow :: Int         -- ^ The number as limit.
            -> [Int]    -- ^ The list to sum.
            -> Int      -- ^ The sum result.
sumBelow n xs = sum (filter (canApply xs) [1..n])

-- | The Main Function. It reads the first argument as Integer
-- and uses it as limit, the remaining arguments are the numbers
-- to be used as list of valid divisors to check.
main :: IO ()
main = do r <- getArgs
          print $ sumBelow (read $ head r) (fmap read $ tail r)
