----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  https://github.com/dmw/prjeuler
-- Repository  :  https://github.com/dmw/prjeuler
--
-- Project Euler, Problem 104.
--
-- http://projecteuler.net/problem=104
--
-- The Fibonacci sequence is defined by the recurrence relation:
--
-- Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
--
-- It turns out that F541, which contains 113 digits, is the first
-- Fibonacci number for which the last nine digits are 1-9 pandigital
-- (contain all the digits 1 to 9, but not necessarily in order).
-- And F2749, which contains 575 digits, is the first Fibonacci
-- number for which the first nine digits are 1-9 pandigital.
--
-- Given that Fk is the first Fibonacci number for which the first
-- nine digits AND the last nine digits are 1-9 pandigital, find k.
-----------------------------------------------------------------------------


module Main (main) where


import Data.Digits (digits)
import Data.List (sort)
import System.Environment (getArgs)


-- | Calculates the Fibonacci Term N using the Binet's Formula.
fib :: Int              -- ^ Term Number to Calculate.
       -> Int           -- ^ Returning Fibonacci term.
fib n = round $ phi ** fromIntegral n / sq5
        where sq5 = sqrt 5 :: Double
              phi = (1 + sq5) / 2

-- | Colection of required digits
reqDigs :: [Int]        -- ^ Required Sequence.
reqDigs = [1..9]

-- | Checks if the given number x have valid last n digits
-- and last n digits
isValidDig :: Int               -- ^ Number to Check.
              -> [Int]          -- ^ Sequence to Check.
              -> Bool           -- ^ Is valid or not.
isValidDig x xs | length (digits 10 x) < length xs = False
                | otherwise = r == s && t == s
                              where ds = digits 10 x
                                    l = length xs
                                    r = sort $ take l ds
                                    s = sort $ take l xs
                                    t = sort $ reverse $ take l ds

-- | Checks sequentially if the given range covers the
-- problem of pandigital sequence of digits using the reqDigs
-- sequence.
isValidPan :: Int               -- ^ Number to check.
              -> Bool           -- ^ True when is valid.
isValidPan x = r `seq` isValidDig r [1..9]
               where r = fib x

-- | Checks sequentially if the given range covers the
-- problem of pandigital problem.
checkRange :: Int               -- ^ Starting number.
              -> Int            -- ^ End number
              -> Int            -- ^ Returning Number (-1 on failure).
checkRange m n = sCheckRange m n m
  where sCheckRange x y z | z >= y = z
                          | isValidPan z = (fib z)
                          | otherwise = r `seq` sCheckRange x y r
                                        where r = z + 1

-- | Reads two arguments, x and y as range, where x should be
-- less than y and uses that range to check if it finds the
-- required panadigital number.
main :: IO ()
main = do [x, y] <- getArgs
          print $ checkRange (read x) (read y)
