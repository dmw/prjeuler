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


import Data.Bits
import Data.Digits
import Data.List
import System.Environment


-- | Calculates the Fibonacci Term N
fib :: Int                      -- ^ Term to calculate.
       -> Integer               -- ^ Fibonacci Term as Result.
fib n = snd . foldl' fib' (1, 0) . dropWhile not $
        [testBit n k | k <- let s = bitSize n in [s - 1, s - 2 .. 0]]
  where
    fib' (f, g) p
      | p         = (f * (f + 2 * g), ss)
      | otherwise = (ss, g * (2 * f - g))
      where ss = f * f + g * g

-- | Checks if the given number x have valid last n digits
-- and last n digits
isValidDig :: Integer           -- ^ Number to Check.
              -> [Integer]      -- ^ Sequence to Check.
              -> Bool           -- ^ Is valid or not.
isValidDig x xs
  | length (digits 10 x) < length xs = False
  | otherwise = let ds = digits 10 x
                    l = length xs
                    s = sort xs
                    r = sort $ take l ds
                    t = sort $ take l $ reverse ds
                in r == s && t == s

-- | Checks sequentially if the given range covers the
-- problem of pandigital sequence of digits using the reqDigs
-- sequence.
isValidPan :: Int               -- ^ Number to check.
              -> Bool           -- ^ True when is valid.
isValidPan x = r `seq` isValidDig r [1..9]
               where r = fib x

-- | Checks sequentially if the given range covers the
-- problem of pandigital problem.
checkRange :: Int                   -- ^ Starting number.
              -> Int                -- ^ End number
              -> Int                -- ^ Returning Number (-1 on failure).
checkRange m n = sCheckRange m n m
  where sCheckRange x y z
          | z >= y = -1
          | isValidPan z = z
          | otherwise = let r = z + 1
                        in r `seq` sCheckRange x y r

-- | Reads two arguments, x and y as range, where x should be
-- less than y and uses that range to check if it finds the
-- required panadigital number.
main :: IO ()
main = do [x, y] <- getArgs
          print $ checkRange (read x) (read y)

