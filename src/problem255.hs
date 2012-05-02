----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  https://github.com/dmw/prjeuler
-- Repository  :  https://github.com/dmw/prjeuler
--
-- Project Euler, Problem 255.
-- http://projecteuler.net/problem=255
-----------------------------------------------------------------------------


module Main (main) where


import Data.List
import System.Environment


-- | Returns the number of digits of the given Integer.
numDigits :: Integer            -- ^ Integer to calculate.
             -> Integer         -- ^ Number of digits.
numDigits n = round (logBase 10 $ abs $ fromIntegral n) + 1

-- | Base number of the Heron Operation.
digitBase :: Integer            -- ^ Number as base.
             -> Integer         -- ^ Returning Operation.
digitBase n | odd n = round ((2 * 10) ** ((fromIntegral n - 1) / 2))
            | otherwise = round ((7 * 10) ** ((fromIntegral n - 2) / 2))

-- | Applies the Heron Operation recursively until it returns
-- the round square root and the number of iterations as tuple.
heronOp :: Integer                 -- ^ Heron Operation to Apply
           -> (Integer, Integer)   -- ^ Pair (RSR, Iterations)
heronOp a = heronOp a a (digitBase $ numDigits a) 0
  where heronOp o n m a
          | o == 0 = (0, 0)
          | o == 1 = (1, 1)
          | o == 2 = (1, 1)
          | o == 3 = (2, 1)
          | n == m = (m, a)
          | otherwise = let x = fromIntegral m
                            y = fromIntegral o / x
                            s = round ((x + y) / 2)
                            t = a + 1
                            in heronOp o m s t

-- | Counts the number of Heron Operations in a range of numbers
-- using a guarded recursion to avoid memory leaks.
heronCounter :: Integer            -- ^ Initial Number
                -> Integer         -- ^ Last Number
                -> Integer         -- ^ Number of Operations
heronCounter a b = sHeronCount a b a 0
  where sHeronCount x y z v
          | y == z = v
          | otherwise = let r = z + 1
                            s = (v + (snd $ heronOp z))
                            in sHeronCount x y r s

-- | Calculates the average of Heron Operations in a range
-- of numbers.
heronAvgCalc :: Integer            -- ^ Range lower bound.
                -> Integer         -- ^ Range Upper bound.
                -> Double          -- ^ Average of Heron Operations.
heronAvgCalc x y | x < y = let r = fromIntegral x
                               s = fromIntegral y
                               t = fromIntegral $ heronCounter x y
                               in t / (s - r)
                 | x > y = let r = fromIntegral x
                               s = fromIntegral y
                               t = fromIntegral $ heronCounter x y
                               in t / (r - s)
                 | otherwise = 0.0


-- | Main Function. Reads the lower and upper bounds of the
-- range.
main :: IO ()
main = do [n, m] <- getArgs
          let a = read n
              b = read m
              in print $ heronAvgCalc a b

