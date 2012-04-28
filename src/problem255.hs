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


import Data.List ()
import System.Environment ()
import Prelude

-- | Returns the number of digits of the given Integer.
numDigits :: Int                -- ^ Integer to calculate.
             -> Int             -- ^ Number of digits.
numDigits n = round (logBase 10 $ abs $ fromIntegral n) + 1

-- | Base number of the Heron Operation.
digitBase :: Int                -- ^ Number as base.
             -> Int             -- ^ Returning Operation.
digitBase n | odd n = round ((2 * 10) ** ((fromIntegral n - 1) / 2))
            | otherwise = round ((7 * 10) ** ((fromIntegral n - 2) / 2))

-- | Applies the Heron Operation recursively until it returns
-- the round square root and the number of iterations as tuple.
applyHeronOp :: Int             -- ^ Number to calculate.
                -> Int          -- ^ Context Recursive First Result.
                -> Int          -- ^ Context Recursive Second Result.
                -> Int          -- ^ Context Recursive Iterations.
                -> (Int, Int)   -- ^ Resulting (RSR, No. Iterations).
applyHeronOp = heronOp
               where
                 heronOp :: Int -> Int -> Int -> Int -> (Int, Int)
                 heronOp o n m a | n == m = m `seq` a `seq` (m, a)
                                 | otherwise = s `seq` t `seq`
                                               heronOp o m s t
                   where x = fromIntegral m
                         y = fromIntegral o / x
                         s = round ((x + y) / 2)
                         t = a + 1

-- | Applies the Heron Operation on the given number n
applyHeron :: Int               -- ^ Number N.
              -> (Int, Int)     -- ^ Tuple (RSR, No. Iterations).
applyHeron n = applyHeronOp n n (digitBase $ numDigits n) 0

-- | Applies the Heron Opeartion recursively in a range of
-- numbers accumalting the sum of the Heron Operation on n
-- using the context counter m and limit r.
applyHeronRec :: Float          -- ^ Accumulator.
                 -> Float       -- ^ Counter.
                 -> Float       -- ^ Limit.
                 -> Float       -- ^ Returning Sum.
applyHeronRec = heronRec
                where
                  heronRec :: Float -> Float -> Float -> Float
                  heronRec n m r | m >= r = n
                                 | otherwise = b `seq` c `seq`
                                               heronRec b c r
                    where a = fromIntegral $ snd $ applyHeron $ round m
                          b = n + a
                          c = m + 1

-- | Calculates the average number of Heron Operations
-- for the given range x and y.
heronCalc :: Float              -- ^ Lower bound.
             -> Float           -- ^ Upper bound.
             -> Float           -- ^ Average.
heronCalc x y = z / (y - x)
                where z = applyHeronRec 0.0 x y

-- | Main Function.
main :: IO ()
main = print $ heronCalc 1.0000e+13 1.0000e+14
