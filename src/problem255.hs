

module Main (main) where

import Data.Digits (digits)
import System.Environment (getArgs)

digitBase :: Int -> Int
digitBase n | odd n = round ((2 * 10) ** ((fromIntegral n - 1) / 2))
digitBase n = round ((7 * 10) ** ((fromIntegral n - 2) / 2))

heronOp :: Int -> Int -> Int -> Int
heronOp o n m | n == m = m
heronOp o n m = heronOp o m (round ((x + y) / 2))
                where x = fromIntegral m
                      y = fromIntegral o / x

main :: IO ()
main = do [x] <- getArgs
          print
            $ heronOp (read x) (read x)
            $ digitBase $ length $ digits (read x) 10

