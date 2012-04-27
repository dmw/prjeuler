

module Main (main) where


import Data.List ()
import System.Environment ()
import Prelude


numDigits :: Int -> Int
numDigits n = round (logBase 10 $ abs $ fromIntegral n) + 1

digitBase :: Int -> Int
digitBase n | odd n = round ((2 * 10) ** ((fromIntegral n - 1) / 2))
            | otherwise = round ((7 * 10) ** ((fromIntegral n - 2) / 2))

applyHeronOp :: Int -> Int -> Int -> Int -> (Int, Int)
applyHeronOp = heronOp
               where
                 heronOp :: Int -> Int -> Int -> Int -> (Int, Int)
                 heronOp o n m a | n == m = m `seq` a `seq` (m, a)
                                 | otherwise = s `seq` t `seq` heronOp o m s t
                   where x = fromIntegral m
                         y = fromIntegral o / x
                         s = round ((x + y) / 2)
                         t = a + 1

applyHeron :: Int -> (Int, Int)
applyHeron n = applyHeronOp n n (digitBase $ numDigits n) 0

applyHeronRec :: Float -> Float -> Float -> Float
applyHeronRec = heronRec
                where
                  heronRec :: Float -> Float -> Float -> Float
                  heronRec n m r | m >= r = n
                                 | otherwise = b `seq` c `seq` heronRec b c r
                    where a = fromIntegral $ snd $ applyHeron $ round m
                          b = n + a
                          c = m + 1

heronCalc :: Float -> Float -> Float
heronCalc x y = z / (y - x)
                where z = applyHeronRec 0.0 x y

main :: IO ()
main = print $ heronCalc 1.0000e+13 1.0000e+14
