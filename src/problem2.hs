

module Main (main) where

import System.Environment (getArgs)

fib :: Int -> Int
fib n = round $ phi ** fromIntegral n / sq5
        where sq5 = sqrt 5 :: Double
              phi = (1 + sq5) / 2

fibEvenSum :: Int -> Int -> Int -> Int
fibEvenSum n m r | fib n >= m = r
                 | even $ fib n = n `seq` r `seq`
                                  fibEvenSum (n + 1) m (r + fib n)
                 | otherwise = n `seq` r `seq`
                               fibEvenSum (n + 1) m r

main :: IO ()
main = do [x] <- getArgs
          print $ fibEvenSum 1 (read x) 0

