-- Project Euler
--
-- Problem 003
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

-- Usage:
-- $ ./problem003 600851475143 
-- Where you giving as argument the number you want to evaluate
-- <Result Here>

module Main where
import System.Environment (getArgs)

findLargestPrimeFactor :: Integer -> Integer
findLargestPrimeFactor x
  | (x <= 1)   = error "There's no prime factor for this numbers!"
  | otherwise  = fLPFAux 2 x 

fLPFAux :: Integer -> Integer -> Integer
fLPFAux x y
  | ( y == x )                                  = y
  | ( rem y x == 0 ) && (isPrime $ div y x )    = div y x
  | otherwise                                   = fLPFAux (x + 1) y

isPrime :: Integer -> Bool
isPrime x
  | (x < 2)   = False
  | otherwise = isPrimeAux 2 x

isPrimeAux :: Integer -> Integer -> Bool
isPrimeAux x y
  | ( x > sqrt' y)                  = True
  | (rem y x == 0)                  = False
  | otherwise                       = isPrimeAux (x + 1) y

sqrt' :: Integer -> Integer
sqrt' = floor . sqrt . fromIntegral

main = do x <- getArgs       
          print ( findLargestPrimeFactor $ read $ head x)           