-- Project Euler
--
-- Problem 003
--
-- The prime factors of 13195 are 5, 7, 13 and 29.
-- What is the largest prime factor of the number 600851475143 ?

-- Usage:
-- $ ./problem003
-- Enter Value: 
-- 600851475143           # <-- Value entered for the problem
-- <Result Here>

module Main where

findLargestPrimeFactor :: Integer -> Integer
findLargestPrimeFactor x
  | (x <= 1)   = error "There's no prime factor for this numbers!"
  | otherwise  = findLargestPrimeFactorAux 2 x 

findLargestPrimeFactorAux :: Integer -> Integer -> Integer
findLargestPrimeFactorAux x y
  | ( y == x )                        = y
  | ( rem y x == 0 ) && (isPrime x)   = div y x
  | otherwise                         = findLargestPrimeFactorAux (x + 1) y

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


main = do putStrLn "Enter Value: "
          x <- readLn          
          print ( findLargestPrimeFactor x)           