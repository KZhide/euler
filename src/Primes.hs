module Primes (intSqrt, minus, union, primes, primesUnder, isPrime, primeFactors, divisors, sumOfDivisors) where

import Data.List (group)
import Control.Arrow ((&&&))

minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
union (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys 
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys

primes :: [Integer]
primes = 2 : 3 : sieve' (union [7, 13..] [5,11..])
primesUnder n = takeWhile (<=n) primes
--sieve [] = []
--sieve (x:xs) = x : sieve (filter ((/= 0) . (`mod` x)) xs)

sieve' [] = []
sieve' (x:xs) =
  let ps = takeWhile (<x^2) xs in
    x : ps ++ sieve' (dropWhile (<x^2) xs `minus` foldr union [] (fmap eliminated (x : ps)))

eliminated n = [n*n, n*n + 2*n..]

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
          | otherwise = all (\p -> n `mod` p /= 0) ((primesUnder . floor . sqrt . fromIntegral) n)

intSqrt = floor.sqrt.fromIntegral

primeFactors n = (fmap (head &&& length) . group . f primes) n
  where
    f (p:ps) n | n == 1 = []
               | mod n p == 0 = p : f (p:ps) (div n p)
               | otherwise = f ps n
    f [] n = [n]

divisors = fmap product . sequence . fmap (\(p,i) -> fmap (p ^) [0..i]) . primeFactors

sumOfDivisors = product . fmap (\(p,i) -> (p ^ (i + 1) - 1) `div` (p - 1)) . primeFactors

