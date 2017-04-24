module P041 where

import Primes
import Digits
import Data.List
import Data.List.Ordered

ds = [1..9]
subsets [] = [[]]
subsets (x:xs) = do
  ss <- subsets xs
  [ss, x:ss]

prefixes l = fmap (`take` l) [1..length l]

divisible m n = m `mod` n == 0

pandigital7 = (sortBy (flip compare) . fmap toNum . filter ((/=5) . head) . filter (odd . head) . permutations) [1..7] 

showResult = print $ find isPrime pandigital7
