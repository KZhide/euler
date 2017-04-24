import Data.List 
import Primes
import Ordered

isAbundant = (>) <$> sumOfDivisors <*> (2*)

sieveAbundant [] = []
sieveAbundant (x:xs) | isAbundant x = let l = filter (\n->mod n x == 0) (x:xs) in l `merge` sieveAbundant (xs`minus`l)
                     | otherwise = sieveAbundant xs

evenAbundants = sieveAbundant [12,14..20161]
oddAbundants = sieveAbundant [945,947..20161]

isAbundantSum n | n < 24 = False
                | even n && n >= 48 = True
                | even n = any (\m -> (n-m) `ordElem` evenAbundants) (takeWhile (<= n `div` 2) evenAbundants)
                | odd n && n < 12 + 945 = False
                | otherwise = any (\m -> (n-m) `ordElem` evenAbundants) (takeWhile (<= n) oddAbundants)

--cands = ((([1..20161] `minus` [48,50..]) `minus` [957,960..]) `minus` [1565,1570..]) `minus` [1617,1624..]

main = print $ sum $ filter (not.isAbundantSum) [1..20161]
--main = print $ evenAbundants
