import Primes
import System.Environment

primeOccurance bound p | bound < p = 0
                       | otherwise = bound `div` p + primeOccurance (bound `div` p) p

subsets [] = [[]]
subsets (x:xs) = fmap (x:) (subsets xs) ++ subsets xs

unitaryDivizors n = fmap (product.(fmap $ uncurry (^))) $ subsets $ primeFactors n
unitaryDivizorSquareSum = foldr1 mmul . fmap ((`madd`1) . (`mpow`2) . uncurry mpow)

pfMult l [] = l
pfMult [] l = l
pfMult l1@((p1, i1) : pfs1) l2@((p2, i2):pfs2) | p1 == p2 = (p1, i1+i2): pfMult pfs1 pfs2
                                               | p1 > p2 = (p2, i2) : pfMult l1 pfs2
                                               | otherwise = (p1, i1) : pfMult pfs1 l2

pfFact 0 = []
pfFact n = pfMult (primeFactors n) (pfFact (n-1))

fact 0 = 1
fact n = n * fact (n-1)

modPow m _ 0 = 1
modPow m n i | even i = modPow m n (i `div` 2) ^ 2 `mod` m
             | otherwise = n * modPow m n (i-1) `mod` m
mpow = modPow 1000000009
modMult m p q = p * q `mod` m
mmul = modMult 1000000009
modAdd m p q = (p + q) `mod` m
madd = modAdd 1000000009

main = do
  n <- fmap (read.head) getArgs
  print $ unitaryDivizorSquareSum $ fmap ( \p -> (p, primeOccurance n p) ) $ primesUnder n
