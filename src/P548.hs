-- if p is a prime, g p = 1.
-- if n == p^i, g n = 2^(i-1).
-- if n == p * q, g n = 3.
-- if n == p * q * r, g n = 1 + 3 + 6 + 3 = 13.
import Primes
import Data.List

neck [x] = []
neck (x:xs) = x:neck xs

g n | n == 1 = 1
    | otherwise = (sum.fmap g.neck.sort.divisors) n

main = print $ g 30
