import Primes
import Data.List

divideBy [] n = 0
divideBy (x:xs) n | n < x = 0
                  | n == x = 1
                  | otherwise = divideBy (x:xs) (n-x) + divideBy xs n

main = print $ find ((>5000).divideBy primes) [1..]
