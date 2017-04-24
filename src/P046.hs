import Primes
import Data.List




isGoldbach n = not (isPrime n) && any (\p -> isPrime (n - 2 * p ^ 2)) [1..floor (2 * sqrt (fromIntegral n))]

main = print $ find (not . isGoldbach) (filter (not . isPrime) [3,5..])
