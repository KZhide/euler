import Primes

d n = sumOfDivisors n - n

candidates = fmap (\n -> (n, d n)) [2..10000]
amicableNumbers = fmap fst $ filter (\(n,a) -> a /= n && (a,n) `elem` candidates) candidates

main = print $ sum amicableNumbers

