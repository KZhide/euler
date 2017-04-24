import Primes

check n (a,b) = isPrime (n^2 + a * n + b)
rec_filter f n l | length l <= 1 = (head l, n)
                 | otherwise = rec_filter f (n+1) (filter (f n) l)

main = print $ rec_filter check 1 [(a,b) | b <- primesUnder 999, a <- [-b..999]]

