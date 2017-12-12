import Primes
import Digits

concatNum = toNum . concatMap digits

concatPrime m n = isPrime (concatNum [m,n]) && isPrime (concatNum [n,m])

main = print $ take 5 $ filter (concatPrime 701) $ filter (concatPrime 3) $ filter (concatPrime 11) primes
