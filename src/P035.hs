import Digits
import Control.Monad
import Primes

ds n = replicateM n [1,2,3,5,7,9]
cycles l = fmap (\n -> drop n l ++ take n l) [0..length l]
circularPrime = all(isPrime.toNum).cycles

main = (print.length.filter circularPrime.concatMap ds) [1..6]

--main = print $ (fmap toNum.filter circularPrime.concatMap ds) [1..2]
