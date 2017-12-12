import Digits
import Primes

harshed n = n `mod` sum (digits n) == 0

rTruncHarshNums = foldr1 (++) (fmap f [1..])
  where
    f 1 = [1..9]
    f digs = do
      h <- f (digs - 1)
      filter harshed $ fmap (h*10+) [0..9]

strongRTruncHarshNums = filter (\n -> isPrime $ n `div` sum (digits n)) rTruncHarshNums

strongRightTruncatableHarshedPrimes = do
  h <- strongRTruncHarshNums
  filter isPrime $ fmap (10*h+) [0..9]

main = print $ sum $ takeWhile (<10^14) strongRightTruncatableHarshedPrimes
