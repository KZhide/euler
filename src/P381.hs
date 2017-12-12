import Primes
import System.Environment

s :: Integer -> Integer
s p = mpow p 24 (p-2) * (-9) `mod` p
  where
    mpow p n i | i == 0 = 1
               | even i = mpow p n (i`div`2) ^ 2 `mod` p
               | odd i = n * mpow p n (i-1) `mod` p

main = do
  args <- getArgs
  let n = read $ head args
  print $ sum $ fmap s $ dropWhile (<5) $ takeWhile (<=n) primes
