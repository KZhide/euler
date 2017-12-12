import Primes
import System.Environment

m n p q = 
  maximum $ 0 : [ p^i * q^j | i <- [1..floor (logBase (fromIntegral p) (fromIntegral n))],
              j <- [floor (logBase (fromIntegral q) (fromIntegral (n `div` p^i)))],
              j >= 1
  ]

priPairs n = [(p, q) | p <- takeWhile (< intSqrt n) primes, q <- dropWhile (<=p) $ takeWhile (<= n`div`p) primes]

s n = sum $ fmap (uncurry (m n)) $ priPairs n

main = do
  args <- getArgs
  let n = read $ head args
  print $ s n
