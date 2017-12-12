import Primes
import Data.List

primeFourths = fmap (^4) primes
primeCubes = fmap (^3) primes
primeSquares = fmap (^2) primes

cands = do
  f <- takeWhile (<50000000) primeFourths
  t <- takeWhile (<50000000-f) primeCubes
  s <- takeWhile (<50000000-f-t) primeSquares
  return (f+t+s)

main = print $ length $ group $ sort cands
