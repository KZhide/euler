import Primes
import Data.List
import Data.Function

rad = product.fmap fst.primeFactors

radPair n = (n, rad n)

main = print $ (sortBy (compare `on` snd) $ fmap radPair [1..100000]) !! 9999
