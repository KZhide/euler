import Digits
import Primes
import Data.List
import Data.Ratio
import Data.Function

relPrim m n = gcd m n == 1

phi :: Integer -> Integer
phi n =
  let pfs = fmap fst (primeFactors n) in
  n * product (fmap (subtract 1) pfs) `div` product pfs

phiRatio n =
  let pfs = fmap fst (primeFactors n) in
  product (fmap (subtract 1) pfs) % product pfs

cond n = (sort.digits) n == (sort.digits.phi) n

maximumProductUnder n (x:xs) | x > n = 1
                             | otherwise = x * maximumProductUnder (n`div`x) xs

main = print $ maximumProductUnder 1000000 primes
--main = do
--  content <- readFile "../tmp/P070_result.txt"
--  let nums = (read::String->[Integer]) content
--  (print.reverse.sortBy(compare`on`phiRatio))nums
