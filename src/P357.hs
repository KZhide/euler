import Primes
import System.Environment

candidates m (x:xs) | m < x = [[]]
                    | otherwise = fmap (x:) (candidates (m`div`x) xs) ++ candidates m xs

cond ls = let n = product ls in 
  all (\d -> isPrime (d + n`div`d)) (divisors n)

main = do
  args <- getArgs
  let m = read (head args)
  let answers = fmap product $ filter cond $ candidates m primes
  mapM_ print $ scanl1 (+) answers
