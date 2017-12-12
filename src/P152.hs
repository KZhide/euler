import Data.Ratio
import Primes
import Data.List

elems = fmap ((1%).(^2)) [2..80]

sums 0 [] = 1
sums _ [] = 0
sums n (x:xs) | n > sum (x:xs) / 2 = sums (sum (x:xs) - n) (x:xs)
              | n >= x = sums n xs + sums (n-x) xs
              | otherwise = sums n (dropWhile (n<) xs)

check [] = Nothing
check (x:xs) | x > sum xs = check xs
             | otherwise = Just x

main = do
  --print $ foldr1 lcm $ fmap (^1) [2..80]
  print $ check elems
