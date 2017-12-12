import System.Environment
import Primes

{-
- a^2 === a mod m 
- a(a-1) === 0 mod p^i
- a(a-1) == kp^i
- a = kp^i or kp^i + 1
-}

cand n pf = reverse $ union [0, pf .. n-1] [1, pf+1 .. n-1]

intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys) | x == y = x: intersect xs ys
                        | x > y = intersect xs (y:ys)
                        | x < y = intersect (x:xs) ys

m n | n == 1 = 0
    | otherwise = head $ foldr1 intersect $ (cand n.uncurry (^)) <$> primeFactors n

main = do
  n <- fmap (read.head) getArgs
  print $ fmap (\a -> (a, m a)) [1..100]
  print $ sum $ fmap m [1..n]
