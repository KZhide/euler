import Primes
import System.Environment(getArgs)

hit :: Integer -> Integer -> Bool
hit a b = gcd a b == 1 && memrad a * memrad b * memrad (a+b) < a + b

memrad :: Integer -> Integer
memrad = ((0:map rad [1..]) !!).fromIntegral
  where
    rad = f (map fromIntegral primes)
    f (x:xs) i | i < x = i
               | i `mod` (x^2) == 0 = memrad (i`div`x)
               | i `mod` x == 0 = x * memrad (i`div`x)
               | otherwise = f xs i

rad :: Integer -> Integer
rad = f primes
  where
    f (x:xs) i | i < x = i
               | i `mod` (x^2) == 0 = f (x:xs) (i`div`x)
               | i `mod` x == 0 = f xs i
               | otherwise = f xs i

cands :: Integer -> [(Integer, Integer)]
cands n = [(a,b)|b <- [1..n-1], a <- [1..min (n-b) b-1], gcd a b == 1]

main = do
  args <- getArgs
  print $ sum $ fmap (uncurry (+)) $ filter (uncurry hit) $ cands (read (head args) :: Integer)
  --print $ uncurry hit $ last $ cands 1000
  --print $ fmap rad [1..100]

-- lemma. if a, b and c are primes, then (a,b,c) does not hit.
-- proof. rad (a,b,c) = abc > c.
-- lemma. if a == 1 and b is a prime, then (a,b,c) does not hit.
-- proof. rad (a,b,c) = rad(b(b+1))
-- c = b+1
-- rad (b * (b+1)) - (b+1)
