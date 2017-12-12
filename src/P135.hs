import Primes

-- (z+2i)^2-(z+i)^2-z^2 = n
-- -z2 + 2zi +3i2 = n
-- -(z-3i)(z+i) = n
-- (3i-z)(z+i) = n
-- 3i-z = a
-- z+i = b
-- 4i = a+b
--
-- i = (a+b)/4
-- z = b - (a+b)/4 = (-a+3b)/4

factors :: Integer -> [Integer]
factors = fmap product.sequence.fmap (\(p,i) -> fmap (p^) [0..i]).primeFactors
factorNum = product.fmap (\(_,i) -> i+1).primeFactors
primePairs :: Integer -> [(Integer, Integer)]
primePairs i = (\f -> (f, i `div` f)) <$> factors i

zAble (a,b) = 3*b-a > 0 && (3*b-a)`mod`4 == 0
iAble (a,b) = (a+b)`mod`4 == 0

sols = length . filter iAble . filter zAble . primePairs

main = do
  --print $ length $ filter ((>=12).factorNum) [1..1000000]
  --print $ primeFactors 36
  --print $ length $ filter ((==10).sols) $ filter ((>=12).factorNum) [1..1000000]
  l <- mapM print $ filter ((==1).sols) $ [1..50000000]
  print $ length l
