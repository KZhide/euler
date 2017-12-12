-- (a - 1)^n = Q.a^2 + (-1)^n * na - (-1)^n
-- (a + 1)^n = P.a^2 + na + 1

r a n | odd n = (2 * n * a) `mod` a^2
      | even n = 2

rmax a | even a = a * (a - 2)
       | odd a = a * (a - 1)

r' a n = ((a-1)^n + (a+1)^n) `mod` a^2

main = do
  print $ fmap (r 7) [1..10]
  print $ fmap (r' 7) [1..10]
  print $ sum $ fmap rmax [3..1000]
