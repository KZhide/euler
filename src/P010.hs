f ps n | n >= 2000000 = [] 
       | any (\p -> n `mod` p == 0) ps = f ps (n+1)
       | otherwise = n : f (n:ps) (n+1)

primesUnder n = g [] [2..n] (floor $ sqrt $ fromIntegral n)
  where
    g ps [] bound = ps
    g ps (x:xs) bound | x <= bound = g (x:ps) (filter (\y -> y `mod` x /= 0) xs) bound
                      | otherwise = ps ++ (x:xs)

primes = f [] 2

main = print $ sum $ primesUnder 2000000
