f ps n | any (\p -> n `mod` p == 0) ps = f ps (n+1)
       | otherwise = n : f (n:ps) (n+1)

primes = f [] 2

main = print $ primes !! 10000
