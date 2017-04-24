fact n | n > 0 = n * fact (n-1)
       | otherwise = 1

combination n r = fact n `div` fact r `div` fact (n-r)

candidates = [(n,r) | n <- [1..100], r <- [0..n]]

main = print $ length $ filter (\(n,r) -> combination n r > 1000000) candidates
