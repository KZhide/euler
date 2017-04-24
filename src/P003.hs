
factors n i | i > n = []
            | i <= n && n `mod` i == 0 = i : (factors (n `div` i) i)
            | otherwise = factors n (i+1)

main = print $ factors 600851475143 2
