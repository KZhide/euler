findFibIndex n b x y | b x = n
                     | b y = n+1
                     | otherwise = findFibIndex (n+1) b y (x+y)

main = print $ findFibIndex 1 (>10^999) 1 1
