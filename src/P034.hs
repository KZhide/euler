fact n | n > 0 = n * fact (n-1)
       | otherwise = 1

digits n | n < 10 = [n]
         | otherwise = mod n 10 : digits (div n 10)

check = (==) <$> id <*> (sum . fmap fact . digits)

main = print $ filter check [1..10000000]

