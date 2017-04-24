import Digits

fact n | n == 1 = 1
       | otherwise = n * fact (n - 1)

main = print $ (sum . digits . fact) 100
