digits n | n < 10 = [n]
         | otherwise = mod n 10 : digits (div n 10)

main = (print . sum . digits) (2^1000)
