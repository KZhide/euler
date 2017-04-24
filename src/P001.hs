multiple3or5 n = filter (\x -> mod x 3 == 0 || mod x 5 == 0) [1..n]
sum3or5 n = sum $ multiple3or5 n


main :: IO ()
main = print $ sum3or5 999
