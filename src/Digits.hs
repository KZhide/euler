module Digits where
digits = digitsWithBase 10
toNum = toNumWithBase 10

digitsWithBase b n | n < b = [n]
                   | otherwise = n `mod` b : digitsWithBase b (n `div` b)

toNumWithBase b [] = 0
toNumWithBase b (x:xs) = x + b * toNumWithBase b xs


