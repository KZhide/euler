import Data.Ratio
import Digits
l = [2,4..] >>= (\n->[1,n,1])

contFrac [] = 0
contFrac [x] = 1 % x
contFrac (x:xs) = 1 / ((x%1) + contFrac xs)

f n l = n + contFrac l

main = print $ sum $ digits $ numerator $ f 2 (take 99 l)
