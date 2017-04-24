import Digits
import Data.Ratio
t n | n == 1 = 2 % 1
    | otherwise = 2 + 1 / t (n-1)
d n = 1 + 1 / t n

main = (print.length.filter ((<)<$>length.digits.denominator<*>length.digits.numerator) . fmap d)[1..1000]
