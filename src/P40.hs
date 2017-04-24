import Data.List
import Digits
endNdigitNum 0 = 0
endNdigitNum n = n * (10 ^ n - 10 ^ (n-1)) + endNdigitNum (n-1)

supEnd k = (head.reverse.takeWhile (\n -> endNdigitNum n < k)) [0..]
d k = 
  let n = supEnd k in 
  let m = n + 1 in
  let targetNum = (k - endNdigitNum n - 1) `div` m + 10^n in
  let targetDigit = (k - endNdigitNum n - 1) `mod` m in
    (reverse.digits)targetNum!!(fromIntegral targetDigit)

main = print $ product $ fmap (d.(10^)) [0..6]
