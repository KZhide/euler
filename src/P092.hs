import Digits

squareDigitSum = sum.fmap (^2).digits

arriveAt89 n | n == 89 = True
             | n == 1 = False
             | n == 0 = False
             | otherwise = arriveAt89 (squareDigitSum n)

mlist = fmap arriveAt89 [0..81*7]

memArriveAt89 n = mlist !! (fromIntegral . squareDigitSum) n

main = print $ length $ filter memArriveAt89 [1..10000000]
