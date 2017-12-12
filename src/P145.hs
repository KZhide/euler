{-
- Let (d1, d2) be moving-up odd-pair if d1+d2 is odd and d1+d2>9, and so on.
- let w = dn,...,d1 is even-length digits number.
- if dn, d1 is moving-up odd-pair, w is not reversible.
- because: in this case, d(n-1) and d2 must be non-moving-up even-pair.
- consider d(n-1) and d2 don't move up. so d(n-2) and d3 must be moving-up odd-pair.
- consider d(n-1) and d2 move up. so d(n-2) and d3 must be even-pair.
-
- if and only if w is even-length digits reversible number, then (d1, dn), ... , (d(n/2), d(n/2+1)) are non-movingsup odd-pair.
-
- 
-}

import System.Environment

nonMovingOddPairs = 2 * (length $ [(x, y)|x <- [1,3..9], y<-[0,2..8], x+y<10])
nonMovingOddPairsWithoutZero = 2 * (length $ [(x,y)|x <- [1,3..9], y <- [2,4..8], x + y < 10])
movingOddPairs = 2 * (length $ [(x, y)|x <- [1,3..9], y <- [0,2..8], x + y >= 10])
movingEvenPairs = 25
nonMovingEvenPairs = 25

reversibleNumbers 1 = 0
reversibleNumbers l | even l = nonMovingOddPairsWithoutZero * nonMovingOddPairs ^ (l`div`2-1)
                    | l `mod` 4 == 3 = 5 * movingOddPairs^((l-3)`div`4+1) * nonMovingEvenPairs ^ ((l-3)`div`4)
                    | otherwise = 0

oddDigiChoice 1 = fmap return [1,3..9]
oddDigiChoice 2 = do
  a <- [1..9]
  b <- if even a then [x| x <- [1,3..9], a+x < 10] else [x|x<-[0,2..8], a+x < 10]
  return [a, b]

oddDigiChoice n = do
  a <- [1..9]
  b <- if even a then [1,3,9] else [2,4..8]
  return [a, b]

reverseN :: Integer -> Integer
reverseN n | n < 10 = n
           | otherwise = n `mod` 10 * 10 ^ floor (logBase 10 (fromIntegral n)) + reverseN (n `div` 10)

isOddDigits n | n < 10 = odd n
              | odd (n `mod` 10) = isOddDigits (n`div`10)
              | otherwise = False

reversible n = isOddDigits (n + reverseN n)

main = do
  n <- fmap (read.head) getArgs
  print $ sum $ fmap  reversibleNumbers $ [1..n]
  print $ length $ filter reversible $ filter ((/=0).(`mod` 10)) [1..10^n]
