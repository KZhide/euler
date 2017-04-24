import Data.Function
import Data.List

collatz :: Integer -> Integer
collatz n | n `mod` 2 == 0 = n `div` 2
          | otherwise = 3 * n + 1

collatzSeq :: Integer -> [Integer]
collatzSeq n | n == 1 = [1]
             | otherwise = n : (collatzSeq . collatz) n

invCollatzOdd n | n `mod` 3 == 1 = [(n - 1) `div` 3]
                | otherwise = []
invCollatzEven n = [2 * n]

invCollatz :: Integer -> [Integer]
invCollatz n = filter (\m -> m > 1 && m <= 1000000) (invCollatzOdd n ++ invCollatzEven n)

invCollatzSeqs :: Integer -> [[Integer]]
invCollatzSeqs n = do
  m <- invCollatz n
  ms <- invCollatzSeqs m
  return (n : ms)

main = print $ maximumBy (compare `on` length) $ fmap collatzSeq [1..1000000]
