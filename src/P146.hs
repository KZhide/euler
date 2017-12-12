import System.Environment
import Primes

condition n = all isPrime [n^2+1, n^2+3, n^2+7, n^2+9, n^2+13, n^2+27]

dividable p n = n `mod` p == 0

isPatPrime n =  all (f n) (primesUnder n) && all (g n) (5:11:[15,17..25])
  where
    f n p = all (not.dividable p.(n^2+)) [1, 3, 7, 9, 13, 27]
    g n d = any (`dividable` (n^2+d)) (primesUnder n) 

--sixtaples (a:ls@(b:c:d:e:f:_)) = (a,b,c,d,e,f): sixtaples ls 
--sixtaples _ = []

--possibleSeq (a,b,c,d,e,f) = all (==a) [b-2, c-6, d-8, e-12, f-26]

squareNum x = ((^2).floor.sqrt.fromIntegral) x == x

--main = mapM_ print $ take 3 $ filter possibleSeq $ filter (\(a,_,_,_,_,_) -> squareNum (a-1))$ sixtaples (primesUnder (10000^2))

possibleMod p = filter (\d -> all ((/=0).(`mod`p)) [d^2+1, d^2+3, d^2+7, d^2+9, d^2+13, d^2+27]) [0..p-1]
possibleNums p n = (fmap intSqrt $ filter squareNum $ fmap (p-) [1, 3, 7, 9, 13, 27]) `union` (foldr1 union $ fmap (\m -> [m, m+p..n]) $ possibleMod p)

sortInter [] _ = []
sortInter _ [] = []
sortInter (x:xs) (y:ys) | x == y = x : sortInter xs ys
                        | x < y = sortInter xs (y:ys)
                        | x > y = sortInter (x:xs) ys

--candidates n = foldr1 sortInter $ fmap (`possibleNums` n) $ primesUnder 50
candidates n = filter isPatPrime ([10,40..n] `union` [20, 50..n])

--candidates :: Integer -> [Integer]
--candidates n =
--  filter (\i -> i `mod` 7 == 3 || i `mod` 7 == 4) $
--  filter (\i -> i `mod` 11 `elem` [0, 1, 4, 6, 7, 10]) $
--  [10,20..n] `minus` [30, 60..n] `minus` [70, 140..n] `minus` [130, 260..n]

main = do
  args <- getArgs
  let n = (read::String->Integer) $ args !! 0
  --let l = candidates n
  --mapM_ print $ filter condition $ candidates n
  --print $ length l
  let l = candidates n
  mapM_ print $ l
  print $ sum l
  --print $ length $ primesUnder n
