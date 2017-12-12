import Digits
import Data.List

palindroms :: Integer -> [a] -> [[a]]
palindroms 0 l = [[]]
palindroms 1 l = do
  a <- l
  return [a]
palindroms n l = do
  a <- l
  s <- palindroms (n-2) l
  return (a : (s ++ [a]))

palindNums n = fmap toNum $ filter ((/=0).head) $ palindroms n [0..9]

palinds n = [1..n] >>= palindNums

sortUnion xs [] = xs
sortUnion [] ys = ys
sortUnion (x:xs) (y:ys) | x < y = x : sortUnion xs (y:ys)
                        | x == y = sortUnion xs (y:ys)
                        | otherwise = y : sortUnion (x:xs) ys
sortIntersect xs [] = []
sortIntersect [] ys = []
sortIntersect (x:xs) (y:ys) | x < y = sortIntersect xs (y:ys)
                            | x == y = x : sortIntersect (dropWhile (==x) xs) (dropWhile (==y) ys)
                            | otherwise = sortIntersect (x:xs) ys

consSquareSumFrom n = fmap (\m -> sum $ fmap (^2) [n..m]) [n+1..]
consSquareSumUnderSquareOf n = foldr sortUnion [] $ fmap consSquareSumFrom [1..n]

main = print $ sum $ (takeWhile (<100000000) (consSquareSumUnderSquareOf 10000) `sortIntersect` palinds 8)
