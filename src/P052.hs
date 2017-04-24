import Digits
import Data.List

cand :: Int -> [Integer]
cand n = (fmap toNum . sequence) ([3,7,9]:replicate n [0..9] ++ [[1]])

condition x = (allSame.fmap (sort.digits.(x*)))[1..6]
  where
    allSame [] = True
    allSame [x] = True
    allSame (x:y:xs) | x == y = allSame (y:xs)
                     | otherwise = False


main = print $ filter condition $ cand 3

