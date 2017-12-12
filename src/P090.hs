import Control.Monad
import Data.List

squares = [(0,1),(0,4),(0,6),(1,6),(2,5),(3,6),(4,6),(8,1)]

cand (d1, d2) (x,y) | x `elem` d1 && y `elem` d2 || x `elem` d2 && y `elem` d1 = [(d1, d2)]
                    | x `elem` d1 && x `elem` d2 = [(d1, y:d2), (y:d1, d2)]
                    | x `elem` d1 = [(d1, y:d2)]
                    | x `elem` d2 = [(y:d1, d2)]
                    | y `elem` d1 && y `elem` d2 = [(x:d1, d2), (d1, x:d2)]
                    | y `elem` d1 = [(d1, x:d2)]
                    | y `elem` d2 = [(x:d1, d2)]
                    | otherwise= [(x:d1, y:d2), (y:d1, x:d2)]

subsetsN 0 _ = [[]]
subsetsN n [] = []
subsetsN n (x:xs) = fmap (x:) (subsetsN (n-1) xs) ++ subsetsN n xs

expand d = do
  l <- subsetsN (6 - length d) ([0..9]\\d)
  return (d++l)

nineFlip d | 6 `elem` d && 9 `notElem` d = [d, fmap (\n -> if n == 6 then 9 else n) d]
           | otherwise = [d]

cs = do
  (d1, d2) <- filter (\(d,d') -> length d <= 6 && length d' <= 6) $ foldM cand ([], []) squares
  dd1 <- expand d1 >>= nineFlip
  dd2 <- expand d2 >>= nineFlip
  return (sort dd1, sort dd2)

--main = print $ fmap (\(d1, d2) -> (sort d1, sort d2))$ foldM cand ([],[]) squares
main = print $length $ nub $ sort cs
