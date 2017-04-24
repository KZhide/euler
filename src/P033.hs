import Data.List
import Digits
import Control.Arrow
import Data.Ratio
elimCands :: (Eq a) => a -> [a] -> [[a]]
elimCands _ [] = []
elimCands a [x] | x == a = [[]]
                | otherwise = []
elimCands a (x:xs) | x == a = xs : fmap (x:) (elimCands a xs)
                   | otherwise = fmap (x:) (elimCands a xs)

f :: (Eq a) => [a] -> [(a,[a])]
f [] = []
f (x:xs) = (x,xs) : fmap (\(y,ys) -> (y, x:ys)) (f xs)

digitElims :: (Eq a) => [a] -> [a] -> [([a], [a])]
digitElims l1 l2 = (nub . fmap (\((_,xs),(_,ys))->(xs,ys)) . filter (\((x,_),(y,_))-> x==y)) [((x,xs),(y,ys)) | (x,xs) <- f l1, (y,ys) <- f l2]

g :: Integer -> Integer -> [(Integer, Integer)]
g x y | 0 `elem` digits y = []
      | otherwise = (filter (\(x',y') -> y'/=0 && x%y == x'%y') . fmap (toNum *** toNum)) (digitElims (digits x) (digits y))

main = print $ denominator $ product $ fmap (\(x,y) -> x%y) $ filter (\(x,y) -> g x y /= []) [(x,y)|x<-[10..99],y<-[(x+1)..99]]
