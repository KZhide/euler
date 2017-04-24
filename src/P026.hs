import Data.List
import Data.Function(on)
divPair p q = (p `div` q, p `mod` q)
hissan p q = f [] p q
  where
    f l p q | divPair p q `elem` l = dropWhile (/=divPair p q) l
            | snd (divPair p q) == 0 = []
            | otherwise = let (n,r) = divPair p q in f (l++[(n,r)]) (10*r) q

main = print $ maximumBy (compare `on` length . hissan 1) [1..1000]
