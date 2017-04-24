module Ordered where

minusBy c xs [] = xs
minusBy c [] _ = []
minusBy c (x:xs) (y:ys) | c x y == LT = x : minusBy c xs (y:ys)
                        | c x y == EQ = minusBy c xs (y:ys)
                        | c x y == GT = minusBy c (x:xs) ys

minus :: (Ord a) => [a] -> [a] -> [a]
minus = minusBy compare

mergeBy c xs [] = xs
mergeBy c [] ys = ys
mergeBy c (x:xs) (y:ys) | c x y == LT = x : mergeBy c xs (y:ys)
                        | c x y == EQ = x : mergeBy c xs ys
                        | c x y == GT = y : mergeBy c (x:xs) ys

merge :: (Ord a) => [a] -> [a] -> [a]
merge = mergeBy compare

ordElemBy c x [] = False
ordElemBy c x (y:ys) | c x y == LT = False
                     | c x y == EQ = True
                     | c x y == GT = ordElemBy c x ys

ordElem :: (Ord a) => a -> [a] -> Bool
ordElem = ordElemBy compare
