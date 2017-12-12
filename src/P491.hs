import Control.Arrow
import Data.List

genDigits 0 0 _ = [[]]
genDigits 0 _ _ = []
genDigits _ _ [] = []
genDigits dNum dSum (x:xs) | x > dSum = []
                           | otherwise = ((x:) <$> genDigits (dNum - 1) (dSum - x) xs) ++ genDigits dNum dSum (dropWhile (==x) xs)

ds = [0,0,1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9]
dsPair ls = (ls, ds `minus` ls)

minus ls [] = ls
minus [] _ = []
minus (x:xs) (y:ys) | x == y = minus xs ys
                    | x < y = x : minus xs (y:ys)
                    | x > y = minus (x:xs) ys

fact 0 = 1
fact n = n * fact (n-1)

pat ls | 0 `notElem` ls = rawPat ls
       | otherwise = rawPat ls - rawPat (tail ls)
rawPat ls = fact (length ls) `div` product (fact.length <$> group ls)

main = do
  print $ sum $ fmap (sum . fmap (uncurry (*) . (pat *** rawPat) . dsPair) . flip (genDigits 10) ds) [1,12..90]
