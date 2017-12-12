import Data.Array.IArray
import Control.Arrow
import Data.Function
import Data.List

extract [] = []
extract (x:xs) = (x,xs) : fmap (second (x:)) (extract xs)

findMaximum :: [[Integer]] -> Integer
findMaximum [] = 0
findMaximum a = maximum $ (\(l,ls) -> head l + findMaximum (fmap tail ls)) <$> extract a

splitWs :: String -> [String]
splitWs [] = []
splitWs (' ':s) = splitWs s
splitWs ('\n':s) = splitWs s
splitWs (c:' ':s) = [c]:splitWs s
splitWs (c:'\n':s) = [c]:splitWs s
splitWs (c:c':s) = let (w:ws) = splitWs (c':s) in (c:w) : ws

g :: [([Integer], Integer)] -> [(Integer, Integer)] -> [([Integer], Integer)]
g prev current = fmap (maximumBy (compare `on` snd)) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ do
  (i, v) <- current
  (iis, v') <- filter ((i `notElem`).fst) prev
  return (sort (i:iis), v+v')

withIndex = withIndex' 0
  where
    withIndex' _ [] = []
    withIndex' n (x:xs) = (n, x): withIndex' (n+1) xs

findMaximumPath = foldl' g [([], 0)] . fmap withIndex

main = do
  content <- readFile "p345_matrix.txt"
  let mat = (fmap read.words) <$> lines content :: [[Integer]]
  print $ findMaximumPath mat
  --[
  --  [7, 53, 183, 439, 863],
  --  [497, 383, 563, 79, 973],
  --  [287, 63, 343, 169, 583],
  --  [627, 343, 773, 959, 943],
  --  [767, 473, 103, 699, 303]]


f :: Array Integer Integer -> Array Integer Integer
f a = array (bounds a) (fmap (\i -> (i, maximum $ fmap snd $ filter ((/=i).fst) $ assocs a)) $ indices a)
