import Primes (minus)
import Data.List

subseqLen 0 _ = [[]]
subseqLen _ [] = []
subseqLen n (x:xs) = fmap (x:) (subseqLen (n-1) xs) ++ subseqLen n xs

divideList n l = fmap (\s -> (s, l `minus` s)) $ subseqLen n l

valid outer inner = (sum outer + sum inner * 2) `mod` length inner == 0
henValue outer inner = (sum outer + sum inner * 2) `div` length inner

ringPerm (x:xs) = fmap (x:) $ permutations xs
pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x,y):pairs (y:xs)
hens (x:xs) = fmap (uncurry (+)) $ pairs (x:xs++[x])

rings outer inner = do
  r <- ringPerm inner
  let hRests = (henValue outer inner -) <$> hens r
  if sort hRests == outer then return (hRests, r) else []

main = do
  let cand = filter (\(outer, _) -> 10 `elem` outer)$ filter (uncurry valid) $ divideList 5 [1..10]
  --print cand
  let candRings = concatMap (uncurry rings) cand
  print candRings
  print $ length candRings
