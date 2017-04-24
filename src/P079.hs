import Data.List

split b l = case break b l of
  (l1,[]) -> [l1]
  (l1,l2) -> l1 : split b (tail l2)

cand n = sequence (replicate n ['0'..'9'])

subseq (x:xs) [] = False
subseq [] _ = True
subseq (x:xs) (y:ys) | x==y = subseq xs ys
                     | otherwise = subseq (x:xs) ys

main = do 
  content <- readFile "p079_keylog.txt"
  keys <- (return . nub.split (=='\n')) content  
  (print . filter (\s -> all (`subseq`s) keys)) (cand 8)
