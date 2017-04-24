dropAt _ [] = []
dropAt n (x:xs) | n == 0 = xs
                | otherwise = x : dropAt (n-1) xs

withIndex = f 0
  where
    f _ [] = []
    f n (x:xs) = (x,n):f (n+1) xs

perm :: [a] -> [[a]]
perm (x:[]) = [[x]]
perm l = do
  (x,n) <- withIndex l
  ll <- perm (dropAt n l)
  return (x:ll)
main = print $ perm "0123456789" !! (1000000-1)
