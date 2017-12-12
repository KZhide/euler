import Data.List
--sep n s | n == s = 1
--        | n < s = 0
--        | s == 1 = 1
--        | s == 2 = n `div` 2
--        | s == 3 = (n+1) `div` 2 - 1 + sep (n-3) 3
--        | otherwise = (sum.fmap (sep (n-s))) [1..s]
--p n = sum (fmap (sep n) [1..n])

p'' = p' 1
  where
    p' k n | k > n = 0
           | k == n = 1
           | otherwise = p' (k+1) n + p' k (n-k)


zP = zipWith (+)
zM = zipWith (-)
signed i l | i `mod` 4 == 0 || i `mod` 4 == 3 = fmap negate l
           | otherwise = l
main = print $ find (\n -> p n `mod` 1000000 == 0) [1..]
  where
    pent i = i * (3*i-1) `div` 2
    pents = 0 : (fmap pent . concatMap (\i->[i,-i])) [1..]
    pa i | i == 0 = [1,1..]
         | i == 1 = let l = take (pents !! i) (pa (i-1)) ++ l in l
         | otherwise = let l = take (pents !! i) (pa (i-1)) ++ foldr zP [0,0..] (fmap (\k -> drop ((pents!!i)-(pents!!k)) (signed k l)) [1..i]) in l
    p n = let i = (head.filter ((n<) . (pents !!))) [1..] in pa i !! n
