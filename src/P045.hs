t n = n * (n+1) `div` 2
p n = n * (3 * n - 1) `div` 2
h n = n * (2 * n - 1)

isT k = (t.floor.sqrt.fromIntegral) (2 * k) == k
isH k = (h.ceiling.sqrt.(/2).fromIntegral) k == k

main = print $ take 3 $ filter ((&&)<$>isT<*>isH) $ fmap p [1..]
