import Data.List
p m n = n * ((m-2)*n+4-m) `div` 2
cand m = filter (\n -> n `mod` 100 >= 10) $ ordFilt (inRange 1000 10000) $ fmap (p m) [1..]

ordFilt b = takeWhile b . dropWhile (not.b)

inRange a b n = a <= n && n < b
isSiritori m = inRange ((m `mod` 100) * 100) ((m `mod` 100) * 100 + 100)

result = do
  perm <- permutations [3,4,5,6,7,8]
  tri <- ordFilt (inRange 1000 10000) $ cand (perm!!0)
  squ <- ordFilt (isSiritori tri) $ cand (perm!!1)
  pent <- ordFilt (isSiritori squ) $ cand (perm!!2)
  hexa <- ordFilt (isSiritori pent) $ cand (perm!!3)
  sept <- ordFilt (isSiritori hexa) $ cand (perm!!4)
  oct <- filter (flip isSiritori tri) $ ordFilt (isSiritori sept) $ cand (perm!!5)
  return [tri, squ, pent, hexa, sept, oct]


main = print $ sum $ head result
