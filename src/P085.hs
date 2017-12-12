{-# LANGUAGE TupleSections #-}
import Primes(intSqrt)
import Data.Function
import Data.List

tri n = n * (n+1) `div` 2

countRect w h = tri w * tri h

bound = intSqrt 2000000
tris = fmap tri [1..]

pairs [] = []
pairs [x] = []
pairs (x:y:xs) = (x,y):pairs(y:xs)

cand x = (\(a,b)->[(x,a), (x,b)]) $ head.dropWhile ((<2000000).(*tri x).tri.snd) $ pairs [1..]
diff (a,b) = abs $ countRect a b - 2000000

main = do
  print $ minimumBy (compare`on`diff) $ concatMap cand $ takeWhile ((<bound).tri)[1..]
  print $ countRect 36 77
  print $ 36 * 77
