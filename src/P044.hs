-- 補題: k < l => D(j,k) < D(j,l)
-- 補題: k < l => D(k, k+d) < D(l, l+d)

import Ordered
import Data.List

p n = n * (3 * n - 1) `div`2
pl = fmap p [1..]

floorSqrt = floor.sqrt.fromIntegral
isSquare n = (floorSqrt n ^ 2)==n

isPentagonal n = isSquare (1 + 24 * n) && floorSqrt (1 + 24 * n) `mod` 6 == 5
isSumPentagonal (j,k) = isPentagonal (p k + p j)
isDiffPentagonal (j,k) = isPentagonal (p k - p j)

main = print $ fmap (\(j,k)->p k - p j)$ find ((&&) <$> isDiffPentagonal <*> isSumPentagonal) [(j,k)|k<-[1..10000], j<-[k-1,k-2..1]]
--main = print $ filter isDiffPentagonal [(j,k)|k<-[1..1000], j<-[k-1,k-2..1]]
--main = print $ take 10 pl

