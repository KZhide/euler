import Digits
import Primes
import Data.List
import Data.List.Extra

candidates = filter (>=1000) (primesUnder 10000)

subseq n [] | n == 0 = [[]]
            | otherwise = []
subseq n (x:xs) = fmap (x:) (subseq (n-1) xs) ++ subseq n xs

isProgression3 [x,y,z] = y - x == z - y

main = print $ filter isProgression3 . concatMap (subseq 3) $ filter ((3 <=) . length) $ groupSortOn (sort . digits) candidates
