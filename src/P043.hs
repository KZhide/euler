import Digits
import Data.List

uniq l = nub l == l
divisable q p = p `mod` q == 0
takeLast n xs = drop (length xs - n) xs

cand098 :: [[Integer]]
cand098 = (filter uniq.fmap digits) [102,119..999]
filtN :: Integer -> [Integer] -> [[Integer]]
filtN n l = (filter(divisable n.toNum.takeLast 3).fmap((l++).return).filter (`notElem`l))[0..9]


main = print $ sum $ fmap toNum (cand098 >>= filtN 13 >>= filtN 11 >>= filtN 7 >>= filtN 5 >>= filtN 3 >>= filtN 2 >>= filtN 1)
