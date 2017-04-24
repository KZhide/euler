import Digits
import Data.List

palind = (==)<$>id<*>reverse
palindNum = palind.digits

reverseAdd = (+) <$> id <*> toNum.reverse.digits
lychrelUntil iteration n = f (iteration-1) (reverseAdd n)
  where
    f i n | palindNum n = False
          | i == 0 = True
          | otherwise = f (i-1) (reverseAdd n)

main = print $ length $ filter (lychrelUntil 50) [0..10000]
