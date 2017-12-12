import Data.Function
import Data.List

withIndex = w 0
  where
    w n [] = []
    w n (x:xs) = (n, x) : w (n+1) xs

f (b,e) = fromIntegral e * log (fromIntegral b)

main = do
  content <- readFile "../tmp/p099_base_exp.txt"
  let ls = lines content
  let pairs = fmap (\s -> (read::String->(Integer,Integer)) ("("++s++")")) ls
  (print.sortBy (compare`on`(negate.f.snd)).withIndex)pairs
