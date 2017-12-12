import Data.List

isSeqMult n = let r = (sqrt.fromInteger) n in floor r /= ceiling r && n == floor r * ceiling r

blue :: Integer -> Maybe Integer
blue n = find (\i -> i*(i-1)*2 == n*(n-1)) [2..n]

main = print $ head $ filter (\i -> isSeqMult (i*(i-1)*2)) [10^12..]
