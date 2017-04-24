import Data.Char (ord)
import Data.List

val c = ord c - ord 'A' + 1
nameScore s index = (sum.fmap val) s * (index + 1)

withIndex = f 0
  where
    f n [] = []
    f n (x:xs) = (x,n):f (n+1) xs

main = do
  contents <- readFile "p022_names.txt"
  print $ (sum.fmap (uncurry nameScore).withIndex.sort.(read :: String -> [String])) ("[" ++ contents ++ "]")
