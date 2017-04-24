t n = n * (n+1) `div` 2
isTriangle x = t ((floor.sqrt.fromIntegral)(2*x)) == x

val c = fromEnum c - fromEnum 'A' + 1
isTriWord = isTriangle . sum . fmap val

main = do
  content <- readFile "p042_words.txt"
  (print.length.filter isTriWord.(read::String->[String]))("["++content++"]")
