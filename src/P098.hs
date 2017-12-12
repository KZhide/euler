import Data.Function
import Data.List

anagrams = filter ((>=2).length) . groupBy ((==) `on` sort) . sortBy (compare `on` sort)

main = do
  content <- readFile "p098_words.txt"
  let wordList = (read::String->[String]) ("["++ content ++"]")
  print $ anagrams wordList
