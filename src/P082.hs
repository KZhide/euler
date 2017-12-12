import Data.List

indices l = [0..length l-1]
zipIndices = zip<$>id<*>indices
adjacentTriples [] = []
adjacentTriples [x] = []
adjacentTriples [x,y] = []
adjacentTriples (x:y:z:ls) = (x,y,z):adjacentTriples (y:z:ls)

cost currentLine bottomLine s d =
  bottomLine !! d + sum (fmap (currentLine!!) [min s d .. max s d])

minCost currentLine bottomLine s = minimum $ fmap (cost currentLine bottomLine s) (indices bottomLine)
minCosts currentLine bottomLine = fmap (minCost currentLine bottomLine) (indices bottomLine)

main = do
  contents <- readFile "./p082_matrix.txt"
  let ls = lines contents
  let mat = fmap ((read::(String -> [Integer])).(\l -> "["++l++"]")) ls
  print $ minimum $ foldr1 minCosts $ transpose mat
