calcValue (x:xs) (y:z:ys) = x + max y z : calcValue xs (z:ys)
calcValue [] _ = []

recCalcValue [] = []
recCalcValue [l] = l
recCalcValue (l:ls) = calcValue l (recCalcValue ls)

main = do
  content <- readFile "p067_triangle.txt"
  let ls = fmap (fmap read . words) (lines content) :: [[Int]] in
    print (recCalcValue ls)
