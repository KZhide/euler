cross (x1, y1) (x2, y2) = x1*y2 - x2*y1
rotsig v1 v2 = cross v1 v2 `div` abs (cross v1 v2)
isContainOrigin v1 v2 v3 = rotsig v1 v2 == rotsig v2 v3 && rotsig v2 v3 == rotsig v3 v1

main = do
  content <- readFile "../etc/p102_triangles.txt"
  let ls = lines content
  let triangles = fmap (f.(\l -> (read::String->[Integer]) ("["++l++"]"))) ls
  print $ length $ filter (\(a,b,c)-> isContainOrigin a b c) triangles
  where
    f [a,b,c,d,e,f] = ((a,b),(c,d),(e,f))
