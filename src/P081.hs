import Data.Array.IO

split b l = case break b l of
  (l1,[]) -> [l1]
  (l1,l2) -> l1 : split b (tail l2)


calcMinPathSum :: (Integer, Integer) -> IOArray (Integer, Integer) (Integer, Bool) -> IO Integer
calcMinPathSum (x,y) m = do
  (v, b) <- readArray m (x,y)
  if b then return v else f (x,y) m
  where
    f (x,y) m | (x,y) == (0,0) = do 
                (v,_) <- readArray m (x,y)
                writeArray m (x,y) (v, True)
                return v
              | x == 0 = do
                vy <- calcMinPathSum (x,(y-1)) m
                (v,_) <- readArray m (x,y)
                writeArray m (x,y) (v+vy, True)
                return (v+vy)
              | y == 0 = do
                vx <- calcMinPathSum ((x-1),y) m
                (v,_) <- readArray m (x,y)
                writeArray m (x,y) (v+vx, True)
                return (v+vx)
              | otherwise = do
                vx <- calcMinPathSum ((x-1),y) m
                vy <- calcMinPathSum (x,(y-1)) m
                (v,_) <- readArray m (x,y)
                writeArray m (x,y) (v+(min vx vy), True)
                return (x+(min vx vy))

minimalPathSum m x y | (x,y) == (0,0) = m!!0!!0
                     | x == 0 = minimalPathSum m x (y-1) + (m!!x!!y)
                     | y == 0 = minimalPathSum m (x-1) y + (m!!x!!y)
                     | otherwise = min (minimalPathSum m x (y-1)) (minimalPathSum m (x-1) y) + (m!!x!!y)

main = do
  content <- readFile "p081_matrix.txt"
  ls <- return $ lines content
  matrix <- return $ fmap (fmap(read::String->Integer).split (==',')) ls
  print $ minimalPathSum matrix 79 79
