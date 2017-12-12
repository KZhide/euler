import Data.Array.IO

split b l = case break b l of
  (l1,[]) -> [l1]
  (l1,l2) -> l1 : split b (tail l2)


minPath :: ((Int,Int)->Integer) -> IOArray (Int,Int) Integer -> (Int,Int) -> IO Integer
minPath w a p = do
  (_, b) <- getBounds a
  f w a b p
  where
    f :: ((Int,Int)->Integer) -> IOArray (Int, Int) Integer -> (Int,Int) -> (Int,Int) -> IO Integer
    f w a (mx, my) (x, y) | (mx,my) == (x,y) = do
                            writeArray a (x,y) (w(x,y))
                            return (w(x,y))
                          | mx == x = do
                            bottom <- readArray a (x,y+1)
                            let v = w (x,y) + bottom
                            writeArray a (x,y) v
                            return v
                          | my == y = do
                            right <- readArray a (x+1,y)
                            let v = w (x,y) + right
                            writeArray a (x,y) v
                            return v
                          | otherwise = do
                            bottom <- readArray a (x,y+1)
                            right <- readArray a (x+1,y)
                            let v = w (x,y) + min bottom right
                            writeArray a (x,y) v
                            return v


main = do
  content <- readFile "../etc/p081_matrix.txt"
  let ls = lines content
  let matrix = fmap (fmap(read::String->Integer).split (==',')) ls
  let w (x,y) = matrix!!y!!x
  let (maxX, maxY) = (length matrix - 1, length (head matrix) - 1)
  let ps = [(x,y)|x<-[maxX, maxX-1..0], y<-[maxY,maxY-1..0]]
  a <- newArray ((0,0),(maxX,maxY)) 0 :: IO (IOArray (Int,Int) Integer)
  mapM_ (minPath w a) ps
  result <- readArray a (0,0)
  print result

