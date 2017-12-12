import Data.Ratio
import System.Environment

lattices a b = (a * b - (a + b - 1) - (gcd a b - 1)) `div` 2

latticesIn (a, b, c, d) = a+b+c+d-3+lattices a b+lattices b c+lattices c d+lattices d a

intSqrt = floor.sqrt.fromIntegral
isSquare n = intSqrt n ^ 2 == n

main = do
  m <- fmap (read.head) getArgs

  print $ length $ filter (isSquare.latticesIn) [(a,b,c,d)|a<-[1..m],b<-[1..m],c<-[1..m],d<-[1..m]]
