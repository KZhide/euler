import Data.List
import Data.Array.Unboxed
import Data.Ratio
import System.Environment

type Pos = (Int, Int)

fleaPos :: (Pos, Pos) -> Pos -> Integer -> Array Pos Double
fleaPos bs pos 0 = array bs [(pos', if pos==pos' then 1.0 else 0.0) | pos' <- range bs]
fleaPos bs (x, y) n =
  let a = fleaPos bs (x,y) (n-1) in
  array bs [(i, sumNeighbors bs a i) | i <- range bs]
  where
    neighbors bs a (x, y) = filter (inRange bs) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    sumNeighbors bs a (x, y) = sum $ fmap (\pos' -> a!pos' / (fromIntegral .length.neighbors bs a) pos') $ neighbors bs a (x, y)

fleaAbsentProb :: (Pos, Pos) -> Pos -> Integer -> Array Pos Double
fleaAbsentProb bs (x, y) n = fmap (1.0 -) $ fleaPos bs (x, y) n

aZipWith f a a' = array (bounds a) [(i, f (a!i) (a'!i)) | i <- indices a]

unoccupiedProb :: (Pos, Pos) -> Integer -> Array Pos Double
unoccupiedProb bs n =
  let as = fmap (flip (fleaAbsentProb bs) n) [pos|pos <- range bs] in
  foldr1 (aZipWith (*)) as

main = do
  args <- getArgs
  let gen = read (head args) :: Integer
  print $ sum $ unoccupiedProb ((1,1), (30, 30)) gen
