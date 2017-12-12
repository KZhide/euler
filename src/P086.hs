import System.Environment
import Control.Arrow
import Data.List
sPathSquare (x, y, z) = z^2+(x+y)^2

isSquare x = ((^2).floor.sqrt.fromIntegral) x == x

squarePaths z = sum $ fmap f [(xy,z)|xy <- [1..z*2]]
  where
    f (xy, z) | isSquare (z^2+xy^2) = length $ filter (\x -> xy-x <= z) [1..xy`div`2]
              | otherwise = 0

main = do
  args <- getArgs
  let least = read (head args)
  print $ find (uncurry $ const (>= least)) $ scanl1 (uncurry $ const (second . (+))) $ fmap (id&&&squarePaths) [1..]
