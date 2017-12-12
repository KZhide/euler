import Primes
import System.Environment

rep n b = sum $ fmap (b^) [0..n-1]

strongReps m = foldl1 union $ do
  n <- [3..floor (logBase 2 (fromIntegral m))]
  return (takeWhile (<m) $ fmap (rep n) [2..])

main = do
  m <- fmap (read.head) getArgs
  print $ strongReps m
