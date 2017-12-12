import Primes
import Data.List (scanl', foldr1, foldl', sort)
import System.Environment

repUnits n = scanl' (\a _ -> a * 10 + 1) 1 [1..n-1]
repUnit n = foldl' (\a _ -> a * 10 + 1) 1 [1..n-1]

main = do
  args <- getArgs
  let n = (read :: String -> Int) $ head args
  --mapM_ print $ take 1 $ filter ((>n).a) $ filter ((==1).(gcd 10)) $ [1..]
  --mapM_ print $ repUnits n
  --print $ head $ filter (\i -> all ((/=0).(`mod` i)) $ repUnits n) ([1,3..] `minus` [5,15..])
  print $ primeFactors $ repUnit 100
