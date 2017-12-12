import Primes
import Digits
import Data.List

data State = State {ps::[Integer], ds::[Integer]} deriving (Eq, Show)

step :: State -> [State]
step st | null $ ds st = [st]
        | otherwise = do
          let n = head $ ds st
          ss <- subsequences $ tail $ ds st
          p <- filter isPrime $ toNum <$> permutations (n:ss)
          let ds' = ds st \\ (n:ss)
          return State {ps = p : ps st, ds = ds'}

{-
step :: State -> [State]
step st | null $ ds st = [st]
        | otherwise = do
          n <- ds st
          primesWith0thDigit st n
-}

convergence f s | s == f s = s
                | otherwise = convergence f (f s)

main = do
  --print $ step $ State {ps = [], ds = [1..9]}
  print $ length $ convergence (>>=step) [State {ps = [], ds = [1..9]}]
