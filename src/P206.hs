import Primes
import Data.List

match [] [] = True
match _ [] = False
match [] _ = False
match (s:ss) (s':ss') | s == s' || s == '.' = match ss ss'
                      | otherwise = False

main = do
  print $ [intSqrt 1020304050607080900, intSqrt 1929394959697989990]
  print $ find (match "1.2.3.4.5.6.7.8.9.0".show.(^2)) [intSqrt 1020304050607080900, intSqrt 1020304050607080900+10 .. intSqrt 1929394959697989990]

