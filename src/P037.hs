import Primes
import Digits
import Data.List

postfixes [x] = [[x]]
postfixes (x:xs) = (x:xs) : postfixes xs
prefixes [x] = [[x]]
prefixes (x:xs) = [x] :  fmap (x:) (prefixes xs)

leftTrancatable = all (isPrime . toNum) . postfixes
rightTrancatable = all (isPrime . toNum) . prefixes
trancatable = (&&) <$> leftTrancatable <*> rightTrancatable

lastDigits = [3,7]
middleDigits = [1,3,7,9]
firstDigits = [2,3,5,7]

listPow n l | n == 0 = [[]]
            | otherwise = do
              x <- l
              xs <- listPow (n-1) l
              return (x:xs)

cand n | n >= 2 = do
  x <- lastDigits
  ys <- listPow (n-2) middleDigits
  z <- firstDigits
  return (x:(ys++[z]))

main = print $ sum $ fmap toNum $ filter trancatable $ cand 2 ++ cand 3 ++ cand 4 ++ cand 5 ++ cand 6
