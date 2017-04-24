import Primes
import Data.List
import Data.Maybe

postfixes [] = []
postfixes (x:xs) = (x:xs) : postfixes xs
prefixes [] = [[]]
prefixes (x:xs) = fmap (x:) (prefixes xs) ++ [[]]
boundedPrefixes n [] = [[]]
boundedPrefixes n (x:xs) | x > n = [[]]
                         | otherwise = fmap(x:)(boundedPrefixes (n-x) xs)++[[]]

subNlength :: Int -> [a] -> [[a]]
subNlength n = filter ((n==) . length) . fmap (take n) . postfixes


main = print $ (sum.head.sortOn (negate.length).concatMap (maybeToList.find(isPrime.sum).boundedPrefixes (10^6)).postfixes) $ primesUnder (10^6`div`21)
