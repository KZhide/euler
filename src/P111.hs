import Primes
import Digits
import Control.Arrow (second)
import Data.List
import Control.Monad(replicateM)

randomPadding :: (Eq a) => Int -> a -> [a] -> [[a]]
randomPadding 0 d l = [l]
randomPadding n d [] = [replicate n d]
randomPadding n d (x:xs) = fmap (d:) (randomPadding (n-1) d (x:xs)) ++ fmap (x:) (randomPadding n d xs)

digitPrimes :: Int -> Integer -> Int -> [Integer]
digitPrimes len d n = filter isPrime $ fmap toNum $ filter ((/=0).last) $ do
  let ds = delete d [0..9]
  l <- filter ((<n).maximum.fmap length.group.sort)$ replicateM (len-n) ds
  randomPadding n d l

m len d = find (not . null . digitPrimes len d) [len-1, len-2 .. 0]
n len d = find (>0) $ fmap (length . digitPrimes len d) [len-1, len-2 .. 0]
s len d = do
  l <- find (not.null) $ fmap (digitPrimes len d) [len-1, len-2 .. 0]
  return (sum l)

sumS len = do 
  l <- sequence $ fmap (s len) [0..9]
  return (sum l)

main = print $ sumS 10
--main = print $ length $ primesUnder (10^5)
