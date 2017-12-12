import System.Environment
import Primes
import Data.Ratio
import Data.List

bottomLeft n = n^2 - (n-1)
topLeft n = n^2 - 2*(n-1)
topRight n = n^2 - 3*(n-1)

diagonal n = 2 * n - 1
diagPrimes :: Integer -> Integer
diagPrimes n | n == 1 = 0
             | otherwise = (fromIntegral.length . filter isPrime) [bottomLeft n, topLeft n, topRight n] + diagPrimes (n-2)

ratio = (%) <$> diagPrimes <*> diagonal

main = do
  args <- getArgs
  let s = (read::String->Integer)(head args)
  print $ find ((< (1%10)).ratio) $ [s,s+2..]
--main = do
--  args <- getArgs
--  print $ ratio (read(head args))
