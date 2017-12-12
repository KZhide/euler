import Primes
import Digits
import Data.Function
import Infix
import Control.Applicative
import Data.List
import Control.Monad

cond = ((isPrime . toNum) . ) . ( . digits) . (++) . digits
cond' = (<*>)<$>(fmap(&&).cond) <*> flip cond

primes1 = filter (\p -> p `mod` 3 == 1) primes
primes2 = filter (\p -> p `mod` 3 == 2) primes

findCliques n [] | n == 0 = [[]]
                 | otherwise = []
findCliques n (x:xs) | n == 0 = [[]]
                     | otherwise = (fmap (x:).findCliques(n-1).filter (cond' x)) xs ++ findCliques n xs

calcCliques :: [[[Integer]]] -> Integer -> [[[Integer]]]
calcCliques [] n = []
calcCliques [cs] n | filter (all (cond' n)) cs /= [] = [cs, (fmap (n:) . filter (all (cond' n))) cs]
                   | otherwise = [cs]
calcCliques (cs1:cs2:ls) n =
  let cs2' = (fmap (n:).filter (all (cond' n))) cs1 ++ cs2 in
  cs1 : calcCliques (cs2':ls) n

--main = print $ find (/=[]) $ fmap (findCliques 5.primesUnder) primes
main = 
  ($ [[[]]]) . ($ primes) . fix $ \loop (x:xs) cliques -> do
    print (x, last cliques)
    let cliques' = calcCliques cliques x
    when (length cliques < 6 && x < 10000) (loop xs cliques')
