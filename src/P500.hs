import Primes
import System.Environment
import Control.Arrow
import Data.Function
import Data.List

ans 0 = []
ans 1 = [(2,1)]
ans n = 
  let pfs = ans (n - 1) in
  let pRest = drop (length pfs) primes in
  let minP = minimumBy (compare `on` g) $ minPFG $ fmap (second (+1)) pfs in
  if g (head pRest, 1) < g minP
     then pfs ++ [(head pRest, 1)]
     else fmap (\(p, i) -> if fst minP == p then (p, (i+1)*2-1) else (p, i)) pfs
  where
    minPFG [] = []
    minPFG [(p, i)] = [(p, i)]
    minPFG ((p, 2):(q, 2):pfs) = [(p, 2)]
    minPFG ((p, i):(q, j):pfs) | i == j = (p, i): minPFG (dropWhile ((==i).snd) pfs)
                               | otherwise = (p,i): minPFG ((q, j):pfs)
  --a' * minimum (head pRest : fmap (\(p,i) -> p^(i+1)) pfs)


g (p, i) = p^i

main = do
  args <- getArgs
  let xsum = read (head args)
  print $ product $ fmap g $ ans xsum
