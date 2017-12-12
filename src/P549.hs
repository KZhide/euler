import System.Environment
import Control.Arrow
import Primes

pfOccurences p = scanl1 (\(n, fpn) -> second (+fpn)) $ fmap (\n -> (n, f p n)) [p, p+p ..]
  where
    f p n | n `div` p == 0 = 0
          | n `mod` p == 0 = 1 + f p (n `div` p)
          | otherwise = 0

s n = maximum $ (\(p,i) -> fst $ head $ dropWhile ((<i).snd) $ pfOccurences p) <$> primeFactors n

main = do
  n <- fmap (read.head) getArgs
  let r = scanl1 (\(m,r) -> second (+r)) $ fmap (\m -> (m, s m)) [2..n]
  mapM_ print $ filter ((==0).(`mod`(n`div`100)).fst) r
