import Digits
import Data.Function

fact n | n == 0 = 1
       | otherwise = n * fact (n-1)

digifact = sum.fmap fact.digits

nonRepeatChain :: (Eq a) => (a -> a) -> a -> [a]
nonRepeatChain = c []
  where
    c chains f n | n `elem` chains = chains
                 | otherwise = c (n:chains) f (f n)

digitGeq :: Integer -> [Integer]
digitGeq n = [n..9]

f :: Integer -> [[Integer]] -> [[Integer]]
f = fix $ \loop i l -> do
  if i > 0 then do
    m <- digitGeq (if null l then 0 else head l)
    return $ loop (i-1) (m:l)
  else
    return []

main = print $ filter ((==60).length.nonRepeatChain digifact) [1..100000]
