import Data.Ratio
zipWithTail _ a [] = a
zipWithTail _ [] a = a
zipWithTail f (x:xs) (y:ys) = f x y : zipWithTail f xs ys

data Poly a = P {terms :: [a]} deriving (Eq, Show)
instance (Num a) => Num (Poly a) where
  a + b = P $ zipWithTail (+) (terms a) (terms b)
  P [] * _ = P []
  P (x:xs) * P a = P (fmap (x*) a) + P xs * P (0:a)
  negate a = P $ fmap negate (terms a)
  fromInteger i = P [fromInteger i]

papp (P []) x = 0
papp (P (a:as)) x = a + x * papp (P as) x

x fs j = fst(fs!!j)
y fs j = snd(fs!!j)
l :: (Fractional a) => [(a,a)] -> Int -> Poly a
l fs j = product $ fmap (\k -> P [-x fs k / (x fs j - x fs k), 1 / (x fs j - x fs k)]) ([0..j-1]++[j+1..length fs - 1])
p fs = (sum.fmap (\j -> P [y fs j] * l fs j)) [0..length fs - 1]

u n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
us = fmap (\n -> (n%1,u n%1)) [1..]
bop n = p (take n us) `papp` (fromIntegral n + 1 %1)

main = do
  print $ sum $ fmap bop [1..10]

