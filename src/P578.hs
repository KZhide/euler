import Primes

floorSqrt = floor.sqrt.fromIntegral

decreasing :: (Ord a) => [a] -> Bool
decreasing [] = True
decreasing [x] = True
decreasing (x:y:xs) = x>=y && decreasing (y:xs)

decPrimePow = decreasing . fmap snd . primeFactors

powSup b n = f b n 0
  where
    f b n i | n^i > b = i-1
            | otherwise = f b n (i+1) 

decPows nBound iBound [] = 1
decPows nBound iBound (p:ps) | floorSqrt nBound < p = 1 + length (takeWhile (<=nBound) (p:ps))
                             | otherwise = (sum . fmap (\i -> decPows (nBound`div`(p^i)) (if i==0 then iBound else i) ps)) [0..min iBound (powSup nBound p)]

main = print $ decPows (10^2) 20 primes
