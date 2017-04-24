import Primes

f = (==4) . length . primeFactors
conseq4 b [] = Nothing
conseq4 b [x] = Nothing
conseq4 b [x,y] = Nothing
conseq4 b [x,y,z] = Nothing
conseq4 b (x:y:z:w:xs) | not (b w) = conseq4 b xs
                       | not(b z) = conseq4 b (w:xs)
                       | not(b y) = conseq4 b (z:w:xs)
                       | not(b x) = conseq4 b (y:z:w:xs)
                       | otherwise = Just [x,y,z,w]

main = print $ conseq4 f [2..]
