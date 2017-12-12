-- (a + b√c) / d
data BinomialSurd = BinomialSurd Integer Integer Integer Integer deriving (Eq)
instance Show BinomialSurd where
  show (BinomialSurd a b c d) = "(" ++ show a ++ "+" ++ show b ++ "√" ++ show c ++ ")/" ++ show d


reciprocial (BinomialSurd a b c d) = let d' = a^2-b^2*c in 
                                     let a' = a*d in
                                     let b' = -b*d in
                                     let q = (a' `gcd` b' `gcd` d') * signum d' in
                                     BinomialSurd (a'`div`q) (b'`div`q) c (d'`div`q)


fl (BinomialSurd a b c d) = floor $ (fromInteger a + fromInteger b * (sqrt.fromInteger) c) / fromInteger d
fractPart (BinomialSurd a b c d) = BinomialSurd (a - fl (BinomialSurd a b c d) * d) b c d

a bs = (fl bs, fractPart bs) : a (reciprocial (fractPart bs))
root i = BinomialSurd 0 1 i 1
expansion bs = fmap fst $ head (a bs):rep (tail (a bs))
rep ((i,r):xs) = (i,r) : takeWhile (\(_,r')->r'/=r) xs

isSquare n = (floor.sqrt.fromInteger) n ^ 2 == n

main = print $ length$filter (even.length.expansion.root) $ filter (not.isSquare) [1..10000]
