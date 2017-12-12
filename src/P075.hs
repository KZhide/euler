coprime m n = gcd m n == 1
coprimes m = [(m,n)|n<-[1..m-1], odd (m-n), coprime m n]
primePitLen (m,n) = (m+n)*2*m

divisableByOneValue [] n = False
divisableByOneValue (x:xs) n | n `mod` x == 0 = all (\y->n`mod`y/=0) xs
                             | otherwise = divisableByOneValue xs n

divisables n = [n, n+n..1500000]

main = do
  let primePyths = filter (<=1500000) $ fmap primePitLen $ concatMap coprimes [1..floor (sqrt 750000.0)]
  print $ length $ filter (divisableByOneValue primePyths) [12,16..1500000]
