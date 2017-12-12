tri m n = [(m^2-n^2, 2*m*n, m^2+n^2), (2*m*n, m^2-n^2, m^2+n^2)]
primPyths m = (concatMap (tri m). filter (\n -> gcd m n == 1 && odd (m-n)))[1..m-1]

triangles = concatMap (\n -> [(n,n,n+1),(n,n,n-1)]) [3,5..100000`div`3]

isSquare n = ((==n).(^2).floor.sqrt.fromIntegral)n
hasIntegralArea (n,_,m) = isSquare (n^2-(m`div`2)^2)

main = print $ primPyths 4
