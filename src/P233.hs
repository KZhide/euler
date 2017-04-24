floorSqrt :: Integer -> Integer
floorSqrt = truncate . sqrt .fromIntegral
isSquare n = floorSqrt n ^ 2 == n

intCoordOn n = f n * 8 - 4
  where
    f n = length $ filter (b n) [ceiling (fromIntegral n/2).. n]
    b n y = let xx = 2*n^2 - (2*y-n)^2 in isSquare xx && even (floorSqrt xx + n)

main = print $ intCoordOn (10^11)
