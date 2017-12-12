import Digits
isSquare = (==)<$>((^2).floor.sqrt.fromIntegral)<*>id
sqrtDigits = f []
  where
    floorDigit g = (head.filter g) [9,8..0]
    f ds n = let d = floorDigit (\k->toNum(k:ds)^2 < (n*10^(length ds*2))) in
             d : f (d:ds) n


main = print $ sum $ concatMap (take 100.sqrtDigits)$ filter (not.isSquare) [1..100]

