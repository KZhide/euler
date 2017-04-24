-- n == (a+b)^2 + a * b
-- 補題: 2 <= a+b <= sqrt n
-- 補題: d^2 +2d - 2n > 0

iSqrt = sqrt.fromIntegral
floorSqrt = floor.sqrt.fromIntegral

hasSol n alpha = let d = 5 * alpha^2 - 4 * n in (floorSqrt d)^2 == d
sols n = length (filter (hasSol n) [ceiling(iSqrt n * 2 / iSqrt 5)..floorSqrt (n-1)])

cond k b = let d = 5*b^2+4*k in (floorSqrt d)^2 == d
sols' k | odd k = length (filter (cond k) [1..floorSqrt (k `div` 5)])
        | even k = length (filter (cond k) [2,4..floorSqrt (k`div`5)])

satisf r n = f r [ceiling(iSqrt n * 2 / iSqrt 5)..floorSqrt (n-1)]
  where
    f r [] = r == 0
    f r (x:xs) | r == 0 && hasSol n x = False
               | hasSol n x = f (r-1) xs
               | otherwise = f r xs

main = print $ length $ filter (satisf 4) [1..10^5-1]
--main = print $ length $ filter (\n -> sols n == 6) [1..10^6-1]
--main = print $ sols 209
