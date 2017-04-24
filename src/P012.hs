import Data.List(find, group)

triangleNum n = n * (n+1) `div` 2

primeFactors n = f [] 2 n
  where
    f factors p n | n == 1 = []
                  | n `mod` p == 0 = p:(f factors p (n `div` p))
                  | otherwise = f factors (p+1) n

dividers n = product $ map ((1+).length) (group (primeFactors n))

nums = ns 1
  where
    ns n = n:(ns (n+1))

main = print $ find (\x -> (dividers x) > 500) (map triangleNum nums)
