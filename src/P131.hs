import Primes

intCurt n = ceiling $ (fromIntegral n)**(1/3)
cubeNum n = any ((==n).(^3)) [intCurt n, intCurt n - 1]

cubes = fmap (^3) [1..]
cubeDiff = do
  c <- cubes
  diffs c cubes
diffs = (.) (reverse . takeWhile (>0)) . fmap . (-)

sortedInfiniteUnion (l:ls) = takeWhile (<head(head ls)) l ++ union (dropWhile (<head(head ls)) l) (sortedInfiniteUnion ls)
cubeDiff' = fmap (`diffs` cubes) cubes

main = print $ length $ takeWhile (<1000000) $ filter isPrime $ sortedInfiniteUnion cubeDiff'
