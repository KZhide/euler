import Data.List

digits n | n < 10 = [n]
         | otherwise = (n `mod` 10) : digits (n `div` 10)

buildNum (d:ds) = d + 10 * buildNum ds
buildNum [] = 0

reverseNum n = buildNum (reverse $ digits n)

palind n = n == reverseNum n

threeDigitMultis = concatMap (\n -> map (n*) [n..999]) [100..999]

sixDigitPalindromes = map (\n -> 1000 * n + (reverseNum n)) (reverse [100..999])
is3digitMult n = any (\i -> n `mod` i == 0 && n `div` i >= 100 && n `div` i < 1000) [100..999]

main = print $ find is3digitMult sixDigitPalindromes
