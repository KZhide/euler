modPow m n 0 = 1
modPow m n 1 = n `mod` m
modPow m n i = (modPow m n (i`div`2))^2 * modPow m n (i`mod`2) `mod` m

main = print $ (2^7830457 * 28433 + 1) `mod` (10^10)
