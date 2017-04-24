my_gcd a b | a < b = my_gcd b a
        | a `mod` b == 0 = b
        | otherwise = my_gcd b (a `mod` b)

my_lcm a b = a * b `div` (my_gcd a b)

main = print $ foldr my_lcm 1 [1..20]
