import Digits

isPalind b n = digitsWithBase b n == (reverse . digitsWithBase b) n
main = print $ (sum . filter (isPalind 2) . filter (isPalind 10)) [0..999999]
