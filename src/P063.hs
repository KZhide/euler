import Data.List
nDigiPow n x = x^n >= 10^(n-1) && x^n < 10^n

nDigiPows n = filter (nDigiPow n) [1..9]

main = (print.sum.takeWhile (>0).fmap (length.nDigiPows)) [1..]
