import Data.List
import Digits

pandigital l ds = sort l == sort ds
pandigitalMult4 n = (pandigital [1..9] . concatMap digits)[2*n,n]

main = print $ fmap (\n -> n*10^5+2*n) $ filter pandigitalMult4 [9123..9876]
