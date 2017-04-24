import Data.List
import Data.Function

primPyths = [(2*m*n,m*m-n*n,m*m+n*n)|m<-[2..250], n<-[1..m-1], tagainiso m n, odd (m-n), 2 * m * (m + n) <= 1000]
  where
    tagainiso m n = gcd m n == 1

solutionsNum n = length $ filter (\(a,b,c) -> n `mod` (a+b+c) == 0) primPyths
main = print $ maximumBy (compare `on` solutionsNum) [1..1000]
