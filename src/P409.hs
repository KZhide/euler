import Data.List
import Data.Bits(xor)
import System.Environment
import System.IO
import Control.Concurrent(threadDelay)

vectors n = 2^n - 1
vectorBindings size = 2^size - 1
-- size個のn次元ベクトルの組で最小線形従属であるものの数
minimalDependentVectors n size = independentVectors n (size-1)
independentVectors n 0 = 1
independentVectors n 1 = vectors n
independentVectors n size = independentVectors n (size-1) * (vectors n - vectorBindings (size-1))

fact 0 = 1
fact n = n * fact (n-1)
c m n = fact m `div` fact n `div` fact (m-n)

winningPattern n 0 = 0
winningPattern n 1 = vectors n
winningPattern n 2 = independentVectors n 2
winningPattern n 3 = independentVectors n 3 
winningPattern n 4 = independentVectors n 4 + minimalDependentVectors n 3 * (vectors n - 3) * 4
winningPattern n 5 = independentVectors n 5 + minimalDependentVectors n 3 * (vectors n - 3) * (vectors n - 4) * c 5 2
winningPattern n size = independentVectors n size + losingPattern n (size-1) * (vectors n - (size-1))
losingPattern n 2 = 0
losingPattern n 3 = independentVectors n 2
losingPattern n 4 = independentVectors n 3
losingPattern n 5 = independentVectors n 4
losingPattern n size = winningPattern n (size-1)

main = mapM print $ fmap (\n -> winningPattern n n) [1..5]

