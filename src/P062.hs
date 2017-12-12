import Digits
import Data.List
import Data.Function
cubes = fmap (^3) [1..]

ordFilt b = takeWhile b . dropWhile (not.b)

inRange a b n = a <= n && n < b

main = print $ minimum $ concat $ filter ((==5).length)$ groupBy ((==) `on` toNum.sort.digits)$ sortBy (compare `on` toNum.sort.digits) $ ordFilt (inRange (10^11) (10^12)) cubes
