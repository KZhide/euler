import Data.List
import Digits

digitFifthPower = sum . fmap (^5) . digits
main = print $ sum $ filter (\i -> i == digitFifthPower i) [2..10^6]

