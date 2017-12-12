import Data.Maybe
import Data.List
import Data.Ratio
import Data.Function

main = print $ sortBy (compare `on` negate) $ concatMap (\n -> (fmap (%n).maybeToList.find ((==1).gcd n)) (cands n)) [2..1000000]
  where
    cands n = [n*3`div`7, n*3`div`7-1..1]

