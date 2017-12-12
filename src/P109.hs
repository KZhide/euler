import Data.List (nubBy, sort)
import Data.Function (on)

data DartField = S Int | D Int | T Int deriving (Eq, Ord, Show)

fields = fmap S (25:[1..20]) ++ fmap D (25:[1..20]) ++ fmap T [1..20]
finals = fmap D (25:[1..20])

score (S i) = i
score (D i) = 2*i
score (T i) = 3*i

checkouts 0 m = [[]]
checkouts len m = do
  f <- filter ((<m).score) finals
  fs <- nubBy ((==) `on` sort) $ checkouts' (len-1) (m - score f)
  return (f:fs)
  where
    checkouts' 0 _ = [[]]
    checkouts' len m | filter ((<m).score) fields == [] = [[]]
                     | otherwise = [] : do
                       f <- filter ((<m).score) fields
                       fs <- checkouts' (len - 1) (m - score f)
                       return (f:fs)

main = print $ length $ checkouts 3 100

