import Data.List

data Edge v = Edge v v deriving (Eq, Show)
data Graph v w = Graph [(Edge v, w)] deriving (Eq, Show)

vertices (Graph l) = nub $ concatMap ((\(Edge v1 v2)->[v1,v2]).fst) l

mat :: [[a]] -> IArray (Integer, Integer) a
mat l = array ((0,0), (length(head l), length l) [()]

main = print $ Edge "a" "b"
