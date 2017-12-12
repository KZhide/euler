import Data.List
data E = E {s::Int, t::Int, w::Int} deriving (Eq, Show)
f (weights, verticles) e | s e `elem` verticles && t e `elem` verticles = (weights, verticles)
                         | s e `elem` verticles = (weights + w e, t e : verticles)
                         | t e `elem` verticles = (weights + w e, s e : verticles)
                         | otherwise = (weights + w e, s e : t e : verticles)

data T = T {vs::[Int], wsum::Int} deriving (Eq, Show)
combine e t1 t2 = T (vs t1 `union` vs t2 `union` [s e, t e]) (wsum t1 + wsum t2 + w e)

g = [E 1 2 16, E 1 4 21, E 1 3 12, E 2 4 17, E 2 5 20, E 3 4 28, E 3 6 31, E 4 5 18, E 4 6 19, E 4 7 23, E 5 7 11, E 6 7 27]

main = print $ foldl' f (0,[]) g
