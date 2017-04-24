import Data.List(nub)
l = [(a,b)|a <- [2..100], b <- [2..100]]
main = print $ (length . nub . fmap (uncurry (^))) l

