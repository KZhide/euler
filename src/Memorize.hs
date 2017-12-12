module Memorize where

memorize :: (Int -> a) -> (Int -> a)
memorize f= (map f [0..] !!)

memorize2 :: (Int -> Int -> a) -> (Int -> Int -> a)
memorize2 f = (!!) . (map (\a -> map (f a) [0..]) [0..] !!)
