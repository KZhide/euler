import Data.Function (fix)

memorize :: (Int -> a) -> (Int -> a)
memorize f= (map f [0..] !!)

tilePat :: (Int -> Int) -> Int -> Int
tilePat f len | len == 1 = 1
              | len == 2 = 2
              | len == 3 = 4
              | len == 4 = 8
              | otherwise = f (len-1) + f (len-2) + f (len-3) + f (len-4)

memmix = fix(memorize.tilePat)

main = do
  print $ memmix 5
  print $ memmix 50
  --print 50
