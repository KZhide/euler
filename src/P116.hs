import Data.Function (fix)

memorize :: (Int -> a) -> (Int -> a)
memorize f= (map f [0..] !!)

tilePat :: Int -> (Int -> Int) -> Int -> Int
tilePat t f len | len < t = 1
                | len == t = 2
                | otherwise = f (len-t) + f (len-1)

memred :: Int -> Int
memred = pred.fix (memorize . tilePat 2)
memgreen = pred.fix (memorize . tilePat 3)
memblue = pred.fix (memorize . tilePat 4)

main = do
  print $ memred 50 + memgreen 50 + memblue 50
  --print 50
