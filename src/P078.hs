import Data.List
sep sup n | n == 0 = 1
          | n < 0 = 0
          | otherwise = (sum.fmap (\i -> sep i (n-i))) [1..sup]

--main = print $ find ((>1000000).sep) [1..]
main = print $ fmap (\i -> sep i i ) [1..40]
