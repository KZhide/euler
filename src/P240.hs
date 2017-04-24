import Control.Arrow
import Data.List

divisions :: Integer -> Integer -> Integer -> [[Integer]]
divisions sup n k | n == 0 = [replicate (fromIntegral k) 0]
                  | k == 0 = [[]]
                  | k == 1 && n <= sup = [[n]]
                  | k == 1 && n > sup = []
                  | otherwise = do
                    c <- [0..sup]
                    l <- divisions c (n-c) (k-1)
                    return (c:l)


groupWithLen :: (Eq a) => [a] -> [(a,Integer)]
groupWithLen = fmap (head &&& fromIntegral . length) . group

topDices :: [[(Integer, Integer)]]
topDices = (fmap ((+1).fst &&& snd) . groupWithLen) <$> divisions 11 60 10

fact n | n == 0 = 1
       | otherwise = n * fact (n-1)

dupFactor = product . fmap fact

dupPerm :: [(a,Integer)] -> Integer
dupPerm = (\ll -> (fact . sum) ll `div` dupFactor ll) . fmap snd
  where
    fact n | n == 0 = 1
           | otherwise = n * fact (n-1)

main = print topDices
--main = print $ dupPerm [("a",2),("b",2)]
