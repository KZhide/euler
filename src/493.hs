import Data.List

combination m n = fact m `div` (fact n) `div` (fact (m-n))
  where
    fact 0 = 1
    fact n = n * fact (n-1)

ballPatterns n = combination 7 n * (sum . fmap pat) (divides n 20)

divides 1 n | n > 0 && n <= 10 = [[n]]
            | otherwise = []
divides d n = do
  r <- [1..10]
  rs <- divides (d-1) (n-r)
  return (r:rs)

pat = product . fmap (combination 10)

main = print $ (fromIntegral.sum) (fmap (\n -> n * ballPatterns n) [1..7]) / (fromIntegral.sum) (fmap ballPatterns [1..7])
