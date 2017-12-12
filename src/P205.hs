import Control.Arrow
divides (l,h) 0 0 = [[]]
divides (l,h) 0 _ = []
divides (l,h) d n = do
  x <- [max l (n - h * (d-1)) .. min h (n - l * (d-1))]
  fmap (x:) (divides (l,h) (d-1) (n-x))

pWins = sum $ do
  (p, pnum) <- fmap (id &&& length.divides (1,4) 9) [9..36]
  (c, cnum) <- fmap (id &&& length.divides (1,6) 6) [6..36]
  [pnum * cnum | p > c]

totalPats = sum (fmap (length.divides (1,4) 9) [9..36]) * sum (fmap (length.divides (1,6) 6) [6..36])

main = do
  print $ fmap (id &&& length.divides (1,4) 9) [9..36]
  print $ fmap (id &&& length.divides (1,6) 6) [6..36]
  print $ fromIntegral pWins / fromIntegral totalPats
