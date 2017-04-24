main = 
  readFile "p013_numbers.txt"
    >>= return . fmap (read :: String -> Integer) . lines
      >>= print . sum
