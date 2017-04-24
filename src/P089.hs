minimalRoman n | n >= 1000 = (replicate (n`div`1000) 'M') ++ minimalRoman (n `mod` 1000)
               | n >= 900 = "CM" ++ minimalRoman (n `mod` 100)
               | n >= 500 = "D" ++ minimalRoman (n `mod` 500)
               | n >= 400 = "CD" ++ minimalRoman (n `mod` 100)
               | n >= 100 = (replicate (n `div` 100) 'C') ++ minimalRoman (n `mod` 100)
               | n >= 90 = "XC" ++ minimalRoman (n `mod` 10)
               | n >= 50 = "L" ++ minimalRoman (n `mod` 50)
               | n >= 40 = "XL" ++ minimalRoman (n `mod` 10)
               | n >= 10 = (replicate (n `div` 10) 'X') ++ minimalRoman (n `mod` 10)
               | n >= 9 = "IX" ++ minimalRoman (n `mod` 1)
               | n >= 5 = "V" ++ minimalRoman (n `mod` 5)
               | n >= 4 = "IV" ++ minimalRoman (n `mod` 1)
               | n >= 1 = (replicate (n `div` 1) 'I') ++ minimalRoman (n `mod` 1)
               | otherwise = ""

readRoman [] = 0
readRoman ('M':s) = 1000 + readRoman s
readRoman ('C':'M':s) = 900 + readRoman s
readRoman ('D':s) = 500 + readRoman s
readRoman ('C':'D':s) = 400 + readRoman s
readRoman ('C':s) = 100 + readRoman s
readRoman ('X':'C':s) = 90 + readRoman s
readRoman ('L':s) = 50 + readRoman s
readRoman ('X':'L':s) = 40 + readRoman s
readRoman ('X':s) = 10 + readRoman s
readRoman ('I':'X':s) = 9 + readRoman s
readRoman ('V':s) = 5 + readRoman s
readRoman ('I':'V':s) = 4 + readRoman s
readRoman ('I':s) = 1 + readRoman s

split b l = case break b l of
  (l1,[]) -> [l1]
  (l1,l2) -> l1 : split b (tail l2)

main = readFile "p089_roman.txt" >>= print . sum. fmap ((-)<$>length<*>length.minimalRoman.readRoman) . split (=='\n')
               
