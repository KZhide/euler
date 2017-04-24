infixl 3 <^
infixl 3 ^>
(<^) :: Functor f => f a -> (a -> b) -> f b
(<^) = flip (<$>)
(^>) :: Applicative f => f (a -> b) -> f a -> f b
(^>) = (<*>)

subseqs4 :: [a] -> [[a]]
subseqs4 (x:y:z:w:xs) = [x,y,z,w]:subseqs4 (y:z:w:xs)
subseqs4 _ = []
horizontal4 :: [[a]] -> [[a]]
horizontal4 = concatMap subseqs4

verticals :: (Eq a) => [[a]] -> [[a]]
verticals ls | elem [] ls = []
             | otherwise = fmap head ls : verticals (fmap tail ls)
verticals4 :: (Eq a) => [[a]] -> [[a]]
verticals4 = concatMap verticals . subseqs4

diagonalHead :: [[a]] -> [a]
diagonalHead [] = []
diagonalHead (l:ls) = head l : (diagonalHead . fmap tail) ls

diagonals :: [[a]] -> [[a]]
diagonals ls | any ((length ls >) . length) ls = []
             | otherwise = diagonalHead ls : (diagonals . fmap tail) ls

diagonals4 =
  concatMap diagonals . subseqs4 <^(++)^> concatMap diagonals . subseqs4 . reverse

adjuscent4 :: Eq a => [[a]] -> [[a]]
adjuscent4 = horizontal4 <^(++)^> verticals4 <^(++)^> diagonals4

main = readFile "p011_grid.txt"
  >>= print . maximum . fmap product . adjuscent4 . fmap (fmap readInt . words) . lines
  where
    readInt = read :: String -> Int
