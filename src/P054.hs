import Data.List
import Text.Read.Lex as L
import Data.Char (isDigit)

data Suit = C|D|H|S
          deriving (Show, Eq, Ord, Read)

newtype Rank = Rank {val :: Int} deriving (Show, Eq)
instance Ord Rank where
  compare (Rank n) (Rank n') | n == 1 && n' /= 1 = GT
                             | n /= 1 && n' == 1 = LT
                             | otherwise = compare n n'

data Card = Card Rank Suit
          deriving (Show, Eq, Ord)

instance Read Card where
  readsPrec _ (r:s:rest) =
    if isDigit r
       then [(Card (Rank $ read [r]) (read [s] :: Suit), rest)]
       else case r of
                 'A' -> [(Card (Rank 1) (read [s] :: Suit), rest)]
                 'T' -> [(Card (Rank 10) (read [s] :: Suit), rest)]
                 'J' -> [(Card (Rank 11) (read [s] :: Suit), rest)]
                 'Q' -> [(Card (Rank 12) (read [s] :: Suit), rest)]
                 'K' -> [(Card (Rank 13) (read [s] :: Suit), rest)]
                 _ -> []

rank (Card n _) = n
suit (Card _ s) = s

data Hand = HighCard
          | OnePair Rank
          | TwoPairs Rank Rank
          | ThreeOfAKind Rank
          | Straight
          | Flush
          | FullHouse Rank
          | FourOfAKind Rank
          | StraightFlush
          | RoyalFlush
          deriving (Show, Eq, Ord)

hand l | fmap (val . rank) l == [10,11,12,13,1] && isFlush l = RoyalFlush
       | isStraight l && isFlush l = StraightFlush
       | fmap length (sortedRankGroup l) == [1,4] = 
          FourOfAKind $ head $ sortedRankGroup l !! 1
       | fmap length (sortedRankGroup l) == [2, 3] =
          FullHouse $ head $ sortedRankGroup l !! 1
       | isFlush l = Flush
       | isStraight l = Straight
       | fmap length (sortedRankGroup l) == [1,1,3] =
          ThreeOfAKind $ head $ sortedRankGroup l !! 2
       | fmap length (sortedRankGroup l) == [1,2,2] =
          TwoPairs (head $ sortedRankGroup l !! 2) (head $ sortedRankGroup l !! 1)
       | fmap length (sortedRankGroup l) == [1,1,1,2] =
          OnePair (head $ sortedRankGroup l !! 3)
       | otherwise = HighCard
  where
    rankGroup = group . fmap rank
    sortedRankGroup = sortOn length . rankGroup
    isStraight = sequencial . fmap (val . rank)
    sequencial [] = True
    sequencial [x] = True
    sequencial (x:y:xs) | x + 1 == y = sequencial (y:xs)
                        | otherwise = False
    isFlush l = length (group (fmap suit l)) == 1

isP1Win (l1, l2) = check (sort l1) (sort l2)
  where
    check l1 l2 | hand l1 < hand l2 = False
                | hand l1 > hand l2 = True
                | otherwise = f (reverse l1) (reverse l2)
    f [] [] = False
    f (x:xs) (y:ys) | x == y = f xs ys
                    | otherwise = x > y


main = do
  content <- readFile "p054_poker.txt"
  let matches = fmap (splitAt 5 . fmap read . words) (lines content) :: [([Card], [Card])] in
--    mapM_ print (fmap (\(l1,l2) -> (hand (sort l1), hand (sort l2), isP1Win (l1,l2))) matches)
    print (length (filter isP1Win matches))

