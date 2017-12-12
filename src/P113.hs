import Data.Function(fix)
import Memorize
import System.Environment(getArgs)

nonBouncyNumWithDigits d = f d 9 + f d 10 - 10
  where
    f = fix (memorize2 . f')
    f' g 0 _ = 1
    f' g d m = sum $ fmap (g $ d - 1) [1..m]


main = do
  args <- getArgs
  print $ sum $ fmap nonBouncyNumWithDigits [1..read (head args) :: Int]
