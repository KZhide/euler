import Primes
import Data.List
import Digits
data D = V Integer | W
       deriving (Eq,Show)

subs [] _ = []
subs ((V v):ds) s = v:subs ds s
subs (W:ds) s = s:subs ds s

ds = W : fmap V [0..9]

nums len = sequence (replicate len ds)
familySize num = (length.nub.filter isPrime.fmap toNum.filter((/=0).last).fmap(subs num)) [0..9]
family num = (nub.filter isPrime.fmap toNum.filter((/=0).last).fmap(subs num)) [0..9]


main = print $ find ((==8).lengt://github.com/KZhide/dotfiles.gitub.filter isPrime.fmap toNum.filter((/=0).last).fmap(subs num)) [0..9]
h) $ fmap family $ nums 6
