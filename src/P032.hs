import Digits
import Data.List

ordUniq [] = True
ordUniq [x] = x/=0
ordUniq (x:y:xs) = x/=0 && x/=y && ordUniq (y:xs)

cond l = length l == 9 && (ordUniq.sort) l

b x y = cond ((digits x) ++ (digits y) ++ (digits (x*y)))

main = print $ sum $ nub $ fmap (uncurry (*)) $ filter (uncurry b) ([(x,y)|x<-[12..98],y<-[123..987]] ++ [(x,y)|x<-[2..9],y<-[1234..9876]])
