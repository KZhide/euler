import Data.Ratio
import Data.Function
import Data.List
import Primes
import System.Environment

data BinomialSurd = BinomialSurd {a::Rational, b::Rational, r::Integer} deriving (Eq)
instance Show BinomialSurd where
  show s = "(" ++ show (a s) ++ "+" ++ show (b s) ++ "√" ++ show (r s) ++ ")"

reciprocial (BinomialSurd a' b' r') = let d = a'^2 - b'^2 * (r'%1) in
                                      let a'' = a' / d in
                                      let b'' = - b' / d in
                                      BinomialSurd a'' b'' r'


fl s = floor $ (fromRational.a) s + (fromRational.b) s * (sqrt.fromInteger.r) s
fractPart s = BinomialSurd (a s - fromIntegral(fl s)) (b s) (r s)

renbunsu bs = (fl bs, fractPart bs) : renbunsu (reciprocial (fractPart bs))
root = BinomialSurd 0 1
expansion bs = fmap fst $ head (renbunsu bs):rep (tail (renbunsu bs))
rep ((i,r):xs) = (i,r) : takeWhile (\(_,r')->r'/=r) xs

isSquare n = (floor.sqrt.fromInteger) n ^ 2 == n

kinjiFract :: BinomialSurd -> Ratio Integer
kinjiFract = f.dropLast.expansion
  where
    f [x] = fromInteger x
    f (x:xs) = fromInteger x + fromInteger 1 / f xs

dropLast [x] = []
dropLast (x:xs) = x:dropLast xs
dropLast [] = []

minimalAns d | (odd.length.expansion.root)d = let k = kinjiFract (root d) in (numerator k, denominator k)
             | otherwise = let k = kinjiFract (root d) in (numerator k^2 + denominator k^2 * d, 2*numerator k*denominator k)

main = do
  args <- getArgs
  let bound = read(head args)::Integer
  print $ maximumBy (compare`on`fst.minimalAns) $ [1..bound] `minus` [x^2|x<-[1..]]
