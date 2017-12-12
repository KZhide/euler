import System.Environment
import Data.IntMap.Strict
import Data.List(find)

data U = B Integer
       | R Integer
       deriving(Eq, Show)

patterns m n = let (bm, rm, bresult) = b m empty empty n in
             let (bm',rm',rresult) = r m bm rm n in
                 bresult + rresult
  where
    b :: Integer -> IntMap Integer -> IntMap Integer -> Integer -> (IntMap Integer, IntMap Integer, Integer)
    b m bm rm n | n <= m = (bm, rm, 1)
              | member (fromIntegral n) bm = (bm, rm, bm ! (fromIntegral n))
              | otherwise = let (bm',rm',result) = f m bm rm [0..n-1] in
                (insert (fromIntegral n) result bm', rm', result)
                  
    f :: Integer -> IntMap Integer -> IntMap Integer -> [Integer] -> (IntMap Integer, IntMap Integer, Integer)
    f m bm rm [] = (bm, rm, 0)
    f m bm rm (x:xs) = let (bm',rm',result) = f m bm rm xs in
                       let (bm'', rm'', rr) = r m bm' rm' x in
                         (bm'', insert (fromIntegral x) rr rm'', rr+result)

    r :: Integer -> IntMap Integer -> IntMap Integer -> Integer -> (IntMap Integer, IntMap Integer, Integer)
    r m bm rm n | n == 0 = (bm, rm, 1)
                | n < m = (bm, rm, 0)
                | n <= m*2 = (bm, rm, n-m+1)
                | member (fromIntegral n) rm = (bm, rm, rm ! (fromIntegral n))
                | otherwise = let (bm',rm',result) = g m bm rm [0..n-m] in
                  (bm', insert (fromIntegral n) result rm', result)

    g :: Integer -> IntMap Integer -> IntMap Integer -> [Integer] -> (IntMap Integer, IntMap Integer, Integer)
    g m bm rm [] = (bm, rm, 0)
    g m bm rm (x:xs) = let (bm',rm',result) = g m bm rm xs in
                       let (bm'', rm'', bb) = b m bm' rm' x in
                         (insert (fromIntegral x) bb bm'', rm'', bb+result)


main = do
  args <- getArgs
  (print.find ((>1000000).patterns 50)) [50,51..]
