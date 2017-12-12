{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

import Primes
import Mod
import System.Environment
import GHC.TypeLits

digitLen = (+1) . floor . logBase 10.0 . fromIntegral

seqPair [] = []
seqPair [x] = []
seqPair (x:y:xs) = (x,y):seqPair (y:xs)

modInv :: (KnownNat m) => Modulo m -> Modulo m
modInv r | r == makeModulo 0 = error "attempt to get invert of 0"
         | otherwise = r^(natVal r - 2)

s p1 p2 = f p1 p2 * 10^digitLen p1 + p1
  where
    f p1 p2 =
      let p1' = makeModulo p1 :: Modulo (reifyNat p2 natVal) in
      fromIntegral $ -p1' * modInv (makeModulo (10^digitLen p1))

main = do
  args <- getArgs
  let bound = read (head args) :: Integer
  print $ sum $ fmap (uncurry s) $ takeWhile ((<=bound).fst) $ seqPair $ drop 2 primes
