{-# LANGUAGE DataKinds, KindSignatures #-}

module Mod (makeModulo, Modulo) where

import GHC.TypeLits

data Modulo (m :: Nat) = Modulo Integer deriving (Eq)
instance KnownNat m => Show (Modulo m) where
  show r@(Modulo m) = show m ++ " (mod " ++ show (natVal r) ++ ")"

makeModulo :: KnownNat m => Integer -> Modulo m
makeModulo n = let r = Modulo (n `mod` natVal r) in r

instance KnownNat m => Num (Modulo m) where
  Modulo x + Modulo y = makeModulo (x+y)
  negate (Modulo x) = makeModulo (-x)
  Modulo x * Modulo y = makeModulo (x*y)
  abs = id
  fromInteger = makeModulo
  signum _ = 1

instance Ord (Modulo m) where
  compare (Modulo x) (Modulo y) | x == y = EQ
                                | x > y = GT
                                | otherwise = LT

instance (KnownNat m) => Real (Modulo m) where
  toRational (Modulo x) = toRational x


instance (KnownNat m) => Enum (Modulo m) where
  fromEnum (Modulo x) = fromInteger x
  toEnum = makeModulo.fromIntegral

instance (KnownNat m) => Integral (Modulo m) where
  quotRem (Modulo n) (Modulo d) = let (q, r) = quotRem n d in (makeModulo q, makeModulo r)
  toInteger (Modulo x) = x

main = print $ (makeModulo 10 :: Modulo 97)^96
