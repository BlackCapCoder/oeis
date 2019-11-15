module OEIS.OEIS
  ( module OEIS.OEIS
  , module GHC.TypeLits
  )
  where

import GHC.TypeLits
import Data.List (genericIndex)


data A (n :: Nat)

pattern A x <- (oeis'->x)

mkA :: forall (n :: Nat). A n
mkA = undefined


instance KnownNat n => Show (A n) where
  show = pure "A" <> show . natVal

predA :: forall (n :: Nat). A n -> A (n - 1)
predA _ = mkA

succA :: forall (n :: Nat). A n -> A (n + 1)
succA _ = mkA


class OEIS (n :: Nat) where
  oeis :: Integral i => [i]
  oeis = oeisIx @n <$> [0..]

  oeisIx :: Integral i => i -> i
  oeisIx ix = genericIndex (oeis' $ mkA @n) ix

  oeis' :: Integral i => A n -> [i]
  oeis' _ = oeis @n

fi = fromIntegral
