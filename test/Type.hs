module Type
  ( module Type
  , module GHC.TypeLits
  ) where

import GHC.TypeLits


type family SpecT (a :: Nat) :: [N]

data N = Pos Nat | Neg Nat
