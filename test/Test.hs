module Test where

import OEIS

import GHC.TypeLits
import Data.Proxy
import qualified Language.Haskell.TH as T
import Data.List


class Reify a b where
  reify :: b

instance Reify '[] [a] where
  reify = []

instance (Reify x a, Reify xs [a]) => Reify (x ': xs) [a] where
  reify = reify @x : reify @xs

instance (KnownNat n, Integral i) => Reify (n :: Nat) i where
  reify = fromIntegral . natVal $ Proxy @n



type Testable (n :: Nat) i
  = (KnownNat n, OEIS n, Integral i)

data Test where
  Test :: (Testable n i) => { testNum :: Int, testSeq :: A n } -> Test


mkTest :: forall n i. Testable n i => Test
mkTest = Test (reify @n) (mkA @n)


-- all instances of OEIS
tests :: [Test]
tests = sortOn testNum $(do
  T.ClassI _ is <- T.reify ''OEIS
  pure $ T.ListE
    [ T.AppTypeE (T.VarE 'mkTest) n
    | T.InstanceD _ _ (T.AppT _ n) _ <- is
    ])

-- List of implemented sequences
implemented :: [Int]
implemented = map testNum tests

