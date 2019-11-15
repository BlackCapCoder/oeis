{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE UndecidableInstances #-}

import Type
import Instances
import Implemented
import OEIS

import Control.Monad
import System.Timeout
import Data.Proxy


class Reify a b where
  reify :: b

instance Reify '[] [a] where
  reify = []

instance (Reify x a, Reify xs [a]) => Reify (x ': xs) [a] where
  reify = reify @x : reify @xs

instance (KnownNat n, Integral i) => Reify (n :: Nat) i where
  reify = fromIntegral . natVal $ Proxy @n

instance (KnownNat n, Integral i) => Reify ('Pos n) i where
  reify = reify @n

instance (KnownNat n, Integral i) => Reify ('Neg n) i where
  reify = negate $ reify @n


type Testable (n :: Nat) i
  = (KnownNat n, OEIS n, Integral i, Reify (SpecT n) [i])

data Test where
  Test :: (Testable n i) => A n -> [i] -> Test

class MkTests (k :: [Nat]) where
  mkTests :: [Test]

instance MkTests '[] where
  mkTests = []

instance (Testable x i, MkTests xs) => MkTests (x ': xs) where
  mkTests = Test (mkA @x) (reify @(SpecT x) @[i]) : mkTests @xs

runTest :: Test -> IO Bool
runTest (Test a@(A as) bs) = do
  res <- timeout 100000 $ pure $! and $ zipWith (==) as $ take 10 bs

  case res of
    Nothing    -> do putStrLn $ show a ++ " timed out!"; pure True
    Just False -> do putStrLn $ show a ++ " not equal spec!"; pure False
    Just True  -> pure True

main :: IO ()
main = do
  putStrLn []
  res <- and <$> mapM runTest tests
  unless res $ error ""

tests :: [Test]
tests = mkTests @Implemented

