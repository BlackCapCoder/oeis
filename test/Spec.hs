{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE UndecidableInstances #-}

import OEIS

import Control.Monad
import System.Timeout
import GHC.TypeLits
import Data.Proxy

import Data.List
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char
import Data.IORef
import System.IO.Unsafe
import System.Exit

import qualified Language.Haskell.TH as T




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


data Component
  = List | Ix

data TestResult
  = SpecMismatch
  | Timeout
  | Slow Int

data Labeled
  = Labeled Int Component TestResult


report n = pure n <> \case
  SpecMismatch -> " not equal to spec!"
  Timeout      -> " timed out!"
  Slow s       -> " is slow. Score: " ++ show s

report' (Labeled n c r)
  = flip report r case c of
      List -> show n
      Ix   -> show n <> " (ix)"


labelRes (Labeled _ _ x) = x

isOk (labelRes->Slow _) = True
isOk                 _  = False


ix :: forall n i. OEIS n => Integral i => A n -> [i]
ix _ = oeisIx @n <$> [0..]


runTest (Test i a) !(M.lookup i->Just (take 10->bs)) =
  forM_ [(List, oeis' a), (Ix, ix a)] \(t, as) -> do
    (res, as') <- pure $ unsafePerformIO do
      cache <- newIORef []
      res <- timeout 50000 $! and <$> zipWithM
          (\x y -> (x == y) <$ modifyIORef' cache (y :)) as bs
      liftM2 (,) (pure res) (readIORef cache)

    case res of
      Just True    -> Right ()
      Just False   -> Left $ Labeled i t SpecMismatch
      _ | null as' -> Left $ Labeled i t Timeout
        | let      -> Left $ Labeled i t $ Slow $ length as'

runTest' :: M.Map Int [Integer] -> Test -> IO Bool
runTest' m t@(Test _ a) = case runTest t m of
  Right _ -> pure True
  Left x  -> isOk x <$ putStrLn do report' x

parse' :: B.ByteString -> [Integer]
parse' = unfoldr $ B.readInteger . B.dropWhile \x -> x /= '-' && not (isDigit x)

parse :: B.ByteString -> (Int, [Integer])
parse (parse'->n:ns) = (fromIntegral n, ns)

getAll :: IO (M.Map Int [Integer])
getAll = M.fromList . map parse . B.lines <$> B.readFile "data/all"

main :: IO ()
main = do
  putStrLn ""
  m <- getAll
  res <- fmap and . forM tests $ runTest' m
  unless res $ exitFailure

