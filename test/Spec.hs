{-# OPTIONS_GHC -freduction-depth=0 #-}
{-# LANGUAGE UndecidableInstances #-}

import Implemented
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
  Test :: (Testable n i) => Int -> A n -> Test

class MkTests (k :: [Nat]) where
  mkTests :: [Test]

instance MkTests '[] where
  mkTests = []

instance (Testable x i, MkTests xs) => MkTests (x ': xs) where
  mkTests = Test (reify @x) (mkA @x) : mkTests @xs


runTest :: M.Map Int [Integer] -> Test -> IO Bool
runTest !m (Test i a@(A as)) = do
  let Just (take 10->bs) = M.lookup i m
  res <- timeout 50000 $ pure $! and $ zipWith (==) as bs

  case res of
    Just False -> do putStrLn $ show a ++ " not equal spec!"; pure False
    Just True  -> pure True
    Nothing    -> do
      score <- bench 1000 a
      case score of
        0 -> putStrLn $ show a ++ " timed out!"
        _ -> putStrLn $ show a ++ " is slow. Score: " ++ show score
      pure $ score > 0

bench time (A as) = do
  r <- newIORef 0
  timeout time $! forM_ as \ !x -> modifyIORef' r (+ 1)
  readIORef r

parse' :: B.ByteString -> [Integer]
parse' = unfoldr $ B.readInteger . B.dropWhile \x -> x /= '-' && not (isDigit x)

parse :: B.ByteString -> (Int, [Integer])
parse (parse'->n:ns) = (fromIntegral n, ns)

getAll :: IO (M.Map Int [Integer])
getAll = M.fromList . map parse . B.lines <$> B.readFile "data/all"

main :: IO ()
main = do
  putStrLn ""
  res <- fmap and . forM tests . runTest =<< getAll
  unless res $ error ""

tests :: [Test]
tests = mkTests @Implemented

