{-# LANGUAGE CPP #-}
module Main where

import Test
import Answers
import OEIS

import qualified Language.Haskell.TH as T
import Data.List


tests :: [Test]


#ifdef TEST_PRIME

#define TIME_LIMIT  10
#define NUM_SAMPLES 10

tests = sortOn testNum $(do
  is <- T.runIO $ primeInstances
  pure $ T.ListE
    [ T.AppTypeE (T.VarE 'mkTest) $ T.LitT $ T.NumTyLit n
    | n <- is
    ])

#endif
#ifdef TEST_FULL

#define TIME_LIMIT  10
#define NUM_SAMPLES 15

tests = sortOn testNum $(do
  T.ClassI _ is <- T.reify ''OEIS
  pure $ T.ListE
    [ T.AppTypeE (T.VarE 'mkTest) n
    | T.InstanceD _ _ (T.AppT _ n) _ <- is
    ])

#endif


-- Deadline for each individual test
timeLimit :: Int
timeLimit = 5000 * TIME_LIMIT

-- Maximum number of samples to test (per sequence)
numSamples :: Int
numSamples = NUM_SAMPLES

-- Instances defined in Prime.hs
implemented :: [Int]
implemented = map testNum tests

main :: IO ()
main = testMain tests timeLimit numSamples


-------


m = main

writeImpl :: IO ()
writeImpl = do
  writeFile "data/impl" $ unwords $ map show implemented

addImpl :: IO ()
addImpl = do
  is <- sort . union implemented . map read . words <$> readFile "data/impl"
  putStrLn $ show (length is) ++ " implemented"
  writeFile "data/impl" $ unwords $ map show is
