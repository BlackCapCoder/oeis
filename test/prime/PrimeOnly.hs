import OEIS
import Test
import Answers

import qualified Language.Haskell.TH as T
import Data.List (sortOn)


-- Reloading all the insances are a bit too slow for comfort in ghci
-- This file only tests the instances in `src/OEIS/Prime.hs`



-- Deadline for each individual test
timeLimit :: Int
timeLimit = 50000 * 1

-- Maximum number of samples to test (per sequence)
numSamples :: Int
numSamples = 10

-- Instances defined in Prime.hs
implemented :: [Int]
implemented = map testNum tests


tests :: [Test]
tests = sortOn testNum $(do
  is <- T.runIO $ primeInstances
  pure $ T.ListE
    [ T.AppTypeE (T.VarE 'mkTest) $ T.LitT $ T.NumTyLit n
    | n <- is
    ])

main :: IO ()
main = testMain tests timeLimit numSamples

m = main
