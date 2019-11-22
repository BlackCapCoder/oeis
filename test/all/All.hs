import OEIS
import Test
import Answers

import qualified Language.Haskell.TH as T
import Data.List (sortOn)

import Control.Monad
import System.Exit



-- Deadline for each individual test
timeLimit :: Int
timeLimit = 50000 * 50

-- Maximum number of samples to test (per sequence)
numSamples :: Int
numSamples = 20


-- List of implemented sequences
implemented :: [Int]
implemented = map testNum tests


-- all instances of OEIS
tests :: [Test]
tests = sortOn testNum $(do
  T.ClassI _ is <- T.reify ''OEIS
  pure $ T.ListE
    [ T.AppTypeE (T.VarE 'mkTest) n
    | T.InstanceD _ _ (T.AppT _ n) _ <- is
    ])

main :: IO ()
main = testMain tests timeLimit numSamples

m = main
