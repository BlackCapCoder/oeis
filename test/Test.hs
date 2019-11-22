module Test where


import OEIS
import Answers

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad

import Data.List
import Data.IORef
import System.IO
import System.IO.Unsafe
import System.Timeout
import System.Exit



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


------------


data Component
  = List | Ix

data TestResult
  = SpecMismatch
  | Timeout
  | Slow Int

data Labeled
  = Labeled Int Component TestResult


report n = pure (n <> "\t") <> \case
  SpecMismatch -> "WRONG"
  Timeout      -> "TIMEOUT"
  Slow s       -> "slow: " ++ show s

report' (Labeled _ c r)
  = flip report r case c of
      List -> "     "
      Ix   -> " (ix)"


labelRes (Labeled _ _ x) = x

isOk (labelRes->Slow _) = True
isOk                 _  = False

isWrong (labelRes->SpecMismatch) = True
isWrong                 _  = False


ix :: forall n i. OEIS n => Integral i => A n -> [i]
ix _ = oeisIx @n <$> [0..]


runTest timeLimit numSamples (Test i a) !(M.lookup i->Just (take numSamples->bs)) =
  forM_ [(List, oeis' a), (Ix, ix a)] \(t, ~as) -> do
    (res, as') <- pure $ unsafePerformIO do
      cache <- newIORef []
      res <- timeout timeLimit $ and <$> zipWithM
          (\ !x y -> (x == y) <$ modifyIORef' cache (x :)) as bs
      liftM2 (,) (pure res) (readIORef cache)

    case res of
      Just True    -> Right ()
      Just False   -> Left $ Labeled i t SpecMismatch
      _ | null as' -> Left $ Labeled i t Timeout
        | let      -> Left $ Labeled i t $ Slow $ length as'

runTest' timeLimit numSamples m t@(Test i a) = case runTest timeLimit numSamples t m of
  Right _ -> pure True
  Left x  -> do
    Just ln <- findSrcLine $ testNum t
    putStrLn do (':' : show ln) <> ('\t' : show a) <> report' x
    when (isWrong x) do
      let Just s = M.lookup i m
      print $ take numSamples s
      print $ take numSamples $ oeis' a
      putStrLn ""
    pure $ isOk x


testMain tests timeout numSamples = do
  m   <- getAll
  res <- fmap and . forM tests $ (runTest' timeout numSamples) m
  unless res $ exitFailure


findSrcLine :: Int -> IO (Maybe Int)
findSrcLine n = do
  a <- findSrcLine' n "src/OEIS/Part1.hs"
  case a of
    Just _  -> pure a
    Nothing -> do
      a <- findSrcLine' n "src/OEIS/Part2.hs"
      case a of
        Just _  -> pure a
        Nothing -> findSrcLine' n "src/OEIS/Prime.hs"

findSrcLine' n pth = do
  let needle = B.pack $ "instance OEIS " ++ show n ++ " where"
  fmap succ . findIndex (B.isPrefixOf needle) . B.lines <$> B.readFile pth

----

exam :: forall n. (Testable n Int) => IO ()
exam = do
  let Test ix (A ~as) = mkTest @n @Int
  Just bs <- M.lookup ix <$> getAll
  hSetBuffering stdout NoBuffering
  forM_ (zip as bs) $ \(a, b) -> do
    if a == b then do
      putStr $ show b ++ ", "
    else do
      putStrLn $ "\nErr: " ++ show b ++ "\t" ++ show a
  putStrLn ""
