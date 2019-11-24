module Test where


import OEIS
import Answers

import GHC.TypeLits
import Data.Proxy
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent

import Data.Maybe
import Data.Either
import Data.Monoid hiding (getAll)

import Data.Foldable
import Data.List
import System.IO
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

ix :: forall n i. OEIS n => Integral i => A n -> [i]
ix _ = oeisIx @n <$> [0..]


------------


data Meta
   = Meta Test Component [Integer] [Integer] Mistake

data Component
  = List
  | Ix

data Mistake
  = WRONG
  | SLOW
  | TIMEOUT

instance Show Mistake where
  show WRONG   = "\10060"
  show SLOW    = "\128012"
  show TIMEOUT = "\129482"

instance Show Component where
  show List = "    "
  show Ix   = "(ix)"

instance Show Meta where
  show (Meta (Test _ a) tag xs ys m)
      = show m <> " " <> show a <> " " <> show tag <> "\n"
     <> if | [x, y] <- align (map show xs) (map show ys)
           -> unlines ["~  " <> x, "=  " <> y]
           | let
           -> ""

underline x = "\ESC[4m"  <> x <> "\ESC[24m"
red       x = "\ESC[31m" <> x <> "\ESC[39m"


align a b
  | (a', b') <- unzip $ zipWith f a b
  = map unwords $ hilight [a', b']
  where
    f [] xs = (' ' <$ xs, xs)
    f xs [] = (xs, ' ' <$ xs)
    f (x:xs) (y:ys) = ([x], [y]) <> f xs ys

hilight = transpose . map f . transpose
  where
    f xs@[a,b]
      | a /= b = map red xs
      | let    = xs

----------


data TestEnv = TestEnv
  { timeLimit'  :: Int
  , sampleSize' :: Int
  , spec        :: Answers
  }


runTest :: Test -> TestEnv -> IO (Maybe Meta)
runTest test@(Test i a) (TestEnv lim ss sp@(M.lookup i -> Just (take ss -> bs))) = do
  fmap getAlt $
    flip foldMap [(List, oeis' a), (Ix, ix a)] \(tag, xs) -> do
      (xs', e) <- takeTimed lim ss xs
      pure $ Meta test tag xs' bs <$> fold
        [ WRONG   <$ guard do or $ zipWith (/=) xs' bs
        , TIMEOUT <$ guard do null xs'
        , SLOW    <$ guard do not e
        ]

runTests :: TestEnv -> [Test] -> IO [Meta]
runTests env = fmap catMaybes . mapM do flip runTest env


testMain tests timeout numSamples = do
  env <- TestEnv timeout numSamples <$> getAll
  putStrLn =<< unlines . fmap show  <$> runTests env tests

  -- unless res $ exitFailure


----


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


takeTimed :: Int -> Int -> [a] -> IO ([a], Bool)
takeTimed lim ss (take ss -> ~as) = do
  ch <- newChan

  forkIO $
    writeChan ch . maybe (Left False) (const $ Left True)
      =<< timeout lim do forM_ as \ !x -> writeChan ch $ Right x

  (xs, ~(Left b:_)) <- span isRight <$> getChanContents ch

  pure (fromRight undefined <$> xs, b)


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


-- runTest' timeLimit numSamples m t@(Test i a)
--   = case runTest timeLimit numSamples t m of
--   -- = case runTestM (runTest'' t) $ TestEnv timeLimit numSamples m of
--       Right _ -> pure True
--       Left x  -> do
--         Just ln <- findSrcLine $ testNum t
--         putStrLn do (':' : show ln) <> ('\t' : show a) <> report' x
--         when (isWrong x) do
--           let Just s = M.lookup i m
--           print $ take numSamples s
--           print $ take numSamples $ oeis' a
--           putStrLn ""
--         pure $ isOk x


