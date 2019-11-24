module Answers where

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Char (isDigit)
import Data.List (unfoldr)
import Data.Maybe


type Answers = M.Map Int [Integer]


getAll :: IO Answers
  = M.fromList
  . map parse
  . B.lines <$> B.readFile "data/all"


parse :: B.ByteString -> (Int, [Integer])
parse (parse'->n:ns)
  = (fromIntegral n, ns)

parse' :: B.ByteString -> [Integer]
  = unfoldr
  $ B.readInteger
  . B.dropWhile \x -> x /= '-' && not (isDigit x)

-------------------

parseInstances :: B.ByteString -> [Integer]
parseInstances
  = mapMaybe (fmap fst . B.readInteger . B.dropWhile (not . isDigit))
  . filter (B.isPrefixOf "instance OEIS ")
  . B.lines

primeInstances = parseInstances <$> B.readFile "src/OEIS/Prime.hs"


parseInstances' :: B.ByteString -> [Integer]
parseInstances'
  = mapMaybe (fmap fst . B.readInteger . B.dropWhile (not . isDigit))
  . filter (B.isPrefixOf "-- instance OEIS ")
  . B.lines

primeInstances' = parseInstances' <$> B.readFile "src/OEIS/Prime.hs"
