module OEIS.Simple () where

-- All instances in this file are self contained

import OEIS.Common

import Data.Bits
import qualified Data.Set as S
import Data.Numbers.Primes hiding (isPrime)
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.Powers.Squares.Internal
import Data.Ratio
import Control.Monad
import Data.Function (fix, on)
import Data.Char (intToDigit, digitToInt)
import Math.NumberTheory.Moduli (powMod)
import Math.NumberTheory.Primes (factorise, unPrime)
import qualified Math.NumberTheory.ArithmeticFunctions as A
import Math.NumberTheory.Recurrences
import Data.Maybe
import Data.Ord (Down (..), comparing)
import Data.Proxy
import Data.Tuple
import qualified Data.List.Ordered as O
import qualified Data.Map as M
import Numeric (showIntAtBase)
import Data.MemoCombinators (memo2, memo3, integral, list, Memo)

import qualified Data.List as L
import Data.List hiding
  ( drop
  , (!!)
  , length
  , replicate
  , splitAt
  , take
  , findIndices
  , elemIndices
  , findIndex
  , elemIndex
  )
import Prelude hiding
  ( drop
  , (!!)
  , length
  , replicate
  , splitAt
  , take
  , findIndices
  )

drop      = genericDrop
(!!)      = genericIndex
length    = genericLength
replicate = genericReplicate
splitAt   = genericSplitAt
take      = genericTake
findIndices f = map fi . L.findIndices f
elemIndices x = map fi . L.elemIndices x
findIndex f = fmap fi . L.findIndex f
elemIndex x = fmap fi . L.elemIndex x


tSegments = transpose $ unfoldr (Just . splitAt 3) $ tail (oeis @78783)

(oeis78783, (oeis117073)) = unzip $
    (0,0) : (1,1) : (3,2) : f 3 2 (2:[4..]) where
    f a d ms@ (m:_) = (a', d') : f a' d' (delete a' ms) where
      (a', d') = if i > d then (m, i) else (a + d + 1, d + 1)
      i = a - m

oeis37372_37407 a b
  = filter f [1..] where
      f x = null $ nub (ds a x) \\ nub (ds b x)
      ds b x = if x > 0 then d : ds b x' else []  where (x', d) = divMod x b

(oeis22328, (oeis22329)) = unzip $ f $ S.singleton (1, (0, 0)) where
    f s = (i, j) :
          f (S.insert (2 * y, (i + 1, j)) $ S.insert (3 * y, (i, j + 1)) s')
          where ((y, (i, j)), s') = S.deleteFindMin s

say :: Integer -> Integer
say = read . concatMap saygroup . group . show
  where saygroup s = (show $ length s) ++ [head s]
look_and_say :: [Integer]
look_and_say = 1 : map say look_and_say

interleave (hdx : tlx) y = hdx : interleave y tlx
-- interleave' xs ys = concat . transpose $ [xs,ys]

oeis003602 = interleave [1..] oeis003602
oeis181988 = interleave [1..] (zipWith (+) oeis003602 oeis181988)



instance OEIS 2 where
  oeis = let a = 1:2: drop 2 (concat . zipWith (replicate . fi) a . cycle $ [1, 2]) in a

instance OEIS 4 where
  oeis   = repeat 0
  oeisIx = const 0

instance OEIS 7 where
  oeis   = 1 : repeat 0
  oeisIx = (0 ^)

instance OEIS 9 where
  oeis = map (pM 1) [0..] where
     pM = memo2 integral integral p
     p _ 0 = 1
     p k m | m < k     = 0
           | otherwise = pM (k + 1) (m - k) + pM (k + 1) m

instance OEIS 10 where
  oeisIx (succ->n) = totient n

instance OEIS 12 where
  oeis   = repeat 1
  oeisIx = const 1

instance OEIS 23 where
  oeisIx n = foldl g 1 [1..n]
    where g n m = n*m + (-2)^m

instance OEIS 27 where
  oeis   = [1..]
  oeisIx = succ

instance OEIS 30 where
  oeisIx = until (< 10) (`div` 10)

instance OEIS 32 where
  oeis = let r = 2 : 1 : do zipTail (+) r in r

instance OEIS 34 where
  oeis   = cycle [1,2]
  oeisIx = succ . (`mod` 2)

instance OEIS 35 where
  oeis   = cycle [0,1]
  oeisIx = (`mod` 2)

instance OEIS 38 where
  oeis     = 2 : repeat 0
  oeisIx 0 = 2
  oeisIx n = 0

instance OEIS 40 where
  oeis = primes

instance OEIS 41 where
  oeis = map (p' 1) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 1) m

instance OEIS 42 where
  oeis     = iterate (\x -> 10*x + 1) 1
  oeisIx (succ->n) = (10 ^ n - 1) `div` 9

-- TODO: Lucas Lehmer
instance OEIS 43 where
  oeis = [ i | (i, n) <- iterate (\ (i,n) -> (i+1, shiftL n 1)) (1,2), isPrime (n - 1) ]

instance OEIS 44 where
  oeis = rabs where
    rabs = 1 : take 12 (tail fibs)
        ++ zipWith3 (\x y z -> x + y - z)
             (drop 12 rabs)
             (drop 11 rabs)
                      rabs

instance OEIS 45 where
  oeis   = fibs
  oeisIx = fi . fib . fi

instance OEIS 59 where
  oeis = [ fi i | i <- [1..], isPrime $ (2*i)^4 + 1 ]

instance OEIS 62 where
  oeisIx n = fi . floor $ fi (n + 1) / (exp 1 - 2)

instance OEIS 73 where
  oeis = xs where xs = 0 : 0 : 1 : zipWith (+) xs do tail $ zipTail (+) xs

instance OEIS 78 where
  oeis = 0 : 0 : 0 : f [0,0,0,1]
    where
      f xs = y : f (y:xs) where
        y = sum $ head $ transpose $ take 4 $ tails xs

instance OEIS 79 where
  oeis   = iterate (*2) 1
  oeisIx = (2 ^)

instance OEIS 85 where
  oeis = xs
    where
      xs = 1 : 1 : zipWith (+) (zipWith (*) [1..] xs) (tail xs)

instance OEIS 108 where
  oeis = map last $ iterate (scanl1 (+) . (++ [0])) [1]

instance OEIS 120 where
  oeis = concat r
    where r = [0] : (map.map) (+1) (scanl1 (++) r)
  oeisIx = fi . popCount . fi

instance OEIS 123 where
  oeis = xs
    where xs = 1 : zipWith (+) xs (tail $ concat $ transpose [xs,xs])

instance OEIS 129 where
  oeis = xs where xs = 0 : 1 : zipWith (+) xs do map (2 *) $ tail xs

instance OEIS 142 where
  oeis     = xs where xs = 1 : zipWith (*) [1..] xs
  oeisIx n = product [1..n]

instance OEIS 149 where
  oeisIx = floor . (exp 1 ^)
  oeis   = let e = exp 1 in map floor $ iterate (* e) 1

instance OEIS 153 where
  oeis = xs
    where
      xs = 0 : 1 : zipWith (+) (zipWith (*) [0..] xs) do zipWith (*) [2..] $ tail xs

instance OEIS 165 where
  oeisIx n = product [2, 4 .. 2 * n]

instance OEIS 166 where
  oeis = xs where xs = 1 : 0 : zipWith (*) [1..] do zipTail (+) xs

instance OEIS 169 where
  oeisIx 0 = 1
  oeisIx (succ->n) = n ^ (n - 1)

instance OEIS 178 where
  oeis = 1 : scanl1 (*) facts

-- TODO: finite precision
instance OEIS 194 where
  oeis = 0 : do concat $ zipWith ($) (map replicate [2,4..]) [1..]

instance OEIS 195 where
  oeisIx = floor . log . succ . fi

instance OEIS 196 where
  oeis     = concat $ zipWith replicate [1,3..] [0..]
  oeisIx 0 = 0
  oeisIx n = isqrtA n

instance OEIS 197 where
  oeisIx = fact . fact

instance OEIS 201 where
  oeis = f [1..] [1..] where
    f (x:xs) (y:ys) = y : f xs do delete (x + y) ys

instance OEIS 203 where
  oeisIx n = fi $ A.sigma 1 (1 + fi n :: Int)

instance OEIS 204 where
  oeis = xs where xs = 1 : 3 : do zipTail (+) xs

instance OEIS 209 where
  oeisIx = round . tan . fi

instance OEIS 210 where
  oeisIx (succ.fi->n) = floor $ n * (exp 1 - 1)

instance OEIS 211 where
  oeis = xs where xs = 4 : 3 : map (subtract 2) do zipTail (+) xs

instance OEIS 212 where
  oeisIx n = div (n*n) 3

instance OEIS 213 where
  oeis = xs where xs = 1 : 1 : 1 : zipWith (+) xs (tail $ zipTail (+) xs)

instance OEIS 215 where
  oeisIx = (+ 1) . (2 ^) . (2 ^)

instance OEIS 217 where
  oeis     = scanl1 (+) [0..]
  oeisIx n = div (n* (n+1)) 2

instance OEIS 225 where
  oeis   = iterate ((+ 1) . (* 2)) 0
  oeisIx = (subtract 1) . (2 ^)

instance OEIS 227 where
  oeisIx n = round $ exp 1 ^ fi n

instance OEIS 230 where
  oeisIx 0 = 2
  oeisIx n
    | Just (p,_) <- find ((==2*n).snd) . zip primes $ zipTail (-) primes = p

instance OEIS 244 where
  oeis   = iterate (*3) 1
  oeisIx = (3 ^)

instance OEIS 247 where
  oeisIx ((+2)->n) = 2 ^ n - n - 2

instance OEIS 248 where
  oeisIx n = sum [ bin n k * (n-k)^k | k <- [0..n] ]

instance OEIS 253 where
  oeis = go 0 1 4 2
    where
      go a b c n
        | x <- 2 * c - b + a + 2^n = a : go b c x (n+1)

instance OEIS 255 where
  oeis = xs where xs = 1 : 1 : zipTail (+) do zipWith (*) [1..] xs

instance OEIS 265 where
  oeisIx = until odd (`div` 2) . succ

instance OEIS 272 where
  oeisIx 0 = 1
  oeisIx 1 = 1
  oeisIx n = n ^ (n - 2)

instance OEIS 277 where
  oeisIx n = 3*n - 2 * isqrtA (4 * n + 5) + 5

instance OEIS 278 where
  oeisIx = go where
    go n | n < 2 = n
         | let = go (n - 1) + go (n - 2) ^ 2

instance OEIS 285 where
  oeis' (A r) = 1 : 4 : zipTail (+) r

instance OEIS 290 where
  oeis   = scanl (+) 0 [1,3..]
  oeisIx = (^ 2)

instance OEIS 295 where
  oeisIx n = 2^n - n - 1

instance OEIS 297 where
  oeisIx (pred->n) = (n + 1) * (n + 3) * (n+8) `div` 6

instance OEIS 302 where
  oeis   = iterate (* 4) 1
  oeisIx = (4 ^)

instance OEIS 304 where
  oeis = fix \r -> 2 : 3 : zipTail (*) r

instance OEIS 312 where
  oeis     = zipWith (^) [0..] [0..]
  oeisIx n = n ^ n

instance OEIS 326 where
  oeisIx n = n * (3 * n - 1) `div` 2

instance OEIS 328 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 <= n^2]

instance OEIS 340 where
  oeisIx n = div (3 ^ (n+2) - 2*n - 5) 4

instance OEIS 351 where
  oeis   = iterate (* 5) 1
  oeisIx = (5 ^)

instance OEIS 400 where
  oeisIx = (6 ^)
  oeis   = iterate (* 6) 1

instance OEIS 420 where
  oeisIx = (7 ^)
  oeis   = iterate (* 7) 1

instance OEIS 461 where
  oeisIx = fi . f . fi . succ
    where
      f n = (read $ concat $ replicate n $ show n) :: Integer

instance OEIS 463 where
  oeis = concatMap (\x -> [x,x^2]) [1..]

instance OEIS 472 where
  oeis = 2 : 5 : zipWith (+) (map (^ 2) $ tail $ oeis @472)
    do zipWith (*) (map (+ 1) $ oeis @472)
                $ zipWith (-) (tail $ oeis @472)
                $ map (^ 2) $ oeis @472

instance OEIS 522 where
  oeisIx = genericLength . choices . enumFromTo 1 . fi

instance OEIS 523 where
  oeisIx 0 = 0
  oeisIx n = 1 + oeisIx @523 (div (n - 1) 2)
  oeis = 0 : f [0] where
    f xs = ys ++ f ys where ys = map (+1) ( xs ++ xs )

instance OEIS 538 where
  oeisIx n = (3 * n * (n + 1) - 1) * (2 * n + 1) * (n + 1) * n `div` 30

instance OEIS 567 where
  oeisIx n = n * (3 * n - 2)

instance OEIS 578 where
  oeisIx = (^ 3)
  oeis =  0 : 1 : 8 : zipWith (+)
    (map (+ 6) $ oeis @578)
    (map (* 3) $ tail $ zipWith (-) (tail $ oeis @578) $ oeis @578)

instance OEIS 584 where
  oeisIx = (^ 5)

instance OEIS 603 where
  oeisIx n = genericLength [ (x,y) | x <- [0..n], y <- [0..n], x^2 + y^ 2 <= n^2]

instance OEIS 689 where
  oeis = 1 : cycle [2,4,8,6]

instance OEIS 695 where
  oeisIx n = if n == 0 then 0 else 4 * oeisIx @695 n' + b
    where (n',b) = divMod n 2

instance OEIS 703 where
  oeisIx = fi . floor . (/ 2) . (+ 7) . sqrt . (+ 1) . (* 24) . fi

instance OEIS 749 where
  oeis = fix \r -> 0 : 0 : 0 : 1 : zipWith3 (\u v w -> 4 * u - 6 * v + 4 * w)
    (drop 3 r) (drop 2 r) (drop 1 r)

instance OEIS 792 where
  oeis = 1 : f [1] where
     f xs = y : f (y:xs) where y = maximum $ zipWith (*) [1..] xs

instance OEIS 793 where
  oeisIx = maximum . map (foldl lcm 1) . partitions where
     partitions n = ps 1 n where
        ps x 0 = [[]]
        ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

instance OEIS 803 where
  oeis = fix \r -> 0 : 0 : 8 : zipWith (+)
                 (tail $ zipWith (+) (tail r) r)
                 (map (subtract 4) r)

instance OEIS 898 where
  oeis = fix \r -> 1 : 2 : do map (* 2) $ zipWith (+) (tail r) (zipWith (*) [1..] r)

instance OEIS 902 where
  oeis = fix \r -> 1 : 1 : 3 : map (* 2) (zipWith (+)
     (drop 2 r) (zipWith (*) [2..] $ tail r))

instance OEIS 904 where
  oeis = fix \r -> 0 : 3 : 13 : zipWith (+) r
     (zipWith (+) (zipWith (*) [6..] $ drop 1 r)
                  (zipWith (*) [5..] $ drop 2 r))

instance OEIS 918 where
  oeisIx = (subtract 2) . (2 ^)
  oeis = iterate ((subtract 2) . (* 2) . (+ 2)) (- 1)

instance OEIS 930 where
  oeis = fix \r -> 1 : 1 : 1 : zipWith (+) r (drop 2 r)

instance OEIS 931 where
  oeis = fix \r -> 1 : 0 : 0 : zipWith (+) r (tail r)

instance OEIS 934 where
  oeisIx = floor . (/ 2) . (+ 7) . sqrt . (+ 1) . (* 48) . fi

instance OEIS 959 where
  oeis = 1 : sieve 2 [1,3..] where
     sieve k xs = z : sieve (k + 1) (lucky xs) where
        z = genericIndex xs (k - 1 )
        lucky ws = us ++ lucky vs where
              (us, _:vs) = splitAt (fi z - 1) ws

instance OEIS 960 where
  oeis = sieve 1 [1..] where
     sieve k (x:xs) = x : sieve (k+1) (flavius xs) where
        flavius xs = us ++ flavius vs where (u:us,vs) = splitAt (k+1) xs

instance OEIS 975 where
  oeis = fix \r -> 0 : 1 : map (+ 1) (zipWith (+) (tail r) (map (* 2) r))

instance OEIS 980 where
  oeisIx n = genericLength $ filter ((== 0) . sum) $ subsequences [-n..n]

instance OEIS 982 where
  oeisIx = (`div` 2) . (+ 1) . (^ 2)

instance OEIS 992 where
  oeis = 1 : f 1 0 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ take x $ zipWith (*) zs $ reverse zs

instance OEIS 1008 where
  oeisIx = numerator . sum . map (1 %) . enumFromTo 1 . succ
  oeis   = map numerator $ scanl1 (+) $ map (1 %) [1..]

instance OEIS 1014 where
  oeis = map (^ 6) [0..]

instance OEIS 1018 where
  oeisIx = (8 ^)
  oeis   = iterate (* 8) 1

instance OEIS 1019 where
  oeisIx = (9 ^)
  oeis   = iterate (* 9) 1

instance OEIS 1021 where
  oeisIx = (12 ^)
  oeis   = iterate (* 12) 1

instance OEIS 1025 where
  oeisIx = (16 ^)
  oeis   = iterate (* 16) 1

instance OEIS 1030 where
  oeis = [2, 1, 1, 2] ++ f [2] [2, 1, 1, 2] where
     f us vs = ws ++ f vs (vs ++ ws) where
               ws = 1 : us ++ 1 : vs

instance OEIS 1040 where
  oeis = fix \r -> 0 : 1 : zipWith (+) r (zipWith (*) [1..] $ tail r)

instance OEIS 1042 where
  oeis = 1 : 2 : zipWith (-) (tail xs) xs
    where xs = map (^ 2) $ oeis @1042

instance OEIS 1047 where
  oeis = map fst $ iterate (\ (u, v) -> (3 * u + v, 2 * v)) (0, 1)

instance OEIS 1053 where
  oeis = fix \r -> 1 : 0 : zipWith (+) r (zipWith (*) [1..] $ tail r)

instance OEIS 1056 where
  oeis = fix \r -> 1 : 3 : (map (+ 1 ) $ zipWith (*) r $ tail r)

instance OEIS 1057 where
  oeisIx n = (n' + m) * (-1) ^ (1 - m) where (n',m) = divMod n 2
  oeis     = 0 : concatMap (\x -> [x,-x]) [1..]

instance OEIS 1064 where
  oeis = fix \r -> 1 : 1 : 0 : zipWith (+) r (tail $ zipWith (*) r (tail r))

instance OEIS 1075 where
  oeis = fix \r -> 1 : 2 : zipWith (-) (map (4 *) $ tail r) r

instance OEIS 1078 where
  oeis = fix \r -> 0 : 2 : zipWith (-) (map (10*) $ tail r) r

instance OEIS 1093 where
  oeisIx = (+ 1) . (^ 3) . pred

instance OEIS 1106 where
  -- oeisIx n = genericLength [ (x,y) | x <- [-n+1..n - 1], y <- [-n+1..n-1], x + y <= n]
  oeisIx n = n* (7*n - 5) `div` 2

instance OEIS 1108 where
  oeis = fix \r -> 0 : 1 : map (+ 2) (zipWith (-) (map (* 6) (tail r)) r)

instance OEIS 1109 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1110 where
  oeis = fix \r -> 0 : 1 : (map (+ 2) $ zipWith (-) (map (* 34) (tail r)) r)

instance OEIS 1113 where
  oeis = step $ sum $ take 100 $ map (1%) $ 1 : facts
    where
      step r | x <- div (numerator r) (denominator r) = x : step ((r - x%1)*10)

instance OEIS 1140 where
  oeis = fix \r -> 4 : map (fi . say . fi) r where
     say :: Integer -> Integer
     say = read . concatMap saygroup . group . show
           where saygroup s = (show $ length s) ++ [head s]

instance OEIS 1146 where
  oeisIx = (2 ^) . (2 ^)
  oeis = iterate (^ 2) 2

instance OEIS 1147 where
  oeisIx n = product [1, 3 .. 2 * n - 1]
  oeis     = fix \r -> 1 : zipWith (*) [1, 3 ..] r

instance OEIS 1175 where
  oeisIx = q . succ
    where
      q 1 = 1
      q n = f 1 ps 0 where
        f 0 (1 : xs) pi = pi
        f _ (x : xs) pi = f x xs (pi + 1)
        ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` n) (tail ps) ps

instance OEIS 1176 where
  oeisIx = q . succ
    where
      q 1 = 1
      q n = f 1 ps 0 where
        f 0 (1 : xs) z = z
        f _ (x : xs) z = f x xs (z + 0 ^ x)
        ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` n) (tail ps) ps

instance OEIS 1196 where
  oeisIx n = if n == 0 then 0 else 4 * oeisIx @1196 n' + 3 * b
              where (n',b) = divMod n 2

instance OEIS 1221 where
  oeisIx = genericLength . snd . unzip . factorise . fi . succ

instance OEIS 1222 where
  oeisIx = fi . sum . snd . unzip . factorise . fi . succ

instance OEIS 1286 where
  oeisIx ((+2)->n) = sum [1..n - 1] * product [1..n - 1]

instance OEIS 1333 where
  oeis = fix \r -> 1 : 1 : zipWith (+) r (map (* 2) $ tail r)

instance OEIS 1353 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (4 *) $ tail r) r

instance OEIS 1444 where
  oeisIx n = div (3 ^ n + 3 ^ (div n 2)) 2

instance OEIS 1462 where
  oeis = 1 : 2 : 2 : g 3  where
     g x = (genericReplicate (oeisIx @1462 $ pred x) x) ++ g (x + 1)

instance OEIS 1477 where
  oeisIx = id
  oeis   = [0..]

instance OEIS 1541 where
  oeis = fix \r -> 1 : 3 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1542 where
  oeis = fix \r -> 0 : 2 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1550 where
  oeisIx n = sum $ map (^ n) [1..3]

instance OEIS 1595 where
  oeis = fix \r -> 1 : 1 : do map (+ 1) $ zipTail (+) r

instance OEIS 1599 where
  oeis = [ fi n | (n :: Int) <- [1..], 1 == denominator do n * A.tau n % A.sigma 1 n ]

instance OEIS 1600 where
  oeis =
    [ fi $ numerator m
    | (n :: Int) <- [1..]
    , let m = n * A.tau n % A.sigma 1 n
    , 1 == denominator m ]

instance OEIS 1608 where
  oeis = fix \r -> 3 : 0 : 2 : zipTail (+) r

instance OEIS 1610 where
  oeis = fix \r -> 0 : 2 : do map (+ 1) $ zipTail (+) r

instance OEIS 1612 where
  oeis = fix \r -> 3 : 2 : do map (subtract 1) $ zipTail (+) r

instance OEIS 1634 where
  oeis = fix \r -> 0 : 2 : 3 : 6 : zipWith (+) r (zipWith (+) (tail r) (drop 2 r))

instance OEIS 1644 where
  oeis = fix \r -> 3 : 1 : 3 : zipWith3 (((+) .) . (+)) r (tail r) (drop 2 r)

instance OEIS 1651 where
  oeisIx = (`div` 2) . (subtract 1) . (* 3) . succ
  oeis   = filter ((/= 0) . (`mod` 3)) [1..]

instance OEIS 1652 where
  oeis = fix \r -> 0 : 3 : map (+ 2) (zipWith (-) (map (* 6) (tail r)) r)

instance OEIS 1653 where
  oeis = fix \r -> 1 : 5 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1696 where
  oeis = fix \r -> 0 : 1 : zipWith (-)
     (zipWith (+) f $ map (^ 2) f)
     (zipWith (*) r f)
     where f = tail $ oeis @1696

instance OEIS 1704 where
  oeis = map fi f
    where f = map read (zipWith (++) iss $ tail iss) :: [Integer]
                 where iss = map show [1..]

instance OEIS 1817 where
  oeisIx (succ->n) = genericLength [d | d <- [1,4..n], mod n d == 0]

instance OEIS 1822 where
  oeisIx (succ->n) = genericLength [d | d <- [2,5..n], mod n d == 0]

instance OEIS 1834 where
  oeis = fix \r -> 1 : 5 : zipWith (-) (map (* 4) $ tail r) r

instance OEIS 1835 where
  oeis = fix \r -> 1 : 1 : zipWith (-) (map (4 *) $ tail r) r

instance OEIS 1845 where
  oeisIx n = (2 * n + 1) * (2 * n ^ 2 + 2 * n + 3) `div` 3

instance OEIS 1855 where
  oeis = fix \r -> 0 : zipWith (+) [1..] do zipTail (+) . concat $ transpose [r, r]

instance OEIS 1857 where
  oeis = 2 : 3 : ulam 2 3 (oeis @1857)

instance OEIS 1906 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (* 3) $ tail r) r

instance OEIS 1911 where
  oeis = fix \r -> 0 : 1 : map (+ 2) do zipTail (+) r

instance OEIS 1945 where
  oeis = fix \r -> 0 : 1 : 1 : 1 : 5 : 1 : zipWith6
     (\u v w x y z -> - u + v + 3*w + x - y - z)
       (drop 5 r) (drop 4 r) (drop 3 r)
       (drop 2 r) (drop 1 r) (drop 0 r)

instance OEIS 1951 where
  oeisIx = floor . (* sqrt 2) . fi

instance OEIS 1952 where
  oeisIx = floor . (* (sqrt 2 + 2)) . fi . succ

instance OEIS 1971 where
  oeisIx = floor . (+ 0.5) . (/ 8) . fi . (^ 2)

instance OEIS 2024 where
  oeisIx = round . sqrt . (* 2) . fi . succ
  oeis = [1..] >>= \n -> genericReplicate n n

instance OEIS 2048 where
  oeis = f [1..] [] where
     f (x:xs) ys = x : f (xs \\ scanl (+) x ys) (x : ys)

instance OEIS 2049 where
  oeis = g [1..] [] where
     g (x:xs) ys = (last zs) : g (xs \\ zs) (x : ys) where
       zs = scanl (+) x ys

instance OEIS 2061 where
  oeisIx n = n * (n - 1) + 1

instance OEIS 2064 where
  oeisIx n = n * 2 ^ n + 1
  oeis = 1 : 3 : (map (+ 1) $ zipWith (-) (tail xs) xs)
     where xs = map (* 4) $ oeis @2064

instance OEIS 2081 where
  oeis = filter ((`elem` [2,4,8,16]) . (`mod` 20)) [1..]

instance OEIS 2083 where
  oeis = 1 : f [1] where
     f xs = x : f (x:xs) where x = sum $ take (div (1 + length xs) 2) xs

instance OEIS 2104 where
  oeisIx = genericLength . filter (\xs -> head xs == minimum xs)
         . tail . choices . enumFromTo 1

instance OEIS 2131 where
  oeisIx (succ->n) = sum [d | d <- [1..n], mod n d == 0, odd $ div n d]

instance OEIS 2187 where
  oeis = tail g where
     g = 0 : 0 : [mex [xor (g !! (a + 1)) (g !! (n - a - 2)) |
                       a <- [-1 .. n - 2]] | n <- [1 ..]]
     xor 0 0 = 0
     xor x y = let ((q,r), (s,t)) = (divMod x 2, divMod y 2)
                in (if r == t then 0 else 1) + 2 * xor q s
     mex xs = head [x | x <- [0..], not (elem x xs)]

instance OEIS 2193 where
  oeis = w 2 0 where
    w x r = dig : w (100 * (x - (20 * r + dig) * dig)) (10 * r + dig)
      where dig = head (dropWhile (\d -> (20 * r + d) * d < x) [0..]) - 1

instance OEIS 2203 where
  oeis' (A r) = 2 : 2 : zipWith (+) (map (* 2) $ tail r) r

instance OEIS 2264 where
  oeis' (A r) = 0 : 0 : 0 : map (+ 1) r

instance OEIS 2266 where
  oeisIx = (`div` 5)
  oeis' (A r) = [0,0,0,0,0] ++ map (+ 1) r

instance OEIS 2275 where
  oeisIx = (`div` 9) . subtract 1 . (10 ^)
  oeis = iterate ((+ 1) . (* 10)) 0

instance OEIS 2283 where
  oeisIx = subtract 1 . (10 ^)

instance OEIS 2315 where
  oeis' (A r) = 1 : 7 : zipWith (-) (map (* 6) (tail r)) r

instance OEIS 2378 where
  oeisIx n = n * (n + 1)
  oeis = zipWith (*) [0..] [1..]

instance OEIS 2379 where
  oeisIx n = 3^n `div` 2^n

instance OEIS 2380 where
  oeisIx n = 3^n `mod` 2^n

instance OEIS 2387 where
  -- oeis = f 0 1 where
  --    f x k = if genericIndex hs k > fi x
  --            then k : f (x + 1) (k + 1) else f x (k + 1)
  --            where hs = scanl (+) 0 $ map recip [1..]
  oeis = (1:) . (2:) . tail
       . scanl (\n xs -> n + genericLength xs) 1
       . groupBy (on (==) floor)
       . scanl1 (+) $ map recip [1..]

instance OEIS 2472 where
  oeisIx (succ->n) = genericLength [x | x <- [1..n], gcd n x == 1, gcd n (x + 2) == 1]

instance OEIS 2473 where
  oeis = f $ S.singleton 1 where
     f s = x : f (s' `S.union` S.fromList (map (* x) [2,3,5,7]))
           where (x, s') = S.deleteFindMin s

instance OEIS 2487 where
  oeis = 0 : 1 : stern [1] where
     stern fuscs = fuscs' ++ stern fuscs' where
       fuscs' = interleave fuscs $ zipWith (+) fuscs $ (tail fuscs) ++ [1]
     interleave []     ys = ys
     interleave (x:xs) ys = x : interleave ys xs

instance OEIS 2491 where
  oeis = sieve 1 [1..] where
     sieve k (x:xs) = x : sieve (k+1) (mancala xs) where
        mancala xs = us ++ mancala vs where (us,v:vs) = splitAt k xs

instance OEIS 2517 where
  oeis' (A r) = 0 : concat (transpose [[2, 5 ..], [3, 12 ..], map (* 3) $ tail r])

instance OEIS 2522 where
  oeisIx = (+ 1) . (^ 2)
  oeis   = scanl (+) 1 [1,3..]

instance OEIS 2541 where
  oeisIx (succ->n) = sum $ zipWith div [n - 1, n - 2 ..] [1 .. n - 1]

instance OEIS 2605 where
  oeis' (A r) = 0 : 1 : map (* 2) (zipTail (+) r)

instance OEIS 2620 where
  oeisIx = (`div` 4) . (^ 2)

instance OEIS 2627 where
  oeis' (A r) = 0 : map (+ 1) (zipWith (*) [1..] r)

instance OEIS 2805 where
  oeisIx = denominator . sum . map (1 %) . enumFromTo 1 . succ
  oeis = map denominator $ scanl1 (+) $ map (1 %) [1..]

instance OEIS 2821 where
  oeisIx = round . sqrt . fi . (^ 3)

instance OEIS 2858 where
 oeis = 1 : 2 : ulam 2 2 (oeis @2858)

instance OEIS 2859 where
 oeis = 1 : 3 : ulam 2 3 (oeis @2859)

instance OEIS 2943 where
  oeisIx n = 2 * n * (2 * n + 1)

instance OEIS 2977 where
  oeis = f $ S.singleton 1 where
     f s = m : (f $ S.insert (3*m+1) $ S.insert (2*m+1) s') where
          (m, s') = S.deleteFindMin s

instance OEIS 3059 where
  oeis = concat $ zipWith ($) (map replicate [1,3..]) [1..]

instance OEIS 3101 where
  oeisIx n = sum $ zipWith (^) [0 ..] [n + 1, n .. 1]

instance OEIS 3105 where
  oeisIx n = p 1 n where
     p k m | m == 0 = 1 | m < k = 0 | otherwise = q k (m-k) + p (k+2) m
     q k m | m == 0 = 1 | m < k = 0 | otherwise = p (k+2) (m-k) + p (k+2) m

instance OEIS 3132 where
  oeisIx 0 = 0
  oeisIx x = d ^ 2 + (oeisIx @3132) x' where (x', d) = divMod x 10

instance OEIS 3136 where
  oeis = f 0 $ S.singleton 0 where
    f x s
      | m < x ^ 2 = m : f x s'
      | otherwise = m : f x'
         (S.union s' $ S.fromList $ map (\y -> x'^2+ (x'+y)*y) [0..x'])
      where
        x' = x + 1
        (m,s') = S.deleteFindMin s

instance OEIS 3159 where
  oeis = f [1..] where f (x:xs) = x : f (delete  (2*x) xs)

instance OEIS 3160 where
  oeis = 1 : 1 : zipWith (-) [3..] (zipWith (+) xs $ tail xs)
     where xs = map (oeisIx @3160 . pred) (oeis @3160)

instance OEIS 3188 where
  oeisIx n = fi $ (fi n) `xor` (shiftR (fi n) 1 :: Integer)

instance OEIS 3215 where
  oeisIx n = 3 * n * (n + 1) + 1

instance OEIS 3229 where
  oeis = 1 : 1 : 3 : zipWith (+) (map (* 2) (oeis @3229)) (drop 2 (oeis @3229))

instance OEIS 3231 where
  oeisIx = floor . (/ 2) . (* (sqrt 5 + 5)) . (+ 1) . fi

instance OEIS 3269 where
  oeis = 0 : 1 : 1 : 1 : zipWith (+) (oeis @3269) (drop 3 (oeis @3269))

instance OEIS 3309 where
  oeis = 1 : f [2..]
     where f (x:xs) = x : f (map snd [ (u, v) | (u, v) <- zip [1..] xs, mod u x > 0])

instance OEIS 3310 where
  oeis = f [3..] where
     f (x:xs) = x : f (g xs) where
       g zs = us ++ g vs where (us, _:vs) = splitAt (fi x - 1) zs

instance OEIS 3311 where
  oeis = f [3..] where
     f (x:xs) = x : f (g xs) where
       g zs = us ++ g vs where (_:us, vs) = splitAt (fi x) zs

instance OEIS 3312 where
  oeis = sieve [3..] where
     sieve (x:xs) = x : (sieve $ xOff xs)
     xOff (x:x':_:xs) = x : x': (xOff xs)

instance OEIS 3314 where
  oeis = 0 : f [0] [2..] where
     f vs (w:ws) = y : f (y:vs) ws where
       y = w + minimum (zipWith (+) vs $ reverse vs)

instance OEIS 3320 where
  oeisIx n = maximum $ zipWith (^) [0 .. n] [n, n - 1 ..]

instance OEIS 3418 where
  oeisIx = foldl lcm 1 . enumFromTo 2

instance OEIS 3462 where
  oeisIx = (`div` 2) . (subtract 1) . (3 ^)
  oeis = iterate ((+ 1) . (* 3)) 0

instance OEIS 3480 where
  oeis = 1 : 2 : 7 : (tail $ zipWith (-)
     (tail $ map (* 4) (oeis @3480)) (map (* 2) (oeis @3480)))

instance OEIS 3485 where
  oeis = 1 : 2 : 4 : 8 : 9 : zipWith (+)
     (drop 4 (oeis @3485)) (zipWith (-) (tail (oeis @3485)) (oeis @3485))

instance OEIS 3500 where
  oeis = 2 : 4 : zipWith (-)
     (map (* 4) $ tail (oeis @3500)) (oeis @3500)

instance OEIS 3586 where
--   import Data.Set (Set, singleton, insert, deleteFindMin)
  oeis = smooth (S.singleton 1)
    where
      smooth s = x : smooth (S.insert (3*x) $ S.insert (2*x) s')
        where (x, s') = S.deleteFindMin s

instance OEIS 3591 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @3591) !! (n - 1)
  oeis = f $ S.singleton 1 where
     f s = y : f (S.insert (2 * y) $ S.insert (7 * y) s')
                 where (y, s') = S.deleteFindMin s

instance OEIS 3592 where
--   import Data.Set (singleton, deleteFindMin, insert)
  -- oeisIx n = (oeis @3592) !! (n - 1)
  oeis = f $ S.singleton 1 where
     f s = y : f (S.insert (2 * y) $ S.insert (5 * y) s')
                 where (y, s') = S.deleteFindMin s

instance OEIS 3593 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @3593) !! (n - 1)
  oeis = f (S.singleton 1) where
     f s = m : f (S.insert (3*m) $ S.insert (5*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 3594 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @3594) !! (n - 1)
  oeis = f $ S.singleton 1 where
     f s = y : f (S.insert (3 * y) $ S.insert (7 * y) s')
                 where (y, s') = S.deleteFindMin s

instance OEIS 3595 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @3595) !! (n - 1)
  oeis = f $ S.singleton 1 where
     f s = y : f (S.insert (5 * y) $ S.insert (7 * y) s')
                 where (y, s') = S.deleteFindMin s

instance OEIS 3596 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @3596) !! (n - 1)
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (2 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 3597 where
--   import Data.Set (singleton, deleteFindMin, insert)
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (3 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 3598 where
--   import Data.Set (singleton, deleteFindMin, insert)
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (5 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 3599 where
--   import Data.Set (singleton, deleteFindMin, insert)
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (7 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 3619 where
  oeisIx (succ->n) = n + floor (log (x + fi (floor $ log x)))
    where x = fi n + 1

instance OEIS 3666 where
  oeis = 1 : 4 : ulam 2 4 (oeis @3666)

instance OEIS 3667 where
  oeis = 1 : 5 : ulam 2 5 (oeis @3667)

instance OEIS 3668 where
  oeis = 2 : 7 : ulam 2 7 (oeis @3668)

instance OEIS 3669 where
  oeis = 3 : 4 : ulam 2 4 (oeis @3669)

instance OEIS 3670 where
  oeis = 4 : 7 : ulam 2 7 (oeis @3670)

instance OEIS 3714 where
  oeisIx n = (oeis @3714) `genericIndex` n
  oeis = 0 : f (S.singleton 1) where
     f s = m : (f $ S.insert (4*m + 1) $ S.insert (2*m) s')
           where (m, s') = S.deleteFindMin s

instance OEIS 3726 where
  oeis = filter f [0..] where
     f x = x < 7 || (x `mod` 8) < 7 && f (x `div` 2)

instance OEIS 3754 where
  oeis = filter f [0..] where
     f x = x == 0 || x `mod` 4 > 0 && f (x `div` 2)

instance OEIS 3796 where
  oeis = filter f [0..] where
     f x  = x < 4 || x `mod` 8 /= 0 && f (x `div` 2)

instance OEIS 3842 where
  oeis = tail $ concat fws where
     fws = [2] : [1] : (zipWith (++) fws $ tail fws)

instance OEIS 3849 where
  oeis = tail $ concat fws where
     fws = [1] : [0] : (zipWith (++) fws $ tail fws)

instance OEIS 3893 where
  oeis = 0 : 1 : zipWith (\u v -> (u + v) `mod` 10)
                         (tail (oeis @3893)) (oeis @3893)

instance OEIS 4001 where
  oeis = 1 : 1 : h 3 1  {- memoization -}
    where h n x = x' : h (n + 1) x'
            where x' = (oeisIx @4001 . pred) x + (oeisIx @4001 . pred) (n - x)

instance OEIS 4050 where
--   import Data.Set (singleton, deleteFindMin, insert)
  oeis = f 1 $ S.singleton (2, 1, 1) where
     f x s = if y /= x then y : f y s'' else f x s''
             where s'' = S.insert (u * 2 + v, u * 2, v) $
                         S.insert (u + 3 * v, u, 3 * v) s'
                   ((y, u, v), s') = S.deleteFindMin s

instance OEIS 4080 where
  oeisIx n = fi . fromJust $ findIndex (fi n <=) $ scanl (+) 0 $ map recip [1..]

instance OEIS 4086 where
  oeisIx = fi . (read :: String -> Integer) . reverse . (show :: Integer -> String) . fi

instance OEIS 4125 where
  oeisIx (succ->n) = sum $ map (mod n) [1..n]

instance OEIS 4149 where
  oeis = 1 : 1 : 1 : f [1,1,1] where
     f xs = y : f (y : xs) where
       y = head xs + sum (zipWith (*) (init $ init $ tail xs) (reverse xs))

instance OEIS 4151 where
  oeisIx = until ((> 0) . (`mod` 10)) (`div` 10) . succ

instance OEIS 4185 where
  oeisIx n = fi (read $ sort $ show (fi n) :: Integer)

instance OEIS 4186 where
  oeisIx n = fi (read $ sortOn Down $ show (fi n) :: Integer)

instance OEIS 4201 where
  oeis = f 1 [1..] where
     f k xs = us ++ f (k + 1) (drop (k) vs) where (us, vs) = splitAt k xs

instance OEIS 4202 where
  oeis = skipTake 1 [1..] where
     skipTake k xs = take k (drop k xs) ++ skipTake (k + 1) (drop (2*k) xs)

instance OEIS 4210 where
  oeis = magics 1 [0] [0] where
     magics n ms tests
        | tests `intersect` nMinus == [] && tests `intersect` nPlus == []
        = n : magics (n+1) (n:ms) (nMinus ++ nPlus ++ tests)
        | otherwise
        = magics (n+1) ms tests
        where nMinus = map (n -) ms
              nPlus  = map (n +) ms

instance OEIS 4216 where
  oeisIx (succ->n) = if n <= 9 then 0 else 1 + (oeisIx @4216) (n `div` 10)

instance OEIS 4233 where
  oeisIx = ceiling . log . fi . succ

instance OEIS 4275 where
  oeisIx n = 2 * n - 1 + signum (1 - n)
  oeis = 0 : 1 : [2, 4 ..]

instance OEIS 4277 where
  oeisIx (succ->n) = 2 * n - 1 + signum (1 - n)
  oeis = 1 : [2, 4 ..]

instance OEIS 4278 where
  oeisIx (succ->n) = if n <= 3 then n else 2 * (n - 2)
  oeis = [1, 2, 3] ++ [4, 6 ..]

instance OEIS 4396 where
  oeis = 0 : 1 : 1 : map (+ 2) (oeis @4396)

instance OEIS 4458 where
  oeisIx n = fi (fi n `xor` 17 :: Integer)

instance OEIS 4488 where
  oeisIx 0 = 0
  oeisIx n = if d == 0 then 3 * (oeisIx @4488) n' else 3 * (oeisIx @4488) n' + 3 - d
              where (n', d) = divMod n 3

instance OEIS 4523 where
  oeis = 0 : 0 : 1 : map (+ 2) (oeis @4523)

instance OEIS 4524 where
  oeisIx n = n `div` 4 + (n + 1) `div` 4
  oeis = 0 : 0 : 0 : 1 : map (+ 2) (oeis @4524)

instance OEIS 4525 where
  oeis' (A xs) = 0 : 1 : 1 : zipWith3 (\x y z -> x - y + z + 1)
    xs (tail xs) (drop 2 xs)

instance OEIS 4526 where
  oeisIx = (`div` 2)
  oeis = concatMap (\x -> [x, x]) [0..]

instance OEIS 4539 where
  oeis = w 2 0 where
     w x r = bit : w (4 * (x - (4 * r + bit) * bit)) (2 * r + bit)
       where bit = head (dropWhile (\b -> (4 * r + b) * b < x) [0..]) - 1

instance OEIS 4652 where
  oeisIx = ceiling . (/ 4) . fi . (^ 2)
  oeis = 0 : 1 : zipWith (+) (oeis @4652) [1..]

instance OEIS 4718 where
  oeis' (A xs) = 0 : concat do transpose [ map (+1) xs, map negate $ tail xs ]

instance OEIS 4719 where
  oeisIx = fi . (read . filter (/= '0') . show :: Integer -> Integer) . fi . succ

instance OEIS 4730 where
  oeis = map (fi.denominator) ggs where
     ggs = 1 : 2 : zipWith (+) ggs (map (1 /) $ tail ggs) :: [Rational]

instance OEIS 4731 where
  oeis = (1 :) . tail $ map (fi . numerator) ggs where
     ggs = 0 : 1 : zipWith (+) ggs (map (1 /) $ tail ggs) :: [Rational]

instance OEIS 4737 where
  oeis = concatMap f $ tail $ inits [1..]
     where f xs = xs ++ tail (reverse xs)

instance OEIS 4739 where
  oeis = concat $ map (\n -> [1..n] ++ [n,n - 1..1]) [1..]

instance OEIS 4741 where
  oeis = concat $ map (\n -> [1,3..2*n - 1] ++ [2*n,2*n - 2..2]) [1..]

instance OEIS 4742 where
  oeis = filter f [0..] where
     f x  = x < 4 || x `mod` 8 /= 5 && f (x `div` 2)

instance OEIS 4743 where
  oeis = filter f [0..] where
     f x  = x < 4 || x `mod` 8 /= 6 && f (x `div` 2)

instance OEIS 4744 where
  oeis = filter f [0..] where
     f x  = x < 4 || x `mod` 8 /= 3 && f (x `div` 2)

instance OEIS 4745 where
  oeis = filter f [0..] where
     f x  = x < 4 || x `mod` 8 /= 1 && f (x `div` 2)

instance OEIS 4746 where
  oeis = filter f [0..] where
     f x  = x < 4 || x `mod` 8 /= 2 && f (x `div` 2)

instance OEIS 4753 where
  oeis = filter f [0..] where
     f 0 = False; f x = x `mod` 4 == 0 || f (x `div` 2)

instance OEIS 4754 where
  oeis = 2 : concat (transpose [zs, map (+ 1) zs]) where zs = map (* 2) (oeis @4754)

instance OEIS 4755 where
  oeis = 3 : concat (transpose [zs, map (+ 1) zs]) where zs = map (* 2) (oeis @4755)

instance OEIS 4756 where
  oeis = 4 : concat (transpose [zs, map (+ 1) zs]) where zs = map (* 2) (oeis @4756)

instance OEIS 4757 where
  oeis = 5 : concat (transpose [zs, map (+ 1) zs]) where zs = map (* 2) (oeis @4757)

instance OEIS 4758 where
  oeis = 6 : concat (transpose [zs, map (+ 1) zs]) where zs = map (* 2) (oeis @4758)

instance OEIS 4759 where
  oeis = 7 : concat (transpose [zs, map (+ 1) zs]) where zs = map (* 2) (oeis @4759)

instance OEIS 4765 where
  oeis = filter f [0..] where
     f x | x <= 8    = x /= 7
         | otherwise = f (x `div` 2)

instance OEIS 4767 where
  oeisIx = (+ 3) . (* 4)
  oeis = [3, 7 ..]

instance OEIS 4770 where
  oeisIx = (subtract 3) . (* 8) . succ
  oeis = [5, 13 ..]

instance OEIS 4771 where
  oeisIx = (+ 7) . (* 8)
  oeis = [7, 15 ..]

instance OEIS 4776 where
  oeis = filter ((/= 5) . (`mod` 8)) [0..]

instance OEIS 4781 where
  oeis = filter f [0..] where
     f x | x < 7     = False
         | otherwise = (x `mod` 8) == 7 || f (x `div` 2)

instance OEIS 4831 where
  oeis = [x ^ 4 + y ^ 4 | x <- [0..], y <- [0..x]]

instance OEIS 5009 where
  oeisIx = (* 7) . (2 ^)

instance OEIS 5043 where
  oeis = 1 : 0 : zipWith div
     (zipWith (*) [1..] (zipWith (+)
         (map (* 2) $ tail (oeis @5043)) (map (* 3) (oeis @5043)))) [3..]

instance OEIS 5055 where
  oeisIx = (* 7) . (5 ^)
  oeis = iterate (* 5) 7

instance OEIS 5132 where
  oeis = 0 : recaman (S.singleton 0) 1 0 where
     -- recaman :: Set Integer -> Integer -> Integer -> [Integer]
     recaman s n x = if x > n && (x - n) `S.notMember` s
                        then (x - n) : recaman (S.insert (x-n) s) (n+1) (x-n)
                        else (x + n) : recaman (S.insert (x+n) s) (n+1) (x+n)

instance OEIS 5150 where
  oeis = map fi look_and_say

instance OEIS 5151 where
  oeis = map fi (1 : f [1] :: [Integer]) where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map length zss, map head zss]
            zss = group $ sort xs

instance OEIS 5185 where
  oeis = 1 : 1 : zipWith (+)
     (map f $ zipWith (-) [3..] (oeis @5185))
     (map f $ zipWith (-) [3..] $ tail (oeis @5185))
    where
      f = oeisIx @5185 . pred

instance OEIS 5187 where
  oeis = 0 : zipWith (+) [1..] (map (oeisIx @5187 . (`div` 2)) [1..])

instance OEIS 5206 where
  oeis = 0 : zipWith (-) [1..] (map (oeisIx @5206) (oeis @5206))

instance OEIS 5210 where
  oeis = 1 : 1 : (zipWith ((abs .) . (-))
      [3..] $ zipWith (+) (tail (oeis @5210)) (map (2 *) (oeis @5210)))

instance OEIS 5228 where
  oeis = 1 : figure 1 [2..] where
     figure n (x:xs) = n' : figure n' (delete n' xs) where n' = n + x

instance OEIS 5229 where
  oeis = 1 : 1 : zipWith ((+) `on` (oeisIx @5229 . pred))
                         (oeis @5229) (zipWith (-) [3..] (oeis @5229))

instance OEIS 5242 where
  oeis = f [1..] 0 0 where
     f (x:xs) y z = x : f (xs \\ [x + y, x + y + z]) x y

instance OEIS 5243 where
  oeis = 1 : h [1] (S.singleton 2) where
     h xs s = m : h (m:xs) (S.union s' $ S.fromList $ map (+ m) $ scanl1 (+) xs)
       where (m, s') = S.deleteFindMin s

instance OEIS 5244 where
  oeis = f [2] (S.singleton 2) where
     f xs s = y :
       f (y : xs) (s' `S.union` S.fromList (map ((subtract 1) . (* y)) xs))
       where (y,s') = S.deleteFindMin s

instance OEIS 5246 where
  oeis = 1 : 1 : 1 : map (+ 1) (zipWith div
     (zipWith (*) (drop 2 (oeis @5246)) (tail (oeis @5246))) (oeis @5246))

instance OEIS 5251 where
  oeis = 0 : 1 : 1 : 1 : zipWith (+) (oeis @5251)
     (drop 2 $ zipWith (+) (oeis @5251) (tail (oeis @5251)))

instance OEIS 5314 where
  oeis = 0 : 1 : 2 : zipWith (+) (oeis @5314)
     (tail $ zipWith (-) (map (2 *) $ tail (oeis @5314)) (oeis @5314))

instance OEIS 5350 where
  oeis = 1 : 1 : 1 : h 4 1 where
     h x y = z : h (x + 1) z
       where z = (oeisIx @5350 . pred) y + (oeisIx @5350 . pred) (x - y)

instance OEIS 5351 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @5351) n' * 2 + m where
     (n', m) = if r < 0 then (q + 1, r + 2) else (q, r)
               where (q, r) = quotRem n (negate 2)

instance OEIS 5374 where
  oeis = 0 : 1 : zipWith (-)
     [2..] (map (oeisIx @5374 . (oeisIx @5374)) $ tail (oeis @5374))

instance OEIS 5375 where
  oeis =  0 : 1 : zipWith (-)
     [2..] (map (oeisIx @5375) (map (oeisIx @5375) (map (oeisIx @5375) (tail (oeis @5375)))))

instance OEIS 5408 where
  oeisIx = (+ 1) . (* 2)
  oeis   = [1, 3 ..]

instance OEIS 5425 where
  oeis = 1 : 2 : zipWith (+)
     (map (* 2) (tail (oeis @5425))) (zipWith (*) [1..] (oeis @5425))

instance OEIS 5448 where
  oeisIx (succ->n) = 3 * n * (n - 1) `div` 2 + 1
  oeis = 1 : zipWith (+) (oeis @5448) [3, 6 ..]

instance OEIS 5563 where
  oeisIx n = n * (n + 2)
  oeis = zipWith (*) [0..] [2..]

instance OEIS 5590 where
  oeis = 0 : 1 : concat (tail $ transpose
     [oeis @5590, zipWith (-) (tail (oeis @5590)) (oeis @5590)])

instance OEIS 5605 where
  oeis = 0 : 1 : zipWith (+) (tail (oeis @5605))
     (zipWith (*) (cycle [-1,1]) (map (^ 2) $ (oeis @5605)))

instance OEIS 5658 where
  oeis = klarner $ S.fromList [1,2] where
     klarner s = m : (klarner $
                      S.insert (2*m) $ S.insert (3*m+2) $ S.insert (6*m+3) s')
        where (m,s') = S.deleteFindMin s

instance OEIS 5665 where
  oeis = 0 : 1 : 5 : zipWith (-)
                 (map (* 3) $ drop 2 (oeis @5665)) (map (* 2) (oeis @5665))

instance OEIS 5707 where
  oeis = 1 : 1 : 1 : 1 : h 5 1 where
     h x y = z : h (x + 1) z
       where z = (oeisIx @5707 . pred) y + (oeisIx @5707 . pred) (x - y)

instance OEIS 5713 where
  oeis = 1 : 1 : concat (sibb [0] [1,1]) where
     sibb xs ys = zs : sibb ys zs where zs = xs ++ ys

instance OEIS 5831 where
  oeis = 0:1:zipWith (*) (tail (oeis @5831)) (map succ (oeis @5831))

instance OEIS 5843 where
  oeisIx = (* 2)
  oeis = [0, 2 ..]

instance OEIS 5912 where
  oeisIx n = (n * (n * (77 * n + 69) + 19) + 3) `div` 3

instance OEIS 5917 where
  oeis = map sum $ f 1 [1, 3 ..] where
     f x ws = us : f (x + 2) vs where (us, vs) = splitAt x ws

instance OEIS 5920 where
  oeisIx n = (n * (n * (3 * n + 7) + 6) + 2) `div` 2

instance OEIS 5942 where
  oeis = 1 : 2 : 4 : 6 : zipWith (+) (drop 6 ts) (drop 5 ts) where
     ts = concat $ transpose [oeis @5942, (oeis @5942)]

instance OEIS 6012 where
  oeis = 1 : 2 : zipWith (-) (tail $ map (* 4) (oeis @6012))
    (map (* 2) (oeis @6012))

instance OEIS 6053 where
  oeis = 0 : 0 : 1 : zipWith (+) (drop 2 (oeis @6053))
     (zipWith (-) (map (2 *) $ tail (oeis @6053)) (oeis @6053))

instance OEIS 6054 where
  oeis = 0 : 0 : 1 : zipWith (+) (map (2 *) $ drop 2 (oeis @6054))
     (zipWith (-) (tail (oeis @6054)) (oeis @6054))

instance OEIS 6128 where
  oeisIx = genericLength . concat . ps 1 where
     ps _ 0 = [[]]
     ps i j = [t:ts | t <- [i..j], ts <- ps t (j - t)]

instance OEIS 6190 where
  oeis = 0 : 1 : zipWith (+) (map (* 3) $ tail (oeis @6190)) (oeis @6190)

instance OEIS 6218 where
  oeisIx n = sum $ map (div n) [1..n]

instance OEIS 6221 where
  oeisIx n = (17 * n * (n + 1) + 5) * (2 * n + 1)

instance OEIS 6231 where
  oeisIx (succ->n) = numerator $
     sum $ tail $ zipWith (%) (scanl1 (*) [n, (n - 1)..1]) [1..n]

instance OEIS 6257 where
  oeis = 0 : 1 : (map (+ 1) $ zipWith mod (map (+ 1) $ tail (oeis @6257)) [2..])

instance OEIS 6277 where
  oeis = 1 : fix \r -> 1 : scanl ((*) . (+ 1)) 2 r

instance OEIS 6318 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where
       y = head xs + sum (zipWith (*) xs $ reverse xs)

instance OEIS 6331 where
  oeisIx n = sum $ zipWith (*) [2*n - 1, 2*n - 3 .. 1] [2, 4 ..]

instance OEIS 6336 where
  oeis = 1 : h 2 1 0 where
    h n last evens = x : h (n + 1) x (evens + 1 - x `mod` 2) where
      x = last + (oeisIx @6336 . pred) (n - 1 - evens)

instance OEIS 6337 where
  oeis = f [1] where
     f xs = ys ++ f ys where
            ys = concatMap (\z -> if z == 1 then [1,2] else [1,1,2]) xs

instance OEIS 6355 where
  oeis = 1 : fib2s where
     fib2s = 0 : map (+ 1) (scanl (+) 1 fib2s)

instance OEIS 6368 where
  oeisIx n | u' == 0   = 3 * u
            | otherwise = 3 * v + (v' + 1) `div` 2
            where (u,u') = divMod n 2; (v,v') = divMod n 4

instance OEIS 6369 where
  oeisIx n | m > 0     = round (4 * fi n / 3)
           | otherwise = 2 * n' where (n',m) = divMod n 3

instance OEIS 6370 where
  oeisIx n | m /= 0    = 3 * n + 1
           | otherwise = n' where (n',m) = divMod n 2

instance OEIS 6451 where
  oeis = 0 : 2 : 5 : 15 : map (+ 2)
     (zipWith (-) (map (* 6) (drop 2 (oeis @6451))) (oeis @6451))

instance OEIS 6497 where
  oeis = 2 : 3 : zipWith (+) (map (* 3) $ tail (oeis @6497)) (oeis @6497)

instance OEIS 6498 where
  oeis = 1 : 1 : 1 : 2 : zipWith (+) (drop 3 (oeis @6498))
     (zipWith (+) (tail (oeis @6498)) (oeis @6498))

instance OEIS 6504 where
  oeisIx (succ->n) = n * (42 + n * (59 + n * (18 + n))) `div` 24

instance OEIS 6516 where
  oeis = 0 : 1 :
      zipWith (-) (map (* 6) $ tail (oeis @6516)) (map (* 8) (oeis @6516))

instance OEIS 6519 where
  oeisIx (succ->fi->n) = fi (n .&. (-n) :: Integer)

instance OEIS 6527 where
  oeisIx n = n * (n ^ 2 + 2) `div` 3

instance OEIS 6530 where -- TODO: Optimize this- it's used a lot
  oeisIx n
    | null fs = 1
    | let     = fi . unPrime . fst $ last fs
    where fs  = factorise (1 + fi n :: Int)

instance OEIS 6564 where
  oeisIx (succ->n) = n * (5 * n * (n - 1) + 2) `div` 2

instance OEIS 6590 where
  oeisIx (succ->n) = sum $ map f [1..n] where
     f x = y + 1 - 0 ^ r where (y, r) = divMod n x

instance OEIS 6720 where
  oeis = [1,1,1,1] ++
     zipWith div (foldr1 (zipWith (+)) (map b [1..2])) (oeis @6720)
     where b i = zipWith (*) (drop i (oeis @6720)) (drop (4-i) (oeis @6720))

instance OEIS 6721 where
  oeis = [1,1,1,1,1] ++
    zipWith div (foldr1 (zipWith (+)) (map b [1..2])) (oeis @6721)
    where b i = zipWith (*) (drop i (oeis @6721)) (drop (5-i) (oeis @6721))

instance OEIS 6722 where
  oeis = [1,1,1,1,1,1] ++
    zipWith div (foldr1 (zipWith (+)) (map b [1..3])) (oeis @6722)
    where b i = zipWith (*) (drop i (oeis @6722)) (drop (6-i) (oeis @6722))

instance OEIS 6723 where
  oeis = [1,1,1,1,1,1,1] ++
    zipWith div (foldr1 (zipWith (+)) (map b [1..3])) (oeis @6723)
    where b i = zipWith (*) (drop i (oeis @6723)) (drop (7-i) (oeis @6723))

instance OEIS 6844 where
  oeis = 4 : 5 : ulam 2 5 (oeis @6844)

instance OEIS 6882 where
  oeis = 1 : 1 : zipWith (*) [2..] (oeis @6882)

instance OEIS 6906 where
  oeisIx n = p 1 n 1 where
     p _ 0 s = s
     p k m s | m<k = 0 | otherwise = p k (m-k) (k*s) + p (k+1) m s

instance OEIS 6949 where
  oeis = 1 : 1 : 1 : zipWith (+) xs (tail xs)
     where xs = map (oeisIx @6949) $ zipWith (-) [1..] $ tail (oeis @6949)

instance OEIS 6968 where
  oeisIx = fi . lenRom 3 . fi . succ where
     lenRom 0 z = z
     lenRom p z = [0, 1, 2, 3, 2, 1, 2, 3, 4, 2] !! m + lenRom (p - 1) z'
                  where (z',m) = divMod z 10

instance OEIS 6999 where
  oeis = 0 : map ((`div` 2) . (+ 2) . (* 3)) (oeis @6999)

instance OEIS 7061 where
  oeis = 1 : f [1] where
     f us = a' : f (us ++ [a']) where
       a' = b $ reverse $ map (`splitAt` us) [0..length us - 1] where
          b ((xs,ys):xyss) | vs `isSuffixOf` xs = 3 - head ys
                           | otherwise          = b xyss
       vs = fromJust $ find (`isInfixOf` init us) $ tails us

instance OEIS 7070 where
  oeis = 1 : 4 : (map (* 2) $ zipWith (-)
     (tail $ map (* 2) (oeis @7070)) (oeis @7070))

instance OEIS 7088 where
  oeisIx 0 = 0
  oeisIx n = 10 * (oeisIx @7088) n' + m where (n',m) = divMod n 2

instance OEIS 7089 where
  oeisIx 0 = 0
  oeisIx n = 10 * (oeisIx @7089) n' + m where (n', m) = divMod n 3

instance OEIS 7090 where
  oeisIx 0 = 0
  oeisIx n = 10 * (oeisIx @7090) n' + m where (n', m) = divMod n 4

instance OEIS 7092 where
  oeisIx 0 = 0
  oeisIx n = 10 * (oeisIx @7092) n' + m where (n', m) = divMod n 6

instance OEIS 7094 where
  oeisIx 0 = 0
  oeisIx n = 10 * (oeisIx @7094) n' + m where (n', m) = divMod n 8

instance OEIS 7095 where
  oeisIx = f where
     f 0 = 0
     f v = 10 * f w + r   where (w, r) = divMod v 9

instance OEIS 7283 where
  oeisIx = (* 3) . (2 ^)
  oeis = iterate (* 2) 3

instance OEIS 7300 where
  oeis = 2 : 5 : ulam 2 5 (oeis @7300)

instance OEIS 7310 where
  oeis = 1 : 5 : map (+ 6) (oeis @7310)

instance OEIS 7335 where
  oeis = 2 : 3 : f [3, 2] (M.singleton 6 1) where
     f xs m | v == 1    = y : f (y : xs) (g (map (y *) xs) m')
            | otherwise = f xs m'
            where g [] m = m
                  g (z:zs) m = g zs $ M.insertWith (+) z 1 m
                  ((y,v),m') = M.deleteFindMin m

instance OEIS 7376 where
  oeis = concatMap (map (fi . (read :: String -> Integer) . return) . show) [0..]

instance OEIS 7395 where
  oeisIx = const 2
  oeis = repeat 2

instance OEIS 7417 where
  oeis = s [1..] where
     s (x:xs) = x : s (delete (3*x) xs)

instance OEIS 7420 where
  oeis = 0 : 0 : 1 : (map (* 2) $ zipWith (+) (drop 2 (oeis @7420))
     (map (* 2) $ zipWith (-) (oeis @7420) (tail (oeis @7420))))

instance OEIS 7448 where
  oeis = f [0] [0] where
     f (x:xs) (y:ys) = z : f (xs ++ [2*z,2*z]) (ys ++ [3*z,3*z,3*z])
       where z = 1 + min x y

instance OEIS 7455 where
  oeis = 1 : 1 : 3 : 5 : zipWith (+)
     (map (* 2) (oeis @7455)) (map (* 3) $ drop 2 (oeis @7455))

instance OEIS 7464 where
  oeis = 1 : 1 : f [1,1] where
     f xs = y : f (y:xs) where y = sum $ zipWith gcd xs $ reverse xs

instance OEIS 7477 where
  oeis = 1 : 1 : f [1,1] where
     f xs = y : f (y:xs) where y = sum $ zipWith (*) (tail xs) (reverse xs)

instance OEIS 7481 where
  oeis = 1 : 2 : 3 : 7 : zipWith (+)
                 (map (* 3) $ drop 2 (oeis @7481)) (map (* 2) (oeis @7481))

instance OEIS 7482 where
  oeis = 1 : 3 : zipWith (+)
                 (map (* 3) $ tail (oeis @7482)) (map (* 2) (oeis @7482))

instance OEIS 7483 where
  oeis = 1 : 5 : zipWith (+)
                 (map (* 3) $ tail (oeis @7483)) (map (* 2) (oeis @7483))

instance OEIS 7484 where
  oeis = 2 : 7 : zipWith (+)
                 (map (* 3) $ tail (oeis @7484)) (map (* 2) (oeis @7484))

instance OEIS 7494 where
  oeisIx =  flip div 2 . (+ 1) . (* 3)

instance OEIS 7526 where
  oeis = 0 : zipWith (*) [1..] (map (+ 1) (oeis @7526))

instance OEIS 7531 where
  oeisIx n = product [n - 2..n]

instance OEIS 7532 where
  oeis = filter f [1..] where
     f x = g x 0 where
       g 0 v = v == x
       g u v = if d <= 1 then g u' (v + d) else v <= x && h d
               where h p = p <= x && (g u' (v + p) || h (p * d))
                     (u', d) = divMod u 10

instance OEIS 7538 where
  oeisIx (succ->n) = f n 2 2 2 where
     f 1 b _ _ = b
     f n b 0 i = f (n - 1) 2 (oeisIx @7538 $ pred i) (i + 1)
     f n b c i = f (n - 1) 3 (c - 1) i

instance OEIS 7590 where
  oeisIx = flip div 2 . (^ 2)

instance OEIS 7599 where
  oeis = 1 : f 1 1  where
     f x m = y : f y (m + 1) where
       y = x + (iterate (oeisIx @7599) (m- 2)) !! (m `div` 2)

instance OEIS 7606 where
  oeis = takeSkip 1 [1..] where
     takeSkip k xs = take k xs ++ takeSkip (k + 2) (drop (2*k + 1) xs)

instance OEIS 7607 where
  oeis = skipTake 1 [1..] where
     skipTake k xs = take (k + 1) (drop k xs)
                     ++ skipTake (k + 2) (drop (2*k + 1) xs)

instance OEIS 7660 where
  oeis = 0 : 0 : map (+ 1) (zipWith (*) (oeis @7660) $ tail (oeis @7660))

instance OEIS 7661 where
  oeis = 1 : 1 : 2 : zipWith (*) (oeis @7661) [3..]
instance Table 7661 where
  rowCol n k = (oeis @7661) `genericIndex` n
  tabl = undefined

instance OEIS 7731 where
  oeis = 1 : (zipWith3 (\u v w -> u + v + w)
     (map (oeisIx @7731 . (`div` 2)) [1..])
     (map (oeisIx @7731 . (`div` 3)) [1..])
     (map (oeisIx @7731 . (`div` 6)) [1..]))

instance OEIS 7745 where
  oeisIx (succ->fi->n) = fi $ n .|. (n ^ 2)

instance OEIS 7814 where
  oeisIx (succ->n) = if m == 0 then 1 + (oeisIx @7814 . pred) n' else 0
              where (n', m) = divMod n 2

instance OEIS 7882 where
  oeisIx (succ->n) = genericLength [ (x, y) | x <- [1..n], y <- [1..n], x^2 + y^2 < n^2]

instance OEIS 7908 where
  oeisIx = fi . (read . concatMap show . enumFromTo 1 :: Integer -> Integer) . fi . succ

instance OEIS 7931 where
  oeisIx (succ->n) = f (n + 1) where
     f x = if x < 2 then 0 else (10 * f x') + m + 1
       where (x', m) = divMod x 2

instance OEIS 7952 where
  oeis = f 1 [0..] where
     f k (x:xs) = x : f (k + 1) (g xs) where
       g ws = us ++ (g vs) where (us, _:vs) = splitAt k ws

instance OEIS 7953 where
  oeis = concat $ iterate (map succ) [0..9]
  oeisIx n | n < 10 = n | otherwise = (oeisIx @7953) n' + r where (n',r) = divMod n 10

instance OEIS 7954 where
  oeisIx n | n < 10 = n
           | otherwise = m * (oeisIx @7954) n' where (n', m) = divMod n 10

instance OEIS 7997 where
  -- oeisIx n = ceiling $ (fi $ (n - 3) * (n - 4)) / 6
  oeis = 0 : 0 : 1 : zipWith (+) (oeis @7997) [1..]

instance OEIS 8133 where
  oeis = zipWith (*) (tail ts) ts where ts = map (`div` 3) [0..]

instance OEIS 8217 where
  oeis = zipWith (*) (tail qs) qs where qs = map (`div` 4) [0..]

instance OEIS 8233 where
  oeisIx n = product $ map (`div` 4) [n..n+3]

instance OEIS 8279 where
  oeis = tablList @8279
instance Table 8279 where
  tabl = iterate f [1] where
     f xs = zipWith (+) ([0] ++ zipWith (*) xs [1..]) (xs ++ [0])

instance OEIS 8281 where
  oeis = tablList @8281
instance Table 8281 where
  tabl = iterate (scanl (+) 0 . reverse) [1]

instance OEIS 8288 where
  oeis = tablList @8288
instance Table 8288 where
  tabl = map fst $ iterate
      (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

instance OEIS 8301 where
  oeis = tablList @8301
instance Table 8301 where
  tabf = iterate f [1] where
     f zs = zs' ++ tail (reverse zs') where
       zs' = (sum zs) : h (0 : take (length zs `div` 2) zs) (sum zs) 0
       h []     _  _ = []
       h (x:xs) y' y = y'' : h xs y'' y' where y'' = 2*y' - 2*x - y

instance OEIS 8318 where
  oeis = f [1] (S.singleton 1) where
     f xs s =
       m : f (m:xs) (foldl (flip S.insert) s' (map (+ m^2) (map (^ 2) xs)))
       where (m,s') = S.deleteFindMin s

instance OEIS 8336 where
  oeis = 1 : zipWith (/*) (oeis @8336) [1..] where
      x /* y = if x `mod` y == 0 then x `div` y else x*y

instance OEIS 8344 where
  oeis = 0 : f 0 [1..] where
     f x (z:zs) = y : f y zs where y = if x < z then x + z else x - z

instance OEIS 8486 where
  oeisIx 0 = 1
  oeisIx n = 3 * n
  oeis = 1 : [3, 6 ..]

instance OEIS 8574 where
  oeisIx 0 = 1
  oeisIx n = 4 * n
  oeis = 1 : [4, 8 ..]

instance OEIS 8579 where
  oeisIx 0 = 1
  oeisIx 1 = 4
  oeisIx n = (10 - 2*m) * n' + 8*m - 2 where (n',m) = divMod n 2
  oeis = 1 : 4 : concatMap (\x -> map (* 2) [5*x - 1,4*x+3]) [1..]

instance OEIS 8585 where
  oeisIx = (* 3)
  oeis = iterate (+ 3) 0

instance OEIS 8586 where
  oeisIx = (* 4)
  oeis = [0, 4 ..]

instance OEIS 8588 where
  oeisIx = (* 6)
  oeis = [0, 6 ..]

instance OEIS 8589 where
  oeisIx = (* 7)
  oeis = [0, 7 ..]

instance OEIS 8590 where
  oeisIx = (* 8)
  oeis = [0,8..]

instance OEIS 8592 where
  oeisIx = (10 *)

instance OEIS 8593 where
  oeisIx = (* 11)
  oeis = [0, 11 ..]

instance OEIS 8594 where
  oeisIx = (* 12)
  oeis = [0, 12 ..]

instance OEIS 8597 where
  oeisIx = (* 15)
  oeis = [0, 15 ..]

instance OEIS 8611 where
  oeisIx n = n' + mod r 2 where (n', r) = divMod (n + 1) 3
  oeis = f [1,0,1] where f xs = xs ++ f (map (+ 1) xs)

instance OEIS 8615 where
  oeisIx n = n `div` 2 - n `div` 3

instance OEIS 8619 where
  oeisIx = (+ 1) . (`div` 2)
  oeis = concatMap (\x -> [x,x]) [1..]

instance OEIS 8620 where
  oeisIx = (+ 1) . (`div` 3)
  oeis   = concatMap (replicate 3) [1..]

instance OEIS 8683 where
  oeisIx = fi . mu . snd . unzip . factorise . fi . succ
    where
      mu [] = 1; mu (1:es) = - mu es; mu (_:es) = 0

instance OEIS 8684 where
  oeis = concatMap (enumFromTo 1) [31,28,31,30,31,30,31,31,30,31,30,31]

instance OEIS 8685 where
  oeis = concatMap t [1..] where
     t y = [31, 28 + leap, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
           where leap = if mod y 4 == 0 &&
                           (mod y 100 > 0 || mod y 400 == 0) then 1 else 0

instance OEIS 8687 where
  oeis = 0 : 1 : c [1] where c (e:es) = e : c (es ++ [e+1,e])

instance OEIS 8776 where
  oeisIx = (* 2) . (3 ^)
  oeis = iterate (* 3) 2

instance OEIS 8846 where
  oeis = filter f [5..] where
     f n = all ((== 1) . (`mod` 4)) $ filter ((== 0) . (n `mod`)) [1..n]

instance OEIS 8851 where
  oeis = [10*n + m | n <- [0..], m <- [0,1,5,6]]

instance OEIS 8865 where
  oeisIx = (subtract 2) . (^ 2) . succ
  oeis = scanl (+) (-1) [3, 5 ..]

instance OEIS 8904 where
  oeis = 1 : 1 : f 2 1 where
     f n x = x' `mod` 10 : f (n+1) x' where
        x' = g (n * x) where
           g m | m `mod` 5 > 0 = m
               | otherwise     = g (m `div` 10)

instance OEIS 8935 where
  oeisIx = f 1 . succ where
     f k x | x == 0    = 0
           | r == 0    = f (k+1) x'
           | otherwise = k^2 + f (k+1) x' where (x',r) = divMod x 2

instance OEIS 8955 where
  oeis = tablList @8955
instance Table 8955 where
  tabl = [1] : f [1] 1 1 where
     f xs u t = ys : f ys v (t * v) where
       ys = zipWith (+) (xs ++ [t^2]) ([0] ++ map (* u^2) (init xs) ++ [0])
       v = u + 1

instance OEIS 8956 where
  oeis = tablList @8956
instance Table 8956 where
  tabl = [1] : f [1] 1 1 where
     f xs u t = ys : f ys v (t * v) where
       ys = zipWith (+) (xs ++ [t^2]) ([0] ++ map (* u^2) (init xs) ++ [0])
       v = u + 2

instance OEIS 8975 where
  oeis = tablList @8975
instance Table 8975 where
  tabl = iterate
     (\row -> map (`mod` 10) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 9293 where
  oeis = f [2] (S.singleton 2) where
     f xs s = m : f xs' (foldl (flip S.insert) s' (map ((+ 1) . (* m)) xs'))
       where xs' = m : xs
             (m,s') = S.deleteFindMin s

instance OEIS 9299 where
  oeis = f [2] (S.singleton 2) where
     f xs s = m : f xs' (foldl (flip S.insert) s' (map ((+ 2) . (* m)) xs'))
       where xs' = m : xs
             (m,s') = S.deleteFindMin s

instance OEIS 9388 where
  oeis = f [2] (S.singleton 2) where
     f xs s = m : f xs' (foldl (flip S.insert) s' (map (pred . (* m)) xs'))
       where xs' = m : xs
             (m,s') = S.deleteFindMin s

instance OEIS 9445 where
  oeisIx n = product [1..2*n+1]

instance OEIS 9766 where
  oeis = tablList @9766
instance Table 9766 where
  tabl = iterate (\row -> scanl1 (+) (row ++ [0])) [1]

instance OEIS 9947 where
  oeis = concatMap (\x -> [2 * x, x, 2 * x + 1]) [0..]

instance OEIS 9994 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = m : f (foldl (flip S.insert) s' $ map (10*m +) [m `mod` 10 ..9])
           where (m,s') = S.deleteFindMin s

instance OEIS 9995 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = case S.minView s of
           Nothing     -> []
           Just (m,s') -> m : f (foldl (flip S.insert) s' $
                                map (10*m +) [0..m `mod` 10 - 1])

instance OEIS 10060 where
  oeis =
     0 : interleave (complement (oeis @10060)) (tail (oeis @10060))
     where complement = map (1 - )
           interleave (x:xs) ys = x : interleave ys xs

instance OEIS 10693 where
  oeisIx = (+ 2) . (`mod` 2)
  oeis = cycle [2,3]

instance OEIS 10701 where
  oeisIx = const 3
  oeis = repeat 3

instance OEIS 10702 where
  oeisIx = (+ 3) . (`mod` 2)
  oeis = cycle [3,4]

instance OEIS 10712 where
  oeisIx = (+ 3) . (4 ^) . flip mod 2
  oeis = cycle [4, 7]

instance OEIS 10785 where
  oeis = 0 : r [1..9] where
     r (x:xs) = x : r (xs ++ [10*x + x `mod` 10])

instance OEIS 10786 where
  oeisIx (succ->n) = product $ map (div n) [1..n]

instance OEIS 10807 where
  oeisIx = (^ 19)

instance OEIS 10872 where
  oeisIx = (`mod` 3)
  oeis = cycle [0,1,2]

instance OEIS 10873 where
  oeisIx = (`mod` 4)
  oeis = cycle [0..3]

instance OEIS 10878 where
  oeisIx = (`mod` 9)
  oeis = cycle [0..8]

instance OEIS 10879 where
  oeisIx = (`mod` 10)
  oeis = cycle [0..9]

instance OEIS 10882 where
  oeisIx = (+ 1) . (`mod` 3)
  oeis = cycle [1,2,3]

instance OEIS 10887 where
  oeisIx = (+ 1) . flip mod 8
  oeis = cycle [1..8]

instance OEIS 11199 where
  oeisIx n = product $ map ((+ 1) . (* n)) [1, 2, 3]

instance OEIS 11531 where
  oeis = filter ((elem '1') . show . fi) [0..]

instance OEIS 11532 where
  oeis = filter ((elem '2') . show . fi) [0..]

instance OEIS 11533 where
  oeis = filter ((elem '3') . show . fi) [0..]

instance OEIS 11557 where
  oeisIx = (10 ^)
  oeis   = iterate (* 10) 1

instance OEIS 11655 where
  oeisIx = fi . fromEnum . ((/= 0) . (`mod` 3))
  oeis   = cycle [0,1,1]

instance OEIS 11782 where
  oeis = 1 : scanl1 (+) (oeis @11782)

instance OEIS 11971 where
  oeis = tablList @11971
instance Table 11971 where
  tabl = iterate (\row -> scanl (+) (last row) row) [1]

instance OEIS 13610 where
  oeis = tablList @13610
instance Table 13610 where
  tabl = iterate (\row ->
     zipWith (+) (map (* 1) (row ++ [0])) (map (* 3) ([0] ++ row))) [1]

instance OEIS 13620 where
  oeis = tablList @13620
instance Table 13620 where
  tabl = iterate (\row ->
     zipWith (+) (map (* 2) (row ++ [0])) (map (* 3) ([0] ++ row))) [1]

instance OEIS 13979 where
  oeis = 1 : 0 : 1 : 1 : zipWith (+) (oeis @13979)
     (zipWith (+) (tail (oeis @13979)) (drop 2 (oeis @13979)))

instance OEIS 14011 where
  oeis = 1 : f 2 [1] where
     f u vs = w : f (u + 1) (w : vs) where
       w = maximum $ zipWith (*) [u, u - 1 ..] $ map (u -) vs

instance OEIS 14113 where
  oeis = 0 : 2 : zipWith (+) (map (* 2) (oeis @14113)) (tail (oeis @14113))

instance OEIS 14261 where
  oeis = filter (all (`elem` "13579") . show . fi) [1,3..]

instance OEIS 14263 where
  oeis = filter (all (`elem` "02468") . show . fi) [0,2..]

instance OEIS 14311 where
  oeis = [2^x + 2^y + 2^z | x <- [2..], y <- [1..x - 1], z <- [0..y - 1]]

instance OEIS 14601 where
  oeis = [x | x <- [0..], mod x 4 `elem` [0, 3]]

instance OEIS 14616 where
  oeisIx (succ->n) = (n * (n + 6) + 1) `div` 4

instance OEIS 14682 where
  oeisIx n = if r > 0 then div (3 * n + 1) 2 else n'
              where (n', r) = divMod n 2

instance OEIS 14707 where
  oeis = f 0 $ cycle [0,0,1,0] where
     f i (x:_:xs) = x : (oeisIx @14707) i : f (i+1) xs

instance OEIS 15632 where
  oeisIx (succ->n) = genericLength [ (x,y,z) | z <- [1..n], y <- [1..z], gcd y z == 1,
                                x <- [1..y], gcd x z == 1, gcd x y == 1]

instance OEIS 15633 where
  oeisIx (succ.succ->n) = genericLength [ (x,y,z) | x <- [2..n], y <- [x..n], z <- [y..n],
                                gcd (gcd x y) z == 1]

instance OEIS 16069 where
  oeis = filter ((== 2) . length . nub . show . fi . (^ 2)) [0..]

instance OEIS 16125 where
  oeis = iterate ((+ 1) . (* 12)) 1

instance OEIS 16742 where
  oeisIx = (* 4) . (^ 2)
  oeis = 0 : map (subtract 4) (zipWith (+) (oeis @16742) [8, 16 ..])

instance OEIS 16777 where
  oeisIx = (+ 1) . (* 3)
  oeis = [1, 4 ..]

instance OEIS 16789 where
  oeisIx = (+ 2) . (* 3)

instance OEIS 16813 where
  oeisIx = (+ 1) . (* 4)
  oeis = [1, 5 ..]

instance OEIS 16825 where
  oeisIx = (+ 2) . (* 4)
  oeis = [2, 6 ..]

instance OEIS 16861 where
  oeisIx = (+ 1) . (* 5)
  oeis = [1, 6 ..]

instance OEIS 16921 where
  oeisIx = (+ 1) . (* 6)
  oeis = [1, 7 ..]

instance OEIS 16933 where
  oeisIx = (+ 2) . (* 6)

instance OEIS 16945 where
  oeisIx = (+ 3) . (* 6)
  oeis = [3, 9 ..]

instance OEIS 16957 where
  oeisIx = (+ 4) . (* 6)

instance OEIS 16993 where
  oeisIx = (+ 1) . (* 7)
  oeis = [1, 8 ..]

instance OEIS 17077 where
  oeisIx = (+ 1) . (* 8)
  oeis = [1, 9 ..]

instance OEIS 17089 where
  oeisIx = (+ 2) . (* 8)
  oeis = [2, 10 ..]

instance OEIS 17113 where
  oeisIx = (+ 4) . (* 8)
  oeis = [4, 12 ..]

instance OEIS 17137 where
  oeisIx = (+ 6) . (* 8)

instance OEIS 17173 where
  oeisIx = (+ 1) . (* 9)
  oeis = [1, 10 ..]

instance OEIS 17197 where
  oeisIx = (+ 3) . (* 9)
  oeis = [3, 12 ..]

instance OEIS 17257 where
  oeisIx = (+ 8) . (* 9)
  oeis = 8 : map (+ 9) (oeis @17257)

instance OEIS 17281 where
  oeisIx = (+ 1) . (* 10)
  oeis = [1,11..]

instance OEIS 17329 where
  oeisIx = (+ 5) . (* 10)
  oeis = [5, 15 ..]

instance OEIS 17557 where
  oeisIx = (+ 3) . (* 12)

instance OEIS 17569 where
  oeisIx = (+ 4) . (* 12)

instance OEIS 17581 where
  oeisIx = (+ 5) . (* 12)

instance OEIS 17617 where
  oeisIx = (+ 8) . (* 12)

instance OEIS 17629 where
  oeisIx = (+ 9) . (* 12)

instance OEIS 17653 where
  oeisIx = (+ 11) . (* 12)

instance OEIS 18804 where
  oeisIx (succ->n) = sum $ map (gcd n) [1..n]

instance OEIS 18805 where
  oeisIx (succ->n) = genericLength [ ()| x <- [1..n], y <- [1..n], gcd x y == 1]

instance OEIS 18834 where
  oeis = map fi $ filter (\(x :: Int) -> show x `isInfixOf` show (x^2)) [0..]

instance OEIS 18892 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n], n^2 `mod` d == 0]

instance OEIS 19446 where
  oeis = 1 : zipWith (-) [3..] (map (oeisIx @19446 . pred) (oeis @19446))

instance OEIS 19464 where
  oeis = 1 : concat (unfoldr ma (1, [1, 1])) where
     ma (x, [_, j]) = Just (ij', (x + 1, ij')) where ij' = [x * j, x * j + x]

instance OEIS 20338 where
  oeisIx (succ->n) = fi (read (ns ++ ns) :: Integer) where ns = show $ fi n

instance OEIS 20639 where
  oeisIx (succ->n) = spf primes where
    spf (p:ps) | n < p^2      = n
               | mod n p == 0 = p
               | otherwise    = spf ps

instance OEIS 20650 where
  oeis = map numerator ks where
     ks = 1 : concat (transpose [map (+ 1) ks, map (recip . (+ 1)) ks])

instance OEIS 20651 where
  oeis = map denominator ks where
     ks = 1 : concat (transpose [map (+ 1) ks, map (recip . (+ 1)) ks])

instance OEIS 20652 where
  oeis = map fst [ (u,v) | v <- [1..], u <- [1..v - 1], gcd u v == 1]

instance OEIS 20944 where
  oeis = -1 : f [1,0] where f (x:y:xs) = x : f (y:xs ++ [x,x+y])

instance OEIS 20951 where
  oeis = 1 : ws where
     ws = 0 : 1 : concat (zipWith (\u v -> [u, u + v]) ws $ tail ws)

instance OEIS 20985 where
  oeis = 1 : 1 : f (tail (oeis @20985)) (-1) where
     f (x:xs) w = x : x*w : f xs (0 - w)

instance OEIS 22112 where
  oeis = 2 : 6 : zipWith (+) (tail (oeis @22112)) (oeis @22112)

instance OEIS 22319 where
  oeis = 1 : 5 : zipWith (+)
     (map (+ 1) (oeis @22319)) (tail (oeis @22319))

instance OEIS 22328 where
  oeis = oeis22328

instance OEIS 22329 where
  oeis = oeis22329

instance OEIS 22481 where
  oeis = map fi do 1 : f [1] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22819 where
  oeisIx n = floor $ sum $ zipWith (%) [1 .. n - 1] [n - 1, n - 2 .. 1]

instance OEIS 22838 where
  oeisIx = floor . (* sqrt 3) . fi . succ

instance OEIS 22840 where
  oeisIx = floor . (* sqrt 6) . fi . succ

instance OEIS 22843 where
  oeis = map (floor . (* e) . fi) [0..] where e = exp 1

instance OEIS 22846 where
  oeisIx = round . (* sqrt 2) . fi

instance OEIS 22852 where
  oeisIx = round . (* exp 1) . fi

instance OEIS 22941 where
  oeis = 1 : 2 : f 2 [3..] where
     f x (z:zs) = y : f y (delete y zs) where y = x + z

instance OEIS 23022 where
  oeisIx ((+2)->n) = genericLength [ (u, v) | u <- [1 .. div n 2],
                               let v = n - u, gcd u v == 1]

instance OEIS 23105 where
  oeisIx n = f (n) where
    f 0 = 1
    f 1 = 2
    f n | even n = 2 * f (n - 1) - 2
    f n | odd  n = 2 * f (n - 1) - 1

instance OEIS 23416 where
  oeisIx 0 = 1
  oeisIx 1 = 0
  oeisIx n = (oeisIx @23416) n' + 1 - m where (n', m) = divMod n 2
  oeis = 1 : c [0] where c (z:zs) = z : c (zs ++ [z+1,z])

instance OEIS 23705 where
  oeis = iterate f 1 where
     f x = 1 + if r < 3 then x else 4 * f x'
           where (x', r) = divMod x 4

instance OEIS 23717 where
  oeis = filter f [0..] where
     f x = x < 3 || (q < 3 && f x') where (x', q) = divMod x 4

instance OEIS 23758 where
  oeis = 0 : f (S.singleton 1) where
    f s = x : f (if even x then S.insert z s' else S.insert z $ S.insert (z+1) s')
     where z = 2*x; (x, s') = S.deleteFindMin s

instance OEIS 23811 where
  oeisIx (succ->n) = foldl (\val dig -> val * n + dig) 0 [0 .. n - 1]

instance OEIS 23855 where
  oeisIx (succ->n) = sum $ zipWith (*) [1 .. div (n+1) 2] [n, n - 1 ..]

instance OEIS 24004 where
  oeisIx = (1 -) . (^ 6)

instance OEIS 24014 where
  -- oeis = oeis24014
  oeisIx n = 2^n-n^4

instance OEIS 24916 where
  oeisIx (succ->n) = sum $ map (\k -> k * div n k) [1..n]

instance OEIS 25147 where
  oeisIx = p 2 where
     p _ 0 = 1
     p k m = if m < k then 0 else p (k + 1) (m - k) + p (k + 1) m

instance OEIS 25169 where
  oeis = 2 : 6 : zipWith (-) (map (* 3) $ tail (oeis @25169)) (oeis @25169)

instance OEIS 25192 where
  oeisIx 0 = 1
  oeisIx n = 2 * 3 ^ (n - 1)
  oeis = 1 : iterate (* 3) 2

instance OEIS 25250 where
  oeis = 0 : 1 : 1 : f 1 [1,1,0] where
     f k xs = x' : f (k+1) (x':xs) where
       x' = sum $ zipWith (*) (oeis @25250) $ take k xs

instance OEIS 25276 where
  oeis = 1 : 0 : 0 : 1 : f [1,0,0,1] where
     f xs = x' : f (x':xs) where
       x' = sum $ zipWith (*) xs (oeis @25276)

instance OEIS 25550 where
  oeisIx (succ->n) = numerator $ sum $ map (1 %) $ take (fi n) [1, 3 ..]

instance OEIS 25613 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 25616 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (3 * y, i + 1, j) $ S.insert (10 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 25620 where
  oeis = f $ S.singleton 1 where
     f s = y : f (S.insert (4 * y) $ S.insert (9 * y) s')
                 where (y, s') = S.deleteFindMin s

instance OEIS 25632 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (7 * y, i + 1, j) $ S.insert (10 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 25635 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (9 * y, i + 1, j) $ S.insert (10 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 26300 where
  oeis = tablList @26300
instance Table 26300 where
  tabl = iterate (\row -> zipWith (+) ([0,0] ++ row) $
                                  zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 26374 where
  oeis = tablList @26374
instance Table 26374 where
  tabl = [1] : map fst (map snd $ iterate f (1, ([1, 1], [1]))) where
     f (0, (us, vs)) = (1, (zipWith (+) ([0] ++ us) (us ++ [0]), us))
     f (1, (us, vs)) = (0, (zipWith (+) ([0] ++ vs ++ [0]) $
                               zipWith (+) ([0] ++ us) (us ++ [0]), us))

instance OEIS 26637 where
  oeis = tablList @26637
instance Table 26637 where
  tabl = [1] : [1,1] : map (fst . snd)
     (iterate f (0, ([1,2,1], [0,1,1,0]))) where
     f (i, (xs, ws)) = (1 - i,
       if i == 1 then (ys, ws) else (zipWith (+) ys ws, ws'))
          where ys = zipWith (+) ([0] ++ xs) (xs ++ [0])
                ws' = [0,1,0,0] ++ drop 2 ws

instance OEIS 26741 where
  oeis = concat $ transpose [[0..], [1,3..]]

instance OEIS 26832 where
  oeisIx 0 = 0
  oeisIx n = p 1 n where
     p _ 0 = 1
     p k m = if m < k then 0 else p (k+1) (m-k) + p (k+1+0^ (n-m)) m

instance OEIS 26898 where
  oeisIx n = sum $ zipWith (^) [n + 1, n .. 1] [0 ..]

instance OEIS 27384 where
  oeisIx n = genericLength $ nub [i*j | i <- [0..n], j <- [0..n]]

instance OEIS 27424 where
  oeisIx (succ->n) = genericLength $ nub [i*j | i <- [1..n], j <- [1..n]]

instance OEIS 27425 where
  oeisIx (succ->n) = genericLength $ nub [i*j*k | i <- [1..n], j <- [1..n], k <- [1..n]]

instance OEIS 27426 where
  oeisIx n = genericLength $ nub [i*j*k | i <- [0..n], j <- [0..n], k <- [0..n]]

instance OEIS 27427 where
  oeisIx n = genericLength $ nub [i*j | j <- [1..n], i <- [0..j - 1]]

instance OEIS 27428 where
  oeisIx (succ->n) = genericLength $ nub [i*j | j <- [2..n], i <- [1..j - 1]]

instance OEIS 27429 where
  oeisIx n = genericLength $ nub [i*j*k | k<-[2..n], j<-[1..k - 1], i<-[0..j - 1]]

instance OEIS 27430 where
  oeisIx (succ->n) = genericLength $ nub [i*j*k | k<-[3..n], j<-[2..k - 1], i<-[1..j - 1]]

instance OEIS 27465 where
  oeis = tablList @27465
instance Table 27465 where
  tabl = iterate (\row ->
     zipWith (+) (map (* 3) (row ++ [0])) (map (* 1) ([0] ++ row))) [1]

instance OEIS 27611 where
  oeisIx (succ->n) = denominator $ sum $ map (n %) [1..n]

instance OEIS 27612 where
  oeisIx (succ->n) = numerator $ sum $ zipWith (%) [1 .. n] [n, n - 1 .. 1]

instance OEIS 27649 where
  oeis = map fst $ iterate (\ (u, v) -> (3 * u + v, 2 * v)) (1, 1)

instance OEIS 27665 where
  oeisIx (succ->n) = round $ 100000 * log (fi n) / (log 10)

instance OEIS 27750 where
  oeis = tablList @27750
instance Table 27750 where
  rowCol n k = (rowT @27750) n `genericIndex` (k - 1)
  rowT = sort . divisors
  tabf = map (rowT @27750) [1..]

instance OEIS 27907 where
  oeis = tablList @27907
instance Table 27907 where
  tabf = [1] : iterate f [1, 1, 1] where
     f row = zipWith3 (((+) .) . (+))
                      (row ++ [0, 0]) ([0] ++ row ++ [0]) ([0, 0] ++ row)

instance OEIS 27934 where
  oeis = 0 : 1 : 2 : zipWith3 (\x y z -> 3 * x - y - 2 * z)
                 (drop 2 (oeis @27934)) (tail (oeis @27934)) (oeis @27934)

instance OEIS 28262 where
  oeis = tablList @28262
instance Table 28262 where
  tabl = [1] : [1,1] : iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,3,1]

instance OEIS 28290 where
  oeis = map (p' 0) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p 5 _ = 0
     p k m | m < parts !! k = 0
           | otherwise = p' k (m - parts !! k) + p' (k + 1) m
     parts = [1, 2, 3, 5, 8]

instance OEIS 28310 where
  oeisIx n = 0 ^ n + n
  oeis = 1 : [1..]

instance OEIS 28326 where
  oeis = tablList @28326
instance Table 28326 where
  tabl = iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [2]

instance OEIS 28387 where
  oeisIx n = n + (n + 1) ^ 2

instance OEIS 28560 where
  oeisIx n = n * (n + 6)

instance OEIS 28846 where
  oeis = f [1] where
     f ds = foldr (\d v -> 10 * v + d) 0 ds : f (s ds)
     s [] = [1]; s (8:ds) = 1 : s ds; s (d:ds) = 2*d : ds

instance OEIS 28859 where
  oeis = 1 : 3 : map (* 2) (zipWith (+) (oeis @28859) (tail (oeis @28859)))

instance OEIS 28860 where
  oeis = -1 : 1 : map (* 2) (zipWith (+) (oeis @28860) (tail (oeis @28860)))

instance OEIS 28884 where
  oeisIx n = (n + 3)^2 - 8

instance OEIS 28897 where
  oeisIx 0 = 0
  oeisIx n = 2 * (oeisIx @28897) n' + d where (n', d) = divMod n 10

instance OEIS 29145 where
  oeis = map (p' 0) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p 4 _ = 0
     p k m | m < parts !! k = 0
           | otherwise = p' k (m - parts !! k) + p' (k + 1) m
     parts = [2, 3, 5, 8]

instance OEIS 29549 where
  oeis = [0,6,210] ++
     zipWith (+) (oeis @29549)
                 (map (* 35) $ tail delta)
     where delta = zipWith (-) (tail (oeis @29549)) (oeis @29549)

instance OEIS 29579 where
  oeisIx n = if m == 0 then n' + 1 else n  where (n', m) = divMod n 2
  oeis = concat $ transpose [[1 ..], [1, 3 ..]]

instance OEIS 29600 where
  oeis = tablList @29600
instance Table 29600 where
  tabl = [1] : iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [2,3]

instance OEIS 29635 where
  oeis = tablList @29635
instance Table 29635 where
  tabl = [2] : iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,2]

instance OEIS 29653 where
  oeis = tablList @29653
instance Table 29653 where
  tabl = [1] : iterate
                 (\xs -> zipWith (+) ([0] ++ xs) (xs ++ [0])) [2, 1]

instance OEIS 29744 where
  oeis = 1 : iterate
     (\x -> if x `mod` 3 == 0 then 4 * x `div` 3 else 3 * x `div` 2) 2

instance OEIS 29793 where
  oeis = filter (\x -> digs x == digs (x^2)) [0..]
     where digs = sort . nub . show . fi

instance OEIS 29858 where
  oeisIx = (`div` 2) . (subtract 3) . (3 ^) . succ
  oeis = iterate ((+ 3) . (* 3)) 0

instance OEIS 29942 where
  oeis = [fi x | (x :: Int) <- [0..], show x `isInfixOf` show (x^3)]

instance OEIS 29943 where
  oeis = filter f [0..] where
     f (fi->x) = show x `isInfixOf` show (x^2) && show x `isInfixOf` show (x^3)

instance OEIS 30067 where
  oeis = concat $ transpose [scanl (+) 1 (oeis @30067), (oeis @30067)]

instance OEIS 30099 where
  oeis = filter (null . (intersect "86420") . show . fi . (^ 3)) [1,3..]

instance OEIS 30101 where
  oeisIx = f 0 where
     f y 0 = y
     f y x = f (2 * y + b) x'  where (x', b) = divMod x 2

instance OEIS 30103 where
  oeisIx n = foldl (\v d -> 4*v + d) 0 $ unfoldr dig n where
      dig x = if x == 0 then Nothing else Just $ swap $ divMod x 4

instance OEIS 30124 where
  oeis = figureDiff 1 [2..] where
     figureDiff n (x:xs) = x : figureDiff n' (delete n' xs) where n' = n + x

instance OEIS 30186 where
  oeis = 1 : 2 : 7 : zipWith (-) (tail $
     zipWith (+) (oeis @30186) $ tail $ map (* 3) (oeis @30186)) (oeis @30186)

instance OEIS 30195 where
  oeis = 0 : 1 : map (* 3) (zipWith (+) (oeis @30195) (tail (oeis @30195)))

instance OEIS 30273 where
  oeisIx n = p (map (^ 2) [1..]) (n^2) where
     p _  0 = 1
     p (k:ks) m | m < k     = 0
                | otherwise = p ks (m - k) + p ks m

instance OEIS 30283 where
  oeis = 0 : f 1 9 0 where
     f u v w = w' : f u' v' w' where
       w' = until (> w) ((+ v) . (* 10)) u
       (u',v') = h u v
       h 1 0 = (2,2); h 9 0 = (1,1); h 9 1 = (2,0); h 9 9 = (1,0)
       h u 2 = (u+1,0); h u v = (u+1,1-v)

instance OEIS 30308 where
  oeis = tablList @30308
instance Table 30308 where
  -- rowCol n k = (tabf @30308) !! n !! k
  -- rowT n = (tabf @30308) !! n
  tabf = iterate bSucc [0] where
     bSucc []       = [1]
     bSucc (0 : bs) = 1 : bs
     bSucc (1 : bs) = 0 : bSucc bs

instance OEIS 30341 where
  oeis = tablList @30341
instance Table 30341 where
  tabf = iterate succ [0] where
     succ []     = [1]
     succ (2:ts) = 0 : succ ts
     succ (t:ts) = (t + 1) : ts

instance OEIS 30386 where
  oeis = tablList @30386
instance Table 30386 where
  tabf = iterate succ [0] where
     succ []     = [1]
     succ (3:ts) = 0 : succ ts
     succ (t:ts) = (t + 1) : ts

instance OEIS 30530 where
  oeis = 0 : concatMap (\n -> unfoldr
     (\x -> if x == 0 then Nothing else Just (n, div x 2)) n) [1..]

instance OEIS 31087 where
  oeis = tablList @31087
instance Table 31087 where
  rowCol n k = (rowT @31087) n !! (k - 1)
  rowT n | n < 9     = [n]
                | otherwise = m : (rowT @31087) n' where (n',m) = divMod n 9
  tabf = map (rowT @31087) [0..]

instance OEIS 31235 where
  oeis = tablList @31235
instance Table 31235 where
  tabf = iterate succ [0] where
     succ []     = [1]
     succ (4:ts) = 0 : succ ts
     succ (t:ts) = (t + 1) : ts

instance OEIS 31298 where
  oeis = tablList @31298
instance Table 31298 where
  tabf = iterate succ [0] where
     succ []     = [1]
     succ (9:ds) = 0 : succ ds
     succ (d:ds) = (d + 1) : ds

instance OEIS 31972 where
  oeisIx n = sum $ take (fi n) $ iterate (* n) n

instance OEIS 32740 where
  oeis = [fi x | (x :: Int) <- [0..], show x `isInfixOf` (show $ 2 ^ x)]

instance OEIS 32766 where
  oeisIx n = div n 2 + n

instance OEIS 32924 where
  oeis = iterate f 1 where
     f x = 1 + if r < 2 then x else 3 * f x'  where (x', r) = divMod x 3

instance OEIS 32925 where
  oeis = 1 : 2 : (concat $ transpose [map (+ 1) fs, map (+ 2) fs])
                 where fs = map (* 4) (oeis @32925)

instance OEIS 33048 where
  oeis = (0:) $ filter (all (< 2) . unfoldr (\x ->
     if x == 0 then Nothing else Just $ swap $ divMod x 12)) [1..]

instance OEIS 33075 where
  oeis = f (S.fromList [1..9]) where
     f s | d == 0    = m : f (S.insert (10*m+1) s')
         | d == 9    = m : f (S.insert (10*m+8) s')
         | otherwise = m : f (S.insert (10*m+d- 1) (S.insert (10*m+d+1) s'))
         where (m,s') = S.deleteFindMin s
               d = mod m 10

instance OEIS 33307 where
  oeis = concatMap (map (fi . read . return) . show . fi) [1..]

instance OEIS 33484 where
  oeisIx = (subtract 2) . (* 3) . (2 ^)
  oeis = iterate ((subtract 2) . (* 2) . (+ 2)) 1

instance OEIS 33581 where
  oeisIx = (* 6) . (^ 2)

instance OEIS 33619 where
  oeis = [0..9] ++ (f $ S.fromList [10..99]) where
     f s = m : f (S.insert (m * 10 + h) s') where
       h = div (mod m 100) 10
       (m,s') = S.deleteFindMin s

instance OEIS 33627 where
  oeis = f [1..] [] where
     f (x:xs) ys = x : f (xs \\ (map (+ x) ys)) (x:ys)

instance OEIS 33638 where
  oeisIx = (+ 1) . (`div` 4) . (^ 2)

instance OEIS 33677 where
  oeisIx (succ->n) = head $
     dropWhile ((< n) . (^ 2)) [d | d <- [1..n], mod n d == 0]

instance OEIS 33845 where
  oeis = f (S.singleton (2*3)) where
     f s = m : f (S.insert (2*m) $ S.insert (3*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 33846 where
  oeis = f (S.singleton (2*5)) where
     f s = m : f (S.insert (2*m) $ S.insert (5*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 33847 where
  oeis = f (S.singleton (2*7)) where
     f s = m : f (S.insert (2*m) $ S.insert (7*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 33848 where
  oeis = f (S.singleton (2*11)) where
     f s = m : f (S.insert (2*m) $ S.insert (11*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 33849 where
  oeis = f (S.singleton (3*5)) where
     f s = m : f (S.insert (3*m) $ S.insert (5*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 33850 where
  oeis = f (S.singleton (3*7)) where
     f s = m : f (S.insert (3*m) $ S.insert (7*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 33851 where
  oeis = f (S.singleton (5*7)) where
     f s = m : f (S.insert (5*m) $ S.insert (7*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 33877 where
  oeis = tablList @33877
instance Table 33877 where
  tabl = iterate
     (\row -> scanl1 (+) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 33931 where
  oeisIx (succ->n) = lcm n (lcm (n + 1) (n + 2))

instance OEIS 33949 where
  oeis = filter (\x -> any ((== 1) . (`mod` x) . (^ 2)) [2 .. x - 2]) [1..]

instance OEIS 33999 where
  oeisIx = (1 -) . (* 2) . (`mod` 2)
  oeis = cycle [1,-1]

instance OEIS 34709 where
  oeis = filter (\i -> i `mod` 10 > 0 && i `mod` (i `mod` 10) == 0) [1..]

instance OEIS 34791 where
  oeis = 1 : f [2..] [1] where
     f (x:xs) ys | and $ map (flip isSquMod x) ys = x : f xs (x:ys)
                 | otherwise                      = f xs ys
     isSquMod u v = u `mod` v `elem` (map ((`mod` v) . (^ 2)) [0..v - 1])

instance OEIS 34793 where
  oeis = 1 : f [2..] [1] where
     f (x:xs) ys | and $ map (isSquMod x) ys = x : f xs (x:ys)
                 | otherwise                 = f xs ys
     isSquMod u v = u `mod` v `elem` (map ((`mod` v) . (^ 2)) [0..v - 1])

instance OEIS 34928 where
  oeis = tablList @34928
instance Table 34928 where
  tabf = iterate f [1,1] where
     f us = vs ++ [last vs] where
            vs = zipWith (+) us (0 : scanl (+) 0 us)

instance OEIS 34930 where
  oeis = tablList @34930
instance Table 34930 where
  tabl = iterate
     (\ws -> zipWith (\u v -> mod (u + v) 8) ([0] ++ ws) (ws ++ [0])) [1]

instance OEIS 34931 where
  oeis = tablList @34931
instance Table 34931 where
  tabl = iterate
     (\ws -> zipWith ((flip mod 4 .) . (+)) ([0] ++ ws) (ws ++ [0])) [1]

instance OEIS 34932 where
  oeis = tablList @34932
instance Table 34932 where
  tabl = iterate
     (\ws -> zipWith ((flip mod 16 .) . (+)) ([0] ++ ws) (ws ++ [0])) [1]

instance OEIS 35294 where
  oeis = f 1 where
     f x = (p' 1 (x - 1)) : f (x + 2)
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 2) m

instance OEIS 35317 where
  oeis = tablList @35317
instance Table 35317 where
  tabl = map snd $ iterate f (0, [1]) where
     f (i, row) = (1 - i, zipWith (+) ([0] ++ row) (row ++ [i]))

instance OEIS 35607 where
  oeis = tablList @35607
instance Table 35607 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 2])

instance OEIS 36241 where
  oeis = f [1..] [] S.empty S.empty where
     f (x:xs) ys s2 s3
      | null (s2' `intersect` y2s) && null (s3' `intersect` y3s)
        = x : f xs (x:ys) (S.fromList s2' `S.union` s2) (S.fromList s3' `S.union` s3)
      | otherwise = f xs ys s2 s3
      where s2' = sort $ map (x +) ys
            s3' = sort $ map (x +) y2s
            y2s = S.toList s2
            y3s = S.toList s3

instance OEIS 36355 where
  oeis = tablList @36355
instance Table 36355 where
  tabl = [1] : f [1] [1,1] where
     f us vs = vs : f vs (zipWith (+)
                         (zipWith (+) ([0,0] ++ us) (us ++ [0,0]))
                         (zipWith (+) ([0] ++ vs) (vs ++ [0])))

instance OEIS 36490 where
  oeis = f $ S.fromList [5,7,11] where
     f s = m : (f $ S.insert (5 * m) $ S.insert (7 * m) $ S.insert (11 * m) s')
           where (m, s') = S.deleteFindMin s

instance OEIS 36552 where
  oeis = g [1..] where
     g (x:xs) = x : (2*x) : (g $ delete (2*x) xs)

instance OEIS 36585 where
  oeis = 3 : concat (map f (oeis @36585))
    where f 1 = [1,2,3]; f 2 = [1,3]; f 3 = [2]

instance OEIS 36695 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [0..n], x^2 + y^2 <= n^2]

instance OEIS 36799 where
  oeisIx n = (n - 1) * 2 ^ (n + 1) + 2

instance OEIS 36827 where
  oeisIx n = 2^ (n+1) * (n^3 - 3*n^2 + 9*n - 13) + 26

instance OEIS 36987 where
  oeisIx n = ibp (n+1) where
     ibp 1 = 1
     ibp n = if r > 0 then 0 else ibp n' where (n',r) = divMod n 2
  oeis = 1 : f [0,1] where f (x:y:xs) = y : f (x:xs ++ [x,x+y])

instance OEIS 36989 where
  oeis = 1 : concat (transpose
     [map (+ 1) (oeis @36989), map ((max 1) . pred) $ tail (oeis @36989)])

instance OEIS 37013 where
  oeis = 0 : filter
     (all (< 0) . (\x -> zipWith (-) (tail $ rls x) $ rls x)) [1..] where
         rls = map length . group . unfoldr
               (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 37014 where
  oeis = 0 : filter
     (all (<= 0) . (\x -> zipWith (-) (tail $ rls x) $ rls x)) [1..] where
         rls = map length . group . unfoldr
               (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 37016 where
  oeis = 0 : filter
     (all (>= 0) . (\x -> zipWith (-) (tail $ rls x) $ rls x)) [1..] where
         rls = map length . group . unfoldr
               (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 37027 where
  oeis = tablList @37027
instance Table 37027 where
  tabl = [1] : [1,1] : f [1] [1,1] where
     f xs ys = ys' : f ys ys' where
       ys' = zipWith3 (\u v w -> u + v + w) (ys ++ [0]) (xs ++ [0,0]) ([0] ++ ys)

instance OEIS 37124 where
  oeis = f [1..9] where f (x:xs) = x : f (xs ++ [10*x])

instance OEIS 37161 where
  oeis = 0 : map numerator
    (concat $ concat $ transpose [map (map negate) qss, map reverse qss])
    where qss = map q [1..]
          q x = map (uncurry (%)) $ filter ((== 1) . uncurry gcd) $
                    zip (reverse zs) zs where zs = [1..x]

instance OEIS 37162 where
  oeis = 1 : map denominator
    (concat $ concat $ transpose [map (map negate) qss, map reverse qss])
    where qss = map q [1..]
          q x = map (uncurry (%)) $ filter ((== 1) . uncurry gcd) $
                    zip (reverse zs) zs where zs = [1..x]

instance OEIS 37372 where
  oeis = oeis37372_37407 2 3

instance OEIS 37373 where
  oeis = oeis37372_37407 2 4

instance OEIS 37374 where
  oeis = oeis37372_37407 2 5

instance OEIS 37375 where
  oeis = oeis37372_37407 2 6

instance OEIS 37376 where
  oeis = oeis37372_37407 2 7

instance OEIS 37377 where
  oeis = oeis37372_37407 2 8

instance OEIS 37378 where
  oeis = oeis37372_37407 2 9

instance OEIS 37379 where
  oeis = oeis37372_37407 2 10

instance OEIS 37380 where
  oeis = oeis37372_37407 3 4

instance OEIS 37381 where
  oeis = oeis37372_37407 3 5

instance OEIS 37382 where
  oeis = oeis37372_37407 3 6

instance OEIS 37383 where
  oeis = oeis37372_37407 3 7

instance OEIS 37384 where
  oeis = oeis37372_37407 3 8

instance OEIS 37385 where
  oeis = oeis37372_37407 3 9

instance OEIS 37386 where
  oeis = oeis37372_37407 3 10

instance OEIS 37387 where
  oeis = oeis37372_37407 4 5

instance OEIS 37388 where
  oeis = oeis37372_37407 4 6

instance OEIS 37389 where
  oeis = oeis37372_37407 4 7

instance OEIS 37390 where
  oeis = oeis37372_37407 4 8

instance OEIS 37391 where
  oeis = oeis37372_37407 4 9

instance OEIS 37392 where
  oeis = oeis37372_37407 4 10

instance OEIS 37393 where
  oeis = oeis37372_37407 5 6

instance OEIS 37394 where
  oeis = oeis37372_37407 5 7

instance OEIS 37395 where
  oeis = oeis37372_37407 5 8

instance OEIS 37396 where
  oeis = oeis37372_37407 5 9

instance OEIS 37397 where
  oeis = oeis37372_37407 5 10

instance OEIS 37398 where
  oeis = oeis37372_37407 6 7

instance OEIS 37399 where
  oeis = oeis37372_37407 6 8

instance OEIS 37400 where
  oeis = oeis37372_37407 6 9

instance OEIS 37401 where
  oeis = oeis37372_37407 6 10

instance OEIS 37402 where
  oeis = oeis37372_37407 7 8

instance OEIS 37403 where
  oeis = oeis37372_37407 7 9

instance OEIS 37404 where
  oeis = oeis37372_37407 7 10

instance OEIS 37405 where
  oeis = oeis37372_37407 8 9

instance OEIS 37406 where
  oeis = oeis37372_37407 8 10

instance OEIS 37407 where
  oeis = oeis37372_37407 9 10

instance OEIS 37904 where
  oeisIx = f 9 0 . succ where
     f u v 0 = v - u
     f u v z = f (min u d) (max v d) z' where (z', d) = divMod z 10

instance OEIS 38135 where
  oeis = 0 : 1 : 1 : 1 : f 1 1 1 4 where
     f u v w x = y : f v w y (x + 1) where
       y = q (x - u) + q (x - v) + q (x - w)
       q z = if abs z >= x then 0 else (oeisIx @38135) $ abs z

instance OEIS 38219 where
  oeis = 0 : f [0] where
     f us = a' : f (us ++ [a']) where
          a' = b $ reverse $ map (`splitAt` us) [0..length us - 1] where
             b ((xs,ys):xyss) | vs `isSuffixOf` xs = 1 - head ys
                              | otherwise          = b xyss
          vs = fromJust $ find (`isInfixOf` init us) $ tails us

instance OEIS 38220 where
  oeis = tablList @38220
instance Table 38220 where
  tabl = iterate (\row ->
     zipWith (+) (map (* 3) (row ++ [0])) (map (* 2) ([0] ++ row))) [1]

instance OEIS 38365 where
  oeis = map fi $ filter (\(x :: Int) -> null (show (2*x) `intersect` show x)) [1..]

instance OEIS 38502 where
  oeisIx (succ->n) = if m > 0 then n else (oeisIx @38502 . pred) n'  where (n', m) = divMod n 3

instance OEIS 38547 where
  oeisIx (succ->n) = fromJust $ find ((== n) . fi . A.sigma 0 . fi) [1,3..]

instance OEIS 38608 where
  oeisIx n = n * (-1) ^ n
  oeis = [0, -1] ++ map negate
     (zipWith (+) (oeis @38608) (map (* 2) $ tail (oeis @38608)))

instance OEIS 38622 where
  oeis = tablList @38622
instance Table 38622 where
  tabl = iterate (\row -> map sum $
     transpose [tail row ++ [0,0], row ++ [0], [head row] ++ row]) [1]

instance OEIS 38712 where
  oeisIx (succ-> (fi->n)) = fi (n `xor` (n - 1) :: Integer)

instance OEIS 38719 where
  oeis = tablList @38719
instance Table 38719 where
  tabl = iterate f [1] where
     f row = zipWith (+) (zipWith (*) [0..] $ [0] ++ row)
                         (zipWith (*) [2..] $ row ++ [0])

instance OEIS 38770 where
  oeis = filter f [1..] where
     f u = g u where
       g v = v > 0 && (((d == 0 || r > 0) && g v') || r == 0)
             where (v',d) = divMod v 10; r = mod u d

instance OEIS 39672 where
  oeis = sieve 1 [1..] where
     sieve k xs = z : sieve (k + 1) (fLucky xs) where
        z = xs !! (k - 1 )
        fLucky ws = us ++ fLucky vs where
               (us, _:vs) = splitAt (z + xs !! k - 1) ws

instance OEIS 39691 where
  oeis = filter (f 0) [0..] where
     f d x = d' + d < 10 && (x < 10 || f d' x') where (x', d') = divMod x 10

instance OEIS 39834 where
  oeis = 1 : 1 : zipWith (-) (oeis @39834) (tail (oeis @39834))

instance OEIS 39941 where
  oeis = 0 : 1 : zipWith3 ($)
     (cycle [ (+), (*)]) (oeis @39941) (tail (oeis @39941))

instance OEIS 39943 where
  oeis = [0,1,4,16,20,37,42,58,89,145]

instance OEIS 39966 where
  oeisIx n = fi $ fromEnum (n < 2 || m < 2 && (oeisIx @39966) n' == 1)
     where (n',m) = divMod n 3

instance OEIS 40000 where
  oeisIx 0 = 1
  oeisIx n = 2
  oeis = 1 : repeat 2

instance OEIS 40001 where
  oeisIx 0 = 1
  oeisIx n = 2 - mod n 2
  oeis = 1 : cycle [1, 2]

instance OEIS 42963 where
  oeis = [x | x <- [0..], mod x 4 `elem` [1,2]]

instance OEIS 42968 where
  oeisIx = (`div` 3) . (subtract 1) . (* 4) . succ
  oeis = filter ((/= 0) . (`mod` 4)) [1..]

instance OEIS 43537 where
  oeisIx = genericLength . nub . show . fi . succ

instance OEIS 44102 where
  oeisIx = (* 36)
  oeis = [0, 36 ..]

instance OEIS 45661 where
  oeisIx (succ->n) = product [n'+d | d <- [1..n], let (n',m) = divMod n d, m == 0]

instance OEIS 45800 where
  oeis = findIndices (`elem` [1,7,43,49]) $ cycle [0..99]

instance OEIS 45801 where
  oeis = findIndices (`elem` [11,39,73,77]) $ cycle [0..99]

instance OEIS 45802 where
  oeis = findIndices (`elem` [3,21,29,47]) $ cycle [0..99]

instance OEIS 45803 where
  oeis = findIndices (`elem` [17,19,31,33]) $ cycle [0..99]

instance OEIS 45804 where
  oeis = findIndices (`elem` [9,41,63,87]) $ cycle [0..99]

instance OEIS 45805 where
  oeis = findIndices (`elem` [51,57,93,99]) $ cycle [0..99]

instance OEIS 45806 where
  oeis = findIndices (`elem` [23,27,61,89]) $ cycle [0..99]

instance OEIS 45807 where
  oeis = findIndices (`elem` [53,71,79,97]) $ cycle [0..99]

instance OEIS 45808 where
  oeis = findIndices (`elem` [67,69,81,83]) $ cycle [0..99]

instance OEIS 45809 where
  oeis = findIndices (`elem` [13,37,59,91]) $ cycle [0..99]

instance OEIS 45887 where
  oeisIx n = genericLength $ filter (`isInfixOf` (show $ fi n)) $ map (show . fi) [0,2..n - 1]

instance OEIS 45888 where
  oeisIx (succ->n) = genericLength $ filter (`isInfixOf` (show $ fi n)) $ map (show . fi) [1,3..n - 1]

instance OEIS 45896 where
  oeisIx n = denominator $ n % ((n + 1) * (n + 2))

instance OEIS 45926 where
  oeis = filter (all (`elem` "2468") . show . fi) [2, 4..]

instance OEIS 45928 where
  oeisIx (succ->n) = 3 * n - 2 * floor (1 + sqrt (fi n - 1))

instance OEIS 45954 where
  oeis =  2 : sieve 2 [2,4..] where
     sieve k xs = z : sieve (k + 1) (lucky xs) where
        z = xs !! (k - 1 )
        lucky ws = us ++ lucky vs where
              (us, _:vs) = splitAt (z - 1) ws

instance OEIS 46034 where
  oeis = filter (all (`elem` "2357") . show . fi ) [0..]

instance OEIS 46075 where
  oeis = f $ S.fromList
                 [100 * a + 10 * b + a | a <- [1..9], b <- [0..9], b /= a]
     where f s = m : f (S.insert (10 * m + div (mod m 100) 10) s')
                 where (m, s') = S.deleteFindMin s

instance OEIS 46109 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 == n^2]

instance OEIS 46660 where
  oeisIx (succ->n) = fi (sum es) - length es
    where
      es = snd $ unzip $ factorise (fi n)

instance OEIS 46699 where
  oeis = 1 : 1 : zipWith (+) zs (tail zs) where
     zs = map (oeisIx @46699 . pred) $ zipWith (-) [2..] (oeis @46699)

instance OEIS 46741 where
  oeis = tablList @46741
instance Table 46741 where
  tabl = [[1], [1, 1], [1, 4, 2]] ++ f [1] [1, 1] [1, 4, 2] where
     f us vs ws = ys : f vs ws ys where
       ys = zipWith (+) (zipWith (+) (ws ++ [0]) ([0] ++ map (* 2) ws))
                        (zipWith (-) ([0] ++ vs ++ [0]) ([0, 0, 0] ++ us))

instance OEIS 46854 where
  oeis = tablList @46854
instance Table 46854 where
  tabl = [1] : f [1] [1,1] where
     f us vs = vs : f vs  (zipWith (+) (us ++ [0,0]) ([0] ++ vs))

instance OEIS 46901 where
  oeis = scanl1 (\u v -> if u > v then u - v else u + v) [1..]

instance OEIS 46902 where
  oeis = tablList @46902
instance Table 46902 where
  tabl = [0] : iterate
                 (\row -> zipWith (+) ([0] ++ row) (row ++ [6])) [1,6]

instance OEIS 46934 where
  oeis = tablList @46934
instance Table 46934 where
  tabl = [1] : iterate (\row -> scanl (+) (last row) row) [0,1]

instance OEIS 46936 where
  oeis = tablList @46936
instance Table 46936 where
  tabl = [0] : iterate (\row -> scanl (+) (last row) row) [1,1]

instance OEIS 46937 where
  oeis = tablList @46937
instance Table 46937 where
  tabl = [1] : iterate (\row -> scanl (+) (last row) row) [2,3]

instance OEIS 47201 where
  oeis = [x | x <- [1..], mod x 5 > 0]

instance OEIS 47209 where
  oeisIx = (flip div 2) . (subtract 2) . (* 5) . succ
  oeis = 1 : 4 : (map (+ 5) (oeis @47209))

instance OEIS 47211 where
  oeis = filter ((`elem` [2,4]) . (`mod` 5)) [1..]

instance OEIS 47221 where
  oeisIx (succ->n) = 5 * ((n - 1) `div` 2) + 3 - n `mod` 2
  oeis = 2 : 3 : map (+ 5) (oeis @47221)

instance OEIS 47228 where
  oeis = 2 : 3 : 4 : map (+ 6) (oeis @47228)

instance OEIS 47229 where
  oeis = filter ((`notElem` [1,5]) . (`mod` 6)) [0..]

instance OEIS 47241 where
  oeis = 1 : 3 : map (+ 6) (oeis @47241)

instance OEIS 47246 where
  oeis = [0..3] ++ map (+ 6) (oeis @47246)

instance OEIS 47253 where
  -- oeisIx n = n + n `div` 5
  oeis = [1..5] ++ map (+ 6) (oeis @47253)

instance OEIS 47255 where
  oeis = 1 : 2 : 3 : 5 : map (+ 6) (oeis @47255)

instance OEIS 47261 where
  oeis = 2 : 4 : 5 : map (+ 6) (oeis @47261)

instance OEIS 47273 where
  oeis = 0 : 1 : 3 : 5 : map (+ 6) (oeis @47273)

instance OEIS 47329 where
  oeis = [1, 3, 5, 6] ++ map (+ 7) (oeis @47329)

instance OEIS 47336 where
  oeis = 1 : 6 : map (+ 7) (oeis @47336)

instance OEIS 47471 where
  oeis = [n | n <- [1..], mod n 8 `elem` [1,3]]

instance OEIS 47476 where
  oeis = [n | n <- [0..], mod n 8 <= 3]

instance OEIS 47522 where
  oeis = 1 : 7 : map (+ 8) (oeis @47522)

instance OEIS 47566 where
  oeis = [n | n <- [1..], mod n 8 > 3]

instance OEIS 47621 where
  oeis = 3 : 5 : map (+ 8) (oeis @47621)

instance OEIS 47726 where
  oeisIx n = genericLength $ nub $ permutations $ show $ fi $ succ n

instance OEIS 47778 where
  oeisIx = (foldl (\v d -> 2*v + d) 0) . concatMap (reverse . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)) .
     enumFromTo 1 . succ

instance OEIS 47800 where
  oeisIx n = genericLength $ nub [i^2 + j^2 | i <- [0..n], j <- [i..n]]

instance OEIS 47836 where
  oeis = f [2] where
     f (x:xs) = x : f (xs `O.union` map (x *) [2..x])

instance OEIS 47904 where
  oeis = 1 : zipWith uncurry (cycle [ (+), (*)]) (zip (oeis @47904) [1..])

instance OEIS 47905 where
  oeis = 1 : zipWith uncurry (cycle [ (*), (+)]) (zip (oeis @47905) [1..])

instance OEIS 47906 where
  oeis = 1 : zipWith uncurry (cycle [ (-), (*)]) (zip (oeis @47906) [1..])

instance OEIS 47907 where
  oeis = 1 : zipWith uncurry (cycle [ (*), (-)]) (zip (oeis @47907) [1..])

instance OEIS 47908 where
  oeis = 1 : zipWith ($) (zipWith ($) (cycle [ (+), (*), (-)]) (oeis @47908)) [1..]

instance OEIS 47920 where
  oeis = tablList @47920
instance Table 47920 where
  tabl = map fst $ iterate e ([1], 1) where
     e (row, n) = (scanl (-) (n * head row) row, n + 1)

instance OEIS 47999 where
  oeis = tablList @47999
instance Table 47999 where
  tabl = fmap (fmap fi) $ iterate (\row -> zipWith fixor ([0] ++ row) (row ++ [0])) [1]
    where fixor a b = xor a b :: Int

instance OEIS 48105 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n], mod n d == 0, gcd d (n `div` d) > 1]

instance OEIS 48321 where
  oeis = filter f [0..] where
     f x = all (< 0) $ zipWith (-) (tail zs) zs
           where zs =  map length $ group $ show $ fi x

instance OEIS 48654 where
  oeis = 1 : 4 : zipWith (+) (oeis @48654) (map (* 2) $ tail (oeis @48654))

instance OEIS 48696 where
  oeis = 1 : 9 : zipWith (+)
                 (oeis @48696) (map (2 *) $ tail (oeis @48696))

instance OEIS 48697 where
  oeis = 1 : 10 : zipWith (+) (oeis @48697) (map (* 2) $ tail (oeis @48697))

instance OEIS 48700 where
  oeis = f 1 $ S.singleton 1 where
     f z s = m : f (z+1) (S.insert (c 0) (S.insert (c 1) s')) where
       c d = foldl (\v d -> 2 * v + d) 0 $ (reverse b) ++ [d] ++ b
       b = unfoldr
           (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) z
       (m,s') = S.deleteFindMin s

instance OEIS 48724 where
  oeisIx (fi->n) = fi do n `xor` shiftL n 1 :: Integer

instance OEIS 48736 where
  oeis = 1 : 1 : 1 : 1 :
     zipWith div
       (zipWith (+)
         (zipWith (*) (drop 3 (oeis @48736))
                      (drop 1 (oeis @48736)))
         (drop 2 (oeis @48736)))
       (oeis @48736)

instance OEIS 48793 where
  oeis = tablList @48793
instance Table 48793 where
  tabf = [0] : [1] : f [[1]] where
     f xss = yss ++ f (xss ++ yss) where
       yss = [y] : map (++ [y]) xss
       y = last (last xss) + 1

instance OEIS 48859 where
  oeis = f 2 [1..] where
     f k xs = us ++ f (k + 1) (drop (k + 1) vs)
              where (us, vs) = splitAt k xs

instance OEIS 48877 where
  oeis = 1 : 8 : zipWith (+) (oeis @48877) (map (* 4) $ tail (oeis @48877))

instance OEIS 48879 where
  oeis = 1 : 10 : zipWith (+) (oeis @48879) (map (* 4) $ tail (oeis @48879))

instance OEIS 48881 where
  oeis = c [0] where c (x:xs) = x : c (xs ++ [x,x+1])

instance OEIS 48896 where
  oeis = f [1] where f (x:xs) = x : f (xs ++ [x,2*x])

instance OEIS 48951 where
  oeis = 2 : 4 : ulam 2 4 (oeis @48951)

instance OEIS 48967 where
  oeis = 0 : xs where
     xs = 0 : concat (transpose [zipWith (+) [1..] xs, map (* 2) xs])

instance OEIS 48991 where
  oeis = f [1..] [] where
     f (x:xs) ys | xs' `isInfixOf` ys = f xs ys
                 | otherwise          = x : f xs (xs' ++ ys)
                 where xs' = reverse $ show $ fi x

instance OEIS 48992 where
  oeis = g [1..] [] where
     g (x:xs) ys | xs' `isInfixOf` ys = x : g xs ys
                 | otherwise          = g xs (xs' ++ ys)
                 where xs' = reverse $ show $ fi x

instance OEIS 48993 where
  oeis = tablList @48993
instance Table 48993 where
  tabl = iterate (\row ->
     [0] ++ (zipWith (+) row $ zipWith (*) [1..] $ tail row) ++ [1]) [1]

instance OEIS 48994 where
  oeis = tablList @48994
instance Table 48994 where
  tabl = map fst $ iterate (\ (row, i) ->
    (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 0)

instance OEIS 49063 where
  oeis = tablList @49063
instance Table 49063 where
  tabf = [1] : iterate f [1, 1] where
     f row = 1 : 2 : zipWith (+) ( map (* 2) row) ((tail row) ++ [0])

instance OEIS 49072 where
  oeis = 1 : 3 :
      zipWith (-) (map (* 3) $ tail (oeis @49072)) (map (* 4) (oeis @49072))

instance OEIS 49320 where
  oeis = 0 : 0 : 1 : 0 : f [0,0,1,0] where
     f xs = drop (length xs) ys ++ f ys where
       ys = concatMap ch xs
       ch 0 = [0,0,1,0]; ch 1 = [1]

instance OEIS 49321 where
  oeis = 0 : 0 : 1 : 2 : f [0,0,1,2] where
     f xs = drop (length xs) ys ++ f ys where
       ys = concatMap ch xs
       ch 0 = [0,0,1,2]; ch 1 = [1,2]; ch 2 = [0,1,2]

instance OEIS 49363 where
  oeisIx (succ->n) = foldl (\v d -> n * v + d) 0 (1 : 0 : [2..n - 1])

instance OEIS 49439 where
  oeis = filter (\x -> ((length $ oddDivs x) `elem` oddDivs x)) [1..]
     where oddDivs n = [d | d <- [1,3..n], mod n d == 0]

instance OEIS 49444 where
  oeis = tablList @49444
instance Table 49444 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 2)

instance OEIS 49451 where
  oeisIx n = n * (3 * n + 1)

instance OEIS 49458 where
  oeis = tablList @49458
instance Table 49458 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 3)

instance OEIS 49459 where
  oeis = tablList @49459
instance Table 49459 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 4)

instance OEIS 49460 where
  oeis = tablList @49460
instance Table 49460 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 5)

instance OEIS 49472 where
  oeisIx = floor . (/ sqrt 2) . fi

instance OEIS 49474 where
  oeisIx = ceiling . (/ sqrt 2) . fi

instance OEIS 50407 where
  oeisIx n = n * (n ^ 2 - 6 * n + 11) `div` 6

instance OEIS 50512 where
  oeis = 0 : 1 : 1 : 1 : (-1) : zipWith div (zipWith (-) (zipWith (*)
     (drop 4 (oeis @50512)) (drop 2 (oeis @50512)))
       (map (^ 2) (drop 3 (oeis @50512)))) (tail (oeis @50512))

instance OEIS 50600 where
  oeis = tablList @50600
instance Table 50600 where
  rowCol (fi->n) (fi->k) = fi $ adder 0 (n - k) k where
     adder :: Int -> Int -> Int -> Int
     adder c u 0 = c
     adder c u v = adder (c + 1) (u `xor` v) (shiftL (u .&. v) 1)
  rowT n = map (rowCol @50600 n) $ reverse [0..n]
  tabl = map (rowT @50600) [0..]

instance OEIS 50935 where
  oeis = 0 : 0 : 1 : zipWith (-) (drop 2 (oeis @50935)) (oeis @50935)

instance OEIS 51010 where
  oeis = tablList @51010
instance Table 51010 where
  rowCol n k = snd $ until ((== 0) . snd . fst)
                      (\ ((x, y), i) -> ((y, mod x y), i + 1)) ((n, k), 0)
  rowT n = map (rowCol @51010 n) [0..n - 1]
  tabl = map (rowT @51010) [1..]

instance OEIS 51037 where
  oeis = f $ S.singleton 1 where
     f s = y : f (S.insert (5 * y) $ S.insert (3 * y) $ S.insert (2 * y) s')
                 where (y, s') = S.deleteFindMin s

instance OEIS 51120 where
  oeis = 1 : f [1] where
    f xs = seen ++ (f $ xs ++ seen) where
      seen = look (reverse $ map length $ group xs') (reverse $ nub xs')
      xs' = sort xs
      look [] []               = []
      look (cnt:cnts) (nr:nrs) = cnt : nr : look cnts nrs

instance OEIS 51132 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 < n^2]

instance OEIS 51159 where
  oeis = tablList @51159
instance Table 51159 where
  tabl = [1] : f [1] [1,1] where
     f us vs = vs : f vs (zipWith (+) ([0,0] ++ us) (us ++ [0,0]))

instance OEIS 51176 where
  oeisIx n = if m == 0 then n' else n  where (n',m) = divMod n 3

instance OEIS 51190 where
  oeisIx (succ->n) = product $ map (gcd n) [1..n - 1]

instance OEIS 51338 where
  oeis = tablList @51338
instance Table 51338 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 6)

instance OEIS 51339 where
  oeis = tablList @51339
instance Table 51339 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 7)

instance OEIS 51379 where
  oeis = tablList @51379
instance Table 51379 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 8)

instance OEIS 51380 where
  oeis = tablList @51380
instance Table 51380 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 9)

instance OEIS 51426 where
  oeisIx (succ->n) = foldl lcm 1 [2,4..2*n]

instance OEIS 51436 where
  oeisIx n = (3 ^ n + 3 ^ m - 2 ^ n + (1 - r) * 2 ^ m) `div` 2 + r
              where (m,r) = divMod n 2

instance OEIS 51523 where
  oeis = tablList @51523
instance Table 51523 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 10)

instance OEIS 51597 where
  oeis = tablList @51597
instance Table 51597 where
  tabl = iterate (\row -> zipWith (+) ([1] ++ row) (row ++ [1])) [1]

instance OEIS 51601 where
  oeis = tablList @51601
instance Table 51601 where
  tabl = iterate (\row -> zipWith (+) ([1] ++ row) (row ++ [1])) [0]

instance OEIS 51631 where
  oeis = tablList @51631
instance Table 51631 where
  tabl = iterate (\row -> zipWith (+) ([1] ++ row) (row ++[1])) [-1]

instance OEIS 51632 where
  oeis = tablList @51632
instance Table 51632 where
  tabl = iterate (\rs -> zipWith (+) ([1] ++ rs) (rs ++ [1])) [-2]

instance OEIS 51666 where
  oeis = tablList @51666
instance Table 51666 where
  tabl = map fst $ iterate
     (\ (vs, w:ws) -> (zipWith (+) ([w] ++ vs) (vs ++ [w]), ws))
     ([0], [1, 3 ..])

instance OEIS 51736 where
  oeis = 1 : 5 : 17 : 63 : zipWith (-) (map (* 2) $ drop 2 $
     zipWith (+) (map (* 3) (oeis @51736)) (tail (oeis @51736))) (oeis @51736)

instance OEIS 51801 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @51801 n') * (m + 0 ^ m) where (n',m) = divMod n 10

instance OEIS 51840 where
  oeis = map floor vs where
     vs = iterate (\x -> x * (4 - 3 * x)) 0.1

instance OEIS 51885 where
  oeisIx n = (m + 1) * 10^n' - 1 where (n',m) = divMod n 9

instance OEIS 51933 where
  oeis = tablList @51933
instance Table 51933 where
  rowCol n k = fi do fi n `xor` fi k :: Int
  rowT n = map (rowCol @51933 n) [0..n]
  tabl = map (rowT @51933) [0..]

instance OEIS 52287 where
  oeis = f [3] where
     f (x:xs) = x : f (xs `O.union` map (x *) [2..x])

instance OEIS 52382 where
  oeis = iterate f 1 where
    f x = 1 + if r < 9 then x else 10 * f x' where (x', r) = divMod x 10

instance OEIS 52413 where
  oeisIx = f where
    f 0 = 0
    f v = 10 * f w + if r > 4 then r + 1 else r where (w, r) = divMod v 9

instance OEIS 52499 where
  oeis = f $ S.singleton 1 where
     f s = m : f (S.insert (2*m) $ S.insert (4*m- 1) s') where
        (m, s') = S.deleteFindMin s

instance OEIS 52509 where
  oeis = tablList @52509
instance Table 52509 where
  tabl = [1] : [1,1] : f [1] [1,1] where
     f row' row = rs : f row rs where
       rs = zipWith (+) ([0] ++ row' ++ [1]) (row ++ [0])

instance OEIS 52530 where
  oeis =
     0 : 2 : zipWith (-) (map (* 4) $ tail (oeis @52530)) (oeis @52530)

instance OEIS 52542 where
  oeis = 1 : 2 : 4 : tail (zipWith (+)
                 (map (* 2) $ tail (oeis @52542)) (oeis @52542))

instance OEIS 52901 where
  oeis = cycle [3,2,2]

instance OEIS 52928 where
  oeisIx = (* 2) . flip div 2
  oeis = 0 : 0 : map (+ 2) (oeis @52928)

instance OEIS 52938 where
  oeis = 1 : 3 : 2 : zipWith (-) [5..] (oeis @52938)

instance OEIS 52955 where
  oeis = 1 : 2 : map ((+ 1) . (* 2)) (oeis @52955)

instance OEIS 53087 where
  oeisIx (succ->n) = head [j | j <- [0..], (2 ^ j) `kara` n /= Nothing] where
     kara a b = if null ks then Nothing else Just $ head ks
                where ks = [c | c <- [1..a], a <= c * b, a > c * (b - 1)]

instance OEIS 53092 where
  oeisIx (succ->n) = f 1 where
     f x = case x `kara` n of
                Nothing -> f $ 2 * x
                Just y  -> y
     kara a b = if null ks then Nothing else Just $ head ks
                where ks = [c | c <- [1..a], a <= c * b, a > c * (b - 1)]

instance OEIS 53121 where
  oeis = tablList @53121
instance Table 53121 where
  tabl = iterate
     (\row -> zipWith (+) ([0] ++ row) (tail row ++ [0,0])) [1]

instance OEIS 53187 where
  oeis = 0 : concatMap (\x -> genericReplicate (2*x) (x ^ 2)) [1..]

instance OEIS 53432 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [8, 5, 4, 9, 1, 7, 6, 3, 2, 0]

instance OEIS 53433 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [8, 5, 4, 9, 1, 7, 6, 3, 2, 0]

instance OEIS 53621 where
  oeisIx = round . (\x -> x / (log x - 1)) . fi . succ

instance OEIS 53644 where
  oeisIx n = if n <= 1 then n else 2 * (oeisIx @53644) (div n 2)
  oeis = 0 : concat (iterate (\zs -> map (* 2) (zs ++ zs)) [1])

instance OEIS 53670 where
  oeisIx (succ->n) = head [x | x <- [3, 5 ..],
                        n `gcd` x == 1, (n + 1) `gcd` x == 1]

instance OEIS 53672 where
  oeisIx (succ->n) = 2 + fromJust
     (elemIndex 1 $ map (gcd $ foldl1 lcm $ take 4 [n..]) [2..])

instance OEIS 53673 where
  oeisIx (succ->n) = 2 + fromJust
     (elemIndex 1 $ map (gcd $ foldl1 lcm $ take 5 [n..]) [2..])

instance OEIS 53674 where
  oeisIx (succ->n) = 2 + fromJust
     (elemIndex 1 $ map (gcd $ foldl1 lcm $ take 6 [n..]) [2..])

instance OEIS 53737 where
  oeisIx n = if n == 0 then 0 else (oeisIx @53737) m + r where (m, r) = divMod n 4

instance OEIS 53755 where
  oeisIx = (+ 1) . (* 4) . (^ 2)

instance OEIS 53816 where
  oeis = 1 : filter f [4..]
    where
     f x = length us - length vs <= 1
        && read (reverse us) + read (reverse vs) == fi x
       where
         (us, vs) = splitAt (length $ show $ fi x) (reverse $ show $ fi (x^2))

instance OEIS 53824 where
  oeisIx 0 = 0
  oeisIx x = (oeisIx @53824) x' + d  where (x', d) = divMod x 5

instance OEIS 53829 where
  oeisIx n = q 0 $ divMod n 8 where
     q r (0, d) = r + d
     q r (m, d) = q (r + d) $ divMod m 8

instance OEIS 53832 where
  oeisIx n = q 0 $ divMod n 12 where
     q r (0, d) = r + d
     q r (m, d) = q (r + d) $ divMod m 12

instance OEIS 53836 where
  oeisIx n = q 0 $ divMod n 16 where
     q r (0, d) = r + d
     q r (m, d) = q (r + d) $ divMod m 16

instance OEIS 53871 where
  oeis = 1 : 0 : zipWith (*)
     [2,4..] (zipWith (+) (oeis @53871) $ tail (oeis @53871))

instance OEIS 54028 where
  oeisIx (succ.succ->n) = head [k | k <- [2..], 2^k >= k^n]

instance OEIS 54054 where
  oeisIx = f 9 where
     f m x | x <= 9 = min m x
           | otherwise = f (min m d) x' where (x',d) = divMod x 10

instance OEIS 54055 where
  oeisIx = f 0 where
     f m x | x <= 9 = max m x
           | otherwise = f (max m d) x' where (x',d) = divMod x 10

instance OEIS 54123 where
  oeis = tablList @54123
instance Table 54123 where
  tabl = [1] : [1, 1] : f [1] [1, 1] where
     f us vs = ws : f vs ws where
               ws = zipWith (+) (0 : init us ++ [0, 0]) (vs ++ [1])

instance OEIS 54385 where
  oeis = map (floor . (* e') . fi) [1..]
     where e' = e / (e - 1); e = exp 1

instance OEIS 54977 where
  oeisIx 0 = 2
  oeisIx n = 1
  oeis = 2 : repeat 1

instance OEIS 54986 where
  oeis = map fi $ filter modest [1..] where
     modest x = or $ zipWith m
                (map read $ (init $ tail $ inits $ show x) :: [Integer])
                (map read $ (tail $ init $ tails $ show x) :: [Integer])
        where m u v = u < v && (x - u) `mod` v == 0

instance OEIS 55045 where
  oeis = filter ((== 5) . (flip mod 8) . f) [1..] where
     f x = if r == 0 then f x' else x  where (x', r) = divMod x 4

instance OEIS 55048 where
  oeis = filter (s 0) [1..] where
     s t u | m > 0  = even t && m == 2
           | m == 0 = s (t + 1) u' where (u',m) = divMod u 3

instance OEIS 55067 where
  oeisIx (succ->n) = product [k | k <- [1..n], mod n k /= 0]

instance OEIS 55098 where
  oeisIx (succ->n) = genericLength $ nub $ filter ((> '0') . head) $ permutations $ show $ fi n

instance OEIS 55640 where
  oeisIx n = genericLength $ filter (/= '0') $ show $ fi n

instance OEIS 55641 where
  oeisIx n | n < 10    = 0 ^ n
           | otherwise = (oeisIx @55641) n' + 0 ^ d where (n',d) = divMod n 10

instance OEIS 55642 where
  oeisIx n = length $ show (fi n)

instance OEIS 55790 where
  oeis = 0 : 2 : zipWith (+)
     (zipWith (*) [0..] (oeis @55790)) (zipWith (*) [2..] $ tail (oeis @55790))

instance OEIS 55874 where
  oeisIx (succ->n) = genericLength $ takeWhile ((== 0) . (mod n)) [1..]

instance OEIS 55897 where
  oeisIx (succ->n) = n * (n - 1) ^ (n - 1)

instance OEIS 55980 where
  -- oeisIx = floor . sum . map (1 %) . enumFromTo 1
  oeis = map floor $ scanl1 (+) $ map (1 %) [1..]

instance OEIS 56020 where
  oeis = 1 : 8 : map (+ 9) (oeis @56020)

instance OEIS 56106 where
  oeisIx n = n * (3 * n - 1) + 1

instance OEIS 56542 where
  oeis = 0 : map (+ 1) (zipWith (*) [2..] (oeis @56542))

instance OEIS 56558 where
  oeis = concatMap (concat . init . inits . enumFromTo 0) [0..]

instance OEIS 56753 where
  oeis = [1] ++ odds [] where
     odds xs = xs ++ (intercalate xs' $ group [y+2,y+4..2*y+1]) ++ odds xs'
          where y = 2 * length xs + 1
                xs' = xs ++ [y] ++ xs

instance OEIS 56832 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where
            y = 1 + sum (zipWith (*) xs $ reverse xs) `mod` 2

instance OEIS 56875 where
  oeis =  f [1..] where
     f zs = head zs : f (g zs) where
       g (x:xs) = us ++ g vs where (us, vs) = splitAt (x - 1) xs

instance OEIS 56918 where
  oeis = 2 : 9 : zipWith (-) (map (* 9) $ tail (oeis @56918)) (oeis @56918)

instance OEIS 56942 where
  oeis = concatMap (\x -> (x ^ 2) : (take x $ repeat (x * (x + 1)))) [0..]

instance OEIS 57077 where
  oeisIx d = (-1)^quot d 2
  oeis     = cycle [1, 1, -1, -1]

instance OEIS 57078 where
  oeisIx = (1 -) . (`mod` 3)

instance OEIS 57142 where
  oeisIx (succ->n) = head $ reverse $ sort $ map length $ group $
              sort [u * v | u <- [1..n], v <- [1..n]]

instance OEIS 57143 where
  oeisIx (succ->n) = head $ head $ reverse $ sortBy (compare `on` length) $
              group $ sort [u * v | u <- [1..n], v <- [1..n]]

instance OEIS 57154 where
  oeis = g [1] [2..] [1] where
     g ds (a:as) us
       | null (ds' `intersect` us) = g ds' (as \\ ds') (us `union` ds')
       | otherwise = a : g ds as us
       where ds' = scanl (+) a ds

instance OEIS 57165 where
  oeis = r (S.singleton 0) 1 0 where
     r s n x = if x > n && (x - n) `S.notMember` s
                  then r (S.insert (x-n) s) (n+1) (x-n)
                  else n : r (S.insert (x+n) s) (n+1) (x+n)

instance OEIS 57194 where
  oeis = 1 : f 1 1 where
     f u v = w : f (u * w) (v + w) where w = u * v

instance OEIS 57338 where
  oeisIx (succ->n) = head $ reverse $ sort $ map length $ group $
              sort [u * v * w | u <- [1..n], v <- [1..n], w <- [1..n]]

instance OEIS 57427 where
  oeisIx = signum
  oeis = 0 : [1, 1 ..]

instance OEIS 57436 where
  oeis = filter (null . (intersect "0789") . show . fi) [1..]

instance OEIS 57597 where
  oeis = 0 : 0 : 1 : zipWith3 (\x y z -> - x - y + z)
                 (drop 2 (oeis @57597)) (tail (oeis @57597)) (oeis @57597)

instance OEIS 57655 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 <= n]

instance OEIS 57918 where
  oeisIx (succ->n) = sum $ map ((0 ^) . (`mod` n) . (^ 2)) [1..n - 1]

instance OEIS 57979 where
  oeisIx n = 1 - rest * (1 - n') where (n', rest) = divMod n 2
  oeis = concat $ transpose [repeat 1, [0..]]

instance OEIS 58187 where
  oeis = 1 : f 1 1 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ zipWith (*) [1..x] [x,x - 1..1]

instance OEIS 58207 where
  oeis = f [0,1,2,3,2] where f xs = xs ++ f (map (+ 1) xs)

instance OEIS 58212 where
  oeisIx n = 1 + n * (n - 3) `div` 6

instance OEIS 58257 where
  oeis = tablList @58257
instance Table 58257 where
  tabl = [1] : ox 0 [1] where
     ox turn xs = ys : ox (mod (turn + 1) 4) ys
        where ys | turn <= 1 = scanl (+) 0 xs
                 | otherwise = reverse $ scanl (+) 0 $ reverse xs

instance OEIS 58345 where
  oeis = 1 : f 1 1 where
     f u v = w : f (u * w) (v + w) where w = lcm u v

instance OEIS 58840 where
  oeis = 1 : renyi' 1 where
     renyi' x = y : renyi' r  where
        (r, y) | q > 1     = (q - 1, 1)
               | otherwise = (q, 0)
        q = 3%2 * x

instance OEIS 59100 where
  oeisIx = (+ 2) . (^ 2)
  oeis = scanl (+) (2) [1, 3 ..]

instance OEIS 59268 where
  oeis = tablList @59268
instance Table 59268 where
  tabl = iterate (scanl (+) 1) [1]

instance OEIS 59283 where
  oeis = tablList @59283
instance Table 59283 where
  tabl = [1] : [0,1] : f [1] [0,1] where
     f us vs = ws : f vs ws where
       ws = scanl1 (+) $ zipWith (+)
                         ([0]++us++[0]) $ zipWith (+) ([0]++vs) (vs++[0])

instance OEIS 59317 where
  oeis = tablList @59317
instance Table 59317 where
  tabf = [1] : [1,1,1] : f [1] [1,1,1] where
     f ws vs = vs' : f vs vs' where
       vs' = zipWith4 (\r s t x -> r + s + t + x)
             (vs ++ [0,0]) ([0] ++ vs ++ [0]) ([0,0] ++ vs)
             ([0,0] ++ ws ++ [0,0])

instance OEIS 59576 where
  oeis = tablList @59576
instance Table 59576 where
  tabl = [1] : map fst (iterate f ([1,1], [2,3,2])) where
     f (us, vs) = (vs, map (* 2) ws) where
       ws = zipWith (-) (zipWith (+) ([0] ++ vs) (vs ++ [0]))
                        ([0] ++ us ++ [0])

instance OEIS 59841 where
  oeisIx = (1 -) . (`mod` 2)
  oeis = cycle [1,0]

instance OEIS 59922 where
  oeis = tablList @59922
instance Table 59922 where
  tabl = iterate (\rs ->
     zipWith (+) (0 : reverse (0 : replicate (length rs - 1) 1))
                 $ zipWith (*) ([1] ++ rs) (rs ++ [1])) [1]

instance OEIS 60000 where
  oeis = 1 : 2 : f 1 2 2 [] where
     f x y m []     = z : f y z z [m+1..z - 1] where z = x + y
     f x y m (h:hs) = h : f y h m hs

instance OEIS 60030 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v ws = y : f v y (delete y ws) where
       y = if null xs then u + v else last xs
       xs = takeWhile (< v) ws

instance OEIS 60142 where
  oeis = 0 : f (S.singleton 1) where
     f s = x : f (S.insert (4 * x) $ S.insert (2 * x + 1) s') where
         (x, s') = S.deleteFindMin s

instance OEIS 60692 where
  oeisIx (succ->n) = uncurry (+) $ divMod (3 ^ n) (2 ^ n)

instance OEIS 60747 where
  oeisIx = subtract 1 . (* 2)
  oeis = [-1, 1 ..]

instance OEIS 60945 where
  oeis' (A r) = 1 : 1 : 2 : 3 : 6 : zipWith (+)
    (tail r) (zipWith (+) (drop 3 r) (drop 4 r))

instance OEIS 61021 where
  oeis = 3 : 3 : 3 : zipWith (-)
    (tail $ zipWith (*) (tail (oeis @61021)) (oeis @61021)) (oeis @61021)

instance OEIS 61037 where
  oeisIx (succ.succ->n) = numerator (1%4 - 1%n^2)

instance OEIS 61038 where
  oeisIx (succ.succ->n) = denominator (1%4 - 1%n^2)

instance OEIS 61039 where
  oeisIx (succ.succ->succ->n) = numerator $ 1%9 - 1%n ^ 2

instance OEIS 61040 where
  oeisIx (succ.succ->succ->n) = denominator $ 1%9 - 1%n^2

instance OEIS 61041 where
  oeisIx ((+4)->n) = numerator (1%16 - 1%n^2)

instance OEIS 61042 where
  oeisIx ((+4)->n) = denominator (1%16 - 1%n^2)

instance OEIS 61043 where
  oeisIx = numerator . (1 % 25 -) . recip . (^ 2) . fi . (+5)

instance OEIS 61044 where
  oeisIx = denominator . (1 % 25 -) . recip . (^ 2) . fi . (+5)

instance OEIS 61045 where
  oeisIx = numerator . (1 % 36 -) . recip . (^ 2) . fi . (+1)

instance OEIS 61046 where
  oeisIx = denominator . (1 % 36 -) . recip . (^ 2) . fi . succ

instance OEIS 61083 where
  oeis = 1 : 2 : zipWith divIgnPnt (oeis @61083) (tail (oeis @61083))
     where divIgnPnt x y = ddiv (10 * m) x' where
              ddiv u w | r == 0    = 10 * w + q
                       | otherwise = ddiv (10 * r) (10 * w + q)
                       where (q,r) = divMod u y
              (x',m) = divMod x y

instance OEIS 61084 where
  oeis = 1 : 2 : zipWith (-) (oeis @61084) (tail (oeis @61084))

instance OEIS 61292 where
  oeis = 2 : 2 : 2 : 2 : zipWith (-)
     (zipWith3 (((*) .) . (*)) (drop 2 xs) (tail xs) xs) (oeis @61292)
     where xs = tail (oeis @61292)

instance OEIS 61293 where
  oeisIx = floor . (** exp 1) . fi . succ

instance OEIS 61313 where
  oeisIx (succ->n) = fst $ until ((== 1) . snd) (\ (u, v) -> (u + 1, f v)) (0, n)
     where f n = if r == 0 then n' else n + 1  where (n', r) = divMod n 2

instance OEIS 61426 where
  oeis = g [1] where
     g ds = if product ds == 2 ^ length ds
            then foldr (\d v -> 10 * v + d) 0 ds : g (s ds) else g (s ds)
     s [] = [1]; s (8:ds) = 1 : s ds; s (d:ds) = 2*d : ds

instance OEIS 61427 where
  oeis = g [1] where
     g ds = if product ds == 3 ^ length ds
            then foldr (\d v -> 10 * v + d) 0 ds : g (s ds) else g (s ds)
     s [] = [1]; s (9:ds) = 1 : s ds; s (d:ds) = 3*d : ds

instance OEIS 61428 where
  oeis = g [1] where
     g ds = if product ds == 4 ^ length ds
            then foldr (\d v -> 10 * v + d) 0 ds : g (s ds) else g (s ds)
     s [] = [1]; s (8:ds) = 1 : s ds; s (d:ds) = 2*d : ds

instance OEIS 61429 where
  oeis = filter (h 1 1) [1..] where
     h 0 _ _ = False
     h u v 0 = u == v
     h u v w = h (r * u) (6 * v) w' where (w', r) = divMod w 10

instance OEIS 61430 where
  oeis = filter g [0..] where
     g u = round (fi p ** (1 / fi k)) ^ k == p where
           (p, k) = h (1, 0) u
           h (p, l) 0 = (p, l)
           h (p, l) v = h (p * r, l + 1) v' where (v', r) = divMod v 10

instance OEIS 61493 where
  oeisIx (succ->n) = fi (read $ r 1 [] n :: Integer) where
    r _ roms 0 = roms
    r p roms z = case p of
      1 -> r 2 (d '1' '2' '3' m) z'
      2 -> r 3 (d '3' '4' '5' m ++ roms) z'
      3 -> r 4 (d '5' '6' '7' m ++ roms) z'
      4 -> replicate z '7' ++ roms
      where (z',m) = divMod z 10
    d i j k c =
      [[],[i],[i,i],[i,i,i],[i,j],[j],[j,i],[j,i,i],[j,i,i,i],[i,k]] !! c

instance OEIS 61646 where
  oeis = 1 : 1 : 1 : zipWith (-) (map (* 2)
     (zipWith (+) (drop 2 (oeis @61646)) (tail (oeis @61646)))) (oeis @61646)

instance OEIS 61857 where
  oeis = tablList @61857
instance Table 61857 where
  rowCol n k = length [ ()| i <- [2..n], j <- [1..i- 1], mod (i + j) k == 0]
  rowT n = map (rowCol @61857 n) [1..n]
  tabl = map (rowT @61857) [1..]

instance OEIS 61862 where
  oeis = filter f [0..] where
     f x = g x 0 where
       g 0 v = v == x
       g u v = if d <= 1 then g u' (v + d) else v <= x && h 1
               where h p = p <= x && (g u' (v + p) || h (p * d))
                     (u', d) = divMod u 10

instance OEIS 61917 where
  oeis = filter chi [0..] where
     chi x = zs == reverse zs where
        zs = dropWhile (== '0') $ reverse $ show $ fi x

instance OEIS 62039 where
  oeis = 1 : f 1 0 where
     f x n | x > n     = (x-n) : f (x-n) (n+1)
           | otherwise =    x' : f x' (n+1) where x' = x + (oeisIx @62039) x

instance OEIS 62090 where
  oeis = f [1, 3 ..] [] where
     f (x:xs) ys = g x ys where
       g _ []     = x : f xs (x : ys)
       g 1 _      = f xs ys
       g z (v:vs) = g (z `div` gcd z v) vs

instance OEIS 62097 where
  oeis = 1 : f 1 1 where
     f u v = w : f (u + w) (v * w) where w = u + v

instance OEIS 62323 where
  oeis = tablList @62323
instance Table 62323 where
  tabl = map fst $ iterate f ([1], [0,1]) where
     f (us, vs) = (vs, ws) where
       ws = (zipWith (+) (us ++ [0]) (map (* v) vs)) ++ [1]
            where v = last (init vs) + 1

instance OEIS 62383 where
  oeis = 1 : zs where
     zs = 2 : (map (* 2) $ concat $ transpose [zs, zs])

instance OEIS 62682 where
  oeis = f (S.singleton (1 + 2^3, (1, 2))) 0 0 where
     f s z z' = if y == z && z' /= z then y : f s'' y z else f s'' y z
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 62725 where
  oeisIx n = n * (9 * n + 5) `div` 2

instance OEIS 62806 where
  oeisIx (succ->n) = sum $ zipWith (*) [1..n] $ iterate (* n) n

instance OEIS 62813 where
  oeisIx (succ->n) = foldr (\dig val -> val * n + dig) 0 [0 .. n - 1]

instance OEIS 62880 where
  oeis = filter f [0..] where
     f 0 = True
     f x = (m == 0 || m == 2) && f x'  where (x', m) = divMod x 4

instance OEIS 63524 where
  oeisIx = fi . fromEnum . (== 1)

instance OEIS 63656 where
  oeis = f 1 [0..] where
     f k xs = us ++ f (k + 1) (drop (k - 1) vs) where
                      (us, vs) = splitAt k xs

instance OEIS 63657 where
  oeis = f 0 [0..] where
     f k (_:xs) = us ++ f (k + 1) (drop (k + 1) vs) where
                          (us, vs) = splitAt k xs

instance OEIS 63660 where
  oeisIx n = head [m | m <- [n + 1 ..],
                        not $ null $ show (fi m) `intersect` show (fi n)]

instance OEIS 63694 where
  oeisIx 0 = 0
  oeisIx n = 4 * (oeisIx @63694) n' + mod q 2
              where (n', q) = divMod n 4

instance OEIS 63695 where
  oeisIx 0 = 0
  oeisIx n = 4 * (oeisIx @63695) n' + 2 * div q 2
              where (n', q) = divMod n 4

instance OEIS 63720 where
  oeis = [6,2,5,5,4,5,5,3,7,5] ++ f 10 where
     f x = (oeisIx @63720 x' + (oeisIx @63720) d) : f (x + 1)
           where (x',d) = divMod x 10

instance OEIS 63882 where
  oeis = 1 : 1 : 1 : 1 : zipWith (+)
     (map f $ zipWith (-) [5..] (oeis @63882))
     (map f $ zipWith (-) [5..] $ drop 3 (oeis @63882))
    where
      f = oeisIx @63882 . pred

instance OEIS 63967 where
  oeis = tablList @63967
instance Table 63967 where
  tabl = [1] : [1,1] : f [1] [1,1] where
     f us vs = ws : f vs ws where
       ws = zipWith (+) ([0] ++ us ++ [0]) $
            zipWith (+) (us ++ [0,0]) $ zipWith (+) ([0] ++ vs) (vs ++ [0])

instance OEIS 63985 where
  oeisIx (succ->n) = genericLength [ ()| x <- [1..n], y <- [x..n], gcd x y > 1]

instance OEIS 64235 where
  oeis = 1 : zs where
     zs = 3 : 3 : (map (* 3) $ concat $ transpose [zs, zs, zs])

instance OEIS 64413 where
  oeis = 1 : ekg 2 [2..] where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 64417 where
  oeis = 1 : 2 : 3 : f 3 [4..] where
     f x us = x' : f x' (delete x' us) where
        x' = head [u | u <- us, gcd u x > 2]

instance OEIS 64419 where
  oeis = [1,2,3,4,5] ++ f 5 [] [6..] where
     f z xs (y:ys) | y `gcd` z > 4 = y : f y [] (reverse xs ++ ys)
                   | otherwise     = f z (y:xs) ys

instance OEIS 64455 where
  oeisIx (succ->n) = n + if m == 0 then n' else - n'  where (n',m) = divMod n 2
  oeis = concat $ transpose [[1 ..], [3, 6 ..]]

instance OEIS 64650 where
  oeis = 1 : 2 : zipWith (+) (oeis @64650) (map (flip div 2) $ tail (oeis @64650))

instance OEIS 64651 where
  oeis = 0 : 1 : zipWith (+)
     (oeis @64651) (map (flip div 2 . (+ 1)) $ tail (oeis @64651))

instance OEIS 64657 where
  oeis = map fromJust $ takeWhile (/= Nothing) zs where
    z n = fromJust $ zs !! n
    zs = (map Just [1,1,1,1]) ++ f 4 where
       f x = y : f (x + 1) where
         y | 2*x < max i3 i4 = Nothing
           | otherwise       = Just $ z (abs (x - i3)) + z (abs (x - i4))
           where i3 = z (x - 3); i4 = z (x - 4)

instance OEIS 64680 where
  oeis = zipWith ($) (cycle [ (`div` 2), (* 2)]) [0..]

instance OEIS 64736 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v (w:ws) = u' : w : f u' w (delete u' ws) where u' = v * w

instance OEIS 64801 where
  oeis = f 1 [1..] where
     f k xs = us ++ f (k + 1) (drop (k + 1) vs)
              where (us, vs) = splitAt k xs

instance OEIS 64861 where
  oeis = tablList @64861
instance Table 64861 where
  tabl = map fst $ iterate f ([1], 2) where
    f (xs, z) = (zipWith (+) ([0] ++ map (* z) xs) (xs ++ [0]), 3 - z)

instance OEIS 65033 where
  oeisIx n = 0 ^ n + div (n + 1) 2

instance OEIS 65039 where
  oeisIx n = sum $ map (fi . read) $ tail $ inits $ show $ fi n

instance OEIS 65094 where
  oeis = 1 : f 1 1 1 where
     f k s x = y : f (k + 1) (s + y) y where y = x + div s k

instance OEIS 65109 where
  oeis = tablList @65109
instance Table 65109 where
  tabl = iterate
     (\row -> zipWith (-) (map (* 2) row ++ [0]) ([0] ++ row)) [1]

instance OEIS 65422 where
  oeis = 1 : 1 : f 2 1 where
     f n x = x' : f (n+1) x' where
         x' | x `mod` n == 0 = until ((> 0) . (`mod` n)) (`div` n) x
            | otherwise      = x * n

instance OEIS 65502 where
  oeis = filter ((> 1) . (gcd 10)) [1..]

instance OEIS 65620 where
  oeis = 0 : fix \(map(*2)->zs) ->
    1 : concat (transpose [zs, map ((+ 1) . negate) zs])

instance OEIS 66484 where
  oeis = filter h [1..] where
     h x = notElem '0' xs && length (nub xs) > 1 &&
           all d (map read $ zipWith (++)
                 (tail $ tails xs) (tail $ inits xs)) where xs = show (fi x)
     d u = g u where
           g v = v == 0 || mod u d == 0 && g v' where (v', d) = divMod v 10

instance OEIS 66571 where
  oeisIx (succ->n) = f [1..] 1 n 0 where
     f (k:ks) l nl x
       | y > nl  = 0
       | y < nl  = f ks (l + 1) (nl + n) y + f ks l nl x
       | otherwise = if y `mod` l == 0 then 1 else 0
       where y = x + k

instance OEIS 66680 where
  oeis = s [2..] where
     s (b:bs) = b : s [x | x <- bs, x > b ^ 2 || mod x b > 0]

instance OEIS 66720 where
  oeis = f [] 1 S.empty where
     f ps z s | S.null s' = f ps (z + 1) s
              | otherwise   = z : f (z:ps) (z + 1) s'
       where s' = g (z:ps) s
             g []     s                      = s
             g (x:qs) s | (z * x) `S.member` s = S.empty
                        | otherwise          = g qs $ S.insert (z * x) s

instance OEIS 66853 where
  oeisIx 0 = 1
  oeisIx (succ->n) = f 1 ps [] where
     f 0 (1 : xs) ys = length ys
     f _ (x : xs) ys = if x `elem` ys then f x xs ys else f x xs (x:ys)
     ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` n) (tail ps) ps

instance OEIS 66897 where
  oeisIx = p 0 1 . succ where
     p o _             0 = o
     p o k m | m < k     = 0
             | otherwise = p (o + mod k 2) k (m - k) + p o (k + 1) m

instance OEIS 66898 where
  oeisIx = p 0 1 . succ where
     p e _             0 = e
     p e k m | m < k     = 0
             | otherwise = p (e + 1 - mod k 2) k (m - k) + p e (k + 1) m

instance OEIS 66926 where
  oeis = 1 : filter f [1..] where
     f x = elem x $ map sum $ map (map (div x)) $ ps 1 x where
       ps u 0 = [[]]
       ps u v = [t:ts | t <- [u..v], t > 1, mod x t == 0, ts <- ps t (v - t)]

instance OEIS 66949 where
  oeis = 0 : 1 : f 2 1 0 where
     f k x x' | z > k     = (z - k) : f (k+1) (z - k) x
              | otherwise = z : f (k+1) z x where z = x + x'

instance OEIS 67016 where
  oeis = [1,4,3,2] ++ f [2,3,4,1] where
     f xs = maxi : f (maxi : xs) where
       maxi = maximum $ zipWith (+) xs (reverse xs)

instance OEIS 67017 where
  oeis =  [1,4,3,2] ++ f [2,3,4,1] where
    f xs = mexi : f (mexi : xs) where
      mexi = head $ [0..] \\ zipWith (+) xs (reverse xs)

instance OEIS 67043 where
  oeis = 0 : f 1 1 0 1 where
     f k x y z
       | y > 0     = (x-y) : f k x (y `div` 10) z
       | k < 9     = x : f (k+1) (2*x-k*z+1) (z `div` 10) z
       | otherwise = x : f 1 (20*z - 1) z (10*z)

instance OEIS 67079 where
  oeisIx = product . map (fi . read) . init . tails . show . fi

instance OEIS 67251 where
  oeis = filter ((> 0) . flip mod 10) [0..]

instance OEIS 67898 where
  oeisIx n = f n [0..10] where
     f x ys | x <= 9    = head $ delete x ys
            | otherwise = f x' $ delete d ys where (x',d) = divMod x 10

instance OEIS 67998 where
  oeisIx n = n * (n - 2)
  oeis = scanl (+) 0 [-1, 1 ..]

instance OEIS 68475 where
  oeisIx n = sum $ zipWith (*) [1..n] $ iterate (* n) 1

instance OEIS 68722 where
  oeisIx n = (1 + 2 * n + 2 * n ^ 2) * (1 + 3 * n + 3 * n ^ 2)

instance OEIS 70198 where
  oeis = map (subtract 1) $ scanl lcm 1 [2..]

instance OEIS 70870 where
  oeis = 6 : f 6 where
     f x = y : f y where
       y = (if even x then 5 * x else x + 1) `div` 2

instance OEIS 70885 where
  oeis = 1 : map (flip (*) 3 . flip div 2 . (+ 1)) (oeis @70885)

instance OEIS 70939 where
  oeisIx n = if n < 2 then 1 else (oeisIx @70939) (n `div` 2) + 1
  oeis = 1 : 1 : l [1] where
     l bs = bs' ++ l bs' where bs' = map (+ 1) (bs ++ bs)

instance OEIS 71222 where
  oeisIx n = head [k | k <- [1..], gcd (n + 1) (k + 1) == gcd n k]

instance OEIS 71797 where
  oeis = f $ tail $ inits [1..] where
     f (xs:_:xss) = xs ++ f xss

instance OEIS 72007 where
  oeis = 0 : f 1 0 [1..] where
     f u v ws = g ws where
       g (x:xs) = if abs (x - v) < u
                     then g xs else x : f (u + 1) x (delete x ws)

instance OEIS 72065 where
  oeis = filter ((`elem` [0,2,9,11]) . (`mod` 12)) [0..]

instance OEIS 72103 where
  oeis = f 9 3 $ S.singleton (4,2) where
     f zz z s
       | xx < zz   = xx : f zz z (S.insert (x*xx, x) $ S.deleteMin s)
       | otherwise = zz : f (zz+2*z+1) (z+1) (S.insert (z*zz, z) s)
       where (xx, x) = S.findMin s

instance OEIS 72979 where
  oeis = 1 : f 2 [1] where
     f z xs = y : f (z + 1) (y : xs) where
       y = sum $ zipWith (*) xs (map (gcd z) [z - 1, z - 2 ..])

instance OEIS 73015 where
  oeis = iterate (\x -> (x - 1) ^ 2) 3

instance OEIS 73364 where
  oeisIx (succ->n) = genericLength $ filter (all isprime)
                       $ map (zipWith (+) [1..n]) (permutations [1..n])
     where isprime = isPrime . fi

instance OEIS 73533 where
  oeis = f 1 3 1 where
     f n p3 x = numerator (y * fi p3) : f (n + 1) (p3 * 3) y
                where y = z - fi (floor z); z = 4%3 * x

instance OEIS 73707 where
  oeis = 1 : f 0 0 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ zipWith (*) hzs (reverse hzs) where hzs = drop x zs

instance OEIS 73941 where
  oeis = 1 : f [1] where
     f xs = x' : f (x':xs) where x' = (1 + sum xs) `div` 2

instance OEIS 74066 where
  oeis = 1 : xs where xs = 4 : 3 : 2 : map (+ 3) xs

instance OEIS 74067 where
  oeis = 1 : 2 : xs where xs = 7 : 6 : 5 : 4 : 3 : map (+ 5) xs

instance OEIS 74068 where
  oeis = 1 : 2 : 3 : xs where
     xs = 10 : 9 : 8 : 7 : 6 : 5 : 4 : map (+ 7) xs

instance OEIS 74294 where
  oeis = f $ inits [1..] where
     f (xs:_:xss) = xs ++ f xss

instance OEIS 74909 where
  oeis = tablList @74909
instance Table 74909 where
  tabl = iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [1])) [1]

instance OEIS 75075 where
  oeis = 1 : 2 : f 1 2 [3..]
    where
      f z z' xs = g xs
        where
          g (u:us) = if (z * u) `mod` z' > 0 then g us else u : f z' u (delete u xs)

instance OEIS 75193 where
  oeis = 1 : -3 : zipWith (-) (oeis @75193) (tail (oeis @75193))

instance OEIS 75427 where
  oeis = 1 : f 1 1 where
     f x y = z : f (x + 1) z where z = (1 + x `mod` 2) * y + 1 - x `mod` 2

instance OEIS 76039 where
  oeis = f 1 1 where
     f n x = x' : f (n+1) x' where
             x' = (if x < n then (*) else div) x n

instance OEIS 76050 where
  oeis = 2 : f [2] where
     f xs = (drop (length xs) xs') ++ (f xs') where
       xs' = concatMap ((enumFromTo 2) . (+ 1)) xs

instance OEIS 76132 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where y = sum $ zipWith (^) xs [1..]

instance OEIS 76309 where
  oeisIx n =  n' - 2 * m where (n', m) = divMod n 10

instance OEIS 76310 where
  oeisIx n =  n' + 4 * m where (n', m) = divMod n 10

instance OEIS 76311 where
  oeisIx n =  n' - 5 * m where (n', m) = divMod n 10

instance OEIS 76312 where
  oeisIx n =  n' + 2 * m where (n', m) = divMod n 10

instance OEIS 76313 where
  oeisIx = uncurry (-) . flip divMod 10

instance OEIS 76314 where
  oeisIx = uncurry (+) . flip divMod 10

instance OEIS 76338 where
  oeisIx = (+ 1) . (* 512)
  oeis = [1,513..]

instance OEIS 76478 where
  oeis = concat $ tail $ map (tail . reverse . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2 )) [1..]

instance OEIS 76493 where
  oeisIx n = genericLength $ (intersect `on` nub . show . fi) n (n^2)

instance OEIS 76845 where
  oeisIx ((+2)->n) = head [ k | k <- [1..] , (isPrime . fi) (n ^ k + n - 1) ]

instance OEIS 76974 where
  oeis = 2 : s [3, 5 ..] where
     s (x:xs) = x : s [z | z <- xs, mod z x /= 2]

instance OEIS 77477 where
  oeis = f [1..] where
     f (x:xs) = x : f (delete (2*x + 1) $ delete (3*x + 1) xs)

instance OEIS 77610 where
  oeis = tablList @77610
instance Table 77610 where
  rowCol n k = (rowT @77610) n !! k
  rowT n = [d | d <- [1..n], let (n',m) = divMod n d, m == 0, gcd d n' == 1]
  tabf = map (rowT @77610) [1..]

instance OEIS 78178 where
  oeisIx ((+2)->n) = head [k | k <- [2..], (isPrime.fi) (n ^ k + n - 1)]

instance OEIS 78408 where
  oeis = f 1 where
     f x = (p' 1 x) : f (x + 2)
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 2) m

instance OEIS 78608 where
  oeisIx = ceiling . (2 /) . (subtract 1) . (2 **) . recip . fi . succ

instance OEIS 78633 where
  oeisIx (succ->n) = 2 * n + ceiling (2 * sqrt (fi n))

instance OEIS 78783 where
  oeis = oeis78783

instance OEIS 78812 where
  oeis = tablList @78812
instance Table 78812 where
  tabl = [1] : [2, 1] : f [1] [2, 1] where
     f us vs = ws : f vs ws where
       ws = zipWith (-) (zipWith (+) ([0] ++ vs) (map (* 2) vs ++ [0]))
                        (us ++ [0, 0])

instance OEIS 79053 where
  oeis = 1 : 2 : r (S.fromList [1,2]) 1 1 1 2 where
    r s i j x y = if v > 0 && v `S.notMember` s
                     then v : r (S.insert v s) j fib y v
                     else w : r (S.insert w s) j fib y w where
      fib = i + j
      v = x + y - fib
      w = x + y + fib

instance OEIS 79122 where
  oeisIx n = p [1..n] (2 * n) where
     p _  0     = 1
     p [] _     = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 79578 where
  oeisIx (succ->n) = head [m | m <- [n + 2 ..], gcd m n == 1]

instance OEIS 79588 where
  oeisIx n = product $ map ((+ 1) . (* n)) [1, 2, 4]

instance OEIS 79878 where
  oeis = 1 : zipWith (\x n -> if x <= n then x else x - n)
                             (map (* 2) (oeis @79878)) [2..]

instance OEIS 79944 where
  oeis =  f [0,1] where f (x:xs) = x : f (xs ++ [x,x])

instance OEIS 79978 where
  oeisIx = fi . fromEnum . (== 0) . (`mod` 3)
  oeis   = cycle [1,0,0]

instance OEIS 80098 where
  oeis = tablList @80098
instance Table 80098 where
  rowCol n k = fi do (fi n) .|. (fi k) :: Int
  rowT n = map (rowCol @80098 n) [0..n]
  tabl = map (rowT @80098) [0..]

instance OEIS 80099 where
  oeis = tablList @80099
instance Table 80099 where
  rowCol n k = fi do fi n .&. fi k :: Int
  rowT n = map (rowCol @80099 n) [0..n]
  tabl = map (rowT @80099) [0..]

instance OEIS 80100 where
  oeis =  1 : zs where
     zs =  1 : (concat $ transpose [map (* 2) zs, zs])

instance OEIS 80342 where
  oeis = 0 : zs where
     zs = 1 : 1 : (map (+ 1) $ concat $ transpose [zs, zs, zs])

instance OEIS 80425 where
  oeisIx = (`mod` 3) . (3 -) . (`mod` 3)
  oeis = cycle [0, 2, 1]

instance OEIS 80512 where
  oeisIx (succ->n) = if m == 0 then 3 * n' else n  where (n', m) = divMod n 2
  oeis = concat $ transpose [[1, 3 ..], [3, 6 ..]]

instance OEIS 81145 where
  oeis = 1 : f 1 [2..] [] where
     f x vs ws = g vs where
       g (y:ys) = if z `elem` ws then g ys else y : f y (delete y vs) (z:ws)
                  where z = abs (x - y)

instance OEIS 81577 where
  oeis = tablList @81577
instance Table 81577 where
  tabl = map fst $ iterate
      (\ (us, vs) -> (vs, zipWith (+) (map (* 2) ([0] ++ us ++ [0])) $
                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

instance OEIS 81578 where
  oeis = tablList @81578
instance Table 81578 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) (map (* 3) ([0] ++ us ++ [0])) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

instance OEIS 81604 where
  oeisIx n = if n < 3 then 1 else (oeisIx @81604) (div n 3) + 1

instance OEIS 81757 where
  oeisIx (succ->n) = genericLength [ () | j <- [2..n], i <- [1..j - 1], i * j + i - j == n]

instance OEIS 82601 where
  oeis = tablList @82601
instance Table 82601 where
  tabl = [1] : [1,0] : [1,1,0] : f [0,0,1] [0,1,0] [1,1,0]
     where f us vs ws = ys : f (0:vs) (0:ws) ys where
                        ys = zipWith3 (((+) .) . (+)) us vs ws ++ [0]

instance OEIS 82977 where
  oeis = [0, 1, 3, 5, 6, 8, 10] ++ map (+ 12) (oeis @82977)

instance OEIS 83093 where
  oeis = tablList @83093
instance Table 83093 where
  tabl = iterate
     (\ws -> zipWith (\u v -> mod (u + v) 3) ([0] ++ ws) (ws ++ [0])) [1]

instance OEIS 83329 where
  oeis = 1 : iterate ((+ 1) . (* 2)) 2

instance OEIS 83416 where
  oeis = 1 : f 2 1 where
     f x y = z : f (x+1) z where z = (1 + x `mod` 2) * y + 1 - x `mod` 2

instance OEIS 83420 where
  oeisIx = subtract 1 . (* 2) . (4 ^)

instance OEIS 84214 where
  oeis = 1 : xs where
     xs = 1 : 4 : zipWith (+) (map (* 2) xs) (tail xs)

instance OEIS 84338 where
  oeis = [1,2,3] ++ zipWith (+) (oeis @84338) (tail (oeis @84338))

instance OEIS 84385 where
  oeis = 1 : f [2..] 1 where
     f xs s = g xs where
       g (y:ys) = if gcd s y == 1 then y : f (delete y xs) (s + y) else g ys

instance OEIS 84640 where
  oeis = 0 : 1 : (map (+ 4) $
     zipWith (+) (map (* 2) (oeis @84640)) (tail (oeis @84640)))

instance OEIS 84662 where
  oeis = 4 : zipWith (+) (oeis @84662) (zipWith gcd (oeis @84662) [2..])

instance OEIS 84663 where
  oeis = 8 : zipWith (+) (oeis @84663) (zipWith gcd (oeis @84663) [2..])

instance OEIS 84937 where
  oeis = 1 : 2 : f 2 1 [3..] where
     f x y zs = g zs where
        g (u:us) | gcd y u > 1 || gcd x u > 1 = g us
                 | otherwise = u : f u x (delete u zs)

instance OEIS 84964 where
  oeis = concat $ transpose [[2..], [0..]]

instance OEIS 84984 where
  oeis = filter (not . any (`elem` "2357") . show . fi) [0..]

instance OEIS 85059 where
  oeis = 1 : f 1 1 where
     f v w = y : f (v + 1) y where
       y = if w > v then w - v else w + v

instance OEIS 85144 where
  oeis = 0 : concat
     (transpose [map negate (oeis @85144), map (+ 1) $ tail (oeis @85144)])

instance OEIS 86099 where
  oeisIx (fi->n) = fi do foldl1 (.|.) $ zipWith (.&.) [0..] $ reverse [0..n] :: Integer

instance OEIS 87057 where
  oeis = f [2..] where
     f (x:xs) = x : f (us ++ vs) where (us, _ : vs) = splitAt (x - 1) xs

instance OEIS 87069 where
  oeisIx =
     sum . unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4))

instance OEIS 87116 where
  oeisIx 0 = 1
  oeisIx n = f 0 n where
     f y 0 = y
     f y x = if r == 0 then g x' else f y x'
             where (x', r) = divMod x 2
                   g z = if r == 0 then g z' else f (y + 1) z'
                         where (z', r) = divMod z 2

instance OEIS 87117 where
  oeisIx 0       = 1
  oeisIx n
    | null $ zs n = 0
    | otherwise   = maximum $ map length $ zs n where
    zs = filter ((== 0) . head) . group .
         unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 87401 where
  oeis = tablList @87401
instance Table 87401 where
  tabl = iterate f [0] where
     f row = row' ++ [last row'] where row' = zipWith (+) row [0..]

instance OEIS 87811 where
  oeisIx (succ->n) = (n + n `mod` 2) * (n + 2 - n `mod` 2) `div` 4

instance OEIS 87960 where
  oeisIx n = (-1) ^ (n * (n + 1) `div` 2)
  oeis = cycle [1,-1,-1,1]

instance OEIS 88157 where
  oeisIx n = mod (div (n ^ n) (60 ^ n)) 60

instance OEIS 88323 where
  oeisIx ((+2)->n) = sum $ map (f n) [2 .. n - 1] where
     f x b = if x == 0 then 1 else if d /= 1 then 0 else f x' b
                                   where (x',d) = divMod x b

instance OEIS 88567 where
  oeis = 1 : tail xs where
     xs = 0 : 1 : zipWith (+) xs (tail $ concat $ transpose [xs, tail xs])

instance OEIS 88696 where
  oeis = f [1] where
     f (x:xs) = x : f (xs ++ [x + 1 - x `mod` 2, x + x `mod` 2])

instance OEIS 88961 where
  oeis = concat $ concat $ map f [1..] where
     f x = take x $ g (take x (1 : [0,0..])) where
       g us = (take x $ g' us) : g (0 : init us)
       g' vs = last $ take (2 * x + 1) $
                      map snd $ iterate h (0, vs ++ reverse vs)
     h (p,ws) = (1 - p, drop p $ zipWith (+) ([0] ++ ws) (ws ++ [0]))

instance OEIS 89088 where
  oeis = 1 : 2 : f [3..] [1,2] where
    f xs ys = y : f (delete y xs) (y : ys) where
      y = head $ filter (\z -> any (> 1) $ map (gcd z) ys) xs

instance OEIS 89128 where
  oeisIx = gcd 6

instance OEIS 89633 where
  oeis = [2 ^ t - 2 ^ k - 1 | t <- [1..], k <- [t - 1,t - 2..0]]

instance OEIS 89781 where
  oeis = 1 : f [1..] 1 where
     f xs y = g xs where
       g (z:zs) = if gcd y z == 1 then y' : f (delete z xs) y' else g zs
                  where y' = y + z

instance OEIS 90529 where
  oeis = f 1 1 0 where
    f w v u = if u <= w then v : f w v (u+1) else v' : f (w*v') v' (u+1)
              where v' = v + 1

instance OEIS 90633 where
  oeisIx (succ->n) = numerator z where
     [z] = (until ((== 1) . length) avg) $ map (1 %) [1..n]
     avg xs = zipWith (\x x' -> (x + x') / 2) (tail xs) xs

instance OEIS 90634 where
  oeisIx (succ->n) = denominator z where
     [z] = (until ((== 1) . length) avg) $ map (1 %) [1..n]
     avg xs = zipWith (\x x' -> (x + x') / 2) (tail xs) xs

instance OEIS 90771 where
  oeis = 1 : 9 : map (+ 10) (oeis @90771)

instance OEIS 90794 where
  oeisIx = genericLength . filter odd . map (length . group) . ps 1 . succ where
     ps x 0 = [[]]
     ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

instance OEIS 90895 where
  oeis = 1 : f 2 1 where
     f x y = z : f (x + 1) z where
          z = if m == 0 then y' else x + y; (y',m) = divMod y 2

instance OEIS 91090 where
  oeis = 1 : f [1,1] where f (x:y:xs) = y : f (x:xs ++ [x,x+y])

instance OEIS 91491 where
  oeis = tablList @91491
instance Table 91491 where
  tabl = iterate (\row -> 1 : scanr1 (+) row) [1]

instance OEIS 91856 where
  oeis = 1 : f 1 1 [2..] where
     f s z xs = g xs where
       g (u:us) = if gcd u z - s /= 1 then u : f (1 - s) u us else g us

instance OEIS 91857 where
  oeis = 1 : f 1 1 [2..] where
     f 1 z xs = g xs where
       g (u:us) = if gcd u z == 1 then u : f 0 u (delete u xs) else g us
     f 0 z xs = h xs where
       h (v:vs) = if gcd v z /= 1 then v : f 1 v (delete v xs) else h vs

instance OEIS 91998 where
  oeis = 1 : 11 : map (+ 12) (oeis @91998)

instance OEIS 91999 where
  oeis = 2 : 10 : map (+ 12) (oeis @91999)

instance OEIS 92038 where
  oeis = 1 : zipWith (\u v -> v + (v `mod` 2) ^ (u `mod` v))
                             [2..] (oeis @92038)

instance OEIS 92306 where
  oeisIx = genericLength . filter even . map (length . group) . ps 1 where
     ps x 0 = [[]]
     ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

instance OEIS 92338 where
  oeisIx (succ->n) = genericLength $ filter (<= 1) $ map (mod n) [1..n]

instance OEIS 92401 where
  oeis = f [1..] where
     f (x:xs) = x : x' : f (delete x' xs) where x' = 3*x

instance OEIS 92525 where
  oeisIx (succ->n) = f n n where
     f x y = if m == 0 then f x' (2 * y + 1) else y
             where (x', m) = divMod x 2

instance OEIS 93391 where
  oeisIx n = sum $ map ((flip div 16) . (+ n)) [0..3]

instance OEIS 93445 where
  oeis = tablList @93445
instance Table 93445 where
  rowCol n k = (rowT @93445) n !! (k- 1)
  rowT n = f [n, n - 1 .. 1] [1 ..] where
     f [] _      = []
     f (x:xs) ys = sum us : f xs vs where (us,vs) = splitAt x ys
  tabl = map (rowT @93445) [1 ..]

instance OEIS 93485 where
  oeisIx n = (9 * n * (3 * n + 1) + 2) `div` 2

instance OEIS 93506 where
  oeis = 1 : 2 : f 1 [1] [3,5..] [4,6..]
     where f 0 (z:zs) odds evens = orun ++ f 1 (zs ++ orun) odds' evens
             where (orun, odds') = splitAt z odds
           f 1 (z:zs) odds evens = erun ++ f 0 (zs ++ erun) odds evens'
             where (erun, evens') = splitAt z evens

instance OEIS 93560 where
  oeis = tablList @93560
instance Table 93560 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [3, 1]

instance OEIS 93561 where
  oeis = tablList @93561
instance Table 93561 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [4, 1]

instance OEIS 93562 where
  oeis = tablList @93562
instance Table 93562 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [5, 1]

instance OEIS 93563 where
  oeis = tablList @93563
instance Table 93563 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [6, 1]

instance OEIS 93564 where
  oeis = tablList @93564
instance Table 93564 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [7, 1]

instance OEIS 93565 where
  oeis = tablList @93565
instance Table 93565 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [8, 1]

instance OEIS 93644 where
  oeis = tablList @93644
instance Table 93644 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [9, 1]

instance OEIS 93645 where
  oeis = tablList @93645
instance Table 93645 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [10, 1]

instance OEIS 93820 where
  oeis = 1 : f [2..] [1] where
     f (x:xs) ys = y : f xs (y:ys) where y = sum $ map (gcd x) ys

instance OEIS 94587 where
  oeis = tablList @94587
instance Table 94587 where
  tabl = map fst $ iterate f ([1], 1)
     where f (row, i) = (map (* i) row ++ [1], i + 1)

instance OEIS 94912 where
  oeisIx n = a 2 n where
     a s 0 = 0 ^ s
     a s x = a (t s b) x' where (x',b) = divMod x 2
     t 2 0 = 2; t 2 1 = 1; t 1 0 = 0; t 1 1 = 2; t 0 0 = 2; t 0 1 = 0

instance OEIS 95258 where
  oeis = 1 : f [2..] 1 where
     f xs z = g xs where
       g (y:ys) = if mod z' y > 0 then g ys else y : f (delete y xs) (z + y)
       z' = z + 2

instance OEIS 95344 where
  oeis = tail xs where
     xs = 1 : 1 : 1 : zipWith (-) (map (* 5) $ zipWith (+) (tail xs) xs) xs

instance OEIS 95660 where
  oeis = tablList @95660
instance Table 95660 where
  tabl = [3] : iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,3]

instance OEIS 95666 where
  oeis = tablList @95666
instance Table 95666 where
  tabl = [4] : iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,4]

instance OEIS 96535 where
  oeis = 1 : 1 : f 2 1 1 where
     f n x x' = y : f (n+1) y x where y = mod (x + x') n

instance OEIS 96777 where
  oeis = 1 : zipWith (+) (oeis @96777)
                                 (scanl1 (+) (map (`mod` 2) (oeis @96777)))

instance OEIS 97054 where
  oeis = f 9 (3, 2) (M.singleton 4 (2, 2)) where
     f zz (bz, be) m
      | xx < zz && even be =
                  f zz (bz, be+1) (M.insert (bx*xx) (bx, be+1) $ M.deleteMin m)
      | xx < zz = xx :
                  f zz (bz, be+1) (M.insert (bx*xx) (bx, be+1) $ M.deleteMin m)
      | xx > zz = f (zz+2*bz+1) (bz+1, 2) (M.insert (bz*zz) (bz, 3) m)
      | otherwise = f (zz + 2 * bz + 1) (bz + 1, 2) m
      where (xx, (bx, be)) = M.findMin m

instance OEIS 97065 where
  oeisIx n = n' - 2 * m where (n', m) = divMod (n + 2) 2
  oeis = concat $ transpose [[1 ..], [-1 ..]]

instance OEIS 97080 where
  oeisIx (succ->n) = 2 * n * (n - 1) + 3

instance OEIS 98311 where
  oeis = 1 : ys where
     ys = 2 : f ys [3..] where
          f (v:vs) ws = us ++ f vs (ws \\ us) where
            us = take 2 $ filter ((== 1) . (gcd v)) ws

instance OEIS 98548 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v (w:ws) = if gcd u w > 1 && gcd v w == 1
                       then w : f v w ws else f u v ws

instance OEIS 98550 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v ws = g ws where
       g (x:xs) = if gcd x u > 1 && gcd x v == 1
                     then x : f v x (delete x ws) else g xs

instance OEIS 98859 where
  oeisIx = p 0 [] 1 where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p m ms k x
       | x < k       = 0
       | m == 0      = p 1 ms k (x - k) + p 0 ms (k + 1) x
       | m `elem` ms = p (m + 1) ms k (x - k)
       | otherwise   = p (m + 1) ms k (x - k) + p 0 (m : ms) (k + 1) x

instance OEIS 99054 where
  oeis = 1 : concatMap fromJust (zipWith stripPrefix ass $ tail ass)
     where ass = iterate f [1]
           f xs = concat $ concat $ transpose [map g $ e xs, map h $ o xs]
           g 1 = [1,2,3]; g 2 = [2,3,1]; g 3 = [3,1,2]
           h 1 = [3,2,1]; h 2 = [1,3,2]; h 3 = [2,1,3]
           e [] = []; e [x] = [x]; e (x:_:xs) = x : e xs
           o [] = []; o [x] = []; o (_:x:xs) = x : o xs

instance OEIS 99267 where
  oeis = f 1 [1..] 0 where
     f k xs y = ys' ++ f (k+1) (ys ++ xs') g where
       ys' = dropWhile (< y) ys
       (ys,_:xs') = span (< g) xs
       g = xs !! (h - 1)
       h = xs !! (k - 1)

instance OEIS 99375 where
  oeis = tablList @99375
instance Table 99375 where
  tabl = iterate (\xs -> (head xs + 2) : xs) [1]

instance OEIS 99427 where
  oeis = 1 : map (+ 1) (zipWith gcd [2..] (oeis @99427))

instance OEIS 99959 where
  oeis = tablList @99959
instance Table 99959 where
  tabl = map snd $ iterate f (False,[1]) where
     f (s,xs) = (not s, if s then zs ++ [last zs] else zs)
       where zs = scanl1 (+) (reverse xs)

instance OEIS 99961 where
  oeis = tablList @99961
instance Table 99961 where
  tabl = map snd $ iterate f (0,[1]) where
     f (s,xs) = (s+1, if s `mod` 3 == 1 then zs ++ [last zs] else zs)
       where zs = scanl1 (+) (reverse xs)

instance OEIS 99996 where
  oeisIx = foldl lcm 1 . enumFromTo 2 . (* 2)

instance OEIS 100104 where
  oeisIx n = n*n*n - n*n + 1

instance OEIS 100471 where
  oeisIx 0 = 1
  oeisIx n = p 0 (n + 1) 1 n where
     p m m' k x | x == 0    = if m < m' || m == 0 then 1 else 0
                | x < k     = 0
                | m == 0    = p 1 m' k (x - k) + p 0 m' (k + 1) x
                | otherwise = p (m + 1) m' k (x - k) +
                              if m < m' then p 0 m (k + 1) x else 0

instance OEIS 100613 where
  oeisIx (succ->n) = genericLength [ ()| x <- [1..n], y <- [1..n], gcd x y > 1]

instance OEIS 100617 where
  oeisIx = f 2 . succ where
     f k x = if x' == 0 then x else f (k + 1) (x - x') where x' = div x k

instance OEIS 100618 where
  oeisIx (succ->n) = f 2 n where
     f k n | n' == 0   = n
           | otherwise = f (k+1) (n-n') where n' = div n (k^2)

instance OEIS 100707 where
  oeis = 1 : f 1 (S.singleton 1) [1..] where
     f y st ds = g ds where
       g (k:ks) | v <= 0      = h ds
                | S.member v st = g ks
                | otherwise   = v : f v (S.insert v st) (delete k ds)
                where v = y - k
       h (k:ks) | S.member w st = h ks
                | otherwise   = w : f w (S.insert w st) (delete k ds)
                where w = y + k

instance OEIS 100830 where
  oeisIx (succ->n)  = n + 9 * (-1) ^ ((n - 1) `div` 9)

instance OEIS 100881 where
  oeisIx = p 0 0 1 where
     p m m' k x | x == 0    = if m > m' || m == 0 then 1 else 0
                | x < k     = 0
                | m == 0    = p 1 m' k (x - k) + p 0 m' (k + 1) x
                | otherwise = p (m + 1) m' k (x - k) +
                              if m > m' then p 0 m (k + 1) x else 0

instance OEIS 101369 where
  oeis = f [1..] where
     f (x:xs) = x : y : f (delete y xs) where y = xs !! (x - 1)

instance OEIS 101881 where
  oeis = scanl1 (+) $ intersperse 1 [1..]

instance OEIS 101986 where
  oeisIx n = sum $ zipWith (*) [1,3..] (reverse [2..n+1])

instance OEIS 102413 where
  oeis = tablList @102413
instance Table 102413 where
  tabl = [1] : [1,1] : f [2] [1,1] where
     f us vs = ws : f vs ws where
               ws = zipWith3 (((+) .) . (+))
                    ([0] ++ us ++ [0]) ([0] ++ vs) (vs ++ [0])

instance OEIS 102487 where
  oeis = filter (all (< 10) . unfoldr (\x ->
     if x == 0 then Nothing else Just $ swap $ divMod x 12)) [0..]

instance OEIS 102488 where
  oeis = filter (any (> 9) . unfoldr (\x ->
     if x == 0 then Nothing else Just $ swap $ divMod x 12)) [1..]

instance OEIS 102489 where
  oeis = mapMaybe dhex [0..] where
     dhex 0                         = Just 0
     dhex x | d > 9 || y == Nothing = Nothing
            | otherwise             = Just $ 16 * fromJust y + d
            where (x', d) = divMod x 16; y = dhex x'

instance OEIS 102490 where
  oeis = filter (any (> 9) . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 16)) [0..]

instance OEIS 102491 where
  oeis = filter (all (<= 9) . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 20)) [0..]

instance OEIS 102492 where
  oeis = filter (any (> 9) . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 20)) [0..]

instance OEIS 102493 where
  oeis = filter (all (<= 9) . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 60)) [0..]

instance OEIS 102494 where
  oeis = filter (any (> 9) . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 60)) [0..]

instance OEIS 102662 where
  oeis = tablList @102662
instance Table 102662 where
  tabl = [1] : [1,3] : f [1] [1,3] where
     f xs ys = zs : f ys zs where
       zs = zipWith (+) ([0] ++ xs ++ [0]) $
                        zipWith (+) ([0] ++ ys) (ys ++ [0])

instance OEIS 102683 where
  oeisIx =  genericLength . filter (== '9') . show . fi

instance OEIS 103127 where
  oeis = [x | x <- [1..], x `mod` 16 `elem` [1,3,5,15]]

instance OEIS 103181 where
  oeisIx n = foldl f 0 $ reverse $ unfoldr g n where
     f v d = 10 * v + mod d 2
     g x = if x == 0 then Nothing else Just $ swap $ divMod x 10

instance OEIS 103215 where
  oeis = [1,2,5,10,13,17] ++ map (+ 24) (oeis @103215)

instance OEIS 103284 where
  oeis = tablList @103284
instance Table 103284 where
  tabl = iterate (\xs -> sort $ zipWith (+) (xs++[0]) ([0]++xs)) [1]

instance OEIS 103391 where
  oeis = 1 : ks where
     ks = concat $ transpose [[2..], ks]

instance OEIS 103631 where
  oeis = tablList @103631
instance Table 103631 where
  tabl = [1] : [0,1] : f [1] [0,1] where
     f xs ys = zs : f ys zs where
       zs = zipWith (+)  ([0,0] ++ xs)  (ys ++ [0])

instance OEIS 104035 where
  oeis = tablList @104035
instance Table 104035 where
  tabl = iterate f [1] where
     f xs = zipWith (+)
       (zipWith (*) [1..] (tail xs) ++ [0,0]) ([0] ++ zipWith (*) [1..] xs)

instance OEIS 104249 where
  oeisIx n = n* (3*n+1) `div` 2 + 1

instance OEIS 104895 where
  oeis = 0 : concat (transpose [map (negate . (+ 1)) zs, tail zs])
                 where zs = map (* 2) (oeis @104895)

instance OEIS 105279 where
  oeis = iterate ((* 10) . (+ 1)) 0

instance OEIS 105809 where
  oeis = tablList @105809
instance Table 105809 where
  tabl = map fst $ iterate
     (\ (u:_, vs) -> (vs, zipWith (+) ([u] ++ vs) (vs ++ [0]))) ([1], [1,1])

instance OEIS 106195 where
  oeis = tablList @106195
instance Table 106195 where
  tabl = [1] : [2, 1] : f [1] [2, 1] where
     f us vs = ws : f vs ws where
       ws = zipWith (-) (zipWith (+) ([0] ++ vs) (map (* 2) vs ++ [0]))
                        ([0] ++ us ++ [0])

instance OEIS 106318 where
  oeisIx = (* 2) . (^ 6) . succ

instance OEIS 106328 where
  oeis = 0 : 3 : zipWith (-) (map (* 6) (tail (oeis @106328))) (oeis @106328)

instance OEIS 106370 where
  oeisIx (succ->n) = f 2 n where
     f b x = g x where
       g 0 = b
       g z = if r == 0 then f (b + 1) n else g z'
             where (z', r) = divMod z b

instance OEIS 106400 where
  oeis =  1 : concat
     (transpose [map negate (oeis @106400), tail (oeis @106400)])

instance OEIS 106579 where
  oeis = tablList @106579
instance Table 106579 where
  tabl = [1] : iterate
     (\row -> scanl1 (+) $ zipWith (+) ([0] ++ row) (row ++ [0])) [0,1]

instance OEIS 106828 where
  oeis = tablList @106828
instance Table 106828 where
  tabf = map (fst . fst) $ iterate f (([1], [0]), 1) where
     f ((us, vs), x) =
       ((vs, map (* x) $ zipWith (+) ([0] ++ us) (vs ++ [0])), x + 1)

instance OEIS 106831 where
  oeis = tablList @106831
instance Table 106831 where
  tabf = map (map (\ (_, _, left, right) -> left * right)) $
     iterate (concatMap (\ (x, f, left, right) -> let f' = f * x in
     [ (x + 1, f', f', right), (3, 2, 2, left * right)])) [ (3, 2, 2, 1)]

instance OEIS 107354 where
  oeisIx n = head $ snd $ until ((== 1) . fst)
                                 f (2^n, replicate (2^n) 1) where
     f (len, xs) = (len', scanl1 (+) $ drop len' xs) where
        len' = len `div` 2

instance OEIS 107788 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (8 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 107846 where
  oeisIx = genericLength . concatMap tail . group . sort . show . fi

instance OEIS 107988 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (4 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108090 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (11 * y, i + 1, j) $ S.insert (13 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108218 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (11 * y, i + 1, j) $ S.insert (12 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108397 where
  oeisIx 0 = 0
  oeisIx 1 = 2
  oeisIx n = n * (n^ (n+1) + n^2 - 2) `div` (2 * (n - 1))

instance OEIS 108398 where
  oeisIx n = n * (1 + n ^ n) `div` 2

instance OEIS 108411 where
  oeisIx = (3 ^) . flip div 2

instance OEIS 108687 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (9 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108696 where
  oeis = 1 : sieve' 2 [2..] where
     sieve' n (x:xs) = x : (sieve' (n+1) $ sieving xs) where
        sieving xs = (take (n - 1) xs) ++ (sieving $ drop n xs)

instance OEIS 108698 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (6 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108761 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (10 * y, i + 1, j) $ S.insert (13 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108779 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (10 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 109008 where
  oeisIx = gcd 4
  oeis = cycle [4,1,2,1]

instance OEIS 109045 where
  oeisIx = lcm 4

instance OEIS 109128 where
  oeis = tablList @109128
instance Table 109128 where
  tabl = iterate (\row -> zipWith (+)
     ([0] ++ row) (1 : (map (+ 1) $ tail row) ++ [0])) [1]

instance OEIS 110380 where
  oeis = drop 1 fn
      where fn    = 0 : 1 : concat (fn' 2)
            fn' n = (map (+ones) (drop nv $ take (n + nv) fn)) : (fn' (n+1))
                    where ones = div (10^n - 1) 9
                          nv   = div ((n - 1)* (n - 2)) 2

instance OEIS 110440 where
  oeis = tablList @110440
instance Table 110440 where
  tabl = iterate (\xs ->
     zipWith (+) ([0] ++ xs) $
     zipWith (+) (map (* 3) (xs ++ [0]))
                 (map (* 2) (tail xs ++ [0,0]))) [1]

instance OEIS 110591 where
  oeisIx 0 = 1
  oeisIx n = genericLength $
     unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4)) n

instance OEIS 111063 where
  oeis = 1 : zipWith (+) [1..] (zipWith (*) [0..] (oeis @111063))

-- instance OEIS 111712 where
--   oeis = scanl (+) 1 (oeis @195013)

instance OEIS 111708 where
  oeisIx 0 = 9
  oeisIx n = f [] n where
     f ys 0 = foldl (\v d -> 10 * v + d) 0 $ ys ++ map (9 -) ys
     f ys x = f (d : ys) x' where (x', d) = divMod x 10

instance OEIS 111721 where
  oeis = 1 : 1 : map (+ 5) (zipWith (+) (oeis @111721) (tail (oeis @111721)))

instance OEIS 112310 where
  oeis = concat fss where
     fss = [0] : [1] : (map (map (+ 1))) (zipWith (++) fss $ tail fss)

instance OEIS 112373 where
  oeis = 1 : 1 : zipWith (\u v -> (u^3 + u^2) `div` v)
                                 (tail (oeis @112373)) (oeis @112373)

instance OEIS 112468 where
  oeis = tablList @112468
instance Table 112468 where
  tabl = iterate (\xs -> zipWith (-) ([2] ++ xs) (xs ++ [0])) [1]

instance OEIS 112765 where
  oeisIx (succ->n) = fives n 0 where
     fives n e | r > 0     = e
               | otherwise = fives n' (e + 1) where (n',r) = divMod n 5

instance OEIS 113630 where
  oeisIx n = sum $ zipWith (*) [1..9] $ iterate (* n) 1

instance OEIS 113801 where
  oeis = 1 : 13 : map (+ 14) (oeis @113801)

instance OEIS 113963 where
  oeis = 1 : f 1 [2..] where
     f z xs = g xs where
       g (y:ys) = if (y + z) `mod` abs (y - z) > 0
                     then y : f y (delete y xs) else g ys

instance OEIS 114283 where
  oeis = tablList @114283
instance Table 114283 where
  tabl = iterate
     (\row -> (sum $ zipWith (+) row $ reverse row) : row) [1]

instance OEIS 114851 where
  oeisIx = open where
    open n = if n<2 then 0 else
             1 + open (n - 2) + sum [open i * open (n - 2 - i) | i <- [0..n - 2]]

instance OEIS 116478 where
  oeis = 1 : f [2..] [1] where
     f (x:xs) ys = y : f xs (y:ys) where y = sum $ map (div x) ys

instance OEIS 116520 where
  oeis = 0 : zs where
     zs = 1 : (concat $ transpose
                        [zipWith (+) vs zs, zipWith (+) vs $ tail zs])
        where vs = map (* 4) zs

instance OEIS 117070 where
  oeis = tSegments !! 0

instance OEIS 117071 where
  oeis = tSegments !! 1

instance OEIS 117072 where
  oeis = tSegments !! 2

instance OEIS 117073 where
  oeis = oeis117073

instance OEIS 117140 where
  oeis = 5 : 7 : ulam 2 7 (oeis @117140)

instance OEIS 118372 where
  oeis = sPerfect 1 [] where
     sPerfect x ss | v > x = sPerfect (x + 1) ss
                   | v < x = sPerfect (x + 1) (x : ss)
                   | otherwise = x : sPerfect (x + 1) (x : ss)
                   where v = sum (filter ((== 0) . mod x) ss)

instance OEIS 118950 where
  oeis = filter (any (`elem` "2357") . show . fi) [0..]

instance OEIS 119258 where
  oeis = tablList @119258
instance Table 119258 where
  tabl = iterate (\row -> zipWith (+)
     ([0] ++ init row ++ [0]) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 119352 where
  oeisIx n = f 2 n where
     f b x = g x where
       g 0 = b
       g z = if r == b - 1 then f (b + 1) n else g z'
             where (z', r) = divMod z b

instance OEIS 120004 where
  oeisIx n = fi . sum $ map (fromEnum . (`isInfixOf` show do fi n) . show . fi) [0..n]

instance OEIS 121022 where
  oeis = filter (('2' `elem`) . show . fi) [0, 2 ..]

instance OEIS 121173 where
  oeis = f 1 [] where
     f x ys = y : f (x + 1) (y : ys) where
       y = if x `elem` ys then sum ys else x + 1

instance OEIS 121216 where
  oeis = 1 : 2 : f 1 2 [3..] where
    f x y zs = g zs where
      g (u:us) = if gcd x u == 1 then h $ delete u zs else g us where
        h (v:vs) = if gcd y v == 1 then u : v : f u v (zs \\ [u,v]) else h vs

instance OEIS 121217 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v xs = g xs where
       g (w:ws) = if gcd w u > 1 then w : f v w (delete w xs) else g ws

instance OEIS 121262 where
  oeisIx = (0 ^) . flip mod 4
  oeis = cycle [1,0,0,0]

instance OEIS 121384 where
  oeisIx = ceiling . (* exp 1) . fi

instance OEIS 121757 where
  oeis = tablList @121757
instance Table 121757 where
  tabl = iterate
     (\xs -> zipWith (+) (xs ++ [0]) (zipWith (*) [1..] ([0] ++ xs))) [1]

instance OEIS 121758 where
  oeisIx = foldl (\v d -> 10 * v + d) 0 . reverse . unfoldr f . succ where
     f 0 = Nothing
     f x | odd d     = Just (- d, x')
         | otherwise = Just (d, x') where (x', d) = divMod x 10

instance OEIS 121759 where
  oeisIx = foldl f 0 . reverse . unfoldr d . succ where
     d 0 = Nothing
     d x = Just $ swap $ divMod x 10
     f v d | even d = 10 * v - d
           | odd  d = 10 * v + d

instance OEIS 122196 where
  oeis = concatMap (\x -> enumFromThenTo x (x - 2) 1) [1..]

instance OEIS 122542 where
  oeis = tablList @122542
instance Table 122542 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [0, 1])

instance OEIS 122841 where
  oeisIx = f 0 . succ where
     f y x = if r > 0 then y else f (y + 1) x'
             where (x', r) = divMod x 6

instance OEIS 123010 where
  oeis = 1 : 0 : 4 : 16 : f 0 4 16
    where f a b c = let x = 5*c + b - 5*a in x : f b c x

instance OEIS 123866 where
  oeisIx = (subtract 1) . (^ 6) . (+1)

instance OEIS 124927 where
  oeis = tablList @124927
instance Table 124927 where
  tabl = iterate
     (\row -> zipWith (+) ([0] ++ reverse row) (row ++ [1])) [1]

instance OEIS 125053 where
  oeis = tablList @125053
instance Table 125053 where
  tabf = iterate f [1] where
    f zs = zs' ++ reverse (init zs') where
      zs' = (sum zs) : g (map (* 2) zs) (sum zs)
      g [x] y = [x + y]
      g xs y = y' : g (tail $ init xs) y' where y' = sum xs + y

instance OEIS 125203 where
  oeisIx (succ->n) = genericLength [ () | x <- [1 .. (n + 1) `div` 3],
                           let (y,m) = divMod (x + n) (4 * x - 1),
                           x <= y, m == 0]

instance OEIS 125605 where
  oeis = tablList @125605
instance Table 125605 where
  tabl = iterate f [1] where
     f xs = zipWith (\v w -> (v + w) `div` gcd v w) ([0] ++ xs) (xs ++ [0])

instance OEIS 125717 where
  oeis =  0 : f [1..] 0 (M.singleton 0 0) where
     f (v:vs) w m = g (reverse[w-v,w- 2*v..1] ++ [w+v,w+2*v..]) where
       g (x:xs) = if x `M.member` m then g xs else x : f vs x (M.insert x v m)

instance OEIS 126025 where
  oeisIx (succ->n) = h n1s 0 where
     h us c = if us == nns then c + 1 else h (succ us) (c + g) where
       g = if and [f x `gcd` f y == f (x `gcd` y) |
                   x <- [1 .. n - 1], y <- [x + 1 .. n]] then 1 else 0
       f = (us !!) . subtract 1
     succ (z:zs) = if z < n then (z + 1) : zs else 1 : succ zs
     n1s = take n [1, 1 ..]; nns = take n [n, n ..]

instance OEIS 126428 where
  oeis =  magics 1 [] [] where
     -- magics :: Integer -> [Integer] -> [Integer] -> [Integer]
     magics n ms tests
        | tests `intersect` nMinus == [] && tests `intersect` nPlus == []
        = n : magics (n+1) (n:ms) (nMinus ++ nPlus ++ tests)
        | otherwise
        = magics (n+1) ms tests
        where nMinus = map (n -) ms
              nPlus  = map (n +) ms

instance OEIS 126646 where
  oeisIx = (subtract 1) . (2 ^) . (+ 1)
  oeis = iterate ((+ 1) . (* 2)) 1

instance OEIS 126796 where
  oeis = map (pMemo 1 1) [0..] where
     pMemo = memo3 integral integral integral p
     p _ _ 0 = 1
     p s k m
       | k > min m s = 0
       | otherwise   = pMemo (s + k) k (m - k) + pMemo s (k + 1) m

instance OEIS 127093 where
  oeis = tablList @127093
instance Table 127093 where
  rowCol n k = (rowT @127093) n !! (k- 1)
  rowT n = zipWith (*) [1..n] $ map ((0 ^) . (mod n)) [1..n]
  tabl = map (rowT @127093) [1..]

instance OEIS 127324 where
  oeis = concatMap (concatMap concat .
                 inits . inits . enumFromTo 0) $ enumFrom 0

instance OEIS 127330 where
  oeis = tablList @127330
instance Table 127330 where
  tabl = step 0 1 where
     step s k = [s .. s + k - 1] : step (2 * s + 2) (k + 1)

instance OEIS 127423 where
  oeis = map fi do 1 : map read (zipWith (++) (tail iss) iss) :: [Integer]
                     where iss = map show [1..]

instance OEIS 127648 where
  oeis = tablList @127648
instance Table 127648 where
  tabl = map reverse $ iterate (\ (x:xs) -> x + 1 : 0 : xs) [1]

instance OEIS 127824 where
  oeis = tablList @127824
instance Table 127824 where
  tabf = iterate f [1] where
     f row = sort $ map (* 2) row `union`
                    [x' | x <- row, let x' = (x - 1) `div` 3,
                          x' * 3 == x - 1, odd x', x' > 1]

instance OEIS 128588 where
  oeis = 1 : cows where
                     cows = 2 : 4 : zipWith (+) cows (tail cows)

instance OEIS 128918 where
  oeisIx n = (n + m - 1) * n' + m * n - m + 1  where (n', m) = divMod n 2

instance OEIS 128966 where
  oeis = tablList @128966
instance Table 128966 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([0], [1, 1])

instance OEIS 129296 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n], (n ^ 2 - 1) `mod` d == 0]

instance OEIS 129713 where
  oeis = tablList @129713
instance Table 129713 where
  tabl = [1] : [1, 1] : f [1] [1, 1] where
     f us vs = ws : f vs ws where
               ws = zipWith (+) (init us ++ [0, 0, 0]) (vs ++ [1])

instance OEIS 130130 where
  oeisIx = min 2
  oeis = 0 : 1 : repeat 2

instance OEIS 130321 where
  oeis = tablList @130321
instance Table 130321 where
  tabl = iterate (\row -> (2 * head row) : row) [1]

instance OEIS 130658 where
  oeisIx = (+ 1) . (`div` 2) . (`mod` 4)
  oeis = cycle [1,1,2,2]

instance OEIS 130720 where
  oeisIx (succ->n)
    = fromJust
    $ find ((show (fi n) `isInfixOf`) . show . fi)
    $ tail
    $ scanl1 (+) [n..]

instance OEIS 131134 where
  oeis = 1 : zipWith (\v w -> (v+w) `div` gcd v w) [2..] (oeis @131134)

instance OEIS 131179 where
  oeisIx n = (n + 1 - m) * n' + m  where (n', m) = divMod n 2

instance OEIS 131507 where
  oeis = tablList @131507
instance Table 131507 where
  tabl = zipWith ($) (map replicate [1..]) [1, 3 ..]

instance OEIS 131813 where
  oeis = 0 : f [[0]] where
     f xss = y : f (bin y : xss) where
       y = sum $ map (fi . fromEnum . (flip isInfixOf $ head xss)) xss
     bin n = if n == 0 then [] else b : bin n' where (n',b) = divMod n 2

instance OEIS 131835 where
  oeis = concat $ iterate (concatMap (\x -> map (+ 10 * x) [0..9])) [1]

instance OEIS 132011 where
  oeisIx = p [1..] . succ where
     p _  0 = 1
     p (k:ks) m = if m < k then 0 else p [3 * k ..] (m - k) + p ks m

instance OEIS 132140 where
  oeis = filter f [1..] where
     f x = x < 3 && x == 1 || t > 0 && f x' where (x', t) = divMod x 3

instance OEIS 132141 where
  oeis = filter ((== 1) . until (< 3) (flip div 3)) [1..]

instance OEIS 132171 where
  oeis = 1 : zs where
     zs = 3 : 3 : 3 : (map (* 3) $ concat $ transpose [zs, zs, zs])

instance OEIS 132223 where
  oeis = f 1 [1] where
     f k xs = ys ++ f (2 * k) ys where
              ys = concat $ transpose [xs, reverse $ take k [k+1 ..]]

instance OEIS 132273 where
  oeisIx (succ->n) = sum $ zipWith (!!) coprimess (reverse [0..n - 1]) where
     coprimess = map (\x -> filter ((== 1) . (gcd x)) [1..]) [1..]

instance OEIS 132429 where
  oeisIx = (3 -) . (* 2) . (`mod` 4)
  oeis = cycle [3, 1, -1, -3]

instance OEIS 132666 where
  oeis = 1 : f 1 [2..] where
     f z xs  = y : f y (delete y xs) where
       y | head xs > z = 2 * z
         | otherwise   = z - 1

instance OEIS 132679 where
  oeis = f $ S.fromList [1,2] where
     f s = m : f (S.insert (4*m) $ S.insert (4*m+3) s') where
         (m,s') = S.deleteFindMin s

instance OEIS 132741 where
  oeisIx = f 2 1 . succ where
     f p y x | r == 0    = f p (y * p) x'
             | otherwise = if p == 2 then f 5 y x else y
             where (x', r) = divMod x p

instance OEIS 133280 where
  oeis = tablList @133280
instance Table 133280 where
  tabl = f 0 1 [0..] where
     f m j xs = (filter ((== m) . (`mod` 2)) ys) : f (1 - m) (j + 2) xs'
       where (ys,xs') = splitAt j xs

instance OEIS 133622 where
  -- oeisIx n = (1 - m) * n' + 1 where (n', m) = divMod n 2
  oeis = concat $ transpose [[1, 1 ..], [2 ..]]

instance OEIS 134636 where
  oeis = tablList @134636
instance Table 134636 where
  tabl = iterate (\row -> zipWith (+) ([2] ++ row) (row ++ [2])) [1]

instance OEIS 134736 where
  oeis = 5 : zipWith (+) (oeis @134736) (zipWith gcd (oeis @134736) [2..])

instance OEIS 135287 where
  oeis = 1 : f 1 1 where
     f x y = z : f (x + 1) z where
          z = if m == 0 then y' else x + y; (y',m) = divMod y 2

instance OEIS 135851 where
  oeis = -1 : 0 : 1 : zipWith (+) (oeis @135851) (drop 2 (oeis @135851))

instance OEIS 136333 where
  oeis = filter (null . intersect "024568" . show . fi) [1..]

instance OEIS 136392 where
  oeisIx (succ->n) = 2 * n * (3*n - 5) + 5

instance OEIS 136399 where
  oeis = filter (any (> '1') . show . fi) [0..]

instance OEIS 136409 where
  oeisIx = floor . (* logBase 3 2) . fi

instance OEIS 136412 where
  oeisIx = (`div` 3) . (+ 1) . (* 5) . (4 ^)

instance OEIS 136431 where
  oeis = tablList @136431
instance Table 136431 where
  tabl = map fst $ iterate h ([0], 1) where
     h (row, fib) = (zipWith (+) ([0] ++ row) (row ++ [fib]), last row)

instance OEIS 136572 where
  oeis = tablList @136572
instance Table 136572 where
  tabl = map fst $ iterate f ([1], 1) where
     f (row, i) = (0 : map (* i) row, i + 1)

instance OEIS 137564 where
  oeisIx = f (-1) where
     f _ 0 = 0
     f r x = if d == r then f r x' else 10 * f d x' + d
             where (x', d) = divMod x 10

instance OEIS 137688 where
  oeis = concat $ zipWith ($) (map replicate [1..]) (map (2^) [0..])

instance OEIS 137921 where
  oeisIx (succ->n) = genericLength $ filter (> 0) $
     map ((mod n) . (+ 1)) [d | d <- [1..n], mod n d == 0]

instance OEIS 139080 where
  oeis = 1 : f 1 [2..] where
    f x zs = g zs where
      g (y:ys) = if x < y && y `div` x == 2 || x `div` y == 2
        then y : f y (delete y zs) else g ys

instance OEIS 139351 where
  oeisIx = sum . map (`mod` 2) .
     unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4))

instance OEIS 139352 where
  oeisIx = sum . map ((`div` 2) . (`mod` 4)) .
     unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4))

instance OEIS 140081 where
  oeisIx n = div (mod n 4 + mod n 2) 2
  oeis = cycle [0, 1, 1, 2]

instance OEIS 140436 where
  oeis = map (maximum . map length . group . sort . map product) $
                     tail pss where
     pss = [] : map p [1..]
     p u = [u] : [v : ps | v <- [1..u], ps <- pss !! (u - v), v <= head ps]

instance OEIS 140690 where
  oeis = f $ S.singleton (1, 1, 2) where
     f s | k == 1 = m : f (S.insert (2*b- 1, 1, 2*b) $ S.insert (b*m, k+1, b) s')
         | even k    = m : f (S.insert (b*m+b- 1, k+1, b) s')
         | otherwise = m : f (S.insert (b*m, k+1, b) s')
         where ((m, k, b), s') = S.deleteFindMin s

instance OEIS 141046 where
  oeisIx = (* 4) . (^ 4)

instance OEIS 143207 where
  oeis = f (S.singleton (2*3*5)) where
     f s = m : f (S.insert (2*m) $ S.insert (3*m) $ S.insert (5*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 143683 where
  oeis = tablList @143683
instance Table 143683 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) (map (* 8) ([0] ++ us ++ [0])) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

instance OEIS 143689 where
  oeisIx n = n* (3*n - 1) `div` 2 + 1

instance OEIS 144299 where
  oeis = tablList @144299
instance Table 144299 where
  tabl = [1] : [1, 0] : f 1 [1] [1, 0] where
     f i us vs = ws : f (i + 1) vs ws where
                 ws = (zipWith (+) (0 : map (i *) us) vs) ++ [0]

instance OEIS 144331 where
  oeis = tablList @144331
instance Table 144331 where
  tabf = iterate (\xs ->
    zipWith (+) ([0] ++ xs ++ [0]) $ zipWith (*) (0:[0..]) ([0,0] ++ xs)) [1]

instance OEIS 144396 where
  oeisIx = (+ 1) . (* 2) . succ
  oeis = [3, 5 ..]

instance OEIS 144944 where
  oeis = tablList @144944
instance Table 144944 where
  tabl = iterate f [1] where
     f us = vs ++ [last vs] where
       vs = scanl1 (+) $ zipWith (+) us $ [0] ++ us

instance OEIS 145812 where
  oeis = filter f [1, 3 ..] where
     f v = v == 0 || even w && f w where w = v `div` 4

instance OEIS 147583 where
  oeisIx = p [1..] . succ where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p [5 * k ..] (m - k) + p ks m

instance OEIS 147991 where
  oeis = f $ S.singleton 1 where
     f s = m : (f $ S.insert (3*m - 1) $ S.insert (3*m + 1) s')
           where (m, s') = S.deleteFindMin s

instance OEIS 152815 where
  oeis = tablList @152815
instance Table 152815 where
  tabl = [1] : [1,0] : t [1,0] where
     t ys = zs : zs' : t zs' where
       zs' = zs ++ [0]; zs = zipWith (+) ([0] ++ ys) (ys ++ [0])

instance OEIS 152842 where
  oeis = tablList @152842
instance Table 152842 where
  tabl = map fst $ iterate f ([1], 3) where
     f (xs, z) = (zipWith (+) ([0] ++ map (* z) xs) (xs ++ [0]), 4 - z)

instance OEIS 155161 where
  oeis = tablList @155161
instance Table 155161 where
  tabl = [1] : [0,1] : f [0] [0,1] where
     f us vs = ws : f vs ws where
       ws = zipWith (+) (us ++ [0,0]) $ zipWith (+) ([0] ++ vs) (vs ++ [0])

instance OEIS 156301 where
  oeisIx = ceiling . (* logBase 3 2) . fi

instance OEIS 157671 where
  oeis = filter ((== 2) . until (< 3) (flip div 3)) [1..]

instance OEIS 157962 where
  oeis = concat $ map (t 1 {- January -}) [0..] where
     t 13 _                       = []
     t m n | h (n+2000) m 13 == 6 = m : n : t (succ m) n
           | otherwise            = t (succ m) n
     h year month day
          | month <= 2 = h  (year - 1)  (month + 12)  day
          | otherwise  = (day + 26 * (month + 1) `div` 10 + y + y `div` 4
                         + century `div` 4 - 2 * century) `mod` 7
            where (century, y) = divMod year 100

instance OEIS 158034 where
  oeis = [x | x <- [1..], (4^x - 2^x + 8*x^2 - 2) `mod` (2*x* (2*x + 1)) == 0]

instance OEIS 158459 where
  oeisIx = (`mod` 4) . negate
  oeis = cycle [0,3,2,1]

instance OEIS 158478 where
  oeisIx n = if n < 4 then n else 2 + mod n 2
  oeis = [0..3] ++ cycle [2,3]

instance OEIS 159684 where
  oeis = 0 : concat (iterate (concatMap s) [1])
     where s 0 = [0,1]; s 1 = [0,1,0]

instance OEIS 160239 where
  oeis = 1 : (concat $
     transpose [a8, hs, zipWith (+) (map (* 2) hs) a8, tail (oeis @160239)])
        where a8 = map (* 8) (oeis @160239);
              hs = h (oeis @160239); h (_:x:xs) = x : h xs

instance OEIS 160256 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v ws = g ws where
       g (x:xs) | mod (x * v) u == 0 = x : f v x (delete x ws)
                | otherwise          = g xs

instance OEIS 160380 where
  oeisIx = sum . map ((0 ^ ) . (`mod` 4)) .
     unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4))

instance OEIS 160385 where
  oeisIx = sum . map (signum . (`mod` 4)) .
    unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4))

instance OEIS 161382 where
  oeis = concatMap (\x -> genericReplicate (x ^ 2) (1 - mod x 2)) [1..]

instance OEIS 161390 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [0, 5, 4, 2, 9, 8, 6, 7, 3, 1]

instance OEIS 161896 where
  oeis = [x | x <- [1..], (9^x - 3*3^x - 4*x) `mod` (2*x* (2*x + 1)) == 0]

instance OEIS 162608 where
  oeis = tablList @162608
instance Table 162608 where
  tabl = map fst $ iterate f ([1], 1) where
     f (row, n) = (row' ++ [head row' + last row'], n + 1) where
       row' = map (* n) row

instance OEIS 163575 where
  oeisIx (succ->n) = f n' where
     f 0 = 0
     f x = if b == parity then f x' else x  where (x', b) = divMod x 2
     (n', parity) = divMod n 2

instance OEIS 163617 where
  oeisIx (fi->n) = fi do n .|. shiftL n 1 :: Integer

instance OEIS 164632 where
  oeis = 1 : concatMap (\x -> genericReplicate (2^ (2*x - 1)) (2^x)) [1..]

instance OEIS 165900 where
  oeisIx n = n * (n - 1) - 1

instance OEIS 166060 where
  oeis = map fst $ iterate (\ (u, v) -> (3 * (u + v), 2 * v)) (1, 1)

instance OEIS 168046 where
  oeisIx = fi . fromEnum . ch0 where
     ch0 x = x > 0 && (x < 10 || d > 0 && ch0 x') where (x', d) = divMod x 10

instance OEIS 168183 where
  oeis = [1..8] ++ map (+ 9) (oeis @168183)

instance OEIS 168184 where
  oeisIx = (1 -) . (0 ^) . (`mod` 10)
  oeis = cycle [0,1,1,1,1,1,1,1,1,1]

instance OEIS 169810 where
  oeisIx (fi->n) = fi do n ^ 2 `xor` n :: Integer

instance OEIS 169837 where
  oeis = 3 : ekg 3 (2 : [4..]) where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 169849 where
  oeis = 9 : ekg 9 (delete 9 [2..]) where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 171946 where
  oeis = 0 : f [2..] where
     f (w:ws) = w : f (delete (2 * w - 1) ws)

instance OEIS 171947 where
  oeis = 1 : f [2..] where
     f (w:ws) = y : f (delete y ws) where y = 2 * w - 1

instance OEIS 171974 where
  oeisIx = floor . (/ 3) . (* sqrt 6) . fi . succ

instance OEIS 171975 where
  oeisIx = floor . (/ 4) . (* sqrt 6) . fi . succ

instance OEIS 173964 where
  oeis = concat $ [1] : f [[1]] where
     f xss = yss ++ f yss where
       yss = [y] : map (++ [y]) xss
       y = head (head xss) + 1

instance OEIS 174452 where
  oeisIx = (`mod` 1000) . (^ 2)

instance OEIS 174813 where
  oeis = f [1] where
     f ds = foldr (\d v -> 10 * v + d) 0 ds : f (s ds)
     s [] = [1]; s (9:ds) = 1 : s ds; s (d:ds) = 3*d : ds

instance OEIS 175332 where
  oeis = f $ S.singleton 3 where
    f s = x : f (if even x then S.insert z s' else S.insert z $ S.insert (z+1) s')
          where z = 2*x; (x, s') = S.deleteFindMin s

instance OEIS 175498 where
  oeis = 1 : f 1 [2..] [] where
     f x zs ds = g zs where
       g (y:ys) | diff `elem` ds = g ys
                | otherwise      = y : f y (delete y zs) (diff:ds)
                where diff = y - x

instance OEIS 175880 where
  oeis = 1 : f [2..] [2..] where
     f (x:xs) (y:ys) | x == y    = x : (f xs $ delete (2*x) ys)
                     | otherwise = 0 : (f xs (y:ys))

instance OEIS 175885 where
  oeis = 1 : 10 : map (+ 11) (oeis @175885)

instance OEIS 175886 where
  oeis = 1 : 12 : map (+ 13) (oeis @175886)

instance OEIS 175887 where
  oeis = 1 : 14 : map (+ 15) (oeis @175887)

instance OEIS 176059 where
  oeisIx = (3 -) . (`mod` 2)

instance OEIS 178788 where
  oeisIx (fi->n) = fi .fromEnum $ nub (show n) == show n

instance OEIS 178830 where
  oeisIx 0 = 0
  oeisIx (succ->n) = z [1..n] (n * (n + 1) `div` 2) 1 where
     z []     s p             = fi $ fromEnum (s == p)
     z (x:xs) s p | s > p     = z xs (s - x) (p * x) + z xs s p
                  | otherwise = fi $ fromEnum (s == p)

instance OEIS 179051 where
  oeisIx = p 1 where
     p _ 0 = 1
     p k m = if m < k then 0 else p k (m - k) + p (k * 10) m

instance OEIS 179070 where
  oeis = 1 : zs where zs = 1 : 1 : 3 : zipWith (+) zs (drop 2 zs)

instance OEIS 179888 where
  oeis = 2 : f (oeis @179888) where
    f (x:xs) = x' : x'' : f (xs ++ [x',x'']) where x' = 4*x+1; x'' = x' + 1

instance OEIS 180410 where
  oeisIx = fi . read . sort . nub . show . fi . succ

instance OEIS 181391 where
  oeis = 0 : (unfoldr g [0]) where
     g xs = Just (m, m : xs) where
          m = 1 + fromMaybe (-1) (findIndex (== head xs) $ tail xs)

instance OEIS 181482 where
  oeis = scanl1 (+) $ zipWith (*) [1..] $ cycle [1, 1, -1]

instance OEIS 181753 where
  oeis = concat $ iterate
                 (map ((+ 1) . flip mod 8 . (+ 4))) [1,3,5,6,7,2,5]

instance OEIS 181765 where
  oeisIx n = genericLength [xs | xs <- subsequences [-n..n], sum xs > 0]

instance OEIS 181935 where
  oeisIx 0 = 1
  oeisIx n = curling $ unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) n where
     curling zs = maximum $ zipWith (\xs ys -> strip 1 xs ys)
                            (tail $ inits zs) (tail $ tails zs) where
        strip i us vs | vs' == Nothing = i
                      | otherwise      = strip (i + 1) us $ fromJust vs'
                      where vs' = stripPrefix us vs

instance OEIS 182323 where
  oeis = filter ((== 43) . (`mod` 97) . (^ 2)) [0..]

instance OEIS 182455 where
  oeis = 1 : zipWith (*) (zipWith mod (oeis @182455) [3..]) [3..]

instance OEIS 182972 where
  oeis = map fst $ concatMap q [3..] where
     q x = [ (num, den) | num <- [1 .. div x 2],
                         let den = x - num, gcd num den == 1]

instance OEIS 182973 where
  oeis = map snd $ concatMap q [3..] where
     q x = [ (num, den) | num <- [1 .. div x 2],
                         let den = x - num, gcd num den == 1]

instance OEIS 185549 where
  oeisIx = ceiling . (** (3 / 2)) . fi

instance OEIS 185670 where
  oeisIx (fi->succ->n) = genericLength [ (x,y) | x <- [1..n - 1], y <- [x+1..n], gcd x y > 1]

instance OEIS 185869 where
  oeis = scanl (+) 2 $ a' 1
    where  a' n = 2 * n + 3 : replicate n 2 ++ a' (n + 1)

instance OEIS 185950 where
  oeisIx n = (4 * n - 1) * n - 1

instance OEIS 186421 where
  oeis = interleave [0,2..] $ rep [1,3..] where
     interleave (x:xs) ys = x : interleave ys xs
     rep (x:xs) = x : x : rep xs

instance OEIS 186809 where
  oeis = cycle [0, 1, 2, 0, -2, -1]

instance OEIS 187202 where
  oeisIx = head . head . dropWhile ((> 1) . length) . iterate diff . divs . succ
     where divs n = filter ((== 0) . mod n) [1..n]
           diff xs = zipWith (-) (tail xs) xs

instance OEIS 187203 where
  oeisIx = head . head . dropWhile ((> 1) . length) . iterate diff . divs . succ
     where divs n = filter ((== 0) . mod n) [1..n]
           diff xs = map abs $ zipWith (-) (tail xs) xs

instance OEIS 187844 where
  oeisIx n = product $ map (negate . fi . digitToInt) $ show $ fi n

instance OEIS 187921 where
  oeis = r (S.singleton 0) 1 0 where
     r s n x | x <= n           = n : r (S.insert (x+n) s) (n+1) (x+n)
             | (x-n) `S.member` s = r (S.insert (x+n) s) (n+1) (x+n)
             | otherwise        = r (S.insert (x-n) s) (n+1) (x-n)

instance OEIS 187922 where
  oeis = r (S.singleton 0) 1 0 where
     r s n x | x <= n           = r (S.insert (x+n) s) (n+1) (x+n)
             | (x-n) `S.member` s = n : r (S.insert (x+n) s) (n+1) (x+n)
             | otherwise        = r (S.insert (x-n) s) (n+1) (x-n)

instance OEIS 188172 where
  oeisIx (succ->fi->n) = genericLength $ filter ((== 0) . mod n) [7,15..n]

instance OEIS 188386 where
  oeis = map numerator $ zipWith (-) (drop 3 hs) hs
     where hs = 0 : scanl1 (+) (map (1 %) [1..])

instance OEIS 188967 where
  oeis = 0 : zipWith ($)
                     (cycle [ (1 -) . (oeisIx @188967 . pred), (1 -) . (oeisIx @188967 . pred), (oeisIx @188967 . pred)])
                     (concat $ transpose [[1, 3 ..], [2, 4 ..], [2 ..]])

instance OEIS 189144 where
  oeisIx n = (foldl1 lcm [n..n+6]) `div` 420

instance OEIS 190600 where
  oeisIx = fi . digitToInt . maximum . flip (showIntAtBase 12 intToDigit) "" . fi

instance OEIS 190803 where
  oeis = 1 : f (S.singleton 2)
     where f s = m : (f $ S.insert (2*m - 1) $ S.insert (3*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190805 where
  oeis = 1 : f (S.singleton 4)
     where f s = m : (f $ S.insert (2*m - 1) $ S.insert (3*m+1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190806 where
  oeis = 1 : f (S.singleton 5)
     where f s = m : (f $ S.insert (2*m - 1) $ S.insert (3*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190807 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (2*m) $ S.insert (3*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190808 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (2*m) $ S.insert (3*m+1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190809 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (2*m) $ S.insert (3*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190810 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (2*m+1) $ S.insert (3*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190811 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (2*m+1) $ S.insert (3*m) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 190812 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (2*m+1) $ S.insert (3*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191113 where
  oeis = 1 : f (S.singleton 2)
     where f s = m : (f $ S.insert (3*m - 2) $ S.insert (4*m- 2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191114 where
  oeis = 1 : f (S.singleton 3)
     where f s = m : (f $ S.insert (3*m - 2) $ S.insert (4*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191115 where
  oeis = 1 : f (S.singleton 4)
     where f s = m : (f $ S.insert (3*m - 2) $ S.insert (4*m) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191116 where
  oeis = 1 : f (S.singleton 5)
     where f s = m : (f $ S.insert (3*m - 2) $ S.insert (4*m+1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191117 where
  oeis = 1 : f (S.singleton 6)
     where f s = m : (f $ S.insert (3*m - 2) $ S.insert (4*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191118 where
  oeis = 1 : f (S.singleton 7)
     where f s = m : (f $ S.insert (3*m - 2) $ S.insert (4*m+3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191119 where
  oeis = 1 : f (S.singleton 2)
     where f s = m : (f $ S.insert (3*m - 1) $ S.insert (4*m- 3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191120 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m - 1) $ S.insert (4*m- 2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191121 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m - 1) $ S.insert (4*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191122 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m - 1) $ S.insert (4*m) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191123 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m - 1) $ S.insert (4*m+1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191124 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m - 1) $ S.insert (4*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191125 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m- 1) $ S.insert (4*m+3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191126 where
  oeis = 1 : f (S.singleton 3)
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m- 3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191127 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m- 2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191128 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191129 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m+1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191130 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191131 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m+3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191132 where
  oeis = 1 : f (S.singleton 4)
     where f s = m : (f $ S.insert (3*m+1) $ S.insert (4*m- 3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191133 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+1) $ S.insert (4*m- 2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191134 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+1) $ S.insert (4*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191135 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+1) $ S.insert (4*m) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191136 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+1) $ S.insert (4*m+1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191137 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+1) $ S.insert (4*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191138 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+1) $ S.insert (4*m+3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191139 where
  oeis = 1 : f (S.singleton 5)
     where f s = m : (f $ S.insert (3*m+2) $ S.insert (4*m- 3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191140 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+2) $ S.insert (4*m- 2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191141 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+2) $ S.insert (4*m- 1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191142 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+2) $ S.insert (4*m) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191143 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+2) $ S.insert (4*m+1) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191144 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+2) $ S.insert (4*m+2) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191145 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m+2) $ S.insert (4*m+3) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 191203 where
  oeis = f $ S.singleton 1 where
     f s = m : f (S.insert (2 * m) $ S.insert (m ^ 2 + 1) s')
           where (m, s') = S.deleteFindMin s

instance OEIS 191211 where
  oeis = f $ S.singleton 1 where
     f s = m : f (S.insert (2 * m + 1) $ S.insert (m ^ 2 + 1) s')
           where (m, s') = S.deleteFindMin s

instance OEIS 192476 where
  oeis = f [1] (S.singleton 1) where
     f xs s =
       m : f xs' (foldl (flip S.insert) s' (map (+ m^2) (map (^ 2) xs')))
       where xs' = m : xs
             (m,s') = S.deleteFindMin s

instance OEIS 192489 where
  oeis = f 2 1 where
     f n x | x' == 2   = n : f (n+1) x'
           | otherwise = f (n+1) x'
           where x' = 1 + gcd n x

instance OEIS 192687 where
  oeis = zipWith (-) females males where
     females = 1 : zipWith (-) [1..] (map (males !!) females)
     males = 0 : zipWith (-) [1..] (map (females !!) males)

instance OEIS 193238 where
  oeisIx n = genericLength $ filter (`elem` "2357") $ show $ fi n

instance OEIS 193641 where
  oeis = drop 2 xs where
     xs = 1 : 1 : 1 : zipWith (+) xs (map (* 2) $ drop 2 xs)

instance OEIS 193773 where
  oeisIx n = genericLength [ () | x <- [1 .. n + 1],
                           let (y,m) = divMod (x + n) (2 * x - 1),
                           x <= y, m == 0]

instance OEIS 193891 where
  oeis = tablList @193891
instance Table 193891 where
  tabl = [1] : map fst (iterate
     (\ (xs, i) -> (zipWith (+) (0:xs) [i, 2 * i ..], i + 1)) ([1,2], 2))

instance OEIS 194005 where
  oeis = tablList @194005
instance Table 194005 where
  tabl = [1] : [1,1] : f [1] [1,1] where
     f row' row = rs : f row rs where
       rs = zipWith (+) ([0,1] ++ row') (row ++ [0])

instance OEIS 195013 where
  oeis = concat $ transpose [[2, 4 ..], [3, 6 ..]]

instance OEIS 195150 where
  oeisIx (succ->fi->n) = genericLength [d | d <- [3..n], mod n d == 0, mod n (d - 1) /= 0]

instance OEIS 195437 where
  oeis = tablList @195437
instance Table 195437 where
  tabl = tail $ g 1 1 [0..] where
     g m j xs = (filter ((== m) . (`mod` 2)) ys) : g (1 - m) (j + 2) xs'
       where (ys,xs') = splitAt j xs
  -- b195437 = bFile' "A195437" (concat $ take 101 (tabl @195437)) 0

instance OEIS 195691 where
  oeisIx = normal True 0 where
    normal qLam k n = if n<2 then 0 else
      (if n - 2<k then 1 else 0) +
      (if qLam then normal True (k+1) (n - 2) else 0) +
      sum [normal False k i * normal True k (n - 2 - i) | i <- [0..n - 2]]

instance OEIS 196032 where
  oeis = filter f [1..] where
     f 0 = False; f x = m == 0 || f x' where (x',m) = divMod x 4

instance OEIS 196168 where
  oeisIx 0 = 1
  oeisIx n = foldl (\v b -> (2 * v + 1)* (b + 1)) 0 $ reverse $ unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) n
     where r v b = (2 * v + 1)* (b+1)

instance OEIS 196189 where
  oeis = f 1 [] where
     f n xs = x : f (n+1) (x:xs) where
       x = head [z | z <- [n+2..], z `notElem` xs, gcd z n == 1]

instance OEIS 196199 where
  oeis = tablList @196199
instance Table 196199 where
  tabf = map (rowT @196199) [0..]
  rowT n = [-n..n]
  -- b196199 = bFile' "A196199" (concat $ take 101 (tabf @196199)) 0

instance OEIS 196368 where
  oeisIx (fi->n) = fi . fromEnum $ and $ zipWith (/=) (tail $ show n) (show n)

instance OEIS 196563 where
  oeisIx (fi->n) = genericLength [d | d <- show n, d `elem` "02468"]

instance OEIS 196564 where
  oeisIx (fi->n) = genericLength [d | d <- show n, d `elem` "13579"]

instance OEIS 198069 where
  oeis = tablList @198069
instance Table 198069 where
  tabf = [1] : iterate f [1, 1] where
     f (x:xs) = ys ++ tail (reverse ys) where ys = (2 * x) : xs

instance OEIS 198388 where
  oeis = map (\ (x,_,_) -> x) ts where
     ts = [ (u,v,w) | w <- [1..], v <- [1..w - 1], u <- [1..v - 1],
                     w^2 - v^2 == v^2 - u^2]

instance OEIS 198389 where
  oeis = map (\ (_,x,_) -> x) ts where
     ts = [ (u,v,w) | w <- [1..], v <- [1..w - 1], u <- [1..v - 1],
                     w^2 - v^2 == v^2 - u^2]

instance OEIS 198390 where
  oeis = map (\ (_,_,x) -> x) ts where
     ts = [ (u,v,w) | w <- [1..], v <- [1..w - 1], u <- [1..v - 1],
                     w^2 - v^2 == v^2 - u^2]

instance OEIS 199162 where
  oeis = 1 : 6 : ulam 2 6 (oeis @199162)

instance OEIS 199682 where
  oeisIx = (+ 1) . (* 2) . (10 ^)

instance OEIS 199799 where
  oeis = [x | x <- [1..111111], gcd x 111111 == 1]

instance OEIS 200745 where
  oeisIx n = p [nd | nd <- [1..n], mod n nd /= 0] n where
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m | m < k = 0 | otherwise = p ks (m - k) + p ks m

instance OEIS 201634 where
  oeis = tablList @201634
instance Table 201634 where
  tabl = iterate (\xs -> scanl1 (+) xs ++ [2 * last xs]) [1]

instance OEIS 202138 where
  oeis = [17, 78, 19, 23, 29, 77, 95, 77, 1, 11, 13, 15, 1, 55]

instance OEIS 203363 where
  oeis = [91, 85, 51, 38, 33, 29, 23, 19, 17, 13, 11, 2, 7, 1]

instance OEIS 204293 where
  oeis = tablList @204293
instance Table 204293 where
  tabl = [1] : [0,0] : f [1] [0,0] where
     f xs ys = xs' : f ys xs' where
       xs' = zipWith (+) ([0,0] ++ xs) (xs ++ [0,0])

instance OEIS 204457 where
  oeis = [x | x <- [1, 3 ..], mod x 13 > 0]

instance OEIS 204674 where
  oeisIx n = n * (n * (4 * n + 5) + 2) + 1

instance OEIS 204675 where
  oeisIx n = 2 * n * (8 * n + 1) + 1

instance OEIS 206282 where
  oeis = 1 : 1 : -1 : -4 :
     zipWith div
       (zipWith (+)
         (zipWith (*) (drop 3 (oeis @206282))
                      (drop 1 (oeis @206282)))
         (drop 2 (oeis @206282)))
       (oeis @206282)

instance OEIS 208101 where
  oeis = tablList @208101
instance Table 208101 where
  tabl =  iterate
     (\row -> zipWith (+) ([0,1] ++ init row) (row ++ [0])) [1]

instance OEIS 209037 where
  oeisIx n = p (max 1 (n `div` 3)) n where
     p _ 0 = 1
     p k m = if m < k then 0 else p k (m - k) + p (k + 1) m

instance OEIS 209038 where
  oeisIx n = p (max 1 (n `div` 4)) n where
     p _ 0 = 1
     p k m = if m < k then 0 else p k (m - k) + p (k + 1) m

instance OEIS 209039 where
  oeisIx n = p (max 1 (n `div` 5)) n where
     p _ 0 = 1
     p k m = if m < k then 0 else p k (m - k) + p (k + 1) m

instance OEIS 209229 where
  oeisIx n | n < 2 = n
           | n > 1 = if m > 0 then 0 else (oeisIx @209229) n'
           where (n',m) = divMod n 2

instance OEIS 210770 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v (w:ws) = u' : w : f u' w (delete u' ws) where u' = v + w

instance OEIS 211866 where
  oeisIx = (flip div 4) . (subtract 5) . (9 ^) . succ

instance OEIS 212193 where
  oeisIx n = f n [0..3] where
     f x ys | x <= 2    = head $ delete x ys
            | otherwise = f x' $ delete d ys where (x',d) = divMod x 3

instance OEIS 212306 where
  oeis = f [1..] where
     f (x:xs) = x : f ((map (subtract x) us) ++ vs)
                where (us, vs) = splitAt x xs

instance OEIS 212721 where
  oeis = tablList @212721
instance Table 212721 where
  rowCol n k = (rowT @212721) n !! (k- 1)
  rowT = nub . sort . (map product) . ps 1 where
     ps x 0 = [[]]
     ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]
  tabf = map (rowT @212721) [0..]

instance OEIS 213541 where
  oeisIx (fi->n) = fi do n .&. n ^ 2 :: Int

instance OEIS 213967 where
  oeis = 0 : xs where
                 xs = 1 : 2 : 3 : map (+ 1)
                      (zipWith3 (((+) .) . (+)) xs (tail xs) (drop 2 xs))

instance OEIS 213998 where
  oeis = tablList @213998
instance Table 213998 where
  tabl = map (map numerator) $ iterate pf [1] where
     pf row = zipWith (+) ([0] ++ row) (row ++ [-1 % (x * (x + 1))])
              where x = denominator $ last row

instance OEIS 213999 where
  oeis = tablList @213999
instance Table 213999 where
  tabl = map (map denominator) $ iterate pf [1] where
     pf row = zipWith (+) ([0] ++ row) (row ++ [-1 % (x * (x + 1))])
              where x = denominator $ last row

instance OEIS 214085 where
  oeisIx n = n^2 * (n^4 - n^2 + n + 1) `div` 2

instance OEIS 214551 where
  oeis = 1 : 1 : 1 : zipWith f (oeis @214551) (drop 2 (oeis @214551))
     where f u v = (u + v) `div` gcd u v

instance OEIS 214659 where
  oeisIx n = ((7 * n - 3) * n - 1) * n `div` 3

instance OEIS 214660 where
  oeisIx (succ->n) = (9 * n - 11) * n + 3

instance OEIS 214675 where
  oeisIx (succ->n) = (9 * n - 13) * n + 5

instance OEIS 214803 where
  oeis = [x * y - x - y | y <- [1..], x <- [1..y - 1], gcd x y == 1]

instance OEIS 214832 where
  oeisIx = floor . (* 440) . (2 **) . (/ 12) . fi . subtract 49 . succ

instance OEIS 214949 where
  oeisIx = f 0 where
     f y 0 = numerator y
     f y x = f (y + if d == 0 then 0 else 1 % d) x'
             where (x',d) = divMod x 10

instance OEIS 214950 where
  oeisIx = f 0 where
     f y 0 = denominator y
     f y x = f (y + if d == 0 then 0 else 1 % d) x'
             where (x',d) = divMod x 10

instance OEIS 214959 where
  oeis = [x | x <- [0..], f x 0] where
     f 0 v = numerator v == 1 && denominator v == 1
     f u v | d > 0     = f u' (v + 1 % d)
           | otherwise = f u' v  where (u',d) = divMod u 10

instance OEIS 215403 where
  oeis = map (foldr (\d v -> 10 * v + d) 0) $
                     concatMap (\x -> map (x :) [plut', nept']) [4..] where
    plut' = [1,1,2,2,2,3,2,1,1,2,1,1,2,2,2,1,2,2,2,3,1,1,2,2,1,3]
    nept' = [1,1,2,2,3,3,1,2,1,1,2,2,1,1,2,2,3,1,1,2,3,3,1,1,2,2,2,1,1,3,1]
instance Table 215403 where
  rowCol n k = (oeis @215403) !! (n - 1)

instance OEIS 215646 where
  oeisIx n = n * (n * (11*n + 6) + 1) `div` 6

instance OEIS 215973 where
  oeis = 1 : f [1] where
     f xs = y : f (y:xs) where
       y = sum $ zipWith (*) xs $ map (+ 1) $ reverse xs

instance OEIS 216151 where
  oeis = 1 : 2 : f 2 3 where
     f u v = w : f (u * w) (v + w) where w = u * v

instance OEIS 217843 where
  oeis = f (S.singleton (0, (0,0))) (-1) where
     f s z = if y /= z then y : f s'' y else f s'' y
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 217928 where
  oeisIx = fi . (sum . nub . map (read . return) . show :: Integer -> Integer) . fi

instance OEIS 219054 where
  oeisIx (succ->n) = n * (n * (8 * n + 3) + 1) `div` 6

instance OEIS 219056 where
  oeisIx = (* 3) . (^ 4)

instance OEIS 219070 where
  oeisIx n = n * (n * (n * (n * (46 * n + 30) + 15)) - 1) `div` 30

instance OEIS 219092 where
  oeisIx 0 = 1
  oeisIx n = floor (exp 1 ** (fi n + 0.5))

instance OEIS 219907 where
  oeis = f 0 S.empty where
     f z s | S.null s || z' <= m = f (z + 1) (s `S.union` (S.fromList ws))
           | otherwise             = m : f z s'
           where (m,s') = S.deleteFindMin s
                 ws = map (h z) [0..z] ++ map (flip h z) [0..z- 1]
                 h i j = 4 * i ^ 2 + 2 * i * j + 7 * j ^ 2
                 z' = h z 0

instance OEIS 220376 where
  oeis = at 1 where
     at z | (reverse (show (z - 1)) `isPrefixOf` fst bird) = at (z + 1)
          | otherwise                = (length $ fst bird) : at (z + 1)
          where bird = fromJust $ find ((show z `isPrefixOf`) . snd) xys
     xys = iterate (\ (us, v : vs) -> (v : us, vs))
                   ([], concatMap show [0 ..])

instance OEIS 220811 where
  oeis = map numerator vs where
     vs = iterate (\x -> x * (4 - 3 * x)) (1 % 10)

instance OEIS 221172 where
  oeis = -2 : 3 : zipWith (+)
                          (map (* 2) $ tail (oeis @221172)) (oeis @221172)

instance OEIS 221173 where
  oeis = -3 : 4 : zipWith (+)
                          (map (* 2) $ tail (oeis @221173)) (oeis @221173)

instance OEIS 221174 where
  oeis = -4 : 5 : zipWith (+)
                          (map (* 2) $ tail (oeis @221174)) (oeis @221174)

instance OEIS 221175 where
  oeis = -5 : 6 : zipWith (+)
                          (map (* 2) $ tail (oeis @221175)) (oeis @221175)

instance OEIS 221639 where
  oeisIx n = 5 ^ (5 * n + 1) + 4 ^ (5 * n + 2) + 3 ^ (5 * n)

instance OEIS 221869 where
  oeis = f 2 7 (S.singleton 1) where
     f u v s | d `S.member` s = f (u + 1) (v + d) s
             | otherwise    = d : f (u + 1) (v + d) (d `S.insert` s)
             where d = gcd u v

instance OEIS 224791 where
  oeis = tablList @224791
instance Table 224791 where
  tabl = iterate
     (\row -> scanl1 (+) $ zipWith (+) ([1] ++ row) (row ++ [1])) [0]

instance OEIS 225693 where
  oeisIx = f 1 0 where
     f _ a 0 = a
     f s a x = f (negate s) (s * a + d) x' where (x', d) = divMod x 10

instance OEIS 225790 where
  oeisIx 0 = 1
  oeisIx (succ->n) = 12 ^ (n1 * n1) * 2 ^ (2 * n1 - 1) * k
    where
      n1 = div n 2
      k = if odd n then 4 else 1

instance OEIS 225985 where
  oeis = map (fi.read) $ filter (not . null) $
      map (filter (`elem` "13579") . show . fi) [0..]

instance OEIS 226134 where
  oeisIx = fi . (foldl (\v d -> 10*v+d) 0 . scanl1 (\d x -> (x+d) `mod` 10) .
            map (read . return) . show :: Int -> Int) . fi

instance OEIS 226203 where
  oeis = concat $ transpose [[1, 3 ..], [-3, -1 ..], [-1, 1 ..], [1, 3 ..], [1, 3 ..]]

instance OEIS 226452 where
  oeis = 1 : 2 : f [[0,0],[0,1],[1,0],[1,1]] where
     f bss = sum (map h bss) : f ((map (0 :) bss) ++ (map (1 :) bss)) where
     h bs = fi . fromEnum $ or $ zipWith
             (\xs ys -> xs == ys && not (xs `isInfixOf` (init $ tail bs)))
             (init $ inits bs) (reverse $ tails $ tail bs)

instance OEIS 227113 where
  oeis = 1 : f [2..] where
     f (x:xs) = x : y : f (delete y xs)
       where y : _ = filter ((> 1) . (gcd x)) xs

instance OEIS 227144 where
  oeis = [1,2,7,17,23] ++ map (+ 24) (oeis @227144)

instance OEIS 227146 where
  oeis = [5,11,13,14,19] ++ map (+ 24) (oeis @227146)

instance OEIS 227362 where
  oeisIx = fi . (read . reverse . sort . nub . show :: Integer -> Integer) . fi

instance OEIS 227426 where
  oeisIx = p 1 1 where
    p _ _ 0 = 1
    p k i m = if m < k then 0 else p (k + i) (3 - i) (m - k) + p (k + 1) 1 m

instance OEIS 227862 where
  oeis = tablList @227862
instance Table 227862 where
  tabl = map snd $ iterate ox (False, [1]) where
     ox (turn, xs) = (not turn, if turn then reverse ys else ys)
        where ys = scanl (+) 1 (if turn then reverse xs else xs)

instance OEIS 228074 where
  oeis = tablList @228074
instance Table 228074 where
  tabl = map fst $ iterate
     (\ (u:_, vs) -> (vs, zipWith (+) ([u] ++ vs) (vs ++ [1]))) ([0], [1,1])

instance OEIS 229037 where
  oeis = f 0 M.empty  where
     f i m = y : f (i + 1) (M.insert (i + 1) y m) where
       y = head [z | z <- [1..],
                     all (\k -> z + m M.! (i - k) /= 2 * m M.! (i - k `div` 2))
                         [1, 3 .. i - 1]]

instance OEIS 229762 where
  oeisIx (fi->n) = fi do (n `xor` shiftR n 1) .&. shiftR n 1 :: Int

instance OEIS 229763 where
  oeisIx (fi->n) = fi do (shiftL n 1 `xor` n) .&. n :: Int

instance OEIS 230089 where
  oeisIx (succ->n) = if odd n then n else if mod n 4 == 0 then 4 else 2

instance OEIS 230871 where
  oeis = tablList @230871
instance Table 230871 where
  tabf = [0] : map (map snd) (rows $ deleham (0, 1)) where
     rows (Dtree left (x, y) right) =
          [ (x, y)] : zipWith (++) (rows left) (rows right)
     deleham (x, y) = Dtree
             (deleham (y, y + x)) (x, y) (deleham (y, 3 * y - x))
data Dtree i = Dtree (Dtree i) (i, i) (Dtree i)

instance OEIS 230959 where
  oeisIx n = fi $ ((if null cds then 0 else read cds) :: Integer)
     where cds = "9876543210" \\ show (fi n)

instance OEIS 231429 where
  oeisIx n = p [1..n - 1] (2*n) where
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 231600 where
  oeisIx n = a 3 n where
     a s 0 = 0 ^ s
     a s x = a (t s b) x' where (x',b) = divMod x 2
     t 3 0 = 1; t 3 1 = 2; t 2 0 = 0; t 2 1 = 3; t 1 _ = 1; t 0 _ = 0

instance OEIS 231692 where
  oeis = map numerator $ 0 : wilson 1 0 where
     wilson x y = y' : wilson (x + 1) y'
                  where y' = y + (if y < 1 % x then 1 else -1) % x

instance OEIS 231693 where
  oeis = map denominator $ 0 : wilson 1 0 where
     wilson x y = y' : wilson (x + 1) y'
                  where y' = y + (if y < 1 % x then 1 else -1) % x

instance OEIS 232991 where
  oeisIx = (0 ^) . subtract 1 . gcd 6 . (+ 1)
  oeis = cycle [1,0,0,0,1,0]

instance OEIS 234959 where
  oeisIx = f 1 . succ where
     f y x
      | (x', 0) <- divMod x 6 = f (y * 6) x'
      | let                   = y

instance OEIS 235052 where
  oeisIx (succ->n) = head [x | x <- [2..], show (fi x) `isInfixOf` (show $ fi $ x ^ n)]

instance OEIS 235702 where
  oeisIx (succ->n) = if n == 1 then 1 else 24 * 5 ^ (n - 2)
  oeis = 1 : iterate (* 5) 24

instance OEIS 235715 where
  oeisIx 0 = 1
  oeisIx (succ->n) = f 1 ps 0 where
     f 0 (1 : xs) z = z
     f _ (x : xs) z = f x xs (z + 0 ^ (n - 1 - x))
     ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` n) (tail ps) ps

instance OEIS 235726 where
  oeisIx 0 = 1
  oeisIx 3 = 2
  oeisIx (succ->n) = head $ filter (`notElem` disallowedValues) [1..] where
    disallowedValues = map (oeisIx @235726.pred) $ (n - 1) : filter (<n) sums where
      sums = map divisorSum divisors where
        divisors = filter (\d -> n `mod` d == 0) [1..n]
        divisorSum d = d + n `div` d

instance OEIS 235933 where
  oeis = filter ((== 1) . gcd 35) [1..]

instance OEIS 236046 where
  oeisIx ((+2)->n) = head [x | x <- [2..], not $ show (fi x) `isInfixOf` (show $ fi $ x ^ n)]

instance OEIS 236076 where
  oeis = tablList @236076
instance Table 236076 where
  tabl = [1] : [0, 2] : f [1] [0, 2] where
     f us vs = ws : f vs ws where
       ws = [0] ++ zipWith (+) (zipWith (+) ([0] ++ us) (us ++ [0])) vs

instance OEIS 238324 where
  oeis = scanl1 (\u v -> if u > v then u - v else u + v) [1, 3 ..]

instance OEIS 239426 where
  oeisIx n = (((21 * n - 36) * n + 25) * n - 8) * n + 1

instance OEIS 239449 where
  oeisIx n = (7 * n - 5) * n + 1

instance OEIS 239728 where
  oeis = f 9 (3, 2) (M.singleton 4 (2, 2)) where
     f zz (bz, be) m
      | xx < zz && gcd 6 be > 1 =
                  f zz (bz, be+1) (M.insert (bx*xx) (bx, be+1) $ M.deleteMin m)
      | xx < zz = xx :
                  f zz (bz, be+1) (M.insert (bx*xx) (bx, be+1) $ M.deleteMin m)
      | xx > zz = f (zz+2*bz+1) (bz+1, 2) (M.insert (bz*zz) (bz, 3) m)
      | otherwise = f (zz + 2 * bz + 1) (bz + 1, 2) m
      where (xx, (bx, be)) = M.findMin m

instance OEIS 239870 where
  oeis = f 9 (3, 2) (M.singleton 4 (2, 2)) where
     f zz (bz, ez) m
      | xx < zz = if ex `mod` 3 > 0
        then xx : f zz (bz, ez+1) (M.insert (bx*xx) (bx, ex+1) $ M.deleteMin m)
        else      f zz (bz, ez+1) (M.insert (bx*xx) (bx, ex+1) $ M.deleteMin m)
      | xx > zz = if ez `mod` 3 > 0
        then zz : f (zz+2*bz+1) (bz+1, 2) (M.insert (bz*zz) (bz, 3) m)
        else      f (zz+2*bz+1) (bz+1, 2) (M.insert (bz*zz) (bz, 3) m)
      | otherwise = f (zz+2*bz+1) (bz+1, 2) m
      where (xx, (bx, ex)) = M.findMin m

instance OEIS 240857 where
  oeis = tablList @240857
instance Table 240857 where
  tabl = iterate (\ (x:xs) -> xs ++ [x, x + 1]) [0]

instance OEIS 241157 where
  oeis = 0 : filter f [0..] where
     f x = d' /= d where d' = mod x' 10; (x', d) = divMod x 10

instance OEIS 241158 where
  oeis = filter (f . show . fi)  [0..] where
     f [_] = True; f (d : d' : _) = d /= d'

instance OEIS 241751 where
  oeisIx = (+ 16) . (^ 2)

instance OEIS 241979 where
  oeis = cycle [0,1,1,0,0,0,1,0,0,1,1,1]

instance OEIS 242179 where
  oeis = tablList @242179
instance Table 242179 where
  tabf = iterate (concatMap (\x -> [-x, x])) [1]

instance OEIS 242217 where
  oeisIx = p [1,2,3,7,11,19,43,67,163] where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 242357 where
  oeis = concatMap f $ tail $ inits [1..] where
     f us = (init us) ++ (take v [v, v ..]) ++ vs
            where (v:vs) = reverse us

instance OEIS 242627 where
  oeisIx n = genericLength $ filter ((== 0) . mod n) [1..9]

instance OEIS 242885 where
  oeisIx (succ->n) = head [k | let nn = n ^ n, k <- [1..], mod (k ^ k + nn) (k + n) == 0]

instance OEIS 244040 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @244040) (2 * n') + t where (n', t) = divMod n 3

instance OEIS 245301 where
  oeisIx n = n * (n * (7 * n + 15) + 8) `div` 6

instance OEIS 245663 where
  oeisIx ((+2)->n) = head [k | k <- [1..], not ((fact k) `mod` (sigbAse (fact k) n) == 0)]
    where
      base 0 b = []
      base a b = (a `mod` b) : base ((a- (a `mod` b)) `div` b) b
      bAse a b = reverse (base a b)
      sigbAse a b = foldl (+) 0 (bAse a b)

instance OEIS 245677 where
  oeisIx ((+3)->n) = numerator $ sum
     [num % den | num <- [1 .. div n 2], let den = n - num, gcd num den == 1]

instance OEIS 245678 where
  oeisIx ((+3)->n) = denominator $ sum
     [num % den | num <- [1 .. div n 2], let den = n - num, gcd num den == 1]

instance OEIS 245826 where
  oeis = tablList @245826
instance Table 245826 where
  rowCol n k = n * k * (2 * n^2 * k^2 - n^2 - k^2) `div` 6
  rowT n = map (rowCol @245826 n) [1..n]
  tabl = map (rowT @245826) [1..]

instance OEIS 245940 where
  oeisIx n = n^3 * (2 * n^3 + 2 * n^2 - 3 * n - 1) * (n + 1) `div` 24

instance OEIS 245941 where
  oeisIx n = n * (16*n^5 - 24*n^4 + 2*n^3 + 11*n^2 - 6*n + 1) `div` 6

instance OEIS 246694 where
  oeis = tablList @246694
instance Table 246694 where
  tabl = [1] : [1,2] : f 1 2 [1,2] where
     f i z xs = ys : f j (z + 1) ys where
       ys = take (z + 1) $ map (+ 1) (xs !! (z - i) : xs !! (z - j) : ys)
       j = 3 - i

instance OEIS 247108 where
  oeis = tablList @247108
instance Table 247108 where
  tabl = iterate (\row -> scanl (+) (- last row) row) [1]

instance OEIS 247143 where
  oeis = map fi $ [0..10] ++ f 11 (map show [11..]) where
     f x zss = (read ys :: Int) : f (x + 1) (delete ys zss) where
               ys = fromJust $ find (elem $ ds !! x) zss
     ds = concatMap show (oeis @247143)

instance OEIS 247366 where
  oeis = h $ S.singleton (1, 0, 0) where
     h s = (floor x) : h (S.insert (f i (j + 1)) $ S.insert (f (i + 1) j) s')
           where ((x, i, j), s') = S.deleteFindMin s
     f u v = (2 ^^ uh * 3 ^^ vh * g ur vr, u, v) where
       g 0 0 = 1; g 0 1 = sqrt 3; g 1 0 = sqrt 2; g 1 1 = sqrt 6
       (uh, ur) = divMod u 2; (vh, vr) = divMod v 2

instance OEIS 247368 where
  oeis = 0 : xs where
     xs = [1, 1, 1, 1] ++ zipWith (flip div) xs (zipWith (+)
                (zipWith (*) (tail xs) (drop 3 xs))
                (zipWith (*) (cycle [1, -1]) (map (^ 2) $ drop 2 xs)))

instance OEIS 247369 where
  oeis = [0, -1, 1, 1, 1, 0] ++ xs where
     xs = [1, 1, 1, 3] ++ zipWith (flip div) xs (zipWith (+)
                (zipWith (*) (tail xs) (drop 3 xs))
                (zipWith (*) (cycle [1, -1]) (map (^ 2) $ drop 2 xs)))

instance OEIS 247370 where
  oeis = [1, 1, 0] ++ xs where
     xs = [1, 1, 1, 2] ++ zipWith (flip div) xs (zipWith (+)
                (zipWith (*) (tail xs) (drop 3 xs))
                (zipWith (*) (cycle [1, -1]) (map (^ 2) $ drop 2 xs)))

instance OEIS 247378 where
  oeis = [1, -2, 1, 1] ++ zipWith (flip div) (oeis @247378)
     (zipWith (+)
          (zipWith (*) (tail (oeis @247378)) (drop 3 (oeis @247378)))
          (zipWith (*) (cycle [1, -1]) (map (^ 2) $ drop 2 (oeis @247378))))

instance OEIS 247560 where
  oeis = 1 : 1 : zipWith (-) (map (* 3) $ tail (oeis @247560))
                                     (map (* 4) (oeis @247560))

instance OEIS 247563 where
  oeis = 2 : 3 : zipWith (-) (map (* 3) $ tail (oeis @247563))
                                     (map (* 4) (oeis @247563))

instance OEIS 247564 where
  oeis = [2,1,3,1] ++ zipWith (-) (map (* 3) $ drop 2 (oeis @247564))
                                          (map (* 4) $ (oeis @247564))

instance OEIS 247616 where
  oeis = filter f [100 .. 9876543210] where
     f x = head vs /= 0 && all (== 0) ws where
           ws = zipWith (-) (tail vs) vs
           vs = zipWith (-) (tail us) us
           us = map (read . return) $ show $ fi x

instance OEIS 247648 where
  oeis = f $ S.singleton 1 where
     f s = x : f (S.insert (4 * x + 1) $ S.insert (2 * x + 1) s')
           where (x, s') = S.deleteFindMin s

instance OEIS 247665 where
  oeis = 2 : 3 : f [3] [4..] where
     f (x:xs) zs = ys ++ f (xs ++ ys) (zs \\ ys) where
       ys = [v, head [w | w <- vs, gcd v w == 1]]
       (v:vs) = filter (\u -> gcd u x == 1 && all ((== 1) . (gcd u)) xs) zs

instance OEIS 247750 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [4, 9, 2, 1, 0, 8, 5, 7, 6, 3]

instance OEIS 247751 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [1, 5, 4, 9, 0, 8, 6, 7, 2, 3]

instance OEIS 247752 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [8, 1, 3, 9, 0, 2, 4, 5, 6, 7]

instance OEIS 247753 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [8, 2, 3, 6, 4, 0, 7, 5, 9, 1]

instance OEIS 247754 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [5, 2, 8, 9, 4, 7, 6, 3, 1, 0]

instance OEIS 247755 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [8, 3, 1, 5, 9, 0, 6, 7, 4, 2]

instance OEIS 247756 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [1, 3, 6, 7, 2, 9, 4, 0, 8, 5]

instance OEIS 247757 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [5, 2, 9, 8, 4, 6, 7, 3, 1, 0]

instance OEIS 247758 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [2, 9, 8, 4, 5, 6, 7, 3, 1, 0]

instance OEIS 247759 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [8, 1, 5, 4, 0, 6, 7, 9, 2, 3]

instance OEIS 247760 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [4, 2, 9, 1, 8, 5, 6, 7, 3, 0]

instance OEIS 247761 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [8, 2, 9, 0, 1, 5, 7, 3, 4, 6]

instance OEIS 247762 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
                  S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
           where (x, s') = S.deleteFindMin s
     digs = [9, 2, 1, 0, 8, 5, 7, 6, 4, 3]

instance OEIS 247764 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [6, 5, 1, 9, 4, 2, 8, 0, 3, 7]

instance OEIS 247796 where
  oeisIx = f 0 where
     f s 0 = s
     f s x = if s + d < 10 then f (s + d) x' else (f d x') * 10 + s
             where (x', d) = divMod x 10

instance OEIS 247800 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [4, 9, 2, 1, 0, 8, 5, 7, 6, 3]

instance OEIS 247801 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [1, 5, 4, 9, 0, 8, 6, 7, 2, 3]

instance OEIS 247802 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [8, 1, 3, 9, 0, 2, 4, 5, 6, 7]

instance OEIS 247803 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [8, 2, 3, 6, 4, 0, 7, 5, 9, 1]

instance OEIS 247804 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [5, 2, 8, 9, 4, 7, 6, 3, 1, 0]

instance OEIS 247805 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [8, 3, 1, 5, 9, 0, 6, 7, 4, 2]

instance OEIS 247806 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [1, 3, 6, 7, 2, 9, 4, 0, 8, 5]

instance OEIS 247807 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [5, 2, 9, 8, 4, 6, 7, 3, 1, 0]

instance OEIS 247808 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [2, 9, 8, 4, 5, 6, 7, 3, 1, 0]

instance OEIS 247809 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [8, 1, 5, 4, 0, 6, 7, 9, 2, 3]

instance OEIS 247810 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [4, 2, 9, 1, 8, 5, 6, 7, 3, 0]

instance OEIS 247811 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [8, 2, 9, 0, 1, 5, 7, 3, 4, 6]

instance OEIS 247812 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [9, 2, 1, 0, 8, 5, 7, 6, 4, 3]

instance OEIS 247813 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [0, 5, 4, 2, 9, 8, 6, 7, 3, 1]

instance OEIS 247814 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s | S.null s = []
         | otherwise  = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
         where (x, s') = S.deleteFindMin s
     digs = [6, 5, 1, 9, 4, 2, 8, 0, 3, 7]

instance OEIS 247875 where
  oeis = filter (\x -> even x || f x) [0..] where
     f x = x > 0 && (x `mod` 4 == 0 || f (x `div` 2))

instance OEIS 248122 where
  oeisIx 0 = 0
  oeisIx 1 = 0
  oeisIx n = 3 * r (n - 1) + 3^ceiling (n % 2) - r (ceiling (n % 2))
    where r = oeisIx @248122

instance OEIS 248737 where
  oeis = 0 : f 1 [0] where
     f x ys = y : f (x + 1) (y : ys) where
       y = (+ 1) $ fromMaybe (x - 1) $ findIndex (\z -> gcd z x /= 1) ys

instance OEIS 248910 where
  oeis = iterate f 1 where
     f x = 1 + if r < 5 then x else 6 * f x'  where (x', r) = divMod x 6

instance OEIS 248952 where
  oeis = oeis248952

instance OEIS 248953 where
  oeis = oeis248953
(oeis248952, (oeis248953)) = unzip $
    map (\x -> minmax 1 x $ S.singleton x) [0..] where
    minmax _ 0 s = (S.findMin s, S.findMax s)
    minmax k x s = minmax (k + 1) y (S.insert y s) where
                          y = x + (if (x - j) `S.member` s then j else -j)
                          j = k * signum x

instance OEIS 249357 where
  oeis = 1 : 2 : 3 : f 2 3 where
     f u v = y : f v y where
       y = head [x | x <- [u + v ..], gcd x u > 1, gcd x v == 1]

instance OEIS 249484 where
  oeis = 1 : concat (zipWith (++) ([2] : f [5,2,7]) (f [4,3])) where
     f = iterate (\row -> [g $ head row] ++ row ++ [g $ last row])
     g x = x + ((5 -) . (* 2) . flip mod 2) x

instance OEIS 249629 where
  oeisIx = a where
    a 0 = 0; a 1 = 0
    a n = 4 * a (n - 1) + 4^ceiling (n % 2) - a (ceiling (n % 2))

instance OEIS 249638 where
  oeisIx = a where
    a 0 = 0; a 1 = 0
    a n = 5 * a (n - 1) + 5^ceiling (n % 2) - a (ceiling (n % 2))

instance OEIS 249777 where
  oeis = 0 : 0 : f 2 1 [3..] where
     f x y zs = g zs 0 where
        g (u:us) w | gcd y u > 1 || gcd x u > 1 = g us (w + 1)
                   | otherwise = w : f u x (delete u zs)

instance OEIS 249783 where
  oeisIx 0 = 0
  oeisIx n = minimum (map (bi n) [0.. (n - 1)])
    where
      bi x y = if (x<y) then (x+y) else (bi y (x-y))

instance OEIS 249856 where
  oeisIx = sum . map (flip mod 2) . (uss `genericIndex`) . succ

instance OEIS 249857 where
  oeisIx = sum . map ((1 -) . flip mod 2) . (uss `genericIndex`) . succ

instance OEIS 249990 where
  oeis = f 2 [1..] where
     f k xs = reverse ys ++ f (k + 1) (g zs) where
              g us = reverse vs ++ g ws where
                     (vs, ws) = splitAt k us
              (ys, zs) = splitAt k xs

instance OEIS 251413 where
  oeis = 1 : 3 : 5 : f 3 5 [7, 9 ..] where
     f u v ws = g ws where
       g (x:xs) = if gcd x u > 1 && gcd x v == 1
                     then x : f v x (delete x ws) else g xs

instance OEIS 251416 where
  oeis = 2 : 3 : f 2 3 [4..] where
     f u v ws = h ws where
       h (x:xs) = if gcd x u > 1 && gcd x v == 1
                     then (head ws) : f v x (delete x ws) else h xs

instance OEIS 251554 where
  oeis = 1 : 2 : 5 : f 2 5 (3 : 4 : [6..]) where
     f u v ws = g ws where
       g (x:xs) = if gcd x u > 1 && gcd x v == 1
                     then x : f v x (delete x ws) else g xs

instance OEIS 251555 where
  oeis = 1 : 3 : 2 : f 3 2 [4..] where
     f u v ws = g ws where
       g (x:xs) = if gcd x u > 1 && gcd x v == 1
                     then x : f v x (delete x ws) else g xs

instance OEIS 251599 where
  oeis = f 0 $ g 1 [1..] where
     f i (us:vs:wss) = [head $ drop i us] ++ (take 2 $ drop i vs) ++
                       f (i + 1) wss
     g k zs = ys : g (k + 1) xs where (ys,xs) = splitAt k zs

instance OEIS 251604 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v ws = g ws where
       g (x:xs) = if gcd x (u + v) > 1 && gcd x v == 1
                     then x : f v x (delete x ws) else g xs

instance OEIS 251622 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v xs = g xs where
       g (w:ws) = if gcd w u > 1 || gcd w v > 1
                     then w : f v w (delete w xs) else g ws

instance OEIS 251635 where
  oeis = tablList @251635
instance Table 251635 where
  tabl = [1] : iterate (0 :) [-2, 1]

instance OEIS 253672 where
  oeis = tablList @253672
instance Table 253672 where
  tabf = [0,1,2] : f [] [0,1,2] [] (iterate (map (+ 3)) [3..5]) where
     f as bs cs (uvws:uvwss) = (as' ++ uvws ++ cs') : f as' uvws cs' uvwss
       where as' = as ++ [u,v]; cs' = [w] ++ cs
             [u,v,w] = bs

instance OEIS 254077 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v ws = g ws where
       g (x:xs) = if gcd x u > gcd x v then x : f v x (delete x ws) else g xs

instance OEIS 254429 where
  oeis = iterate ((+ 1) . (2 ^)) 0

instance OEIS 254732 where
  oeisIx (succ->n) = head [k | k <- [n + 1 ..], mod (k ^ 2) n == 0]

instance OEIS 254744 where
  oeis = 1 : f 2 [1] where
     f x ys = y : f (x * 2) (y : ys) where
              y = x * (sum $ zipWith (*) ys $ reverse ys)

instance OEIS 254788 where
  oeis = 1 : f [2..] 1 [] where
     f xs y zs = g xs where
       g (w:ws) | s `elem` zs || d `elem` zs = g ws
                | otherwise = w : f (delete w xs) w (d : s : zs)
                where s = y + w; d = abs (y - w)

instance Table 254858 where
  tabl = diags [] $ iterate (scanl1 (+)) (oeis @40) where
     diags uss (vs:vss) = (map head wss) : diags (map tail wss) vss
                          where wss = vs : uss
instance OEIS 254858 where
  oeis = tablList @254858

instance OEIS 254868 where
  oeis = 6 : kehrig (S.singleton 6) 6 where
     kehrig s x | x > 4 && (x - 4) `S.notMember` s =
                  (x - 4) : kehrig (S.insert (x - 4) s) (x - 4)
                | (x + 4) `S.notMember` s =
                  (x + 4) : kehrig (S.insert (x + 4) s) (x + 4)
                | otherwise =
                  (x * 4) : kehrig (S.insert (x * 4) s) (x * 4)

instance OEIS 255582 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v ws = y : f v y (delete y ws) where
                y = head [z | z <- ws, let d = gcd u z, d > 1, gcd v z <= d]

instance OEIS 255723 where
  oeis = 0 : concat (transpose [map (subtract 2) (oeis @255723),
                                        map (-1 -) (oeis @255723),
                                        map (+ 2) (oeis @255723),
                                        tail (oeis @255723)])

instance OEIS 255805 where
  oeis = iterate f 1 where
     f x = 1 + if r < 7 then x else 8 * f x'  where (x', r) = divMod x 8

instance OEIS 255808 where
  oeis = iterate f 1 where
     f x = 1 + if r < 8 then x else 9 * f x'  where (x', r) = divMod x 9

instance OEIS 256184 where
  oeis = 0 : concat (transpose [map (subtract 2) (oeis @256184),
                                        map (subtract 1) (oeis @256184),
                                        map negate $ tail (oeis @256184)])

instance OEIS 256185 where
  oeis = 0 : concat (transpose [map (subtract 3) (oeis @256185),
                                        map (-2 -) (oeis @256185),
                                        map negate $ tail (oeis @256185)])

instance OEIS 256512 where
  oeisIx n = n * (1 + (2 * n) ^ n)

instance OEIS 256556 where
  oeisIx (succ->n) = n * uncurry (^) (divMod n 10)

instance OEIS 257212 where
  oeisIx n = head [d | d <- [1..], div n d - div n (d+1) <= 1]

instance OEIS 257213 where
  oeisIx n = head [d | d <- [1..], div n d == div n (d + 1)]

instance OEIS 257588 where
  oeisIx = abs . f 1 where
     f _ 0 = 0
     f s x = s * d ^ 2 + f (negate s) x' where (x', d) = divMod x 10

instance OEIS 257836 where
  oeis = f $ S.singleton (15, 3, 5) where
     f s = y : f (S.insert (w, u, v') $ S.insert (w `div` u, u + 2, v') s')
           where w = y * v'; v' = v + 2
                 ((y, u, v), s') = S.deleteFindMin s

instance OEIS 258073 where
  oeisIx = (+ 1) . (* 78557) . (2 ^) . succ

instance OEIS 258703 where
  oeisIx = floor . (/ 2) . subtract 1 . (* sqrt 2) . fi

instance OEIS 258721 where
  oeisIx n = 4 * n * (6 * n + 13) + 29

instance OEIS 259022 where
  oeis = cycle [1, -1, -1, 1, 0, -1, 1, 1, -1]

instance OEIS 260112 where
  oeisIx n = b n 0
    where
      c i = if i `mod` 4 == 0 then i `div` 4 else i - 1
      b 0 foldCount = foldCount
      b sheetCount foldCount = b (c sheetCount) (foldCount + 1)

instance OEIS 260194 where
  oeis = 1 : 1 : 1 : f 1 1 1 where
     f u v v' = w : f w u v where w = u + gcd u (u + v')

instance OEIS 261012 where
  oeisIx = signum . pred
  oeis = -1 : 0 : [1, 1 ..]

instance OEIS 262065 where
  oeis = O.union us vs where
     us = [val60 $ bs ++ reverse bs | bs <- bss]
     vs = [0..59] ++ [val60 $ bs ++ cs ++ reverse bs |
            bs <- tail bss, cs <- take 60 bss]
     bss = iterate s [0] where
           s [] = [1]; s (59:ds) = 0 : s ds; s (d:ds) = (d + 1) : ds
     val60 = foldr (\b v -> 60 * v + b) 0

instance OEIS 262277 where
  oeis = filter f [1..] where
     f x = sort ds' == sort (map (9 -) ds') where
       ds' = nub $ ds x
       ds 0 = []; ds z = d : ds z' where (z', d) = divMod z 10

instance OEIS 262437 where
  oeis = tablList @262437
instance Table 262437 where
  tabf = iterate succ [0] where
     succ []      = [1]
     succ (15:hs) = 0 : succ hs
     succ (h:hs)  = (h + 1) : hs

instance OEIS 262564 where
  oeis = [2, 3, 5, 4] ++ [6..]

instance OEIS 262565 where
  oeis = cycle [2,3,5,5,3,2]

instance OEIS 265158 where
  oeis = 1 : concat
     (transpose [map (* 2) (oeis @265158), map (flip div 2) (oeis @265158)])

instance OEIS 265377 where
  oeis = f (S.singleton (1 + 2^3, (1, 2))) (-1) where
     f s z = if y /= z then y : f s'' y else f s'' y
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance Table 265705 where
  tabl = map (rowT @265705) [0..]
  rowT n = map (rowCol @265705 n) [0..n]
  rowCol n k = k `bimpl` n
instance OEIS 265705 where
  oeis = tablList @265705

instance OEIS 265716 where
  oeisIx n = n `bimpl` (2 * n)

instance OEIS 265845 where
  oeis = f (S.singleton (1, (1, 1))) 0 0 where
     f s z z' = if y == z && z' /= z then y : f s'' y z else f s'' y z
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 269347 where
  oeis = 1 : map a [2..] where
    a n = sum $ filter ((==) 0 . mod n . (oeisIx @269347 . pred)) [1..n - 1]

instance OEIS 271268 where
  oeis = 8 : cycle [88, 1664, 17144, 17112, 1214]

instance OEIS 275673 where
  oeis = scanl (+) 1 $ concatMap (replicate 6) [1..]

instance OEIS 276163 where
  oeisIx (succ->n) = maximum $ map minimax $ permutations [1..n]

instance OEIS 276168 where
  oeisIx (succ->n) = minimum $ map minimax $ permutations [1..n]

instance OEIS 287355 where
  oeisIx = go 0 2 . succ
    where
      go a r n
        | n >= c             = go (a+t) (r+1) (n - c)
        | n >= r*div n r + m = a + 2*div n r + 1
        | n >= r*div n r + m' + 1 = a + 2*div n r + 1
        | otherwise          = a + 2*div n r
        where
          t  = totient r
          c  = div (r*t) 2
          m  = midnum r
          m' = midnum (r - 1)
      midnum r = head [a | a<-[div (r+1) 2..], gcd a r==1]

instance OEIS 288208 where
  oeisIx = a . succ where
    pairs l = zip l (drop 1 l)
    d n = filter (all (uncurry (/=)) . zip [1..]) $ Data.List.permutations [1..n]
    a n = length $ filter (all ((1<) . abs . uncurry (-)) . pairs) $ d n

instance OEIS 305461 where
  oeisIx (succ->n) = genericLength $ filter (\i -> (i^3 - i^2) `mod` n == 0) [0..n - 1]

instance OEIS 306216 where
  oeis = 1 : 1 : concat (unfoldr nextGeneration [1,1]) where
    nextGeneration l = Just (diff l, l ++ diff l)
    diff xs =  zipWith subtract xs (tail xs)

instance OEIS 308576 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @308576 (n - 1)) + (oeisIx @308576 (oeisIx @308576 (n - 1) `mod` n)) + 1

instance OEIS 323186 where
  oeisIx 0 = 0
  oeisIx 1 = -1
  oeisIx 2 = -1
  oeisIx n
    | a (n - 2) == 0 = a (n - 1) + a' (n - 1) - a'' (n - 1)
    | otherwise      = a (n - 1) + a' (n - 1) + a'' (n - 1)
    where a     = oeisIx @323186
          a'  n = a  n - a (n - 1)
          a'' n = a' n - a' (n - 1)

instance OEIS 325902 where
  oeis = filter (\n -> sumPartitionable $ primeFactors (n - 1) ++ primeFactors (n+1)) [2..]
    where
      sumPartitionable ns
        | p <- \ms -> sum ms == sum (ns \\ ms)
        = any p $ subsequences ns

instance OEIS 55126 where
  oeisIx 0 = 0
  oeisIx n = if d == 0 then 16 * (oeisIx @55126) n' else 16 * (oeisIx @55126) n' + 16 - d
              where (n', d) = divMod n 16

instance OEIS 57144 where
  oeisIx (succ->n) = head $ last $ head $ groupBy ((==) `on` length) $
              reverse $ sortBy (compare `on` length) $
              group $ sort [u * v | u <- [1..n], v <- [1..n]]

instance OEIS 59707 where
  oeisIx n = if u == n || v == n then n else (oeisIx @59707) (u * v) where
     (u,v) = foldl (\ (x,y) d -> if odd d then (10*x+d,y) else (x,10*y+d))
          (0,0) $ reverse $ unfoldr
          (\z -> if z == 0 then Nothing else Just $ swap $ divMod z 10) n

instance OEIS 59717 where
  oeisIx n = if u == n || v == n then n else (oeisIx @59717) (u + v) where
     (u,v) = foldl (\ (x,y) d -> if odd d then (10*x+d,y) else (x,10*y+d))
          (0,0) $ reverse $ unfoldr
          (\z -> if z == 0 then Nothing else Just $ swap $ divMod z 10) n

instance OEIS 61601 where
  oeisIx n = if n <= 9 then 9 - n else 10 * ad n' + 9 - d
              where (n',d) = divMod n 10
                    ad = undefined

instance OEIS 62050 where
  oeisIx (succ->n) = if n < 2 then n else 2 * (oeisIx @62050.pred) n' + m - 1
              where (n',m) = divMod n 2

instance OEIS 62756 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @62756) n' + m `mod` 2 where (n',m) = divMod n 3

instance OEIS 63232 where
  oeis = 5 : 16 : 24 : 36 : zipWith3 (((-) .) . (+))
     (drop 3 (oeis @63232)) (drop 2 (oeis @63232)) (tail (oeis @63232))

instance OEIS 65031 where
  oeisIx n = f n  where
     f x | x < 10    = 2 - x `mod` 2
         | otherwise = 10 * (f x') + 2 - m `mod` 2
         where (x',m) = divMod x 10

instance OEIS 65331 where
  oeisIx = f 2 1 . succ where
     f p y x | r == 0    = f p (y * p) x'
             | otherwise = if p == 2 then f 3 y x else y
             where (x', r) = divMod x p

instance OEIS 65359 where
  oeisIx 0 = 0
  oeisIx n = - (oeisIx @65359) n' + m where (n', m) = divMod n 2

instance OEIS 66186 where
  oeisIx = sum . concat . ps 1 where
     ps _ 0 = [[]]
     ps i j = [t:ts | t <- [i..j], ts <- ps t (j - t)]

instance OEIS 66411 where
  oeisIx 0 = 1
  oeisIx n = genericLength $ nub $ map
     apex [perm | perm <- permutations [0..n], head perm < last perm] where
     apex = head . until ((== 1) . length)
                         (\xs -> (zipWith (+) xs $ tail xs))

instance OEIS 67080 where
  oeisIx (succ->n) = if n <= 9 then n else n * (oeisIx @67080.pred) (n `div` 10)

instance OEIS 67824 where
  oeisIx (succ->n) = 1 + sum (map (oeisIx @67824.pred) [d | d <- [1..n - 1], mod n d == 0])

instance OEIS 68522 where
  oeisIx 0 = 0
  oeisIx n = 10 * (oeisIx @68522) n' + m ^ 2  where (n', m) = divMod n 10

instance OEIS 70047 where
  oeisIx n = p 1 n where
     p k m | m == 0 = 1 | m < k = 0 | otherwise = q k (m-k) + p (k+1) m
     q k m | m == 0 = 1 | m < k = 0 | otherwise = p (k+2) (m-k) + p (k+2) m

instance OEIS 70550 where
  oeis = 1 : 2 : 2 : 3 :
     zipWith (+) (oeis @70550)
                 (zipWith (+) (tail (oeis @70550)) (drop 3 (oeis @70550)))

instance OEIS 71413 where
  oeisIx 0 = 0
  oeisIx n | m == 0    = (oeisIx @71413) n' + n
            | otherwise = (oeisIx @71413) n' - n  where (n',m) = divMod n 2

instance OEIS 71954 where
  oeis = 2 : 4 : zipWith (-)
                 (map ((4 *) . pred) (tail (oeis @71954))) (oeis @71954)

instance OEIS 72221 where
  oeis = 1 : 4 : (map (+ 2) $
     zipWith (-) (map (* 6) $ tail (oeis @72221)) (oeis @72221))

instance OEIS 72229 where
  oeis = [0, 0, 0, 0, 1, 2, 3, 4] ++ zipWith (+)
                 (zipWith (-) (tail (oeis @72229)) (oeis @72229))
                 (drop 7 (oeis @72229))

instance OEIS 73101 where
  oeisIx (succ->n) = genericLength [ (x,y) |
     x <- [n `div` 4 + 1 .. 3 * n `div` 4],   let y' = recip $ 4%n - 1%x,
     y <- [floor y' + 1 .. floor (2*y') + 1], let z' = recip $ 4%n - 1%x - 1%y,
     denominator z' == 1 && numerator z' > y && y > x]

instance OEIS 73785 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @73785) n' * 10 + m where
     (n', m) = if r < 0 then (q + 1, r + 3) else (q, r)
               where (q, r) = quotRem n (negate 3)

instance OEIS 73789 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @73789) n' * 10 + m where
     (n', m) = if r < 0 then (q + 1, r + 8) else (q, r)
               where (q, r) = quotRem n (negate 8)

instance OEIS 74394 where
  oeis = 1 : 2 : 3 : zipWith (-)
     (tail $ zipWith (*) (tail (oeis @74394)) (oeis @74394)) (oeis @74394)

instance OEIS 74677 where
  oeis = 0 : 1 : 1 : 1 : zipWith (+) (oeis @74677)
     (zipWith (+) (tail (oeis @74677)) (drop 3 (oeis @74677)))

instance OEIS 75877 where
  oeisIx (succ->n) = if n < 10 then n else (oeisIx @75877.pred) (n `div` 10) ^ (n `mod` 10)

instance OEIS 77623 where
  oeis = 1 : 2 : 4 : zipWith3 (\u v w -> abs (w - v - u))
                 (oeis @77623) (tail (oeis @77623)) (drop 2 (oeis @77623))

instance OEIS 77653 where
  oeis = 1 : 2 : 2 : zipWith3 (\u v w -> abs (w - v - u))
                 (oeis @77653) (tail (oeis @77653)) (drop 2 (oeis @77653))

instance OEIS 78012 where
  oeis = 1 : 0 : 0 : 1 : zipWith (+) (oeis @78012)
     (zipWith (+) (tail (oeis @78012)) (drop 2 (oeis @78012)))

instance OEIS 78343 where
  oeis = -1 : 2 : zipWith (+)
                          (map (* 2) $ tail (oeis @78343)) (oeis @78343)

instance OEIS 78495 where
  oeis = [1, 1, 1, 1, 1, 1, 1] ++
    zipWith div (foldr1 (zipWith (+)) (map b [1,3])) (oeis @78495)
    where b i = zipWith (*) (drop i (oeis @78495)) (drop (7-i) (oeis @78495))

instance OEIS 79623 where
  oeis = 1 : 1 : 4 : zipWith3 (\u v w -> abs (w - v - u))
                 (oeis @79623) (tail (oeis @79623)) (drop 2 (oeis @79623))

instance OEIS 79624 where
  oeis = 1 : 1 : 6 : zipWith3 (\u v w -> abs (w - v - u))
                 (oeis @79624) (tail (oeis @79624)) (drop 2 (oeis @79624))

instance OEIS 79935 where
  oeis =
     1 : 3 : zipWith (-) (map (4 *) $ tail (oeis @79935)) (oeis @79935)

instance OEIS 80040 where
  oeis =
     2 : 2 : map (* 2) (zipWith (+) (oeis @80040) (tail (oeis @80040)))

instance OEIS 80096 where
  oeis = 1 : 1 : 2 : zipWith3 (\u v w -> abs (w - v - u))
                 (oeis @80096) (tail (oeis @80096)) (drop 2 (oeis @80096))

instance OEIS 81603 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @81603) n' + m `div` 2 where (n',m) = divMod n 3

instance OEIS 82498 where
  oeis = 0 : 1 : concat
     (zipWith (\u v -> [u, u + v]) (tail (oeis @82498)) (oeis @82498))

instance OEIS 83337 where
  oeis =
     0 : 3 : map (* 2) (zipWith (+) (oeis @83337) (tail (oeis @83337)))

instance OEIS 86283 where
  oeis = 1 : 1 : f 1 0 [1..] where
     f x y (z:zs) = u : f u (abs $ x - u) zs where
       u = minimum [if v < x then x - v else x + v |
                    v <- if y < z then [y + z] else [y + z, y - z]]

instance OEIS 86901 where
  oeis = 1 : 1 : zipWith (+)
                 (map (* 3) (oeis @86901)) (map (* 4) $ tail (oeis @86901))

instance OEIS 87808 where
  oeis = 0 : concat
     (transpose [map (+ 1) (oeis @87808), map (* 2) $ tail (oeis @87808)])

instance OEIS 88226 where
  oeis = 0 : 0 : 1 : zipWith3 (\u v w -> abs (w - v - u))
                 (oeis @88226) (tail (oeis @88226)) (drop 2 (oeis @88226))

instance OEIS 89898 where
  oeisIx n = if n < 10 then n + 1 else (d + 1) * (oeisIx @89898) n'
              where (n', d) = divMod n 10

instance OEIS 89911 where
  oeis = 0 : 1 : zipWith (\u v -> (u + v) `mod` 12)
                         (tail (oeis @89911)) (oeis @89911)

instance OEIS 90390 where
  oeis = 1 : 1 : 9 : zipWith (-) (map (* 5) $
     tail $ zipWith (+) (tail (oeis @90390)) (oeis @90390)) (oeis @90390)

instance OEIS 90597 where
  oeis = [0,1,1,3,3,8,12] ++ zipWith (-)
     (drop 4 $ zipWith (-) (map (* 5) zs) (drop 2 (oeis @90597)))
     (zipWith (+) (drop 2 $ map (* 2) zs) (map (* 8) zs))
     where zs = zipWith (+) (oeis @90597) $ tail (oeis @90597)

instance OEIS 96095 where
  oeis = 1 : 1 : zipWith dadd (oeis @96095) (tail (oeis @96095)) where
     dadd x y = foldl (\v d -> (if d < 10 then 10 else 100)*v + d)
                      0 $ reverse $ unfoldr f (x,y) where
          f (x,y) | x + y == 0 = Nothing
                  | otherwise  = Just (xd + yd, (x',y'))
                  where (x',xd) = divMod x 10; (y',yd) = divMod y 10

instance OEIS 97133 where
  oeis = 1 : 2 : 4 : zipWith (+)
                 (map (* 2) $ tail (oeis @97133)) (oeis @97133)


instance OEIS 101265 where
  oeis = 1 : 2 : 6 : zipWith (+) (oeis @101265)
      (map (* 5) $ tail $ zipWith (-) (tail (oeis @101265)) (oeis @101265))

instance OEIS 102900 where
  oeis = 1 : 1 : zipWith (+)
                 (map (* 4) (oeis @102900)) (map (* 3) $ tail (oeis @102900))

instance OEIS 105186 where
  oeisIx 0 = 0
  oeisIx n = 9 * (oeisIx @105186) n' + mod t 3
              where (n', t) = divMod n 9

instance OEIS 105471 where
  oeis = 0 : 1 :
     zipWith ((flip mod 100 .) . (+)) (oeis @105471) (tail (oeis @105471))

instance OEIS 106108 where
  oeis =
     7 : zipWith (+) (oeis @106108) (zipWith gcd (oeis @106108) [2..])

instance OEIS 107128 where
  oeisIx n = if n == 0 then 0 else 10 * (oeisIx @107128 n') + m * d + (1 - m) * d'
              where (d', m) = divMod d 2
                    (n', d) = divMod n 10

instance OEIS 107458 where
  oeis = 1 : 0 : 0 : 0 : zipWith (+) (oeis @107458)
     (zipWith (+) (tail (oeis @107458)) (drop 2 (oeis @107458)))

instance OEIS 108898 where
  oeis = -1 : 1 : 3 :
     zipWith (-) (map (* 3) $ drop 2 (oeis @108898)) (map (* 2) (oeis @108898))

instance OEIS 109613 where
  oeisIx = (+ 1) . (* 2) . (`div` 2)
  oeis = 1 : 1 : map (+ 2) (oeis @109613)

instance OEIS 109671 where
  oeis = concat (transpose [1 : f 1 (oeis @109671), (oeis @109671)])
     where f u (v:vs) = y : f y vs where
             y = if u > v then u - v else u + v

instance OEIS 111006 where
  oeis = tablList @111006
instance Table 111006 where
  tabl =  map fst $ iterate (\ (us, vs) ->
     (vs, zipWith (+) (zipWith (+) ([0] ++ us ++ [0]) ([0,0] ++ us))
                      ([0] ++ vs))) ([1], [0,1])

instance OEIS 115339 where
  oeis = [1, 1, 2, 3] ++
                 zipWith (+) (oeis @115339) (drop 2 (oeis @115339))

instance OEIS 115390 where
  oeis = 0 : 0 : 1 : map (* 2) (zipWith (-) (oeis @115390)
     (tail $ map (* 2) $ zipWith (-) (oeis @115390) (tail (oeis @115390))))

instance OEIS 122972 where
  oeis = 1 : 2 : zipWith (+)
     (zipWith (*) [2..] (oeis @122972)) (zipWith (*) [1..] $ tail (oeis @122972))

instance OEIS 123270 where
  oeis = 1 : 1 : zipWith (-) (map (* 5) $
     zipWith (+) (tail (oeis @123270)) (oeis @123270)) (oeis @123270)

instance OEIS 124108 where
  oeisIx 0 = 0
  oeisIx x = 2 * (b + 1) * (oeisIx @124108) x' + (b * 2)
              where (x', b) = divMod x 2

instance OEIS 125145 where
  oeis =
     1 : 4 : map (* 3) (zipWith (+) (oeis @125145) (tail (oeis @125145)))

instance OEIS 126759 where
  oeis = 1 : f 1 where
     f n = (case mod n 6 of 1 -> 2 * div n 6 + 2
                            5 -> 2 * div n 6 + 3
                            3 -> (oeisIx @126759) $ div n 3
                            _ -> (oeisIx @126759) $ div n 2) : f (n + 1)

instance OEIS 128217 where
  oeis = filter f [0..] where
     f x = 4 * abs (root - fi (round root)) < 1
           where root = sqrt $ fi x

instance OEIS 130578 where
  oeis = 0 : 0 : 1 : 3 : zipWith (+)
     (map (* 2) $ drop 3 (oeis @130578))
     (zipWith (-) (map (+ 1) (oeis @130578)) (drop 2 (oeis @130578)))

instance OEIS 130667 where
  oeis = 1 : (concat $ transpose
     [zipWith (+) vs (oeis @130667), zipWith (+) vs $ tail (oeis @130667)])
     where vs = map (* 5) (oeis @130667)

instance OEIS 135504 where
  oeis = 1 : zipWith (+)
                     (oeis @135504) (zipWith lcm (oeis @135504) [2..])

instance OEIS 135507 where
  oeis = 1 : zipWith (+)
     (map (* 2) $ (oeis @135507)) (zipWith lcm (oeis @135507) [2..])

instance OEIS 136400 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @136400) n' * 10 + min 1 d where (n',d) = divMod n 10

instance OEIS 140472 where
  oeis = 0 : 1 : h 2 1 where
    h x y = z : h (x + 1) z where z = (oeisIx @140472) (x - y) + (oeisIx @140472) (x `div` 2)

instance OEIS 141036 where
  oeis = 2 : 1 : 1 : zipWith3 (((+) .) . (+))
     (oeis @141036) (tail (oeis @141036)) (drop 2 (oeis @141036))

instance OEIS 145037 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @145037) n' + 2*m - 1 where (n', m) = divMod n 2

instance OEIS 151666 where
  oeisIx n = fi $ fromEnum (n < 2 || m < 2 && (oeisIx @151666) n' == 1)
     where (n', m) = divMod n 4

instance OEIS 152223 where
  oeis = 1 : -6 : zipWith (-)
     (map (* 6) $ (oeis @152223)) (map (* 4) $ tail (oeis @152223))

instance OEIS 153733 where
  oeisIx n = if b == 0 then n else (oeisIx @153733) n'  where (n', b) = divMod n 2

instance OEIS 162610 where
  oeis = tablList @162610
instance Table 162610 where
  rowCol n k = k * n - k + n
  rowT n = map (rowCol @162610 n) [1..n]
  tabl = map (rowT @162610) [1..]

instance OEIS 164055 where
  oeis = 1 : 10 : 325 : zipWith (+) (oeis @164055)
     (map (* 35) $ tail $ zipWith (-) (tail (oeis @164055)) (oeis @164055))

instance OEIS 166238 where
  oeis = a' 0 where a' n = n : takeWhile (< (n - 2)) (oeis @166238) ++ a' (n + 1)

instance OEIS 171874 where
  oeis = [0, 0, 0, 1, 1] ++ zipWith5 (\z y x w v -> z + x*y + w^v)
     (drop 4 (oeis @171874)) (drop 3 (oeis @171874))
     (drop 2 (oeis @171874)) (tail (oeis @171874)) (oeis @171874)

instance OEIS 174168 where
  oeis = [1,2,5,17] ++ zipWith div (zipWith (+)
     (zipWith (*) (tail (oeis @174168)) (drop 3 (oeis @174168)))
                  (map ((* 3) . (^ 2)) (drop 2 (oeis @174168)))) (oeis @174168)

instance OEIS 175899 where
  oeis = 0 : 2 : 3 : 10 : zipWith (+) (map (* 2) (oeis @175899))
     (zipWith (+) (tail (oeis @175899)) (drop 2 (oeis @175899)))

instance OEIS 176892 where
  oeisIx 0 = 2
  oeisIx n = foldl (\v d -> 10 * v + d + 2) 0 $
     unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) n

instance OEIS 178384 where
  oeis = [1, 1, 1, 3] ++
     zipWith div (foldr1 (zipWith subtract) (map b [1..2])) (oeis @178384)
     where b i = zipWith (*) (drop i (oeis @178384)) (drop (4-i) (oeis @178384))

instance OEIS 180046 where
  oeis = [1..4] ++ zipWith4 (((((+) .) . (+)) .) . (-))
                            (drop 3 (oeis @180046)) (drop 2 (oeis @180046))
                              (tail (oeis @180046)) (oeis @180046)

instance OEIS 182229 where
  oeis = 2 : 3 : zipWith (+)
                         (map (flip div 3) (oeis @182229)) (tail (oeis @182229))

instance OEIS 182230 where
  oeis = 3 : 4 : zipWith (+)
                         (map (flip div 4) (oeis @182230)) (tail (oeis @182230))

instance OEIS 182280 where
  oeis = 3 : 4 : zipWith (+)
                         (oeis @182280) (map (flip div 4) $ tail (oeis @182280))

instance OEIS 182281 where
  oeis = 2 : 3 : zipWith (+)
                         (oeis @182281) (map (flip div 3) $ tail (oeis @182281))

instance OEIS 182458 where
  oeis = 1 : 2 : zipWith mod
     (map (+ 1) $ zipWith (*) (oeis @182458) (tail (oeis @182458))) [2..]

instance OEIS 193286 where
  -- See Theorem 5 in John Derbyshire link.
  oeisIx (succ->n) = p n [] where
     p 0 ks       = product ks
     p n []       = p (n - 1) [1]
     p n (k:ks)
       | n < 0     = 0
       | otherwise = max (p (n - 1) ((k+1):ks)) (p (n- 3) (1:k:ks))

instance OEIS 193358 where
  oeis =
     1 : 2 : (map ((+ 2) . (oeisIx @193358)) $ zipWith (-) [2..] (oeis @193358))

instance OEIS 200069 where
  oeis = 1 : 4 : zipWith (+)
     (map (* 4) $ tail (oeis @200069)) (map (* 13) (oeis @200069))

instance OEIS 203976 where
  oeis = 0 : 1 : 5 : 4 : zipWith (-)
     (map (* 3) $ drop 2 (oeis @203976)) (oeis @203976)

instance OEIS 204200 where
  oeis = 1 : 1 : 2 : zipWith (+) (oeis @204200) (tail $ zipWith (-)
     (map (* 4) (tail (oeis @204200))) (map (* 3) (oeis @204200)))

instance OEIS 206351 where
  oeis = 1 : 3 : map (subtract 4)
                 (zipWith (-) (map (* 7) (tail (oeis @206351))) (oeis @206351))

instance OEIS 213190 where
  oeis = 1 : 1 : zipWith (+)
     (zipWith (*) [2..] $ tail (oeis @213190)) (map (* 3) (oeis @213190))

instance OEIS 214626 where
  oeis = 1 : 1 : 3 : zipWith f (oeis @214626) (drop 2 (oeis @214626))
     where f u v = (u + v) `div` gcd u v

instance OEIS 214727 where
  oeis = 1 : 2 : 2 : zipWith3 (\x y z -> x + y + z)
     (oeis @214727) (tail (oeis @214727)) (drop 2 (oeis @214727))

instance OEIS 215879 where
  oeisIx n = if t == 0 then 0 else (oeisIx @215879) n' + 1
              where (n',t) = divMod n 3

instance OEIS 217657 where
  oeisIx n | n <= 9    = 0
            | otherwise = 10 * (oeisIx @217657) n' + m where (n', m) = divMod n 10

instance OEIS 224909 where
  oeis = 1 : 1 : zipWith mod
     (zipWith (+) (oeis @224909) $ tail (oeis @224909))
     (zipWith (-) [3..] $ tail (oeis @224909))

instance OEIS 226222 where
  oeis = 1 : 1 : 1 : zipWith (+)
     (map (oeisIx @226222.pred) $ zipWith (-) [3..] (oeis @226222))
     (map (oeisIx @226222.pred) $ zipWith (-) [2..] $ tail (oeis @226222))

instance OEIS 235049 where
  oeisIx x = if x == 0 then 0 else 10 * (oeisIx @235049) x' + max 0 (d - 1)
              where (x', d) = divMod x 10

instance OEIS 235708 where
  oeisIx (succ->n) = f n where
     f 1 = 1
     f b = if isPandigital b n then b else f (b - 1) where
           isPandigital b = (== b) . length . nub . unfoldr
             (\x -> if x == 0 then Nothing else Just $ swap $ divMod x b)

instance OEIS 238845 where
  oeisIx n = genericLength $ takeWhile (== 0) $ zipWith (-) (bin n) (bin (n+1))
    where
      bin = reverse . unfoldr
        (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 239549 where
  oeis = 0 : 1 : zipWith (+)
                 (map (* 8) $ tail (oeis @239549)) (map (* 12) (oeis @239549))

instance OEIS 240807 where
  oeis = -1 : -1 : 2 : zipWith (+) xs (tail xs)
     where xs = map (oeisIx @240807) $ zipWith (-) [1..] $ tail (oeis @240807)

instance OEIS 240808 where
  oeis = 2 : 1 : 0 : zipWith (+) xs (tail xs)
     where xs = map (oeisIx @240808) $ zipWith (-) [1..] $ tail (oeis @240808)

instance OEIS 241426 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 . concat . inits .
            unfoldr (\x -> if x == 0 then Nothing
                                     else Just $ swap $ divMod x 2)

instance OEIS 244477 where
  oeis = 3 : 2 : 1 : zipWith (+)
     (map (oeisIx @244477.pred) $ zipWith (-) [4..] $ tail (oeis @244477))
     (map (oeisIx @244477.pred) $ zipWith (-) [4..] $ drop 2 (oeis @244477))

instance OEIS 244478 where
  oeis = 2 : 0 : 2 : zipWith (+) xs (tail xs)
     where xs = map (oeisIx @244478) $ zipWith (-) [1..] $ tail (oeis @244478)

instance OEIS 244483 where
  oeis = 3 : 1 : 0 : zipWith (+) xs (tail xs)
     where xs = map (oeisIx @244483) $ zipWith (-) [1..] $ tail (oeis @244483)

instance OEIS 245492 where
  oeis = [0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0] ++
                 zipWith3 (((+) .) . (+))
                 (drop 8 (oeis @245492)) (drop 10 (oeis @245492))
                 (cycle [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 2, 0, 0, 1, 0])

instance OEIS 246435 where
  oeisIx n = if n < 3 then 1 else (oeisIx @246435) (2 * div n 3) + 1

instance OEIS 247061 where
  oeis = [1,8,16,17] ++ zipWith (+)
     (drop 3 (oeis @247061)) (zipWith (-) (tail (oeis @247061)) (oeis @247061))

instance OEIS 247062 where
  oeis = [1,2,5,6,8,11,12,16,17] ++ zipWith (+)
     (drop 8 (oeis @247062)) (zipWith (-) (tail (oeis @247062)) (oeis @247062))

instance OEIS 247065 where
  oeis = [1,16,24,32,40,49,64,65] ++ zipWith (+)
     (drop 7 (oeis @247065)) (zipWith (-) (tail (oeis @247065)) (oeis @247065))

instance OEIS 247066 where
  oeis = [1,2,6,8,12,16,17,21,24,27,32,33] ++ zipWith (+)
     (drop 11 (oeis @247066)) (zipWith (-) (tail (oeis @247066)) (oeis @247066))

instance OEIS 247160 where
  oeis = [1..14] ++ [16,17] ++ zipWith (+)
     (drop 15 (oeis @247160)) (zipWith (-) (tail (oeis @247160)) (oeis @247160))

instance OEIS 247161 where
  oeis = [1,2,4,5,6,8,9,11,12,13,16,17] ++ zipWith (+)
     (drop 11 (oeis @247161)) (zipWith (-) (tail (oeis @247161)) (oeis @247161))

instance OEIS 247382 where
  oeis = [-3, 7, 1, 46] ++ zipWith (flip div) (oeis @247382)
     (zipWith (+)
          (zipWith (*) (tail (oeis @247382)) (drop 3 (oeis @247382)))
          (zipWith (*) (cycle [-1, 1]) (map (^ 2) $ drop 2 (oeis @247382))))

instance OEIS 247540 where
  oeis = 1 : 1 : zipWith (-)
     (map (* 2) xs) (zipWith div (map ((* 3) . (^ 2)) xs) (oeis @247540))
     where xs = tail (oeis @247540)

instance OEIS 247594 where
  oeis = 1 : 2 : 5 : zipWith (+)
     (tail $ zipWith (+) (oeis @247594) $ tail (oeis @247594))
     (map (* 3) (oeis @247594))

instance OEIS 247595 where
  oeis = 1 : 3 : 10 : map (* 4) (zipWith3 (((+) .) . (-))
     (drop 2 (oeis @247595)) (tail (oeis @247595)) (oeis @247595))

instance OEIS 248098 where
  oeis = 1 : 1 : 1 : map (+ 1) (zipWith3 (((+) .) . (+))
                 (oeis @248098) (tail (oeis @248098)) (drop 2 (oeis @248098)))

instance OEIS 248479 where
  oeis = 1 : 3 : zipWith ($) (map uncurry $ cycle [ (-), (*)])
                                     (zip (tail (oeis @248479)) (oeis @248479))

instance OEIS 249039 where
  oeis = 1 : 2 : f 2 2 1 1 where
     f x u v w = y : f (x + 1) y (v + 1 - mod y 2) (w + mod y 2)
                 where y = u + (oeisIx @249039.pred) (x - v) + (oeisIx @249039.pred) (x - w)

instance OEIS 249569 where
  oeis = 1 : 1 : zipWith (+)
     (map (oeisIx @249569.pred) $ zipWith (-) [3..] $ tail (oeis @249569))
     (map (oeisIx @249569.pred) $ zipWith (-) [3..] $ map (oeisIx @249569.pred) (oeis @249569))

instance OEIS 251984 where
  oeisIx (succ->n) = if d > 0 then 10 - d else 10 * (oeisIx @251984.pred) n'
              where (n',d) = divMod n 10

instance OEIS 253853 where
  oeis = 1 : 1 : 1 : map (+ 1)
                             (zipWith (*) (oeis @253853) $ tail (oeis @253853))

instance OEIS 254308 where
  oeis = 0 : 1 : 1 : zipWith3 (\u v w -> u + if odd u then v else w)
                 (drop 2 (oeis @254308)) (tail (oeis @254308)) (oeis @254308)

instance OEIS 257145 where
  oeisIx 0 = 1
  oeisIx n = div (n + 2) 5 * 5 - n

instance OEIS 259043 where
  oeisIx x = if x < 10 then x else (oeisIx @259043) (x' + d) + d
              where (x', d) = divMod x 10

instance OEIS 259967 where
  oeis = 3 : 2 : 2 : 5 : zipWith3 (((+) .) . (+))
     (oeis @259967) (drop 2 (oeis @259967)) (drop 3 (oeis @259967))

instance OEIS 259968 where
  oeis = 1 : 1 : 3 : 6 : zipWith3 (((+) .) . (+))
     (oeis @259968) (drop 2 (oeis @259968)) (drop 3 (oeis @259968))

instance OEIS 261301 where
  oeis = 1 : map abs
     (zipWith (-) (oeis @261301) (zipWith gcd [1..] (oeis @261301)))

instance OEIS 261575 where
  oeis = tablList @261575
instance Table 261575 where
  tabf = [0] : [1] :
     zipWith (add 0) (tail (tabf @261575)) (tabf @261575) where
     add c (a:as) (b:bs) = y : add c' as bs where (c', y) = divMod (a+b+c) 60
     add c (a:as) [] = y : add c' as [] where (c', y) = divMod (a+c) 60
     add 1 _ _ = [1]
     add _ _ _ = []

instance OEIS 261606 where
  oeis = 0 : 1 : map (flip mod 60)
                             (zipWith (+) (oeis @261606) $ tail (oeis @261606))

instance OEIS 263231 where
  oeisIx n = n * (25 * n - 39) `div` 2
  oeis = 0 : -7 : 11 : zipWith (+) (oeis @263231)
     (map (* 3) $ tail $ zipWith (-) (tail (oeis @263231)) (oeis @263231))

instance OEIS 7608 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @7608) n' * 10 + m where
     (n', m) = if r < 0 then (q + 1, r + 4) else (q, r)
               where (q, r) = quotRem n (negate 4)

instance OEIS 10078 where
  oeisIx = x where
     x m = if m == 0 then 1 else 2 * x m' + 1 - b
              where (m',b) = divMod m 2

instance OEIS 14480 where
  oeis = 1 : 6 : map (* 4)
     (zipWith (-) (tail (oeis @14480)) (oeis @14480))

instance OEIS 24629 where
  oeisIx 0 = 0
  oeisIx n = 10 * (oeisIx @24629) (2 * n') + t where (n', t) = divMod n 3

instance OEIS 33428 where
  oeisIx = (* 3) . (^ 2)
  oeis = 0 : 3 : 12 : zipWith (+) (oeis @33428)
     (map (* 3) $ tail $ zipWith (-) (tail (oeis @33428)) (oeis @33428))


instance OEIS 33538 where
  oeis =
     1 : 1 : (map (+ 1) $ zipWith (+) (oeis @33538)
                                      $ map (3 *) $ tail (oeis @33538))

instance OEIS 33539 where
  oeis =
     1 : 1 : 1 : (map (+ 1) $ zipWith (+) (tail (oeis @33539))
                                          (map (2 *) $ drop 2 (oeis @33539)))

instance OEIS 33876 where
  oeisIx n = sum $ zipWith (!!) zss [0..n] where
     zss = take (n+1) $ g (take (n+1) (1 : [0,0..])) where
         g us = (take (n+1) $ g' us) : g (0 : init us)
         g' vs = last $ take (2 * n + 3) $
                        map snd $ iterate h (0, vs ++ reverse vs)
     h (p,ws) = (1 - p, drop p $ zipWith (+) ([0] ++ ws) (ws ++ [0]))

instance OEIS 34182 where
  oeis = 1 : 5 : (map (+ 4) $
     zipWith (+) (oeis @34182) (map (* 2) $ tail (oeis @34182)))

instance OEIS 35302 where
  oeis = 0 : 1 : (-2) :
     zipWith (+) (drop 2 $ map (* 2) (oeis @35302))
                 (map (* 4) $ zipWith (-) (oeis @35302) $ tail (oeis @35302))

instance OEIS 35327 where
  oeisIx n = if n <= 1 then 1 - n else 2 * (oeisIx @35327) n' + 1 - b
              where (n',b) = divMod n 2

instance OEIS 36044 where
  oeisIx 0 = 1
  oeisIx n = foldl (\v d -> 2 * v + d) 0 (unfoldr bc n) where
     bc 0 = Nothing
     bc x = Just (1 - m, x') where (x',m) = divMod x 2

instance OEIS 38500 where
  oeisIx = f 1 . succ where
     f y x = if m == 0 then f (y * 3) x' else y  where (x', m) = divMod x 3

instance OEIS 39724 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @39724) n' * 10 + m where
     (n', m) = if r < 0 then (q + 1, r + 2) else (q, r)
               where (q, r) = quotRem n (negate 2)

instance OEIS 40026 where
  oeisIx (succ->n) = f 1 where
     f t | (1 - t*r) `mod` s == 0 = t*r
         | (1 + t*r) `mod` s == 0 = - t*r
         | otherwise              = f (t + 1)
     (r,s) = split n 1
     split x y | m == 0 = split x' (2 * y)
               | m == 1 = (x,y) where (x',m) = divMod x 2

instance OEIS 42965 where
  oeisIx = (`div` 3) . (subtract 3) . (* 4) . succ
  oeis = 0 : 1 : 3 : map (+ 4) (oeis @42965)

instance OEIS 42974 where
  oeis =  1 : 2 :
     concat (zipWith replicate (tail (oeis @42974)) (oeis @42974))

instance OEIS 46090 where
  oeis = 1 : 4 : map (subtract 2)
     (zipWith (-) (map (* 6) (tail (oeis @46090))) (oeis @46090))

instance OEIS 48379 where
  oeisIx n = if n == 0 then 1 else x n where
     x m = if m == 0 then 0 else 10 * x m' + (d + 1) `mod` 10
           where (m',d) = divMod m 10

instance OEIS 48647 where
  oeisIx 0 = 0
  oeisIx n = 4 * (oeisIx @48647) n' + if m == 0 then 0 else 4 - m
              where (n', m) = divMod n 4

instance OEIS 48678 where
  oeisIx 0 = 0
  oeisIx x = 2 * (b + 1) * (oeisIx @48678) x' + b
              where (x', b) = divMod x 2

instance OEIS 49502 where
  oeisIx = f 0 1 where
     f m i x = if x <= 4
                  then m else f (if mod x 4 == 1
                                    then m + i else m) (i + 1) $ div x 2

instance OEIS 49853 where
  oeis = 1 : 2 : 2 : 3 :
     zipWith (+) (oeis @49853)
                 (zipWith (+) (drop 2 (oeis @49853)) (drop 3 (oeis @49853)))



instance OEIS 51022 where
  oeisIx n = if n < 10 then n else (oeisIx @51022) n' * 100 + r
              where (n', r) = divMod n 10

instance OEIS 51424 where
  oeisIx = genericLength . filter f . partitions where
     f [] = True
     f (p:ps) = (all (== 1) $ map (gcd p) ps) && f ps
     partitions n = ps 1 n where
       ps x 0 = [[]]
       ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

instance OEIS 51786 where
  oeis = 1 : 1 : 1 : 1 :
     zipWith div (tail $ zipWith3 (\u v w -> 1 + u * v * w)
                 (drop 2 (oeis @51786)) (tail (oeis @51786)) (oeis @51786))
                 (oeis @51786)

instance OEIS 52383 where
  oeisIx = f where
     f 0 = 0
     f v = 10 * f w + if r > 0 then r + 1 else 0  where (w, r) = divMod v 9

instance OEIS 52404 where
  oeisIx = f where
     f 0 = 0
     f v = 10 * f w + if r > 1 then r + 1 else r  where (w, r) = divMod v 9

instance OEIS 52406 where
  oeisIx = f where
    f 0 = 0
    f v = 10 * f w + if r > 3 then r + 1 else r where (w, r) = divMod v 9

instance OEIS 52414 where
  oeisIx = f where
    f 0 = 0
    f v = 10 * f w + if r > 5 then r + 1 else r where (w, r) = divMod v 9

instance OEIS 52419 where
  oeisIx = f where
    f 0 = 0
    f v = 10 * f w + if r > 6 then r + 1 else r where (w, r) = divMod v 9

instance OEIS 52421 where
  oeisIx = f where
    f 0 = 0
    f v = 10 * f w + if r > 7 then r + 1 else r where (w, r) = divMod v 9

instance OEIS 52423 where
  oeisIx (succ->n) = f n n where
     f x 1 = 1
     f x y | x < 10    = gcd x y
           | otherwise = if d == 1 then 1 else f x' (gcd d y)
           where (x', d) = divMod x 10

instance OEIS 53141 where
  oeis = 0 : 2 : map (+ 2)
     (zipWith (-) (map (* 6) (tail (oeis @53141))) (oeis @53141))

instance OEIS 55118 where
  oeisIx 0 = 0
  oeisIx n = if d == 0 then 8 * (oeisIx @55118) n' else 8 * (oeisIx @55118) n' + 8 - d
              where (n', d) = divMod n 8

instance OEIS 55120 where
  oeisIx = foldl f 0 . reverse . unfoldr g where
     f v d = if d == 0 then 10 * v else 10 * v + 10 - d
     g x = if x == 0 then Nothing else Just $ swap $ divMod x 10

instance OEIS 55122 where
  oeisIx 0 = 0
  oeisIx n = if d == 0 then 12 * (oeisIx @55122) n' else 12 * (oeisIx @55122) n' + 12 - d
              where (n', d) = divMod n 12

instance OEIS 55124 where
  oeisIx 0 = 0
  oeisIx n = if d == 0 then 14 * (oeisIx @55124) n' else 14 * (oeisIx @55124) n' + 14 - d
              where (n', d) = divMod n 14

instance OEIS 7949 where
  oeisIx = f . succ
    where
      f n | m > 0 = 0
          | let   = 1 + (oeisIx @7949 . pred) n'
       where (n', m) = divMod n 3

instance OEIS 38374 where
  oeisIx = maximum . map length . filter ((== 1) . head) . group .
     unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)
        . succ

instance OEIS 52405 where
  oeisIx = f where
     f 0 = 0
     f v = 10 * f w + if r > 2 then r + 1 else r  where (w, r) = divMod v 9
--   oeisIx (succ->n) = head [a | a <- [1..], n `kara` a == Nothing] where
--      kara a b = if null ks then Nothing else Just $ head ks
--                 where ks = [c | c <- [1..a], a <= c * b, a > c * (b - 1)]

instance OEIS 69099 where
  oeisIx (succ->n) = length
     [ (x,y) | x <- [-n+1..n - 1], y <- [-n+1..n- 1], x + y <= n - 1]

instance OEIS 70867 where
  oeis = 1 : 1 : zipWith (+)
     (map (oeisIx @70867.pred) $ zipWith (-) [2..] (oeis @70867))
     (map (oeisIx @70867.pred) $ zipWith (-) [2..] $ tail (oeis @70867))

instance OEIS 84471 where
  oeisIx 0 = 1
  oeisIx (succ->x) = 2 * (2 - d) * (oeisIx @84471.pred) x' + d  where (x',d) = divMod x 2

instance OEIS 84473 where
  oeisIx 0 = 1
  oeisIx (succ->x) = 2 * (if b == 1 then 1 else 8) * (oeisIx @84473.pred) x' + b
              where (x', b) = divMod x 2

instance OEIS 135414 where
  oeis = 1 : 1 : zipWith (-) [3..] (map (oeisIx @135414.pred) (oeis @135414))

instance OEIS 254531 where
  oeisIx = (+ 49) . round . (* 12) . logBase 2 . (/ 440) . (+27) . fi


instance OEIS 25480 where
  oeis = concat $ (tabf @25480)
  -- oeis = interleave [0..] (oeis @25480)
instance Table 25480 where
  tabf = iterate (\xs -> concat $
     transpose [xs, [length xs .. 2 * length xs - 1]]) [0]

instance OEIS 162247 where
  oeis = tablList @162247
instance Table 162247 where
  rowCol = rowCol_off @162247 @1 @1
  rowT   = rowT_off   @162247 @1
  tabl = map (concat . sortBy (comparing length)) $ tail fss where
     fss = [] : map fact [1..] where
           fact x = [x] : [d : fs | d <- [2..x], let (x',r) = divMod x d,
                                    r == 0, fs <- fss !! x', d <= head fs]
instance OEIS 50000 where
  oeis = 1 : f [1,0] where
     f xs'@ (x:xs) | x `div` 2 `elem` xs = 3 * x : f (3 * x : xs')
                   | otherwise = x `div` 2 : f (x `div` 2 : xs')

instance OEIS 257905 where
  oeis = 0 : f [0] [0] where
     f xs@ (x:_) ds = g [2 - x .. -1] where
       g [] = y : f (y:xs) (h:ds) where
                    y = x + h
                    (h:_) = [z | z <- [1..] \\ ds, x - z `notElem` xs]
       g (h:hs) | h `notElem` ds && y `notElem` xs = y : f (y:xs) (h:ds)
                | otherwise = g hs
                where y = x + h

instance OEIS 257906 where
  oeis = 0 : f [0] [1] where
     f xs@ (x:_) ds = g [2 - x .. -1] where
       g [] = y : f (y:xs) (h:ds) where
                    y = x + h
                    (h:_) = [z | z <- [1..] \\ ds, x - z `notElem` xs]
       g (h:hs) | h `notElem` ds && y `notElem` xs = y : f (y:xs) (h:ds)
                | otherwise = g hs
                where y = x + h

instance OEIS 257907 where
  oeis = 1 : f [0] [1] where
     f xs@ (x:_) ds = g [2 - x .. -1] where
       g [] = h : f ((x + h) : xs) (h : ds) where
                    (h:_) = [z | z <- [1..] \\ ds, x - z `notElem` xs]
       g (h:hs) | h `notElem` ds && y `notElem` xs = h : f (y:xs) (h:ds)
                | otherwise = g hs
                where y = x + h

instance OEIS 132739 where
  oeisIx = f . succ where
    f n
      | r > 0     = n
      | otherwise = f n'
      where (n',r) = divMod n 5

instance OEIS 181971 where
  oeis = tablList @181971
instance Table 181971 where
  tabl = map snd $ iterate f (1, [1]) where
     f (i, row) = (1 - i, zipWith (+) ([0] ++ row) (row ++ [i]))

instance OEIS 3602 where
  oeis = oeis003602
  -- oeisIx = (`div` 2) . (+ 1) . (oeisIx @265)

instance OEIS 181988 where
  oeis = oeis181988

instance OEIS 170942 where
  oeis = tablList @170942
instance Table 170942 where
  rowCol = rowCol_off @170942 @1 @1
  rowT n = map fps $ sort $ permutations [1..n] where
     fps perm = sum $ map (fi.fromEnum) $ zipWith (==) perm [1..n]
  tabf = map (rowT @170942) [1..]

instance OEIS 2658 where
  oeis = 1 : 1 : f [1,1] where
     f (x:xs) = y : f (y:x:xs) where y = x * sum xs + x * (x + 1) `div` 2

instance OEIS 63733 where
  oeis = 1 : f 0 [1] where
     f x ys@ (y:_) | u > 0 && u `notElem` ys = u : f (x + 1) (u : ys)
                  | otherwise               = v : f (x + 1) (v : ys)
                  where u = y - x; v = x + y

instance OEIS 62980 where
  oeis = 1 : 5 : f 2 [5,1] where
     f u vs'@ (v:vs) = w : f (u + 1) (w : vs') where
       w = 6 * u * v + sum (zipWith (*) vs_ $ reverse vs_)
       vs_ = init vs

instance OEIS 37444 where
  oeisIx n = p (map (^ 2) [1..]) (n^2) where
     p _      0 = 1
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 111650 where
  oeis = tablList @111650
instance Table 111650 where
  rowCol = rowCol_off @111650 @1 @1
  rowT   = rowT_off   @111650 @1
  tabl = iterate (\xs@ (x:_) -> map (+ 2) (x:xs)) [2]

instance OEIS 140513 where
  oeis = tablList @140513
instance Table 140513 where
  rowCol = rowCol_off @140513 @1 @1
  rowT   = rowT_off   @140513 @1
  tabl = iterate (\xs@ (x:_) -> map (* 2) (x:xs)) [2]

instance OEIS 151945 where
  oeis = 1 : 1 : f [2..] where
     f (x:xs) = p (take x (oeis @151945)) x : f xs
     p _ 0 = 1; p [] _ = 0
     p ds'@ (d:ds) m = if m < d then 0 else p ds' (m - d) + p ds m

instance OEIS 170949 where
  oeis = tablList @170949
instance Table 170949 where
  rowCol = rowCol_off @170949 @1 @1
  rowT   = rowT_off @170949 @1
  tabf = [1] : (map fst $ iterate f ([3,2,4], 3)) where
    f (xs@ (x:_), i) = ([x + i + 2] ++ (map (+ i) xs) ++ [x + i + 3], i + 2)

instance OEIS 199332 where
  oeis = tablList @199332
instance Table 199332 where
  rowCol = rowCol_off @199332 @1 @1
  rowT   = rowT_off   @199332 @1
  tabl = f [1..] [1..] where
     f (x:xs) ys'@ (y:ys) | odd x  = (replicate x y) : f xs ys
                         | even x = us : f xs vs
                         where (us,vs) = splitAt x ys'

instance OEIS 253415 where
  oeis = f [2..] 1 where
     f xs z = g xs where
       g (y:ys) = if mod z' y > 0 then g ys else x : f xs' (z + y)
                  where xs'@ (x:_) = delete y xs
       z' = z + 2

instance OEIS 245340 where
  oeis = 0 : f [1..] [1..] 0 (M.singleton 0 0) where
     f us'@ (u:us) vs'@ (v:vs) w m
       | u `M.member` m = (m M.! u) : f us vs' w m
       | otherwise    = g (reverse[w-v,w- 2*v..1] ++ [w+v,w+2*v..]) where
       g (x:xs) = if x `M.member` m then g xs else f us' vs x $ M.insert x v m

instance OEIS 105870 where
  oeis = 0 : xs where
    xs = 1 : 1 : zipWith (\u v -> (u + v) `mod` 7) (tail xs) xs


instance OEIS 1650 where
  oeis = tablList @1650
instance Table 1650 where
  rowCol = rowCol_off @1650 @1 @1
  rowT   = rowT_off   @1650 @1
  tabf   = iterate (\xs@ (x:_) -> map (+ 2) (x:x:xs)) [1]

instance OEIS 39723 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @39723) n' * 10 + m where
     (n',m) = if r < 0 then (q + 1, r + 10) else qr where
              qr@ (q, r) = quotRem n (negate 10)

instance OEIS 48645 where
  oeis = tablList @48645
instance Table 48645 where
  rowCol = rowCol_off @48645 @1 @1
  rowT   = rowT_off   @48645 @1
  tabl   = iterate (\xs -> insert (2 * head xs + 1) $ map ((* 2)) xs) [1]

instance OEIS 70861 where
  oeis = tablList @70861
instance Table 70861 where
  tabf = [1] : f 2 [1] where
     f n ps = ps' : f (n+1) ps' where ps' = m ps $ map (n*) ps
     m []         ys = ys
     m xs'@ (x:xs) ys'@ (y:ys)
         | x < y     = x : m xs ys'
         | x == y    = x : m xs ys
         | otherwise = y : m xs' ys

instance OEIS 81118 where
  oeis = tablList @81118
instance Table 81118 where
  rowCol = rowCol_off @81118 @1 @1
  rowT   = rowT_off   @81118 @1
  tabl  = iterate
     (\row -> (map ((+ 1) . (* 2)) row) ++ [4 * (head row) + 1]) [1]

instance OEIS 108730 where
  oeis = tablList @108730
instance Table 108730 where
  rowCol = rowCol_off @108730 @1 @1
  rowT = f . group . reverse . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) where
     f [] = []
     f [os] = replicate (length os) 0
     f (os:zs:dss) = replicate (length os - 1) 0 ++ [length zs] ++ f dss
  tabf = map (rowT @108730) [1..]
