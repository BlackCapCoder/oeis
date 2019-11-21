module OEIS.Part2 () where

import OEIS.Common
import OEIS.Part1

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
import Data.Ord (Down (..))
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


instance OEIS 9 where
  oeisIx n = (oeis @9) !! n
  oeis = map (pM 1) [0..] where
     pM = memo2 integral integral p
     p _ 0 = 1
     p k m | m < k     = 0
           | otherwise = pM (k + 1) (m - k) + pM (k + 1) m

instance OEIS 41 where
  oeisIx n = (oeis @41) !! n
  oeis = map (p' 1) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 1) m

instance OEIS 203 where
  oeisIx n = fi $ A.sigma 1 (1 + fi n :: Int)

instance OEIS 385 where
  oeisIx (succ->n) = sum $ zipWith (*) sigmas $ reverse sigmas
    where
      sigmas = take n $ oeis @203

instance OEIS 396 where
  oeis = [ x | x <- [1..], oeisIx @203 x == 2 * x ]

instance OEIS 404 where
  oeis = findIndices (> 0) $ oeis @25426

instance OEIS 408 where
  oeis = filter ((> 0) . oeisIx @25427) [1..]

instance OEIS 660 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : [1..])

instance OEIS 674 where
  oeisIx n = sum $ zipWith (*) ((rowT @109449) n) (1 : repeat 2)

instance OEIS 688 where
  oeisIx = product . map (oeisIx @41) . (rowT @124010) . succ

instance OEIS 718 where
  oeisIx n = sum $ zipWith (*) ((rowT @109449) n) (1 : tail (oeis @217))

instance OEIS 732 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (oeis @8578)

instance OEIS 978 where
 oeis = filter ((== 1) . (oeisIx @10051) . pred . (oeisIx @1045)) (oeis @65091)

instance OEIS 1065 where
  oeisIx n = (oeisIx @203 n) - n - 1

instance OEIS 1255 where
  oeisIx = (^ 2) . oeisIx @41

instance OEIS 1792 where
  oeis = scanl1 (+) (oeis @45623)

instance OEIS 1859 where
  oeisIx n = (oeisIx @217) n + (oeisIx @2620) (n + 1)

instance OEIS 2124 where
 oeis = 1 : f 1 [] (oeis @65091) where
     f x qs ps'@ (p:ps)
       | p <= x    = f x (p:qs) ps
       | otherwise = sum (map ((oeisIx @2124) . (x -)) qs) : f (x + 1) qs ps'

instance OEIS 2577 where
  oeis = f [1] where
     f xs = (p' xs $ last xs) : f (1 : map (* 2) xs)
     p' = memo2 (list integral) integral p
     p _ 0 = 1; p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m


instance OEIS 3107 where
  oeis = map (p' 2) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m | m < fib   = 0
           | otherwise = p' k (m - fib) + p' (k + 1) m where fib = (oeisIx @45) k

instance OEIS 3136 where
  oeis = f 0 $ S.singleton 0 where
    f x s
      | m < x ^ 2 = m : f x s'
      | otherwise = m : f x'
         (S.union s' $ S.fromList $ map (\y -> x'^2+ (x'+y)*y) [0..x'])
      where
        x' = x + 1
        (m,s') = S.deleteFindMin s

instance OEIS 3137 where
  oeis = tablList @3137
instance Table 3137 where
  rowCol = rowCol_off @3137 @1 @0
  rowT   = rowT_off @3137 @1
  tabf   = map reverse $ tail (tabf @30341)

instance OEIS 3401 where
  oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @209229) (oeis @10)

instance OEIS 4169 where
  oeis = map succ $ elemIndices 0 $ map (oeisIx @209229) (oeis @10)

instance OEIS 5097 where
  oeisIx = (`div` 2) . (oeisIx @65091)

instance OEIS 5117 where
  oeis = filter ((== 1) . (oeisIx @8966) . pred) [1..]

instance OEIS 5185 where
  oeis = 1 : 1 : zipWith (+)
     (map f $ zipWith (-) [3..] (oeis @5185))
     (map f $ zipWith (-) [3..] $ tail (oeis @5185))
    where
      f = oeisIx @5185 . pred

instance OEIS 5211 where
  oeis = oeis5211

instance OEIS 24014 where
  -- oeis = oeis24014
  oeisIx n = 2^n-n^4

(oeis5211, oeis24014) = unzip $ (1, 1) :  f 1 1
  where
    f i x | y > x     = (y, i) : f (i + 1) y
          | otherwise = f (i + 1) x
          where y = (oeisIx @5210) i

instance OEIS 5408 where
  oeisIx = (+ 1) . (* 2)
  oeis   = [1, 3 ..]

instance OEIS 5713 where
  oeis = 1 : 1 : concat (sibb [0] [1,1]) where
     sibb xs ys = zs : sibb ys zs where zs = xs ++ ys

instance OEIS 5831 where
  oeis = 0:1:zipWith (*) (tail (oeis @5831)) (map succ (oeis @5831))

instance OEIS 5900 where
  oeisIx n = sum $ zipWith (*) odds $ reverse odds
    where odds = take n (oeis @5408)
  oeis = scanl (+) 0 (oeis @1844)

instance OEIS 5917 where
  oeis = map sum $ f 1 [1, 3 ..] where
     f x ws = us : f (x + 2) vs where (us, vs) = splitAt x ws

instance OEIS 6047 where
  oeisIx = sum . map signum . (rowT @83093)

instance OEIS 6093 where
  oeisIx = (subtract 1) . (oeisIx @40)

instance OEIS 6094 where
  oeis = zipWith (*) (oeis @40) (oeis @65091)

instance OEIS 6190 where
  oeis = 0 : 1 : zipWith (+) (map (* 3) $ tail (oeis @6190)) (oeis @6190)

instance OEIS 6257 where
  oeis = 0 : 1 : (map (+ 1) $ zipWith mod (map (+ 1) $ tail (oeis @6257)) [2..])

instance OEIS 6277 where
  oeis = 1 : fix \r -> 1 : scanl ((*) . (+ 1)) 2 r

instance OEIS 6318 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where
       y = head xs + sum (zipWith (*) xs $ reverse xs)

instance OEIS 6337 where
  oeis = f [1] where
     f xs = ys ++ f ys where
            ys = concatMap (\z -> if z == 1 then [1,2] else [1,1,2]) xs

instance OEIS 6355 where
  oeis = 1 : fib2s where
     fib2s = 0 : map (+ 1) (scanl (+) 1 fib2s)

instance OEIS 6370 where
  oeisIx n | m /= 0    = 3 * n + 1
           | otherwise = n' where (n',m) = divMod n 2

instance OEIS 6516 where
  oeis = 0 : 1 :
      zipWith (-) (map (* 6) $ tail (oeis @6516)) (map (* 8) (oeis @6516))

instance OEIS 6530 where -- TODO: Optimize this- it's used a lot
  oeisIx n
    | null fs = 1
    | let     = fi . unPrime . fst $ last fs
    where fs  = factorise (1 + fi n :: Int)

instance OEIS 6666 where
  oeisIx = length . filter even . takeWhile (> 1) . (iterate (oeisIx @6370)) . succ


instance OEIS 6882 where
  oeis = 1 : 1 : zipWith (*) [2..] (oeis @6882)

instance OEIS 6968 where
  oeisIx = fi . lenRom 3 . fi . succ where
     lenRom 0 z = z
     lenRom p z = [0, 1, 2, 3, 2, 1, 2, 3, 4, 2] !! m + lenRom (p - 1) z'
                  where (z',m) = divMod z 10

instance OEIS 6985 where
  oeis = 1 : map (oeisIx @45 . (+ 2)) (oeis @6985)

instance OEIS 6996 where
  oeisIx n = (rowCol @83093) (2 * n) n

instance OEIS 6999 where
  oeis = 0 : map ((`div` 2) . (+ 2) . (* 3)) (oeis @6999)

instance OEIS 7000 where
  oeis = map (p' 1) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m | m < fib   = 0
           | otherwise = p' k (m - fib) + p' (k + 1) m where fib = (oeisIx @45) k

instance OEIS 7061 where
  oeis = 1 : f [1] where
     f us = a' : f (us ++ [a']) where
       a' = b $ reverse $ map (`splitAt` us) [0..length us - 1] where
          b ((xs,ys):xyss) | vs `isSuffixOf` xs = 3 - head ys
                           | otherwise          = b xyss
       vs = fromJust $ find (`isInfixOf` init us) $ tails us

instance OEIS 7448 where
  oeis = f [0] [0] where
     f (x:xs) (y:ys) = z : f (xs ++ [2*z,2*z]) (ys ++ [3*z,3*z,3*z])
       where z = 1 + min x y

instance OEIS 7477 where
  oeis = 1 : 1 : f [1,1] where
     f xs = y : f (y:xs) where y = sum $ zipWith (*) (tail xs) (reverse xs)

instance OEIS 7526 where
  oeis = 0 : zipWith (*) [1..] (map (+ 1) (oeis @7526))

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

instance OEIS 7606 where
  oeis = takeSkip 1 [1..] where
     takeSkip k xs = take k xs ++ takeSkip (k + 2) (drop (2*k + 1) xs)

instance OEIS 7607 where
  oeis = skipTake 1 [1..] where
     skipTake k xs = take (k + 1) (drop k xs)
                     ++ skipTake (k + 2) (drop (2*k + 1) xs)

instance OEIS 7814 where
  oeisIx (succ->n) = if m == 0 then 1 + (oeisIx @7814 . pred) n' else 0
              where (n', m) = divMod n 2

instance OEIS 7821 where
  oeisIx = (oeisIx @40) . pred . (oeisIx @18252)

instance OEIS 7918 where
  oeis = 2 : 2 : 2 : concat do
    zipWith (\p q -> (replicate (q - p) q))
            (oeis @40) $ tail (oeis @40)

instance OEIS 7947 where
  oeisIx = product . (rowT @27748) . succ

instance OEIS 7952 where
  oeis = f 1 [0..] where
     f k (x:xs) = x : f (k + 1) (g xs) where
       g ws = us ++ (g vs) where (us, _:vs) = splitAt k ws

instance OEIS 8344 where
  oeis = 0 : f 0 [1..] where
     f x (z:zs) = y : f y zs where y = if x < z then x + z else x - z

instance OEIS 8472 where
  oeisIx 0 = 0
  oeisIx n = sum . (rowT @27748) $ succ n

instance OEIS 8557 where
  oeis = iterate (oeisIx @7094) 8

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

instance OEIS 8619 where
  oeisIx = (+ 1) . (`div` 2)
  oeis = concatMap (\x -> [x,x]) [1..]

instance OEIS 8684 where
  oeis = concatMap (enumFromTo 1) [31,28,31,30,31,30,31,31,30,31,30,31]

instance OEIS 8685 where
  oeis = concatMap t [1..] where
     t y = [31, 28 + leap, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
           where leap = if mod y 4 == 0 &&
                           (mod y 100 > 0 || mod y 400 == 0) then 1 else 0

instance OEIS 8776 where
  oeisIx = (* 2) . (3 ^)
  oeis = iterate (* 3) 2

instance OEIS 8846 where
  oeis = filter f [5..] where
     f n = all ((== 1) . (`mod` 4)) $ filter ((== 0) . (n `mod`)) [1..n]

instance OEIS 8864 where
  oeisIx = (+ 1) . (oeisIx @40)

instance OEIS 8873 where
  oeis = iterate (oeisIx @6370) 97

instance OEIS 8874 where
  oeis = iterate (oeisIx @6370) 63

instance OEIS 8875 where
  oeis = drop 2 (oeis @8874)

instance OEIS 8876 where
  oeis = iterate (oeisIx @6370) 81

instance OEIS 8877 where
  oeis = iterate (oeisIx @6370) 57

instance OEIS 8904 where
  oeis = 1 : 1 : f 2 1 where
     f n x = x' `mod` 10 : f (n+1) x' where
        x' = g (n * x) where
           g m | m `mod` 5 > 0 = m
               | otherwise     = g (m `div` 10)

instance OEIS 8908 where
  oeisIx = length . (rowT @70165) . succ

instance OEIS 8919 where
  oeis = [x | x <- [1..], let (x',m) = divMod (oeisIx @4086 x) x, m == 0, x' > 1]

instance OEIS 8935 where
  oeisIx = f 1 . succ where
     f k x | x == 0    = 0
           | r == 0    = f (k+1) x'
           | otherwise = k^2 + f (k+1) x' where (x',r) = divMod x 2

instance OEIS 8937 where
  oeis = tail $ scanl1 (+) (oeis @73)

instance OEIS 8949 where
  oeis = tablList @8949
instance Table 8949 where
  tabl = map (scanl1 (+)) (tabl @7318)

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

instance OEIS 10054 where
  oeisIx = (oeisIx @10052) . (+ 1) . (* 8)
  oeis   = concatMap (\x -> 1 : replicate x 0) [0..]

instance OEIS 10055 where
  oeisIx n = if (oeisIx @1221) n <= 1 then 1 else 0

instance OEIS 10702 where
  oeisIx = (+ 3) . (`mod` 2)
  oeis = cycle [3,4]

instance OEIS 10785 where
  oeis = 0 : r [1..9] where
     r (x:xs) = x : r (xs ++ [10*x + x `mod` 10])

instance OEIS 11782 where
  oeis = 1 : scanl1 (+) (oeis @11782)

instance OEIS 14011 where
  oeis = 1 : f 2 [1] where
     f u vs = w : f (u + 1) (w : vs) where
       w = maximum $ zipWith (*) [u, u - 1 ..] $ map (u -) vs

instance OEIS 14148 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 2

instance OEIS 14150 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 3

instance OEIS 14311 where
  oeis = [2^x + 2^y + 2^z | x <- [2..], y <- [1..x - 1], z <- [0..y - 1]]

instance OEIS 14631 where
  oeis = 1 : (nub $ concatMap tail (tabf @34868))

instance OEIS 14688 where
  oeis = zipWith (+) [1..] (oeis @40)

instance OEIS 14707 where
  oeis = f 0 $ cycle [0,0,1,0] where
     f i (x:_:xs) = x : (oeisIx @14707) i : f (i+1) xs

instance OEIS 14980 where
  oeis = iterate (oeisIx @2620) 5

instance OEIS 15614 where
  oeisIx = (subtract 1) . (oeisIx @2088) . succ

instance OEIS 15632 where
  oeisIx (succ->n) = genericLength [ (x,y,z) | z <- [1..n], y <- [1..z], gcd y z == 1,
                                x <- [1..y], gcd x z == 1, gcd x y == 1]

instance OEIS 15633 where
  oeisIx (succ.succ->n) = genericLength [ (x,y,z) | x <- [2..n], y <- [x..n], z <- [y..n],
                                gcd (gcd x y) z == 1]

instance OEIS 16069 where
  oeis = filter ((== 2) . length . nub . show . fi . (^ 2)) [0..]

instance OEIS 18252 where
  oeis = map (+1) $ filter ((== 0) . (oeisIx @10051)) [0..]

instance OEIS 18834 where
  oeis = map fi $ filter (\(x :: Int) -> show x `isInfixOf` show (x^2)) [0..]

instance OEIS 19446 where
  oeis = 1 : zipWith (-) [3..] (map (oeisIx @19446 . pred) (oeis @19446))

instance OEIS 19464 where
  oeis = 1 : concat (unfoldr ma (1, [1, 1])) where
     ma (x, [_, j]) = Just (ij', (x + 1, ij')) where ij' = [x * j, x * j + x]

instance OEIS 20522 where
  oeisIx = (* 2) . (oeisIx @6516)

instance OEIS 20650 where
  oeis = map numerator ks where
     ks = 1 : concat (transpose [map (+ 1) ks, map (recip . (+ 1)) ks])

instance OEIS 20651 where
  oeis = map denominator ks where
     ks = 1 : concat (transpose [map (+ 1) ks, map (recip . (+ 1)) ks])

instance OEIS 22831 where
  oeis = 2 : f 2 (tail (oeis @40)) where
     f x (p:ps) | x' > 0    = x' : f x' ps
                | otherwise = xp : f xp ps where x' = x - p; xp = x + p

instance OEIS 22941 where
  oeis = 1 : 2 : f 2 [3..] where
     f x (z:zs) = y : f y (delete y zs) where y = x + z

instance OEIS 23523 where
  oeis =  map (+ 1) $ zipWith (*) (oeis @40) (oeis @8578)

instance OEIS 23626 where
  oeis = f (oeis @40) [1] where
     f (p:ps) rs = (sum $ zipWith (*) rs (oeis @8578)) : f ps (p : rs)

instance OEIS 23705 where
  oeis = iterate f 1 where
     f x = 1 + if r < 3 then x else 4 * f x'
           where (x', r) = divMod x 4

instance OEIS 23717 where
  oeis = filter f [0..] where
     f x = x < 3 || (q < 3 && f x') where (x', q) = divMod x 4

instance OEIS 25547 where
  oeis = scanl1 lcm (oeis @5408)

instance OEIS 27434 where
  oeisIx = (+ 1) . (oeisIx @196) . (subtract 3) . (* 4) . succ
  oeis = 2 : concat (map (\x -> replicate (x `div` 2) x) [3..])

instance OEIS 27709 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @27434 . pred) n * 2

instance OEIS 27746 where
  oeis = tablList @27746
instance Table 27746 where
  rowCol = rowCol_off @27746 @1 @1
  tabl = map (rowT @27746) [1..]
  rowT 1 = [1]
  rowT n = unfoldr fact n where
     fact 1 = Nothing
     fact x = Just (p, x `div` p) where p = (oeisIx @20639 . pred) x

instance OEIS 27751 where
  oeis = tablList @27751
instance Table 27751 where
  rowCol = rowCol_off @27751 @1 @1
  rowT   = rowT_off @27751 @1
  tabf   = [1] : map init (tail (tabf @27750))

instance OEIS 28290 where
  oeis = map (p' 0) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p 5 _ = 0
     p k m | m < parts !! k = 0
           | otherwise = p' k (m - parts !! k) + p' (k + 1) m
     parts = [1, 2, 3, 5, 8]

instance OEIS 28846 where
  oeis = f [1] where
     f ds = foldr (\d v -> 10 * v + d) 0 ds : f (s ds)
     s [] = [1]; s (8:ds) = 1 : s ds; s (d:ds) = 2*d : ds

instance OEIS 29145 where
  oeis = map (p' 0) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p 4 _ = 0
     p k m | m < parts !! k = 0
           | otherwise = p' k (m - parts !! k) + p' (k + 1) m
     parts = [2, 3, 5, 8]

instance OEIS 29744 where
  oeis = 1 : iterate
     (\x -> if x `mod` 3 == 0 then 4 * x `div` 3 else 3 * x `div` 2) 2

instance OEIS 29793 where
  oeis = filter (\x -> digs x == digs (x^2)) [0..]
     where digs = sort . nub . show . fi

instance OEIS 29942 where
  oeis = [fi x | (x :: Int) <- [0..], show x `isInfixOf` show (x^3)]

instance OEIS 30067 where
  oeis = concat $ transpose [scanl (+) 1 (oeis @30067), (oeis @30067)]

instance OEIS 30096 where
  oeis = filter f (oeis @40) where
     f x = odd d && (x < 10 || f x') where (x', d) = divMod x 10

instance OEIS 30099 where
  oeis = filter (null . (intersect "86420") . show . fi . (^ 3)) [1,3..]

instance OEIS 30101 where
  oeisIx = f 0 where
     f y 0 = y
     f y x = f (2 * y + b) x'  where (x', b) = divMod x 2

instance OEIS 30102 where
  oeisIx = foldl (\v d -> 3 * v + d) 0 . (rowT @30341)

instance OEIS 30103 where
  oeisIx n = foldl (\v d -> 4*v + d) 0 $ unfoldr dig n where
      dig x = if x == 0 then Nothing else Just $ swap $ divMod x 4

instance OEIS 30109 where
  oeisIx = flip div 2 . subtract 1 . (oeisIx @30101) . succ

instance OEIS 30124 where
  oeis = figureDiff 1 [2..] where
     figureDiff n (x:xs) = x : figureDiff n' (delete n' xs) where n' = n + x

instance OEIS 30130 where
  oeis = filter ((== 1) . (oeisIx @23416)) [0..]

instance OEIS 30132 where
  oeis =
     0 : 1 : map (oeisIx @7953) (zipWith (+) (oeis @30132) (tail (oeis @30132)))

instance OEIS 30133 where
  oeis =
     2 : 1 : map (oeisIx @7953) (zipWith (+) (oeis @30133) $ tail (oeis @30133))

instance OEIS 30141 where
  oeis = filter ((== 1) . (oeisIx @228710)) [0..]

instance OEIS 30142 where
  oeis = filter odd (oeis @30141)

instance OEIS 30143 where
  oeis = filter even (oeis @30141)

instance OEIS 30144 where
  oeis = filter ((== 1) . (oeisIx @228710)) (oeis @40)

instance OEIS 30195 where
  oeis = 0 : 1 : map (* 3) (zipWith (+) (oeis @30195) (tail (oeis @30195)))

instance OEIS 30230 where
  oeis = filter (odd . (oeisIx @1221) . pred) [1..]

instance OEIS 30231 where
  oeis = filter (even . (oeisIx @1221) . pred) [1..]

instance OEIS 30283 where
  oeis = 0 : f 1 9 0 where
     f u v w = w' : f u' v' w' where
       w' = until (> w) ((+ v) . (* 10)) u
       (u',v') = h u v
       h 1 0 = (2,2); h 9 0 = (1,1); h 9 1 = (2,0); h 9 9 = (1,0)
       h u 2 = (u+1,0); h u v = (u+1,1-v)

instance OEIS 30341 where
  oeis = tablList @30341
instance Table 30341 where
  tabf = iterate succ [0] where
     succ []     = [1]
     succ (2:ts) = 0 : succ ts
     succ (t:ts) = (t + 1) : ts

instance OEIS 30530 where
  oeis = 0 : concatMap (\n -> unfoldr
     (\x -> if x == 0 then Nothing else Just (n, div x 2)) n) [1..]

instance OEIS 31298 where
  oeis = tablList @31298
instance Table 31298 where
  tabf = iterate succ [0] where
     succ []     = [1]
     succ (9:ds) = 0 : succ ds
     succ (d:ds) = (d + 1) : ds

instance OEIS 31368 where
  oeisIx = (oeisIx @40) . (* 2)
  oeis = map (oeisIx @40) [0, 2 ..]

instance OEIS 32352 where
  oeis = filter
     (\x -> all (== 0) $ map (oeisIx @10051 . pred . (10*x +)) [1..9]) [1..]

instance OEIS 32740 where
  oeis = [fi x | (x :: Int) <- [0..], show x `isInfixOf` (show $ 2 ^ x)]

instance OEIS 32742 where
  oeisIx n = succ n `div` (oeisIx @20639) n

instance OEIS 32924 where
  oeis = iterate f 1 where
     f x = 1 + if r < 2 then x else 3 * f x'  where (x', r) = divMod x 3

instance OEIS 32925 where
  oeis = 1 : 2 : (concat $ transpose [map (+ 1) fs, map (+ 2) fs])
                 where fs = map (* 4) (oeis @32925)

instance OEIS 33048 where
  oeis = (0:) $ filter (all (< 2) . unfoldr (\x ->
     if x == 0 then Nothing else Just $ swap $ divMod x 12)) [1..]

instance OEIS 33493 where
  oeisIx = sum . (rowT @70165) . succ

instance OEIS 33496 where
  oeis = 1 : filter f [2, 4 ..] where
     f x = x == maximum (takeWhile (/= 1) $ iterate (oeisIx @6370) x)

instance OEIS 33627 where
  oeis = f [1..] [] where
     f (x:xs) ys = x : f (xs \\ (map (+ x) ys)) (x:ys)

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

instance OEIS 34856 where
  oeisIx = subtract 1 . (oeisIx @96) . succ

instance OEIS 34868 where
  oeis = tablList @34868
instance Table 34868 where
  tabf = map reverse (tabf @34869)

instance OEIS 34869 where
  oeis = tablList @34869
instance Table 34869 where
  tabf = [1] : f 0 [1] where
     f 0 us'@ (_:us) = ys : f 1 ys where
                      ys = zipWith (+) us' (us ++ [0])
     f 1 vs@ (v:_) = ys : f 0 ys where
                    ys = zipWith (+) (vs ++ [0]) ([v] ++ vs)

instance OEIS 35294 where
  oeis = f 1 where
     f x = (p' 1 (x - 1)) : f (x + 2)
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 2) m

instance OEIS 36234 where
  oeisIx = (+ 1) . (oeisIx @720)

instance OEIS 36552 where
  oeis = g [1..] where
     g (x:xs) = x : (2*x) : (g $ delete (2*x) xs)

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

instance OEIS 38107 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @720) . pred $ (oeisIx @290) n

instance OEIS 38219 where
  oeis = 0 : f [0] where
     f us = a' : f (us ++ [a']) where
          a' = b $ reverse $ map (`splitAt` us) [0..length us - 1] where
             b ((xs,ys):xyss) | vs `isSuffixOf` xs = 1 - head ys
                              | otherwise          = b xyss
          vs = fromJust $ find (`isInfixOf` init us) $ tails us

instance OEIS 38365 where
  oeis = map fi $ filter (\(x :: Int) -> null (show (2*x) `intersect` show x)) [1..]

instance OEIS 38566 where
  oeis = tablList @38566
instance Table 38566 where
  rowCol = rowCol_off @38566 @1 @1
  rowT   = rowT_off @38566 @1
  tabf   = zipWith (\v ws -> filter ((== 1) . (gcd v)) ws) [1..] (tabl @2260)

instance OEIS 38770 where
  oeis = filter f [1..] where
     f u = g u where
       g v = v > 0 && (((d == 0 || r > 0) && g v') || r == 0)
             where (v',d) = divMod v 10; r = mod u d

instance OEIS 39649 where
  oeisIx = (+ 1) . (oeisIx @10)

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

instance OEIS 40014 where
  oeisIx = (oeisIx @720) . pred . (oeisIx @149)

instance OEIS 45331 where
  oeis = filter ((< 4) . (`mod` 6)) (oeis @40)

instance OEIS 45623 where
  oeis = tail $ f (oeis @11782) [] where
     f (u:us) vs = sum (zipWith (*) vs $ reverse ws) : f us ws
       where ws = u : vs

instance OEIS 45708 where
  oeis = filter ((== 2) . (oeisIx @30)) (oeis @40)

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

instance OEIS 45954 where
  oeis =  2 : sieve 2 [2,4..] where
     sieve k xs = z : sieve (k + 1) (lucky xs) where
        z = xs !! (k - 1 )
        lucky ws = us ++ lucky vs where
              (us, _:vs) = splitAt (z - 1) ws

instance OEIS 46022 where
  oeis = [1..4] ++ drop 2 (oeis @40)

instance OEIS 46660 where
  oeisIx (succ->n) = fi (sum es) - length es
    where
      es = snd $ unzip $ factorise (fi n)

--   oeisIx 0 = 0
--   oeisIx 1 = 0
--   oeisIx n = mexp (oeisIx' n)
--   oeisIx' n = (map trymove [0.. (div (n - 1) 2)])
--     where trymove k = nimSum (oeis !! k) (oeis !! (n-k-1))

instance OEIS 46992 where
  oeis = scanl1 (+) (oeis @720)

instance OEIS 47209 where
  oeisIx = (flip div 2) . (subtract 2) . (* 5) . succ
  oeis = 1 : 4 : (map (+ 5) (oeis @47209))

instance OEIS 47221 where
  oeisIx (succ->n) = 5 * ((n - 1) `div` 2) + 3 - n `mod` 2
  oeis = 2 : 3 : map (+ 5) (oeis @47221)

instance OEIS 47228 where
  oeis = 2 : 3 : 4 : map (+ 6) (oeis @47228)

instance OEIS 47241 where
  oeis = 1 : 3 : map (+ 6) (oeis @47241)

instance OEIS 47242 where
  oeis = elemIndices 0 (oeis @214090)

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

instance OEIS 47355 where
  oeis = scanl (+) 0 (oeis @10702)

instance OEIS 47520 where
  oeisIx n = sum $ zipWith (*) (reverse $ genericTake n $ tail (oeis @290)) (oeis @79)

instance OEIS 47522 where
  oeis = 1 : 7 : map (+ 8) (oeis @47522)

instance OEIS 47621 where
  oeis = 3 : 5 : map (+ 8) (oeis @47621)

instance OEIS 47778 where
  oeisIx = (foldl (\v d -> 2*v + d) 0) . concatMap (reverse . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)) .
     enumFromTo 1 . succ

instance OEIS 47781 where
  oeisIx n = (rowCol @49600) (2 * n) n

instance OEIS 47892 where
  oeis = iterate (oeisIx @57147) 2

instance OEIS 47897 where
  oeis = iterate (oeisIx @57147) 5

instance OEIS 47898 where
  oeis = iterate (oeisIx @57147) 6

instance OEIS 47899 where
  oeis = iterate (oeisIx @57147) 7

instance OEIS 47900 where
  oeis = iterate (oeisIx @57147) 8

instance OEIS 47901 where
  oeis = iterate (oeisIx @57147) 9

instance OEIS 47902 where
  oeis = iterate (oeisIx @57147) 11

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

instance OEIS 47912 where
  oeis = iterate (oeisIx @57147) 3

instance OEIS 48321 where
  oeis = filter f [0..] where
     f x = all (< 0) $ zipWith (-) (tail zs) zs
           where zs =  map length $ group $ show $ fi x

instance OEIS 48574 where
  oeis = f (drop 2 (oeis @41)) [1] where
    f (p:ps) rs = (sum $ zipWith (*) rs $ tail (oeis @41)) : f ps (p : rs)

instance OEIS 48859 where
  oeis = f 2 [1..] where
     f k xs = us ++ f (k + 1) (drop (k + 1) vs)
              where (us, vs) = splitAt k xs

instance OEIS 48877 where
  oeis = 1 : 8 : zipWith (+) (oeis @48877) (map (* 4) $ tail (oeis @48877))

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

instance OEIS 49084 where
  oeis = unfoldr x (1, 1, (oeis @40)) where
     x (i, z, ps'@ (p:ps)) | i == p = Just (z, (i + 1, z + 1, ps))
                          | i /= p = Just (0, (i + 1, z, ps'))

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

instance OEIS 49439 where
  oeis = filter (\x -> ((length $ oddDivs x) `elem` oddDivs x)) [1..]
     where oddDivs n = [d | d <- [1,3..n], mod n d == 0]

instance OEIS 49600 where
  oeis = tablList @49600
instance Table 49600 where
  tabl = [0] : map (0 :) (tabl @208341)

instance OEIS 50376 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @100995) . pred) [1..]

instance OEIS 50873 where
  oeis = tablList @50873
instance Table 50873 where
  rowCol = gcd
  rowT   = rowT_off @50873 @1
  tabl   = zipWith (map . gcd) [1..] (tabl @2260)

instance OEIS 50909 where
  oeis = iterate (oeisIx @217) 9

instance OEIS 50935 where
  oeis = 0 : 0 : 1 : zipWith (-) (drop 2 (oeis @50935)) (oeis @50935)

instance OEIS 51120 where
  oeis = 1 : f [1] where
    f xs = seen ++ (f $ xs ++ seen) where
      seen = look (reverse $ map length $ group xs') (reverse $ nub xs')
      xs' = sort xs
      look [] []               = []
      look (cnt:cnts) (nr:nrs) = cnt : nr : look cnts nrs

instance OEIS 51173 where
  oeis = tablList @51173
instance Table 51173 where
  rowCol = lcm
  rowT   = rowT_off @51173 @1
  tabl   = map (\x -> map (lcm x) [1..x]) [1..]

instance OEIS 51193 where
  oeisIx = sum . (rowT @51173) . succ

instance OEIS 51537 where
  oeis = tablList @51537
instance Table 51537 where
  rowCol = rowCol_off @51537 @1 @1
  rowT   = rowT_off   @51537 @1
  tabl = zipWith (zipWith div) (tabl @51173) (tabl @50873)

instance OEIS 51612 where
  oeisIx n = oeisIx @203 n - oeisIx @10 n

instance OEIS 51613 where
  oeisIx = p 1 2 where
     p x _ 0 = 1
     p x k m | m < qq       = 0
             | mod x q == 0 = p x (k + 1) m
             | otherwise    = p (q * x) (k + 1) (m - qq) + p x (k + 1) m
             where
               q  = oeisIx @25473 $ pred k
               qq = oeisIx @961   $ pred k

instance OEIS 51638 where
  oeisIx = sum . (rowT @83093)

instance OEIS 51840 where
  oeis = map floor vs where
     vs = iterate (\x -> x * (4 - 3 * x)) 0.1

instance OEIS 52001 where
  oeis = filter even (oeis @41)

instance OEIS 52002 where
  oeis = findIndices odd (oeis @41)

instance OEIS 52003 where
  oeis = tail $ filter odd (oeis @41)

instance OEIS 52147 where
  oeisIx = (+ 2) . (oeisIx @40)

instance OEIS 52382 where
  oeis = iterate f 1 where
    f x = 1 + if r < 9 then x else 10 * f x' where (x', r) = divMod x 10

instance OEIS 52938 where
  oeis = 1 : 3 : 2 : zipWith (-) [5..] (oeis @52938)

instance OEIS 52955 where
  oeis = 1 : 2 : map ((+ 1) . (* 2)) (oeis @52955)

instance OEIS 53398 where
  oeis = tablList @53398
instance Table 53398 where
  rowCol n k = (oeisIx @7814 . pred) $ (rowCol @3986) (n - 1) (k - 1) + 1
  rowT n = map (rowCol @53398 n) [1..n]
  tabl = map (rowT @53398) [1..]

instance OEIS 54385 where
  oeis = map (floor . (* e') . fi) [1..]
     where e' = e / (e - 1); e = exp 1

instance OEIS 54685 where
  oeis = map (p' 2) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < pp then 0 else p' (k + 1) (m - pp) + p' (k + 1) m
             where pp = oeisIx @961 $ pred k

instance OEIS 55045 where
  oeis = filter ((== 5) . (flip mod 8) . f) [1..] where
     f x = if r == 0 then f x' else x  where (x', r) = divMod x 4

instance OEIS 55048 where
  oeis = filter (s 0) [1..] where
     s t u | m > 0  = even t && m == 2
           | m == 0 = s (t + 1) u' where (u',m) = divMod u 3

instance OEIS 55642 where
  oeisIx n = length $ show (fi n)

instance OEIS 55790 where
  oeis = 0 : 2 : zipWith (+)
     (zipWith (*) [0..] (oeis @55790)) (zipWith (*) [2..] $ tail (oeis @55790))

instance OEIS 55944 where
  oeisIx n = n + (oeisIx @30101) n

instance OEIS 55948 where
  oeisIx n = n + (oeisIx @30103) n

instance OEIS 56020 where
  oeis = 1 : 8 : map (+ 9) (oeis @56020)

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

instance OEIS 56924 where
  oeisIx = (`div` 2) . (oeisIx @5)

instance OEIS 56942 where
  oeis = concatMap (\x -> (x ^ 2) : (take x $ repeat (x * (x + 1)))) [0..]

instance OEIS 56964 where
  oeisIx n = n + oeisIx @4086 n

instance OEIS 57147 where
  oeisIx n = (oeisIx @7953) n * n

instance OEIS 57154 where
  oeis = g [1] [2..] [1] where
     g ds (a:as) us
       | null (ds' `intersect` us) = g ds' (as \\ ds') (us `union` ds')
       | otherwise = a : g ds as us
       where ds' = scanl (+) a ds

instance OEIS 57194 where
  oeis = 1 : f 1 1 where
     f u v = w : f (u * w) (v + w) where w = u * v

instance OEIS 57427 where
  oeisIx = signum
  oeis = 0 : [1, 1 ..]

instance OEIS 57436 where
  oeis = filter (null . (intersect "0789") . show . fi) [1..]

instance OEIS 57660 where
  oeisIx (succ->n) = sum $ map (div n) $ (rowT @50873) n

instance OEIS 58187 where
  oeis = 1 : f 1 1 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ zipWith (*) [1..x] [x,x - 1..1]

instance OEIS 58345 where
  oeis = 1 : f 1 1 where
     f u v = w : f (u * w) (v + w) where w = lcm u v

instance OEIS 58698 where
  oeis = map (pMemo 1) (oeis @40) where
     pMemo = memo2 integral integral p
     p _ 0 = 1
     p k m | m < k     = 0
           | otherwise = pMemo k (m - k) + pMemo (k + 1) m

instance OEIS 58840 where
  oeis = 1 : renyi' 1 where
     renyi' x = y : renyi' r  where
        (r, y) | q > 1     = (q - 1, 1)
               | otherwise = (q, 0)
        q = 3%2 * x

instance OEIS 58841 where
  oeis =
     0 : (map length $ filter ((== 0) . head) $ group (oeis @58840))

instance OEIS 60000 where
  oeis = 1 : 2 : f 1 2 2 [] where
     f x y m []     = z : f y z z [m+1..z - 1] where z = x + y
     f x y m (h:hs) = h : f y h m hs

instance OEIS 60030 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v ws = y : f v y (delete y ws) where
       y = if null xs then u + v else last xs
       xs = takeWhile (< v) ws

instance OEIS 60384 where
  oeisIx = (oeisIx @55642) . (oeisIx @45)

instance OEIS 60445 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ takeWhile (>= n') $ (rowT @70165) n'
              where n' = 2 * n + 1

instance OEIS 61084 where
  oeis = 1 : 2 : zipWith (-) (oeis @61084) (tail (oeis @61084))

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

instance OEIS 62090 where
  oeis = f [1, 3 ..] [] where
     f (x:xs) ys = g x ys where
       g _ []     = x : f xs (x : ys)
       g 1 _      = f xs ys
       g z (v:vs) = g (z `div` gcd z v) vs

instance OEIS 62097 where
  oeis = 1 : f 1 1 where
     f u v = w : f (u + w) (v * w) where w = u + v

instance OEIS 62296 where
  oeisIx = sum . map ((1 -) . signum) . (rowT @83093)

instance OEIS 62383 where
  oeis = 1 : zs where
     zs = 2 : (map (* 2) $ concat $ transpose [zs, zs])

instance OEIS 62880 where
  oeis = filter f [0..] where
     f 0 = True
     f x = (m == 0 || m == 2) && f x'  where (x', m) = divMod x 4

instance OEIS 63656 where
  oeis = f 1 [0..] where
     f k xs = us ++ f (k + 1) (drop (k - 1) vs) where
                      (us, vs) = splitAt k xs

instance OEIS 63657 where
  oeis = f 0 [0..] where
     f k (_:xs) = us ++ f (k + 1) (drop (k + 1) vs) where
                          (us, vs) = splitAt k xs

instance OEIS 64222 where
  oeis = iterate (oeisIx @4186 . (+ 1)) 0

instance OEIS 64223 where
  oeis = iterate (\x -> x + ((oeisIx @55642) x)) 1

instance OEIS 64235 where
  oeis = 1 : zs where
     zs = 3 : 3 : (map (* 3) $ concat $ transpose [zs, zs, zs])

instance OEIS 64417 where
  oeis = 1 : 2 : 3 : f 3 [4..] where
     f x us = x' : f x' (delete x' us) where
        x' = head [u | u <- us, gcd u x > 2]

instance OEIS 64419 where
  oeis = [1,2,3,4,5] ++ f 5 [] [6..] where
     f z xs (y:ys) | y `gcd` z > 4 = y : f y [] (reverse xs ++ ys)
                   | otherwise     = f z (y:xs) ys

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

instance OEIS 64736 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v (w:ws) = u' : w : f u' w (delete u' ws) where u' = v * w

instance OEIS 64801 where
  oeis = f 1 [1..] where
     f k xs = us ++ f (k + 1) (drop (k + 1) vs)
              where (us, vs) = splitAt k xs

instance OEIS 64911 where
  oeisIx = oeisIx @10051 . pred . oeisIx @32742

instance OEIS 65091 where
  oeis = tail (oeis @40)

instance OEIS 65094 where
  oeis = 1 : f 1 1 1 where
     f k s x = y : f (k + 1) (s + y) y where y = x + div s k

instance OEIS 65305 where
  oeis = tablList @65305
instance Table 65305 where
  rowCol = rowCol_off @65305 @2 @1
  rowT = rowT_off @65305 @2
  tabl = zipWith (map . (flip div 2 .) . (+))
                         (oeis @65091) $ tail $ inits (oeis @65091)

instance OEIS 65422 where
  oeis = 1 : 1 : f 2 1 where
     f n x = x' : f (n+1) x' where
         x' | x `mod` n == 0 = until ((> 0) . (`mod` n)) (`div` n) x
            | otherwise      = x * n

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

instance OEIS 66680 where
  oeis = s [2..] where
     s (b:bs) = b : s [x | x <- bs, x > b ^ 2 || mod x b > 0]

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

instance OEIS 67898 where
  oeisIx n = f n [0..10] where
     f x ys | x <= 9    = head $ delete x ys
            | otherwise = f x' $ delete d ys where (x',d) = divMod x 10

instance OEIS 67998 where
  oeisIx n = n * (n - 2)
  oeis = scanl (+) 0 [-1, 1 ..]

instance OEIS 70165 where
  oeis = tablList @70165
instance Table 70165 where
  rowCol = rowCol_off @70165 @1 @1
  rowT n = (takeWhile (/= 1) $ iterate (oeisIx @6370) n) ++ [1]
  tabf = map (rowT @70165) [1..]

instance OEIS 70167 where
  oeisIx (succ->n) = fromJust (findIndex (elem n) (tabf @70165)) + 1

instance OEIS 70870 where
  oeis = 6 : f 6 where
     f x = y : f y where
       y = (if even x then 5 * x else x + 1) `div` 2

instance OEIS 70885 where
  oeis = 1 : map (flip (*) 3 . flip div 2 . (+ 1)) (oeis @70885)

instance OEIS 70991 where
  oeis = filter (\x -> (x - 1) `elem` (rowT @70165) x) [1..]

instance OEIS 71797 where
  oeis = f $ tail $ inits [1..] where
     f (xs:_:xss) = xs ++ f xss

instance OEIS 72007 where
  oeis = 0 : f 1 0 [1..] where
     f u v ws = g ws where
       g (x:xs) = if abs (x - v) < u
                     then g xs else x : f (u + 1) x (delete x ws)

instance OEIS 72214 where
  oeisIx = (oeisIx @41) . (oeisIx @45) . (+ 1)

instance OEIS 72979 where
  oeis = 1 : f 2 [1] where
     f z xs = y : f (z + 1) (y : xs) where
       y = sum $ zipWith (*) xs (map (gcd z) [z - 1, z - 2 ..])

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

instance OEIS 74068 where
  oeis = 1 : 2 : 3 : xs where
     xs = 10 : 9 : 8 : 7 : 6 : 5 : 4 : map (+ 7) xs

instance OEIS 74294 where
  oeis = f $ inits [1..] where
     f (xs:_:xss) = xs ++ f xss

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

instance OEIS 76338 where
  oeisIx = (+ 1) . (* 512)
  oeis = [1,513..]

instance OEIS 76478 where
  oeis = concat $ tail $ map (tail . reverse . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2 )) [1..]

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

instance OEIS 78125 where
  oeis = f [1] where
     f xs = (p' xs $ last xs) : f (1 : map (* 3) xs)
     p' = memo2 (list integral) integral p
     p _ 0 = 1; p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m

instance OEIS 78310 where
  oeisIx (succ->n) = n * (oeisIx @7947 $ pred n) + 1

instance OEIS 78408 where
  oeis = f 1 where
     f x = (p' 1 x) : f (x + 2)
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 2) m

instance OEIS 78783 where
  oeis = oeis78783

instance OEIS 117073 where
  oeis = oeis117073

(oeis78783, (oeis117073)) = unzip $
    (0,0) : (1,1) : (3,2) : f 3 2 (2:[4..]) where
    f a d ms@ (m:_) = (a', d') : f a' d' (delete a' ms) where
      (a', d') = if i > d then (m, i) else (a + d + 1, d + 1)
      i = a - m

instance OEIS 79066 where
  oeisIx (succ->n) = length $ filter (`isInfixOf` (primesDec !! n)) $ take n primesDec
    where
      primesDec = "_" : map (show.fi) (oeis @40)

instance OEIS 79079 where
  oeis = map (`div` 4) $
                 zipWith (*) (oeis @8864) $ tail (oeis @8864)

instance OEIS 79122 where
  oeisIx n = p [1..n] (2 * n) where
     p _  0     = 1
     p [] _     = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 79878 where
  oeis = 1 : zipWith (\x n -> if x <= n then x else x - n)
                             (map (* 2) (oeis @79878)) [2..]

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

instance OEIS 80757 where
  oeisIx = (subtract 1) . (oeisIx @7538)

instance OEIS 81145 where
  oeis = 1 : f 1 [2..] [] where
     f x vs ws = g vs where
       g (y:ys) = if z `elem` ws then g ys else y : f y (delete y vs) (z:ws)
                  where z = abs (x - y)

instance OEIS 82500 where
  oeis = concat $ transpose [[1..], (oeis @40)]

instance OEIS 82977 where
  oeis = [0, 1, 3, 5, 6, 8, 10] ++ map (+ 12) (oeis @82977)

instance OEIS 83093 where
  oeis = tablList @83093
instance Table 83093 where
  tabl = iterate
     (\ws -> zipWith (\u v -> mod (u + v) 3) ([0] ++ ws) (ws ++ [0])) [1]

instance OEIS 83416 where
  oeis = 1 : f 2 1 where
     f x y = z : f (x+1) z where z = (1 + x `mod` 2) * y + 1 - x `mod` 2

instance OEIS 84214 where
  oeis = 1 : xs where
     xs = 1 : 4 : zipWith (+) (map (* 2) xs) (tail xs)

instance OEIS 84338 where
  oeis = [1,2,3] ++ zipWith (+) (oeis @84338) (tail (oeis @84338))

instance OEIS 84385 where
  oeis = 1 : f [2..] 1 where
     f xs s = g xs where
       g (y:ys) = if gcd s y == 1 then y : f (delete y xs) (s + y) else g ys

instance OEIS 84937 where
  oeis = 1 : 2 : f 2 1 [3..] where
     f x y zs = g zs where
        g (u:us) | gcd y u > 1 || gcd x u > 1 = g us
                 | otherwise = u : f u x (delete u zs)

instance OEIS 84964 where
  oeis = concat $ transpose [[2..], [0..]]

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

instance OEIS 87226 where
  oeisIx = foldl1 lcm . (rowT @70165) . succ

instance OEIS 87960 where
  oeisIx n = (-1) ^ (n * (n + 1) `div` 2)
  oeis = cycle [1,-1,-1,1]

instance OEIS 88230 where
  oeis = 0 : f [1..] [0] where
     f (x:xs) ys@ (y:_)
      | x <= y && (length $ filter (== z) ys) <= 1 = z : f xs (z : ys)
      | otherwise = (x + y) : f xs ((x + y) : ys)  where z = y - x

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

instance OEIS 89229 where
  oeis = f (oeis @18252) $ tail (oeis @290) where
     f (u:us) vs'@ (v:vs) = if u < v then u : f us vs' else f us vs

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

instance OEIS 90636 where
  oeis = iterate (oeisIx @3415) 15

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

instance OEIS 92401 where
  oeis = f [1..] where
     f (x:xs) = x : x' : f (delete x' xs) where x' = 3*x

instance OEIS 92525 where
  oeisIx (succ->n) = f n n where
     f x y = if m == 0 then f x' (2 * y + 1) else y
             where (x', m) = divMod x 2

instance OEIS 93017 where
  oeisIx n = if n == 0 then 0 else (oeisIx @93017) n' + (oeisIx @7953) (2 * t) + d
              where (n', td) = divMod n 100; (t, d) = divMod td 10

instance OEIS 93018 where
  oeis = filter ((== 1) . (oeisIx @249832)) [0..]

instance OEIS 93019 where
  oeisIx = flip mod 10 . (oeisIx @93018)

instance OEIS 93506 where
  oeis = 1 : 2 : f 1 [1] [3,5..] [4,6..]
     where f 0 (z:zs) odds evens = orun ++ f 1 (zs ++ orun) odds' evens
             where (orun, odds') = splitAt z odds
           f 1 (z:zs) odds evens = erun ++ f 0 (zs ++ erun) odds evens'
             where (erun, evens') = splitAt z evens

instance OEIS 93820 where
  oeis = 1 : f [2..] [1] where
     f (x:xs) ys = y : f xs (y:ys) where y = sum $ map (gcd x) ys

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

instance OEIS 95259 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @95258)) . succ

instance OEIS 95344 where
  oeis = tail xs where
     xs = 1 : 1 : 1 : zipWith (-) (map (* 5) $ zipWith (+) (tail xs) xs) xs

instance OEIS 96258 where
  oeisIx = p (oeis @18252) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 96535 where
  oeis = 1 : 1 : f 2 1 1 where
     f n x x' = y : f (n+1) y x where y = mod (x + x') n

instance OEIS 97062 where
  oeis = concat $ transpose [oeis @5408, (-1) : (oeis @5408)]

instance OEIS 97065 where
  oeisIx n = n' - 2 * m where (n', m) = divMod (n + 2) 2
  oeis = concat $ transpose [[1 ..], [-1 ..]]

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

instance OEIS 98551 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98550)) . succ

instance OEIS 98552 where
  oeisIx = (oeisIx @98550) . pred . (oeisIx @98550)

instance OEIS 98700 where
  oeis = filter
     (\z -> all (/= z) $ map (oeisIx @3415) [1 .. (oeisIx @2620) z]) [2..]

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

instance OEIS 99427 where
  oeis = 1 : map (+ 1) (zipWith gcd [2..] (oeis @99427))

instance OEIS 99773 where
  oeisIx = p (oeis @65091) where
     p _      0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 99774 where
  oeisIx = (oeisIx @5) . pred . (oeisIx @5408)

instance OEIS 99920 where
  oeis = zipWith (*) [1..] (oeis @45)

instance OEIS 100471 where
  oeisIx 0 = 1
  oeisIx n = p 0 (n + 1) 1 n where
     p m m' k x | x == 0    = if m < m' || m == 0 then 1 else 0
                | x < k     = 0
                | m == 0    = p 1 m' k (x - k) + p 0 m' (k + 1) x
                | otherwise = p (m + 1) m' k (x - k) +
                              if m < m' then p 0 m (k + 1) x else 0

instance OEIS 100484 where
  oeis = map (* 2) (oeis @40)

instance OEIS 100587 where
  oeisIx = (subtract 1) . (2 ^) . (oeisIx @5)

instance OEIS 100617 where
  oeisIx = f 2 . succ where
     f k x = if x' == 0 then x else f (k + 1) (x - x') where x' = div x k

instance OEIS 100618 where
  oeisIx (succ->n) = f 2 n where
     f k n | n' == 0   = n
           | otherwise = f (k+1) (n-n') where n' = div n (k^2)

instance OEIS 100881 where
  oeisIx = p 0 0 1 where
     p m m' k x | x == 0    = if m > m' || m == 0 then 1 else 0
                | x < k     = 0
                | m == 0    = p 1 m' k (x - k) + p 0 m' (k + 1) x
                | otherwise = p (m + 1) m' k (x - k) +
                              if m > m' then p 0 m (k + 1) x else 0

instance OEIS 100995 where
  oeisIx (succ->n) = f 0 n where
     f e 1 = e
     f e x = if r > 0 then 0 else f (e + 1) x'
             where (x', r) = divMod x p
     p = (oeisIx @20639 . pred) n

instance OEIS 101369 where
  oeis = f [1..] where
     f (x:xs) = x : y : f (delete y xs) where y = xs !! (x - 1)

instance OEIS 101881 where
  oeis = scanl1 (+) $ intersperse 1 [1..]

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

instance OEIS 102696 where
  oeisIx (succ->n) = genericLength $ nub
     [p + q | p <- genericTake n (oeis @65091), q <- takeWhile (<= p) (oeis @65091)]

instance OEIS 103128 where
  oeisIx = (oeisIx @196) . (subtract 1) . (* 2) . succ

instance OEIS 103181 where
  oeisIx n = foldl f 0 $ reverse $ unfoldr g n where
     f v d = 10 * v + mod d 2
     g x = if x == 0 then Nothing else Just $ swap $ divMod x 10

instance OEIS 103215 where
  oeis = [1,2,5,10,13,17] ++ map (+ 24) (oeis @103215)

instance OEIS 103369 where
  oeisIx = until (`elem` (oeis @39943)) (oeisIx @3132) . succ

instance OEIS 103391 where
  oeis = 1 : ks where
     ks = concat $ transpose [[2..], ks]

instance OEIS 104895 where
  oeis = 0 : concat (transpose [map (negate . (+ 1)) zs, tail zs])
                 where zs = map (* 2) (oeis @104895)

instance OEIS 105801 where
  oeis = 1 : 2 : fc 2 1 where
     fc x x' = y : fc y x where y = (oeisIx @6370) (x + x')

instance OEIS 106195 where
  oeis = tablList @106195
instance Table 106195 where
  tabl = [1] : [2, 1] : f [1] [2, 1] where
     f us vs = ws : f vs ws where
       ws = zipWith (-) (zipWith (+) ([0] ++ vs) (map (* 2) vs ++ [0]))
                        ([0] ++ us ++ [0])

instance OEIS 106244 where
  oeis = map (p' 1) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < pp then 0 else p' (k + 1) (m - pp) + p' (k + 1) m
             where pp = oeisIx @961 $ pred k

instance OEIS 106328 where
  oeis = 0 : 3 : zipWith (-) (map (* 6) (tail (oeis @106328))) (oeis @106328)

instance OEIS 106370 where
  oeisIx (succ->n) = f 2 n where
     f b x = g x where
       g 0 = b
       g z = if r == 0 then f (b + 1) n else g z'
             where (z', r) = divMod z b

instance OEIS 107354 where
  oeisIx n = head $ snd $ until ((== 1) . fst)
                                 f (2^n, replicate (2^n) 1) where
     f (len, xs) = (len', scanl1 (+) $ drop len' xs) where
        len' = len `div` 2

instance OEIS 108397 where
  oeisIx 0 = 0
  oeisIx 1 = 2
  oeisIx n = n * (n^ (n+1) + n^2 - 2) `div` (2 * (n - 1))

instance OEIS 108696 where
  oeis = 1 : sieve' 2 [2..] where
     sieve' n (x:xs) = x : (sieve' (n+1) $ sieving xs) where
        sieving xs = (take (n - 1) xs) ++ (sieving $ drop n xs)

instance OEIS 109008 where
  oeisIx = gcd 4
  oeis = cycle [4,1,2,1]

instance OEIS 110380 where
  oeis = drop 1 fn
      where fn    = 0 : 1 : concat (fn' 2)
            fn' n = (map (+ones) (drop nv $ take (n + nv) fn)) : (fn' (n+1))
                    where ones = div (10^n - 1) 9
                          nv   = div ((n - 1)* (n - 2)) 2

instance OEIS 110591 where
  oeisIx 0 = 1
  oeisIx n = genericLength $
     unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4)) n

instance OEIS 110654 where
  oeisIx = (`div` 2) . (+ 1)
  oeis = tail (oeis @4526)

instance OEIS 111063 where
  oeis = 1 : zipWith (+) [1..] (zipWith (*) [0..] (oeis @111063))

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

instance OEIS 112765 where
  oeisIx (succ->n) = fives n 0 where
     fives n e | r > 0     = e
               | otherwise = fives n' (e + 1) where (n',r) = divMod n 5

instance OEIS 113801 where
  oeis = 1 : 13 : map (+ 14) (oeis @113801)

instance OEIS 113963 where
  oeis = 1 : f 1 [2..] where
     f z xs = g xs where
       g (y:ys) = if (y + z) `mod` abs (y - z) > 0
                     then y : f y (delete y xs) else g ys

instance OEIS 113966 where
  oeis = 1 : f [2..] [1] where
     f xs ys'@ (y:ys) = y' : f (delete y' xs) (y':ys') where
       y' = head [z | z <- xs, y `mod` abs (z - y) > 0]

instance OEIS 114851 where
  oeisIx = open where
    open n = if n<2 then 0 else
             1 + open (n - 2) + sum [open i * open (n - 2 - i) | i <- [0..n - 2]]

instance OEIS 117070 where
  oeis = tSegments !! 0

instance OEIS 117071 where
  oeis = tSegments !! 1

instance OEIS 117072 where
  oeis = tSegments !! 2

tSegments = transpose $ unfoldr (Just . splitAt 3) $ tail (oeis @78783)

instance OEIS 118372 where
  oeis = sPerfect 1 [] where
     sPerfect x ss | v > x = sPerfect (x + 1) ss
                   | v < x = sPerfect (x + 1) (x : ss)
                   | otherwise = x : sPerfect (x + 1) (x : ss)
                   where v = sum (filter ((== 0) . mod x) ss)

instance OEIS 119352 where
  oeisIx n = f 2 n where
     f b x = g x where
       g 0 = b
       g z = if r == b - 1 then f (b + 1) n else g z'
             where (z', r) = divMod z b

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

instance OEIS 122197 where
  oeis = tablList @122197
instance Table 122197 where
  rowCol = rowCol_off @122197 @0 @1
  rowT   = rowT_off   @122197 @1
  tabf   = concat $ transpose [tabl @2260, tabl @2260]

instance OEIS 122841 where
  oeisIx = f 0 . succ where
     f y x = if r > 0 then y else f (y + 1) x'
             where (x', r) = divMod x 6

instance OEIS 125203 where
  oeisIx (succ->n) = genericLength [ () | x <- [1 .. (n + 1) `div` 3],
                           let (y,m) = divMod (x + n) (4 * x - 1),
                           x <= y, m == 0]

instance OEIS 126025 where
  oeisIx n = h n1s 0 where
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

instance OEIS 127118 where
  oeisIx n = (oeisIx @40) n * (oeisIx @18252) n

instance OEIS 127324 where
  oeis = concatMap (concatMap concat .
                 inits . inits . enumFromTo 0) $ enumFrom 0

instance OEIS 128588 where
  oeis = 1 : cows where
                     cows = 2 : 4 : zipWith (+) cows (tail cows)

instance OEIS 130130 where
  oeisIx = min 2
  oeis = 0 : 1 : repeat 2

instance OEIS 130658 where
  oeisIx = (+ 1) . (`div` 2) . (`mod` 4)
  oeis = cycle [1,1,2,2]

instance OEIS 130665 where
  oeisIx = sum . map (3 ^) . (`take` (oeis @120)) . (+ 1)

instance OEIS 131134 where
  oeis = 1 : zipWith (\v w -> (v+w) `div` gcd v w) [2..] (oeis @131134)

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

instance OEIS 132442 where
  oeis = tablList @132442
instance Table 132442 where
  rowCol = rowCol_off @132442 @1 @1
  rowT   = rowT_off   @132442 @1
  tabl   = map (map (oeisIx @203 . pred)) (tabl @50873)

instance OEIS 132666 where
  oeis = 1 : f 1 [2..] where
     f z xs  = y : f y (delete y xs) where
       y | head xs > z = 2 * z
         | otherwise   = z - 1

instance OEIS 132741 where
  oeisIx = f 2 1 . succ where
     f p y x | r == 0    = f p (y * p) x'
             | otherwise = if p == 2 then f 5 y x else y
             where (x', r) = divMod x p

instance OEIS 133622 where
  -- oeisIx n = (1 - m) * n' + 1 where (n', m) = divMod n 2
  oeis = concat $ transpose [[1, 1 ..], [2 ..]]

instance OEIS 135287 where
  oeis = 1 : f 1 1 where
     f x y = z : f (x + 1) z where
          z = if m == 0 then y' else x + y; (y',m) = divMod y 2

instance OEIS 135851 where
  oeis = -1 : 0 : 1 : zipWith (+) (oeis @135851) (drop 2 (oeis @135851))

instance OEIS 136333 where
  oeis = filter (null . intersect "024568" . show . fi) [1..]

instance OEIS 136522 where
  oeisIx n = fi . fromEnum $ n == (oeisIx @4086) n

instance OEIS 137564 where
  oeisIx = f (-1) where
     f _ 0 = 0
     f r x = if d == r then f r x' else 10 * f d x' + d
             where (x', d) = divMod x 10

instance OEIS 137921 where
  oeisIx (succ->n) = genericLength $ filter (> 0) $
     map ((mod n) . (+ 1)) [d | d <- [1..n], mod n d == 0]

instance OEIS 138166 where
  oeis = filter (\x -> show (fi $ oeisIx @55642 x) `isInfixOf` show (fi x)) [0..]

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

instance OEIS 141468 where
  oeis = 0 : (oeis @18252)

instance OEIS 141809 where
  oeis = tablList @141809
instance Table 141809 where
  rowCol n k = (rowT @141809) n !! (k - 1)
  rowT 1 = [1]
  rowT n = zipWith (^) (rowT @27748 n) (rowT @124010 n)
  tabf = map (rowT @141809) [1..]

instance OEIS 142150 where
  oeisIx = uncurry (*) . (`divMod` 2) . (+ 1)
  oeis = scanl (+) 0 (oeis @1057)

instance OEIS 144396 where
  oeisIx = (+ 1) . (* 2) . succ
  oeis = [3, 5 ..]

instance OEIS 145513 where
  oeis = f [1] where
     f xs = (p' xs $ last xs) : f (1 : map (* 10) xs)
     p' = memo2 (list integral) integral p
     p _ 0 = 1; p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m

instance OEIS 145812 where
  oeis = filter f [1, 3 ..] where
     f v = v == 0 || even w && f w where w = v `div` 4

instance OEIS 147583 where
  oeisIx = p [1..] . succ where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p [5 * k ..] (m - k) + p ks m

instance OEIS 151800 where
  oeisIx = oeisIx @7918 . succ

instance OEIS 153727 where
  oeis = iterate (oeisIx @6370) 1

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

instance OEIS 158582 where
  oeis = [x | x <- [0..], (oeisIx @23416) x > 1]

instance OEIS 159477 where
  oeis = 1 : concat
     (zipWith (\p q -> genericReplicate (q - p) q)
              (oeis @8578) $ tail (oeis @8578))

instance OEIS 159684 where
  oeis = 0 : concat (iterate (concatMap s) [1])
     where s 0 = [0,1]; s 1 = [0,1,0]

instance OEIS 159999 where
  oeisIx (succ->n) = genericLength $ takeWhile (<= n) $ sort $ (rowT @70165) n

instance OEIS 160000 where
  oeisIx (succ->n) = p (takeWhile (<= n) $ sort $ (rowT @70165) n) n where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 160001 where
  oeisIx (succ->n) = p (takeWhile (<= n) $ sort $ (rowT @70165) n) n where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

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

instance OEIS 161896 where
  oeis = [x | x <- [1..], (9^x - 3*3^x - 4*x) `mod` (2*x* (2*x + 1)) == 0]

instance OEIS 163575 where
  oeisIx (succ->n) = f n' where
     f 0 = 0
     f x = if b == parity then f x' else x  where (x', b) = divMod x 2
     (n', parity) = divMod n 2

instance OEIS 165560 where
  oeisIx = flip mod 2 . (oeisIx @3415)

instance OEIS 166863 where
  oeis = 1 : zipWith (+) (oeis @166863) (drop 3 $ map (* 2) (oeis @45))

instance OEIS 168183 where
  oeis = [1..8] ++ map (+ 9) (oeis @168183)

instance OEIS 168184 where
  oeisIx = (1 -) . (0 ^) . (`mod` 10)
  oeis = cycle [0,1,1,1,1,1,1,1,1,1]

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

instance OEIS 173019 where
  oeisIx = foldr (\t v -> 3 * v + t) 0 . (rowT @83093)

instance OEIS 173964 where
  oeis = concat $ [1] : f [[1]] where
     f xss = yss ++ f yss where
       yss = [y] : map (++ [y]) xss
       y = head (head xss) + 1

instance OEIS 174813 where
  oeis = f [1] where
     f ds = foldr (\d v -> 10 * v + d) 0 ds : f (s ds)
     s [] = [1]; s (9:ds) = 1 : s ds; s (d:ds) = 3*d : ds

instance OEIS 175498 where
  oeis = 1 : f 1 [2..] [] where
     f x zs ds = g zs where
       g (y:ys) | diff `elem` ds = g ys
                | otherwise      = y : f y (delete y zs) (diff:ds)
                where diff = y - x

instance OEIS 175885 where
  oeis = 1 : 10 : map (+ 11) (oeis @175885)

instance OEIS 175886 where
  oeis = 1 : 12 : map (+ 13) (oeis @175886)

instance OEIS 175887 where
  oeis = 1 : 14 : map (+ 15) (oeis @175887)

instance OEIS 175965 where
  oeis = scanl (+) 1 (oeis @8578)

instance OEIS 175967 where
  oeis = scanl (+) 1 (oeis @18252)

instance OEIS 178138 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 4

instance OEIS 178225 where
  oeisIx n = fi . fromEnum $ n == (oeisIx @30101) n

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

instance OEIS 179243 where
  oeis = filter ((== 3) . (oeisIx @7895)) [1..]

instance OEIS 179244 where
  oeis = filter ((== 4) . (oeisIx @7895)) [1..]

instance OEIS 179245 where
  oeis = filter ((== 5) . (oeisIx @7895)) [1..]

instance OEIS 179246 where
  oeis = filter ((== 6) . (oeisIx @7895)) [1..]

instance OEIS 179247 where
  oeis = filter ((== 7) . (oeisIx @7895)) [1..]

instance OEIS 179888 where
  oeis = 2 : f (oeis @179888) where
    f (x:xs) = x' : x'' : f (xs ++ [x',x'']) where x' = 4*x+1; x'' = x' + 1

instance OEIS 181391 where
  oeis = 0 : (unfoldr g [0]) where
     g xs = Just (m, m : xs) where
          m = 1 + fromMaybe (-1) (findIndex (== head xs) $ tail xs)

instance OEIS 181717 where
  oeis = 0 : 1 : fc 1 0 where
     fc x x' = y : fc y x where y = (oeisIx @6370) (x + x')

instance OEIS 181753 where
  oeis = concat $ iterate
                 (map ((+ 1) . flip mod 8 . (+ 4))) [1,3,5,6,7,2,5]

instance OEIS 181935 where
  oeisIx 0 = 1
  oeisIx n = curling $ unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) n where
     curling zs = maximum $ zipWith (\xs ys -> strip 1 xs ys)
                            (tail $ inits zs) (tail $ tails zs) where
        strip i us vs | vs' == Nothing = i
                      | otherwise      = strip (i + 1) us $ fromJust vs'
                      where vs' = stripPrefix us vs

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

instance OEIS 185869 where
  oeis = scanl (+) 2 $ a' 1
    where  a' n = 2 * n + 3 : replicate n 2 ++ a' (n + 1)

instance OEIS 186421 where
  oeis = interleave [0,2..] $ rep [1,3..] where
     interleave (x:xs) ys = x : interleave ys xs
     rep (x:xs) = x : x : rep xs

instance OEIS 186422 where
  oeis = zipWith (-) (tail (oeis @186421)) (oeis @186421)

instance OEIS 186423 where
  oeis = scanl1 (+) (oeis @186421)

instance OEIS 186424 where
  oeis = filter odd (oeis @186423)

instance OEIS 186711 where
  oeis = zipWith gcd (oeis @3586) $ tail (oeis @3586)

instance OEIS 187202 where
  oeisIx = head . head . dropWhile ((> 1) . length) . iterate diff . divs . succ
     where divs n = filter ((== 0) . mod n) [1..n]
           diff xs = zipWith (-) (tail xs) xs

instance OEIS 187203 where
  oeisIx = head . head . dropWhile ((> 1) . length) . iterate diff . divs . succ
     where divs n = filter ((== 0) . mod n) [1..n]
           diff xs = map abs $ zipWith (-) (tail xs) xs

instance OEIS 187763 where
  oeis = map genericLength $
                 zipWith intersect (tabf @70165) $ tail (tabf @70165)

instance OEIS 187831 where
  oeisIx 0 = 1
  oeisIx n = head $ fromJust $
          find (n `elem`) $ genericDrop n (tabf @70165)

instance OEIS 188068 where
  oeisIx = (subtract 2) . (oeisIx @7538)

instance OEIS 188386 where
  oeis = map numerator $ zipWith (-) (drop 3 hs) hs
     where hs = 0 : scanl1 (+) (map (1 %) [1..])

instance OEIS 188429 where
  oeis = [1, 0, 2, 0, 0, 3, 4, 0, 0, 4, 5, 5, 6, 7] ++
                 f [15 ..] (drop 15 (oeis @10054)) 0 4
     where f (x:xs) (t:ts) r k | t == 1    = (k + 1) : f xs ts 1 (k + 1)
                               | r < k - 1 = (k + 1) : f xs ts (r + 1) k
                               | otherwise = (k + 2) : f xs ts (r + 1) k

instance OEIS 188430 where
  oeis = [1, 0, 2, 0, 0, 3, 4, 0, 0, 4, 5, 6, 7, 7, 8, 6, 7, 8, 9]
      ++ (drop 19 (oeis @8619))

instance OEIS 188431 where
  oeis = 1 : flip map [0..] \x -> sum
      [ fMemo x i
      | i <- [oeisIx @188429 x .. (oeisIx @188430) x]
      ]
    where
      fMemo = memo2 integral integral f
      f _ 1 = 1
      f m i = sum
        [ fMemo (m - i) j
        | j <- [oeisIx @188429 (m - i) .. min (oeisIx @188430 (m - i)) (i - 1)]
        ]

instance OEIS 190600 where
  oeisIx = fi . digitToInt . maximum . flip (showIntAtBase 12 intToDigit) "" . fi

instance OEIS 192489 where
  oeis = f 2 1 where
     f n x | x' == 2   = n : f (n+1) x'
           | otherwise = f (n+1) x'
           where x' = 1 + gcd n x

instance OEIS 192687 where
  oeis = zipWith (-) females males where
     females = 1 : zipWith (-) [1..] (map (males !!) females)
     males = 0 : zipWith (-) [1..] (map (females !!) males)

instance OEIS 193641 where
  oeis = drop 2 xs where
     xs = 1 : 1 : 1 : zipWith (+) xs (map (* 2) $ drop 2 xs)

instance OEIS 193773 where
  oeisIx n = genericLength [ () | x <- [1 .. n + 1],
                           let (y,m) = divMod (x + n) (2 * x - 1),
                           x <= y, m == 0]

instance OEIS 195013 where
  oeis = concat $ transpose [[2, 4 ..], [3, 6 ..]]

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

instance OEIS 197081 where
  oeisIx = p (drop 2 (oeis @2620)) where
     p _      0             = 1
     p (k:ks) m | m < k     = 0
                | otherwise = p ks (m - k) + p ks m

instance OEIS 197122 where
  oeisIx = p (drop 2 (oeis @2620)) where
     p _          0             = 1
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

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

instance OEIS 198726 where
  oeis = f 0 [] $ tail (oeis @3136) where
     f u vs ws'@ (w:ws) | u < w = (p' vs u) : f (u + 1) vs ws'
                        | otherwise = f u (vs ++ [w]) ws
     p' = memo2 (list integral) integral p
     p _  0 = 1
     p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m

instance OEIS 198727 where
  oeis = f 0 [] $ tail (oeis @3136) where
     f u vs ws'@ (w:ws) | u < w = (p' vs u) : f (u + 1) vs ws'
                        | otherwise = f u (vs ++ [w]) ws
     p' = memo2 (list integral) integral p
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m = if m < k then 0 else p' ks (m - k) + p' ks m

instance OEIS 200745 where
  oeisIx n = p [nd | nd <- [1..n], mod n nd /= 0] n where
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m | m < k = 0 | otherwise = p ks (m - k) + p ks m

instance OEIS 201208 where
  oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @34)

instance OEIS 201629 where
  oeisIx = (* 2) . (oeisIx @4524) . (+ 1)

instance OEIS 206424 where
  oeisIx = genericLength . filter (== 1) . (rowT @83093)

instance OEIS 208341 where
  oeis = tablList @208341
instance Table 208341 where
  rowCol = rowCol_off @208341 @1 @1
  rowT   = rowT_off   @208341 @1
  tabl = map reverse (tabl @106195)

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

instance OEIS 209403 where
  oeisIx (succ->n) = sum $
     zipWith (*) (reverse $ genericTake n (oeis @40)) (oeis @65091)

instance OEIS 210461 where
  oeisIx = (`div` 8) . (subtract 1) . (9 ^) . (oeisIx @65091)

instance OEIS 210770 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v (w:ws) = u' : w : f u' w (delete u' ws) where u' = v + w

instance OEIS 212193 where
  oeisIx n = f n [0..3] where
     f x ys | x <= 2    = head $ delete x ys
            | otherwise = f x' $ delete d ys where (x',d) = divMod x 3

instance OEIS 212306 where
  oeis = f [1..] where
     f (x:xs) = x : f ((map (subtract x) us) ++ vs)
                where (us, vs) = splitAt x xs

instance OEIS 212793 where
  oeisIx = cubeFree (oeis @40) 0 0 . succ where
     cubeFree ps'@ (p:ps) q e x
        | e > 2     = 0
        | x == 1    = 1
        | r > 0     = cubeFree ps  p 0 x
        | otherwise = cubeFree ps' p (e + 1) x' where (x', r) = divMod x p

instance OEIS 213925 where
  oeis = tablList @213925
instance Table 213925 where
  rowCol n k = (rowT @213925) n !! (k - 1)
  rowT 1 = [1]
  rowT n = reverse $ fd n (reverse $ takeWhile (<= n) (oeis @50376))
     where fd 1 _      = []
           fd x (q:qs) = if m == 0 then q : fd x' qs else fd x qs
                         where (x',m) = divMod x q
  tabf = map (rowT @213925) [1..]

instance OEIS 213967 where
  oeis = 0 : xs where
                 xs = 1 : 2 : 3 : map (+ 1)
                      (zipWith3 (((+) .) . (+)) xs (tail xs) (drop 2 xs))

instance OEIS 214090 where
  oeisIx = (`mod` 2) . (oeisIx @9947)

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

instance OEIS 215973 where
  oeis = 1 : f [1] where
     f xs = y : f (y:xs) where
       y = sum $ zipWith (*) xs $ map (+ 1) $ reverse xs

instance OEIS 216022 where
  oeisIx = genericLength .
     takeWhile (== 0) . zipWith (-) [1..] . sort . (rowT @70165) . succ

instance OEIS 216059 where
  oeisIx  (succ->n) = head $ enumFromTo 1 (maximum ts + 1) \\ ts
     where ts = (rowT @70165) n

instance OEIS 216151 where
  oeis = 1 : 2 : f 2 3 where
     f u v = w : f (u * w) (v + w) where w = u * v

instance OEIS 219696 where
  oeis = filter (\x -> collatz'' x == x) [1..] where
     collatz'' x = until (`elem` [1, x]) (oeisIx @6370) (3 * x + 1)

instance OEIS 220104 where
  oeis = concatMap (\x -> genericTake (oeisIx @2378 x) $ repeat x) [1..]

instance OEIS 220237 where
  oeis = tablList @220237
instance Table 220237 where
  rowCol = rowCol_off @220237 @1 @1
  rowT   = rowT_off @220237 @1
  tabf   = map sort (tabf @70165)

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

instance OEIS 225693 where
  oeisIx = f 1 0 where
     f _ a 0 = a
     f s a x = f (negate s) (s * a + d) x' where (x', d) = divMod x 10

instance OEIS 225761 where
  oeisIx = numerator . sum . map (recip . fi) . (rowT @70165) . succ

instance OEIS 225784 where
  oeisIx = denominator . sum . map (recip . fi) . (rowT @70165) . succ

instance OEIS 225790 where
  oeisIx 0 = 1
  oeisIx (succ->n) = 12 ^ (n1 * n1) * 2 ^ (2 * n1 - 1) * k
    where
      n1 = div n 2
      k = if odd n then 4 else 1

instance OEIS 225843 where
  oeisIx = floor . sum . map (recip . fi) . (rowT @70165) . succ

instance OEIS 226123 where
  oeisIx = sum . map (oeisIx @209229) . (rowT @70165) . succ

instance OEIS 226203 where
  oeis = concat $ transpose [[1, 3 ..], [-3, -1 ..], [-1, 1 ..], [1, 3 ..], [1, 3 ..]]

instance OEIS 226390 where
  oeis = zipWith (-) (tail (oeis @14011)) (oeis @14011)

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

instance OEIS 227426 where
  oeisIx = p 1 1 where
    p _ _ 0 = 1
    p k i m = if m < k then 0 else p (k + i) (3 - i) (m - k) + p (k + 1) 1 m

instance OEIS 227428 where
  oeisIx = sum . map (flip div 2) . (rowT @83093)

instance OEIS 228709 where
  oeis = filter ((== 0) . (oeisIx @228710)) [0..]

instance OEIS 228710 where
  -- oeisIx n = signum n == (oeisIx @30101) n
  oeisIx n = fi . fromEnum $ f (n `div` 10) (n `mod` 2) where
     f x p = x == 0 || (x `mod` 2) /= p && f (x `div` 10) (1 - p)

instance OEIS 228722 where
  oeisIx n = head [x | x <- [n, n - 1 ..], (oeisIx @228710) x == 1]

instance OEIS 228723 where
  oeisIx n = head [x | x <- [n..], (oeisIx @228710) x == 1]

instance OEIS 229362 where
  oeis = 1 : 2 : 3 : f 4 [1,2,3] where
     f x ys = y : f (x + 1) (ys ++ [y]) where y = p ys x
     p _          0 = 1
     p []         _ = 0
     p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

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

instance OEIS 235711 where
  oeisIx = (oeisIx @3415) . (oeisIx @2620)

instance OEIS 235715 where
  oeisIx 0 = 1
  oeisIx (succ->n) = f 1 ps 0 where
     f 0 (1 : xs) z = z
     f _ (x : xs) z = f x xs (z + 0 ^ (n - 1 - x))
     ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` n) (tail ps) ps

instance OEIS 239965 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @240024)) . (oeisIx @18252)

instance OEIS 240024 where
  oeis = 1 : ekg 4 (oeis @2808) where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 242357 where
  oeis = concatMap f $ tail $ inits [1..] where
     f us = (init us) ++ (take v [v, v ..]) ++ vs
            where (v:vs) = reverse us

instance OEIS 245093 where
  oeis = tablList @245093
instance Table 245093 where
  rowCol = rowCol_off @245093 @1 @1
  rowT   = rowT_off   @245093 @1
  tabl = tail $ inits $ (oeis @203)

instance OEIS 245180 where
  oeisIx = flip div 8 . (oeisIx @160239) . succ

instance OEIS 245542 where
  oeis = scanl1 (+) (oeis @160239)

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

instance OEIS 247875 where
  oeis = filter (\x -> even x || f x) [0..] where
     f x = x > 0 && (x `mod` 4 == 0 || f (x `div` 2))

instance OEIS 248737 where
  oeis = 0 : f 1 [0] where
     f x ys = y : f (x + 1) (y : ys) where
       y = (+ 1) $ fromMaybe (x - 1) $ findIndex (\z -> gcd z x /= 1) ys

instance OEIS 248910 where
  oeis = iterate f 1 where
     f x = 1 + if r < 5 then x else 6 * f x'  where (x', r) = divMod x 6

instance OEIS 249484 where
  oeis = 1 : concat (zipWith (++) ([2] : f [5,2,7]) (f [4,3])) where
     f = iterate (\row -> [g $ head row] ++ row ++ [g $ last row])
     g x = x + ((5 -) . (* 2) . flip mod 2) x

instance OEIS 249832 where
  oeisIx = (0 ^) . flip mod 10 . (oeisIx @93017)

instance OEIS 249943 where
  oeis = scanl1 max $ map (oeisIx @98551) [0..]

instance OEIS 249990 where
  oeis = f 2 [1..] where
     f k xs = reverse ys ++ f (k + 1) (g zs) where
              g us = reverse vs ++ g ws where
                     (vs, ws) = splitAt k us
              (ys, zs) = splitAt k xs

instance OEIS 250299 where
  oeisIx = flip mod 2 . (oeisIx @98550)

instance OEIS 251237 where
  oeis = filter (even . (oeisIx @98550) . pred) [1..]

instance OEIS 251238 where
  oeis = filter (odd . (oeisIx @98550) . pred) [1..]

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

instance OEIS 251417 where
  oeis = map length $ group (oeis @251416)

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

instance OEIS 255420 where
  oeis = iterate (oeisIx @3309 . (+ 1)) 1

instance OEIS 255805 where
  oeis = iterate f 1 where
     f x = 1 + if r < 7 then x else 8 * f x'  where (x', r) = divMod x 8

instance OEIS 255808 where
  oeis = iterate f 1 where
     f x = 1 + if r < 8 then x else 9 * f x'  where (x', r) = divMod x 9

instance OEIS 257502 where
  oeisIx = fromJust . (`elemIndex` (oeis @78783))

instance OEIS 260798 where
  oeis = map (subtract 1 . pMemo 2) (oeis @40) where
     pMemo = memo2 integral integral p
     p _ 0 = 1
     p k m | m < k     = 0
           | otherwise = pMemo k (m - k) + pMemo (k + 1) m

instance OEIS 261036 where
  oeis = tablList @261036
instance Table 261036 where
  rowCol = rowCol_off @261036 @1 @1
  rowT   = rowT_off @261036 @1
  tabf = zipWith (map . flip dMemo) [1..] (tabf @122197) where
     dMemo = memo2 integral integral d
     d 0 _ = 0
     d _ 0 = 0
     d 1 _ = 1
     d k n | n <= 2 * k - 2 = 0
           | n <= 3 * k - 2 = dMemo (k - 1) (n - 1)
           | otherwise      = dMemo (k - 1) (n - 1) + dMemo k (n - k)

instance OEIS 262277 where
  oeis = filter f [1..] where
     f x = sort ds' == sort (map (9 -) ds') where
       ds' = nub $ ds x
       ds 0 = []; ds z = d : ds z' where (z', d) = divMod z 10

instance OEIS 263451 where
  oeis = iterate (oeisIx @4186 . (* 2)) 1

instance OEIS 264997 where
  oeis = f 0 [] (oeis @3593) where
     f u vs ws'@ (w:ws) | u < w = (p' vs u) : f (u + 1) vs ws'
                       | otherwise = f u (vs ++ [w]) ws
     p' = memo2 (list integral) integral p
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m = if m < k then 0 else p' ks (m - k) + p' ks m

instance OEIS 264998 where
  oeis = f 0 [] (1 : 2 : tail (oeis @3593)) where
     f u vs ws'@ (w:ws) | u < w = (p' vs u) : f (u + 1) vs ws'
                       | otherwise = f u (vs ++ [w]) ws
     p' = memo2 (list integral) integral p
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m = if m < k then 0 else p' ks (m - k) + p' ks m

instance OEIS 265652 where
  oeis = tablList @265652
instance Table 265652 where
  rowCol = rowCol_off @265652 @1 @1
  rowT   = rowT_off   @265652 @1
  tabl = zipWith (zipWith (-))
     (zipWith (map . (+)) (oeis @203) (tabl @245093)) (tabl @132442)

instance OEIS 276374 where
  oeis = map succ $ filter (\i -> (oeisIx @240024) i == (oeisIx @2808) i) [1..]

instance OEIS 276375 where
  oeis = (1 :) . map succ $ filter (\i -> (oeisIx @240024) (i + 1) == (oeisIx @2808) i) [1..]

instance OEIS 308576 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @308576 (n - 1)) + (oeisIx @308576 (oeisIx @308576 (n - 1) `mod` n)) + 1

