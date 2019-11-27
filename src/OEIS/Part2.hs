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


happy 0 = (0, 0, 0, 0, 0)
happy x = if a > 0 then (0, a, a, a, a) else h' 1 divs
      where a = (oeisIx @37213) x
            divs = (rowT @27750) x
            h' r []                                = h' (r + 1) divs
            h' r (d:ds)
              | d' > 1 && rest1 == 0 && ss == s ^ 2 = (1, d, d', r, s)
              | rest2 == 0 && odd u && uu == u ^ 2  = (2, d, d', t, u)
              | otherwise                           = h' r ds
              where (ss, rest1) = divMod (d * r ^ 2 + 1) d'
                    (uu, rest2) = divMod (d * t ^ 2 + 2) d'
                    s = (oeisIx @196) ss; u = (oeisIx @196) uu; t = 2 * r - 1
                    d' = div x d

sqrtPair = (\(_,_,_,a, b) -> (a, b)) . happy . pred


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

instance OEIS 82 where
  oeisIx 0 = 1
  oeisIx (succ->n)
    = product $ zipWith (\p e -> p ^ (2*e - 1) * (p + 1))
        (rowT @27748 n) (rowT @124010 n)

instance OEIS 89 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ zipWith f (rowT @27748 n) (rowT @124010 n) where
   f 2 e = if e == 1 then 1 else 0
   f p _ = if p `mod` 4 == 1 then 2 else 0

instance OEIS 95 where
  oeisIx 0=1
  oeisIx (succ->n) = product $ zipWith f ((rowT @27748) n) ((rowT @124010) n) where
   f 2 e = if e == 1 then 2 else 0
   f p _ = if p `mod` 4 == 1 then 2 else 0

instance OEIS 118 where
  oeisIx 0 = 1
  oeisIx n = 8 * (oeisIx @46897 . pred) n

instance OEIS 139 where
  oeisIx 0 = 2
  oeisIx n = (rowCol @7318 (3 * n) (2 * n + 1)) `div` (oeisIx @217) n

instance OEIS 161 where
  oeisIx n = sum $ map ((oeisIx @10052) . (n -)) $ takeWhile (<= n `div` 2) (oeis @290)

instance OEIS 179 where
  oeis = (1:) $ (negate 1 :) $ drop 2 r
    where
      r = 1 : 0 : 0 : 1 : zipWith5
            (\v w x y z -> (x * y + (v + 2) * z - w) `div` v) [2..] (cycle [4,-4])
            (drop 4 (oeis @67998)) (drop 3 r) (drop 2 r)

instance OEIS 188 where
  oeisIx (succ->n) = product $ zipWith (^)
                      (rowT @27748 n) $ map (`div` 2) (rowT @124010 n)

instance OEIS 203 where
  oeisIx n = fi $ A.sigma 1 (1 + fi n :: Int)

instance OEIS 224 where
  oeisIx (succ->n) = product $ zipWith f ((rowT @27748) n) ((rowT @124010) n) where
   f 2 e = 2 ^ e `div` 6 + 2
   f p e = p ^ (e + 1) `div` (2 * p + 2) + 1

instance OEIS 262 where
  oeis = 1 : 1 : zipWith (-)
               (tail $ zipWith (*) (oeis @5408) (oeis @262))
                      (zipWith (*) (oeis @2378) (oeis @262))

instance OEIS 360 where
  oeis = fix \r -> 1 : concat (transpose
    [ zipWith (+) r $ drop 2 $ oeis @57078
    , zipTail (+) r ])

instance OEIS 385 where
  oeisIx (succ->n) = sum $ zipWith (*) sigmas $ reverse sigmas
    where
      sigmas = take n $ oeis @203

instance OEIS 396 where
  oeis = [ x | x <- [1..], oeisIx @203 (pred x) == 2 * x ]

instance OEIS 404 where
  oeis = findIndices (> 0) $ oeis @25426

instance OEIS 408 where
  oeis = filter ((> 0) . oeisIx @25427) [1..]

instance OEIS 419 where
  oeis = filter ((== 3) . oeisIx @2828) [1..]

instance OEIS 469 where
  oeis = filter ((== 0) . oeisIx @10051 . pred) (oeis @5117)

instance OEIS 660 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : [1..])

instance OEIS 667 where
  oeisIx n = if x == 1 then last xs else x
              where xs@ (x:_) = rowT @227862 n

instance OEIS 674 where
  oeisIx n = sum $ zipWith (*) ((rowT @109449) n) (1 : repeat 2)

instance OEIS 688 where
  oeisIx = product . map (oeisIx @41) . (rowT @124010) . succ

instance OEIS 712 where
  oeisIx = p $ oeis @8619 where
    p _          0 = 1
    p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 718 where
  oeisIx n = sum $ zipWith (*) ((rowT @109449) n) (1 : tail (oeis @217))

instance OEIS 732 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (oeis @8578)

instance OEIS 733 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : (oeis @41))

instance OEIS 751 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) $ oeis @41

instance OEIS 891 where
  oeisIx (succ->n) = (rowCol @1263) (2 * n - 1) n

instance OEIS 894 where
  oeisIx n = (rowCol @132813) (2 * n) n

instance OEIS 967 where
  oeisIx (succ->n) = round $ sum $
              zipWith ((/) `on` fi) ((rowT @258993) n) [1, 3 ..]

instance OEIS 969 where
  oeisIx = flip div 3 . (oeisIx @14105) . (+ 1)

instance OEIS 970 where
  oeisIx n = (rowCol @258708) (n+5) n

instance OEIS 971 where
  oeisIx n = (rowCol @258708) (n+6) n

instance OEIS 972 where
  oeisIx n = (rowCol @258708) (n+7) n

instance OEIS 973 where
  oeisIx n = (rowCol @258708) (n+8) n

instance OEIS 977 where
  oeis = filter ((> 2) . oeisIx @1221 . pred) [1..]

instance OEIS 978 where
 oeis = filter ((== 1) . (oeisIx @10051) . pred . (oeisIx @1045)) (oeis @65091)

instance OEIS 979 where
 oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @7583)

instance OEIS 990 where
  oeisIx = p $ tail (oeis @8619) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 994 where
  oeis = 1 : 0 : us where
     us = 1 : 1 : f 2 where
       f x = (1 + sum (zipWith (*) (map ((rowCol @7318) x) [2 .. x]) us)) : f (x + 1)

instance OEIS 995 where
  oeis = 0 : 1 : vs where
     vs = 0 : 1 : g 2 where
       g x = (x + sum (zipWith (*) (map ((rowCol @7318) x) [2 .. x]) vs)) : g (x + 1)

instance OEIS 1003 where
  oeisIx = last . (rowT @144944)

instance OEIS 1013 where
 oeis = 1 : h 0 S.empty [1] (drop 2 (oeis @142)) where
     h z s mcs xs'@ (x:xs)
      | S.null s || x < m = h z (S.union s (S.fromList $ map (* x) mcs)) mcs xs
      | m == z = h m s' mcs xs'
      | otherwise = m : h m (S.union s' (S.fromList (map (* m) $ init (m:mcs)))) (m:mcs) xs'
      where (m, s') = S.deleteFindMin s

instance OEIS 1055 where
  oeisIx = (map last (tabl @66032) !!)

instance OEIS 1065 where
  oeisIx n = (oeisIx @203 n) - n - 1

instance OEIS 1066 where
  oeis = f (S.fromList [h, 2 * h]) $ tail (oeis @3038) where
     h = head (oeis @3038)
     f s (x:xs) = m : f (x `S.insert` ((2 * x) `S.insert` s')) xs where
       (m, s') = S.deleteFindMin s

instance OEIS 1082 where
  oeis = scanl (+) 0 $ tail (oeis @22998)

instance OEIS 1101 where
 oeis = map succ $ findIndices p [1..] where
     p n = m == 0 && (oeisIx @10051 . pred) n' == 1 where
        (n', m) = divMod n ((oeisIx @7953) n)

instance OEIS 1103 where
 oeis = filter f (oeis @52382) where
     f x = m == 0 && (x' == 1 || (oeisIx @10051 . pred) x' == 1) where
         (x',m) = divMod x $ (oeisIx @7954) x

instance OEIS 1105 where
  oeisIx = (oeisIx @5843) . (oeisIx @290)

instance OEIS 1127 where
  oeis = iterate (oeisIx @56964) 1

instance OEIS 1129 where
 oeis = 0 : 1 : zipWith (+) iccanobifs (tail iccanobifs)
  where iccanobifs = map (oeisIx @4086) (oeis @1129)

instance OEIS 1132 where
 oeis = [x | x <- (oeis @47522), (oeisIx @10051 . pred) x == 1]

instance OEIS 1227 where
  oeisIx = sum . (rowT @247795) . succ

instance OEIS 1255 where
  oeisIx = (^ 2) . oeisIx @41

instance OEIS 1317 where
  oeisIx = foldr (\u v-> 2*v + u) 0 . (rowT @47999)

instance OEIS 1359 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (+ 2)) (oeis @40)

instance OEIS 1400 where
 oeis = drop 3 $ scanl1 (+) (oeis @5044)

instance OEIS 1414 where
  oeisIx 0 = 0
  oeisIx n = sum $ (rowT @27746 . succ) n

instance OEIS 1461 where
 oeis = scanl1 (+) (oeis @6206)

instance OEIS 1468 where
 oeis = map length $ group (oeis @5206)

instance OEIS 1481 where
  oeis = [x | x <- [0..], oeisIx @161 x > 0]

((oeis1597), (oeis25478), (oeis25479)) =
    unzip3 $ (1, 1, 2) : f 9 (3, 2) (M.singleton 4 (2, 2)) where
      f zz (bz, ez) m
        | xx < zz = (xx, bx, ex) :
                    f zz (bz, ez+1) (M.insert (bx*xx) (bx, ex+1) $ M.deleteMin m)
        | xx > zz = (zz, bz, 2) :
                    f (zz+2*bz+1) (bz+1, 2) (M.insert (bz*zz) (bz, 3) m)
        | otherwise = f (zz+2*bz+1) (bz+1, 2) m
        where (xx, (bx, ex)) = M.findMin m

instance OEIS 1499 where
 oeis = 1 : 0 : 1 : zipWith (*) (drop 2 (oeis @2411))
     (zipWith (+) (zipWith (*) [3, 5 ..] $ tail (oeis @1499))
                  (zipWith (*) (tail (oeis @290)) (oeis @1499)))

instance OEIS 1519 where
--  oeis = 1 : zipWith (-) (tail (oeis @1906)) (oeis @1906)
 oeis = 1 : f (oeis @45) where f (_:x:xs) = x : f xs

instance OEIS 1583 where
 oeis = filter
     (\p -> mod ((oeisIx @45) $ div (p - 1) 5) p == 0) (oeis @30430)

instance OEIS 1597 where
  oeis = oeis1597
instance OEIS 1615 where
  oeisIx 0 = 1
  oeisIx (succ->n) = numerator (fi n * (product $
              map ((+ 1) . recip . fi) $ (rowT @27748) n))

instance OEIS 1633 where
 oeis = filter (odd . (oeisIx @55642)) [0..]

instance OEIS 1637 where
  oeis = filter (even . (oeisIx @55642)) [0..]

instance OEIS 1682 where
  oeis = [k | k <- [0..], let m = 3^k, (oeisIx @55642) m == (oeisIx @55642) (9*m)]

instance OEIS 1690 where
  oeis = elemIndices 0 (oeis @10056)

instance OEIS 1751 where
  oeis = 2 : filter (\n -> ((oeisIx @10051) . pred $ div n $ gcd 2 n) == 1) [1..]


instance OEIS 1764 where
 oeis = 1 : [rowCol @258708 (2 * n) n | n <- [1..]]

instance OEIS 1783 where
  oeisIx = product . (rowT @38566) . succ

instance OEIS 1792 where
  oeis = scanl1 (+) (oeis @45623)

instance OEIS 1857 where
  oeis = 2 : 3 : ulam 2 3 (oeis @1857)

instance OEIS 1859 where
  oeisIx n = (oeisIx @217) n + (oeisIx @2620) (n + 1)

instance OEIS 1983 where
  oeis = [x | x <- [0..], (oeisIx @25435) x > 0]

instance OEIS 2019 where
 oeis = 1 : 1 : zipWith (-)
     (tail (oeis @2019)) (zipWith (*) (oeis @2019) (oeis @2378))

instance OEIS 2035 where
  oeis = filter (all odd . (rowT @124010)) [1..]

instance OEIS 2095 where
  oeisIx = p (oeis @18252) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 2113 where
   oeis = filter ((== 1) . (oeisIx @136522)) [0..]

instance OEIS 2121 where
  oeis = 1 : 0 : -1 : f 0 (-1) 3 where
     f v w x = y : f w y (x + 1) where
       y = sum (map ((oeisIx @2121) . (x -)) $ takeWhile (<= x) (oeis @65091)) - v

instance OEIS 2122 where
  oeis = uncurry conv $ splitAt 1 $ oeis @2121 where
     conv xs (z:zs) = sum (zipWith (*) xs $ reverse xs) : conv (z:xs) zs

instance OEIS 2124 where
 oeis = 1 : f 1 [] (oeis @65091) where
     f x qs ps'@ (p:ps)
       | p <= x    = f x (p:qs) ps
       | otherwise = sum (map ((oeisIx @2124) . (x -)) qs) : f (x + 1) qs ps'

instance OEIS 2125 where
  oeis = uncurry conv $ splitAt 1 (oeis @2124) where
     conv xs (z:zs) = sum (zipWith (*) xs $ reverse xs) : conv (z:xs) zs

instance OEIS 2145 where
 oeis = filter ((== 1) . (oeisIx @10051 . pred)) [3, 7 ..]

instance OEIS 2180 where
  oeisIx = flip div 2 . oeisIx @2202 . succ

instance OEIS 2202 where
 oeis = f [1..] (tail $ oeis @2110) [] where
     f (x:xs) ps'@ (p:ps) us
       | x < p = f xs ps' $ O.insertSet ix us
       | otherwise = vs ++ f xs ps ws
       where (vs, ws) = span (<= ix) us
             ix = oeisIx @10 $ pred x

instance OEIS 2296 where
  oeis = 1 : [rowCol @258708 (4 * n) (3 * n) | n <- [1..]]

instance OEIS 2312 where
  oeis = filter (\x -> 2 * x > (oeisIx @6530 . pred) (x ^ 2 + 1)) [1..]

instance OEIS 2327 where
 oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @28387)

instance OEIS 2372 where
  oeisIx (succ->n) = sum $ map ((oeisIx @10051 . pred) . (2*n -)) $ takeWhile (< 2*n) (oeis @65091)

instance OEIS 2375 where
  oeisIx (succ->n) = sum $ map ((oeisIx @10051 . pred) . (2 * n -)) $ takeWhile (<= n) (oeis @65091)

instance OEIS 2385 where
  oeis = filter ((== 1) . (oeisIx @136522)) (oeis @40)

instance OEIS 2426 where
  oeisIx n = (rowCol @27907) n n

instance OEIS 2471 where
  oeisIx n = sum $ map ((oeisIx @10051) . (n -)) $ takeWhile (< n) (oeis @290)

instance OEIS 2473 where
  oeis = f $ S.singleton 1 where
     f s = x : f (s' `S.union` S.fromList (map (* x) [2,3,5,7]))
           where (x, s') = S.deleteFindMin s

instance OEIS 2479 where
  oeis = 0 : filter f [1..] where
     f x = all (even . snd) $ filter ((`elem` [5,7]) . (`mod` 8) . fst) $
                              zip ((rowT @27748) x) ((rowT @124010) x)

instance OEIS 2496 where
 oeis = filter ((== 1) . oeisIx @10051 . pred) (oeis @2522)


instance OEIS 2577 where
  oeis = f [1] where
     f xs = (p' xs $ last xs) : f (1 : map (* 2) xs)
     p' = memo2 (list integral) integral p
     p _ 0 = 1; p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m


instance OEIS 2646 where
  oeis -- TODO: Inaccurate because of div?
    = [ hqp
      | x <- [1, 3 ..]
      , y <- [1, 3 .. x - 1]
      , let hqp = div (x ^ 4 + y ^ 4) 2
      , (oeisIx @10051 . pred) hqp == 1 ]

instance OEIS 2731 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @982)) [1, 3 ..]

instance OEIS 2778 where
  oeis = filter ((== 1) . (oeisIx @136522) . (^ 2)) [0..]

instance OEIS 2779 where
  oeis = filter ((== 1) . (oeisIx @136522)) (oeis @290)

instance OEIS 2815 where
  oeisIx 0 = 0
  oeisIx n = oeisIx @46992 (pred n) + n

instance OEIS 2819 where
  oeis = scanl (+) 0 (oeis @8836)

instance OEIS 2828 where
  oeisIx 0 = 0
  oeisIx n
     | (oeisIx @10052) n == 1 = 1
     | (oeisIx @25426) n > 0 = 2
     | (oeisIx @25427) n > 0 = 3
     | otherwise = 4

instance OEIS 2858 where
 oeis = 1 : 2 : ulam 2 2 (oeis @2858)

instance OEIS 2859 where
 oeis = 1 : 3 : ulam 2 3 (oeis @2859)

instance OEIS 2868 where
  oeisIx n = if n == 0 then 1 else maximum $ map abs $ (rowT @8297) n

instance OEIS 2869 where
  oeisIx 0 = 1
  oeisIx n = maximum $ (rowT @19538) n

instance OEIS 2889 where
  oeis' (A r) = 1 : 10 : 56 : zipWith (+)
     (zipWith (-) (map (* 2) $ drop 2 r) r)
     (drop 2 $ zipWith (+) (tail $ oeis @2941) $ oeis @2941)

instance OEIS 2939 where
  oeisIx = (* 2) . (oeisIx @384)
  oeis   = 0 : scanl1 (+) (oeis @17089)

instance OEIS 2944 where
  oeisIx (succ->n) = oeisIx @3418 n `div` n

instance OEIS 2961 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @53222)

instance OEIS 2963 where
  oeisIx = ch 0 . oeisIx @61493 where
       ch s 0 = s
       ch s x = ch (s + [0,1,2,2,2,2,3,4] !! d) x'
                where  (x',d) = divMod x 10

instance OEIS 3038 where
  oeis = f (S.fromList (3 : [14, 52, 78, 133, 248]))
     (drop 2 (oeis @5563)) (drop 4 (oeis @217)) where
     f s (x:xs) (y:ys) = m : f (x `S.insert` (y `S.insert` s')) xs ys where
       (m, s') = S.deleteFindMin s

instance OEIS 3052 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @230093

instance OEIS 3071 where
  oeisIx (succ->n) = 1 - 2 ^ last es +
     sum (zipWith (*) (zipWith (+) es [0..]) (map (2 ^) es))
     where es = reverse $ (rowT @133457) n

instance OEIS 3106 where
  oeisIx = p (oeis @47221) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 3107 where
  oeis = map (p' 2) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m | m < fib   = 0
           | otherwise = p' k (m - fib) + p' (k + 1) m where fib = (oeisIx @45) k

instance OEIS 3114 where
  oeisIx = p (oeis @47209) where
     p _      0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 3128 where
  oeis = zipWith3 (\x y z -> (x - 3 * y + z) `div` 2)
                 (oeis @110) (tail (oeis @110)) (drop 2 (oeis @110))

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

instance OEIS 3160 where
  oeis = 1 : 1 : zipWith (-) [3..] (zipWith (+) xs $ tail xs)
     where xs = map (oeisIx @3160 . pred) (oeis @3160)

instance OEIS 3261 where
  oeisIx = (subtract 1) . (oeisIx @36289) . succ

instance OEIS 3401 where
  oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @209229) (oeis @10)

instance OEIS 3484 where
  oeisIx n = 2 * e + cycle [1,0,0,2] `genericIndex` e  where e = (oeisIx @7814) n

instance OEIS 3508 where
  oeis = 1 : map
        (\x -> x + 1 + sum (takeWhile (< x) $ ((rowT @27748) x))) (oeis @3508)

instance OEIS 3607 where
  oeis = elemIndices 0 (oeis @30190)

instance OEIS 3628 where
  oeis = filter ((== 1) . oeisIx @10051 . pred) (oeis @47566)

instance OEIS 3631 where
  oeis = filter ((== 1) . oeisIx @10051 . pred) (oeis @47221)

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

instance OEIS 3958 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (subtract 1) $ (rowT @27746) n

instance OEIS 3959 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (+ 1) $ (rowT @27746) n

instance OEIS 3961 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (oeisIx @40 . (oeisIx @49084) . pred) $ (rowT @27746) n

instance OEIS 4000 where
  oeis = iterate (oeisIx @36839) 1

instance OEIS 4051 where
  oeis = filter ((== 1) . oeisIx @10051 . pred) (oeis @4050)

instance OEIS 4128 where
  oeis = scanl (+) 0 (oeis @51064)

instance OEIS 4144 where
  oeis = map succ $ elemIndices 0 (oeis @5089)

instance OEIS 4169 where
  oeis = map succ $ elemIndices 0 $ map (oeisIx @209229) (oeis @10)

instance OEIS 4207 where
  oeis = 1 : iterate (oeisIx @62028) 1

instance OEIS 4214 where
  oeis = tail $ elemIndices 0 $ oeis @25427

instance OEIS 4215 where
  oeis = map succ $ elemIndices 4 $ tail $ oeis @2828

instance OEIS 4290 where
  oeisIx 0 = 0
  oeisIx n = head [x | x <- tail (oeis @7088), mod x n == 0]

instance OEIS 4426 where
  oeisIx n = (oeisIx @7953 n) `div` (oeisIx @55642 n)

instance OEIS 4435 where
  oeis = [x | x <- [1..], (oeisIx @25435) x == 0]

instance OEIS 4514 where
  oeisIx = (oeisIx @63695) . (+ 1) . (* 2)

instance OEIS 4676 where
  oeisIx = (oeisIx @7088) . (oeisIx @40)

instance OEIS 4709 where
  oeis = filter ((== 1) . (oeisIx @212793.pred)) [1..]

instance OEIS 4788 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @1142)

instance OEIS 4789 where
  oeisIx = fromJust . (`elemIndex` (oeis @4788))

instance OEIS 4999 where
  oeis = 0 : filter c2 [1..] where
     c2 x = any (== 1) $ map (oeisIx @10057) $
                         takeWhile (>= 0) $ map (x -) $ tail (oeis @578)

instance OEIS 5002 where
  oeis = 1 : zipWith (+) (map (* 2) (oeis @5002))
                                 (drop 2 (oeis @110))

instance OEIS 5089 where
  oeisIx = sum . map (oeisIx @79260 . pred) . (rowT @27748) . succ

instance OEIS 5091 where
  oeisIx = sum . map (oeisIx @79261 . pred) . (rowT @27748) . succ

instance OEIS 5094 where
  oeisIx n = (oeisIx @5089) n - (oeisIx @5091) n

instance OEIS 5097 where
  oeisIx = (`div` 2) . (oeisIx @65091)

instance OEIS 5098 where
  oeisIx = (`div` 4) . (subtract 1) . (oeisIx @2144)

instance OEIS 5100 where
  oeis = filter (\x -> (oeisIx @1065 . pred) x < x) [1..]

instance OEIS 5101 where
  oeis = filter (\x -> (oeisIx @1065 . pred) x > x) [1..]

instance OEIS 5117 where
  oeis = filter ((== 1) . (oeisIx @8966) . pred) [1..]

instance OEIS 5145 where
  oeis = tablList @5145
instance Table 5145 where
  rowCol = rowCol_off @5145 @1 @1
  rowT   = rowT_off   @5145 @1
  tabl = zipWith ($) (map replicate [1..]) (oeis @40)

instance OEIS 5153 where
  oeis = filter (\x -> all (p $ (rowT @27750) x) [1..x]) [1..]
     where p _  0 = True
           p [] _ = False
           p ds'@ (d:ds) m = d <= m && (p ds (m - d) || p ds m)

instance OEIS 5169 where
  oeisIx 0 = 1
  oeisIx n = (rowCol @168396) n 1

instance OEIS 5185 where
  oeis = 1 : 1 : zipWith (+)
     (map f $ zipWith (-) [3..] (oeis @5185))
     (map f $ zipWith (-) [3..] $ tail (oeis @5185))
    where
      f = oeisIx @5185 . pred

instance OEIS 5211 where
  oeis = oeis5211

instance OEIS 5214 where
  oeis = tail $ O.union (oeis @290) (oeis @217)

instance OEIS 5229 where
  oeis = 1 : 1 : zipWith ((+) `on` (oeisIx @5229 . pred))
                         (oeis @5229) (zipWith (-) [3..] (oeis @5229))

instance OEIS 5235 where
  oeisIx (succ->n) = head
    [ m | m <- [3, 5 ..]
        , 1 == oeisIx @10051 do (oeisIx @2110 n) + m - 1
    ]

instance OEIS 5236 where
  oeis = filter (\x -> all (<= x) $ map (oeisIx @229109 . pred) [1..x- 1]) [2..]

instance OEIS 5245 where
  oeis = 1 : f 2 [1] where
     f x ys = y : f (x + 1) (y : ys) where
       y = minimum $
           (zipWith (+) (take (x `div` 2) ys) (reverse ys)) ++
           (zipWith (+) (map (oeisIx @5245.pred) $ tail $ (rowT @161906) x)
                        (map (oeisIx @5245.pred) $ reverse $ init $ (rowT @161908) x))

instance OEIS 5248 where
  oeis = zipWith (+) (tail (oeis @1519)) (oeis @1519)

instance OEIS 5251 where
  oeis = 0 : 1 : 1 : 1 : zipWith (+) (oeis @5251)
     (drop 2 $ zipWith (+) (oeis @5251) (tail (oeis @5251)))

instance OEIS 5254 where
  oeisIx = sum . (rowT @37254) . succ

instance OEIS 5255 where
  oeis = scanl (+) 0 $ tail (oeis @2083)

instance OEIS 5256 where
  oeis = map (subtract 2) $ drop 3 (oeis @62178)

instance OEIS 5259 where
  oeis = 1 : 5 : zipWith div (zipWith (-)
     (tail $ zipWith (*) (oeis @6221) (oeis @5259))
     (zipWith (*) (tail (oeis @578)) (oeis @5259))) (drop 2 (oeis @578))

instance OEIS 5277 where
  oeis = filter even (oeis @7617)

instance OEIS 5314 where
  oeis = 0 : 1 : 2 : zipWith (+) (oeis @5314)
     (tail $ zipWith (-) (map (2 *) $ tail (oeis @5314)) (oeis @5314))

instance OEIS 5318 where
  oeis = 0 : 1 : zipWith (-)
     (map (* 2) $ tail (oeis @5318)) (map (oeisIx @5318) (oeis @83920))

instance OEIS 5374 where
  oeis = 0 : 1 : zipWith (-)
     [2..] (map (oeisIx @5374 . (oeisIx @5374)) $ tail (oeis @5374))

instance OEIS 5375 where
  oeis =  0 : 1 : zipWith (-)
     [2..] (map (oeisIx @5375) (map (oeisIx @5375) (map (oeisIx @5375) (tail (oeis @5375)))))

instance OEIS 5382 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (subtract 1) . (* 2)) (oeis @40)

instance OEIS 5383 where
  oeis = [p | p <- (oeis @65091), (oeisIx @10051 . pred) ((p + 1) `div` 2) == 1]

instance OEIS 5385 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (`div` 2)) (oeis @40)

instance OEIS 5408 where
  oeisIx = (+ 1) . (* 2)
  oeis   = [1, 3 ..]

instance OEIS 5409 where
  oeis = 1 : scanl1 (+) (tail (oeis @1333))

instance OEIS 5412 where
  oeis = 1 : f 2 [1] where
     f v ws@ (w:_) = y : f (v + 2) (y : ws) where
                    y = v * w + (sum $ zipWith (*) ws $ reverse ws)

instance OEIS 5418 where
  oeisIx = sum . rowT @34851

instance OEIS 5425 where
  oeis = 1 : 2 : zipWith (+)
     (map (* 2) (tail (oeis @5425))) (zipWith (*) [1..] (oeis @5425))

instance OEIS 5473 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ map (+ 4) (oeis @290)

instance OEIS 5528 where
  oeis = filter (\x -> 2 * x <= (oeisIx @6530 . pred) (x ^ 2 + 1)) [1..]

instance OEIS 5563 where
  oeisIx n = n * (n + 2)
  oeis = zipWith (*) [0..] [2..]

instance OEIS 5574 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (+ 1) . (^ 2)) [0..]

instance OEIS 5590 where
  oeis = 0 : 1 : concat (tail $ transpose
     [oeis @5590, zipWith (-) (tail (oeis @5590)) (oeis @5590)])

instance OEIS 5592 where
  oeis = (1:) . map (subtract 1) $ tail $ zipWith (+) (oeis @1519) $ tail (oeis @1519)

instance OEIS 5598 where
  oeisIx n = 1 + sum (zipWith (*) [n, n - 1 .. 1] (oeis @10))

instance OEIS 5599 where
  oeis = scanl (+) 0 $ f (oeis @106400)
     where f (x:_:_:xs) = x : f xs

instance OEIS 5605 where
  oeis = 0 : 1 : zipWith (+) (tail (oeis @5605))
     (zipWith (*) (cycle [-1,1]) (map (^ 2) $ (oeis @5605)))

instance OEIS 5614 where
  oeis = map (1 -) (oeis @3849)

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

instance OEIS 5728 where
  oeis = scanl (+) 1 (oeis @10)

instance OEIS 5774 where
  oeisIx 0 = 0
  oeisIx n = (rowCol @38622) n 1

instance OEIS 5803 where
  oeisIx n = 2 ^ n - 2 * n
  oeis = 1 : f 1 [0, 2 ..] where
     f x (z:zs@ (z':_)) = y : f y zs  where y = (x + z) * 2 - z'

instance OEIS 5811 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ group $ (rowT @30308) n
  oeis = 0 : f [1] where
     f (x:xs) = x : f (xs ++ [x + x `mod` 2, x + 1 - x `mod` 2])

instance OEIS 5831 where
  oeis = 0:1:zipWith (*) (tail (oeis @5831)) (map succ (oeis @5831))

instance OEIS 5836 where
  oeis = filter ((== 1) . (oeisIx @39966)) [0..]

instance OEIS 5843 where
  oeisIx = (* 2)
  oeis = [0, 2 ..]

instance OEIS 5867 where
  oeis = scanl (*) 1 (oeis @6093)

instance OEIS 5900 where
  oeisIx n = sum $ zipWith (*) odds $ reverse odds
    where odds = take n (oeis @5408)
  oeis = scanl (+) 0 (oeis @1844)

instance OEIS 5917 where
  oeis = map sum $ f 1 [1, 3 ..] where
     f x ws = us : f (x + 2) vs where (us, vs) = splitAt x ws

instance OEIS 5940 where
  oeisIx (succ->n) = f (n - 1) 1 1 where
     f 0 y _          = y
     f x y i | m == 0 = f x' y (i + 1)
             | m == 1 = f x' (y * (oeisIx @40 . pred) i) i
             where (x',m) = divMod x 2

instance OEIS 6038 where
  oeis = filter f [945, 955 ..] where
     f x = sum pdivs > x
        && all (\d -> (oeisIx @203 . pred) d - 2 * d <= 0) pdivs
           where pdivs = (rowT @27751) x

instance OEIS 6046 where
  oeisIx = sum . concat . (`take` (tabl @47999))

instance OEIS 6047 where
  oeisIx = sum . map signum . (rowT @83093)

instance OEIS 6093 where
  oeisIx = (subtract 1) . (oeisIx @40)

instance OEIS 6094 where
  oeis = zipWith (*) (oeis @40) (oeis @65091)

instance OEIS 6127 where
  oeisIx n = (oeisIx @79) n + n
  oeis = s [1] where
     s xs = last xs : (s $ zipWith (+) [1..] (xs ++ reverse xs))

instance OEIS 6128 where
  oeisIx = genericLength . concat . ps 1 where
     ps _ 0 = [[]]
     ps i j = [t:ts | t <- [i..j], ts <- ps t (j - t)]

instance OEIS 6190 where
  oeis = 0 : 1 : zipWith (+) (map (* 3) $ tail (oeis @6190)) (oeis @6190)

instance OEIS 6206 where
  oeisIx (succ->n) = sum (map f $ (rowT @27750) n) `div` n where
     f d = (oeisIx @8683 . pred) (n `div` d) * (oeisIx @45 (d - 1) + (oeisIx @45) (d + 1))

instance OEIS 6231 where
  oeisIx (succ->n) = numerator $
     sum $ tail $ zipWith (%) (scanl1 (*) [n, (n - 1)..1]) [1..n]

instance OEIS 6252 where
  oeisIx 0 = 1
  oeisIx n = sum $ (rowT @48594) n

instance OEIS 6256 where
  oeis = f (tail (oeis @5809)) [1] where
     f (x:xs) zs = (sum $ zipWith (*) zs (oeis @5809)) : f xs (x : zs)

instance OEIS 6257 where
  oeis = 0 : 1 : (map (+ 1) $ zipWith mod (map (+ 1) $ tail (oeis @6257)) [2..])

instance OEIS 6277 where
  oeis = 1 : fix \r -> 1 : scanl ((*) . (+ 1)) 2 r

instance OEIS 6318 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where
       y = head xs + sum (zipWith (*) xs $ reverse xs)

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

instance OEIS 6431 where
  oeis = elemIndices 1 (oeis @2635)

instance OEIS 6446 where
  oeis = filter (\x -> x `mod` (oeisIx @196) x == 0) [1..]

instance OEIS 6449 where
  oeisIx = sum . (rowT @45995)

instance OEIS 6450 where
  oeisIx = (oeisIx @40 . pred) . (oeisIx @40)
  oeis   = map (oeisIx @40 . pred) (oeis @40)

instance OEIS 6451 where
  oeis = 0 : 2 : 5 : 15 : map (+ 2)
     (zipWith (-) (map (* 6) (drop 2 (oeis @6451))) (oeis @6451))

instance OEIS 6460 where
  oeisIx = f 0 . succ where
     f k x | mod k 3 == 0 && x `elem` [1, 2, 4] = x
           | otherwise                          = f (k+1) (oeisIx @6370 x)

instance OEIS 6489 where
  oeis = filter
     ((== 1) . (oeisIx @10051 . pred) . (subtract 6)) $ dropWhile (<= 6) (oeis @23201)

instance OEIS 6507 where
  oeis = iterate (oeisIx @62028) 7

instance OEIS 6512 where
  oeisIx = (+ 2) . (oeisIx @1359)

instance OEIS 6516 where
  oeis = 0 : 1 :
      zipWith (-) (map (* 6) $ tail (oeis @6516)) (map (* 8) (oeis @6516))

instance OEIS 6521 where
  oeis = filter (\x -> (oeisIx @51) x `mod` x == 0) [1..]

instance OEIS 6530 where -- TODO: Optimize this- it's used a lot
  oeisIx n
    | null fs = 1
    | let     = fi . unPrime . fst $ last fs
    where fs  = factorise (1 + fi n :: Int)

instance OEIS 6562 where
  oeis = filter ((== 1) . (oeisIx @10051).pred) (oeis @75540)

instance OEIS 6566 where
  oeisIx n = n * (3 * n - 1) * (3 * n - 2) `div` 2
  oeis = scanl (+) 0 (oeis @93485)

instance OEIS 6567 where
  oeis = filter f (oeis @40) where
     f p = (oeisIx @10051 . pred) q == 1 && q /= p  where q = (oeisIx @4086) p

instance OEIS 6590 where
  oeisIx (succ->n) = sum $ map f [1..n] where
     f x = y + 1 - 0 ^ r where (y, r) = divMod n x

instance OEIS 6659 where
  oeisIx (succ->n) = 2 * (rowCol @7318) (2 * n + 2) (n - 1)

instance OEIS 6666 where
  oeisIx = length . filter even . takeWhile (> 1) . (iterate (oeisIx @6370)) . succ


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

instance OEIS 6769 where
  -- oeisIx n = (oeis @50512) !! n
  oeis = 0 : 1 : 1 : (-1) : 1 : zipWith div (zipWith (+) (zipWith (*)
     (drop 4 (oeis @6769)) (drop 2 (oeis @6769)))
       (map (^ 2) (drop 3 (oeis @6769)))) (tail (oeis @6769))

instance OEIS 6844 where
  oeis = 4 : 5 : ulam 2 5 (oeis @6844)

instance OEIS 6881 where
  oeis = filter chi [1..] where
     chi n = p /= q && (oeisIx @10051) (pred q) == 1 where
        p = (oeisIx @20639) $ pred n
        q = n `div` p

instance OEIS 6882 where
  oeis = 1 : 1 : zipWith (*) [2..] (oeis @6882)

instance OEIS 6884 where
  oeis = f 1 0 (oeis @25586) where
     f i r (x:xs) = if x > r then i : f (i + 1) x xs else f (i + 1) r xs

instance OEIS 6885 where
  oeisIx = (oeisIx @25586) . pred . (oeisIx @6884)

instance OEIS 6895 where
  oeis = 1 : f 0 0 (tail (oeis @79)) (tail (oeis @244)) where
     f x y us'@ (u:us) vs'@ (v:vs)
       | x > 0     = (u - x) : f 0 (u - x + y) us vs'
       | y > v - u = (v - y) : f (v + x - y) 0 us' vs
       | otherwise =       u : f 0 (u + y) us vs'

instance OEIS 6921 where
  oeisIx = sum . zipWith (*)
                  (oeis @79) . map (flip mod 2) . reverse . (rowT @11973)

instance OEIS 6939 where
  oeis = scanl1 (*) (oeis @2110)

instance OEIS 6949 where
  oeis = 1 : 1 : 1 : zipWith (+) xs (tail xs)
     where xs = map (oeisIx @6949) $ zipWith (-) [1..] $ tail (oeis @6949)

instance OEIS 6960 where
  oeis = iterate (oeisIx @56964) 196

instance OEIS 6968 where
  oeisIx = fi . lenRom 3 . fi . succ where
     lenRom 0 z = z
     lenRom p z = [0, 1, 2, 3, 2, 1, 2, 3, 4, 2] !! m + lenRom (p - 1) z'
                  where (z',m) = divMod z 10

instance OEIS 6985 where
  oeis = 1 : map (oeisIx @45 . (+ 2)) (oeis @6985)

instance OEIS 6995 where
  oeis = 0 : filter ((== 1) . (oeisIx @178225)) (oeis @5408)

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

instance OEIS 7012 where
  oeis = 1 : map (sum . map signum) (tail (tabl @53200))

instance OEIS 7018 where
  oeis = iterate (oeisIx @2378) 1

instance OEIS 7047 where
  oeisIx = sum . (rowT @38719)

instance OEIS 7061 where
  oeis = 1 : f [1] where
     f us = a' : f (us ++ [a']) where
       a' = b $ reverse $ map (`splitAt` us) [0..length us - 1] where
          b ((xs,ys):xyss) | vs `isSuffixOf` xs = 3 - head ys
                           | otherwise          = b xyss
       vs = fromJust $ find (`isInfixOf` init us) $ tails us

instance OEIS 7066 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = if x `elem` zs then z + 2 else z + 3

instance OEIS 7068 where
  oeis = 1 : 3 : zipWith (+)
     (tail (oeis @7068)) (zipWith (*) (oeis @34) (oeis @7068))

instance OEIS 7070 where
  oeis = 1 : 4 : (map (* 2) $ zipWith (-)
     (tail $ map (* 2) (oeis @7070)) (oeis @7070))

instance OEIS 7300 where
  oeis = 2 : 5 : ulam 2 5 (oeis @7300)

instance OEIS 7304 where
  oeis = map succ $ filter f [1..] where
    f u = p < q && q < w && (oeisIx @10051) w == 1 where
      p = (oeisIx @20639) u
      v = div u p
      q = (oeisIx @20639) v
      w = div v q

instance OEIS 7377 where
  oeis = elemIndices 0 (oeis @27870)

instance OEIS 7416 where
  oeis = f 1 [] where
     f x ts = if tau `elem` ts then f (x + 1) ts else x : f (x + 1) (tau:ts)
              where tau = (oeisIx @5 . pred) x

instance OEIS 7420 where
  oeis = 0 : 0 : 1 : (map (* 2) $ zipWith (+) (drop 2 (oeis @7420))
     (map (* 2) $ zipWith (-) (oeis @7420) (tail (oeis @7420))))

instance OEIS 7422 where
  oeis = [x | x <- [1..], (oeisIx @7956 . pred) x == x]

instance OEIS 7425 where
  oeisIx = sum . map (oeisIx @5.pred) . (rowT @27750) . succ

instance OEIS 7427 where
  oeisIx (succ->n) = sum $ zipWith (*) mds $ reverse mds where
     mds = (rowT @225817) n

instance OEIS 7430 where
  oeisIx (succ->n) = sum $ zipWith (*) (map (oeisIx @5.pred) ds) (map (oeisIx @203.pred) $ reverse ds)
              where ds = (rowT @27750) n

instance OEIS 7431 where
  oeisIx 0 = 0
  oeisIx n = sum $ map (oeisIx @8683.pred . gcd n) [1..n]

instance OEIS 7448 where
  oeis = f [0] [0] where
     f (x:xs) (y:ys) = z : f (xs ++ [2*z,2*z]) (ys ++ [3*z,3*z,3*z])
       where z = 1 + min x y

instance OEIS 7455 where
  oeis = 1 : 1 : 3 : 5 : zipWith (+)
     (map (* 2) (oeis @7455)) (map (* 3) $ drop 2 (oeis @7455))

instance OEIS 7456 where
  oeisIx 0 = 0
  oeisIx (succ->n) = (oeisIx @523 . pred) (n - 1) + mod n 2 + 1

instance OEIS 7459 where
  oeis = f 1 (oeis @40) where
    f q (p:ps) = if mod q (p - 1) == 0 then p : f (q * p ^ 2) ps else f q ps

instance OEIS 7466 where
  oeisIx (succ->n) = (rowCol @228643) n n

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

instance OEIS 7491 where
  oeisIx = (oeisIx @7918) . (oeisIx @290) . succ

instance OEIS 7497 where
  oeis = iterate (oeisIx @203 . pred) 2

instance OEIS 7500 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @4086)) (oeis @40)

instance OEIS 7503 where
  oeisIx = sum . map (+ 1) . (rowT @27750) . succ

instance OEIS 7505 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred) (oeis @83329)

instance OEIS 7521 where
  oeis = filter ((== 1). (oeisIx @10051) . pred) (oeis @4770)

instance OEIS 7526 where
  oeis = 0 : zipWith (*) [1..] (map (+ 1) (oeis @7526))

instance OEIS 7528 where
  oeis = [x | k <- [0..], let x = 6 * k + 5, (oeisIx @10051 . pred) x == 1]

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

instance OEIS 7556 where
  oeisIx 0 = 1
  oeisIx n = (rowCol @7318) (8 * n) (n - 1) `div` n

instance OEIS 7599 where
  oeis = 1 : f 1 1  where
     f x m = y : f y (m + 1) where
       y = x + (iterate (oeisIx @7599) (m- 2)) !! (m `div` 2)

instance OEIS 7602 where
  oeis = map succ $ elemIndices 1 $ map (oeisIx @188642) [1..]

instance OEIS 7606 where
  oeis = takeSkip 1 [1..] where
     takeSkip k xs = take k xs ++ takeSkip (k + 2) (drop (2*k + 1) xs)

instance OEIS 7607 where
  oeis = skipTake 1 [1..] where
     skipTake k xs = take (k + 1) (drop k xs)
                     ++ skipTake (k + 2) (drop (2*k + 1) xs)

instance OEIS 7612 where
  oeis = iterate (oeisIx @64806 . pred) 1

instance OEIS 7614 where
  oeis = f [1..] (oeis @2110) [] where
     f xs'@ (x:xs) ps'@ (p:ps) us
       | x < p = f xs ps' $ O.insertBag (oeisIx @10 $ pred x) us
       | otherwise = vs ++ f xs' ps ws
       where (vs, ws) = span (<= (oeisIx @10 . pred) x) us

instance OEIS 7617 where
  oeis = [1..] `O.minus` (oeis @2202)

instance OEIS 7618 where
  oeis = iterate (oeisIx @62028) 5

instance OEIS 7628 where
  oeis = filter f (oeis @125308) where
     f p = (oeisIx @10051 . pred) q == 1 && q /= p  where q = (oeisIx @4086) p

instance OEIS 7632 where
  oeis = filter ((== 1) . (oeisIx @178225)) (oeis @2113)

instance OEIS 7645 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ tail (oeis @3136)

instance OEIS 7651 where
  oeisIx = foldl1 (\v d -> 10 * v + d) . (rowT @220424) . succ

instance OEIS 7660 where
  oeis = 0 : 0 : map (+ 1) (zipWith (*) (oeis @7660) $ tail (oeis @7660))

instance OEIS 7689 where
  oeisIx n = (oeisIx @79) n + (oeisIx @244) n

instance OEIS 7692 where
  oeis = findIndices (> 1) (oeis @25426)

instance OEIS 7694 where
  oeis = 1 : filter even (oeis @3586)

instance OEIS 7697 where
  oeisIx (succ->n) = 2 * (fromJust $ findIndex (>= n) (oeis @46921)) + 1

instance OEIS 7731 where
  oeis = 1 : (zipWith3 (\u v w -> u + v + w)
     (map (oeisIx @7731 . (`div` 2)) [1..])
     (map (oeisIx @7731 . (`div` 3)) [1..])
     (map (oeisIx @7731 . (`div` 6)) [1..]))

instance OEIS 7733 where
  oeisIx = (oeisIx @2326) . flip div 2 . subtract 1 . (oeisIx @265)

instance OEIS 7770 where
  oeis = filter ((== 1) . (oeisIx @103369) . pred) [1..]

instance OEIS 7774 where
  oeis = filter ((== 2) . (oeisIx @1221) . pred) [1..]

instance OEIS 7775 where
  oeis = 1 : filter ((> 5) . oeisIx @20639 . pred) [7..]

instance OEIS 7814 where
  oeisIx (succ->n) = if m == 0 then 1 + (oeisIx @7814 . pred) n' else 0
              where (n', m) = divMod n 2

instance OEIS 7821 where
  oeisIx = (oeisIx @40) . pred . (oeisIx @18252)

instance OEIS 7862 where
  oeisIx = sum . map (oeisIx @10054) . (rowT @27750) . succ

instance OEIS 7875 where
  oeisIx = genericLength . filter (> 0) . (rowT @225817) . succ

instance OEIS 7913 where
  oeisIx (succ->n) = product $ zipWith (^) (rowT @27748 n) (map (`mod` 2) $ (rowT @124010) n)

instance OEIS 7917 where
  oeisIx n = f (n + 2)
    where
      f m | oeisIx @10051 (m - 1) == 1 = m
          | let                        = f (m - 1)

instance OEIS 7918 where
  oeis = 2 : 2 : 2 : concat do
    zipWith (\p q -> (replicate (q - p) q))
            (oeis @40) $ tail (oeis @40)

instance OEIS 7921 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred) . (+ 2)) [1, 3 ..]

instance OEIS 7928 where
  oeis = findIndices (> 0) (oeis @196563)

instance OEIS 7931 where
  oeisIx (succ->n) = f (n + 1) where
     f x = if x < 2 then 0 else (10 * f x') + m + 1
       where (x', m) = divMod x 2

instance OEIS 7947 where
  oeisIx = product . (rowT @27748) . succ

instance OEIS 7948 where
  oeisIx = last . filter ((== 1) . (oeisIx @212793.pred)) . (rowT @27750).succ

instance OEIS 7952 where
  oeis = f 1 [0..] where
     f k (x:xs) = x : f (k + 1) (g xs) where
       g ws = us ++ (g vs) where (us, _:vs) = splitAt k ws

instance OEIS 7956 where
  oeisIx = product . (rowT @27751) . succ

instance OEIS 7966 where
  oeisIx = (\(_,x,_,_,_) -> x) . happy

instance OEIS 7967 where
  oeisIx = (\(_,_,x,_,_) -> x) . happy

instance OEIS 7968 where
  oeisIx = (\(x,_,_,_,_) -> x) . happy

instance OEIS 7969 where
  oeis = filter ((== 1) . oeisIx @7968) [0..]

instance OEIS 7970 where
  oeis = filter ((== 2) . oeisIx @7968) [0..]

instance OEIS 7978 where
  oeisIx = head . ([1..] \\) . (rowT @27750) . succ

instance OEIS 7997 where
  -- oeisIx n = ceiling $ (fi $ (n - 3) * (n - 4)) / 6
  oeis = 0 : 0 : 1 : zipWith (+) (oeis @7997) [1..]

instance OEIS 8275 where
  oeis = tablList @8275
instance Table 8275 where
  rowCol = rowCol_off @8275 @1 @1
  rowT   = rowT_off   @8275 @1
  tabl = map tail $ tail (tabl @48994)

instance OEIS 8276 where
  oeis = tablList @8276
instance Table 8276 where
  rowCol = rowCol_off @8276 @1 @1
  rowT   = rowT_off   @8276 @1
  tabl = map init $ tail (tabl @54654)

instance OEIS 8277 where
  oeis = tablList @8277
instance Table 8277 where
  rowCol = rowCol_off @8277 @1 @1
  rowT   = rowT_off   @8277 @1
  tabl = map tail $ (tabl @48993)

instance OEIS 8278 where
  oeis = tablList @8278
instance Table 8278 where
  rowCol = rowCol_off @8278 @1 @1
  rowT   = rowT_off   @8278 @1
  tabl = iterate st2 [1] where
    st2 row = zipWith (+) ([0] ++ row') (row ++ [0])
              where row' = reverse $ zipWith (*) [1..] $ reverse row

instance OEIS 8279 where
  oeis = tablList @8279
instance Table 8279 where
  tabl = iterate f [1] where
     f xs = zipWith (+) ([0] ++ zipWith (*) xs [1..]) (xs ++ [0])

instance OEIS 8282 where
  oeis = tablList @8282
instance Table 8282 where
  rowCol = rowCol_off @8282 @1 @1
  rowT   = rowT_off   @8282 @1
  tabl = iterate f [1] where
     f xs = zs ++ [last zs] where zs = scanl1 (+) (reverse xs)

instance OEIS 8284 where
  oeis = tablList @8284
instance Table 8284 where
  rowCol = rowCol_off @8284 @1 @1
  rowT   = rowT_off   @8284 @1
  tabl = [1] : f [[1]] where
     f xss = ys : f (ys : xss) where
       ys = (map sum $ zipWith take [1..] xss) ++ [1]

instance OEIS 8288 where
  oeis = tablList @8288
instance Table 8288 where
  tabl = map fst $ iterate
      (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

instance OEIS 8290 where
  oeis = tablList @8290
instance Table 8290 where
  tabl = map reverse (tabl @98825)

instance OEIS 8292 where
  oeis = tablList @8292
instance Table 8292 where
  rowCol = rowCol_off @8292 @1 @1
  rowT   = rowT_off   @8292 @1
  tabl = iterate f [1] where
     f xs = zipWith (+)
       (zipWith (*) ([0] ++ xs) (reverse ks)) (zipWith (*) (xs ++ [0]) ks)
       where ks = [1 .. 1 + genericLength xs]

instance OEIS 8297 where
  oeis = tablList @8297
instance Table 8297 where
  rowCol = rowCol_off @8297 @1 @1
  rowT   = rowT_off   @8297 @1
  tabl = [-1] : f [-1] 2 where
     f xs i = ys : f ys (i + 1) where
       ys = map negate $
            zipWith (+) ([0] ++ xs) (zipWith (*) [i, i + 1 ..] (xs ++ [0]))

instance OEIS 8301 where
  oeis = tablList @8301
instance Table 8301 where
  tabf = iterate f [1] where
     f zs = zs' ++ tail (reverse zs') where
       zs' = (sum zs) : h (0 : take (length zs `div` 2) zs) (sum zs) 0
       h []     _  _ = []
       h (x:xs) y' y = y'' : h xs y'' y' where y'' = 2*y' - 2*x - y

instance OEIS 8306 where
  oeis = tablList @8306
instance Table 8306 where
  rowCol = rowCol_off @8306 @2 @1
  rowT   = rowT_off @8306 @2
  tabf = map (fst . fst) $ iterate f (([1], [2]), 3) where
     f ((us, vs), x) =
       ((vs, map (* x) $ zipWith (+) ([0] ++ us) (vs ++ [0])), x + 1)

instance OEIS 8313 where
  oeis = tablList @8313
instance Table 8313 where
  tabf = map (filter (> 0)) (tabl @53121)

instance OEIS 8315 where
  oeis = tablList @8315
instance Table 8315 where
  tabf = map reverse (tabf @8313)

instance OEIS 8336 where
  oeis = 1 : zipWith (/*) (oeis @8336) [1..] where
      x /* y = if x `mod` y == 0 then x `div` y else x*y

instance OEIS 8344 where
  oeis = 0 : f 0 [1..] where
     f x (z:zs) = y : f y zs where y = if x < z then x + z else x - z

instance OEIS 8347 where
  oeis = 0 : zipWith (-) (oeis @40) (oeis @8347)

instance OEIS 8364 where
  oeis = 1 : filter ((> 7) . (oeisIx @20639) . pred) [1..]

instance OEIS 8438 where
  oeisIx = (oeisIx @203) . pred . (oeisIx @5408)

instance OEIS 8441 where
  oeisIx n = sumdiv (4*n+1) $ oeisIx @57077

instance OEIS 8472 where
  oeisIx 0 = 0
  oeisIx n = sum . (rowT @27748) $ succ n

instance OEIS 8475 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ (rowT @141809) n

instance OEIS 8480 where
  oeisIx (succ->n) = foldl div (oeisIx @142 $ sum es) (map (oeisIx @142) es)
              where es = (rowT @124010) n

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

instance OEIS 8836 where
  oeisIx = (1 -) . (* 2) . (oeisIx @66829)

instance OEIS 8846 where
  oeis = filter f [5..] where
     f n = all ((== 1) . (`mod` 4)) $ filter ((== 0) . (n `mod`)) [1..n]

instance OEIS 8864 where
  oeisIx = (+ 1) . (oeisIx @40)

instance OEIS 8865 where
  oeisIx = (subtract 2) . (^ 2) . succ
  oeis = scanl (+) (-1) [3, 5 ..]

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

instance OEIS 8905 where
  oeisIx = (oeisIx @30) . (oeisIx @142)

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

instance OEIS 8957 where
  oeis = tablList @8957
instance Table 8957 where
  rowCol = rowCol_off @8957 @1 @1
  rowT   = rowT_off   @8957 @1
  tabl = map reverse (tabl @36969)

instance OEIS 8975 where
  oeis = tablList @8975
instance Table 8975 where
  tabl = iterate
     (\row -> map (`mod` 10) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 8996 where
  oeis = 1 : f 0 (filter (> 1) $
                          map length $ group $ drop 3 (oeis @10051))
     where f m (u : us) = if u <= m then f m us else u : f u us

instance OEIS 9003 where
  oeis = map (+ 1) $ findIndices (> 0) (oeis @5089)

instance OEIS 9191 where
  oeisIx n = gcd (1+n) $ (oeisIx @5) n

instance OEIS 9194 where
  oeisIx n = gcd (oeisIx @203 n) (1+n)

instance OEIS 9195 where
  oeisIx n = (1+n) `gcd` (oeisIx @10) n

instance OEIS 9223 where
  oeisIx n = gcd (oeisIx @203 n) (oeisIx @10 n)

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

instance OEIS 9998 where
  oeis = tablList @9998
instance Table 9998 where
  rowCol n k = (k + 1) ^ (n - k)
  tabl = map reverse (tabl @9999)

instance OEIS 9999 where
  oeis = tablList @9999
instance Table 9999 where
  rowCol n k = (n + 1 - k) ^ k
  tabl = [1] : map snd (iterate f ([1,1], [1,1])) where
     f (us@ (u:_), vs) = (us', 1 : zipWith (*) us' vs)
                        where us' = (u + 1) : us

instance OEIS 10049 where
  oeis = uncurry c $ splitAt 1 (oeis @45) where
     c us (v:vs) = (sum $ zipWith (*) us (1 : reverse us)) : c (v:us) vs

instance OEIS 10054 where
  oeisIx = (oeisIx @10052) . (+ 1) . (* 8)
  oeis   = concatMap (\x -> 1 : replicate x 0) [0..]

instance OEIS 10055 where
  oeisIx n = if (oeisIx @1221) n <= 1 then 1 else 0

instance OEIS 10056 where
  oeis = 1 : 1 : ch [2..] (drop 3 (oeis @45)) where
     ch (x:xs) fs'@ (f:fs) = if x == f then 1 : ch xs fs else 0 : ch xs fs'

instance OEIS 10057 where
  oeisIx 0 = 1
  oeisIx n = fi . fromEnum $ all ((== 0) . (`mod` 3)) $ (rowT @124010) n
  oeis = concatMap (\x -> 1 : replicate (oeisIx @3215 x - 1) 0) [0..]

instance OEIS 10061 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @228085

instance OEIS 10062 where
  oeis = iterate (oeisIx @92391) 1

instance OEIS 10065 where
  oeis = iterate (oeisIx @230631) 1

instance OEIS 10330 where
  oeisIx = (+ 2) . (oeisIx @2311)

instance OEIS 10551 where
  oeis = scanl (*) 1 (oeis @8619)

instance OEIS 10693 where
  oeisIx = (+ 2) . (`mod` 2)
  oeis = cycle [2,3]

instance OEIS 10701 where
  oeisIx = const 3
  oeis = repeat 3

instance OEIS 10702 where
  oeisIx = (+ 3) . (`mod` 2)
  oeis = cycle [3,4]

instance OEIS 10704 where
  oeisIx = (* 3) . (oeisIx @34)
  oeis = cycle [3,6]

instance OEIS 10712 where
  oeisIx = (+ 3) . (4 ^) . flip mod 2
  oeis = cycle [4, 7]

instance OEIS 10766 where
  oeis = tablList @10766
instance Table 10766 where
  rowCol = div
  rowT = rowT_off @10766 @1
  tabl = zipWith (map . div) [1..] (tabl @2260)

instance OEIS 10783 where
  oeis = tablList @10783
instance Table 10783 where
  rowCol n k = (n + 1 - k) `div` k
  rowT   = rowT_off   @10783 @1
  tabl = map reverse (tabl @10766)

instance OEIS 10784 where
  oeis = elemIndices 1 (oeis @178788)

instance OEIS 10785 where
  oeis = 0 : r [1..9] where
     r (x:xs) = x : r (xs ++ [10*x + x `mod` 10])

instance OEIS 11540 where
  oeis = elemIndices 0 (oeis @168046)

instance OEIS 11769 where
  oeis = 1 : zipWith (-) (map (* 3) (oeis @11769)) (oeis @59727)

instance OEIS 11782 where
  oeis = 1 : scanl1 (+) (oeis @11782)

instance OEIS 11971 where
  oeis = tablList @11971
instance Table 11971 where
  tabl = iterate (\row -> scanl (+) (last row) row) [1]

instance OEIS 11973 where
  oeis = tablList @11973
instance Table 11973 where
  tabf = zipWith (zipWith (rowCol @7318)) (tabl @25581) (tabf @55087)

instance OEIS 12257 where
  oeis = tablList @12257
instance Table 12257 where
  rowCol = rowCol_off @12257 @1 @1
  rowT   = rowT_off @12257 @1
  tabf = iterate (\row -> concat $
                          zipWith replicate (reverse row) [1..]) [1, 1]

instance OEIS 13610 where
  oeis = tablList @13610
instance Table 13610 where
  tabl = iterate (\row ->
     zipWith (+) (map (* 1) (row ++ [0])) (map (* 3) ([0] ++ row))) [1]

instance OEIS 13613 where
  oeis = tablList @13613
instance Table 13613 where
  tabl = zipWith (zipWith (*))
                 (tail $ inits (oeis @400)) (tabl @7318)

instance OEIS 13620 where
  oeis = tablList @13620
instance Table 13620 where
  tabl = iterate (\row ->
     zipWith (+) (map (* 2) (row ++ [0])) (map (* 3) ([0] ++ row))) [1]

instance OEIS 13918 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (tail $ oeis @7504)

instance OEIS 13928 where
  oeis = scanl (+) 0 $ map (oeisIx @8966 . pred) [1..]

instance OEIS 13939 where
  oeis = scanl (+) 0 $ map (oeisIx @1221) [1..]

instance OEIS 13942 where
  oeis = tablList @13942
instance Table 13942 where
  rowCol = rowCol_off @13942 @1 @1
  rowT n = map (div (n * 2)) [1 .. 2 * n]
  tabf = map (rowT @13942) [1 ..]

instance OEIS 13979 where
  oeis = 1 : 0 : 1 : 1 : zipWith (+) (oeis @13979)
     (zipWith (+) (tail (oeis @13979)) (drop 2 (oeis @13979)))

instance OEIS 14011 where
  oeis = 1 : f 2 [1] where
     f u vs = w : f (u + 1) (w : vs) where
       w = maximum $ zipWith (*) [u, u - 1 ..] $ map (u -) vs

instance OEIS 14076 where
  oeis = filter ((== 0) . (oeisIx @10051) . pred) (oeis @5408)

instance OEIS 14082 where
  oeisIx = sum . map (fi . fromEnum . ([1,1,1] `isPrefixOf`)) .
                      tails . (rowT @30308)

instance OEIS 14085 where
  oeisIx 0 = 0
  oeisIx n = sum $ map (oeisIx @10051 . pred) [n^2.. (n+1)^2]

instance OEIS 14091 where
  oeis = filter (\x -> any ((== 1) . (oeisIx @10051 . pred)) $
                        map (x -) $ takeWhile (< x) (oeis @40)) [1..]

instance OEIS 14092 where
  oeis = filter (\x ->
     all ((== 0) . (oeisIx @10051 . pred)) $ map (x -) $ takeWhile (< x) (oeis @40)) [1..]

instance OEIS 14118 where
  oeis = iterate (oeisIx @5836) 2

instance OEIS 14132 where
--   oeisIx n = n + round (sqrt $ 2 * fromInteger n)
  oeis = elemIndices 0 (oeis @10054)

instance OEIS 14138 where
  oeis = scanl (+) 0 $ tail (oeis @108)

instance OEIS 14148 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 2

instance OEIS 14150 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 3

instance OEIS 14217 where
  oeis = 1 : 1 : zipWith (+)
     (oeis @35) (zipWith (+) (oeis @14217) $ tail (oeis @14217))

instance OEIS 14237 where
  oeisIx n = (oeisIx @40) n - (oeisIx @18252) n

instance OEIS 14311 where
  oeis = [2^x + 2^y + 2^z | x <- [2..], y <- [1..x - 1], z <- [0..y - 1]]

instance OEIS 14410 where
  oeis = tablList @14410
instance Table 14410 where
  rowCol = rowCol_off @14410 @2 @1
  rowT = rowT_off @14410 @2
  tabl = map (init . tail) $ drop 2 (tabl @7318)

instance OEIS 14413 where
  oeis = tablList @14413
instance Table 14413 where
  rowCol = rowCol_off @14413 @1 @1
  rowT   = rowT_off @14413 @1
  tabf = [1] : f 1 [1] where
     f 0 us'@ (_:us) = ys : f 1 ys where
                      ys = zipWith (+) us' (us ++ [0])
     f 1 vs@ (v:_) = ys' : f 0 ys where
                    ys@ (_:ys') = zipWith (+) (vs ++ [0]) ([v] ++ vs)

instance OEIS 14430 where
  oeis = tablList @14430
instance Table 14430 where
  tabl = map (init . tail) $ drop 2 (tabl @14473)

instance OEIS 14462 where
  oeis = tablList @14462
instance Table 14462 where
  rowCol = rowCol_off @14462 @1 @1
  rowT   = rowT_off @14462 @1
  tabf = map reverse (tabf @14413)

instance OEIS 14473 where
  oeis = tablList @14473
instance Table 14473 where
  tabl = map (map (subtract 1)) (tabl @7318)

instance OEIS 14551 where
  oeisIx n = (oeisIx @79) n + (oeisIx @33999) n
  oeis = map fst $ iterate (\ (x,s) -> (2 * x - 3 * s, -s)) (2, 1)

instance OEIS 14574 where
  oeis = [x | x <- [2,4..], (oeisIx @10051 . pred) (x - 1) == 1, (oeisIx @10051 . pred) (x+1) == 1]

instance OEIS 14631 where
  oeis = 1 : (nub $ concatMap tail (tabf @34868))

instance OEIS 14682 where
  oeisIx n = if r > 0 then div (3 * n + 1) 2 else n'
              where (n', r) = divMod n 2

instance OEIS 14683 where
  oeisIx (succ->n) = n + (oeisIx @10051 . pred) n

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

instance OEIS 16052 where
  oeis = iterate (oeisIx @62028) 3

instance OEIS 16067 where
  oeis = map (+ 1)
       . findIndices (> 1)
       . zipTail (-)
       . scanl max 0
       $ oeis @46920

instance OEIS 16069 where
  oeis = filter ((== 2) . length . nub . show . fi . (^ 2)) [0..]

instance OEIS 16096 where
  oeis = iterate (oeisIx @62028) 9

instance OEIS 16105 where
  oeis = f [3,7] (drop 2 (oeis @2145)) 21 (S.singleton 21) where
     f qs (p:p':ps) t s
       | m < t     = m : f qs (p:p':ps) t s'
       | otherwise = m : f (p:qs) (p':ps) t' (s' `S.union` (S.fromList pqs))
       where (m,s') = S.deleteFindMin s
             t' = head $ dropWhile (> 3*p') pqs
             pqs = map (p *) qs

instance OEIS 16189 where
  oeisIx n = 10 ^ n - 9 ^ n
  oeis = 0 : zipWith (+) (map (* 9) (oeis @16189)) (oeis @11557)

instance OEIS 16742 where
  oeisIx = (* 4) . (^ 2)
  oeis = 0 : map (subtract 4) (zipWith (+) (oeis @16742) [8, 16 ..])

instance OEIS 17665 where
  oeisIx = numerator . sum . map (1 %) . (rowT @27750).succ

instance OEIS 17666 where
  oeisIx = denominator . sum . map (1 %) . (rowT @27750).succ

instance OEIS 18252 where
  oeis = map (+1) $ filter ((== 0) . (oeisIx @10051)) [0..]

instance OEIS 18818 where
  oeisIx (succ->n) = p (init $ (rowT @27750) n) n + 1 where
     p _      0 = 1
     p []     _ = 0
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 18834 where
  oeis = map fi $ filter (\(x :: Int) -> show x `isInfixOf` show (x^2)) [0..]

instance OEIS 18900 where
  oeis = elemIndices 2 (oeis @73267)

instance OEIS 19312 where
  oeisIx = t . enumFromTo 1.succ where
     t xs = foldl max 0 [z + t (xs \\ ds) | z <- xs,
               let ds = (rowT @27750) z, not $ null $ intersect xs $ init ds]

instance OEIS 19446 where
  oeis = 1 : zipWith (-) [3..] (map (oeisIx @19446 . pred) (oeis @19446))

instance OEIS 19464 where
  oeis = 1 : concat (unfoldr ma (1, [1, 1])) where
     ma (x, [_, j]) = Just (ij', (x + 1, ij')) where ij' = [x * j, x * j + x]

instance OEIS 19538 where
  oeis = tablList @19538
instance Table 19538 where
  rowCol = rowCol_off @19538 @1 @1
  rowT   = rowT_off   @19538 @1
  tabl = iterate f [1] where
     f xs = zipWith (*) [1..] $ zipWith (+) ([0] ++ xs) (xs ++ [0])

instance OEIS 20474 where
  oeis = tablList @20474
instance Table 20474 where
  rowCol = rowCol_off @20474 @2 @2
  rowT = rowT_off @20474 @2
  tabl = map fst $ iterate f ([1], [0,1]) where
     f (us,vs) = (vs, scanl (+) 0 ws) where
       ws = zipWith (+) (us ++ [0]) vs

instance OEIS 20481 where
  oeisIx (succ.succ->n) = head [p | p <- (oeis @40)
                     , let q = 2 * n - p
                     , (oeisIx @10051 . pred) q == 1]

instance OEIS 20483 where
  oeisIx (n) = head [p | p <- (oeis @40), (oeisIx @10051 . pred) (p + 2 * n) == 1]

instance OEIS 20484 where
  oeisIx n = head [q | p <- (oeis @40), let q = p + 2*n, (oeisIx @10051 . pred) q == 1]

instance OEIS 20522 where
  oeisIx = (* 2) . (oeisIx @6516)

instance OEIS 20650 where
  oeis = map numerator ks where
     ks = 1 : concat (transpose [map (+ 1) ks, map (recip . (+ 1)) ks])

instance OEIS 20651 where
  oeis = map denominator ks where
     ks = 1 : concat (transpose [map (+ 1) ks, map (recip . (+ 1)) ks])

instance OEIS 20696 where
  oeisIx = product . map (+ 1) . (rowT @27750).succ

instance OEIS 20756 where
  oeis = filter ((> 0) . (oeisIx @52343)) [0..]

instance OEIS 20757 where
  oeis = elemIndices 0 (oeis @52343)

instance OEIS 20884 where
  oeis = f 1 1 where
     f u v | v > uu `div` 2        = f (u + 1) (u + 2)
           | gcd u v > 1 || w == 0 = f u (v + 2)
           | otherwise             = u : f u (v + 2)
           where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

instance OEIS 20899 where
  oeis = filter (odd . (oeisIx @7895)) [1..]

instance OEIS 22307 where
  oeisIx n = if n == 0 then 0 else (oeisIx @1221 . pred) $ (oeisIx @45) n

instance OEIS 22544 where
  oeis = elemIndices 0 (oeis @161)

instance OEIS 22559 where
  oeis = scanl (+) 0 $ map (oeisIx @1222 . pred) [1..]

instance OEIS 22831 where
  oeis = 2 : f 2 (tail (oeis @40)) where
     f x (p:ps) | x' > 0    = x' : f x' ps
                | otherwise = xp : f xp ps where x' = x - p; xp = x + p

instance OEIS 22941 where
  oeis = 1 : 2 : f 2 [3..] where
     f x (z:zs) = y : f y (delete y zs) where y = x + z

instance OEIS 23143 where
  oeis = 1 : map (+ 1) (elemIndices 1 (oeis @4648))

instance OEIS 23172 where
  oeis = map (+ 1) $ elemIndices 0 $ zipWith mod (tail (oeis @45)) [1..]

instance OEIS 23200 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $
                 map (subtract 4) $ drop 2 (oeis @40)

instance OEIS 23208 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (+ 2) . (* 3)) (oeis @40)

instance OEIS 23523 where
  oeis =  map (+ 1) $ zipWith (*) (oeis @40) (oeis @8578)

instance OEIS 23532 where
  oeisIx = (1 -) . (oeisIx @10052) . (+ 9) . (* 8)
  oeis = concat $ iterate (\rs -> 1 : rs) [0]

instance OEIS 23607 where
  oeis = zipWith (*) [0..] $ tail (oeis @45)

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

instance OEIS 23758 where
  oeis = 0 : f (S.singleton 1) where
    f s = x : f (if even x then S.insert z s' else S.insert z $ S.insert (z+1) s')
     where z = 2*x; (x, s') = S.deleteFindMin s

instance OEIS 23890 where
  oeisIx (succ->n) = sum $ zipWith (*) divs $ map ((1 -) . (oeisIx @10051 . pred)) divs
              where divs = (rowT @27750) n

instance OEIS 24014 where
  -- oeis = oeis24014
  oeisIx n = 2^n-n^4

(oeis5211, oeis24014) = unzip $ (1, 1) :  f 1 1
  where
    f i x | y > x     = (y, i) : f (i + 1) y
          | otherwise = f (i + 1) x
          where y = (oeisIx @5210) i

instance OEIS 24206 where
  oeisIx (succ->n) = (n - 1) * (n + 3) `div` 4
  oeis = scanl (+) 0 $ tail (oeis @8619)

instance OEIS 24362 where
  oeisIx (succ->n) = sum [oeisIx @10052 y | x <- takeWhile (< nn) $ tail (oeis @290),
                               let y = nn - x, y <= x, gcd x y == 1]
              where nn = n ^ 2

instance OEIS 25475 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @961)

instance OEIS 25478 where
  oeis = oeis25478
instance OEIS 25479 where
  oeis = oeis25479

instance OEIS 25487 where
  oeis = 1 : h [b] (S.singleton b) bs where
     (_ : b : bs) = (oeis @2110)
     h cs s xs'@ (x:xs)
       | m <= x    = m : h (m:cs) (s' `S.union` S.fromList (map (* m) cs)) xs'
       | otherwise = x : h (x:cs) (s  `S.union` S.fromList (map (* x) (x:cs))) xs
       where (m, s') = S.deleteFindMin s

instance OEIS 25547 where
  oeis = scanl1 lcm (oeis @5408)

instance OEIS 25581 where
  oeis = tablList @25581
instance Table 25581 where
  rowCol n k = n - k
  rowT n = [n, n - 1 .. 0]
  tabl = iterate (\xs@ (x:_) -> (x + 1) : xs) [0]

instance OEIS 25586 where
  oeisIx = last . (rowT @220237) . succ

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

instance OEIS 26430 where
  oeis = scanl (+) 0 (oeis @1285)

instance OEIS 26532 where
  oeis = scanl (*) 1 $ (oeis @176059)

instance OEIS 26549 where
  oeis = scanl (*) 1 $ (oeis @10693)

instance OEIS 26637 where
  oeis = tablList @26637
instance Table 26637 where
  tabl = [1] : [1,1] : map (fst . snd)
     (iterate f (0, ([1,2,1], [0,1,1,0]))) where
     f (i, (xs, ws)) = (1 - i,
       if i == 1 then (ys, ws) else (zipWith (+) ys ws, ws'))
          where ys = zipWith (+) ([0] ++ xs) (xs ++ [0])
                ws' = [0,1,0,0] ++ drop 2 ws

instance OEIS 26807 where
  oeis = tablList @26807
instance Table 26807 where
  rowCol = rowCol_off @26807 @1 @1
  rowT   = rowT_off   @26807 @1
  tabl = map
     (\row -> map (p $ last row) $ init $ tails row) (tabl @2260)
     where p 0  _ = 1
           p _ [] = 0
           p m ks'@ (k:ks) = if m < k then 0 else p (m - k) ks' + p m ks

instance OEIS 26820 where
  oeis = tablList @26820
instance Table 26820 where
  rowCol = rowCol_off @26820 @1 @1
  rowT   = rowT_off   @26820 @1
  tabl = zipWith
     (\x -> map (p x) . tail . inits) [1..] $ tail $ inits [1..] where
     p 0 _ = 1
     p _ [] = 0
     p m ks'@ (k:ks) = if m < k then 0 else p (m - k) ks' + p m ks

instance OEIS 26832 where
  oeisIx 0 = 0
  oeisIx n = p 1 n where
     p _ 0 = 1
     p k m = if m < k then 0 else p (k+1) (m-k) + p (k+1+0^ (n-m)) m

instance OEIS 26835 where
  oeis = tablList @26835
instance Table 26835 where
  rowCol = rowCol_off @26835 @1 @1
  rowT   = rowT_off   @26835 @1
  tabl = map
     (\row -> map (p $ last row) $ init $ tails row) (tabl @2260)
     where p 0      _ = 1
           p _     [] = 0
           p m (k:ks) = if m < k then 0 else p (m - k) ks + p m ks

instance OEIS 27023 where
  oeis = tablList @27023
instance Table 27023 where
  rowCol = rowCol_off @27023 @1 @1
  rowT   = rowT_off @27023 @1
  tabf = [1] : iterate f [1, 1, 1] where
     f row = 1 : 1 : 1 :
             zipWith3 (((+) .) . (+)) (drop 2 row) (tail row) row ++ [1]

instance OEIS 27420 where
  oeis = tablList @27420
instance Table 27420 where
  tabl = zipWith (zipWith z) (tabl @2262) (tabl @25581)
                 where z u v = length $ nub $ [i * j | i <- zs, j <- zs]
                               where zs = [min u v .. max u v]

instance OEIS 27423 where
  oeisIx n = f 1 $ map (\p -> iterate (* p) p) (oeis @40) where
     f y ((pps@ (p:_)):ppss)
       | p <= n = f (y * (sum (map (div n) $ takeWhile (<= n) pps) + 1)) ppss
       | otherwise = y

instance OEIS 27434 where
  oeisIx = (+ 1) . (oeisIx @196) . (subtract 3) . (* 4) . succ
  oeis = 2 : concat (map (\x -> replicate (x `div` 2) x) [3..])

instance OEIS 27465 where
  oeis = tablList @27465
instance Table 27465 where
  tabl = iterate (\row ->
     zipWith (+) (map (* 3) (row ++ [0])) (map (* 1) ([0] ++ row))) [1]

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

instance OEIS 27749 where
  oeis = tablList @27749
instance Table 27749 where
  rowCol = rowCol_off @27749 @1 @1
  tabf = [1] : map tail (tail (tabf @27750))

instance OEIS 27751 where
  oeis = tablList @27751
instance Table 27751 where
  rowCol = rowCol_off @27751 @1 @1
  rowT   = rowT_off @27751 @1
  tabf   = [1] : map init (tail (tabf @27750))

instance OEIS 27861 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @1844)) [0..]

instance OEIS 27870 where
  oeisIx = oeisIx @55641 . oeisIx @79

instance OEIS 27907 where
  oeis = tablList @27907
instance Table 27907 where
  tabf = [1] : iterate f [1, 1, 1] where
     f row = zipWith3 (((+) .) . (+))
                      (row ++ [0, 0]) ([0] ++ row ++ [0]) ([0, 0] ++ row)

instance OEIS 27926 where
  oeis = tablList @27926
instance Table 27926 where
  tabf = iterate (\xs -> zipWith (+) ([0] ++ xs ++ [0]) ([1,0] ++ xs)) [1]
  -- oeisIx_tabf' = zipWith (++) (tabl @104763) (map tail (tabl @105809))

instance OEIS 27934 where
  oeis = 0 : 1 : 2 : zipWith3 (\x y z -> 3 * x - y - 2 * z)
                 (drop 2 (oeis @27934)) (tail (oeis @27934)) (oeis @27934)

instance OEIS 27941 where
  oeisIx = (subtract 1) . (oeisIx @45) . (+ 1) . (* 2)

instance OEIS 27990 where
  oeis = c [1] (oeis @45) where
     c us (v:vs) = (sum $ zipWith (*) us vs) : c (v:us) vs

instance OEIS 28233 where
  oeisIx = head . (rowT @141809) . succ

instance OEIS 28234 where
  oeisIx (succ->n) = n `div` (oeisIx @28233.pred) n

instance OEIS 28236 where
  oeisIx (succ->n) = sum $ map (div n) $ (rowT @141809) n

instance OEIS 28260 where
  oeis = filter (even . (oeisIx @1222) . pred) [1..]

instance OEIS 28262 where
  oeis = tablList @28262
instance Table 28262 where
  tabl = [1] : [1,1] : iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,3,1]

instance OEIS 28263 where
  oeis = tablList @28263
instance Table 28263 where
  tabl = zipWith (zipWith (+)) (tabl @7318) (tabl @14410)

instance OEIS 28290 where
  oeis = map (p' 0) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p 5 _ = 0
     p k m | m < parts !! k = 0
           | otherwise = p' k (m - parts !! k) + p' (k + 1) m
     parts = [1, 2, 3, 5, 8]

instance OEIS 28326 where
  oeis = tablList @28326
instance Table 28326 where
  tabl = iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [2]

instance OEIS 28391 where
  oeisIx n = n - (oeisIx @196) n

instance OEIS 28392 where
  oeisIx n = n + (oeisIx @196) n

instance OEIS 28393 where
  oeis = iterate (oeisIx @6368) 8

instance OEIS 28394 where
  oeis = iterate (oeisIx @6369) 8

instance OEIS 28395 where
  oeis = iterate (oeisIx @6368) 14

instance OEIS 28396 where
  oeis = iterate (oeisIx @6369) 14

instance OEIS 28422 where
  oeisIx 0 = 0
  oeisIx (succ->n) = (map (last . init) (tabl @66032)) !! (n - 1)

instance OEIS 28834 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @7953)) [1..]

instance OEIS 28835 where
  oeis = findIndices (`elem` [2,3,5,7]) $ map (oeisIx @10888) [0..]

instance OEIS 28846 where
  oeis = f [1] where
     f ds = foldr (\d v -> 10 * v + d) 0 ds : f (s ds)
     s [] = [1]; s (8:ds) = 1 : s ds; s (d:ds) = 2*d : ds

instance OEIS 28859 where
  oeis = 1 : 3 : map (* 2) (zipWith (+) (oeis @28859) (tail (oeis @28859)))

instance OEIS 28860 where
  oeis = -1 : 1 : map (* 2) (zipWith (+) (oeis @28860) (tail (oeis @28860)))

instance OEIS 28871 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) $ tail (oeis @8865)

instance OEIS 28884 where
  oeisIx n = (n + 3)^2 - 8

instance OEIS 28897 where
  oeisIx 0 = 0
  oeisIx n = 2 * (oeisIx @28897) n' + d where (n', d) = divMod n 10

instance OEIS 28906 where
  oeisIx = (oeisIx @4186) . (oeisIx @40)

instance OEIS 28982 where
  oeis = tail $ O.union (oeis @290) (oeis @1105)

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

instance OEIS 29578 where
  oeisIx n =  (n - n `mod` 2) `div` (2 - n `mod` 2)
  oeis = concat $ transpose [oeis @1477, (oeis @5843)]

instance OEIS 29579 where
  oeisIx n = if m == 0 then n' + 1 else n  where (n', m) = divMod n 2
  oeis = concat $ transpose [[1 ..], [1, 3 ..]]

instance OEIS 29600 where
  oeis = tablList @29600
instance Table 29600 where
  tabl = [1] : iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [2,3]

instance OEIS 29609 where
  oeisIx n = (rowCol @29600) (2*n) n

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

instance OEIS 29742 where
  oeis = filter ((== 0) . (oeisIx @136522)) [1..]

instance OEIS 29743 where
  oeis = filter ((== 1) . (oeisIx @10051).pred) (oeis @10784)

instance OEIS 29744 where
  oeis = 1 : iterate
     (\x -> if x `mod` 3 == 0 then 4 * x `div` 3 else 3 * x `div` 2) 2

instance OEIS 29793 where
  oeis = filter (\x -> digs x == digs (x^2)) [0..]
     where digs = sort . nub . show . fi

instance OEIS 29837 where
  oeis = scanl1 (+) (oeis @209229)

instance OEIS 29854 where
  oeis = zipWith gcd (oeis @1043) $ tail (oeis @1043)

instance OEIS 29858 where
  oeisIx = (`div` 2) . (subtract 3) . (3 ^) . succ
  oeis = iterate ((+ 3) . (* 3)) 0

instance OEIS 29931 where
  oeisIx = sum . zipWith (*) [1..] . (rowT @30308)

instance OEIS 29942 where
  oeis = [fi x | (x :: Int) <- [0..], show x `isInfixOf` show (x^3)]

instance OEIS 29943 where
  oeis = filter f [0..] where
     f (fi->x) = show x `isInfixOf` show (x^2) && show x `isInfixOf` show (x^3)

instance OEIS 30067 where
  oeis = concat $ transpose [scanl (+) 1 (oeis @30067), (oeis @30067)]

instance OEIS 30078 where
  oeisIx = (oeisIx @578) . (oeisIx @40)
  oeis = map (oeisIx @578) (oeis @40)

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

instance OEIS 30173 where
  oeis = O.union [2, 4 ..] $ tail (oeis @40976)

instance OEIS 30186 where
  oeis = 1 : 2 : 7 : zipWith (-) (tail $
     zipWith (+) (oeis @30186) $ tail $ map (* 3) (oeis @30186)) (oeis @30186)

instance OEIS 30190 where
  oeis = concatMap reverse (tabf @30308)

instance OEIS 30195 where
  oeis = 0 : 1 : map (* 3) (zipWith (+) (oeis @30195) (tail (oeis @30195)))

instance OEIS 30230 where
  oeis = filter (odd . (oeisIx @1221) . pred) [1..]

instance OEIS 30231 where
  oeis = filter (even . (oeisIx @1221) . pred) [1..]

instance OEIS 30237 where
  oeis = tablList @30237
instance Table 30237 where
  tabl = map init $ tail (tabl @9766)

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

instance OEIS 30298 where
  oeis = tablList @30298
instance Table 30298 where
  rowCol = rowCol_off @30298 @1 @1
  rowT = concat . sort . permutations . enumFromTo 1
  tabf = map (rowT @30298) [1..]

instance OEIS 30303 where
  oeis = elemIndices 1 (oeis @30190)

instance OEIS 30339 where
  oeis = scanl1
     (\u v -> u + (fi.fromEnum) (v == 1) - (fi.fromEnum) (v == 0)) (oeis @3137)

instance OEIS 30340 where
  oeis = scanl1
     (\u v -> u + (fi.fromEnum) (v == 1) - (fi.fromEnum) (v == 2)) (oeis @3137)

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

instance OEIS 30430 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @17281)

instance OEIS 30457 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @1704 .pred)) [1..]

instance OEIS 30514 where
  oeisIx = (^ 4) . (oeisIx @40)
  oeis = map (^ 4) (oeis @40)

instance OEIS 30516 where
  oeisIx = (^ 6) . (oeisIx @40)
  oeis = map (^ 6) (oeis @40)

instance OEIS 30530 where
  oeis = 0 : concatMap (\n -> unfoldr
     (\x -> if x == 0 then Nothing else Just (n, div x 2)) n) [1..]

instance OEIS 30717 where
  oeis = tablList @30717
instance Table 30717 where
  rowCol = rowCol_off @30717 @1 @1
  rowT   = rowT_off @30717 @1
  tabf = [1] : f [1] where
     f xs = ys : f ((filter (> 0) ys) ++ xs) where
            ys = h (group $ sort xs) [1..] where
                 h [] _ = []
                 h vss'@ (vs:vss) (w:ws)
                   | head vs == w = (length vs) : h vss ws
                   | otherwise    = 0 : h vss' ws

instance OEIS 30719 where
  oeis = tail $ zipWith (-) (tail (oeis @253170)) (oeis @253170)

instance OEIS 31087 where
  oeis = tablList @31087
instance Table 31087 where
  rowCol n k = (rowT @31087) n !! (k - 1)
  rowT n | n < 9     = [n]
                | otherwise = m : (rowT @31087) n' where (n',m) = divMod n 9
  tabf = map (rowT @31087) [0..]

instance OEIS 31131 where
  oeis = zipWith (-) (drop 2 (oeis @40)) (oeis @40)

instance OEIS 31165 where
  oeis = zipWith (-) (drop 3 (oeis @40)) (oeis @40)

instance OEIS 31167 where
  oeis = zipWith (-) (drop 5 (oeis @40)) (oeis @40)

instance OEIS 31168 where
  oeis = zipWith (-) (drop 6 (oeis @40)) (oeis @40)

instance OEIS 31169 where
  oeis = zipWith (-) (drop 7 (oeis @40)) (oeis @40)

instance OEIS 31170 where
  oeis = zipWith (-) (drop 8 (oeis @40)) (oeis @40)

instance OEIS 31171 where
  oeis = zipWith (-) (drop 9 (oeis @40)) (oeis @40)

instance OEIS 31177 where
  oeis = filter ((/= 1) . (oeisIx @103369) . pred) [1..]

instance OEIS 31218 where
  oeisIx (succ->n) = last $ takeWhile (<= n) (oeis @961)

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

instance OEIS 31368 where
  oeisIx = (oeisIx @40) . (* 2)
  oeis = map (oeisIx @40) [0, 2 ..]

instance OEIS 31883 where
  oeis = zipWith (-) (tail (oeis @959)) (oeis @959)

instance OEIS 31944 where
  oeis = elemIndices 3 (oeis @212193)

instance OEIS 31955 where
  oeis = map succ $ elemIndices 2 (oeis @43537)

instance OEIS 32031 where
  oeis = scanl (*) 1 $ tail (oeis @8585)

instance OEIS 32352 where
  oeis = filter
     (\x -> all (== 0) $ map (oeisIx @10051 . pred . (10*x +)) [1..9]) [1..]

instance OEIS 32527 where
  oeis = scanl (+) 0 (oeis @47209)

instance OEIS 32528 where
  oeis = scanl (+) 0 (oeis @7310)

instance OEIS 32740 where
  oeis = [fi x | (x :: Int) <- [0..], show x `isInfixOf` (show $ 2 ^ x)]

instance OEIS 32742 where
  oeisIx n = succ n `div` (oeisIx @20639) n

instance OEIS 32759 where
  oeis = map fi (2 : map read (zipWith (++) vs (tail us)) :: [Integer])
     where (us,vs) = unzip $ map ((splitAt 1) . show) (oeis @40)

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

instance OEIS 33184 where
  oeis = tablList @33184
instance Table 33184 where
  rowCol = rowCol_off @33184 @1 @1
  rowT   = rowT_off   @33184 @1
  tabl = map reverse (tabl @9766)

instance OEIS 33200 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @47471)

instance OEIS 33203 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) $ tail (oeis @47476)

instance OEIS 33270 where
  oeis = 0 : 0 : scanl1 (+) (drop 2 (oeis @10051))

instance OEIS 33273 where
  oeisIx = genericLength . filter ((== 0) . (oeisIx @10051) . pred) . (rowT @27750) . succ

instance OEIS 33286 where
  oeisIx (succ->n) = (oeisIx @40 . pred) n * n

instance OEIS 33291 where
  oeis = tablList @33291
instance Table 33291 where
  rowCol = rowCol_off @33291 @1 @1
  rowT   = rowT_off   @33291 @1
  tabl = f 1 [1..] where
     f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
       ys  = take k $ filter ((== 0) . (`mod` k)) xs

instance OEIS 33292 where
  oeis = tablList @33292
instance Table 33292 where
  rowCol = rowCol_off @33292 @1 @1
  rowT   = rowT_off   @33292 @1
  tabl = f 1 [1..] where
     f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
       ys  = take k $ filter ((== 0) . (`mod` 3) . (subtract k)) xs

instance OEIS 33293 where
  oeis = tablList @33293
instance Table 33293 where
  rowCol = rowCol_off @33293 @1 @1
  rowT   = rowT_off   @33293 @1
  tabl = f 1 [1..] where
     f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
       ys  = take k $ filter ((== 0) . (`mod` 8) . (subtract k)) xs

instance OEIS 33484 where
  oeisIx = (subtract 2) . (* 3) . (2 ^)
  oeis = iterate ((subtract 2) . (* 2) . (+ 2)) 1

instance OEIS 33493 where
  oeisIx = sum . (rowT @70165) . succ

instance OEIS 33496 where
  oeis = 1 : filter f [2, 4 ..] where
     f x = x == maximum (takeWhile (/= 1) $ iterate (oeisIx @6370) x)

instance OEIS 33619 where
  oeis = [0..9] ++ (f $ S.fromList [10..99]) where
     f s = m : f (S.insert (m * 10 + h) s') where
       h = div (mod m 100) 10
       (m,s') = S.deleteFindMin s

instance OEIS 33627 where
  oeis = f [1..] [] where
     f (x:xs) ys = x : f (xs \\ (map (+ x) ys)) (x:ys)

instance OEIS 33648 where
  oeis = iterate (oeisIx @56964) 3

instance OEIS 33649 where
  oeis = iterate (oeisIx @56964) 5

instance OEIS 33650 where
  oeis = iterate (oeisIx @56964) 7

instance OEIS 33676 where
  oeisIx (succ->n) = last $ takeWhile (<= (oeisIx @196) n) $ (rowT @27750) n

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

instance OEIS 34387 where
  oeis = scanl1 (+) (oeis @61397)

instance OEIS 34693 where
  oeisIx (succ->n) = head [k | k <- [1..], (oeisIx @10051 . pred) (k * n + 1) == 1]

instance OEIS 34694 where
  oeisIx (succ->n) = until ((== 1) . (oeisIx @10051.pred)) (+ n) (n + 1)

instance OEIS 34698 where
  oeis = f [2..] [1] where
     f (x:xs) ys | (oeisIx @10051.pred) x == 1 &&
                   (and $ map (isSquMod x) ys) = x : f xs (x:ys)
                 | otherwise                   = f xs ys
     isSquMod u v = v `mod` u `elem` (map ((`mod` u) . (^ 2)) [0..u - 1])

instance OEIS 34700 where
  oeis = f [1,5..] [1] where
     f (x:xs) ys | (oeisIx @10051 . pred) x == 1 &&
                   (and $ map (isSquMod x) ys) = x : f xs (x:ys)
                 | otherwise                   = f xs ys
     isSquMod u v = v `mod` u `elem` (map ((`mod` u) . (^ 2)) [0..u - 1])

instance OEIS 34705 where
  oeis = f 0 (tail $ inits $ (oeis @290)) (S.fromList [0]) where
     f x vss'@ (vs:vss) s
       | y < x = y : f x vss' s'
       | otherwise = f w vss (S.union s $ S.fromList $ scanl1 (+) ws)
       where ws@ (w:_) = reverse vs
             (y, s') = S.deleteFindMin s

instance OEIS 34706 where
  oeis = f 0 (tail $ inits $ (oeis @217)) (S.fromList [0]) where
     f x vss'@ (vs:vss) s
       | y < x = y : f x vss' s'
       | otherwise = f w vss (S.union s $ S.fromList $ scanl1 (+) ws)
       where ws@ (w:_) = reverse vs
             (y, s') = S.deleteFindMin s

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

instance OEIS 34851 where
  oeis = tablList @34851
instance Table 34851 where
  rowT 0 = [1]
  rowT 1 = [1,1]
  rowT n = zipWith (-) (zipWith (+) ([0] ++ losa) (losa ++ [0]))
                              ([0] ++ (rowT @204293) (n - 2) ++ [0])
     where losa = (rowT @34851) (n - 1)
  tabl = map (rowT @34851) [0..]

instance OEIS 34852 where
  oeis = tablList @34852
instance Table 34852 where
  tabl = zipWith (zipWith (-)) (tabl @7318) (tabl @34851)

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

instance OEIS 34870 where
  oeis = tablList @34870
instance Table 34870 where
  tabf = map (rowT @7318) [0, 2 ..]

instance OEIS 34877 where
  oeis = tablList @34877
instance Table 34877 where
  tabl = map (init . tail) $ drop 2 (tabl @34852)

instance OEIS 34879 where
  oeis = iterate (oeisIx @66459) 3

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

instance OEIS 35026 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051 .pred. (2 * n -)) $
     takeWhile (< 2 * n) (oeis @40)

instance OEIS 35106 where
  oeis = 1 : tail (O.union (oeis @2378) (oeis @5563))

instance OEIS 35137 where
  oeis = elemIndices 0 (oeis @260254)

instance OEIS 35166 where
  oeis = map (+ 1) $ findIndices (/= 0) $ zipWith (-) (tail gs) gs
     where gs = 0 : map (oeisIx @7913 . pred) (oeis @7407)

instance OEIS 35250 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051 .pred) [n..2*n]

instance OEIS 35263 where
  oeis = map fi $ zipWith fixor (oeis @10060) $ tail (oeis @10060)
    where fixor a b = xor (fi a :: Int) (fi b :: Int)

instance OEIS 35294 where
  oeis = f 1 where
     f x = (p' 1 (x - 1)) : f (x + 2)
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 2) m

instance OEIS 35306 where
  oeis = tablList @35306
instance Table 35306 where
  rowCol n k = (rowT @35306) n !! (k - 1)
  rowT 1 = [1,1]
  rowT n = concat $ transpose [rowT @27748 n, (rowT @124010) n]
  tabf = map (rowT @35306) [1..]

instance OEIS 35317 where
  oeis = tablList @35317
instance Table 35317 where
  tabl = map snd $ iterate f (0, [1]) where
     f (i, row) = (1 - i, zipWith (+) ([0] ++ row) (row ++ [i]))

instance OEIS 35324 where
  oeis = tablList @35324
instance Table 35324 where
  rowCol = rowCol_off @35324 @1 @1
  rowT   = rowT_off   @35324 @1
  tabl = map snd $ iterate f (1, [1]) where
     f (i, xs)  = (i + 1, map (`div` (i + 1)) $
        zipWith (+) ((map (* 2) $ zipWith (*) [2 * i + 1 ..] xs) ++ [0])
                    ([0] ++ zipWith (*) [2 ..] xs))

instance OEIS 35336 where
  oeis = elemIndices 0 (oeis @5713)

instance OEIS 35342 where
  oeis = tablList @35342
instance Table 35342 where
  rowCol = rowCol_off @35342 @1 @1
  rowT   = rowT_off   @35342 @1
  tabl = map fst $ iterate (\ (xs, i) -> (zipWith (+)
     ([0] ++ xs) $ zipWith (*) [i..] (xs ++ [0]), i + 2)) ([1], 3)

instance OEIS 35517 where
  oeis = tablList @35517
instance Table 35517 where
  tabf = map reverse (tabf @35516)

instance OEIS 35522 where
  oeis = iterate (oeisIx @55944) 1

instance OEIS 35524 where
  oeis = iterate (oeisIx @55948) 1

instance OEIS 35532 where
  oeisIx 0 = 1
  oeisIx (succ->n)
    = if (oeisIx @10051.pred) n == 0
         then phi2
         else phi2 - (oeisIx @120) n + 1
    where phi2 = 2 * (oeisIx @10 . pred) n

instance OEIS 35607 where
  oeis = tablList @35607
instance Table 35607 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 2])

instance OEIS 36234 where
  oeisIx = (+ 1) . (oeisIx @720)

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

instance OEIS 36441 where
  oeis = tail (oeis @76271)

instance OEIS 36447 where
  oeis = iterate (oeisIx @4093) 1

instance OEIS 36467 where
  oeis = 1 : zipWith (-) (oeis @40) (oeis @36467)

instance OEIS 36490 where
  oeis = f $ S.fromList [5,7,11] where
     f s = m : (f $ S.insert (5 * m) $ S.insert (7 * m) $ S.insert (11 * m) s')
           where (m, s') = S.deleteFindMin s

instance OEIS 36552 where
  oeis = g [1..] where
     g (x:xs) = x : (2*x) : (g $ delete (2*x) xs)

instance OEIS 36561 where
  oeis = tablList @36561
instance Table 36561 where
  tabf = iterate (\xs@ (x:_) -> x * 2 : map (* 3) xs) [1]

instance OEIS 36581 where
  oeis = zipWith (\u v -> if u /= v then 2 * u + v - 1 else 2)
                         (oeis @10060) $ tail (oeis @10060)

instance OEIS 36585 where
  oeis = 3 : concat (map f (oeis @36585))
    where f 1 = [1,2,3]; f 2 = [1,3]; f 3 = [2]

instance OEIS 36689 where
  oeis = zipWith (*) (oeis @40) $ map pred (oeis @40)

instance OEIS 36691 where
  oeis = 1 : scanl1 (*) (oeis @2808)

instance OEIS 36839 where
  oeisIx = (oeisIx @4185) . (oeisIx @56964)

instance OEIS 36966 where
  oeis = 1 : f (S.singleton z) [1, z] zs where
     f s q3s p3s'@ (p3:p3s)
       | m < p3 = m : f (S.union (S.fromList $ map (* m) ps) s') q3s p3s'
       | otherwise = f (S.union (S.fromList $ map (* p3) q3s) s) (p3:q3s) p3s
       where ps = (rowT @27748) m
             (m, s') = S.deleteFindMin s
     (z:zs) = (oeis @30078)

instance OEIS 36967 where
  oeis = 1 : f (S.singleton z) [1, z] zs where
     f s q4s p4s'@ (p4:p4s)
       | m < p4 = m : f (S.union (S.fromList $ map (* m) ps) s') q4s p4s'
       | otherwise = f (S.union (S.fromList $ map (* p4) q4s) s) (p4:q4s) p4s
       where ps = (rowT @27748) m
             (m, s') = S.deleteFindMin s
     (z:zs) = (oeis @30514)

instance OEIS 36969 where
  oeis = tablList @36969
instance Table 36969 where
  rowCol = rowCol_off @36969 @1 @1
  rowT   = rowT_off   @36969 @1
  tabl = iterate f [1] where
     f row = zipWith (+)
       ([0] ++ row) (zipWith (*) (tail (oeis @290)) (row ++ [0]))

instance OEIS 37020 where
  oeis = map succ $ filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @1065)) [1..]

instance OEIS 37027 where
  oeis = tablList @37027
instance Table 37027 where
  tabl = [1] : [1,1] : f [1] [1,1] where
     f xs ys = ys' : f ys ys' where
       ys' = zipWith3 (\u v w -> u + v + w) (ys ++ [0]) (xs ++ [0,0]) ([0] ++ ys)

instance OEIS 37074 where
  oeisIx = pred . (oeisIx @75369)

instance OEIS 37126 where
  oeis = tablList @37126
instance Table 37126 where
  rowCol = rowCol_off @37126 @1 @1
  rowT   = rowT_off   @37126 @1
  tabl = map (`take` (oeis @40)) [1..]

instance OEIS 37143 where
  oeis = 1 : merge (oeis @40) (oeis @1358) where
     merge xs'@ (x:xs) ys'@ (y:ys) =
           if x < y then x : merge xs ys' else y : merge xs' ys

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

instance OEIS 37166 where
  oeis = zipWith (*) (oeis @40) $
                             map (subtract 1) $ tail (oeis @40)

instance OEIS 37213 where
  oeisIx n = if n == r ^ 2 then r else 0  where r = (oeisIx @196) n
  oeis = zipWith (*) (oeis @10052) (oeis @196)

instance OEIS 37227 where
  oeisIx = (+ 1) . (* 2) . (oeisIx @7814)

instance OEIS 37254 where
  oeis = tablList @37254
instance Table 37254 where
  rowCol = rowCol_off @37254 @1 @1
  rowT   = rowT_off   @37254 @1
  tabl = map fst $ iterate f ([1], drop 2 (oeis @2083)) where
     f (row, (x:xs)) = (map (+ x) (0 : row), xs)

instance OEIS 37306 where
  oeis = tablList @37306
instance Table 37306 where
  rowCol n k = div (sum $ map f $ (rowT @27750) $ gcd n k) n where
     f d = (oeisIx @10) d * (rowCol @7318) (div n d) (div k d)
  rowT n = map (rowCol @37306 n) [1..n]
  tabl = map (rowT @37306) [1..]

instance OEIS 37904 where
  oeisIx = f 9 0 . succ where
     f u v 0 = v - u
     f u v z = f (min u d) (max v d) z' where (z', d) = divMod z 10

instance OEIS 37952 where
  oeis = zipWith (-) (tail (oeis @1405)) (oeis @1405)

instance OEIS 38040 where
  oeisIx (succ->n) = (oeisIx @5 . pred) n * n

instance OEIS 38044 where
  oeis = 1 : f 1 [1] where
     f x ys = y : f (x + 1) (y:ys) where
       y = sum $ zipWith ((*) `on` (oeisIx @38044.pred)) divs $ reverse divs
           where divs = (rowT @27750) x

instance OEIS 38107 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @720) . pred $ (oeisIx @290) n

instance OEIS 38135 where
  oeis = 0 : 1 : 1 : 1 : f 1 1 1 4 where
     f u v w x = y : f v w y (x + 1) where
       y = q (x - u) + q (x - v) + q (x - w)
       q z = if abs z >= x then 0 else (oeisIx @38135) $ abs z

instance OEIS 38137 where
  oeis = tablList @38137
instance Table 38137 where
  tabl = map reverse (tabl @37027)

instance OEIS 38194 where
  oeisIx = flip mod 9 . (oeisIx @40)

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

instance OEIS 38255 where
  oeis = tablList @38255
instance Table 38255 where
  tabl = map reverse (tabl @13613)

instance OEIS 38365 where
  oeis = map fi $ filter (\(x :: Int) -> null (show (2*x) `intersect` show x)) [1..]

instance OEIS 38502 where
  oeisIx (succ->n) = if m > 0 then n else (oeisIx @38502 . pred) n'  where (n', m) = divMod n 3

instance OEIS 38505 where
  oeis = zipWith (-) (tail (oeis @749)) (oeis @749)

instance OEIS 38547 where
  oeisIx (succ->n) = fromJust $ find ((== n) . fi . A.sigma 0 . fi) [1,3..]

instance OEIS 38548 where
  oeisIx (succ->n) = genericLength $ takeWhile (<= (oeisIx @196) n) $ (rowT @27750) n

instance OEIS 38566 where
  oeis = tablList @38566
instance Table 38566 where
  rowCol = rowCol_off @38566 @1 @1
  rowT   = rowT_off @38566 @1
  tabf   = zipWith (\v ws -> filter ((== 1) . (gcd v)) ws) [1..] (tabl @2260)

instance OEIS 38608 where
  oeisIx n = n * (-1) ^ n
  oeis = [0, -1] ++ map negate
     (zipWith (+) (oeis @38608) (map (* 2) $ tail (oeis @38608)))

instance OEIS 38622 where
  oeis = tablList @38622
instance Table 38622 where
  tabl = iterate (\row -> map sum $
     transpose [tail row ++ [0,0], row ++ [0], [head row] ++ row]) [1]

instance OEIS 38699 where
  oeisIx 0 = 3
  oeisIx n = until ((== 1) . (oeisIx @10051 . pred)) ((+ 1) . (* 2)) $ n

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

instance OEIS 39634 where
  oeisIx 0 = 1
  oeisIx (succ->n) = until ((== 1) . (oeisIx @10051.pred)) (flip div 2) n

instance OEIS 39635 where
  oeisIx 0 = 1
  oeisIx (succ->n) = until ((== 1) . (oeisIx @10051.pred)) ((flip div 2) . (+ 1)) n

instance OEIS 39636 where
  oeisIx 0 = 1
  oeisIx (succ->n) = snd $ until ((== 1) . (oeisIx @10051.pred) . fst)
                          (\ (x, i) -> (x `div` 2 , i + 1)) (n, 1)

instance OEIS 39637 where
  oeisIx 0 = 1
  oeisIx (succ->n) = snd $ until ((== 1) . (oeisIx @10051.pred) . fst)
                          (\ (x, i) -> ((x + 1) `div` 2 , i + 1)) (n, 1)

instance OEIS 39639 where
  oeisIx = until ((== 1) . (oeisIx @10051.pred)) (flip div 2) . (+ 1) . (oeisIx @40)

instance OEIS 39641 where
  oeisIx = until ((== 1) . (oeisIx @10051.pred)) (flip div 2 . (+ 1)) . (+ 1) . (oeisIx @40)

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

instance OEIS 39702 where
  oeisIx = (`mod` 4) . (oeisIx @40)

instance OEIS 39834 where
  oeis = 1 : 1 : zipWith (-) (oeis @39834) (tail (oeis @39834))

instance OEIS 39913 where
  oeis = tablList @39913
instance Table 39913 where
  tabl = [[0], [1, 1]] ++ f [0] [1, 1] where
     f us@ (u:us') vs@ (v:vs') = ws : f vs ws where
       ws = [u + v, u + v + v] ++ zipWith (+) us vs'

instance OEIS 39941 where
  oeis = 0 : 1 : zipWith3 ($)
     (cycle [ (+), (*)]) (oeis @39941) (tail (oeis @39941))

instance OEIS 39955 where
  oeis = filter ((== 1) . (`mod` 4)) (oeis @5117)

instance OEIS 39956 where
  oeis = filter even (oeis @5117)

instance OEIS 39957 where
  oeis = filter ((== 3) . (`mod` 4)) (oeis @5117)

instance OEIS 39966 where
  oeisIx n = fi $ fromEnum (n < 2 || m < 2 && (oeisIx @39966) n' == 1)
     where (n',m) = divMod n 3



instance OEIS 39997 where
  oeisIx (succ->n) = length [p | p <- takeWhile (<= n) $ oeis @40,
                          show (fi p) `isInfixOf` show (fi n)]

instance OEIS 40014 where
  oeisIx = (oeisIx @720) . pred . (oeisIx @149)

instance OEIS 40081 where
  oeisIx 0 = 2
  oeisIx n = genericLength . takeWhile ((== 0) . (oeisIx @10051.pred)) .
                         iterate  ((+ 1) . (* 2)) $ n

instance OEIS 40976 where
  oeisIx n = (oeisIx @40) n - 2
  oeis = map (subtract 2) (oeis @40)

instance OEIS 42968 where
  oeisIx = (`div` 3) . (subtract 1) . (* 4) . succ
  oeis = filter ((/= 0) . (`mod` 4)) [1..]

instance OEIS 43096 where
  oeis = elemIndices 1 (oeis @196368)

instance OEIS 44102 where
  oeisIx = (* 36)
  oeis = [0, 36 ..]

instance OEIS 44432 where
  oeis = scanl1 (\v b -> 2 * v + b) (oeis @5614)

instance OEIS 45323 where
  oeis = filter ((== 1). (oeisIx @10051.pred)) $ tail (oeis @4776)

instance OEIS 45331 where
  oeis = filter ((< 4) . (`mod` 6)) (oeis @40)

instance OEIS 45468 where
  oeis = [x | x <- (oeis @47209), (oeisIx @10051.pred) x == 1]

instance OEIS 45472 where
  oeis = [x | x <- (oeis @47336), (oeisIx @10051.pred) x == 1]

instance OEIS 45542 where
  oeis = map (subtract 1) $ tail (oeis @1597)

instance OEIS 45623 where
  oeis = tail $ f (oeis @11782) [] where
     f (u:us) vs = sum (zipWith (*) vs $ reverse ws) : f us ws
       where ws = u : vs

instance OEIS 45636 where
  oeis = findIndices (> 0) (oeis @45698)

instance OEIS 45698 where
  oeisIx n = genericLength $ filter (\x -> x > 0 && (oeisIx @10051.pred) x == 1) $
    map (oeisIx @37213 . (n -)) $
    takeWhile (<= div n 2) (oeis @1248)

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

instance OEIS 45844 where
  oeis = iterate (oeisIx @95815 . pred) 1

instance OEIS 45954 where
  oeis =  2 : sieve 2 [2,4..] where
     sieve k xs = z : sieve (k + 1) (lucky xs) where
        z = xs !! (k - 1 )
        lucky ws = us ++ lucky vs where
              (us, _:vs) = splitAt (z - 1) ws

instance OEIS 45965 where
  oeisIx 0 = 2
  oeisIx n = oeisIx @3961 n

  oeis = 2 : tail (oeis @3961)

instance OEIS 45975 where
  oeis = tablList @45975
instance Table 45975 where
  rowCol = rowCol_off @45975 @1 @1
  rowT   = rowT_off   @45975 @1
  tabl = f 1 [1..] where
     f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
       ys | even k    = take k ms
          | otherwise = take k $ filter odd ms
       ms = filter ((== 0) . (`mod` k)) xs

instance OEIS 45980 where
  oeis = 0 : filter f [1..] where
     f x = g $ takeWhile ((<= 4 * x) . (^ 3)) $ (rowT @27750) x where
       g [] = False
       g (d:ds) = r == 0 && (oeisIx @10052) (d ^ 2 - 4 * y) == 1 || g ds
         where (y, r) = divMod (d ^ 2 - div x d) 3

instance OEIS 45995 where
  oeis = tablList @45995
instance Table 45995 where
  tabl = map (map (oeisIx @45)) (tabl @7318)

instance OEIS 46022 where
  oeis = [1..4] ++ drop 2 (oeis @40)

instance OEIS 46042 where
  oeisIx = p $ tail (oeis @583) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 46071 where
  oeis = tablList @46071
instance Table 46071 where
  rowCol = rowCol_off  @46071 @2 @1
  rowT = rowT_off @46071 @2
  tabf = f [1] 2 3 where
     f qs@ (q:_) i j = ys : f ((q + j) : qs) (i + 1) (j + 2) where
                      ys = nub $ sort $ filter (> 0) $ map (flip mod i) qs

instance OEIS 46075 where
  oeis = f $ S.fromList
                 [100 * a + 10 * b + a | a <- [1..9], b <- [0..9], b /= a]
     where f s = m : f (S.insert (10 * m + div (mod m 100) 10) s')
                 where (m, s') = S.deleteFindMin s

instance OEIS 46132 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) $ map (+ 4) (oeis @40)

instance OEIS 46301 where
  oeis = zipWith3 (((*) .) . (*))
                 (oeis @40) (tail (oeis @40)) (drop 2 (oeis @40))

instance OEIS 46660 where
  oeisIx (succ->n) = fi (sum es) - length es
    where
      es = snd $ unzip $ factorise (fi n)

--   oeisIx 0 = 0
--   oeisIx 1 = 0
--   oeisIx n = mexp (oeisIx' n)
--   oeisIx' n = (map trymove [0.. (div (n - 1) 2)])
--     where trymove k = nimSum (oeis !! k) (oeis !! (n-k-1))

instance OEIS 46669 where
 oeis = scanl1 (+) (oeis @20639)

instance OEIS 46670 where
  oeis = scanl1 (+) (oeis @6530)


instance OEIS 46699 where
  oeis = 1 : 1 : zipWith (+) zs (tail zs) where
     zs = map (oeisIx @46699 . pred) $ zipWith (-) [2..] (oeis @46699)

instance OEIS 46704 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @7953)) (oeis @40)

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

instance OEIS 46897 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ zipWith
              (\p e -> if p == 2 then 3 else div (p ^ (e + 1) - 1) (p - 1))
              (rowT @27748 n) (rowT @124010 n)

instance OEIS 46899 where
  oeis = tablList @46899
instance Table 46899 where
  tabl = zipWith take [1..] $ transpose (tabl @7318)

instance OEIS 46902 where
  oeis = tablList @46902
instance Table 46902 where
  tabl = [0] : iterate
                 (\row -> zipWith (+) ([0] ++ row) (row ++ [6])) [1,6]

instance OEIS 46920 where
  oeisIx (succ->n)
      = length
      . filter ((\x -> x == 1 || oeisIx @10051 (pred x) == 1) . (n -))
      $ takeWhile (< n) (oeis @1105)



instance OEIS 46921 where
  oeisIx = (oeisIx @46920) . pred . (oeisIx @5408)

instance OEIS 46951 where
  oeisIx = sum . map (oeisIx @10052) . (rowT @27750) . succ

instance OEIS 46953 where
  oeis = map (`div` 6) $
     filter ((== 0) . (oeisIx @10051.pred) . subtract 1) [6,12..]

instance OEIS 46954 where
  oeis = map (`div` 6) $ filter ((== 0) . (oeisIx @10051.pred) . (+ 1)) [0,6..]

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

instance OEIS 47791 where
  oeis = filter ((== 1) . (oeisIx @10051).pred . (oeisIx @62028)) [1..]

instance OEIS 47836 where
  oeis = f [2] where
     f (x:xs) = x : f (xs `O.union` map (x *) [2..x])

instance OEIS 47845 where
  oeisIx = (`div` 2) . (oeisIx @14076)

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

instance OEIS 47916 where
  oeis = tablList @47916
instance Table 47916 where
  rowCol = rowCol_off @47916 @1 @1
  rowT   = rowT_off   @47916 @1
  tabl = zipWith4 (zipWith4 (\x u v w -> x * v ^ u * w))
                 (tabl @54523) (tabl @2260) (tabl @10766) (tabl @166350)

instance OEIS 47917 where
  oeis = tablList @47917
instance Table 47917 where
  rowCol = rowCol_off @47917 @1 @1
  rowT   = rowT_off   @47917 @1
  tabl = zipWith (zipWith div) (tabl @47916) (tabl @2024)

instance OEIS 47918 where
  oeis = tablList @47918
instance Table 47918 where
  rowCol n k = sum [oeisIx @8683 (pred d) * (rowCol @47916) n (k `div` d) |
                     mod n k == 0, d <- [1..k], mod k d == 0]
  rowT n = map (rowCol @47918 n) [1..n]
  tabl = map (rowT @47918) [1..]

instance OEIS 47919 where
  oeis = tablList @47919
instance Table 47919 where
  rowCol = rowCol_off @47919 @1 @1
  rowT   = rowT_off   @47919 @1
  tabl = zipWith (zipWith div) (tabl @47918) (tabl @2024)

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

instance OEIS 48102 where
  oeis = 1 : f S.empty [1] (oeis @51674) where
    f s ys pps'@ (pp:pps)
      | S.null s = f (S.fromList (map (* pp) ys)) (pp:ys) pps
      | pp < m     = f (s `S.union` S.map (* pp) s `S.union`
                        S.fromList (map (* pp) ys)) ys pps
      | otherwise  = m : f s' (m:ys) pps'
      where (m,s') = S.deleteFindMin s

instance OEIS 48152 where
  oeis = tablList @48152
instance Table 48152 where
  rowCol = rowCol_off @48152 @1 @1
  rowT   = rowT_off   @48152 @1
  tabl = zipWith (map . flip mod) [1..] (tabl @133819)

instance OEIS 48161 where
  oeis = [p | p <- (oeis @65091), (oeisIx @10051.pred) ((p^2 + 1) `div` 2) == 1]

instance OEIS 48321 where
  oeis = filter f [0..] where
     f x = all (< 0) $ zipWith (-) (tail zs) zs
           where zs =  map length $ group $ show $ fi x

instance OEIS 48574 where
  oeis = f (drop 2 (oeis @41)) [1] where
    f (p:ps) rs = (sum $ zipWith (*) rs $ tail (oeis @41)) : f ps (p : rs)

instance OEIS 48594 where
  oeis = tablList @48594
instance Table 48594 where
  rowCol = rowCol_off @48594 @1 @1
  rowT   = rowT_off   @48594 @1
  tabl = map snd $ iterate f (1, [1]) where
     f (i, xs) = (i + 1, zipWith (-) (zipWith (*) [1..] ([0] ++ xs))
                                     (map (* i) (xs ++ [0])))

instance OEIS 48654 where
  oeis = 1 : 4 : zipWith (+) (oeis @48654) (map (* 2) $ tail (oeis @48654))

instance OEIS 48673 where
  oeisIx = (`div` 2) . (+ 1) . (oeisIx @45965)

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

instance OEIS 48736 where
  oeis = 1 : 1 : 1 : 1 :
     zipWith div
       (zipWith (+)
         (zipWith (*) (drop 3 (oeis @48736))
                      (drop 1 (oeis @48736)))
         (drop 2 (oeis @48736)))
       (oeis @48736)

instance OEIS 48760 where
  oeisIx = (^ 2) . (oeisIx @196)

instance OEIS 48772 where
  oeis = scanl1 (+) (oeis @48696)

instance OEIS 48793 where
  oeis = tablList @48793
instance Table 48793 where
  tabf = [0] : [1] : f [[1]] where
     f xss = yss ++ f (xss ++ yss) where
       yss = [y] : map (++ [y]) xss
       y = last (last xss) + 1

instance OEIS 48803 where
  oeis = scanl (*) 1 (oeis @7947)

instance OEIS 48853 where
  oeisIx (succ->n) = (sum $ map (oeisIx @10051 . pred . fi . (read :: String -> Integer)) $ tail $ nub $ concat $ zipWith
    (\its tls -> map ((\xs ys d -> xs ++ (d:ys)) its tls) "0123456789")
      (map init $ tail $ inits $ show $ fi n) (tail $ tails $ show $ fi n)) -
        (oeisIx @10051 . pred) n

instance OEIS 48859 where
  oeis = f 2 [1..] where
     f k xs = us ++ f (k + 1) (drop (k + 1) vs)
              where (us, vs) = splitAt k xs

instance OEIS 48865 where
  oeisIx (succ->n) = sum [oeisIx @10051 (pred t) | t <- [1..n], gcd n t == 1]

instance OEIS 48877 where
  oeis = 1 : 8 : zipWith (+) (oeis @48877) (map (* 4) $ tail (oeis @48877))

instance OEIS 48879 where
  oeis = 1 : 10 : zipWith (+) (oeis @48879) (map (* 4) $ tail (oeis @48879))

instance OEIS 48951 where
  oeis = 2 : 4 : ulam 2 4 (oeis @48951)

instance OEIS 48966 where
  oeis = tablList @48966
instance Table 48966 where
  rowCol = rowCol_off @48966 @1 @1
  rowT   = rowT_off   @48966 @1
  tabl = [1] : f 2 [1] where
     f x xs = ys : f (x + 1) ys where
       ys = map (flip div x) $ zipWith (+)
            (map (* 3) $ zipWith (*) (map (3 * (x - 1) -) [1..]) (xs ++ [0]))
            (zipWith (*) [1..] ([0] ++ xs))

instance OEIS 48967 where
  oeis = 0 : xs where
     xs = 0 : concat (transpose [zipWith (+) [1..] xs, map (* 2) xs])

instance OEIS 48973 where
  oeis = [1..] `O.minus` (oeis @5243)

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

instance OEIS 48994 where
  oeis = tablList @48994
instance Table 48994 where
  tabl = map fst $ iterate (\ (row, i) ->
    (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 0)

instance OEIS 49002 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @49001)

instance OEIS 49039 where
  oeis = tablList @49039
instance Table 49039 where
  rowCol = rowCol_off @49039 @1 @1
  rowT   = rowT_off   @49039 @1
  tabl = f 1 1 [1..] where
     f k p xs = ys : f (2 * k) (1 - p) (dropWhile (<= last ys) xs) where
       ys  = take k $ filter ((== p) . (`mod` 2)) xs

instance OEIS 49061 where
  oeis = tablList @49061
instance Table 49061 where
  rowCol = rowCol_off @49061 @1 @1
  rowT   = rowT_off   @49061 @1
  tabl = map fst $ iterate t ([1], 1) where
     t (row, k) = (if odd k then us else vs, k + 1) where
       us = zipWith (-) (row ++ [0]) ([0] ++ row)
       vs = zipWith (+) ((zipWith (*) ks row) ++ [0])
                        ([0] ++ (zipWith (*) (reverse ks) row))
            where ks = [1..k]

instance OEIS 49063 where
  oeis = tablList @49063
instance Table 49063 where
  tabf = [1] : iterate f [1, 1] where
     f row = 1 : 2 : zipWith (+) ( map (* 2) row) ((tail row) ++ [0])

instance OEIS 49072 where
  oeis = 1 : 3 :
      zipWith (-) (map (* 3) $ tail (oeis @49072)) (map (* 4) (oeis @49072))

instance OEIS 49073 where
  oeisIx = foldl lcm 1 . filter ((== 1) . (oeisIx @10055)) . (rowT @27750) . succ

instance OEIS 49084 where
  oeis = unfoldr x (1, 1, (oeis @40)) where
     x (i, z, ps'@ (p:ps)) | i == p = Just (z, (i + 1, z + 1, ps))
                          | i /= p = Just (0, (i + 1, z, ps'))

instance OEIS 49200 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (subtract 1) $ (rowT @265668) n

instance OEIS 49296 where
  oeis = zipWith (-) (tail (oeis @8364)) (oeis @8364)

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

instance OEIS 49343 where
  oeis = map fi $ elemIndices 0
     $ zipWith ((-) `on` (oeisIx @7953)) (oeis @5843) (oeis @290)

instance OEIS 49439 where
  oeis = filter (\x -> ((length $ oddDivs x) `elem` oddDivs x)) [1..]
     where oddDivs n = [d | d <- [1,3..n], mod n d == 0]

instance OEIS 49444 where
  oeis = tablList @49444
instance Table 49444 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 2)

instance OEIS 49455 where
  oeis = tablList @49455
instance Table 49455 where
  rowCol = rowCol_off @49455 @1 @1
  rowT   = rowT_off @49455 @1
  tabf = map (map numerator) $ iterate
     (\row -> concat $ transpose [row, zipWith (+/+) row $ tail row]) [0, 1]
     where u +/+ v = (numerator u + numerator v) %
                     (denominator u + denominator v)

instance OEIS 49456 where
  oeis = tablList @49456
instance Table 49456 where
  rowCol = rowCol_off @49456 @1 @1
  rowT   = rowT_off @49456 @1
  tabf = iterate
     (\row -> concat $ transpose [row, zipWith (+) row $ tail row]) [1, 1]

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

instance OEIS 49600 where
  oeis = tablList @49600
instance Table 49600 where
  tabl = [0] : map (0 :) (tabl @208341)

instance OEIS 49773 where
  oeis = tablList @49773
instance Table 49773 where
  rowCol = rowCol_off @49773 @1 @1
  rowT   = rowT_off @49773 @1
  tabf = iterate f [1] where
     f vs = (map (subtract 1) ws) ++ ws where ws = map (* 2) vs

instance OEIS 49820 where
  oeisIx n = 1 + n - (oeisIx @5) n

instance OEIS 50150 where
  oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @100995.pred)) [1, 3 ..]

instance OEIS 50168 where
  oeis = 1 : zipWith (+) (oeis @1405) (tail (oeis @1405))

instance OEIS 50216 where
  oeis = map length $ filter (/= [0]) $ group $ map (oeisIx @10051 . pred) (oeis @430)

instance OEIS 50292 where
  oeis = scanl (+) 0 (oeis @35263)

instance OEIS 50326 where
  oeis = 1 : f 2 where
     f x = (if x /= s then (oeisIx @50326 . pred) s
                      else length $ filter (== x) $ map product $
                           subsequences $ tail $ (rowT @206778) x) : f (x + 1)
           where s = (oeisIx @46523 . pred) x

instance OEIS 50376 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @100995) . pred) [1..]

instance OEIS 50512 where
  oeis = 0 : 1 : 1 : 1 : (-1) : zipWith div (zipWith (-) (zipWith (*)
     (drop 4 (oeis @50512)) (drop 2 (oeis @50512)))
       (map (^ 2) (drop 3 (oeis @50512)))) (tail (oeis @50512))

instance OEIS 50536 where
  oeis = iterate (oeisIx @217) 8

instance OEIS 50542 where
  oeis = iterate (oeisIx @217) 5

instance OEIS 50548 where
  oeis = iterate (oeisIx @217) 7

instance OEIS 50600 where
  oeis = tablList @50600
instance Table 50600 where
  rowCol (fi->n) (fi->k) = fi $ adder 0 (n - k) k where
     adder :: Int -> Int -> Int -> Int
     adder c u 0 = c
     adder c u v = adder (c + 1) (u `xor` v) (shiftL (u .&. v) 1)
  rowT n = map (rowCol @50600 n) $ reverse [0..n]
  tabl = map (rowT @50600) [0..]

instance OEIS 50804 where
  oeis = elemIndices 1 (oeis @84888)

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

instance OEIS 50936 where
  oeis = f S.empty [2] 2 $ tail (oeis @40) where
     f s bs c (p:ps)
       | S.null s || head bs <= m = f (foldl (flip S.insert) s bs') bs' p ps
       | otherwise                  = m : f (S.deleteMin s) bs c (p:ps)
       where m = S.findMin s
             bs' = map (+ p) (c : bs)

instance OEIS 50941 where
  oeis = O.minus [0..] (oeis @34706)

instance OEIS 50997 where
  oeisIx = (^ 5) . (oeisIx @40)
  oeis = map (^ 5) (oeis @40)

instance OEIS 51010 where
  oeis = tablList @51010
instance Table 51010 where
  rowCol n k = snd $ until ((== 0) . snd . fst)
                      (\ ((x, y), i) -> ((y, mod x y), i + 1)) ((n, k), 0)
  rowT n = map (rowCol @51010 n) [0..n - 1]
  tabl = map (rowT @51010) [1..]

instance OEIS 51023 where
  oeisIx n = (rowCol @70950) n n

instance OEIS 51037 where
  oeis = f $ S.singleton 1 where
     f s = y : f (S.insert (5 * y) $ S.insert (3 * y) $ S.insert (2 * y) s')
                 where (y, s') = S.deleteFindMin s

instance OEIS 51046 where
  oeis = filter
     (\x -> fi (oeisIx @720 $ pred x) > hs !! (x - 1)) [1..]
     where hs = zipWith (/)
                [1..] $ map (subtract 1.5) $ scanl1 (+) $ map (1 /) [1..]

instance OEIS 51064 where
  oeisIx = (+ 1) . length . takeWhile (== 3) . dropWhile (== 2) . (rowT @27746) . succ

instance OEIS 51120 where
  oeis = 1 : f [1] where
    f xs = seen ++ (f $ xs ++ seen) where
      seen = look (reverse $ map length $ group xs') (reverse $ nub xs')
      xs' = sort xs
      look [] []               = []
      look (cnt:cnts) (nr:nrs) = cnt : nr : look cnts nrs

instance OEIS 51129 where
  oeis = tablList @51129
instance Table 51129 where
  rowCol n k = k ^ (n - k)
  rowT   = rowT_off   @51129 @1
  tabl = zipWith (zipWith (^)) (tabl @2260) $ map reverse (tabl @2260)

instance OEIS 51159 where
  oeis = tablList @51159
instance Table 51159 where
  tabl = [1] : f [1] [1,1] where
     f us vs = vs : f vs (zipWith (+) ([0,0] ++ us) (us ++ [0,0]))

instance OEIS 51162 where
  oeis = tablList @51162
instance Table 51162 where
  tabl = iterate (\xs@ (x:_) -> (x + 1) : map (+ 2) xs) [0]

instance OEIS 51173 where
  oeis = tablList @51173
instance Table 51173 where
  rowCol = lcm
  rowT   = rowT_off @51173 @1
  tabl   = map (\x -> map (lcm x) [1..x]) [1..]

instance OEIS 51193 where
  oeisIx = sum . (rowT @51173) . succ

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

instance OEIS 51352 where
  oeis = 0 : zipWith (+)
     (oeis @51352) (zipWith (*) [1..] $ map ((1 -) . (* 2)) (oeis @10051))

instance OEIS 51353 where
  oeis = 0 : zipWith (+) (oeis @51353)
     (zipWith (\chi x -> x * (chi * (x + 1) - 1)) (oeis @10051) [1..])

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

instance OEIS 51417 where
  oeis = zipWith div (tail (oeis @25547)) (oeis @25547)

instance OEIS 51436 where
  oeisIx n = (3 ^ n + 3 ^ m - 2 ^ n + (1 - r) * 2 ^ m) `div` 2 + r
              where (m,r) = divMod n 2

instance OEIS 51451 where
  oeis = scanl1 lcm (oeis @961)

instance OEIS 51523 where
  oeis = tablList @51523
instance Table 51523 where
  tabl = map fst $ iterate (\ (row, i) ->
     (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 10)

instance OEIS 51533 where
  oeis = filter ((> 0) . (oeisIx @53603)) [1..]

instance OEIS 51536 where
  oeis = scanl1 lcm (oeis @16777)

instance OEIS 51537 where
  oeis = tablList @51537
instance Table 51537 where
  rowCol = rowCol_off @51537 @1 @1
  rowT   = rowT_off   @51537 @1
  tabl = zipWith (zipWith div) (tabl @51173) (tabl @50873)

instance OEIS 51538 where
  oeis = scanl1 lcm $ tail (oeis @330)

instance OEIS 51542 where
  oeis = zipWith div (tail (oeis @51538)) (oeis @51538)

instance OEIS 51543 where
  -- oeisIx n = (oeis @51542) !! (n - 1)
  oeis = zipWith div (tail (oeis @25555)) (oeis @25555)

instance OEIS 51597 where
  oeis = tablList @51597
instance Table 51597 where
  tabl = iterate (\row -> zipWith (+) ([1] ++ row) (row ++ [1])) [1]

instance OEIS 51599 where
  oeis = tablList @51599
instance Table 51599 where
  tabl = map fst $ iterate f ([2], (oeis @1223)) where
     f (row, (d:ds)) =  (zipWith (+) ([d] ++ row) (row ++ [d]), ds)

instance OEIS 51601 where
  oeis = tablList @51601
instance Table 51601 where
  tabl = iterate (\row -> zipWith (+) ([1] ++ row) (row ++ [1])) [0]

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

instance OEIS 51632 where
  oeis = tablList @51632
instance Table 51632 where
  tabl = iterate (\rs -> zipWith (+) ([1] ++ rs) (rs ++ [1])) [-2]

instance OEIS 51634 where
  oeis = f (oeis @40) where
     f (p:qs@ (q:r:ps)) = if 2 * q > (p + r) then q : f qs else f qs

instance OEIS 51635 where
  oeis = g (oeis @40) where
     g (p:qs@ (q:r:ps)) = if 2 * q < (p + r) then q : g qs else g qs

instance OEIS 51638 where
  oeisIx = sum . (rowT @83093)

instance OEIS 51666 where
  oeis = tablList @51666
instance Table 51666 where
  tabl = map fst $ iterate
     (\ (vs, w:ws) -> (zipWith (+) ([w] ++ vs) (vs ++ [w]), ws))
     ([0], [1, 3 ..])

instance OEIS 51683 where
  oeis = tablList @51683
instance Table 51683 where
  rowCol = rowCol_off @51683 @1 @1
  rowT   = rowT_off   @51683 @1
  tabl = map fst $ iterate f ([1], 2) where
     f (row, n) = (row' ++ [head row' + last row'], n + 1) where
       row' = map (* n) row

instance OEIS 51736 where
  oeis = 1 : 5 : 17 : 63 : zipWith (-) (map (* 2) $ drop 2 $
     zipWith (+) (map (* 3) (oeis @51736)) (tail (oeis @51736))) (oeis @51736)

instance OEIS 51777 where
  oeis = tablList @51777
instance Table 51777 where
  rowCol = rowCol_off @51777 @0 @1
  rowT n = map (mod n) [n, n - 1 .. 1]
  tabl = map (rowT @51777) [1..]

instance OEIS 51778 where
  oeis = tablList @51778
instance Table 51778 where
  rowCol = rowCol_off @51778 @3 @1
  rowT = rowT_off @51778 @3
  tabl = map (\xs -> map (mod (head xs + 1)) xs) $
                     iterate (\xs -> (head xs + 1) : xs) [2]

instance OEIS 51793 where
  oeis = 1 : 1 : 1 : 1 : f [1, 1, 1, 1] [-1, 1, -1, 1] where
     f xs'@ (x:xs) as'@ (a:as) = y : f (xs ++ [y]) (as ++ [a]) where
       y = sum $ zipWith (*) xs' as'

instance OEIS 51801 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @51801 n') * (m + 0 ^ m) where (n',m) = divMod n 10

instance OEIS 51802 where
  oeisIx 0 = 1
  oeisIx n = until (< 10) (oeisIx @51801) n

instance OEIS 51838 where
  oeis = (1:) . (3:) . map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @2110) $ tail (oeis @7504)

instance OEIS 51840 where
  oeis = map floor vs where
     vs = iterate (\x -> x * (4 - 3 * x)) 0.1

instance OEIS 51841 where
  oeisIx (succ->n) = (sum $ zipWith (\u v -> gcd 2 u * (oeisIx @8683.pred) u * 2 ^ v)
               ds $ reverse ds) `div` (2 * n) where ds = (rowT @27750) n

instance OEIS 51884 where
  oeis =  1 : f 1 (oeis @2808) where
     f x cs = y : f y (dropWhile (<= y) cs) where
       y = head [z | z <- cs, x `gcd` z == 1]

instance OEIS 51924 where
  oeis = zipWith (-) (tail (oeis @984)) (oeis @984)

instance OEIS 51933 where
  oeis = tablList @51933
instance Table 51933 where
  rowCol n k = fi do fi n `xor` fi k :: Int
  rowT n = map (rowCol @51933 n) [0..n]
  tabl = map (rowT @51933) [0..]

instance OEIS 51934 where
  oeis = 2 : tail do f 0 (oeis @2113) where
     f x (m:ms) | (oeisIx @10051) (x + m) == 1 = m : f (x + m) ms
                | otherwise            = f x ms

instance OEIS 51936 where
  oeisIx = (subtract 9) . (oeisIx @217) . (+4)
  oeis = scanl (+) 1 [5..]

instance OEIS 51950 where
  oeis = zipWith (-) (tail (oeis @5)) (oeis @5)

instance OEIS 51953 where
  oeisIx (succ->n) = n - (oeisIx @10 . pred) n

instance OEIS 52001 where
  oeis = filter even (oeis @41)

instance OEIS 52002 where
  oeis = findIndices odd (oeis @41)

instance OEIS 52003 where
  oeis = tail $ filter odd (oeis @41)

instance OEIS 52147 where
  oeisIx = (+ 2) . (oeisIx @40)

instance OEIS 52180 where
  oeis = f [4..] where
     f ws = maximum (map (oeisIx @20639 . pred) us) : f vs where
       (us, _:vs) = span ((== 0) . oeisIx @10051 . pred) ws

instance OEIS 52216 where
  oeis = 2 : f [2] 9 where
     f xs@ (x:_) z = ys ++ f ys (10 * z) where
                    ys = (x + z) : map (* 10) xs

instance OEIS 52287 where
  oeis = f [3] where
     f (x:xs) = x : f (xs `O.union` map (x *) [2..x])

instance OEIS 52294 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (oeisIx @120)) [1..]

instance OEIS 52343 where
  oeisIx = (flip div 2) . (+ 1) . (oeisIx @8441)

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

instance OEIS 52582 where
  oeis =  0 : 2 : zipWith
     div (zipWith (*) (tail (oeis @52582)) (drop 2 (oeis @290))) [1..]

instance OEIS 52849 where
  oeisIx n = if n == 0 then 0 else 2 * (oeisIx @142) n
  oeis = 0 : fs where fs = 2 : zipWith (*) [2..] fs

instance OEIS 52928 where
  oeisIx = (* 2) . flip div 2
  oeis = 0 : 0 : map (+ 2) (oeis @52928)

instance OEIS 52938 where
  oeis = 1 : 3 : 2 : zipWith (-) [5..] (oeis @52938)

instance OEIS 52952 where
  oeis = 1 : 1 : zipWith (+)
     (oeis @59841) (zipWith (+) (oeis @52952) $ tail (oeis @52952))

instance OEIS 52955 where
  oeis = 1 : 2 : map ((+ 1) . (* 2)) (oeis @52955)

instance OEIS 53001 where
  oeisIx = oeisIx @7917 . pred . pred . oeisIx @290 . succ . succ

instance OEIS 53012 where
  oeis = tail $ f
     [oeis @292, (oeis @578), (oeis @5900), (oeis @6566), (oeis @6564)]
     where f pss = m : f (map (dropWhile (<= m)) pss)
                   where m = minimum (map head pss)

instance OEIS 53029 where
  oeis = filter ((== 4) . (oeisIx @1176 . pred)) [1..]

instance OEIS 53030 where
  oeis = filter ((== 2) . (oeisIx @1176 . pred)) [1..]

instance OEIS 53031 where
  oeis = filter ((== 1) . (oeisIx @1176 . pred)) [1..]

instance OEIS 53050 where
  oeisIx (succ->n) = head [k | (k, x) <- zip [1..] $ tail (oeis @7504), mod x n == 0]

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

instance OEIS 53132 where
  oeis = f [1] $ drop 2 (oeis @217) where
     f xs ts'@ (t:ts) = (sum $ zipWith (*) xs ts') : f (t:xs) ts

instance OEIS 53186 where
  oeisIx n = n - (oeisIx @48760) n
  oeis = f 0 0 (map fst $ iterate (\ (y,z) -> (y+z,z+2)) (0,1))
     where f e x ys'@ (y:ys) | x < y  = e : f (e + 1) (x + 1) ys'
                            | x == y = 0 : f 1 (x + 1) ys

instance OEIS 53188 where
  oeisIx 0 = 0
  oeisIx n = min (n - last xs) (head ys - n) where
     (xs,ys) = span (< n) (oeis @290)

instance OEIS 53200 where
  oeis = tablList @53200
instance Table 53200 where
  tabl = [0] : zipWith (map . flip mod) [1..] (tail (tabl @7318))

instance OEIS 53201 where
  oeis = tablList @53201
instance Table 53201 where
  rowCol = rowCol_off @53201 @2 @1
  rowT = rowT_off @53201 @2
  tabl = zipWith (map . (flip mod)) [2..] (tabl @14410)

instance OEIS 53202 where
  oeis = tablList @53202
instance Table 53202 where
  rowCol = rowCol_off @53202 @4 @0
  rowT = rowT_off @53202 @4
  tabl = zipWith (\k row -> take (k - 3) $ drop 2 row)
                         [4..] $ drop 4 (tabl @53200)

instance OEIS 53203 where
  oeis = tablList @53203
instance Table 53203 where
  rowCol = rowCol_off @53203 @6 @0
  rowT = rowT_off @53203 @6
  tabl = zipWith (\k row -> take (k - 5) $ drop 3 row)
                         [6..] $ drop 6 (tabl @53200)

instance OEIS 53204 where
  oeisIx = sum . (rowT @53200)

instance OEIS 53205 where
  oeisIx = sum . (rowT @53201) . succ . succ

instance OEIS 53206 where
  oeisIx = sum . (rowT @53203) . (+ 6)

instance OEIS 53214 where
  oeisIx 0 = 1
  oeisIx n = (rowCol @53200) (2 * n) n

instance OEIS 53220 where
  oeisIx (succ->n) = (rowCol @56242) (n + 1) n

instance OEIS 53222 where
  oeis = zipWith (-) (tail (oeis @203)) (oeis @203)

instance OEIS 53224 where
  oeis = map (+ 1) $ elemIndices True $
     zipWith (<) (oeis @203) $ tail (oeis @203)

instance OEIS 53226 where
  oeis = map (+ 1) $ findIndices (< 0) (oeis @53222)

instance OEIS 53230 where
  oeis = zipWith (-) (tail (oeis @53224)) (oeis @53224)

instance OEIS 53233 where
  oeis = map (+ 1) $ elemIndices 2 (oeis @53230)

instance OEIS 53234 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @53230)

instance OEIS 53235 where
  oeis = map (+ 1) $ elemIndices 3 (oeis @53230)

instance OEIS 53236 where
  oeis = map (+ 1) $ elemIndices 4 (oeis @53230)

instance OEIS 53238 where
  oeis = zipWith (-) (tail (oeis @53226)) (oeis @53226)

instance OEIS 53240 where
  oeis = map (+ 1) $ findIndices (/= 2) (oeis @53238)

instance OEIS 53241 where
  oeis = map (+ 1) $ elemIndices 2 (oeis @53238)

instance OEIS 53242 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @53238)

instance OEIS 53245 where
  oeis = f (oeis @53242) where
     f (x:x':xs) | x' == x+1 = x : f xs
                 | otherwise = f (x':xs)

instance OEIS 53384 where
  oeisIx = flip (rowCol @53398) 4 . (+ 3) . succ

instance OEIS 53385 where
  oeisIx = flip (rowCol @53398) 5 . (+ 4) . succ

instance OEIS 53386 where
  oeisIx = flip (rowCol @53398) 6 . (+ 5) . succ

instance OEIS 53387 where
  oeisIx = flip (rowCol @53398) 7 . (+ 6) . succ

instance OEIS 53388 where
  oeisIx = flip (rowCol @53398) 8 . (+ 7) . succ

instance OEIS 53389 where
  oeisIx = flip (rowCol @53398) 9 . (+ 8) . succ

instance OEIS 53390 where
  oeisIx = flip (rowCol @53398) 10 . (+ 9) . succ

instance OEIS 53398 where
  oeis = tablList @53398
instance Table 53398 where
  rowCol n k = (oeisIx @7814 . pred) $ (rowCol @3986) (n - 1) (k - 1) + 1
  rowT n = map (rowCol @53398 n) [1..n]
  tabl = map (rowT @53398) [1..]

instance OEIS 53399 where
  oeisIx = flip (rowCol @53398) 3 . (+ 2) . succ

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

instance OEIS 53478 where
  oeisIx = (+ 1) . sum . takeWhile (/= 1) . iterate (oeisIx @10 . pred) . succ

instance OEIS 53575 where
  oeisIx = (oeisIx @265 . pred) . (oeisIx @10)

instance OEIS 53585 where
  oeisIx = last . (rowT @141809) . succ

instance OEIS 53590 where
  oeisIx 0 = 1
  oeisIx (succ->n) = last $ takeWhile ((== 0) . (mod n)) $
                     scanl1 (*) $ dropWhile (< (oeisIx @20639 . pred) n) (oeis @40)

instance OEIS 53603 where
  oeisIx n = sum $ map (oeisIx @10054 . (n -)) $
                    takeWhile (< n) $ tail (oeis @217)

instance OEIS 53610 where
  oeisIx (succ->n) = s n $ reverse $ takeWhile (<= n) $ tail (oeis @290) where
    s _ []                 = 0
    s m (x:xs) | x > m     = s m xs
               | otherwise = m' + s r xs where (m',r) = divMod m x

instance OEIS 53644 where
  oeisIx n = if n <= 1 then n else 2 * (oeisIx @53644) (div n 2)
  oeis = 0 : concat (iterate (\zs -> map (* 2) (zs ++ zs)) [1])

instance OEIS 53650 where
  oeisIx = (oeisIx @51953 . pred) . (oeisIx @290) . succ

instance OEIS 53661 where
  oeis = filter (> 0) (oeis @175880)

instance OEIS 53669 where
  oeisIx (succ->n) = head $ dropWhile ((== 0) . (mod n)) (oeis @40)

instance OEIS 53670 where
  oeisIx (succ->n) = head [x | x <- [3, 5 ..],
                        n `gcd` x == 1, (n + 1) `gcd` x == 1]

instance OEIS 53671 where
  oeisIx (succ->n) = f $ drop 2 (oeis @40) where
     f (p:ps) | (n `mod` p) * ((n+1) `mod` p) * ((n+2) `mod` p) > 0 = p
              | otherwise = f ps

instance OEIS 53672 where
  oeisIx (succ->n) = 2 + fromJust
     (elemIndex 1 $ map (gcd $ foldl1 lcm $ take 4 [n..]) [2..])

instance OEIS 53673 where
  oeisIx (succ->n) = 2 + fromJust
     (elemIndex 1 $ map (gcd $ foldl1 lcm $ take 5 [n..]) [2..])

instance OEIS 53674 where
  oeisIx (succ->n) = 2 + fromJust
     (elemIndex 1 $ map (gcd $ foldl1 lcm $ take 6 [n..]) [2..])

instance OEIS 53685 where
  oeis = dropWhile (<= 7) $ i (oeis @47211) (oeis @5382) where
     i xs'@ (x:xs) ys'@ (y:ys) | x < y     = i xs ys'
                             | x > y     = i xs' ys
                             | otherwise = x : i xs ys

instance OEIS 53735 where
  oeisIx = sum . (rowT @30341)

instance OEIS 53737 where
  oeisIx n = if n == 0 then 0 else (oeisIx @53737) m + r where (m, r) = divMod n 4

instance OEIS 53754 where
  oeis = 0 : filter (even . (oeisIx @70939)) [1..]

instance OEIS 53767 where
  oeis = scanl (+) 0 (oeis @2808)

instance OEIS 53810 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (oeisIx @100995) . pred) $ tail (oeis @961)

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

instance OEIS 53868 where
  oeis = filter (odd . (oeisIx @1065) . pred) [1..]

instance OEIS 53869 where
  oeis = filter (even . (oeisIx @1065) . pred) [1..]
  -- oeis = map (+ 1) $ findIndices even $ map (oeisIx @1065) [1..]

instance OEIS 53871 where
  oeis = 1 : 0 : zipWith (*)
     [2,4..] (zipWith (+) (oeis @53871) $ tail (oeis @53871))

instance OEIS 53989 where
  oeisIx 0=3
  oeisIx n = head [k | k <- [1..], (oeisIx @10051) (k * n - 1) == 1]

instance OEIS 54008 where
  oeisIx (succ->n) = n `mod` (oeisIx @5 . pred) n

instance OEIS 54024 where
  oeisIx (succ->n) = mod (oeisIx @203 $ pred n) n

instance OEIS 54025 where
  oeis = zipWith mod (oeis @203) (oeis @5)

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

instance OEIS 54124 where
  oeis = tablList @54124
instance Table 54124 where
  tabl = map reverse (tabl @54123)

instance OEIS 54211 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @127423).pred) [1..]

instance OEIS 54353 where
  oeis = scanl1 (+) (oeis @2)

instance OEIS 54354 where
  oeis = zipWith (-) (tail (oeis @2)) (oeis @2)

instance OEIS 54385 where
  oeis = map (floor . (* e') . fi) [1..]
     where e' = e / (e - 1); e = exp 1

instance OEIS 54429 where
  oeis = f [1..] where
     f xs@ (x:_) = reverse us ++ f vs where (us, vs) = splitAt x xs

instance OEIS 54522 where
  oeis = tablList @54522
instance Table 54522 where
  rowCol = rowCol_off @54522 @1 @1
  tabl = map (rowT @54522) [1..]
  rowT n = map (\k -> if n `mod` k == 0 then (oeisIx @10 . pred) k else 0) [1..n]

instance OEIS 54523 where
  oeis = tablList @54523
instance Table 54523 where
  rowCol = rowCol_off @54523 @1 @1
  rowT   = rowT_off   @54523 @1
  tabl = map (map (\x -> if x == 0 then 0 else (oeisIx @10 . pred) x)) (tabl @126988)

instance OEIS 54527 where
  oeis = tablList @54527
instance Table 54527 where
  rowCol = rowCol_off @54527 @1 @1
  rowT   = rowT_off   @54527 @1
  tabl = tail $ inits (oeis @8683)

instance OEIS 54531 where
  oeis = tablList @54531
instance Table 54531 where
  rowCol n k = div n $ gcd n k
  rowT   = rowT_off   @54531 @1
  tabl = zipWith (\u vs -> map (div u) vs) [1..] (tabl @50873)

instance OEIS 54582 where
  oeis = tablList @54582
instance Table 54582 where
  tabl = iterate
     (\xs@ (x:_) -> (2 * x) : zipWith (+) xs (iterate (`div` 2) (2 * x))) [1]

instance OEIS 54632 where
  oeis = scanl1 (+) (oeis @7376)

instance OEIS 54635 where
  oeis = tablList @54635
instance Table 54635 where
  tabf = map reverse (tabf @30341)

instance OEIS 54654 where
  oeis = tablList @54654
instance Table 54654 where
  tabl = map reverse (tabl @48994)

instance OEIS 54685 where
  oeis = map (p' 2) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < pp then 0 else p' (k + 1) (m - pp) + p' (k + 1) m
             where pp = oeisIx @961 $ pred k

instance OEIS 54791 where
  oeis = 0 : 1 : f 2 where
     f x | r ^ 2 == x  = (oeisIx @54791) r ^ 2 : f (x + 1)
         | odd (x - r) = x + 1         : f (x + 1)
         | otherwise   = (x - 1) ^ 2   : f (x + 1)
         where r = (oeisIx @196) x

instance OEIS 54842 where
  oeisIx = f (oeis @40) 1 where
     f _      y 0 = y
     f (p:ps) y x = f ps (y * p ^ d) x'  where (x', d) = divMod x 10

instance OEIS 54888 where
  oeis = 1 : zipWith (+) (tail (oeis @2878)) (oeis @2878)

instance OEIS 54895 where
  oeis = scanl (+) 0 (oeis @122841)

instance OEIS 54977 where
  oeisIx 0 = 2
  oeisIx n = 1
  oeis = 2 : repeat 1

instance OEIS 54978 where
  oeis = map head $ iterate
                 (\lds -> map abs $ zipWith (-) (tail lds) lds) (oeis @959)

instance OEIS 54986 where
  oeis = map fi $ filter modest [1..] where
     modest x = or $ zipWith m
                (map read $ (init $ tail $ inits $ show x) :: [Integer])
                (map read $ (tail $ init $ tails $ show x) :: [Integer])
        where m u v = u < v && (x - u) `mod` v == 0

instance OEIS 55011 where
  oeis = iterate (oeisIx @208241 . pred) 2

instance OEIS 55018 where
  oeis = map (oeisIx @54986)
       $ elemIndices 1
       $ zipWith (-) (tail (oeis @54986)) (oeis @54986)

instance OEIS 55029 where
  oeisIx 2 = 1
  oeisIx n = 2 * (oeisIx @79260 . pred) n + (oeisIx @79261 . pred) (oeisIx @37213 n)

instance OEIS 55038 where
  oeis = scanl1 (+) (oeis @66829)

instance OEIS 55040 where
  oeis = map (* 3) (oeis @55048)

instance OEIS 55045 where
  oeis = filter ((== 5) . (flip mod 8) . f) [1..] where
     f x = if r == 0 then f x' else x  where (x', r) = divMod x 4

instance OEIS 55048 where
  oeis = filter (s 0) [1..] where
     s t u | m > 0  = even t && m == 2
           | m == 0 = s (t + 1) u' where (u',m) = divMod u 3

instance OEIS 55079 where
  oeisIx (succ->n) = head [x | x <- [1..], (oeisIx @33273 . pred) x == n]

instance OEIS 55087 where
  oeis = tablList @55087
instance Table 55087 where
  tabf = concat $ transpose [tabl @2262, (tabl @2262)]

instance OEIS 55096 where
  oeis = tablList @55096
instance Table 55096 where
  rowCol = rowCol_off @55096 @1 @1
  rowT   = rowT_off   @55096 @1
  tabl = zipWith (zipWith (+)) (tabl @133819) (tabl @140978)

instance OEIS 55248 where
  oeis = tablList @55248
instance Table 55248 where
  tabl = map reverse (tabl @8949)

instance OEIS 55265 where
  oeis = 1 : f 1 [2..] where
     f x vs = g vs where
       g (w:ws) = if (oeisIx @10051.pred) (x + w) == 1
                     then w : f w (delete w vs) else g ws

instance OEIS 55266 where
  oeis = 1 : f 1 [2..] where
     f u vs = g vs where
       g (w:ws) | (oeisIx @10051.pred) (u + w) == 0 = w : f w (delete w vs)
                | otherwise = g ws

instance OEIS 55401 where
  oeisIx n = s n $ reverse $ takeWhile (<= n) $ tail (oeis @578) where
    s _ []                 = 0
    s m (x:xs) | x > m     = s m xs
               | otherwise = m' + s r xs where (m',r) = divMod m x

instance OEIS 55483 where
  oeisIx (succ->n) = gcd n $ (oeisIx @4086) n

instance OEIS 55491 where
  oeisIx = (^ 2) . (oeisIx @7913)

instance OEIS 55498 where
  oeis = 0 : 1 : map (oeisIx @7918) do zipTail (+) $ oeis @55498

instance OEIS 55500 where
  oeis = 1 : 1 : map (oeisIx @7917 . pred . pred) do zipTail (+) $ oeis @55500

instance OEIS 55608 where
  oeis = 1 : 13 : 92 : zipWith (+)
     (zipWith (-) (map (* 2) $ drop 2 (oeis @55608)) (oeis @55608))
     (drop 2 $ zipWith (+) (tail (oeis @2889)) (oeis @2889))

instance OEIS 55615 where
  oeisIx (succ->n) = (oeisIx @8683 . pred) n * n

instance OEIS 55641 where
  oeisIx n | n < 10    = 0 ^ n
           | otherwise = (oeisIx @55641) n' + 0 ^ d where (n',d) = divMod n 10

instance OEIS 55642 where
  oeisIx n = length $ show (fi n)

instance OEIS 55653 where
  oeisIx = sum . map (oeisIx @10 . pred) . (rowT @77610) . succ

instance OEIS 55654 where
  oeis = zipWith (-) [1..] (oeis @55653)

instance OEIS 55790 where
  oeis = 0 : 2 : zipWith (+)
     (zipWith (*) [0..] (oeis @55790)) (zipWith (*) [2..] $ tail (oeis @55790))

instance OEIS 55944 where
  oeisIx n = n + (oeisIx @30101) n

instance OEIS 55948 where
  oeisIx n = n + (oeisIx @30103) n

instance OEIS 55980 where
  -- oeisIx = floor . sum . map (1 %) . enumFromTo 1
  oeis = map floor $ scanl1 (+) $ map (1 %) [1..]

instance OEIS 56020 where
  oeis = 1 : 8 : map (+ 9) (oeis @56020)

instance OEIS 56230 where
  oeis = tablList @56230
instance Table 56230 where
  rowCol = rowCol_off @56230 @1 @1
  tabl = [1] : f [1] [2..] [1] where
     f adiag (a:as) us | null (adiag' `intersect` us) =
                         adiag' : f adiag' (as \\ adiag') (us `union` adiag')
                       | otherwise = f adiag as us
                       where adiag' = scanl (+) a adiag

instance OEIS 56242 where
  oeis = tablList @56242
instance Table 56242 where
  rowCol = rowCol_off @56242 @1 @1
  rowT   = rowT_off   @56242 @1
  tabl = [1] : [1,2] : f [1] [1,2] where
     f us vs = ws : f vs ws where
       ws = zipWith (-) (map (* 2) $ zipWith (+) ([0] ++ vs) (vs ++ [0]))
                        (zipWith (+) ([0] ++ us ++ [0]) (us ++ [0,0]))

instance OEIS 56538 where
  oeis = tablList @56538
instance Table 56538 where
  rowCol = rowCol_off @56538 @1 @1
  rowT   = rowT_off @56538 @1
  tabf = map reverse (tabf @27750)

instance OEIS 56542 where
  oeis = 0 : map (+ 1) (zipWith (*) [2..] (oeis @56542))

instance OEIS 56558 where
  oeis = concatMap (concat . init . inits . enumFromTo 0) [0..]

instance OEIS 56561 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @202018)) [0..]

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

instance OEIS 56980 where
  oeisIx = sum . map (fi.fromEnum . ([0,1,1] `isPrefixOf`)) .
                      tails . (rowT @30308).succ

instance OEIS 56992 where
  oeisIx = (oeisIx @10888) . (oeisIx @290) . succ

instance OEIS 57020 where
  oeisIx n = numerator $ (oeisIx @203) n % (oeisIx @5) n

instance OEIS 57021 where
  oeisIx n = denominator $ (oeisIx @203) n % (oeisIx @5) n

instance OEIS 57022 where
  oeisIx n = (oeisIx @203) n `div` (oeisIx @5) n

instance OEIS 57062 where
  oeis = f 1 [1..] where
     f j xs = (replicate (sum $ map (oeisIx @10051.pred) dia) j) ++ f (j + 1) xs'
       where (dia, xs') = splitAt j xs

instance OEIS 57077 where
  oeisIx d = (-1)^quot d 2
  oeis     = cycle [1, 1, -1, -1]

instance OEIS 57142 where
  oeisIx (succ->n) = head $ reverse $ sort $ map length $ group $
              sort [u * v | u <- [1..n], v <- [1..n]]

instance OEIS 57143 where
  oeisIx (succ->n) = head $ head $ reverse $ sortBy (compare `on` length) $
              group $ sort [u * v | u <- [1..n], v <- [1..n]]

instance OEIS 57147 where
  oeisIx n = (oeisIx @7953) n * n

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

instance OEIS 57606 where
  oeis = tablList @57606
instance Table 57606 where
  rowCol = rowCol_off @57606 @3 @1
  rowT   = rowT_off @57606 @3
  tabf = map g $ drop 3 $
    iterate (\xs -> (map (0 :) xs) ++ (map (1 :) xs)) [[]] where
    g xss = map length $ fill0 $ group $ sort $ map (length . del2) xss
      where fill0 uss = f0 uss [1 .. length xss `div` 4] where
             f0 _  []        = []
             f0 [] (j:js)    = [] : f0 [] js
             f0 vss'@ (vs:vss) (j:js)
              | j == head vs = vs : f0 vss js
              | otherwise    = [] : f0 vss' js
    del2 = nub . (concatMap del1) . del1
    del1 xs = nub $
              zipWith (++) (init $ inits xs) (map tail $ init $ tails xs)

instance OEIS 57607 where
  oeis = tablList @57607
instance Table 57607 where
  rowCol = rowCol_off @57607 @2 @0
  rowT   = rowT_off @57607 @2
  tabf =  [2] : map (0 :) (tabf @57606)

instance OEIS 57660 where
  oeisIx (succ->n) = sum $ map (div n) $ (rowT @50873) n

instance OEIS 57716 where
  oeis = elemIndices 0 (oeis @209229)

instance OEIS 57728 where
  oeis = tablList @57728
instance Table 57728 where
  rowCol = rowCol_off @57728 @1 @1
  rowT   = rowT_off   @57728 @1
  tabl = iterate
     (\row -> zipWith (+) (row ++ [0]) ([0] ++ tail row ++ [1])) [1]

instance OEIS 57820 where
  oeis = zipWith (-) (tail (oeis @961)) (oeis @961)

instance OEIS 57944 where
  oeis = tablList @57944
instance Table 57944 where
  tabl = zipWith ($) (map replicate [1..]) (oeis @217)

instance OEIS 57979 where
  oeisIx n = 1 - rest * (1 - n') where (n', rest) = divMod n 2
  oeis = concat $ transpose [repeat 1, [0..]]

instance OEIS 58006 where
  oeis = scanl1 (+) (oeis @133942)

instance OEIS 58187 where
  oeis = 1 : f 1 1 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ zipWith (*) [1..x] [x,x - 1..1]

instance OEIS 58254 where
  oeis = scanl1 lcm (oeis @6093)

instance OEIS 58257 where
  oeis = tablList @58257
instance Table 58257 where
  tabl = [1] : ox 0 [1] where
     ox turn xs = ys : ox (mod (turn + 1) 4) ys
        where ys | turn <= 1 = scanl (+) 0 xs
                 | otherwise = reverse $ scanl (+) 0 $ reverse xs

instance OEIS 58294 where
  oeis = tablList @58294
instance Table 58294 where
  rowCol = rowCol_off @58294 @1 @1
  rowT   = rowT_off @58294 @1
  tabf = [1] : zipWith (++) xss (map (tail . reverse) xss)
                 where xss = tail (tabl @102473)

instance OEIS 58312 where
  oeis = map denominator $ scanl1 (+) $
                     map (1 %) $ tail (oeis @181983)

instance OEIS 58313 where
  oeis = map numerator $ scanl1 (+) $ map (1 %) $ tail (oeis @181983)

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

instance OEIS 58922 where
  oeisIx (succ->n) = (n - 1) * 2 ^ n
  oeis = zipWith (*) [0..] $ tail (oeis @79)

instance OEIS 59015 where
  oeis = scanl1 (+) $ map (oeisIx @23416) [0..]

instance OEIS 59100 where
  oeisIx = (+ 2) . (^ 2)
  oeis = scanl (+) (2) [1, 3 ..]

instance OEIS 59169 where
  oeis = map abs $ zipWith (-) (tail (oeis @178804)) (oeis @178804)

instance OEIS 59175 where
  oeisIx n = f [n % 1] where
     f xs@ (x:_) | denominator y == 1 = numerator y
                | y `elem` xs        = 0
                | otherwise          = f (y : xs)
                where y = (numerator x * denominator x) %
                          (oeisIx @7953 (numerator x) + (oeisIx @7953) (denominator x))



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

instance OEIS 59496 where
  oeis = 2 : f [2] [2] where
     f qs xs = g candidates where
       g [] = []
       g (ys:yss) | (oeisIx @10051.pred) q == 0 || q `elem` qs = g yss
                  | otherwise = q : f (q:qs) ys
                  where q = foldr (\d r -> 10 * r + d) 0 ys
       candidates = [us ++ [z] ++ vs | i <- [0 .. length xs - 1],
                           let (us, (_:vs)) = splitAt i xs, z <- [1..9]] ++
                    [xs ++ [z] | z <- [1..9]]

instance OEIS 59576 where
  oeis = tablList @59576
instance Table 59576 where
  tabl = [1] : map fst (iterate f ([1,1], [2,3,2])) where
     f (us, vs) = (vs, map (* 2) ws) where
       ws = zipWith (-) (zipWith (+) ([0] ++ vs) (vs ++ [0]))
                        ([0] ++ us ++ [0])

instance OEIS 59727 where
  oeis = zipWith (*) (oeis @45) $ map (+ 1) (oeis @45)

instance OEIS 59730 where
  oeisIx ((+3)->n) = (tabl @59922) !! n !! (n - 3)
instance OEIS 59731 where
  oeisIx n = sum (tabl @59922 !! n)
instance OEIS 59732 where
  oeisIx n = (tabl @59922) !! (2*n) !! n
instance OEIS 59733 where
  oeisIx n = (tabl @59922) !! n !! (n `div` 2)

instance OEIS 59841 where
  oeisIx = (1 -) . (`mod` 2)
  oeis = cycle [1,0]

instance OEIS 59893 where
  oeisIx = foldl (\v b -> v * 2 + b) 1 . init . (rowT @30308) . succ

instance OEIS 59922 where
  oeis = tablList @59922
instance Table 59922 where
  tabl = iterate (\rs ->
     zipWith (+) (0 : reverse (0 : replicate (length rs - 1) 1))
                 $ zipWith (*) ([1] ++ rs) (rs ++ [1])) [1]


instance OEIS 59942 where
  oeis = map (foldr (\d v -> v * 2 + d) 0) $ f (tabf @30341) where
     f (xs:xss)
       | 0 `elem` xs = f xss
       | otherwise = map (fi . fromEnum) (zipWith (==)
                     (tail $ inits xs) (reverse $ init $ tails xs)) : f xss

instance OEIS 59943 where
  oeisIx = (* 2) . (oeisIx @59942)

instance OEIS 60000 where
  oeis = 1 : 2 : f 1 2 2 [] where
     f x y m []     = z : f y z z [m+1..z - 1] where z = x + y
     f x y m (h:hs) = h : f y h m hs

instance OEIS 60030 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v ws = y : f v y (delete y ws) where
       y = if null xs then u + v else last xs
       xs = takeWhile (< v) ws

instance OEIS 60036 where
  oeis = tablList @60036
instance Table 60036 where
  rowCol = rowCol_off @60036 @2 @1
  rowT = rowT_off @60036 @2
  tabl = map init $ tail (tabl @48152)

instance OEIS 60142 where
  oeis = 0 : f (S.singleton 1) where
     f s = x : f (S.insert (4 * x) $ S.insert (2 * x + 1) s') where
         (x, s') = S.deleteFindMin s

instance OEIS 60144 where
  oeis = 0 : 0 : scanl1 (+) (oeis @3849)

instance OEIS 60264 where
  oeisIx = (oeisIx @151800) . (* 2)

instance OEIS 60265 where
  oeisIx = (oeisIx @7917) . (* 2) . succ

instance OEIS 60278 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ filter ((== 0) . (oeisIx @10051.pred)) $ tail $ (rowT @27751) n

instance OEIS 60324 where
  oeisIx (succ->n) = head [q | q <- (oeis @40), (oeisIx @10051.pred) (n * (q + 1) - 1) == 1]

instance OEIS 60384 where
  oeisIx = (oeisIx @55642) . (oeisIx @45)

instance OEIS 60431 where
  oeis = scanl1 (+) (oeis @212793)

instance OEIS 60441 where
  oeis = tablList @60441
instance Table 60441 where
  rowCol = rowCol_off @60441 @1 @1
  rowT   = rowT_off @60441 @1
  tabf = [0] : [1] : [1] : map (rowT @27746) (drop 3 (oeis @45))

instance OEIS 60442 where
  oeis = tablList @60442
instance Table 60442 where
  tabf = [0] : [1] : [1] : map (rowT @27748) (drop 3 (oeis @45))

instance OEIS 60445 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ takeWhile (>= n') $ (rowT @70165) n'
              where n' = 2 * n + 1

instance OEIS 60448 where
  oeisIx (succ->n) = genericLength [us | let ds = (rowT @27750) n,
                           us <- init $ tail $ subsequences ds,
                           let vs = ds \\ us, head us < head vs,
                           product us `mod` product vs == 0] + 1

instance OEIS 60476 where
  oeis = filter ((== 0) . (oeisIx @10051.pred) . (+ 1) . (oeisIx @51903).pred) [1..]

instance OEIS 60547 where
  oeisIx = (2 ^) . (oeisIx @8611)
  oeis = f [2,1,2] where f xs = xs ++ f (map (* 2) xs)

instance OEIS 60640 where
  oeisIx (succ->n) = sum [d * (oeisIx @5.pred) d | d <- (rowT @27750) n]

instance OEIS 60715 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051.pred) [n+1..2*n - 1]

instance OEIS 60747 where
  oeisIx = subtract 1 . (* 2)
  oeis = [-1, 1 ..]

instance OEIS 60819 where
  oeisIx (succ->n) = n `div` (oeisIx @109008) n

instance OEIS 60945 where
  oeis' (A r) = 1 : 1 : 2 : 3 : 6 : zipWith (+)
    (tail r) (zipWith (+) (drop 3 r) (drop 4 r))

instance OEIS 60984 where
  oeis = iterate (\x -> x + (oeisIx @48760) x) 1

instance OEIS 60985 where
  oeis = iterate (oeisIx @61885) 1

instance OEIS 61019 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map negate $ (rowT @27746) n

instance OEIS 61020 where
  oeisIx = sum . map (oeisIx @61019.pred) . (rowT @27750) . succ

instance OEIS 61021 where
  oeis = 3 : 3 : 3 : zipWith (-)
    (tail $ zipWith (*) (tail (oeis @61021)) (oeis @61021)) (oeis @61021)



instance OEIS 61035 where
  oeis = tablList @61035
instance Table 61035 where
  rowCol = rowCol_off @61035 @1 @1
  rowT = map numerator . balmer where
     balmer n = map (subtract (1 % n ^ 2) . (1 %) . (^ 2)) [n, n - 1 .. 1]
  tabl = map (rowT @61035) [1..]

instance OEIS 61036 where
  oeis = tablList @61036
instance Table 61036 where
  rowCol = rowCol_off @61036 @1 @1
  rowT = map denominator . balmer where
     balmer n = map (subtract (1 % n ^ 2) . (1 %) . (^ 2)) [n, n - 1 .. 1]
  tabl = map (rowT @61036) [1..]

instance OEIS 61083 where
  oeis = 1 : 2 : zipWith divIgnPnt (oeis @61083) (tail (oeis @61083))
     where divIgnPnt x y = ddiv (10 * m) x' where
              ddiv u w | r == 0    = 10 * w + q
                       | otherwise = ddiv (10 * r) (10 * w + q)
                       where (q,r) = divMod u y
              (x',m) = divMod x y

instance OEIS 61084 where
  oeis = 1 : 2 : zipWith (-) (oeis @61084) (tail (oeis @61084))

instance OEIS 61168 where
  oeis = zipTail (+) . concat $ transpose [oeis @1855, oeis @1855]

instance OEIS 61217 where
  oeis = scanl1 (+) $ map (oeisIx @55641) [1..]

instance OEIS 61228 where
  oeisIx n = 1 + n + (oeisIx @20639) n

instance OEIS 61259 where
  oeisIx (succ->n) = sum $ zipWith (*) divs $ map (oeisIx @41) divs
              where divs = (rowT @27750) n

instance OEIS 61265 where
  oeis = map sum $
     zipWith (\u v -> map (oeisIx @10052) [u..v]) (oeis @40) $ tail (oeis @40)

instance OEIS 61292 where
  oeis = 2 : 2 : 2 : 2 : zipWith (-)
     (zipWith3 (((*) .) . (*)) (drop 2 xs) (tail xs) xs) (oeis @61292)
     where xs = tail (oeis @61292)

instance OEIS 61313 where
  oeisIx (succ->n) = fst $ until ((== 1) . snd) (\ (u, v) -> (u + 1, f v)) (0, n)
     where f n = if r == 0 then n' else n + 1  where (n', r) = divMod n 2

instance OEIS 61357 where
  oeisIx (succ->n) = sum $
     zipWith (\u v -> (oeisIx @10051.pred) u * (oeisIx @10051.pred) v) [n+1..] $ reverse [1..n - 1]

instance OEIS 61397 where
  oeisIx (succ->n) = ((oeisIx @10051 . pred) n) * n

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

instance OEIS 61470 where
  oeis = elemIndices 1 (oeis @225693)

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

instance OEIS 61561 where
  oeis = iterate (oeisIx @55944) 22

instance OEIS 61646 where
  oeis = 1 : 1 : 1 : zipWith (-) (map (* 2)
     (zipWith (+) (drop 2 (oeis @61646)) (tail (oeis @61646)))) (oeis @61646)

instance OEIS 61673 where
  oeis = filter bothComp [4,6..] where
     bothComp n = (1 - (oeisIx @10051.pred) (n - 1)) * (1 - (oeisIx @10051.pred) (n+1)) > 0

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

instance OEIS 61883 where
  oeis = 1 : zipWith (-) (tail (oeis @60985)) (oeis @60985)

instance OEIS 61885 where
  oeisIx n = n + (oeisIx @57944) n

instance OEIS 61886 where
  oeis = 1 : zipWith (-) (tail (oeis @60984)) (oeis @60984)

instance OEIS 61917 where
  oeis = filter chi [0..] where
     chi x = zs == reverse zs where
        zs = dropWhile (== '0') $ reverse $ show $ fi x

instance OEIS 62028 where
  oeisIx n = oeisIx @7953 n + n

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

instance OEIS 62113 where
  oeis = 1 : 2 : zipWith (+)
     (tail (oeis @62113)) (zipWith (*) (oeis @34) (oeis @62113))

instance OEIS 62178 where
  oeis = scanl (+) 0 (oeis @2083)

instance OEIS 62234 where
  oeis = zipWith (-) (map (* 2) (oeis @40)) (tail (oeis @40))

instance OEIS 62249 where
  oeisIx n = succ $ (oeisIx @5) n + n

instance OEIS 62296 where
  oeisIx = sum . map ((1 -) . signum) . (rowT @83093)

instance OEIS 62298 where
  oeis = scanl1 (+) $ map (1 -) (oeis @10051)

instance OEIS 62323 where
  oeis = tablList @62323
instance Table 62323 where
  tabl = map fst $ iterate f ([1], [0,1]) where
     f (us, vs) = (vs, ws) where
       ws = (zipWith (+) (us ++ [0]) (map (* v) vs)) ++ [1]
            where v = last (init vs) + 1

instance OEIS 62326 where
  oeisIx = (oeisIx @40) . pred . (oeisIx @137291)
  oeis = map (oeisIx @40) $
                 elemIndices 1 $ map (oeisIx @10051.pred) $ (oeis @49001)

instance OEIS 62383 where
  oeis = 1 : zs where
     zs = 2 : (map (* 2) $ concat $ transpose [zs, zs])

instance OEIS 62509 where
  oeisIx (succ->n) = n ^ (oeisIx @1221 . pred) n

instance OEIS 62682 where
  oeis = f (S.singleton (1 + 2^3, (1, 2))) 0 0 where
     f s z z' = if y == z && z' /= z then y : f s'' y z else f s'' y z
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 62723 where
  oeis = scanl1 lcm (oeis @792)

instance OEIS 62789 where
  oeisIx (succ->n) = gcd n (phi * (phi + 1)) where phi = (oeisIx @10.pred) n

instance OEIS 62799 where
  oeisIx = sum . map (oeisIx @1221 . pred) . (rowT @27750) . succ

instance OEIS 62822 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (+ 1) $ (rowT @265668) n

instance OEIS 62825 where
  oeisIx 0 = 0
  oeisIx n = sum $ (rowT @163870) n

instance OEIS 62880 where
  oeis = filter f [0..] where
     f 0 = True
     f x = (m == 0 || m == 2) && f x'  where (x', m) = divMod x 4

instance OEIS 62974 where
  oeis = map (+ 1) $ findIndices (< 0) $
     zipWith (-) (tail (oeis @1221)) $ map (* 2) (oeis @1221)

instance OEIS 63007 where
  oeis = tablList @63007
instance Table 63007 where
  tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @46899)

instance OEIS 63433 where
  oeis = iterate (oeisIx @56964) 10577

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

instance OEIS 63662 where
  oeis = iterate (oeisIx @63660) 0

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


instance OEIS 63908 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (subtract 3) . (* 2)) (oeis @40)

instance OEIS 63967 where
  oeis = tablList @63967
instance Table 63967 where
  tabl = [1] : [1,1] : f [1] [1,1] where
     f us vs = ws : f vs ws where
       ws = zipWith (+) ([0] ++ us ++ [0]) $
            zipWith (+) (us ++ [0,0]) $ zipWith (+) ([0] ++ vs) (vs ++ [0])

instance OEIS 63982 where
  oeis = f [] $ tail (oeis @225) where
     f us (v:vs) = (length ds) : f (v:us) vs where
       ds = [d | d <- (rowT @27750) v, all ((== 1). (gcd d)) us]

instance OEIS 63995 where
  oeis = tablList @63995
instance Table 63995 where
  rowCol n k = (tabf @63995) !! (n - 1) !! (n- 1+k)
  rowT   = rowT_off @63995 @1
  tabf = [[1], [1, 0, 1]] ++ (map
     (\rs -> [1, 0] ++ (init $ tail $ rs) ++ [0, 1]) $ drop 2 $ map
     (map length . group . sort . map rank) $ tail pss) where
        rank ps = maximum ps - length ps
        pss = [] : map (\u -> [u] : [v : ps | v <- [1..u],
                               ps <- pss !! (u - v), v <= head ps]) [1..]

instance OEIS 64222 where
  oeis = iterate (oeisIx @4186 . (+ 1)) 0

instance OEIS 64223 where
  oeis = iterate (\x -> x + ((oeisIx @55642) x)) 1

instance OEIS 64235 where
  oeis = 1 : zs where
     zs = 3 : 3 : (map (* 3) $ concat $ transpose [zs, zs, zs])

instance OEIS 64364 where
  oeis = tablList @64364
instance Table 64364 where
  rowCol = rowCol_off @64364 @1 @1
  rowT   = rowT_off @64364 @1
  tabf = [1] : tail (f 1 [] 1 (map (oeisIx @792) [2..])) where
     f k pqs v (w:ws) = (map snd pqs') :
       f (k + 1) (O.union pqs'' (zip (map (oeisIx @1414.pred) us) us )) w ws where
         us = [v + 1 .. w]
         (pqs', pqs'') = Data.List.partition ((== k) . fst) pqs

instance OEIS 64365 where
  oeis = 0 : f 0 (oeis @40) (S.singleton 0) where
     f x (p:ps) s | x' > 0 && x' `S.notMember` s = x' : f x' ps (S.insert x' s)
                  | otherwise                  = xp : f xp ps (S.insert xp s)
                  where x' = x - p; xp = x + p

instance OEIS 64413 where
  oeis = 1 : ekg 2 [2..] where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 64415 where
  oeisIx 0 = 0
  oeisIx (succ->n) = (oeisIx @3434 . pred) n - n `mod` 2

instance OEIS 64417 where
  oeis = 1 : 2 : 3 : f 3 [4..] where
     f x us = x' : f x' (delete x' us) where
        x' = head [u | u <- us, gcd u x > 2]

instance OEIS 64419 where
  oeis = [1,2,3,4,5] ++ f 5 [] [6..] where
     f z xs (y:ys) | y `gcd` z > 4 = y : f y [] (reverse xs ++ ys)
                   | otherwise     = f z (y:xs) ys

instance OEIS 64426 where
  oeis = zipWith (-) (tail (oeis @64413)) (oeis @64413)

instance OEIS 64437 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = if x `elem` zs then z + 3 else z + 2

instance OEIS 64455 where
  oeisIx (succ->n) = n + if m == 0 then n' else - n'  where (n',m) = divMod n 2
  oeis = concat $ transpose [[1 ..], [3, 6 ..]]

instance OEIS 64476 where
  oeis = filter (\x -> (oeisIx @3959 . pred) x `mod` x == 0) [1..]

instance OEIS 64478 where
  oeisIx n = if n <= 1 then n + 1 else (oeisIx @3959 . pred) n

instance OEIS 64491 where
  oeis = 1 : map succ do iterate (pred . oeisIx @62249) 1

instance OEIS 64547 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ (rowT @213925 . succ) n

instance OEIS 64550 where
  oeis = 1 : 2 : zipWith3 (\a q n -> a + 2 * q - n)
      (tail (oeis @64550)) (drop 2 (oeis @5185)) [2..]

instance OEIS 64551 where
  oeis = 1 : zipWith (+) (oeis @64551)
                     (map (* 2) $ zipWith (-) (drop 2 (oeis @45)) [1..])

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

instance OEIS 64664 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @64413)) . succ

instance OEIS 64736 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v (w:ws) = u' : w : f u' w (delete u' ws) where u' = v * w

instance OEIS 64801 where
  oeis = f 1 [1..] where
     f k xs = us ++ f (k + 1) (drop (k + 1) vs)
              where (us, vs) = splitAt k xs

instance OEIS 64806 where
  oeisIx n = 1 + n + (oeisIx @10888 . succ) n

instance OEIS 64847 where
  oeis = 1 : f [1,1] where
     f xs'@ (x:xs) = y : f (y : xs') where y = x * sum xs

instance OEIS 64861 where
  oeis = tablList @64861
instance Table 64861 where
  tabl = map fst $ iterate f ([1], 2) where
    f (xs, z) = (zipWith (+) ([0] ++ map (* z) xs) (xs ++ [0]), 3 - z)

instance OEIS 64911 where
  oeisIx = oeisIx @10051 . pred . oeisIx @32742

instance OEIS 64924 where
  oeis = concat $ zipWith (\p g -> genericTake g [p, 2 * p ..])
     (oeis @40) $ zipWith (-) (tail (oeis @40)) (oeis @40)

instance OEIS 64944 where
  oeisIx = sum . zipWith (*) [1..] . (rowT @27750) . succ

instance OEIS 64945 where
  oeisIx = sum . zipWith (*) [1..] . reverse . (rowT @27750) . succ

instance OEIS 64953 where
  oeis = map (+ 1) $ findIndices even (oeis @64413)

instance OEIS 64955 where
  oeis = map ((+ 1) . fromJust . (`elemIndex` (oeis @64413))) (oeis @40)

instance OEIS 64957 where
  oeis = map (+ 1) $ findIndices odd (oeis @64413)

instance OEIS 65091 where
  oeis = tail (oeis @40)

instance OEIS 65094 where
  oeis = 1 : f 1 1 1 where
     f k s x = y : f (k + 1) (s + y) y where y = x + div s k

instance OEIS 65109 where
  oeis = tablList @65109
instance Table 65109 where
  tabl = iterate
     (\row -> zipWith (-) (map (* 2) row ++ [0]) ([0] ++ row)) [1]

instance OEIS 65220 where
  oeis = zipWith (-) (oeis @45) [0..]

instance OEIS 65305 where
  oeis = tablList @65305
instance Table 65305 where
  rowCol = rowCol_off @65305 @2 @1
  rowT = rowT_off @65305 @2
  tabl = zipWith (map . (flip div 2 .) . (+))
                         (oeis @65091) $ tail $ inits (oeis @65091)

instance OEIS 65342 where
  oeis = tablList @65342
instance Table 65342 where
  rowCol = rowCol_off @65342 @1 @1
  rowT   = rowT_off   @65342 @1
  tabl = zipWith (map . (+)) (oeis @40) $ tail $ inits (oeis @40)

instance OEIS 65380 where
  oeis = filter f (tail $ oeis @40) where
     f p = any ((== 1) . (oeisIx @10051) . (p -)) $ takeWhile (<= p) (oeis @79)

instance OEIS 65381 where
  oeis = 2 : filter f (tail $ oeis @40) where
     f p = all ((== 0) . (oeisIx @10051.pred) . (p -)) $ takeWhile (<= p) (oeis @79)

instance OEIS 65422 where
  oeis = 1 : 1 : f 2 1 where
     f n x = x' : f (n+1) x' where
         x' | x `mod` n == 0 = until ((> 0) . (`mod` n)) (`div` n) x
            | otherwise      = x * n

instance OEIS 65620 where
  oeis = 0 : fix \(map(*2)->zs) ->
    1 : concat (transpose [zs, map ((+ 1) . negate) zs])

instance OEIS 65855 where
  oeis = scanl1 (+) (map (oeisIx @66247) [0..])

instance OEIS 65896 where
  oeisIx = (oeisIx @65855 . pred) . (* 2) . succ

instance OEIS 65941 where
  oeis = tablList @65941
instance Table 65941 where
  tabl = iterate (\row ->
     zipWith (+) ([0] ++ row) (zipWith (*) (row ++ [0]) (oeis @59841))) [1]

instance OEIS 66028 where
  oeisIx = maximum . filter ((== 1) . (oeisIx @10051.pred)) .
                      map sum . tail . subsequences . flip take (oeis @40)
                      .succ

instance OEIS 66032 where
  oeis = tablList @66032
instance Table 66032 where
  rowCol 1 1 = 1
  rowCol n k = fi (fromEnum (n <= k)) +
     (sum $ map (\d -> (rowCol @66032) (n `div` d) d) $
                takeWhile (<= k) $ tail $ (rowT @27751) n)
  rowT n = map (rowCol @66032 n) [1..n]
  tabl = map (rowT @66032) [1..]

instance Table 66099 where
  tabf = map (rowT @66099) [1..]
  rowT n = reverse $ (rowT @228351) n
instance OEIS 66099 where
  oeis = tablList @66099

instance OEIS 66339 where
  oeis = (0:) . scanl1 (+) $ map (oeisIx @79260) [1..]

instance OEIS 66459 where
  oeisIx = product . map (oeisIx @142) .
     unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 10)

instance OEIS 66484 where
  oeis = filter h [1..] where
     h x = notElem '0' xs && length (nub xs) > 1 &&
           all d (map read $ zipWith (++)
                 (tail $ tails xs) (tail $ inits xs)) where xs = show (fi x)
     d u = g u where
           g v = v == 0 || mod u d == 0 && g v' where (v', d) = divMod v 10

instance OEIS 66490 where
  oeis = scanl1 (+) $ map (oeisIx @79261) [0..]

instance OEIS 66520 where
  oeis = scanl1 (+) $ map (negate . (oeisIx @151763)) [0..]

instance OEIS 66522 where
  oeis = filter f [1..] where
     f x = genericLength ds == maximum ds where ds = (rowT @161906) x

instance OEIS 66571 where
  oeisIx (succ->n) = f [1..] 1 n 0 where
     f (k:ks) l nl x
       | y > nl  = 0
       | y < nl  = f ks (l + 1) (nl + n) y + f ks l nl x
       | otherwise = if y `mod` l == 0 then 1 else 0
       where y = x + k

instance OEIS 66664 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) $ tail (oeis @66522)

instance OEIS 66680 where
  oeis = s [2..] where
     s (b:bs) = b : s [x | x <- bs, x > b ^ 2 || mod x b > 0]

instance OEIS 66681 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @66680)

instance OEIS 66710 where
  oeis = iterate (oeisIx @36839) 3

instance OEIS 66711 where
  oeis = iterate (oeisIx @36839) 9

instance OEIS 66713 where
  oeisIx = (oeisIx @36839) . (2 ^)

instance OEIS 66720 where
  oeis = f [] 1 S.empty where
     f ps z s | S.null s' = f ps (z + 1) s
              | otherwise   = z : f (z:ps) (z + 1) s'
       where s' = g (z:ps) s
             g []     s                      = s
             g (x:qs) s | (z * x) `S.member` s = S.empty
                        | otherwise          = g qs $ S.insert (z * x) s

instance OEIS 66721 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @66720)

instance OEIS 66829 where
  oeisIx = (`mod` 2) . (oeisIx @1222)

instance OEIS 66839 where
  oeisIx = sum . (rowT @161906) . succ

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

instance OEIS 67240 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ map (oeisIx @10 . pred) $ (rowT @141809) $ n

instance OEIS 67255 where
  oeis = tablList @67255
instance Table 67255 where
  rowCol = rowCol_off @67255 @1 @1
  rowT 1 = [0]
  rowT n = f n (oeis @40) where
     f 1 _      = []
     f u (p:ps) = g u 0 where
       g v e = if m == 0 then g v' (e + 1) else e : f v ps
               where (v',m) = divMod v p
  tabf = map (rowT @67255) [1..]

instance OEIS 67266 where
  oeis = filter (\x -> (oeisIx @1221 . pred) x == (oeisIx @2321 . pred) x) [1..]

instance OEIS 67434 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @984) . succ

instance OEIS 67611 where
  oeis = map (`div` 6) $
     filter (\x -> (oeisIx @10051.pred) (x - 1) == 0 || (oeisIx @10051.pred) (x+1) == 0) [6,12..]

instance OEIS 67898 where
  oeisIx n = f n [0..10] where
     f x ys | x <= 9    = head $ delete x ys
            | otherwise = f x' $ delete d ys where (x',d) = divMod x 10

instance OEIS 67998 where
  oeisIx n = n * (n - 2)
  oeis = scanl (+) 0 [-1, 1 ..]

instance OEIS 68050 where
  oeisIx (succ->n) = genericLength [k | k <- [1..n], (oeisIx @10051.pred) (n `div` k) == 1]

instance OEIS 68106 where
  oeis = tablList @68106
instance Table 68106 where
  tabl = map reverse (tabl @47920)

instance OEIS 68336 where
  oeis = 1 : f 1 where
     f x = (1 + sum (map (oeisIx @68336.pred) $ (rowT @27750) x)) : f (x + 1)

instance OEIS 68340 where
  oeis = scanl1 (+) (oeis @55615)

instance OEIS 68700 where
  oeis = O.isect (oeis @30457) (oeis @54211)

instance OEIS 68872 where
  oeis = filter
     (all (== 1) . map (`mod` 10) . (rowT @27750)) (oeis @2808)

instance OEIS 69011 where
  oeis = tablList @69011
instance Table 69011 where
  tabl = map snd $ iterate f (1, [0]) where
     f (i, xs@ (x:_)) = (i + 2, (x + i) : zipWith (+) xs [i + 1, i + 3 ..])

instance OEIS 69106 where
  oeis = [x | x <- tail (oeis @2808), (oeisIx @45) (x - 1) `mod` x == 0]

instance OEIS 69107 where
  oeis = h 2 $ drop 3 (oeis @45) where
     h n (fib:fibs)
      | fib `mod` n > 0 || (oeisIx @10051.pred) n == 1
      = h (n+1) fibs
      | let = n : h (n+1) fibs

instance OEIS 69489 where
  oeis = filter g $ dropWhile (<= 1000) (oeis @40) where
     g x = x < 100 || (oeisIx @10051.pred) (x `mod` 1000) == 1 && g (x `div` 10)

instance OEIS 69490 where
  oeis = f $ S.fromList [1..9] where
     f s | m < 1000               = f s''
         | h m && (oeisIx @10051.pred) m == 1 = m : f s''
         | otherwise              = f s''
         where s'' = S.union s' $ S.fromList $ map (+ (m * 10)) [1, 3, 7, 9]
               (m, s') = S.deleteFindMin s
     h x = x < 100 && (oeisIx @10051.pred) x == 1 ||
           (oeisIx @10051.pred) (x `mod` 1000) == 1 &&
           (oeisIx @10051.pred) (x `mod` 100) == 1 && h (x `div` 10)

instance OEIS 69492 where
  oeis = 1 : f (S.singleton z) [1, z] zs where
     f s q5s p5s'@ (p5:p5s)
       | m < p5 = m : f (S.union (S.fromList $ map (* m) ps) s') q5s p5s'
       | otherwise = f (S.union (S.fromList $ map (* p5) q5s) s) (p5:q5s) p5s
       where ps = (rowT @27748) m
             (m, s') = S.deleteFindMin s
     (z:zs) = (oeis @50997)

instance OEIS 69493 where
  oeis = 1 : f (S.singleton z) [1, z] zs where
     f s q6s p6s'@ (p6:p6s)
       | m < p6 = m : f (S.union (S.fromList $ map (* m) ps) s') q6s p6s'
       | otherwise = f (S.union (S.fromList $ map (* p6) q6s) s) (p6:q6s) p6s
       where ps = (rowT @27748) m
             (m, s') = S.deleteFindMin s
     (z:zs) = (oeis @30516)

instance OEIS 69754 where
  oeisIx 0 = 0
  oeisIx 1 = 1
  oeisIx n = 2 * (oeisIx @720) n - 2 - ((oeisIx @10051) n)

instance OEIS 69817 where
  oeisIx = a . succ where
    a 1 = 1
    a n = if null ms then n else minimum $ map (`mod` n) ms
      where ms = zipWith (+) ds $ map (div n') ds
            ds = takeWhile (< n - 1) $ tail $ (rowT @27750) n'
            n' = n ^ 2 - 1

instance OEIS 70073 where
  oeisIx (succ->n) = genericLength [ () | x <- [1..n], y <- [1..x], z <- [1..y],
                           (oeisIx @212793.pred) (x*y*z) == 1]

instance OEIS 70165 where
  oeis = tablList @70165
instance Table 70165 where
  rowCol = rowCol_off @70165 @1 @1
  rowT n = (takeWhile (/= 1) $ iterate (oeisIx @6370) n) ++ [1]
  tabf = map (rowT @70165) [1..]

instance OEIS 70167 where
  oeisIx (succ->n) = fromJust (findIndex (elem n) (tabf @70165)) + 1

instance OEIS 70229 where
  oeisIx n = 1 + n + (oeisIx @6530) n

instance OEIS 70319 where
  oeis = (1:) . scanl1 max $ map (oeisIx @5) [1..]

instance OEIS 70870 where
  oeis = 6 : f 6 where
     f x = y : f y where
       y = (if even x then 5 * x else x + 1) `div` 2

instance OEIS 70885 where
  oeis = 1 : map (flip (*) 3 . flip div 2 . (+ 1)) (oeis @70885)

instance OEIS 70887 where
  oeis = tablList @70887
instance Table 70887 where
  rowCol = rowCol_off @70887 @1 @1
  rowT   = rowT_off   @70887 @1
  tabl = zipWith take [1..] (tabf @75437)

instance OEIS 70897 where
  oeisIx (succ->n) = genericLength $ filter (all ((== 1) . (oeisIx @10051.pred)))
                       $ map (zipWith (+) [1..n]) (permutations [n+1..2*n])

instance OEIS 70950 where
  oeis = tablList @70950
instance Table 70950 where
  tabf = iterate rule30 [1] where
     rule30 row = f ([0,0] ++ row ++ [0,0]) where
         f [_,_]          = []
         f (u:ws@ (0:0:_)) = u : f ws
         f (u:ws)         = (1 - u) : f ws

instance OEIS 70951 where
  oeisIx = genericLength . filter (== 0) . (rowT @70950)

instance OEIS 70952 where
  oeisIx = sum . (rowT @70950)

instance OEIS 70991 where
  oeis = filter (\x -> (x - 1) `elem` (rowT @70165) x) [1..]

instance OEIS 71032 where
  oeis = tablList @71032
instance Table 71032 where
  tabf = map reverse (tabf @70950)

instance OEIS 71317 where
  oeis = scanl1 (+) (oeis @4159)

instance OEIS 71318 where
  oeis = [x | x <- [1..],  (oeisIx @212793.pred) x == 1, (oeisIx @8966.pred) x == 0,
                      let y = x+1, (oeisIx @212793.pred) y == 1, (oeisIx @8966.pred) y == 0]

instance OEIS 71367 where
  oeis = filter f [211..] where
     f x = and $ map g [5, 4 .. 1] where
       g k = sum (map h $ map (+ x) [0..4]) == 1 where
         h z = if r == 0 then (oeisIx @10051.pred) z' else 0
               where (z', r) = divMod z k

instance OEIS 71368 where
  oeis = filter f [18362..] where
     f x = and $ map g [6, 5 .. 1] where
       g k = sum (map h $ map (+ x) [0..5]) == 1 where
         h z = if r == 0 then (oeisIx @10051.pred) z' else 0
               where (z', r) = divMod z k

instance OEIS 71407 where
  oeisIx n
    = head [ k | k <- [2,4..]
           , let x = k * (oeisIx @40) n
           , (oeisIx @10051.pred) (x - 1) == 1
           , (oeisIx @10051.pred) (x + 1) == 1]

instance OEIS 71558 where
  oeisIx (succ->n)
    = head [ k | k <- [1..]
           , let x = k * n
           , (oeisIx @10051.pred) (x - 1) == 1
           , (oeisIx @10051.pred) (x + 1) == 1]

instance OEIS 71695 where
  oeis = [p | p <- (oeis @2144), (oeisIx @10051.pred) (p + 2) == 1]

instance OEIS 71696 where
  oeis = [p | p <- tail (oeis @2145), (oeisIx @10051.pred) (p - 2) == 1]

instance OEIS 71698 where
  oeis = [x | x <- [3, 7 ..], (oeisIx @10051.pred) x == 1, (oeisIx @10051.pred) (x+2) == 1]

instance OEIS 71699 where
  oeis = [x | x <- [5, 9 ..], (oeisIx @10051.pred) x == 1, (oeisIx @10051.pred) (x - 2) == 1]

instance OEIS 71700 where
  oeis = [x * y | x <- [3, 7 ..], (oeisIx @10051.pred) x == 1,
                          let y = x + 2, (oeisIx @10051.pred) y == 1]

instance OEIS 71797 where
  oeis = f $ tail $ inits [1..] where
     f (xs:_:xss) = xs ++ f xss

instance OEIS 71810 where
  oeisIx = sum . map (oeisIx @10051.pred) . map sum .
            tail . subsequences . flip take (oeis @40) . succ

instance OEIS 71977 where
  oeis = tablList @71977
instance Table 71977 where
  rowCol = rowCol_off @71977 @1 @1
  rowT   = rowT_off   @71977 @1
  tabl = f 1 [1..] where
     f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
       ys  = take k $ filter ((== 1) . (gcd k)) xs

instance OEIS 72007 where
  oeis = 0 : f 1 0 [1..] where
     f u v ws = g ws where
       g (x:xs) = if abs (x - v) < u
                     then g xs else x : f (u + 1) x (delete x ws)

instance OEIS 72047 where
  oeis = map (oeisIx @1221 . pred) $ (oeis @5117)

instance OEIS 72048 where
  oeisIx = (2 ^) . (oeisIx @72047)

instance OEIS 72055 where
  oeisIx = (+ 1) . (* 2) . (oeisIx @40)

instance OEIS 72057 where
  oeisIx = (oeisIx @203 . pred) . (oeisIx @72055)

instance OEIS 72103 where
  oeis = f 9 3 $ S.singleton (4,2) where
     f zz z s
       | xx < zz   = xx : f zz z (S.insert (x*xx, x) $ S.deleteMin s)
       | otherwise = zz : f (zz+2*z+1) (z+1) (S.insert (z*zz, z) s)
       where (xx, x) = S.findMin s

instance OEIS 72203 where
  oeis = scanl1 (\x y -> x + 2*y - 1) (oeis @66829)

instance OEIS 72214 where
  oeisIx = (oeisIx @41) . (oeisIx @45) . (+ 1)

instance OEIS 72292 where
  oeis = (0:) . scanl (+) 0 $ drop 2 (oeis @75802)

instance OEIS 72358 where
  oeis = scanl1 (+) $
     zipWith (*) (oeis @212793) $ map (1 -) (oeis @8966)

instance OEIS 72404 where
  oeis = map denominator $
    scanl1 (-) $ map ((1 %) . (oeisIx @244)) $ (oeis @29837)

instance OEIS 72437 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @5091)

instance OEIS 72486 where
  oeis = scanl1 (*) (oeis @20639)

instance OEIS 72499 where
  oeisIx = product . (rowT @161906) . succ

instance OEIS 72618 where
  oeis = filter f [1 ..] where
     f x = any (all ((== 1) . (oeisIx @10051).pred . fi)) $
           map cs [concat $ transpose [[2*x, 2*x - 2 .. 2] , us] |
                   us <- map (uncurry (++) . (uncurry $ flip (,))
                              . flip splitAt [1, 3 .. 2 * x]) [1 .. x]]
     cs zs = (head zs + last zs) : zipWith (+) zs (tail zs)

instance OEIS 72627 where
  oeisIx = genericLength . filter ((== 1) . (oeisIx @10051.pred) . (subtract 1)) . (rowT @27749)

instance OEIS 72649 where
  oeis = f 1 where
     f n = (replicate ((oeisIx @45) n) n) ++ f (n+1)

instance OEIS 72701 where
  oeisIx (succ->n) = f (oeis @40) 1 n 0 where
     f (p:ps) l nl x
       | y > nl    = 0
       | y < nl    = f ps (l + 1) (nl + n) y + f ps l nl x
       | otherwise = if y `mod` l == 0 then 1 else 0
       where y = x + p

instance OEIS 72750 where
  oeis = scanl1 (+) $ map ((0 ^) . (`mod` 7)) (oeis @5117)

instance OEIS 72762 where
  oeisIx (succ->n) = foldl (\v d -> 2*v + d) 0 $ map (oeisIx @10051.pred) [1..n]

instance OEIS 72777 where
  oeis = f 9 (drop 2 (oeis @5117)) (M.singleton 4 (2, 2)) where
     f vv vs'@ (v:ws@ (w:_)) m
      | xx < vv = xx : f vv vs' (M.insert (bx*xx) (bx, ex+1) $ M.deleteMin m)
      | xx > vv = vv : f (w*w) ws (M.insert (v^3) (v, 3) m)
      where (xx, (bx, ex)) = M.findMin m

instance OEIS 72823 where
  oeis = tail $ elemIndices 0 (oeis @73267)

instance OEIS 72873 where
  oeis = 1 : h S.empty [1] (oeis @51674) where
     h s mcs xs'@ (x:xs)
      | S.null s || x < m = h (s `S.union` S.fromList (map (* x) mcs)) mcs xs
      | otherwise = m : h (s' `S.union` S.fromList (map (* m) $ init (m:mcs)))
                          (m:mcs) xs'
      where (m, s') = S.deleteFindMin s

instance OEIS 72979 where
  oeis = 1 : f 2 [1] where
     f z xs = y : f (z + 1) (y : xs) where
       y = sum $ zipWith (*) xs (map (gcd z) [z - 1, z - 2 ..])

instance OEIS 73093 where
  oeisIx = genericLength . rowT @210208 . succ

instance OEIS 73180 where
  oeisIx (succ->n) = genericLength [x | x <- (rowT @27750) n, x <= (oeisIx @7947.pred) n]

instance OEIS 73184 where
  oeisIx = sum . map (oeisIx @212793.pred) . (rowT @27750).succ

instance OEIS 73185 where
  oeisIx = sum . filter ((== 1) . (oeisIx @212793).pred) . (rowT @27750).succ

instance OEIS 73267 where
  oeisIx n = sum $ zipWith (*) (oeis @209229) $ reverse $ take n (oeis @36987)

instance OEIS 73364 where
  oeisIx (succ->n) = genericLength $ filter (all isprime)
                       $ map (zipWith (+) [1..n]) (permutations [1..n])
     where isprime = isPrime . fi

instance OEIS 73533 where
  oeis = f 1 3 1 where
     f n p3 x = numerator (y * fi p3) : f (n + 1) (p3 * 3) y
                where y = z - fi (floor z); z = 4%3 * x

instance OEIS 73579 where
  oeisIx n = p * (2 - p `mod` 4) where p = (oeisIx @40) n

instance OEIS 73703 where
  oeisIx (n) = head [p | p <- (oeis @40), (oeisIx @10051.pred) (p + 2 * (oeisIx @40) n) == 1]

instance OEIS 73707 where
  oeis = 1 : f 0 0 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ zipWith (*) hzs (reverse hzs) where hzs = drop x zs

instance OEIS 73734 where
  oeis = zipWith gcd (oeis @64413) $ tail (oeis @64413)

instance OEIS 73736 where
  oeis = scanl1 (+) (oeis @73737)

instance OEIS 73737 where
  oeis =
     1 : 1 : zipWith (-) (oeis @65091)
                         (zipWith (+) (oeis @73737) $ tail (oeis @73737))

instance OEIS 73941 where
  oeis = 1 : f [1] where
     f xs = x' : f (x':xs) where x' = (1 + sum xs) `div` 2

instance OEIS 74068 where
  oeis = 1 : 2 : 3 : xs where
     xs = 10 : 9 : 8 : 7 : 6 : 5 : 4 : map (+ 7) xs

instance OEIS 74294 where
  oeis = f $ inits [1..] where
     f (xs:_:xss) = xs ++ f xss

instance OEIS 74480 where
  oeis = multClosure (oeis @37074) where
    multClosure []     = [1]
    multClosure (b:bs) = 1:h [b] (S.singleton b) bs where
     h cs s []    = m:h (m:cs) (foldl (flip S.insert) s' $ map (*m) cs) []
      where (m, s') = S.deleteFindMin s
     h cs s xs'@ (x:xs)
      | m < x     = m:h (m:cs) (foldl (flip S.insert) s' $ map (*m) cs) xs'
      | otherwise = x:h (x:cs) (foldl (flip S.insert) s  $ map (*x) (x:cs)) xs
      where (m, s') = S.deleteFindMin s

instance OEIS 74583 where
  oeis = 1 : f (S.singleton 2) (oeis @40) where
    f s ps'@ (p:p':ps)
      | m == p      = p : f (S.insert (p*p) $ S.insert p' s') (p':ps)
      | m < spf^spf = m : f (S.insert (m*spf) s') ps'
      | otherwise   = m : f s' ps'
        where spf = (oeisIx @20639) m
              (m, s') = S.deleteFindMin s
  -- Simpler version:
  -- oeis = map (oeisIx @961) (oeis @192188)

instance OEIS 74695 where
  oeisIx (succ->n) = gcd n $ (oeisIx @48760) n

instance OEIS 74829 where
  oeis = tablList @74829
instance Table 74829 where
  rowCol = rowCol_off @74829 @1 @1
  rowT   = rowT_off   @74829 @1
  tabl = map fst $ iterate
     (\ (u:_, vs) -> (vs, zipWith (+) ([u] ++ vs) (vs ++ [u]))) ([1], [1,1])

instance OEIS 74909 where
  oeis = tablList @74909
instance Table 74909 where
  tabl = iterate
     (\row -> zipWith (+) ([0] ++ row) (row ++ [1])) [1]

instance OEIS 74911 where
  oeis = tablList @74911
instance Table 74911 where
  rowCol = rowCol_off @74911 @1 @1
  rowT   = rowT_off   @74911 @1
  tabl = map fst $ iterate
     (\ (vs, w:ws) -> (zipWith (+) ([w] ++ vs) (vs ++ [w]), ws))
     ([1], tail (oeis @1563))

instance OEIS 74940 where
  oeis = elemIndices 0 (oeis @39966)

instance OEIS 75075 where
  oeis = 1 : 2 : f 1 2 [3..]
    where
      f z z' xs = g xs
        where
          g (u:us) = if (z * u) `mod` z' > 0 then g us else u : f z' u (delete u xs)

instance OEIS 75177 where
  oeis = map succ $ elemIndices 1 $ map (oeisIx @10051.pred . oeisIx @7953) (oeis @40)

instance OEIS 75188 where
  oeis = f 1 [] where
     f x hs = (length $ filter ((== 1) . (oeisIx @10051).pred) (map numerator hs')) :
              f (x + 1) hs' where hs' = hs ++ map (+ recip x) (0 : hs)

instance OEIS 75189 where
  oeis = f 1 S.empty S.empty where
     f x s s1 = fi (S.size s1') : f (x + 1) (s `S.union` S.fromList hs) s1' where
       s1' = s1 `S.union` S.fromList
             (filter ((== 1) . (oeisIx @10051.pred)) $ map numerator hs)
       hs = map (+ 1 % x) $ 0 : S.toList s

instance OEIS 75193 where
  oeis = 1 : -3 : zipWith (-) (oeis @75193) (tail (oeis @75193))

instance OEIS 75226 where
  oeis = f 2 [recip 1] where
     f x hs = (maximum $ filter ((== 1) . (oeisIx @10051.pred)) (map numerator hs')) :
              f (x + 1) hs' where hs' = hs ++ map (+ recip x) hs

instance OEIS 75227 where
  oeis = f 1 S.empty (oeis @65091) where
     f x s ps = head qs : f (x + 1) (s `S.union` S.fromList hs) qs where
       qs = foldl (flip del)
            ps $ filter ((== 1) . (oeisIx @10051.pred)) $ map numerator hs
       hs = map (+ 1 % x) $ 0 : S.toList s
     del u vs'@ (v:vs) = case compare u v
                        of LT -> vs'; EQ -> vs; GT -> v : del u vs

instance OEIS 75323 where
  oeis = f 1 [] $ tail (oeis @40) where
     f k ys qs = g qs where
       g (p:ps) | (oeisIx @10051.pred) pk == 0 || pk `elem` ys = g ps
                | otherwise = p : pk : f (k + 1) (p:pk:ys) (qs \\ [p, pk])
                where pk = p + 2 * k

instance OEIS 75326 where
  oeis = 0 : f [1..] where
     f ws@ (u:v:_) = y : f (ws \\ [u, v, y]) where y = u + v

instance OEIS 75345 where
  oeisIx = sum . (rowT @75348) . succ

instance OEIS 75346 where
  oeisIx = head . (rowT @75348) . succ

instance OEIS 75347 where
  oeisIx = last . (rowT @75348) . succ

instance OEIS 75348 where
  oeis = tablList @75348
instance Table 75348 where
  rowCol = rowCol_off @75348 @1 @1
  rowT   = rowT_off   @75348 @1
  tabl = f 0 [1..] where
     f x zs = (us ++ [y]) : f (x + 1) (zs \\ (y : us)) where
       y = g vs
       g (w:ws) = if (oeisIx @10051.pred) (sum us + w) == 1 then w else g ws
       (us, vs) = splitAt x zs

instance OEIS 75362 where
  oeis = tablList @75362
instance Table 75362 where
  rowCol = rowCol_off @75362 @1 @1
  rowT   = rowT_off   @75362 @1
  tabl = zipWith (zipWith (*)) (tabl @2260) (tabl @2024)

instance OEIS 75366 where
  oeis = 1 : f 2 1 (oeis @40) where
     f x pp ps'@ (p:ps)
       | p <= x    = f x (p * pp) ps
       | otherwise = g $ dropWhile (< pp) $ scanl1 (*) [x+1, x+2 ..]
       where g (z:zs) | mod z pp == 0 = z : f (x + 1) pp ps'
                      | otherwise     = g zs

instance OEIS 75369 where
  oeisIx = (^ 2) . (oeisIx @14574)

instance OEIS 75383 where
  oeis = tablList @75383
instance Table 75383 where
  rowCol = rowCol_off @75383 @1 @1
  rowT   = rowT_off   @75383 @1
  tabl = f 1 [1..] where
     f x zs = ys : f (x + 1) (zs \\ ys) where
              ys = take x $ filter ((== 0) . (`mod` x)) zs

instance OEIS 75384 where
  oeisIx = head . (rowT @75383) . succ

instance OEIS 75386 where
  oeisIx = sum . (rowT @75383) . succ

instance OEIS 75387 where
  oeisIx = product . (rowT @75383) . succ

instance OEIS 75388 where
  oeisIx (succ->n) = (oeisIx @75384.pred) n `div` n

instance OEIS 75390 where
  oeisIx (succ->n) = (oeisIx @75386.pred) n `div` n

instance OEIS 75427 where
  oeis = 1 : f 1 1 where
     f x y = z : f (x + 1) z where z = (1 + x `mod` 2) * y + 1 - x `mod` 2

instance OEIS 75432 where
  oeis = [ p | p <- oeis @40
         , (oeisIx @8966.pred) (p - 1) == 0
         , (oeisIx @8966.pred) (p + 1) == 0 ]

instance OEIS 75437 where
  oeis = tablList @75437
instance Table 75437 where
  tabf = iterate rule110 [1] where
     rule110 row = f ([0,0] ++ row ++ [0,0]) where
         f [_,_]          = []
         f (_:ws@ (0:0:_)) = 0 : f ws
         f (1:ws@ (1:1:_)) = 0 : f ws
         f (_:ws@ (_:_:_)) = 1 : f ws

instance OEIS 75518 where
  oeisIx = (`div` 4) . (oeisIx @40)

instance OEIS 75520 where
  oeis = zipWith (+) (oeis @1749) (oeis @39702)

instance OEIS 75521 where
  oeis = map (oeisIx @40)
       $ filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @75520)) [1..]

instance OEIS 75524 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @75520)

instance OEIS 75540 where
  oeis = map fst $ filter ((== 0) . snd) $
     zipWith3 (\x y z -> divMod (x + y + z) 3)
              (oeis @40) (tail (oeis @40)) (drop 2 (oeis @40))

instance OEIS 75677 where
  oeisIx = (oeisIx @265) . pred . subtract 2 . (* 6) . succ

instance OEIS 75802 where
  oeisIx 0 = 1
  oeisIx n = signum $ (oeisIx @52409) n - 1

instance OEIS 76039 where
  oeis = f 1 1 where
     f n x = x' : f (n+1) x' where
             x' = (if x < n then (*) else div) x n

instance OEIS 76050 where
  oeis = 2 : f [2] where
     f xs = (drop (length xs) xs') ++ (f xs') where
       xs' = concatMap ((enumFromTo 2) . (+ 1)) xs

instance OEIS 76052 where
  oeis = scanl1 (+) $ map (oeisIx @6460) [0..]

instance OEIS 76132 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where y = sum $ zipWith (^) xs [1..]

instance OEIS 76271 where
  oeis = iterate (oeisIx @70229 . pred) 1

instance OEIS 76338 where
  oeisIx = (+ 1) . (* 512)
  oeis = [1,513..]

instance OEIS 76339 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) [1,513..]

instance OEIS 76398 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @25478)

instance OEIS 76468 where
  oeis = 1 : f [2..] S.empty where
     f xs'@ (x:xs) s | S.null s || m > x ^ 4 = f xs $ S.insert (x ^ 4, x) s
                    | m == x ^ 4  = f xs s
                    | otherwise = m : f xs' (S.insert (m * b, b) s')
                    where ((m, b), s') = S.deleteFindMin s

instance OEIS 76478 where
  oeis = concat $ tail $ map (tail . reverse . unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2 )) [1..]

instance OEIS 76479 where
  oeisIx = (oeisIx @8683) . pred . (oeisIx @7947)

instance OEIS 76489 where
  oeis = map (length . nub) $
                 zipWith intersect (tail (tabf @31298)) (tabf @31298)

instance OEIS 76644 where
  oeis = scanl1 (+) (oeis @122196)

instance OEIS 76805 where
  oeis = filter (not . ("13" `isInfixOf`) . show . fi) (oeis @40)

instance OEIS 76845 where
  oeisIx ((+2)->n) = head [ k | k <- [1..] , (isPrime . fi) (n ^ k + n - 1) ]

instance OEIS 76974 where
  oeis = 2 : s [3, 5 ..] where
     s (x:xs) = x : s [z | z <- xs, mod z x /= 2]

instance OEIS 77039 where
  oeis = scanl1 (+) (oeis @73579)

instance OEIS 77065 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (`div` 2) . succ) (oeis @6093)

instance OEIS 77068 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (`div` 2)) (oeis @8864)

instance OEIS 77477 where
  oeis = f [1..] where
     f (x:xs) = x : f (delete (2*x + 1) $ delete (3*x + 1) xs)



instance OEIS 77581 where
  oeis = tablList @77581
instance Table 77581 where
  rowCol = rowCol_off @77581 @1 @1
  rowT   = rowT_off   @77581 @1
  tabl = map (\x -> take x [z | z <- [1..], gcd x z == 1]) [1..]

instance OEIS 77610 where
  oeis = tablList @77610
instance Table 77610 where
  rowCol n k = (rowT @77610) n !! k
  rowT n = [d | d <- [1..n], let (n',m) = divMod n d, m == 0, gcd d n' == 1]
  tabf = map (rowT @77610) [1..]

instance OEIS 77664 where
  oeis = tablList @77664
instance Table 77664 where
  rowCol = rowCol_off @77664 @1 @1
  rowT   = rowT_off   @77664 @1
  tabl = map (\x -> take x $ filter ((== 1). gcd x) [x + 1 ..]) [1..]

instance OEIS 78125 where
  oeis = f [1] where
     f xs = (p' xs $ last xs) : f (1 : map (* 3) xs)
     p' = memo2 (list integral) integral p
     p _ 0 = 1; p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m

instance OEIS 78174 where
  oeis = filter (\x -> (oeisIx @8472 . pred) x `mod` (oeisIx @1221 . pred) x == 0) [2..]

instance OEIS 78178 where
  oeisIx ((+2)->n) = head [k | k <- [2..], (isPrime.fi) (n ^ k + n - 1)]

instance OEIS 78180 where
  oeis = 1 : f 1 1 where
     f x k = y : f y (k+1) where
       y = head [z | z <- [x+1..], all (q z) $ take k (oeis @78180)]
       q u v = m > 0 || (oeisIx @10051.pred) u' == 0 where (u',m) = divMod (u - 1) v

instance OEIS 78310 where
  oeisIx (succ->n) = n * (oeisIx @7947 $ pred n) + 1

instance OEIS 78313 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @78310)

instance OEIS 78324 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @224866)

instance OEIS 78325 where
  oeis = filter ((== 1) . (oeisIx @8966).pred) (oeis @224866)

instance OEIS 78358 where
  oeis = elemIndices 0 (oeis @5369)

instance OEIS 78408 where
  oeis = f 1 where
     f x = (p' 1 x) : f (x + 2)
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < k then 0 else p' k (m - k) + p' (k + 2) m

instance OEIS 78783 where
  oeis = oeis78783

instance OEIS 78812 where
  oeis = tablList @78812
instance Table 78812 where
  tabl = [1] : [2, 1] : f [1] [2, 1] where
     f us vs = ws : f vs ws where
       ws = zipWith (-) (zipWith (+) ([0] ++ vs) (map (* 2) vs ++ [0]))
                        (us ++ [0, 0])

instance OEIS 78898 where
  oeis = 0 : 1 : f M.empty 2 where
     f m x = y : f (M.insert p y m) (x + 1) where
             y = M.findWithDefault 0 p m + 1
             p = (oeisIx @20639.pred) x

instance OEIS 79053 where
  oeis = 1 : 2 : r (S.fromList [1,2]) 1 1 1 2 where
    r s i j x y = if v > 0 && v `S.notMember` s
                     then v : r (S.insert v s) j fib y v
                     else w : r (S.insert w s) j fib y w where
      fib = i + j
      v = x + y - fib
      w = x + y + fib

instance OEIS 79066 where
  oeisIx (succ->n) = length $ filter (`isInfixOf` (primesDec !! n)) $ take n primesDec
    where
      primesDec = "_" : map (show.fi) (oeis @40)

instance OEIS 79079 where
  oeis = map (`div` 4) $
                 zipWith (*) (oeis @8864) $ tail (oeis @8864)

instance OEIS 79085 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @79079)

instance OEIS 79122 where
  oeisIx n = p [1..n] (2 * n) where
     p _  0     = 1
     p [] _     = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 79260 where
  oeisIx (succ->n) = fi . fromEnum $ n `mod` 4 == 1 && (oeisIx @10051 . pred) n == 1

instance OEIS 79261 where
  oeisIx (succ->n) = fi . fromEnum $ n `mod` 4 == 3 && (oeisIx @10051 . pred) n == 1

instance OEIS 79364 where
  oeis = filter
     (\x -> (oeisIx @10051.pred) (x - 1) == 0 && (oeisIx @10051.pred) (x + 1) == 0) (oeis @2808)

instance OEIS 79523 where
  oeis = elemIndices 0 (oeis @35263)

instance OEIS 79559 where
  oeisIx = p $ tail (oeis @225) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 79635 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ map ((2 - ) . (`mod` 4)) $ (rowT @27746) n

instance OEIS 79648 where
  oeisIx = sum . map (oeisIx @10051 . pred) . (rowT @214084)

instance OEIS 79695 where
  oeis = [1..] `O.minus` (oeis @2180)

instance OEIS 79878 where
  oeis = 1 : zipWith (\x n -> if x <= n then x else x - n)
                             (map (* 2) (oeis @79878)) [2..]

instance OEIS 79892 where
  oeisIx (succ->n) = head [x | x <- [n + 1 ..], (oeisIx @1221 . pred) x == 1 + (oeisIx @1221 . pred) n]

instance OEIS 79901 where
  oeis = tablList @79901
instance Table 79901 where
  tabl = zipWith (map . (^)) [0..] (tabl @2262)

instance OEIS 80046 where
  oeis = tablList @80046
instance Table 80046 where
  rowCol = rowCol_off @80046 @1 @1
  rowT   = rowT_off   @80046 @1
  tabl = iterate f [1] where
     f (x:xs) = [x + 1] ++ (zipWith (*) xs $ reverse xs) ++ [x + 1]

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

instance OEIS 80237 where
  oeis = tablList @80237
instance Table 80237 where
  rowCol = rowCol_off @80237 @1 @1
  rowT   = rowT_off @80237 @1
  tabf = [1] : f (tabf @80237) where
     f [[]] =[]
     f (xs:xss) = concatMap (enumFromTo 1 . (+ 1)) xs : f xss

instance OEIS 80342 where
  oeis = 0 : zs where
     zs = 1 : 1 : (map (+ 1) $ concat $ transpose [zs, zs, zs])

instance OEIS 80425 where
  oeisIx = (`mod` 3) . (3 -) . (`mod` 3)
  oeis = cycle [0, 2, 1]

instance OEIS 80444 where
  oeis = tablList @80444
instance Table 80444 where
  rowCol = rowCol_off @80444 @1 @1
  rowT   = rowT_off @80444 @1
  tabf   = zipWith replicate (oeis @1055) [1..]

instance OEIS 80478 where
  oeis = 1 : f 1 [2..] where
     f x (y:ys) | (oeisIx @10051.pred) (x*x + y*y) == 1 = y : (f y ys)
                | otherwise                = f x ys

instance OEIS 80512 where
  oeisIx (succ->n) = if m == 0 then 3 * n' else n  where (n', m) = divMod n 2
  oeis = concat $ transpose [[1, 3 ..], [3, 6 ..]]

instance OEIS 80715 where
  oeis = 1 : filter (\x -> all ((== 1) . (oeisIx @10051.pred)) $
     zipWith (+) (rowT @27750 x) (reverse $ (rowT @27750) x)) [2,4..]

instance OEIS 80737 where
  oeis = 0 : (map f [2..]) where
    f n | mod n 4 == 2 = (oeisIx @80737 . pred) $ div n 2
        | otherwise = (oeisIx @67240 . pred) n

instance Table 80738 where
  tabf = f 3 (drop 2 (oeis @80737)) 3 (M.singleton 0 [2,1]) where
     f i xs'@ (x:xs) till m
       | i > till  = (reverse row) : f i xs' (3 * head row) m'
       | otherwise = f (i + 1) xs till (M.insertWith (++) (div x 2) [i] m)
       where ((_,row),m')  = M.deleteFindMin m
instance OEIS 80738 where
  oeis = tablList @80738

instance OEIS 80739 where
  oeis = map length (tabf @80738)
instance Table 80739 where
  rowCol n k = (oeis @80739) !! n

instance OEIS 80740 where
  oeis = scanl1 (+) (oeis @80739)
instance Table 80740 where
  rowCol n k = (oeis @80740) !! n

instance OEIS 80741 where
  oeis = map head (tabf @80738)
instance Table 80741 where
  rowCol n k = (oeis @80741) !! n

instance OEIS 80742 where
  oeis = map last (tabf @80738)
instance Table 80742 where
  rowCol n k = (oeis @80742) !! n

instance OEIS 80757 where
  oeisIx = (subtract 1) . (oeisIx @7538)

instance OEIS 80786 where
  oeis = tablList @80786
instance Table 80786 where
  rowCol = rowCol_off @80786 @1 @1
  rowT   = rowT_off   @80786 @1
  tabl = map reverse $ iterate f [1] where
     f xs@ (x:_) = (x + 1) :
                  (zipWith (+) xs (map (fi . fromEnum . (lpf <=)) [x, x- 1 ..]))
          where lpf = (oeisIx @6530 . pred) $ (x + 1)

instance OEIS 81091 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @14311)

instance OEIS 81092 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @52294)

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

instance OEIS 81605 where
  oeis = findIndices (/= 0) (oeis @212193)

instance OEIS 81611 where
  oeis = scanl1 (+) (oeis @39966)

instance OEIS 82500 where
  oeis = concat $ transpose [[1..], (oeis @40)]

instance OEIS 82560 where
  oeis = tablList @82560
instance Table 82560 where
  rowCol = rowCol_off @82560 @1 @1
  rowT   = rowT_off @82560 @1
  tabf = iterate (concatMap (\x -> [x + 1, 2 * x + 2])) [1]

instance OEIS 82601 where
  oeis = tablList @82601
instance Table 82601 where
  tabl = [1] : [1,0] : [1,1,0] : f [0,0,1] [0,1,0] [1,1,0]
     where f us vs ws = ys : f (0:vs) (0:ws) ys where
                        ys = zipWith3 (((+) .) . (+)) us vs ws ++ [0]

instance OEIS 82784 where
  oeisIx = (oeisIx @7) . (`mod` 7)
  oeis = cycle [1,0,0,0,0,0,0]

instance OEIS 82792 where
  oeisIx (succ->n) = until ((== 3) . (oeisIx @30)) (+ n) n

instance OEIS 82794 where
  oeisIx (succ->n) = until ((== 4) . (oeisIx @30)) (+ n) n

instance OEIS 82795 where
  oeisIx (succ->n) = until ((== 5) . (oeisIx @30)) (+ n) n

instance OEIS 82796 where
  oeisIx (succ->n) = until ((== 6) . (oeisIx @30)) (+ n) n

instance OEIS 82797 where
  oeisIx (succ->n) = until ((== 7) . (oeisIx @30)) (+ n) n

instance OEIS 82798 where
  oeisIx (succ->n) = until ((== 8) . (oeisIx @30)) (+ n) n

instance OEIS 82811 where
  oeisIx (succ->n) = until ((== 2) . (oeisIx @30)) (+ n) n

instance OEIS 82870 where
  oeis = tablList @82870
instance Table 82870 where
  tabf = map (takeWhile (> 0)) (tabl @82601)

instance OEIS 82949 where
  oeis = f $ S.singleton (2 ^ 3 * 3 ^ 2, 2, 3) where
     f s = y : f (if p' < q then S.insert (p' ^ q * q ^ p', p', q) s'' else s'')
           where s'' = S.insert (p ^ q' * q' ^ p, p, q') s'
                 p' = (oeisIx @151800) p; q' = (oeisIx @151800) q
                 ((y, p, q), s') = S.deleteFindMin s

instance OEIS 82977 where
  oeis = [0, 1, 3, 5, 6, 8, 10] ++ map (+ 12) (oeis @82977)

instance OEIS 83093 where
  oeis = tablList @83093
instance Table 83093 where
  tabl = iterate
     (\ws -> zipWith (\u v -> mod (u + v) 3) ([0] ++ ws) (ws ++ [0])) [1]

instance OEIS 83278 where
  oeis = 1 : f S.empty (drop 2 (oeis @2275)) where
     f rups rus'@ (ru:rus)
       | S.null rups || m > ru = f (S.insert (ru,ru) rups) rus
       | otherwise = m : f (S.insert (m*m',m') (S.deleteMin rups)) rus'
       where (m,m') = S.findMin rups

instance OEIS 83382 where
  oeisIx (succ->n) = f n n (oeis @10051) where
     f m 0 _     = m
     f m k chips = f (min m $ sum chin) (k - 1) chips' where
       (chin,chips') = splitAt n chips

instance OEIS 83383 where
  oeis = 1 : f 0 [2..] (tail (oeis @83382)) where
     f m (x:xs) (y:ys) | y <= m    = f m xs ys
                       | otherwise = x : f y xs ys

instance OEIS 83399 where
  oeisIx = (+ 1) . (oeisIx @1221)

instance OEIS 83414 where
  oeisIx (succ->n) = minimum $ map c $ filter ((== 1) . (gcd n)) [1..n] where
     c k = sum $ map (oeisIx @10051.pred) $ enumFromThenTo k (k + n) (n ^ 2)

instance OEIS 83415 where
  oeis = tablList @83415
instance Table 83415 where
  rowCol n k = (rowT @83415) n !! (k - 1)
  rowT n = f n (oeis @10051) where
     f 0 _     = []
     f k chips = (sum chin) : f (k - 1) chips' where
       (chin,chips') = splitAt n chips
  tabl = map (rowT @83415) [1..]

instance OEIS 83416 where
  oeis = 1 : f 2 1 where
     f x y = z : f (x+1) z where z = (1 + x `mod` 2) * y + 1 - x `mod` 2

instance OEIS 83866 where
  oeis = elemIndices 0 (oeis @4718)

instance OEIS 83920 where
  oeis = scanl1 (+) $ map (1 -) (oeis @10054)

instance OEIS 84110 where
  oeisIx = foldl (/*) 1 . (rowT @27750) . succ where
     x /* y = if m == 0 then x' else x*y where (x',m) = divMod x y

instance OEIS 84111 where
  oeis = [x | x <- [1..], (oeisIx @84110.pred) x == x]

instance OEIS 84112 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @84111)

instance OEIS 84188 where
  oeis = scanl1 (\u v -> 2 * u + v) (oeis @4539)

instance OEIS 84196 where
  oeis = f [] (oeis @40) where
     f ps' (p:ps) = length [q | q <- ps', mod (p + 1) (q + 1) == 0] :
                    f (p : ps') ps where

instance OEIS 84198 where
  oeis = map (oeisIx @40) $ filter ((== 1) . (oeisIx @84196)) [1..]

instance OEIS 84214 where
  oeis = 1 : xs where
     xs = 1 : 4 : zipWith (+) (map (* 2) xs) (tail xs)

instance OEIS 84338 where
  oeis = [1,2,3] ++ zipWith (+) (oeis @84338) (tail (oeis @84338))

instance OEIS 84345 where
  oeis = filter ((== 0) . (oeisIx @10051.pred) . (oeisIx @120)) [0..]

instance OEIS 84385 where
  oeis = 1 : f [2..] 1 where
     f xs s = g xs where
       g (y:ys) = if gcd s y == 1 then y : f (delete y xs) (s + y) else g ys

instance OEIS 84888 where
  oeisIx = (oeisIx @25426) . (oeisIx @578)

instance OEIS 84937 where
  oeis = 1 : 2 : f 2 1 [3..] where
     f x y zs = g zs where
        g (u:us) | gcd y u > 1 || gcd x u > 1 = g us
                 | otherwise = u : f u x (delete u zs)

instance OEIS 84964 where
  oeis = concat $ transpose [[2..], [0..]]

instance OEIS 85478 where
  oeis = tablList @85478
instance Table 85478 where
  tabl = zipWith (zipWith (rowCol @7318)) (tabl @51162) (tabl @25581)

instance OEIS 85612 where
  oeis = tablList @85612
instance Table 85612 where
  rowCol n k = (rowT @85612) n !! (k- 1)
  rowT   = rowT_off @85612 @1
  tabf = f 0 $ zip [1..] (oeis @46523) where
     f x zs'@ (z:zs) = (map fst ys) : f (x + 1) (zs' \\ ys) where
       ys = z : take x (filter ((== snd z) . snd) zs)

instance OEIS 85713 where
  oeis = 1 : r yx3ss where
     r (ps:pss) | (oeisIx @10051.pred) cd == 1 &&
                  map (flip div cd) ps == [3, 4, 6] = cd : r pss
                | otherwise = r pss  where cd = foldl1 gcd ps
     yx3ss = filter ((== 3) . length) $
         map (map snd) $ groupBy ((==) `on` fst) $
         f [1..] (oeis @2110) []
         where f is'@ (i:is) ps'@ (p:ps) yxs
                | i < p = f is ps' $ O.insertBag (oeisIx @10 $ pred i, i) yxs
                | otherwise = yxs' ++ f is' ps yxs''
                where (yxs', yxs'') = span ((<= (oeisIx @10.pred) i) . fst) yxs

instance OEIS 86500 where
  oeis = scanl1 (+) $ tail (oeis @181900)

instance OEIS 86517 where
  oeis = 1 : f 1 [3, 5 ..] where
     f x zs = g zs where
       g (y:ys) = if (oeisIx @10051.pred) ((x + y) `div` 2) == 1
                     then y : f y (delete y zs) else g ys

instance OEIS 87057 where
  oeis = f [2..] where
     f (x:xs) = x : f (us ++ vs) where (us, _ : vs) = splitAt (x - 1) xs

instance OEIS 87069 where
  oeisIx =
     sum . unfoldr (\x -> if x == 0 then Nothing else Just (x, x `div` 4))

instance OEIS 87112 where
  oeis = tablList @87112
instance Table 87112 where
  rowCol = rowCol_off @87112 @1 @1
  rowT n = map (* last ps) ps where ps = take n (oeis @40)
  tabl = map (rowT @87112) [1..]

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

instance OEIS 87279 where
  oeis = 0 : 2 : f (drop 2 (oeis @290))
     where f (x:xs) = x - 1 : x+1 : f xs

instance OEIS 87349 where
  oeisIx (succ->n) = (oeisIx @20639 n) + n

instance OEIS 87370 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . subtract 1 . (* 3)) [0..]

instance OEIS 87401 where
  oeis = tablList @87401
instance Table 87401 where
  tabl = iterate f [0] where
     f row = row' ++ [last row'] where row' = zipWith (+) row [0..]

instance OEIS 87624 where
  oeisIx n = if (oeisIx @10051) n == 1 then 0 else (oeisIx @1221) n

instance OEIS 87695 where
  oeis = filter
     (\x -> (oeisIx @10051.pred) (x - 3) == 1 && (oeisIx @10051.pred) (x + 3) == 1) [2, 4 ..]

instance OEIS 87960 where
  oeisIx n = (-1) ^ (n * (n + 1) `div` 2)
  oeis = cycle [1,-1,-1,1]

instance OEIS 88208 where
  oeis = tablList @88208
instance Table 88208 where
  rowCol = rowCol_off @88208 @1 @1
  rowT   = rowT_off @88208 @1
  tabf = iterate f [1] where
     f vs = (map (subtract 1) ws) ++ reverse ws where ws = map (* 2) vs

instance OEIS 88230 where
  oeis = 0 : f [1..] [0] where
     f (x:xs) ys@ (y:_)
      | x <= y && (length $ filter (== z) ys) <= 1 = z : f xs (z : ys)
      | otherwise = (x + y) : f xs ((x + y) : ys)  where z = y - x

instance OEIS 88567 where
  oeis = 1 : tail xs where
     xs = 0 : 1 : zipWith (+) xs (tail $ concat $ transpose [xs, tail xs])

instance OEIS 88580 where
  oeisIx = (+ 1) . (oeisIx @203)
  oeis   = map succ $ oeis @203

instance OEIS 88696 where
  oeis = f [1] where
     f (x:xs) = x : f (xs ++ [x + 1 - x `mod` 2, x + x `mod` 2])

instance OEIS 88732 where
  oeisIx n = head [q | q <- [2 * n + 1, 3 * n + 2 ..], (oeisIx @10051.pred) q == 1]

instance OEIS 88733 where
  oeisIx (succ->n) = last $ take n $
              [q | q <- [2 * n + 1, 3 * n + 2 ..], (oeisIx @10051.pred) q == 1]

instance OEIS 88878 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . subtract 2 . (* 3)) (oeis @40)

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

instance OEIS 89189 where
  oeis = oeis @97375
instance OEIS 89194 where
  oeis = filter ((== 1) . (oeisIx @212793)) (oeis @97375)

instance OEIS 89229 where
  oeis = f (oeis @18252) $ tail (oeis @290) where
     f (u:us) vs'@ (v:vs) = if u < v then u : f us vs' else f us vs

instance OEIS 89237 where
  oeis = merge (oeis @40) (oeis @290) where
     merge xs'@ (x:xs) ys'@ (y:ys) =
           if x < y then x : merge xs ys' else y : merge xs' ys

instance OEIS 89610 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051.pred) [n^2 .. n* (n+1)]

instance OEIS 89781 where
  oeis = 1 : f [1..] 1 where
     f xs y = g xs where
       g (z:zs) = if gcd y z == 1 then y' : f (delete z xs) y' else g zs
                  where y' = y + z

instance OEIS 90529 where
  oeis = f 1 1 0 where
    f w v u = if u <= w then v : f w v (u+1) else v' : f (w*v') v' (u+1)
              where v' = v + 1

instance OEIS 90582 where
  oeis = tablList @90582
instance Table 90582 where
  rowCol = rowCol_off @90582 @1 @1
  rowT   = rowT_off   @90582 @1
  tabl = map reverse (tabl @19538)

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

instance OEIS 90824 where
  oeis = tablList @90824
instance Table 90824 where
  tabl = zipWith (zipWith p)
     (map (\x -> map (`enumFromTo` x) [1..x+1]) [0..]) (tabl @7318)
     where p _          0 = 1
           p []         _ = 0
           p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 90895 where
  oeis = 1 : f 2 1 where
     f x y = z : f (x + 1) z where
          z = if m == 0 then y' else x + y; (y',m) = divMod y 2

instance OEIS 91441 where
  oeis = tablList @91441
instance Table 91441 where
  rowCol = rowCol_off @91441 @1 @1
  rowT   = rowT_off   @91441 @1
  tabl = iterate f [1] where
     f xs = zipWith (+)
       (zipWith (*) ([0] ++ xs) ks) (zipWith (*) (xs ++ [0]) (reverse ks))
       where ks = [1 .. 1 + genericLength xs]

instance OEIS 91491 where
  oeis = tablList @91491
instance Table 91491 where
  tabl = iterate (\row -> 1 : scanr1 (+) row) [1]

instance OEIS 91633 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @136333)

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

instance OEIS 92391 where
  oeisIx n = n + oeisIx @120 n

instance OEIS 92392 where
  oeis = tablList @92392
instance Table 92392 where
  rowCol = rowCol_off @92392 @1 @1
  rowT   = rowT_off   @92392 @1
  tabl = map reverse (tabl @46899)

instance OEIS 92401 where
  oeis = f [1..] where
     f (x:xs) = x : x' : f (delete x' xs) where x' = 3*x

instance OEIS 92525 where
  oeisIx (succ->n) = f n n where
     f x y = if m == 0 then f x' (2 * y + 1) else y
             where (x', m) = divMod x 2

instance OEIS 92539 where
  oeis = scanl1 (\v d -> 2 * v + d) $ oeis @51023

instance OEIS 92620 where
  oeis = elemIndices 1 (oeis @193238)

instance OEIS 92624 where
  oeis = elemIndices 2 (oeis @193238)

instance OEIS 92625 where
  oeis = elemIndices 3 (oeis @193238)

instance OEIS 92695 where
  oeis = scanl (+) 0 $ map (fi . fromEnum . (> 7)) (8 : tail (oeis @20639))

instance OEIS 92892 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @6666))

instance OEIS 92953 where
  oeisIx (succ->n) = sum $
     zipWith (\u v -> (oeisIx @10051.pred) u * (oeisIx @10051.pred) v) [1 .. n - 1] [n + 1 ..]

instance OEIS 93017 where
  oeisIx n = if n == 0 then 0 else (oeisIx @93017) n' + (oeisIx @7953) (2 * t) + d
              where (n', td) = divMod n 100; (t, d) = divMod td 10

instance OEIS 93018 where
  oeis = filter ((== 1) . (oeisIx @249832)) [0..]

instance OEIS 93019 where
  oeisIx = flip mod 10 . (oeisIx @93018)

instance OEIS 93020 where
  oeis = elemIndices 0 (oeis @93019)

instance OEIS 93021 where
  oeis = elemIndices 1 (oeis @93019)

instance OEIS 93022 where
  oeis = elemIndices 2 (oeis @93019)

instance OEIS 93023 where
  oeis = elemIndices 3 (oeis @93019)

instance OEIS 93024 where
  oeis = elemIndices 4 (oeis @93019)

instance OEIS 93025 where
  oeis = elemIndices 5 (oeis @93019)

instance OEIS 93026 where
  oeis = elemIndices 6 (oeis @93019)

instance OEIS 93027 where
  oeis = elemIndices 7 (oeis @93019)

instance OEIS 93028 where
  oeis = elemIndices 8 (oeis @93019)

instance OEIS 93029 where
  oeis = elemIndices 9 (oeis @93019)

instance OEIS 93445 where
  oeis = tablList @93445
instance Table 93445 where
  rowCol n k = (rowT @93445) n !! (k- 1)
  rowT n = f [n, n - 1 .. 1] [1 ..] where
     f [] _      = []
     f (x:xs) ys = sum us : f xs vs where (us,vs) = splitAt x ys
  tabl = map (rowT @93445) [1 ..]

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

instance OEIS 93573 where
  oeis = tablList @93573
instance Table 93573 where
  rowCol n k = (rowT @93573) n !! (k- 1)
  rowT n = take n $ elemIndices n (oeis @20986)
  tabl = map (rowT @93573) [1..]

instance OEIS 93644 where
  oeis = tablList @93644
instance Table 93644 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [9, 1]

instance OEIS 93645 where
  oeis = tablList @93645
instance Table 93645 where
  tabl = [1] : iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [10, 1]

instance OEIS 93771 where
  oeis = [oeisIx @1597 x | x <- [1..], (oeisIx @10051.pred) (oeisIx @25479 x) == 1]

instance OEIS 93820 where
  oeis = 1 : f [2..] [1] where
     f (x:xs) ys = y : f xs (y:ys) where y = sum $ map (gcd x) ys

instance OEIS 93995 where
  oeis = tablList @93995
instance Table 93995 where
  rowCol = rowCol_off @93995 @1 @1
  rowT   = rowT_off   @93995 @1
  tabl = zipWith replicate [1..] $ tail (oeis @290)

instance OEIS 94189 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051.pred) [n* (n - 1) .. n^2]

instance OEIS 94305 where
  oeis = tablList @94305
instance Table 94305 where
  tabl = zipWith (map . (*)) (tail (oeis @217)) (tabl @7318)

instance OEIS 94407 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) [1,17..]

instance OEIS 94587 where
  oeis = tablList @94587
instance Table 94587 where
  tabl = map fst $ iterate f ([1], 1)
     where f (row, i) = (map (* i) row ++ [1], i + 1)

instance OEIS 94727 where
  oeis = tablList @94727
instance Table 94727 where
  rowCol n k = n + k
  rowT   = rowT_off   @94727 @1
  tabl = iterate (\row@ (h:_) -> (h + 1) : map (+ 2) row) [1]

instance OEIS 94912 where
  oeisIx n = a 2 n where
     a s 0 = 0 ^ s
     a s x = a (t s b) x' where (x',b) = divMod x 2
     t 2 0 = 2; t 2 1 = 1; t 1 0 = 0; t 1 1 = 2; t 0 0 = 2; t 0 1 = 0


instance OEIS 95116 where
  oeisIx (succ->n) = (oeisIx @40 . pred) n + n - 1

instance OEIS 95117 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @720.pred) n + n

instance OEIS 95180 where
  oeis =filter ((== 1) . (oeisIx @10051.pred)) (oeis @4087)

instance OEIS 95195 where
  oeis = tablList @95195
instance Table 95195 where
  rowCol = rowCol_off @95195 @1 @1
  rowT   = rowT_off   @95195 @1
  tabl = f (oeis @40) [] where
     f (p:ps) xs = ys : f ps ys where ys = scanl (-) p xs

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

instance OEIS 95774 where
  oeisIx n = 2 * (oeisIx @3160) n - n - 1

instance OEIS 95775 where
  oeis = map (`div` 2) $ map succ $ elemIndices 0 (oeis @95774)

instance OEIS 95815 where
  oeisIx (succ->n) = n + oeisIx @54055 n

instance OEIS 96008 where
  oeis = tablList @96008
instance Table 96008 where
  rowCol = rowCol_off @96008 @1 @1
  rowT   = rowT_off @96008 @1
  tabf = [0] : map (0 :) (tabf @46071)

instance OEIS 96139 where
  oeisIx (succ->n) = sum (map (oeisIx @10051.pred) gs') + do fi $ fromEnum (1 `elem` gs')
     where gs' = map (2 * n -) $ takeWhile (< 2 * n) (oeis @8578)

instance OEIS 96145 where
  oeis = tablList @96145
instance Table 96145 where
  tabl = map (map (oeisIx @7953)) (tabl @7318)

instance OEIS 96258 where
  oeisIx = p (oeis @18252) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 96274 where
  oeis = elemIndices 0 (oeis @96535)

instance OEIS 96465 where
  oeis = tablList @96465
instance Table 96465 where
  tabl = map reverse (tabl @91491)

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

instance OEIS 97062 where
  oeis = concat $ transpose [oeis @5408, (-1) : (oeis @5408)]

instance OEIS 97065 where
  oeisIx n = n' - 2 * m where (n', m) = divMod (n + 2) 2
  oeis = concat $ transpose [[1 ..], [-1 ..]]

instance OEIS 97207 where
  oeis = tablList @97207
instance Table 97207 where
  tabl = map init $ tail (tabl @29635)

instance OEIS 97375 where
  oeis = filter ((== 1) . (oeisIx @212793) . (subtract 2)) (oeis @40)

instance OEIS 97807 where
  oeis = tablList @97807
instance Table 97807 where
  tabl = iterate (\xs@ (x:_) -> - x : xs) [1]

instance OEIS 97889 where
  oeis = f $ S.singleton (6, 2, 3) where
     f s = y : f (S.insert (w, p, q') $ S.insert (w `div` p, (oeisIx @151800) p, q') s')
           where w = y * q'; q' = (oeisIx @151800) q
                 ((y, p, q), s') = S.deleteFindMin s

instance OEIS 97893 where
  oeis = scanl1 (+) (oeis @2426)

instance OEIS 97933 where
  oeis = [x | x <- (oeis @91998), (oeisIx @10051 . pred) x == 1]

instance OEIS 98012 where
  oeis = tablList @98012
instance Table 98012 where
  rowCol = rowCol_off @98012 @1 @1
  rowT   = rowT_off   @98012 @1
  tabl = map (scanl1 (*)) (tabl @104887)

instance OEIS 98311 where
  oeis = 1 : ys where
     ys = 2 : f ys [3..] where
          f (v:vs) ws = us ++ f vs (ws \\ us) where
            us = take 2 $ filter ((== 1) . (gcd v)) ws

instance OEIS 98424 where
  oeisIx (succ->n) = genericLength [ (p,q,r) | p <- takeWhile (<= n) (oeis @40),
              let r = p + 6, (oeisIx @10051.pred) r == 1, q <- [p+1..r - 1], (oeisIx @10051.pred) q == 1]

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

instance OEIS 98743 where
  oeisIx n = p [nd | nd <- [1..n], mod n nd /= 0] n where
     p _  0 = 1
     p [] _ = 0
     p ks'@ (k:ks) m | m < k = 0 | otherwise = p ks' (m - k) + p ks m
  oeis = map (\x -> pMemo x 1 x) [0..] where
     pMemo = memo3 integral integral integral p
     p _ _ 0 = 1
     p x k m | m < k        = 0
             | mod x k == 0 = pMemo x (k + 1) m
             | otherwise    = pMemo x k (m - k) + pMemo x (k + 1) m


instance OEIS 98825 where
  oeis = tablList @98825
instance Table 98825 where
  tabl = map (zipWith (*) (oeis @166)) (tabl @7318)

instance OEIS 98859 where
  oeisIx = p 0 [] 1 where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p m ms k x
       | x < k       = 0
       | m == 0      = p 1 ms k (x - k) + p 0 ms (k + 1) x
       | m `elem` ms = p (m + 1) ms k (x - k)
       | otherwise   = p (m + 1) ms k (x - k) + p 0 (m : ms) (k + 1) x

instance OEIS 99036 where
  oeis = zipWith (-) (oeis @79) (oeis @45)

instance OEIS 99047 where
  oeis = [m | m <- [1..],
                      (oeisIx @10051.pred) (m - 1) == 0 && (oeisIx @10051.pred) (m + 1) == 0]

instance OEIS 99054 where
  oeis = 1 : concatMap fromJust (zipWith stripPrefix ass $ tail ass)
     where ass = iterate f [1]
           f xs = concat $ concat $ transpose [map g $ e xs, map h $ o xs]
           g 1 = [1,2,3]; g 2 = [2,3,1]; g 3 = [3,1,2]
           h 1 = [3,2,1]; h 2 = [1,3,2]; h 3 = [2,1,3]
           e [] = []; e [x] = [x]; e (x:_:xs) = x : e xs
           o [] = []; o [x] = []; o (_:x:xs) = x : o xs

instance OEIS 99244 where
  oeisIx (succ->n) = gcd (oeisIx @70939 n) (oeisIx @120 n)

instance OEIS 99247 where
  oeis = filter ((== 1) . (oeisIx @99244 . pred)) [1..]

instance OEIS 99248 where
  oeis = map (+1) $ filter ((> 1) . (oeisIx @99244)) [1..]

instance OEIS 99249 where
  oeis = scanl1 (+) $ map ((0 ^) . (subtract 1)) (oeis @99244)

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

instance OEIS 99627 where
  oeis = tablList @99627
instance Table 99627 where
  tabl = iterate (\xs@ (x:_) -> (2 * x) : map ((+ 1) . (* 2)) xs) [1]



instance OEIS 99773 where
  oeisIx = p (oeis @65091) where
     p _      0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 99774 where
  oeisIx = (oeisIx @5) . pred . (oeisIx @5408)

instance OEIS 99849 where
  oeis = scanl1 (+) (oeis @8480)

instance OEIS 99920 where
  oeis = zipWith (*) [1..] (oeis @45)

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

instance OEIS 99964 where
  oeis = tablList @99964
instance Table 99964 where
  tabf = scanl f [1] $ tail (oeis @10054) where
     f row t = if t == 1 then row' ++ [last row'] else row'
             where row' = scanl1 (+) $ reverse row

instance OEIS 100104 where
  oeisIx n = n*n*n - n*n + 1

instance OEIS 100208 where
  oeis = 1 : (f 1 [1..] $ S.singleton 1) where
     f x (w:ws) s
       | w `S.notMember` s &&
         (oeisIx @10051.pred) (x*x + w*w) == 1 = w : (f w [1..] $ S.insert w s)
       | otherwise                = f x ws s where

instance OEIS 100326 where
  oeis = tablList @100326
instance Table 100326 where
  tabl = [1] : f [[1]] where
    f xss@ (xs:_) = ys : f (ys : xss) where
      ys = y : map (sum . zipWith (*) (zs ++ [y])) (map reverse zss)
      y = sum $ zipWith (*) [1..] xs
      zss@((_:zs):_) = transpose $ reverse xss

instance OEIS 100368 where
  oeis = f (S.singleton 6) (tail (oeis @65091)) where
    f s ps'@ (p:ps) | mod m 4 > 0 = m : f (S.insert (2*p) $ S.insert (2*m) s') ps
      | otherwise = m : f (S.insert (2*m) s') ps'
     where (m,s') = S.deleteFindMin s

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

instance OEIS 100717 where
  oeis = map (+1) $ elemIndices 0 (oeis @203908)

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

instance OEIS 101164 where
  oeis = tablList @101164
instance Table 101164 where
  tabl = zipWith (zipWith (-)) (tabl @8288) (tabl @7318)

instance OEIS 101203 where
  oeis = scanl (+) 0 $ zipWith (*) [1..] $ map (1 -) (oeis @10051)

instance OEIS 101211 where
  oeis = tablList @101211
instance Table 101211 where
  rowCol = rowCol_off @101211 @1 @1
  rowT   = rowT_off @101211 @1
  tabf = map (reverse . map length . group) $ tail (tabf @30308)

instance OEIS 101301 where
  oeis = scanl1 (+) (oeis @6093)

instance OEIS 101369 where
  oeis = f [1..] where
     f (x:xs) = x : y : f (delete y xs) where y = xs !! (x - 1)

instance OEIS 101881 where
  oeis = scanl1 (+) $ intersperse 1 [1..]

instance OEIS 102413 where
  oeis = tablList @102413
instance Table 102413 where
  tabl = [1] : [1,1] : f [2] [1,1] where
     f us vs = ws : f vs ws where
               ws = zipWith3 (((+) .) . (+))
                    ([0] ++ us ++ [0]) ([0] ++ vs) (vs ++ [0])

instance OEIS 102472 where
  oeis = tablList @102472
instance Table 102472 where
  rowCol = rowCol_off @102472 @1 @1
  rowT   = rowT_off   @102472 @1
  tabl = map reverse (tabl @102473)

instance OEIS 102473 where
  oeis = tablList @102473
instance Table 102473 where
  rowCol = rowCol_off @102473 @1 @1
  rowT   = rowT_off   @102473 @1
  tabl = [1] : [1, 1] : f [1] [1, 1] 2 where
     f us vs x = ws : f vs ws (x + 1) where
                 ws = 1 : zipWith (+) ([0] ++ us) (map (* x) vs)

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

instance OEIS 102661 where
  oeis = tablList @102661
instance Table 102661 where
  rowCol = rowCol_off @102661 @1 @1
  rowT   = rowT_off   @102661 @1
  tabl = map (scanl1 (+) . tail) $ tail (tabl @48993)

instance OEIS 102662 where
  oeis = tablList @102662
instance Table 102662 where
  tabl = [1] : [1,3] : f [1] [1,3] where
     f xs ys = zs : f ys zs where
       zs = zipWith (+) ([0] ++ xs ++ [0]) $
                        zipWith (+) ([0] ++ ys) (ys ++ [0])

instance OEIS 102696 where
  oeisIx (succ->n) = genericLength $ nub
     [p + q | p <- genericTake n (oeis @65091), q <- takeWhile (<= p) (oeis @65091)]

instance OEIS 102820 where
  oeis =  map (sum . (map (oeisIx @10051.pred))) $
     zipWith enumFromTo (oeis @100484) (tail (oeis @100484))

instance OEIS 103128 where
  oeisIx = (oeisIx @196) . (subtract 1) . (* 2) . succ

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

instance OEIS 103369 where
  oeisIx = until (`elem` (oeis @39943)) (oeisIx @3132) . succ

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

instance OEIS 104125 where
  oeisIx = (^ 2) . (oeisIx @64413)

instance OEIS 104278 where
  oeis = [m | m <- [1..],
                      (oeisIx @10051.pred) (2 * m - 1) == 0 && (oeisIx @10051.pred) (2 * m + 1) == 0]

instance OEIS 104350 where
  oeis = scanl1 (*) (oeis @6530)

instance OEIS 104499 where
  oeis = findIndices ((== 1) . (oeisIx @10051 . pred)) (oeis @1945)

instance OEIS 104698 where
  oeis = tablList @104698
instance Table 104698 where
  rowCol = rowCol_off @104698 @1 @1
  rowT   = rowT_off   @104698 @1
  tabl = [1] : [2,1] : f [1] [2,1] where
     f us vs = ws : f vs ws where
       ws = zipWith (+) ([0] ++ us ++ [0]) $
            zipWith (+) ([1] ++ vs) (vs ++ [0])

instance OEIS 104763 where
  oeis = tablList @104763
instance Table 104763 where
  rowCol = rowCol_off @104763 @1 @1
  rowT   = rowT_off   @104763 @1
  tabl = map (flip take $ tail (oeis @45)) [1..]

instance OEIS 104887 where
  oeis = tablList @104887
instance Table 104887 where
  rowCol = rowCol_off @104887 @1 @1
  rowT   = rowT_off   @104887 @1
  tabl = map reverse $ tail $ inits (oeis @40)

instance OEIS 104895 where
  oeis = 0 : concat (transpose [map (negate . (+ 1)) zs, tail zs])
                 where zs = map (* 2) (oeis @104895)

instance OEIS 105025 where
  oeisIx n = fi . foldl fior 0 $ zipWith fiand
                    (oeis @79) $ reverse $ enumFromTo n (n - 1 + (oeisIx @70939) n)
    where fiand x y = fi x .&. fi y
          fior  x y = fi x .|. fi y

instance OEIS 105027 where
  oeisIx n = fi . foldl fior 0 $ zipWith fiand
                    (oeis @79) $ enumFromTo (n + 1 - (oeisIx @70939) n) n
    where fiand x y = fi x .&. fi y
          fior  x y = fi x .|. fi y

instance OEIS 105278 where
  oeis = tablList @105278
instance Table 105278 where
  rowCol = rowCol_off @105278 @1 @1
  rowT   = rowT_off   @105278 @1
  tabl = [1] : f [1] 2 where
     f xs i = ys : f ys (i + 1) where
       ys = zipWith (+) ([0] ++ xs) (zipWith (*) [i, i + 1 ..] (xs ++ [0]))

instance OEIS 105728 where
  oeis = tablList @105728
instance Table 105728 where
  rowCol = rowCol_off @105728 @1 @1
  rowT   = rowT_off   @105728 @1
  tabl = iterate (\row -> zipWith (+) ([0] ++ tail row ++ [1]) $
                                  zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 105801 where
  oeis = 1 : 2 : fc 2 1 where
     fc x x' = y : fc y x where y = (oeisIx @6370) (x + x')

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

instance OEIS 107430 where
  oeis = tablList @107430
instance Table 107430 where
  tabl = map sort (tabl @7318)

instance OEIS 107715 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @7090)

instance OEIS 107743 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred) . (oeisIx @62028)) [1..]

instance OEIS 107788 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (8 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 107988 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (4 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108018 where
  oeisIx = sum . map (oeisIx @10051 . pred) . nub . map sum .
            tail . subsequences . flip take (oeis @40) . succ

instance OEIS 108035 where
  oeis = tablList @108035
instance Table 108035 where
  rowCol = rowCol_off @108035 @1 @1
  rowT   = rowT_off   @108035 @1
  tabl = zipWith replicate [1..] $ drop 2 (oeis @45)

instance OEIS 108037 where
  oeis = tablList @108037
instance Table 108037 where
  tabl = zipWith replicate [1..] (oeis @45)

instance OEIS 108040 where
  oeis = tablList @108040
instance Table 108040 where
  tabl = ox False (tabl @8281) where
    ox turn (xs:xss) = (if turn then reverse xs else xs) : ox (not turn) xss

instance OEIS 108044 where
  oeis = tablList @108044
instance Table 108044 where
  tabl = zipWith drop [0..] $ map (intersperse 0) (tabl @7318)

instance OEIS 108090 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (11 * y, i + 1, j) $ S.insert (13 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108218 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (11 * y, i + 1, j) $ S.insert (12 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108299 where
  oeis = tablList @108299
instance Table 108299 where
  tabl = [1] : iterate (\row ->
     zipWith (+) (zipWith (*) ([0] ++ row) (oeis @33999))
                 (zipWith (*) (row ++ [0]) (oeis @59841))) [1,-1]

instance OEIS 108396 where
  oeis = tablList @108396
instance Table 108396 where
  tabl = zipWith (\v ws -> map (flip div 2 . (* v) . (+ 1)) ws)
                         [0..] (tabl @79901)

instance OEIS 108397 where
  oeisIx 0 = 0
  oeisIx 1 = 2
  oeisIx n = n * (n^ (n+1) + n^2 - 2) `div` (2 * (n - 1))

instance OEIS 108561 where
  oeis = tablList @108561
instance Table 108561 where
  tabl = map reverse (tabl @112465)

instance OEIS 108617 where
  oeis = tablList @108617
instance Table 108617 where
  tabl = [0] : iterate f [1,1] where
     f row@ (u:v:_) = zipWith (+) ([v - u] ++ row) (row ++ [v - u])

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

instance OEIS 108731 where
  oeis = tablList @108731
instance Table 108731 where
  rowT 0 = [0]
  rowT n = t n $ reverse $ takeWhile (<= n) $ tail (oeis @142)
     where t 0 []     = []
           t x (b:bs) = x' : t m bs where (x',m) = divMod x b
  tabf = map (rowT @108731) [0..]

instance OEIS 108761 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (10 * y, i + 1, j) $ S.insert (13 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108779 where
  oeis = f $ S.singleton (1,0,0) where
     f s = y : f (S.insert (10 * y, i + 1, j) $ S.insert (11 * y, i, j + 1) s')
           where ((y, i, j), s') = S.deleteFindMin s

instance OEIS 108872 where
  oeis = tablList @108872
instance Table 108872 where
  rowCol = rowCol_off @108872 @1 @1
  rowT   = rowT_off   @108872 @1
  tabl = map (\x -> [x + 1 .. 2 * x]) [1..]

instance OEIS 109008 where
  oeisIx = gcd 4
  oeis = cycle [4,1,2,1]

instance OEIS 109043 where
  oeisIx = (lcm 2)
  oeis = zipWith (*) [0..] (oeis @34)

instance OEIS 109128 where
  oeis = tablList @109128
instance Table 109128 where
  tabl = iterate (\row -> zipWith (+)
     ([0] ++ row) (1 : (map (+ 1) $ tail row) ++ [0])) [1]

instance OEIS 109465 where
  oeis = f 1 [1..] where
     f o xs = g xs where
       g (z:zs) = if o' == o then g zs else z : f o' (delete z xs)
                  where o' = (oeisIx @1221 . pred) z

instance OEIS 109981 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @55642)) (oeis @46704)

instance OEIS 110080 where
  oeis = 1 : f 1 (oeis @40) (S.singleton 1) where
     f x (p:ps) m = y : f y ps (S.insert y m) where
       y = g x p
       g 0 _ = h x p
       g u 0 = u
       g u v = g (u - 1) (if S.member (u - 1) m then v else v - 1)
       h u 0 = u
       h u v = h (u + 1) (if S.member (u + 1) m then v else v - 1)

instance OEIS 110088 where
  oeisIx (succ->n) = (oeisIx @5 . pred) n ^ (oeisIx @1221 . pred) n

instance OEIS 110267 where
  oeis = scanl1 (+) (oeis @70952)

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

instance OEIS 110654 where
  oeisIx = (`div` 2) . (+ 1)
  oeis = tail (oeis @4526)

instance OEIS 111060 where
  oeisIx 0 = 0
  oeisIx n = sum $ (rowT @265668 . succ) n

instance OEIS 111063 where
  oeis = 1 : zipWith (+) [1..] (zipWith (*) [0..] (oeis @111063))

instance OEIS 111244 where
  oeis = scanl1 (+) (oeis @84385)

instance OEIS 111282 where
  oeis = 1 : (oeis @25169)

instance OEIS 111708 where
  oeisIx 0 = 9
  oeisIx n = f [] n where
     f ys 0 = foldl (\v d -> 10 * v + d) 0 $ ys ++ map (9 -) ys
     f ys x = f (d : ys) x' where (x', d) = divMod x 10

-- instance OEIS 111712 where
--   oeis = scanl (+) 1 (oeis @195013)
--
instance OEIS 111721 where
  oeis = 1 : 1 : map (+ 5) (zipWith (+) (oeis @111721) (tail (oeis @111721)))

instance OEIS 112310 where
  oeis = concat fss where
     fss = [0] : [1] : (map (map (+ 1))) (zipWith (++) fss $ tail fss)

instance OEIS 112373 where
  oeis = 1 : 1 : zipWith (\u v -> (u^3 + u^2) `div` v)
                                 (tail (oeis @112373)) (oeis @112373)

instance OEIS 112465 where
  oeis = tablList @112465
instance Table 112465 where
  tabl = iterate f [1] where
     f xs'@ (x:xs) = zipWith (+) ([-x] ++ xs ++ [0]) ([0] ++ xs')

instance OEIS 112468 where
  oeis = tablList @112468
instance Table 112468 where
  tabl = iterate (\xs -> zipWith (-) ([2] ++ xs) (xs ++ [0])) [1]

instance OEIS 112632 where
  oeis = scanl1 (+) $ map negate (oeis @134323)

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

instance OEIS 114227 where
  oeisIx ((+2)->n) = head [p | p <- tail (oeis @40),
                        (oeisIx @10051 . pred) (2 * p + (oeisIx @40) n) == 1]

instance OEIS 114230 where
  oeisIx (succ->n) = head [p | let q = (oeisIx @40) n,
                        p <- reverse $ takeWhile (< q) (oeis @40),
                        (oeisIx @10051 . pred) (q + 2 * p) == 1]

instance OEIS 114231 where
  oeisIx (succ->n) = head
    [m | m <- [1..]
    , (oeisIx @10051 . pred) (oeisIx @40 n + 2 * (oeisIx @40) (n - m)) == 1]

instance OEIS 114283 where
  oeis = tablList @114283
instance Table 114283 where
  tabl = iterate
     (\row -> (sum $ zipWith (+) row $ reverse row) : row) [1]

instance OEIS 114327 where
  oeis = tablList @114327
instance Table 114327 where
  tabl = zipWith (zipWith (-)) (tabl @25581) (tabl @2262)

instance OEIS 114851 where
  oeisIx = open where
    open n = if n<2 then 0 else
             1 + open (n - 2) + sum [open i * open (n - 2 - i) | i <- [0..n - 2]]

instance OEIS 115068 where
  oeis = tablList @115068
instance Table 115068 where
  rowCol = rowCol_off @115068 @1 @1
  rowT   = rowT_off   @115068 @1
  tabl = iterate (\row -> zipWith (+) (row ++ [1]) $
                                  zipWith (+) (row ++ [0]) ([0] ++ row)) [1]

instance OEIS 116666 where
  oeis = tablList @116666
instance Table 116666 where
  rowCol = rowCol_off @116666 @1 @1
  rowT   = rowT_off   @116666 @1
  tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @158405)

instance OEIS 116853 where
  oeis = tablList @116853
instance Table 116853 where
  rowCol = rowCol_off @116853 @1 @1
  rowT   = rowT_off   @116853 @1
  tabl = map reverse $ f (tail (oeis @142)) [] where
     f (u:us) vs = ws : f us ws where ws = scanl (-) u vs

instance OEIS 116854 where
  oeis = tablList @116854
instance Table 116854 where
  rowCol = rowCol_off @116854 @1 @1
  rowT   = rowT_off   @116854 @1
  tabl = [1] : zipWith (:) (tail $ map head tss) tss
                 where tss = (tabl @116853)

instance OEIS 117047 where
  oeis = [x | k <- [0..], let x = 60 * k + 11, (oeisIx @10051 . pred) x == 1]

instance OEIS 117070 where
  oeis = tSegments !! 0

instance OEIS 117071 where
  oeis = tSegments !! 1

instance OEIS 117072 where
  oeis = tSegments !! 2

tSegments = transpose $ unfoldr (Just . splitAt 3) $ tail (oeis @78783)

instance OEIS 117073 where
  oeis = oeis117073

(oeis78783, (oeis117073)) = unzip $
    (0,0) : (1,1) : (3,2) : f 3 2 (2:[4..]) where
    f a d ms@ (m:_) = (a', d') : f a' d' (delete a' ms) where
      (a', d') = if i > d then (m, i) else (a + d + 1, d + 1)
      i = a - m

instance OEIS 117128 where
  oeis = 1 : f 1 (oeis @40) (S.singleton 1) where
     f x (p:ps) s | x' > 0 && x' `S.notMember` s = x' : f x' ps (S.insert x' s)
                  | otherwise                  = xp : f xp ps (S.insert xp s)
                  where x' = x - p; xp = x + p

instance OEIS 117140 where
  oeis = 5 : 7 : ulam 2 7 (oeis @117140)

instance OEIS 117317 where
  oeis = tablList @117317
instance Table 117317 where
  tabl = map reverse (tabl @56242)

instance OEIS 117499 where
  oeisIx 0 = sum $ map (oeisIx @10051 . pred) [1, 2, 0 + 1, 0 + 2, 1 + 2, 0 + 1 + 2]
  oeisIx (succ->n) = sum $ map (oeisIx @10051 . pred) [n - 1, n, n + 1, 2 * n - 1, 2 * n + 1]

instance OEIS 117818 where
  oeisIx (succ->n)
    = if (oeisIx @10051 . pred) n == 1 then n else (oeisIx @32742 . pred) n

instance OEIS 118013 where
  oeis = tablList @118013
instance Table 118013 where
  rowCol = rowCol_off @118013 @1 @1
  rowT n = map (div (n^2)) [1..n]
  tabl = map (rowT @118013) [1..]

instance OEIS 118139 where
  oeis = filter ((> 1) . (oeisIx @52343)) [0..]

instance OEIS 118372 where
  oeis = sPerfect 1 [] where
     sPerfect x ss | v > x = sPerfect (x + 1) ss
                   | v < x = sPerfect (x + 1) (x : ss)
                   | otherwise = x : sPerfect (x + 1) (x : ss)
                   where v = sum (filter ((== 0) . mod x) ss)

instance OEIS 118668 where
  oeisIx = (oeisIx @43537.pred) . (oeisIx @217)
  oeis = map (oeisIx @43537.pred) (oeis @217)

instance OEIS 118914 where
  oeis = tablList @118914
instance Table 118914 where
  rowCol = rowCol_off @118914 @2 @1
  rowT   = rowT_off @118914 @2
  tabf = map sort $ tail (tabf @124010)

instance OEIS 119258 where
  oeis = tablList @119258
instance Table 119258 where
  tabl = iterate (\row -> zipWith (+)
     ([0] ++ init row ++ [0]) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 119345 where
  oeis = elemIndices 1 (oeis @52343)

instance OEIS 119352 where
  oeisIx n = f 2 n where
     f b x = g x where
       g 0 = b
       g z = if r == b - 1 then f (b + 1) n else g z'
             where (z', r) = divMod z b

instance OEIS 119709 where
  oeis = tablList @119709
instance Table 119709 where
  rowT n = map (foldr (\d v -> v * 2 + d) 0) $
     filter (`isInfixOf` (rowT @30308 n)) $ take (n + 1) (tabf @30308)
  tabf = map (rowT @119709) [0..]

instance OEIS 120444 where
  oeis = zipWith (-) (tail (oeis @4125)) (oeis @4125)

instance OEIS 120486 where
  oeis = scanl1 (+) (oeis @188)

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

instance OEIS 121281 where
  oeis = tablList @121281
instance Table 121281 where
  tabl = [1] : f [1] (oeis @40) where
     f xs@ (x:_) (p:ps) = ys : f ys ps where ys = (map (* p) xs) ++ [x]

instance OEIS 121539 where
  oeis = elemIndices 1 (oeis @35263)

instance OEIS 121573 where
  oeis = scanl1 (+) $ map (oeisIx @36263 . pred) [1, 3 ..]

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

instance OEIS 122132 where
  oeis = filter ((== 1) . (oeisIx @8966 . pred) . (oeisIx @265) . pred) [1..]

instance OEIS 122197 where
  oeis = tablList @122197
instance Table 122197 where
  rowCol = rowCol_off @122197 @0 @1
  rowT   = rowT_off   @122197 @1
  tabf   = concat $ transpose [tabl @2260, tabl @2260]

instance OEIS 122366 where
  oeis = tablList @122366
instance Table 122366 where
  tabl = f 1 (tabl @7318) where
     f x (_:bs:pss) = (take x bs) : f (x + 1) pss

instance OEIS 122425 where
  oeisIx = maximumBy (comparing $ show.fi) . (rowT @27750) . succ

instance OEIS 122494 where
  oeis = f (S.singleton (4, 2)) 27 [3..] where
     f s uu us@ (u:us'@ (u':_))
       | vv > uu = uu : f (S.insert (uu * u, u) s) (u' ^ u') us'
       | vv < uu = vv : f (S.insert (vv * v, v) s') uu us
       | otherwise = vv : f (S.insert (vv * v, v) s') (u' ^ u') us'
       where ((vv, v), s') = S.deleteFindMin s

instance OEIS 122516 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @46992)

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

instance OEIS 123125 where
  oeis = tablList @123125
instance Table 123125 where
  tabl = [1] : zipWith (:) [0, 0 ..] (tabl @8292)

instance OEIS 123346 where
  oeis = tablList @123346
instance Table 123346 where
  tabl = map reverse (tabl @11971)

instance OEIS 123921 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $
     map (flip (-) 2) $ zipWith (*) (oeis @40) (tail (oeis @40))

instance OEIS 124844 where
  oeis = tablList @124844
instance Table 124844 where
  tabl = zipWith (zipWith (*))
                         (tabl @7318) $ tail $ inits (oeis @61084)

instance OEIS 124927 where
  oeis = tablList @124927
instance Table 124927 where
  tabl = iterate
     (\row -> zipWith (+) ([0] ++ reverse row) (row ++ [1])) [1]

instance OEIS 125022 where
  oeis = elemIndices 1 (oeis @161)

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

instance OEIS 125308 where
  oeis = 3 : h [1,3] where
     h (u:us) | null (show (fi v) `intersect` "245679") &&
                (oeisIx @10051 . pred) v == 1 = v : h (us ++ [v])
              | otherwise       = h (us ++ [v])
              where v = u + 10

instance OEIS 125605 where
  oeis = tablList @125605
instance Table 125605 where
  tabl = iterate f [1] where
     f xs = zipWith (\v w -> (v + w) `div` gcd v w) ([0] ++ xs) (xs ++ [0])

instance OEIS 125717 where
  oeis =  0 : f [1..] 0 (M.singleton 0 0) where
     f (v:vs) w m = g (reverse[w-v,w- 2*v..1] ++ [w+v,w+2*v..]) where
       g (x:xs) = if x `M.member` m then g xs else x : f vs x (M.insert x v m)

instance OEIS 125855 where
  oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051 . pred)) $
     iterate (zipWith (+) [1, 1, 1, 1]) [1, 3, 7, 9]

instance OEIS 126025 where
  oeisIx (succ->n) = h n1s 0 where
     h us c = if us == nns then c + 1 else h (succ us) (c + g) where
       g = if and [f x `gcd` f y == f (x `gcd` y) |
                   x <- [1 .. n - 1], y <- [x + 1 .. n]] then 1 else 0
       f = (us !!) . subtract 1
     succ (z:zs) = if z < n then (z + 1) : zs else 1 : succ zs
     n1s = take n [1, 1 ..]; nns = take n [n, n ..]

instance OEIS 126064 where
  oeis = tablList @126064
instance Table 126064 where
  tabl =  zipWith (zipWith (*)) (tabl @94587) (tabl @59268)

instance OEIS 126256 where
  oeis = f (tabl @7318) [] where
     f (xs:xss) zs = g xs zs where
       g []     ys = length ys : f xss ys
       g (x:xs) ys = g xs (O.insertSet x ys)

instance OEIS 126257 where
  oeis = f [] (tabf @34868) where
     f zs (xs:xss) = (length ys) : f (ys `O.union` zs) xss
                     where ys = xs `O.minus` zs

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

instance OEIS 126890 where
  oeis = tablList @126890
instance Table 126890 where
  tabl = map fst $ iterate
     (\ (xs@ (x:_), i) -> (zipWith (+) ((x-i):xs) [2*i+1 ..], i+1)) ([0], 0)

instance OEIS 126988 where
  oeis = tablList @126988
instance Table 126988 where
  rowCol = rowCol_off @126988 @1 @1
  rowT   = rowT_off   @126988 @1
  tabl = zipWith (zipWith (*)) (tabl @10766) (tabl @51731)

instance OEIS 127013 where
  oeis = tablList @127013
instance Table 127013 where
  rowCol = rowCol_off @127013 @1 @1
  rowT   = rowT_off   @127013 @1
  tabl = map reverse (tabl @126988)

instance OEIS 127057 where
  oeis = tablList @127057
instance Table 127057 where
  rowCol = rowCol_off @127057 @1 @1
  rowT   = rowT_off   @127057 @1
  tabl = map (scanr1 (+)) (tabl @126988)

instance OEIS 127093 where
  oeis = tablList @127093
instance Table 127093 where
  rowCol n k = (rowT @127093) n !! (k- 1)
  rowT n = zipWith (*) [1..n] $ map ((0 ^) . (mod n)) [1..n]
  tabl = map (rowT @127093) [1..]

instance OEIS 127118 where
  oeisIx n = (oeisIx @40) n * (oeisIx @18252) n

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

instance OEIS 127446 where
  oeis = tablList @127446
instance Table 127446 where
  rowCol = rowCol_off @127446 @1 @1
  rowT   = rowT_off   @127446 @1
  tabl = zipWith (\v ws -> map (* v) ws) [1..] (tabl @51731)

instance OEIS 127542 where
  oeisIx = genericLength . filter ((== 1) . (oeisIx @10051 . pred) . sum) .
                            subsequences . enumFromTo 1 . succ

instance OEIS 127648 where
  oeis = tablList @127648
instance Table 127648 where
  tabl = map reverse $ iterate (\ (x:xs) -> x + 1 : 0 : xs) [1]

instance OEIS 127739 where
  oeis = tablList @127739
instance Table 127739 where
  rowCol = rowCol_off @127739 @1 @1
  rowT   = rowT_off   @127739 @1
  tabl = zipWith ($) (map replicate [1..]) $ tail (oeis @217)

instance OEIS 127824 where
  oeis = tablList @127824
instance Table 127824 where
  tabf = iterate f [1] where
     f row = sort $ map (* 2) row `union`
                    [x' | x <- row, let x' = (x - 1) `div` 3,
                          x' * 3 == x - 1, odd x', x' > 1]

instance OEIS 127899 where
  oeis = tablList @127899
instance Table 127899 where
  rowCol = rowCol_off @127899 @1 @1
  rowT   = rowT_off   @127899 @1
  tabl = map reverse ([1] : xss) where
     xss = iterate (\ (u : v : ws) -> u + 1 : v - 1 : ws ++ [0]) [2, -2]

instance OEIS 127936 where
  oeis = findIndices ((== 1) . (oeisIx @10051 . pred)) (oeis @7583)

instance OEIS 128014 where
  oeisIx = (oeisIx @984) . flip div 2

instance OEIS 128059 where
  oeisIx 0 = 1
  oeisIx n = f n n where
     f 1 _ = 1
     f x q = if (oeisIx @10051 . pred) q' == 1 then q' else f x' q'
             where x' = x - 1; q' = q + x'

instance OEIS 128174 where
  oeis = tablList @128174
instance Table 128174 where
  rowCol = rowCol_off @128174 @1 @1
  rowT   = rowT_off   @128174 @1
  tabl = iterate (\xs@ (x:_) -> (1 - x) : xs) [1]

instance OEIS 128588 where
  oeis = 1 : cows where
                     cows = 2 : 4 : zipWith (+) cows (tail cows)

instance OEIS 128924 where
  oeis = tablList @128924
instance Table 128924 where
  rowCol = rowCol_off @128924 @1 @1
  tabl = map (rowT @128924) [1..]
  rowT 1 = [1]
  rowT n = f [0..n - 1] $ group $ sort $ g 1 ps where
     f []     _                            = []
     f (v:vs) wss'@ (ws:wss) | head ws == v = length ws : f vs wss
                            | otherwise    = 0 : f vs wss'
     g 0 (1 : xs) = []
     g _ (x : xs) = x : g x xs
     ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` n) (tail ps) ps

instance OEIS 128966 where
  oeis = tablList @128966
instance Table 128966 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([0], [1, 1])

instance OEIS 129511 where
  oeis = filter (f [] . (rowT @27750)) [1..] where
     f _ [_] = True
     f zs (d:ds) = null (dds `O.isect` zs) && f (dds `O.union` zs) ds
                   where dds = map (subtract d) ds

instance OEIS 129512 where
  oeis = O.minus [1..] (oeis @129511)

instance OEIS 129521 where
  oeisIx n = p * (2 * p - 1) where p = (oeisIx @5382) n

instance OEIS 129713 where
  oeis = tablList @129713
instance Table 129713 where
  tabl = [1] : [1, 1] : f [1] [1, 1] where
     f us vs = ws : f vs ws where
               ws = zipWith (+) (init us ++ [0, 0, 0]) (vs ++ [1])

instance OEIS 129805 where
  oeis = [x | x <- (oeis @56020), (oeisIx @10051 . pred) x == 1]

instance OEIS 130130 where
  oeisIx = min 2
  oeis = 0 : 1 : repeat 2

instance OEIS 130321 where
  oeis = tablList @130321
instance Table 130321 where
  tabl = iterate (\row -> (2 * head row) : row) [1]

instance OEIS 130330 where
  oeis = tablList @130330
instance Table 130330 where
  rowCol n k = (rowT @130330) n !! (k- 1)
  rowT   = rowT_off   @130330 @1
  tabl = iterate (\xs -> (2 * head xs + 1) : xs) [1]

instance OEIS 130517 where
  oeis = tablList @130517
instance Table 130517 where
  rowCol = rowCol_off @130517 @1 @1
  rowT   = rowT_off   @130517 @1
  tabl = iterate (\row -> (head row + 1) : reverse row) [1]

instance OEIS 130658 where
  oeisIx = (+ 1) . (`div` 2) . (`mod` 4)
  oeis = cycle [1,1,2,2]

instance OEIS 130665 where
  oeisIx = sum . map (3 ^) . (`take` (oeis @120)) . (+ 1)

instance OEIS 130720 where
  oeisIx (succ->n)
    = fromJust
    $ find ((show (fi n) `isInfixOf`) . show . fi)
    $ tail
    $ scanl1 (+) [n..]

instance OEIS 131073 where
  oeis = 2 : f 2 1 where
     f x c = y : f y (c + (oeisIx @10051 . pred) y) where y = x + c

instance OEIS 131094 where
  oeis = tablList @131094
instance Table 131094 where
  rowCol = rowCol_off @131094 @1 @1
  rowT   = rowT_off   @131094 @1
  tabl = [2] : f 2 [2] where
     f v ws = ys : f (v + 1) ys where
              ys = take v $ nub $ sort $ concatMap h ws
     h z = [2 * z, 4 * z + 1, 4 * z' + b] where (z', b) = divMod z 2

instance OEIS 131095 where
  oeis = tablList @131095
instance Table 131095 where
  rowCol = rowCol_off @131095 @1 @1
  rowT   = rowT_off   @131095 @1
  tabl = [2] : [4, 9] : [17, 18, 20] : f 4 [17, 18, 20] where
     f v ws = ys : f (v + 1) ys where
       ys = take v $ dropWhile (<= last ws) $ nub $ sort $ concatMap h ws
     h z = [2 * z, 4 * z + 1, 4 * z' + b] where (z', b) = divMod z 2

instance OEIS 131134 where
  oeis = 1 : zipWith (\v w -> (v+w) `div` gcd v w) [2..] (oeis @131134)

instance OEIS 131205 where
  oeis = scanl1 (+) (oeis @123)

instance OEIS 131364 where
  oeisIx (fi->n) = p [r | r <- tail (oeis @10785), head (show $ fi r) `elem` show n] n
     where p _      0 = 1
           p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 131365 where
  oeis = elemIndices 0 (oeis @131364)

instance OEIS 131366 where
  oeis = tail $ findIndices (> 0) (oeis @131364)

instance OEIS 131410 where
  oeis = tablList @131410
instance Table 131410 where
  rowCol = rowCol_off @131410 @1 @1
  rowT   = rowT_off   @131410 @1
  tabl = zipWith replicate [1..] $ tail (oeis @45)

instance OEIS 131507 where
  oeis = tablList @131507
instance Table 131507 where
  tabl = zipWith ($) (map replicate [1..]) [1, 3 ..]

instance OEIS 131813 where
  oeis = 0 : f [[0]] where
     f xss = y : f (bin y : xss) where
       y = sum $ map (fi . fromEnum . (flip isInfixOf $ head xss)) xss
     bin n = if n == 0 then [] else b : bin n' where (n',b) = divMod n 2

instance OEIS 131816 where
  oeis = tablList @131816
instance Table 131816 where
  tabl = map (map (subtract 1)) $
     zipWith (zipWith (+)) (tabl @130321) (tabl @59268)

instance OEIS 131835 where
  oeis = concat $ iterate (concatMap (\x -> map (+ 10 * x) [0..9])) [1]

instance OEIS 132011 where
  oeisIx = p [1..] . succ where
     p _  0 = 1
     p (k:ks) m = if m < k then 0 else p [3 * k ..] (m - k) + p ks m

instance OEIS 132140 where
  oeis = filter f [1..] where
     f x = x < 3 && x == 1 || t > 0 && f x' where (x', t) = divMod x 3

instance OEIS 132159 where
  oeis = tablList @132159
instance Table 132159 where
  tabl = map reverse (tabl @121757)

instance OEIS 132171 where
  oeis = 1 : zs where
     zs = 3 : 3 : 3 : (map (* 3) $ concat $ transpose [zs, zs, zs])

instance OEIS 132213 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051 . pred) $
              nub $ genericTake n $ map (`mod` n) $ tail (oeis @290)

instance OEIS 132223 where
  oeis = f 1 [1] where
     f k xs = ys ++ f (2 * k) ys where
              ys = concat $ transpose [xs, reverse $ take k [k+1 ..]]

instance OEIS 132231 where
  oeis = [x | k <- [0..], let x = 30 * k + 7, (oeisIx @10051 . pred) x == 1]

instance OEIS 132240 where
  oeis = [x | x <- (oeis @175887), (oeisIx @10051 . pred) x == 1]

instance OEIS 132273 where
  oeisIx (succ->n) = sum $ zipWith (!!) coprimess (reverse [0..n - 1]) where
     coprimess = map (\x -> filter ((== 1) . (gcd x)) [1..]) [1..]

instance OEIS 132393 where
  oeis = tablList @132393
instance Table 132393 where
  tabl = map (map abs) (tabl @48994)

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

instance OEIS 132678 where
  oeis = elemIndices 1 (oeis @96535)

instance OEIS 132679 where
  oeis = f $ S.fromList [1,2] where
     f s = m : f (S.insert (4*m) $ S.insert (4*m+3) s') where
         (m,s') = S.deleteFindMin s

instance OEIS 132741 where
  oeisIx = f 2 1 . succ where
     f p y x | r == 0    = f p (y * p) x'
             | otherwise = if p == 2 then f 5 y x else y
             where (x', r) = divMod x p

instance OEIS 132813 where
  oeis = tablList @132813
instance Table 132813 where
  tabl = zipWith (zipWith (*)) (tabl @7318) $ tail (tabl @7318)

instance OEIS 133048 where
  oeisIx 0 = 0
  oeisIx n = train $ dropWhile (== 0) $ (rowT @31298) n where
     train []       = 1
     train [x]      = x
     train (u:v:ws) = u ^ v * (train ws)

instance OEIS 133280 where
  oeis = tablList @133280
instance Table 133280 where
  tabl = f 0 1 [0..] where
     f m j xs = (filter ((== m) . (`mod` 2)) ys) : f (1 - m) (j + 2) xs'
       where (ys,xs') = splitAt j xs

instance OEIS 133457 where
  oeis = tablList @133457
instance Table 133457 where
  rowCol = rowCol_off @133457 @1 @0
  rowT   = rowT_off @133457 @1
  tabf = map (fst . unzip . filter ((> 0) . snd) . zip [0..]) $
                     tail (tabf @30308)

instance OEIS 133500 where
  oeisIx = train . reverse . (rowT @31298) where
     train []       = 1
     train [x]      = x
     train (u:v:ws) = u ^ v * (train ws)

instance OEIS 133622 where
  -- oeisIx n = (1 - m) * n' + 1 where (n', m) = divMod n 2
  oeis = concat $ transpose [[1, 1 ..], [2 ..]]

instance OEIS 133808 where
  oeis = 1 : f (S.singleton (2, 2, 1)) where
     f s = y : f (S.insert (y * p, p, e + 1) $ S.insert (y * q^e, q, e) s')
               where q = (oeisIx @151800) p
                     ((y, p, e), s') = S.deleteFindMin s

instance OEIS 133809 where
  oeis = 1 : f (S.singleton (2, 2, 1)) where
     f s = y : f (S.insert (y*p, p, e+1) $ S.insert (y*q^ (e+1), q, e+1) s')
               where q = (oeisIx @151800) p
                     ((y, p, e), s') = S.deleteFindMin s

instance OEIS 133819 where
  oeis = tablList @133819
instance Table 133819 where
  rowCol = rowCol_off @133819 @1 @1
  rowT   = rowT_off   @133819 @1
  tabl = map (`take` (tail (oeis @290))) [1..]

instance OEIS 133820 where
  oeis = tablList @133820
instance Table 133820 where
  rowCol = rowCol_off @133820 @1 @1
  rowT   = rowT_off   @133820 @1
  tabl = map (`take` (tail (oeis @578))) [1..]

instance OEIS 133821 where
  oeis = tablList @133821
instance Table 133821 where
  rowCol = rowCol_off @133821 @1 @1
  rowT   = rowT_off   @133821 @1
  tabl = map (`take` (tail (oeis @583))) [1..]

instance OEIS 133870 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) [1,33..]

instance OEIS 133942 where
  oeis = zipWith (*) (oeis @142) $ cycle [1, -1]

instance OEIS 134239 where
  oeis = tablList @134239
instance Table 134239 where
  tabl = [1] : zipWith (map . (*))
                 [2..] (map reverse $ tail (tabl @29635))

instance OEIS 134323 where
  oeisIx n = (1 - 0 ^ m) * (-1) ^ (m + 1) where m = (oeisIx @40) n `mod` 3

instance OEIS 134636 where
  oeis = tablList @134636
instance Table 134636 where
  tabl = iterate (\row -> zipWith (+) ([2] ++ row) (row ++ [2])) [1]

instance OEIS 134640 where
  oeis = tablList @134640
instance Table 134640 where
  rowCol = rowCol_off @134640 @1 @1
  rowT n = sort $
     map (foldr (\dig val -> val * n + dig) 0) $ permutations [0 .. n - 1]
  tabf = map (rowT @134640) [1..]

instance OEIS 134941 where
  oeis = elemIndices 1 (oeis @178333)

instance OEIS 135287 where
  oeis = 1 : f 1 1 where
     f x y = z : f (x + 1) z where
          z = if m == 0 then y' else x + y; (y',m) = divMod y 2

instance OEIS 135837 where
  oeis = tablList @135837
instance Table 135837 where
  rowCol = rowCol_off @135837 @1 @1
  rowT   = rowT_off   @135837 @1
  tabl = [1] : [1, 2] : f [1] [1, 2] where
     f xs ys = ys' : f ys ys' where
       ys' = zipWith3 (\u v w -> 2 * u - v + 2 * w)
                      (ys ++ [0]) (xs ++ [0, 0]) ([0, 0] ++ xs)

instance OEIS 135851 where
  oeis = -1 : 0 : 1 : zipWith (+) (oeis @135851) (drop 2 (oeis @135851))

instance OEIS 136333 where
  oeis = filter (null . intersect "024568" . show . fi) [1..]

instance OEIS 136431 where
  oeis = tablList @136431
instance Table 136431 where
  tabl = map fst $ iterate h ([0], 1) where
     h (row, fib) = (zipWith (+) ([0] ++ row) (row ++ [fib]), last row)

instance OEIS 136522 where
  oeisIx n = fi . fromEnum $ n == (oeisIx @4086) n

instance OEIS 136572 where
  oeis = tablList @136572
instance Table 136572 where
  tabl = map fst $ iterate f ([1], 1) where
     f (row, i) = (0 : map (* i) row, i + 1)

instance OEIS 136798 where
  oeis = tail $ map (+ 1) $ elemIndices 1 $
     zipWith (*) (0 : (oeis @10051)) $ map (1 -) $ tail (oeis @10051)

instance OEIS 137291 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @49001).pred) [1..]

instance OEIS 137564 where
  oeisIx = f (-1) where
     f _ 0 = 0
     f r x = if d == r then f r x' else 10 * f d x' + d
             where (x', d) = divMod x 10

instance OEIS 137581 where
  oeisIx = (oeisIx @55641) . (oeisIx @4154)

instance OEIS 137921 where
  oeisIx (succ->n) = genericLength $ filter (> 0) $
     map ((mod n) . (+ 1)) [d | d <- [1..n], mod n d == 0]

instance OEIS 138166 where
  oeis = filter (\x -> show (fi $ oeisIx @55642 x) `isInfixOf` show (fi x)) [0..]

instance OEIS 138353 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ map (+ 9) (oeis @290)

instance OEIS 138510 where
  oeis = mapMaybe f [1..] where
    f x | (oeisIx @10051 . pred) q == 0 = Nothing
        | q == p          = Just 1
        | otherwise       = Just $
          head [b | b <- [2..], length (d b p) == length (d b q)]
        where q = div x p; p = (oeisIx @20639.pred) x
    d b = unfoldr (\z -> if z == 0 then Nothing else Just $ swap $ divMod z b)

instance OEIS 138511 where
  oeis = filter f [1..] where
                        f x = p ^ 2 < q && (oeisIx @10051 . pred) q == 1
                              where q = div x p; p = (oeisIx @20639.pred) x

instance OEIS 138530 where
  oeis = tablList @138530
instance Table 138530 where
  rowCol = rowCol_off @138530 @1 @1
  rowT   = rowT_off   @138530 @1
  tabl = zipWith (map . flip q) [1..] (tabl @2260) where
     q 1 n = n
     q b n = if n < b then n else q b n' + d where (n', d) = divMod n b

instance OEIS 138666 where
  oeis = map (head . tail) $
     filter (all (== 0) . map (oeisIx @10051 . pred) . tail) $ drop 2 (tabl @87401)

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

instance OEIS 139532 where
  oeis = [x | x <- [0..], (oeisIx @10051 . pred) (24 * x + 19) == 1]

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

instance OEIS 140750 where
  oeis = tablList @140750
instance Table 140750 where
  rowCol = rowCol_off @140750 @1 @1
  rowT   = rowT_off @140750 @1
  tabf = [1] : [1,1,1] : f [1] [1,1,1] where
     f ws vs = vs' : f vs vs' where
       vs' = zipWith3 (\r s x -> r + s + x)
             (vs ++ [0,0]) ([0,0] ++ ws ++ [0,0]) ([0,0] ++ vs)

instance OEIS 140978 where
  oeis = tablList @140978
instance Table 140978 where
  rowCol = rowCol_off @140978 @1 @1
  rowT   = rowT_off   @140978 @1
  tabl = map snd $ iterate (\ (i, xs@ (x:_)) -> (i + 2, map (+ i) (x:xs))) (5, [4])

instance OEIS 141169 where
  oeis = tablList @141169
instance Table 141169 where
  tabl = tail $ inits (oeis @45)

instance OEIS 141418 where
  oeis = tablList @141418
instance Table 141418 where
  rowCol n k = k * (2 * n - k - 1) `div` 2
  rowT   = rowT_off   @141418 @1
  tabl = map (scanl1 (+)) (tabl @25581)

instance OEIS 141419 where
  oeis = tablList @141419
instance Table 141419 where
  rowCol n k =  k * (2 * n - k + 1) `div` 2
  rowT   = rowT_off   @141419 @1
  tabl = map (scanl1 (+)) (tabl @4736)

instance OEIS 141468 where
  oeis = 0 : (oeis @18252)

instance OEIS 141686 where
  oeis = tablList @141686
instance Table 141686 where
  rowCol = rowCol_off @141686 @1 @1
  rowT   = rowT_off   @141686 @1
  tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @8292)

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

instance OEIS 142925 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) [1,65..]

instance OEIS 142978 where
  oeis = tablList @142978
instance Table 142978 where
  rowCol = rowCol_off @142978 @1 @1
  rowT   = rowT_off   @142978 @1
  tabl = map reverse (tabl @104698)

instance OEIS 143127 where
  oeis = scanl1 (+) (oeis @38040)

instance OEIS 143158 where
  oeis = tablList @143158
instance Table 143158 where
  rowCol = rowCol_off @143158 @1 @1
  rowT   = rowT_off   @143158 @1
  tabl = map (map sum . init . tails) (tabl @54527)

instance OEIS 143207 where
  oeis = f (S.singleton (2*3*5)) where
     f s = m : f (S.insert (2*m) $ S.insert (3*m) $ S.insert (5*m) s') where
       (m,s') = S.deleteFindMin s

instance OEIS 143333 where
  oeis = tablList @143333
instance Table 143333 where
  rowCol = rowCol_off @143333 @1 @1
  rowT   = rowT_off   @143333 @1
  tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @47999)

instance OEIS 143536 where
  oeis = tablList @143536
instance Table 143536 where
  rowCol = rowCol_off @143536 @1 @1
  rowT   = rowT_off   @143536 @1
  tabl = zipWith take [1..] $ map repeat (oeis @10051)

instance OEIS 143683 where
  oeis = tablList @143683
instance Table 143683 where
  tabl = map fst $ iterate
     (\ (us, vs) -> (vs, zipWith (+) (map (* 8) ([0] ++ us ++ [0])) $
                        zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

instance OEIS 144299 where
  oeis = tablList @144299
instance Table 144299 where
  tabl = [1] : [1, 0] : f 1 [1] [1, 0] where
     f i us vs = ws : f (i + 1) vs ws where
                 ws = (zipWith (+) (0 : map (i *) us) vs) ++ [0]

instance OEIS 144328 where
  oeis = tablList @144328
instance Table 144328 where
  rowCol = rowCol_off @144328 @1 @1
  rowT   = rowT_off   @144328 @1
  tabl = [1] : map (\xs@ (x:_) -> x : xs) (tabl @2260)

instance OEIS 144331 where
  oeis = tablList @144331
instance Table 144331 where
  tabf = iterate (\xs ->
    zipWith (+) ([0] ++ xs ++ [0]) $ zipWith (*) (0:[0..]) ([0,0] ++ xs)) [1]

instance OEIS 144394 where
  oeis = tablList @144394
instance Table 144394 where
  rowCol = rowCol_off @144394 @4 @0
  rowT = rowT_off @144394 @4
  tabl = map (drop 2 . reverse . drop 2) $ drop 4 (tabl @7318)

instance OEIS 144396 where
  oeisIx = (+ 1) . (* 2) . succ
  oeis = [3, 5 ..]

instance OEIS 144757 where
  oeisIx (succ->n) = (oeisIx @108) (oeisIx @1222 n - 1) * (oeisIx @8480) n

instance OEIS 144907 where
  oeisIx (succ->x)
    | (oeisIx @10051 . pred) x == 1 = 1
    | x `mod` 4 == 0 = 2 * rad
    | otherwise      = rad  where rad = (oeisIx @7947.pred) x

instance OEIS 144944 where
  oeis = tablList @144944
instance Table 144944 where
  tabl = iterate f [1] where
     f us = vs ++ [last vs] where
       vs = scanl1 (+) $ zipWith (+) us $ [0] ++ us

instance OEIS 144968 where
  oeis = zipWith (-) (tail (oeis @185549)) (oeis @185549)

instance OEIS 145011 where
  oeis = zipWith (-) (tail (oeis @7775)) (oeis @7775)

instance OEIS 145071 where
  -- oeisIx n = 2 ^ (n + 1) + n - 2
  oeis = scanl1 (+) $ tail (oeis @51)

instance OEIS 145204 where
  oeis = 0 : map (+ 1) (findIndices even (oeis @51064))

instance OEIS 145292 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @202018)

instance OEIS 145513 where
  oeis = f [1] where
     f xs = (p' xs $ last xs) : f (1 : map (* 10) xs)
     p' = memo2 (list integral) integral p
     p _ 0 = 1; p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m

instance OEIS 145654 where
  oeis = scanl1 (+) $ tail (oeis @918)

instance OEIS 145768 where
  oeis = map fi $ scanl1 xor $ map fi (oeis @290)

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

instance OEIS 151763 where
  oeisIx (succ->n)
    | even n           = 0
    | oeisIx @10051 (pred n) == 1 = 2 - n `mod` 4
    | otherwise        = 0

instance OEIS 151800 where
  oeisIx = oeisIx @7918 . succ

instance OEIS 152458 where
  oeis = [x | x <- [1..], (oeisIx @64413.pred) x == x]

instance OEIS 152749 where
  oeis = scanl1 (+) (oeis @109043)

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

instance OEIS 153727 where
  oeis = iterate (oeisIx @6370) 1

instance OEIS 153860 where
  oeis = tablList @153860
instance Table 153860 where
  rowCol = rowCol_off @153860 @1 @1
  rowT   = rowT_off   @153860 @1
  tabl = [1] : [0, 1] : iterate (\ (x:xs) -> -x : 0 : xs) [1, 1, 1]

instance OEIS 154314 where
  oeis = findIndices (/= 3) (oeis @212193)

instance OEIS 154530 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @248378)

instance OEIS 154809 where
  oeis = elemIndices 0 (oeis @178225)

instance OEIS 155038 where
  oeis = tablList @155038
instance Table 155038 where
  rowCol = rowCol_off @155038 @1 @1
  rowT   = rowT_off   @155038 @1
  tabl = iterate
     (\row -> zipWith (+) (row ++ [0]) (init row ++ [0,1])) [1]

instance OEIS 155043 where
  oeis = 0 : map ((+ 1) . (oeisIx @155043)) (oeis @49820)

instance OEIS 155161 where
  oeis = tablList @155161
instance Table 155161 where
  tabl = [1] : [0,1] : f [0] [0,1] where
     f us vs = ws : f vs ws where
       ws = zipWith (+) (us ++ [0,0]) $ zipWith (+) ([0] ++ vs) (vs ++ [0])

instance OEIS 156659 where
  oeisIx n = fi . fromEnum $ (oeisIx @10051 . pred) n == 1 && (oeisIx @10051 . pred) (n `div` 2) == 1

instance OEIS 156660 where
  oeisIx n = fi . fromEnum $ (oeisIx @10051 . pred) n == 1 && (oeisIx @10051 . pred) (2 * n + 1) == 1

instance OEIS 156685 where
  oeis = scanl1 (+) (oeis @24362)

instance OEIS 157037 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @3415)) [1..]

instance OEIS 157454 where
  oeis = tablList @157454
instance Table 157454 where
  rowCol = rowCol_off @157454 @1 @1
  rowT   = rowT_off   @157454 @1
  tabl = concatMap h $ tail $ inits [1, 3 ..] where
     h xs = [xs ++ tail xs', xs ++ xs'] where xs' = reverse xs

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

instance OEIS 158405 where
  oeis = tablList @158405
instance Table 158405 where
  -- rowCol = rolCol_off @158405 @0 @1
  rowT   = rowT_off   @158405 @1
  tabl = map reverse (tabl @99375)

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

instance OEIS 159693 where
  oeis = scanl1 (+) (oeis @463)

instance OEIS 159781 where
  oeis = map (+ 1) $ elemIndices 4 (oeis @24362)

instance OEIS 159918 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @120) . (oeisIx @290) $ n

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

instance OEIS 161187 where
  oeis = tail $ findIndices ((== 1) . (oeisIx @10052)) (oeis @89237)

instance OEIS 161188 where
  oeis = map (+ 1) $ findIndices ((== 1) . (oeisIx @10051 . pred)) $ tail (oeis @89237)

instance OEIS 161385 where
  oeisIx = (+ 1) . (oeisIx @161382)

instance OEIS 161390 where
  oeis = 0 : f (S.fromList [1..9]) where
     f s = x : f (s' `S.union`
           S.fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
       where (x, s') = S.deleteFindMin s
     digs = [0, 5, 4, 2, 9, 8, 6, 7, 3, 1]

instance OEIS 161896 where
  oeis = [x | x <- [1..], (9^x - 3*3^x - 4*x) `mod` (2*x* (2*x + 1)) == 0]

instance OEIS 161906 where
  oeis = tablList @161906
instance Table 161906 where
  rowCol = rowCol_off @161906 @1 @1
  rowT   = rowT_off @161906 @1
  tabf = zipWith (\m ds -> takeWhile ((<= m) . (^ 2)) ds)
                         [1..] (tabf @27750)

instance OEIS 161908 where
  oeis = tablList @161908
instance Table 161908 where
  rowCol = rowCol_off @161908 @1 @1
  rowT   = rowT_off @161908 @1
  tabf = zipWith (\x ds -> reverse $ map (div x) ds) [1..] (tabf @161906)

instance OEIS 162608 where
  oeis = tablList @162608
instance Table 162608 where
  tabl = map fst $ iterate f ([1], 1) where
     f (row, n) = (row' ++ [head row' + last row'], n + 1) where
       row' = map (* n) row

instance OEIS 162741 where
  oeis = tablList @162741
instance Table 162741 where
  rowCol = rowCol_off @162741 @1 @1
  rowT   = rowT_off @162741 @1
  tabf = iterate
     (\row -> zipWith (+) ([0] ++ row ++ [0]) (row ++ [0,1])) [1]

instance OEIS 162995 where
  oeis = tablList @162995
instance Table 162995 where
  rowCol = rowCol_off @162995 @1 @1
  rowT   = rowT_off   @162995 @1
  tabl = map fst $ iterate f ([1], 3)
     where f (row, i) = (map (* i) row ++ [1], i + 1)

instance OEIS 163575 where
  oeisIx (succ->n) = f n' where
     f 0 = 0
     f x = if b == parity then f x' else x  where (x', b) = divMod x 2
     (n', parity) = divMod n 2

instance OEIS 163870 where
  oeis = tablList @163870
instance Table 163870 where
  rowCol = rowCol_off @163870 @1 @1
  rowT   = rowT_off @163870 @1
  tabf = filter (not . null) $ map tail (tabf @27751)

instance OEIS 163925 where
  oeis = tablList @163925
instance Table 163925 where
  rowCol = rowCol_off @163925 @1 @1
  tabf = map (rowT @163925) [1..]
  rowT n = [k | k <- takeWhile (<= n ^ 2) (oeis @18252),
                       let k' = k * n, let divs = (rowT @27750) k',
                       last (takeWhile ((<= k') . (^ 2)) divs) == n]

instance OEIS 164338 where
  oeis = iterate (oeisIx @36839) 12334444

instance OEIS 164349 where
  oeisIx n = if n == 0 then 0 else until (<= 1) (oeisIx @53645 . subtract 2) n

instance OEIS 164874 where
  oeis = tablList @164874
instance Table 164874 where
  rowCol = rowCol_off @164874 @1 @1
  rowT   = rowT_off   @164874 @1
  tabl = map reverse $ iterate f [2] where
     f xs@ (x:_) = (2 * x + 2) : map ((+ 1) . (* 2)) xs

instance OEIS 165416 where
  oeis = tablList @165416
instance Table 165416 where
  rowCol = rowCol_off @165416 @1 @1
  rowT   = rowT_off @165416 @1
  tabf   = map (dropWhile (== 0)) $ tail (tabf @119709)

instance OEIS 165560 where
  oeisIx = flip mod 2 . (oeisIx @3415)

instance OEIS 166234 where
  oeisIx 0 = 1
  oeisIx n
    = product . map (oeisIx @8683 . pred) $ rowT @124010 (n+1)

instance OEIS 166350 where
  oeis = tablList @166350
instance Table 166350 where
  rowCol = rowCol_off @166350 @1 @1
  rowT   = rowT_off   @166350 @1
  tabl = tail $ inits $ tail (oeis @142)

instance OEIS 166360 where
  oeis = tablList @166360
instance Table 166360 where
  rowCol = rowCol_off @166360 @1 @1
  rowT   = rowT_off   @166360 @1
  tabl = map (map (flip mod 2)) (tabl @1263)

instance OEIS 166863 where
  oeis = 1 : zipWith (+) (oeis @166863) (drop 3 $ map (* 2) (oeis @45))

instance OEIS 167376 where
  oeis = O.minus [0..] (oeis @41)

instance OEIS 167377 where
  oeis = O.minus [0..] (oeis @9)

instance OEIS 167392 where
  oeisIx = fi . fromEnum . flip O.member (oeis @41)

instance OEIS 167393 where
  oeisIx = fi . fromEnum . flip O.member (oeis @9)

instance OEIS 168183 where
  oeis = [1..8] ++ map (+ 9) (oeis @168183)

instance OEIS 168184 where
  oeisIx = (1 -) . (0 ^) . (`mod` 10)
  oeis = cycle [0,1,1,1,1,1,1,1,1,1]

instance OEIS 168396 where
  oeis = tablList @168396
instance Table 168396 where
  rowCol = rowCol_off @168396 @1 @1
  rowT   = rowT_off   @168396 @1
  tabl = [1] : f [[1]] where
     f xss = ys : f (ys : xss) where
       ys = (map sum $ zipWith take [2..] xss) ++ [1]

instance OEIS 169837 where
  oeis = 3 : ekg 3 (2 : [4..]) where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 169849 where
  oeis = 9 : ekg 9 (delete 9 [2..]) where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 169964 where
  oeis = map (* 5) (oeis @7088)

instance OEIS 169965 where
  oeis = map (* 2) (oeis @7088)

instance OEIS 169966 where
  oeis = map (* 3) (oeis @7088)

instance OEIS 169967 where
  oeis = map (* 4) (oeis @7088)

instance OEIS 170803 where
  oeis = scanl1 (+) (oeis @6899)

instance OEIS 171637 where
  oeis = tablList @171637
instance Table 171637 where
  rowCol = rowCol_off @171637 @2 @1
  tabf = map (rowT @171637) [2..]
  rowT n = reverse $ filter ((== 1) . (oeisIx @10051 . pred)) $
     map (2 * n -) $ takeWhile (<= 2 * n) (oeis @40)

instance OEIS 171901 where
  oeis = elemIndices 0 (oeis @196368)

instance OEIS 171946 where
  oeis = 0 : f [2..] where
     f (w:ws) = w : f (delete (2 * w - 1) ws)

instance OEIS 171947 where
  oeis = 1 : f [2..] where
     f (w:ws) = y : f (delete y ws) where y = 2 * w - 1

instance OEIS 173019 where
  oeisIx = foldr (\t v -> 3 * v + t) 0 . (rowT @83093)

instance OEIS 173333 where
  oeis = tablList @173333
instance Table 173333 where
  rowCol = rowCol_off @173333 @1 @1
  rowT   = rowT_off   @173333 @1
  tabl = map fst $ iterate f ([1], 2)
     where f (row, i) = (map (* i) row ++ [1], i + 1)

instance OEIS 173540 where
  oeis = tablList @173540
instance Table 173540 where
  rowCol n k = (rowT @173540) n !! (k- 1)
  rowT   = rowT_off @173540 @1
  tabf = [0] : [0] : map
                 (\v -> [w | w <- [2 .. v - 1], mod v w > 0]) [3..]

instance OEIS 173541 where
  oeis = tablList @173541
instance Table 173541 where
  rowCol = rowCol_off @173541 @1 @1
  rowT   = rowT_off   @173541 @1
  tabl = zipWith (zipWith (*))
                         (tabl @2260) $ map (map (1 -)) (tabl @51731)

instance OEIS 173964 where
  oeis = concat $ [1] : f [[1]] where
     f xss = yss ++ f yss where
       yss = [y] : map (++ [y]) xss
       y = head (head xss) + 1

instance OEIS 174382 where
  oeis = tablList @174382
instance Table 174382 where
  rowCol = rowCol_off @174382 @1 @0
  rowT   = rowT_off @174382 @1
  tabf = iterate f [0] where
     f xs = g (xs ++ [0, 0 ..]) [0..] (map head zs) (map length zs)
       where g _ _ _ [] = []
             g (u:us) (k:ks) hs'@ (h:hs) vs'@ (v:vs)
               | k == h = u + v : g us ks hs vs
               | k /= h = u : g us ks hs' vs'
             zs = group $ sort xs

instance OEIS 174466 where
  oeisIx (id->n)
    = sum $ zipWith3 (((*) .) . (*))
        divs (map (oeisIx @203 . pred) $ reverse divs) (map (oeisIx @5 . pred) divs)
    where divs = (rowT @27750 . succ) n

instance OEIS 174813 where
  oeis = f [1] where
     f ds = foldr (\d v -> 10 * v + d) 0 ds : f (s ds)
     s [] = [1]; s (9:ds) = 1 : s ds; s (d:ds) = 3*d : ds

instance OEIS 174863 where
  oeis = scanl1 (+) (oeis @76479)

instance OEIS 175130 where
  oeis = map (+ 1) $ findIndices ((== 0) . (oeisIx @212793.pred)) $ tail (oeis @45)

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

instance OEIS 175755 where
  oeis = m (map (^ 48) (oeis @40)) (map (^ 6) (oeis @6881)) where
     m xs'@ (x:xs) ys'@ (y:ys) | x < y = x : m xs ys'
                             | otherwise = y : m xs' ys

instance OEIS 175836 where
  oeis = scanl1 (*) (oeis @1615)

instance OEIS 175840 where
  oeis = tablList @175840
instance Table 175840 where
  tabf = iterate (\xs@ (x:_) -> x * 3 : map (* 2) xs) [1]

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

instance OEIS 175943 where
  oeis = scanl1 (*) $ concat (tabf @27746)

instance OEIS 175965 where
  oeis = scanl (+) 1 (oeis @8578)

instance OEIS 175967 where
  oeis = scanl (+) 1 (oeis @18252)

instance OEIS 176271 where
  oeis = tablList @176271
instance Table 176271 where
  rowCol = rowCol_off @176271 @1 @1
  rowT   = rowT_off   @176271 @1
  tabl = f 1 (oeis @5408) where
     f x ws = us : f (x + 1) vs where (us, vs) = splitAt x ws

instance OEIS 176352 where
  oeis = 1 : f 1 (S.singleton 1) (concat $ drop 2 $ zipWith (zipWith (%)) (tabf @38566) $ map reverse (tabf @38566))
     where f x ws qs = h qs
             where h (r:rs) | denominator y /= 1 || v `S.member` ws = h rs
                            | otherwise = v : f y (S.insert v ws) (delete r qs)
                            where v = numerator y; y = x * r

instance OEIS 177853 where
  oeis = scanl1 (+) (oeis @18805)

instance OEIS 177994 where
  oeis = tablList @177994
instance Table 177994 where
  tabl = [1] : [1,1] : map f (tabl @177994)
                 where f xs@ (x:_) = (x + 1) : 1 : xs

instance OEIS 178063 where
  oeis = scanl1 (+) (oeis @7464)

instance OEIS 178138 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 4

instance OEIS 178156 where
  oeis = insert 9 $ insert 8 (oeis @1751)

instance OEIS 178225 where
  oeisIx n = fi . fromEnum $ n == (oeisIx @30101) n

instance OEIS 178333 where
  oeisIx (fi->n) = fi . fromEnum $
     n `mod` 10 == 1 && (oeisIx @30) n == 1 && (oeisIx @196368) n == 1 && and down where
        down = dropWhile (== False) $ zipWith (<) (tail $ show n) (show n)

instance OEIS 178787 where
  oeis = scanl1 (+) (oeis @178788)

instance OEIS 178804 where
  oeis = concat $ transpose [oeis @8619, (oeis @27)]

instance OEIS 178830 where
  oeisIx 0 = 0
  oeisIx (succ->n) = z [1..n] (n * (n + 1) `div` 2) 1 where
     z []     s p             = fi $ fromEnum (s == p)
     z (x:xs) s p | s > p     = z xs (s - x) (p * x) + z xs s p
                  | otherwise = fi $ fromEnum (s == p)

instance OEIS 178943 where
  oeis = 2 : h (oeis @40) where
     h (p:qs@ (q:r:ps)) = if 2 * q /= (p + r) then q : h qs else h qs

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

instance OEIS 180149 where
  oeis = elemIndices 2 (oeis @2635)

instance OEIS 180639 where
  oeis = (0:) . scanl1 (+) $ map ((1 -) . (oeisIx @264739)) [1..]

instance OEIS 180662 where
  oeis = tablList @180662
instance Table 180662 where
  tabl = tail $ inits (oeis @1654)

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

instance OEIS 181900 where
  oeisIx n = (oeisIx @22998) n * n

instance OEIS 181935 where
  oeisIx 0 = 1
  oeisIx n = curling $ unfoldr
     (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) n where
     curling zs = maximum $ zipWith (\xs ys -> strip 1 xs ys)
                            (tail $ inits zs) (tail $ tails zs) where
        strip i us vs | vs' == Nothing = i
                      | otherwise      = strip (i + 1) us $ fromJust vs'
                      where vs' = stripPrefix us vs

instance OEIS 181983 where
  oeisIx = negate . (oeisIx @38608)
  oeis = [0, 1] ++ map negate
     (zipWith (+) (oeis @181983) (map (* 2) $ tail (oeis @181983)))

instance OEIS 182028 where
  oeis = scanl1 (\v b -> 2 * v + b) (oeis @3849)

instance OEIS 182086 where
  oeisIx = p [1,2,5,10,50,100,200,500] where
     p _ 0 = 1; p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 182093 where
  oeis = scanl1 (+) (oeis @5590)

instance OEIS 182210 where
  oeis = tablList @182210
instance Table 182210 where
  rowCol = rowCol_off @182210 @1 @1
  tabl = [[k* (n+1) `div` (k+1) | k <- [1..n]] | n <- [1..]]

instance OEIS 182455 where
  oeis = 1 : zipWith (*) (zipWith mod (oeis @182455) [3..]) [3..]

instance OEIS 182579 where
  oeis = tablList @182579
instance Table 182579 where
  tabl = [1] : iterate (\row ->
    zipWith (+) ([0] ++ row) (zipWith (*) (row ++ [0]) (oeis @59841))) [1,2]

instance OEIS 182972 where
  oeis = map fst $ concatMap q [3..] where
     q x = [ (num, den) | num <- [1 .. div x 2],
                         let den = x - num, gcd num den == 1]

instance OEIS 182973 where
  oeis = map snd $ concatMap q [3..] where
     q x = [ (num, den) | num <- [1 .. div x 2],
                         let den = x - num, gcd num den == 1]

instance OEIS 183209 where
  oeis = tablList @183209
instance Table 183209 where
  rowCol = rowCol_off @183209 @1 @1
  rowT   = rowT_off @183209 @1
  tabf = [1] : iterate (\xs -> concat $
     transpose [map (oeisIx @32766) xs, map (oeisIx @16789 . subtract 1) xs]) [2]

instance OEIS 185550 where
  oeis = [0..] `O.minus` (oeis @185549)

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

instance OEIS 186826 where
  oeis = tablList @186826
instance Table 186826 where
  tabl = map reverse (tabl @144944)

instance OEIS 187072 where
  oeis = goldbach 0 (oeis @65091) S.empty where
    goldbach q (p:ps) gbEven
        | qp `S.member` gbEven = goldbach q ps gbEven
        | otherwise          = p : goldbach p (oeis @65091) (S.insert qp gbEven)
        where qp = q + p

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

instance OEIS 187790 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @93506)) . succ

instance OEIS 187792 where
  oeis = filter (\x -> (oeisIx @93506 . pred) x == x) [1..]

instance OEIS 187811 where
  oeis = map (+ 1) $ findIndices (> 0) (oeis @5091)

instance OEIS 187831 where
  oeisIx 0 = 1
  oeisIx n = head $ fromJust $
          find (n `elem`) $ genericDrop n (tabf @70165)

instance OEIS 187845 where
  oeis = scanl1 (+) $ map (oeisIx @187844) [0..]

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

instance OEIS 188915 where
  oeis = O.union (oeis @290) (oeis @79)

instance OEIS 188967 where
  oeis = 0 : zipWith ($)
                     (cycle [ (1 -) . (oeisIx @188967 . pred), (1 -) . (oeisIx @188967 . pred), (oeisIx @188967 . pred)])
                     (concat $ transpose [[1, 3 ..], [2, 4 ..], [2 ..]])

instance OEIS 189711 where
  oeis = tablList @189711
instance Table 189711 where
  rowCol n k = (n - k) ^ k - 2 * (rowCol @7318) (n - 1) k + n - k
  rowT n = map (rowCol @189711 n) [3..n - 2]
  tabl = map (rowT @189711) [5..]

instance OEIS 189920 where
  oeis = tablList @189920
instance Table 189920 where
  rowT n = z n $ reverse $ takeWhile (<= n) $ tail (oeis @45) where
     z x (f:fs'@ (_:fs)) | f == 1 = if x == 1 then [1] else []
                        | f == x = 1 : replicate (length fs) 0
                        | f < x  = 1 : 0 : z (x - f) fs
                        | f > x  = 0 : z x fs'
  tabf = map (rowT @189920) [1..]

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

instance OEIS 191587 where
  oeisIx (succ->n) = head [p | p <- dropWhile (<= n) (oeis @40),
                        (oeisIx @1221 . pred) (p - n) == n]

instance OEIS 191854 where
  oeisIx = (oeisIx @7966) . (oeisIx @7969)

instance OEIS 191855 where
  oeisIx = (oeisIx @7967) . (oeisIx @7969)

instance OEIS 191856 where
  oeisIx = (oeisIx @7966) . (oeisIx @7970)

instance OEIS 191857 where
  oeisIx = (oeisIx @7967) . (oeisIx @7970)

instance OEIS 191860 where
  oeisIx = fst . sqrtPair . (oeisIx @7969)

instance OEIS 191861 where
  oeisIx = snd . sqrtPair . (oeisIx @7969)

instance OEIS 191862 where
  oeisIx = fst . sqrtPair . (oeisIx @7970)

instance OEIS 191863 where
  oeisIx = snd . sqrtPair . (oeisIx @7970)

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

instance OEIS 192545 where
  oeis = map (+2) $ elemIndices 0 $ map (oeisIx @48853) [1..]

instance OEIS 192687 where
  oeis = zipWith (-) females males where
     females = 1 : zipWith (-) [1..] (map (males !!) females)
     males = 0 : zipWith (-) [1..] (map (females !!) males)

instance OEIS 192735 where
  oeisIx (succ->n) = head $ (tabl @33291) !! (n - 1)
instance OEIS 192736 where
  oeisIx (succ->n) = last $ (tabl @33291) !! (n - 1)

instance OEIS 193331 where
  oeis = tablList @193331
instance Table 193331 where
  rowCol = rowCol_off @193331 @1 @1
  tabl = map (rowT @193331) [1..]
  rowT n = zipWith div (map (* n^2) [0..n - 1]) (map (2 *) [1..n])

instance OEIS 193596 where
  oeis = tablList @193596
instance Table 193596 where
  tabl = map (map ((flip div 2) . (+ 1))) (tabl @7318)

instance OEIS 193641 where
  oeis = drop 2 xs where
     xs = 1 : 1 : 1 : zipWith (+) xs (map (* 2) $ drop 2 xs)

instance OEIS 193711 where
  oeis = scanl1 (+) (oeis @5214)

instance OEIS 193773 where
  oeisIx n = genericLength [ () | x <- [1 .. n + 1],
                           let (y,m) = divMod (x + n) (2 * x - 1),
                           x <= y, m == 0]

instance OEIS 193854 where
  oeis = elemIndices 1 (oeis @62039)

instance OEIS 193891 where
  oeis = tablList @193891
instance Table 193891 where
  tabl = [1] : map fst (iterate
     (\ (xs, i) -> (zipWith (+) (0:xs) [i, 2 * i ..], i + 1)) ([1,2], 2))

instance OEIS 193926 where
  oeis = zipWith (-) (tail (oeis @62039)) (oeis @62039)

instance OEIS 194005 where
  oeis = tablList @194005
instance Table 194005 where
  tabl = [1] : [1,1] : f [1] [1,1] where
     f row' row = rs : f row rs where
       rs = zipWith (+) ([0,1] ++ row') (row ++ [0])

instance OEIS 195013 where
  oeis = concat $ transpose [[2, 4 ..], [3, 6 ..]]

instance OEIS 195238 where
  oeis = filter (\x -> (oeisIx @1221 . pred) x `elem` [2,3] &&
                               (oeisIx @6530 . pred) x `elem` [5,7] &&
                               (mod x 7 == 0 || mod x 15 == 0)) [1..]

instance OEIS 195376 where
  oeis = map (`mod` 2) (oeis @64413)

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

instance OEIS 196486 where
  oeis = tablList @196486
instance Table 196486 where
  rowCol = rowCol_off @196486 @1 @1
  rowT   = rowT_off @196486 @1
  tabf = map (tail . reverse) $ tail (tabf @227048)

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

instance OEIS 197181 where
  oeis = map (+ 1) $ elemIndices 0 $
     zipWith mod (map (oeisIx @66459) [1..]) [1..]

instance OEIS 197410 where
  oeisIx = product . scanl1 (+) . (rowT @27750) . succ

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

instance OEIS 199162 where
  oeis = 1 : 6 : ulam 2 6 (oeis @199162)

instance OEIS 199333 where
  oeis = tablList @199333
instance Table 199333 where
  tabl = iterate
     (\row -> map (oeisIx @159477 . pred) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 200737 where
  oeis = tablList @200737
instance Table 200737 where
  rowCol = rowCol_off @200737 @1 @1
  rowT n = sort
     [v*w + w*u + u*v | w <- [1..n], v <- [1..w], u <- [1..v]]
  tabl = map (rowT @200737) [1..]

instance OEIS 200741 where
  oeis = tablList @200741
instance Table 200741 where
  rowCol = rowCol_off @200741 @1 @1
  rowT = nub . (rowT @200737)
  tabl = map (rowT @200741) [1..]

instance OEIS 200745 where
  oeisIx n = p [nd | nd <- [1..n], mod n nd /= 0] n where
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m | m < k = 0 | otherwise = p ks (m - k) + p ks m

instance OEIS 201208 where
  oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @34)

instance OEIS 201266 where
  oeisIx n = [d | d <- [1..], (oeisIx @175755) n `mod` d == 0] !! 6

instance OEIS 201629 where
  oeisIx = (* 2) . (oeisIx @4524) . (+ 1)

instance OEIS 201634 where
  oeis = tablList @201634
instance Table 201634 where
  tabl = iterate (\xs -> scanl1 (+) xs ++ [2 * last xs]) [1]

instance OEIS 201881 where
  oeis = map length $ group (oeis @7061)

instance OEIS 202018 where
  oeisIx = (+ 41) . (oeisIx @2378)
  oeis   = map (+ 41) (oeis @2378)

instance OEIS 202089 where
  oeis = elemIndices 0 (oeis @240752)

instance OEIS 202340 where
  oeis = map length $ group (oeis @5374)

instance OEIS 202341 where
  oeis = elemIndices 1 (oeis @202340)

instance OEIS 202342 where
  oeis = elemIndices 2 (oeis @202340)

instance OEIS 203400 where
  oeis = scanl1 (+) (oeis @50935)

instance OEIS 203463 where
  oeis = elemIndices 1 (oeis @20985)

instance OEIS 203531 where
  oeis = map length $ group (oeis @20985)

instance OEIS 203602 where
  oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @92401)) [1..]

instance OEIS 203776 where
  oeisIx = p (oeis @47209) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 203908 where
  oeisIx (succ->n) = product $ map abs $
              zipWith (-) (rowT @27748 n) (rowT @124010 n)

instance OEIS 204293 where
  oeis = tablList @204293
instance Table 204293 where
  tabl = [1] : [0,0] : f [1] [0,0] where
     f xs ys = xs' : f ys xs' where
       xs' = zipWith (+) ([0,0] ++ xs) (xs ++ [0,0])

instance OEIS 205667 where
  oeis = map succ $ elemIndices 1 (oeis @39997)

instance OEIS 206282 where
  oeis = 1 : 1 : -1 : -4 :
     zipWith div
       (zipWith (+)
         (zipWith (*) (drop 3 (oeis @206282))
                      (drop 1 (oeis @206282)))
         (drop 2 (oeis @206282)))
       (oeis @206282)

instance OEIS 206424 where
  oeisIx = genericLength . filter (== 1) . (rowT @83093)

instance OEIS 206778 where
  oeis = tablList @206778
instance Table 206778 where
  rowCol n k = (rowT @206778) n !! k
  rowT = filter ((== 1) . (oeisIx @8966 . pred)) . (rowT @27750)
  tabf = map (rowT @206778) [1..]

instance OEIS 206913 where
  oeisIx n = last $ takeWhile (<= n) (oeis @6995)

instance OEIS 206914 where
  oeisIx n = head $ dropWhile (< n) (oeis @6995)

instance OEIS 206920 where
  oeis = scanl1 (+) (oeis @6995)

instance OEIS 208101 where
  oeis = tablList @208101
instance Table 208101 where
  tabl =  iterate
     (\row -> zipWith (+) ([0,1] ++ init row) (row ++ [0])) [1]

instance OEIS 208239 where
  oeis = tablList @208239
instance Table 208239 where
  rowT n = map (+ n) $ zipWith (-) divs $ reverse divs
                  where divs = (rowT @27750) n
  tabl = map (rowT @208239) [1..]

instance OEIS 208241 where
  oeis = f nns $ filter ((== 1) . (oeisIx @10051 . pred) . fst) nns where
     f mms'@ ((m,ms):mms) pps'@ ((p,ps):pps) =
       if m == p then f mms' pps else q : f mms pps'
       where q = fst $ fromJust $ find ((ms `isPrefixOf`) . snd) pps'
     nns = zip [1..] $ map reverse $ tail (tabf @30308)

instance OEIS 208245 where
  oeis = tablList @208245
instance Table 208245 where
  rowCol = rowCol_off @208245 @1 @1
  rowT   = rowT_off   @208245 @1
  tabl = map fst $ iterate f ([1], [1, 1]) where
     f (us, vs) = (vs, zipWith (+) ([0] ++ us ++ [0]) (us ++ [0, 1]))

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

instance OEIS 209561 where
  oeis = tablList @209561
instance Table 209561 where
  rowCol = rowCol_off @209561 @1 @1
  rowT   = rowT_off   @209561 @1
  tabl = [1] : iterate
                 (\row -> zipWith (+) ([1] ++ row) (row ++ [0])) [1,1]

instance OEIS 209675 where
  oeisIx = (oeisIx @3484 . succ) . (* 2)

instance OEIS 209802 where
  oeis = scanl1 (+) (oeis @166234)

instance OEIS 209815 where
  oeisIx (succ->n) = p [1..n - 1] (2*n) where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 209816 where
  oeisIx (succ->n) = p [1..n] (2*n) where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 209878 where
  oeis = iterate (oeisIx @36839) 20169

instance OEIS 209879 where
  oeis = iterate (oeisIx @36839) 6999

instance OEIS 209880 where
  oeis = iterate (oeisIx @36839) 29

instance OEIS 209933 where
  oeis = filter f [1..] where
     f x = head (ds x) /= 0 && all (== 0) (map ((mod x)) (ds x)) where
       ds = sort . nub . concatMap (unfoldr (\z ->
            if z == 0 then Nothing else Just $ swap $ divMod z 10)) .
            (rowT @27750)

instance OEIS 210108 where
  oeis = tablList @210108
instance Table 210108 where
  tabl = zipWith take [1..] (tabf @8301)

instance OEIS 210111 where
  oeis = tablList @210111
instance Table 210111 where
  tabl = zipWith take [1..] (tabf @125053)

instance OEIS 210208 where
  oeis = tablList @210208
instance Table 210208 where
  rowCol = rowCol_off @210208 @1 @1
  rowT   = rowT_off @210208 @1
  tabf = map (filter ((== 1) . (oeisIx @10055 . pred))) (tabf @27750)

instance OEIS 210241 where
  oeis = scanl1 (+) (oeis @73093)

instance OEIS 210461 where
  oeisIx = (`div` 8) . (subtract 1) . (9 ^) . (oeisIx @65091)

instance OEIS 210770 where
  oeis = 1 : 2 : f 1 2 [3..] where
     f u v (w:ws) = u' : w : f u' w (delete u' ws) where u' = v + w

instance OEIS 211520 where
  oeis = 0 : 0 : 0 : scanl1 (+) (oeis @178804)

instance OEIS 211538 where
  oeis = scanl (+) 0 (oeis @29578)

instance OEIS 212193 where
  oeisIx n = f n [0..3] where
     f x ys | x <= 2    = head $ delete x ys
            | otherwise = f x' $ delete d ys where (x',d) = divMod x 3

instance OEIS 212210 where
  oeis = tablList @212210
instance Table 212210 where
  rowCol = rowCol_off @212210 @1 @1
  rowT   = rowT_off   @212210 @1
  tabl = f $ tail $ zip (inits pis) (tails pis) where
     f ((xs,ys) : zss) = (zipWith (-) (map (+ last xs) (xs)) ys) : f zss
     pis = (oeis @720)

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

instance OEIS 212793 where
  oeisIx = cubeFree (oeis @40) 0 0 . succ where
     cubeFree ps'@ (p:ps) q e x
        | e > 2     = 0
        | x == 1    = 1
        | r > 0     = cubeFree ps  p 0 x
        | otherwise = cubeFree ps' p (e + 1) x' where (x', r) = divMod x p

instance OEIS 213517 where
  oeis = filter ((<= 2) . (oeisIx @118668)) [0..]

instance OEIS 213518 where
  oeis = elemIndices 2 (oeis @118668)

instance OEIS 213629 where
  oeis = tablList @213629
instance Table 213629 where
  rowCol = rowCol_off @213629 @1 @1
  rowT   = rowT_off   @213629 @1
  tabl = map f $ tail $ inits $ tail $ map reverse (tabf @30308) where
     f xss = map (\xs ->
             sum $ map (fi . fromEnum . (xs `isPrefixOf`)) $ tails $ last xss) xss

instance OEIS 213676 where
  oeis = tablList @213676
instance Table 213676 where
  tabf = [0] : map reverse (tabf @189920)

instance OEIS 213714 where
  oeis = f [0..] (oeis @5187) 0 where
     f (x:xs) ys'@ (y:ys) i | x == y    = i : f xs ys (i+1)
                           | otherwise = 0 : f xs ys' i

instance OEIS 213723 where
  oeisIx = (* 2) . (oeisIx @213714)

instance OEIS 213724 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @213723) n + signum (oeisIx @213723 n)

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

instance OEIS 214075 where
  oeis = tablList @214075
instance Table 214075 where
  tabl = zipWith (zipWith div) (tabl @213998) (tabl @213999)

instance OEIS 214084 where
  oeis = tablList @214084
instance Table 214084 where
  tabf = zipWith enumFromTo (oeis @290) (oeis @578)

instance OEIS 214090 where
  oeisIx = (`mod` 2) . (oeisIx @9947)

instance OEIS 214178 where
  oeis = tablList @214178
instance Table 214178 where
  tabl = [0] : map f (tabl @37027) where
     f row = (zipWith (*) (oeis @142) row) ++ [0]

instance OEIS 214292 where
  oeis = tablList @214292
instance Table 214292 where
  tabl = map diff $ tail (tabl @7318)
     where diff row = zipWith (-) (tail row) row

instance OEIS 214322 where
  oeis = 1 : 1 : 1 : zipWith (+) (oeis @214551) (drop 2 (oeis @214551))

instance OEIS 214323 where
  oeis = 1 : 1 : 1 : zipWith gcd (oeis @214551) (drop 2 (oeis @214551))

instance OEIS 214390 where
  oeis = map numerator $ scanl1 (+) $ map (1 %) (oeis @45542)

instance OEIS 214391 where
  oeis = map denominator $ scanl1 (+) $ map (1 %) (oeis @45542)

instance OEIS 214414 where
  oeis = [x | x <- [0..], (oeisIx @105027) x == x]

instance OEIS 214416 where
  oeisIx = fromJust . (`elemIndex` (oeis @105025))

instance OEIS 214417 where
  oeisIx = fromJust . (`elemIndex` (oeis @105027))

instance OEIS 214551 where
  oeis = 1 : 1 : 1 : zipWith f (oeis @214551) (drop 2 (oeis @214551))
     where f u v = (u + v) `div` gcd u v

instance OEIS 214604 where
  oeis = tablList @214604
instance Table 214604 where
  rowCol = rowCol_off @214604 @1 @1
  rowT   = rowT_off   @214604 @1
  tabl = zipWith take [1..] $ transpose (tabl @176271)

instance OEIS 214614 where
  oeis = tablList @214614
instance Table 214614 where
  rowCol = rowCol_off @214614 @1 @1
  rowT   = rowT_off @214614 @1
  tabf = zipWith f [1..] (tabf @70165) where
                         f v ws = sort $ filter (<= v) ws

instance OEIS 214653 where
  oeis = elemIndices 1 (oeis @214323)

instance OEIS 214661 where
  oeis = tablList @214661
instance Table 214661 where
  rowCol = rowCol_off @214661 @1 @1
  rowT   = rowT_off   @214661 @1
  tabl = zipWith take [1..] $ transpose $ map reverse (tabl @176271)

instance OEIS 214723 where
  oeis = elemIndices 1 (oeis @45698)

instance OEIS 214866 where
  oeis = elemIndices 0 (oeis @59175)

instance OEIS 214879 where
  oeis = elemIndices 0 (oeis @45698)

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

instance OEIS 215630 where
  oeis = tablList @215630
instance Table 215630 where
  rowCol = rowCol_off @215630 @1 @1
  rowT   = rowT_off   @215630 @1
  tabl = zipWith3 (zipWith3 (\u v w -> u - v + w))
                          (tabl @93995) (tabl @75362) (tabl @133819)

instance OEIS 215631 where
  oeis = tablList @215631
instance Table 215631 where
  rowCol = rowCol_off @215631 @1 @1
  rowT   = rowT_off   @215631 @1
  tabl = zipWith3 (zipWith3 (\u v w -> u + v + w))
                          (tabl @93995) (tabl @75362) (tabl @133819)

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

instance OEIS 217261 where
  oeis = f [3..] $ S.singleton (16, (2, 2)) where
     f xs'@ (x:xs) s
       | m > x ^ 4  = f xs $ S.insert (x ^ 4, (x, 2)) s
       | m == x ^ 4 = f xs s
       | otherwise  = m : f xs' (S.insert (i ^ (j + 1) ^ 2, (i, j + 1)) s')
       where ((m, (i,j)), s') = S.deleteFindMin s

instance OEIS 217712 where
  oeis = f 1 S.empty S.empty where
     f x s s1 = fi (S.size s1') : f (x + 1) (s `S.union` S.fromList hs) s1' where
       s1' = g s1 $ filter ((== 1) . (oeisIx @10051 . pred)) $ map numerator hs
       g v []                    = v
       g v (w:ws) | w `S.member` v = g (S.delete w v) ws
                  | otherwise    = g (S.insert w v) ws
       hs = map (+ 1 % x) $ 0 : S.toList s

instance OEIS 217793 where
  oeis = tablList @217793
instance Table 217793 where
  rowCol = rowCol_off @217793 @1 @0
  rowT   = rowT_off @217793 @1
  tabf =
     map (\p -> [2*p*k + k^2 `mod` p | k <- [0..p- 1]]) (oeis @65091)

instance OEIS 217843 where
  oeis = f (S.singleton (0, (0,0))) (-1) where
     f s z = if y /= z then y : f s'' y else f s'' y
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 218535 where
  oeis = zipWith gcd (oeis @176352) $ tail (oeis @176352)

instance OEIS 218978 where
  oeis = tablList @218978
instance Table 218978 where
  tabf = map (sort . nub . map (foldr (\d v -> 10 * v + d) 0) .
                     concatMap (tail . inits) . tails) (tabf @31298)

instance OEIS 219031 where
  oeis = tablList @219031
instance Table 219031 where
  tabf = map (rowT @218978) (oeis @290)

instance OEIS 219032 where
  oeisIx = sum . map (oeisIx @10052) . (rowT @219031)

instance OEIS 219206 where
  oeis = tablList @219206
instance Table 219206 where
  tabl = zipWith (zipWith (^)) (tabl @7318) (tabl @2262)

instance OEIS 219696 where
  oeis = filter (\x -> collatz'' x == x) [1..] where
     collatz'' x = until (`elem` [1, x]) (oeisIx @6370) (3 * x + 1)

instance OEIS 219907 where
  oeis = f 0 S.empty where
     f z s | S.null s || z' <= m = f (z + 1) (s `S.union` (S.fromList ws))
           | otherwise             = m : f z s'
           where (m,s') = S.deleteFindMin s
                 ws = map (h z) [0..z] ++ map (flip h z) [0..z- 1]
                 h i j = 4 * i ^ 2 + 2 * i * j + 7 * j ^ 2
                 z' = h z 0

instance OEIS 220053 where
  oeis = tablList @220053
instance Table 220053 where
  rowCol = rowCol_off @220053 @1 @1
  rowT   = rowT_off   @220053 @1
  tabl = map (scanl1 (+)) (tabl @130517)

instance OEIS 220073 where
  oeis = tablList @220073
instance Table 220073 where
  rowCol = rowCol_off @220073 @1 @1
  rowT   = rowT_off   @220073 @1
  tabl = map reverse (tabl @130517)

instance OEIS 220075 where
  oeis = tablList @220075
instance Table 220075 where
  rowCol = rowCol_off @220075 @1 @1
  rowT   = rowT_off   @220075 @1
  tabl = map (scanl1 (+)) (tabl @220073)

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

instance OEIS 220423 where
  oeis = f (splitAt 1 (oeis @2110)) S.empty where
     f (us'@ (u:_), vs'@ (v:vs)) s
       | S.null s || m > u
                   = f (v:us', vs) (s `S.union` (S.fromList $ map (* u) us'))
       | otherwise = m : f (us', vs') s'
       where (m,s') = S.deleteFindMin s

instance OEIS 220424 where
  oeis = tablList @220424
instance Table 220424 where
  rowCol = rowCol_off @220424 @1 @1
  rowT   = rowT_off @220424 @1
  tabf = iterate (concatMap (\xs -> [head xs, length xs]) . group) [1]

instance OEIS 220811 where
  oeis = map numerator vs where
     vs = iterate (\x -> x * (4 - 3 * x)) (1 % 10)

instance OEIS 220885 where
  oeis = 5 : 8 : zs where
     zs = 12 : zipWith (+) zs (drop 13 (oeis @931))

instance OEIS 220968 where
  oeis = map (+ 1) $ findIndices odd (oeis @30229)

instance OEIS 220969 where
  oeis = map (+ 1) $ findIndices even (oeis @30229)

instance OEIS 221054 where
  oeis = filter (z 0 0 . (rowT @27748)) $ tail (oeis @5843) where
     z u v []     = u == v
     z u v (p:ps) = z (u + p) v ps || z u (v + p) ps

instance OEIS 221056 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @61265)

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

instance OEIS 221221 where
  oeis = filter (\x -> (oeisIx @133500) x == (oeisIx @133048) x) [0..]

instance OEIS 221265 where
  oeis = filter ((> 0) . (oeisIx @5094) . pred) [5..]

instance OEIS 221309 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @117499)

instance OEIS 221310 where
  oeis = map (+ 1) $ elemIndices 4 (oeis @117499)

instance OEIS 221469 where
  oeisIx 0 = 0
  oeisIx n = fi . sum $ map fromEnum $ zipWith (>) (tail ts) ts where
     ts = scanl1 max $ (rowT @70165 . succ) n

instance OEIS 221470 where
  oeisIx = succ . fromJust . (`elemIndex` (map (oeisIx @221469) [0..]))

instance OEIS 221490 where
  oeisIx (succ->n) = sum [oeisIx @10051 $ pred (k*n + k - n) | k <- [1..n]]

instance OEIS 221491 where
  oeisIx (succ->n) = sum [oeisIx @10051 $ pred (k*n - k + n) | k <- [1..n]]

instance OEIS 221869 where
  oeis = f 2 7 (S.singleton 1) where
     f u v s | d `S.member` s = f (u + 1) (v + d) s
             | otherwise    = d : f (u + 1) (v + d) (d `S.insert` s)
             where d = gcd u v

instance OEIS 222946 where
  oeis = tablList @222946
instance Table 222946 where
  rowCol = rowCol_off @222946 @2 @1
  rowT = rowT_off @222946 @2
  tabl = zipWith p [2..] (tabl @55096) where
     p x row = zipWith (*) row $
               map (\k -> ((x + k) `mod` 2) * (oeisIx @63524) (gcd x k)) [1..]

instance OEIS 224075 where
  oeis = tablList @224075
instance Table 224075 where
  rowCol = rowCol_off @224075 @1 @1
  rowT   = rowT_off @224075 @1
  tabf = f 3 where
     f x = g [] 3 1 where
       g ps i k2 | x <= k2        = ps : f (x + 1)
                 | gcd k2 x > 1   = g ps (i + 2) (k2 + i)
                 | (oeisIx @10051 . pred) q == 1 = g (q:ps) (i + 2) (k2 + i)
                 | otherwise      = f (x + 1)
                 where q = x - k2

instance OEIS 224694 where
  oeis = elemIndices 0 (oeis @213541)

instance OEIS 224791 where
  oeis = tablList @224791
instance Table 224791 where
  tabl = iterate
     (\row -> scanl1 (+) $ zipWith (+) ([1] ++ row) (row ++ [1])) [0]

instance OEIS 224823 where
  oeisIx n = genericLength [ () | let ts = takeWhile (<= n) (oeis @217),
              x <- ts, y <- ts, z <- takeWhile (<= div (n - x - y) 3) ts,
              x + y + 3 * z == n]

instance OEIS 224829 where
  oeis = elemIndices 0 (oeis @224823)

instance OEIS 224866 where
  oeis = [x | x <- [2..]
            , let x' = x - 1
            , let k = (oeisIx @7947.pred) x'
            , let (y,m) = divMod x' k
            , m == 0
            , (oeisIx @7947.pred) y == k]

instance OEIS 225043 where
  oeis = tablList @225043
instance Table 225043 where
  tabl = zipWith (map . flip mod) [1..] (tabl @7318)

instance OEIS 225230 where
  oeisIx (succ->n) = (oeisIx @1221 . pred) n - (oeisIx @51903 . pred) n

instance OEIS 225243 where
  oeis = tablList @225243
instance Table 225243 where
  rowCol = rowCol_off @225243 @1 @1
  rowT   = rowT_off @225243 @1
  tabf = [1] : map (filter ((== 1) . (oeisIx @10051 . pred))) (tail (tabf @165416))

instance OEIS 225413 where
  oeis = tablList @225413
instance Table 225413 where
  tabl = map (map (`div` 2)) $
                 zipWith (zipWith (-)) (tabl @101164) (tabl @14473)

instance OEIS 225493 where
  oeis = 1 : h (S.singleton p) ps [p] where
     (p:ps) = (oeis @51634)
     h s xs'@ (x:xs) ys
       | m > x     = h (s `S.union` (S.fromList $ map (* x) (1 : ys))) xs ys
       | otherwise = m : h (s' `S.union` (S.fromList $ map (* m) ys')) xs' ys'
       where ys' = m : ys; (m, s') = S.deleteFindMin s

instance OEIS 225494 where
  oeis = 1 : h (S.singleton p) ps [p] where
     (p:ps) = (oeis @6562)
     h s xs'@ (x:xs) ys
       | m > x     = h (s `S.union` (S.fromList $ map (* x) (1 : ys))) xs ys
       | otherwise = m : h (s' `S.union` (S.fromList $ map (* m) ys')) xs' ys'
       where ys' = m : ys; (m, s') = S.deleteFindMin s

instance OEIS 225495 where
  oeis = 1 : h (S.singleton p) ps [p] where
     (p:ps) = (oeis @51635)
     h s xs'@ (x:xs) ys
       | m > x     = h (s `S.union` (S.fromList $ map (* x) (1 : ys))) xs ys
       | otherwise = m : h (s' `S.union` (S.fromList $ map (* m) ys')) xs' ys'
       where ys' = m : ys; (m, s') = S.deleteFindMin s

instance OEIS 225496 where
  oeis = 1 : h (S.singleton p) ps [p] where
     (p:ps) = (oeis @178943)
     h s xs'@ (x:xs) ys
       | m > x     = h (s `S.union` (S.fromList $ map (* x) (1 : ys))) xs ys
       | otherwise = m : h (s' `S.union` (S.fromList $ map (* m) ys')) xs' ys'
       where ys' = m : ys; (m, s') = S.deleteFindMin s

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

instance OEIS 225817 where
  oeis = tablList @225817
instance Table 225817 where
  rowCol = rowCol_off @225817 @1 @1
  rowT   = rowT_off @225817 @1
  tabf = map (map (oeisIx @8683 . pred)) (tabf @27750)

instance OEIS 225843 where
  oeisIx = floor . sum . map (recip . fi) . (rowT @70165) . succ

instance OEIS 225985 where
  oeis = map (fi.read) $ filter (not . null) $
      map (filter (`elem` "13579") . show . fi) [0..]

instance OEIS 226077 where
  oeis = map fi $ 1 : f 1 [2..] where
     f :: Integer -> [Integer] -> [Integer]
     f x zs = g zs where
       g (y:ys) | (oeisIx @209229) (x .&. y) == 0 = g ys
                | otherwise = y : f y (delete y zs)

instance OEIS 226078 where
  oeis = tablList @226078
instance Table 226078 where
  tabf = map (rowT @141809) (oeis @984)

instance OEIS 226091 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @225985)) . (oeisIx @14261)

instance OEIS 226093 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226077)) . succ

instance OEIS 226110 where
  oeisIx 0 = 0
  oeisIx (succ->n) = maximum $ map abs $ zipWith (-) (tail ts) ts
     where ts = (rowT @70165) n

instance OEIS 226123 where
  oeisIx = sum . map (oeisIx @209229) . (rowT @70165) . succ

instance OEIS 226134 where
  oeisIx = fi . (foldl (\v d -> 10*v+d) 0 . scanl1 (\d x -> (x+d) `mod` 10) .
            map (read . return) . show :: Int -> Int) . fi

instance OEIS 226203 where
  oeis = concat $ transpose [[1, 3 ..], [-3, -1 ..], [-1, 1 ..], [1, 3 ..], [1, 3 ..]]

instance OEIS 226314 where
  oeis = tablList @226314
instance Table 226314 where
  rowCol n k = n - (n - k) `div` gcd n k
  rowT   = rowT_off   @226314 @1
  tabl = map f $ tail (tabl @2262) where
     f us'@ (_:us) = map (v -) $ zipWith div vs (map (gcd v) us)
       where (v:vs) = reverse us'

instance OEIS 226390 where
  oeis = zipWith (-) (tail (oeis @14011)) (oeis @14011)

instance OEIS 226452 where
  oeis = 1 : 2 : f [[0,0],[0,1],[1,0],[1,1]] where
     f bss = sum (map h bss) : f ((map (0 :) bss) ++ (map (1 :) bss)) where
     h bs = fi . fromEnum $ or $ zipWith
             (\xs ys -> xs == ys && not (xs `isInfixOf` (init $ tail bs)))
             (init $ inits bs) (reverse $ tails $ tail bs)

instance OEIS 226463 where
  oeis = tablList @226463
instance Table 226463 where
  tabf = map (map (1 -)) (tabf @70950)

instance OEIS 226464 where
  oeis = tablList @226464
instance Table 226464 where
  tabf = map reverse (tabf @226463)

instance OEIS 226474 where
  oeisIx = (1 -) . (oeisIx @51023)

instance OEIS 226481 where
  oeis = tablList @226481
instance Table 226481 where
  tabf = map (map length . group) (tabf @70950)

instance OEIS 226637 where
  oeis = elemIndices 0 (oeis @76489)

instance OEIS 226777 where
  oeis = f (oeis @76467) S.empty where
     f (x:xs) s | S.null $ S.filter ((`S.member` s) . (x -)) s'
                            = f xs (x `S.insert` s)
                | otherwise = x : f xs (x `S.insert` s)
                where (s', _) = S.split (x `div` 2) s

instance OEIS 227048 where
  oeis = tablList @227048
instance Table 227048 where
  rowCol = rowCol_off @227048 @0 @1
  tabf = map f (oeis @244)  where
     f x = reverse $ map (x -) $ takeWhile (<= x) (oeis @79)

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

instance OEIS 227431 where
  oeis = tablList @227431
instance Table 227431 where
  rowCol = rowCol_off @227431 @1 @1
  rowT   = rowT_off   @227431 @1
  tabl = h [] 0 1 where
     h row u v = row' : h row' v (u + v) where row' = scanl (-) v row

instance OEIS 227550 where
  oeis = tablList @227550
instance Table 227550 where
  tabl = map fst $ iterate
     (\ (vs, w:ws) -> (zipWith (+) ([w] ++ vs) (vs ++ [w]), ws))
     ([1], (oeis @1563))

instance OEIS 227617 where
  oeis = f 1 M.empty $ zip (oeis @100707) [1..] where
     f i mp (uv:uvs)
       | M.null mp = f i (uncurry M.insert uv mp) uvs
       | y == i      = x : f (i + 1) (uncurry M.insert uv mp') uvs
       | otherwise   = f i (uncurry M.insert uv mp) uvs
       where ((y,x), mp') = M.deleteFindMin mp

instance OEIS 227736 where
  oeis = tablList @227736
instance Table 227736 where
  rowCol = rowCol_off @227736 @1 @1
  rowT   = rowT_off @227736 @1
  tabf = map (map length . group) $ tail (tabf @30308)

instance OEIS 227862 where
  oeis = tablList @227862
instance Table 227862 where
  tabl = map snd $ iterate ox (False, [1]) where
     ox (turn, xs) = (not turn, if turn then reverse ys else ys)
        where ys = scanl (+) 1 (if turn then reverse xs else xs)

instance OEIS 227915 where
  oeis = map succ $ elemIndices 4 $ tail $ oeis @228085

instance OEIS 228053 where
  oeis = tablList @228053
instance Table 228053 where
  tabl = iterate (\row@ (i:_) -> zipWith (+)
     ([- i] ++ tail row ++ [0]) ([0] ++ init row ++ [- i])) [- 1]


instance OEIS 228074 where
  oeis = tablList @228074
instance Table 228074 where
  tabl = map fst $ iterate
     (\ (u:_, vs) -> (vs, zipWith (+) ([u] ++ vs) (vs ++ [1]))) ([0], [1,1])

instance OEIS 228082 where
  oeis = 0 : filter ((> 0) . (oeisIx @228085)) [1..]

instance OEIS 228085 where
  oeisIx n = genericLength $ filter ((== n) . (oeisIx @92391)) [n - (oeisIx @70939) n .. n]

instance OEIS 228088 where
  oeis = 0 : (map succ $ elemIndices 1 $ tail $ oeis @228085)

instance OEIS 228340 where
  oeis = tablList @228340
instance Table 228340 where
  rowCol = rowCol_off @228340 @1 @0
  rowT   = rowT_off   @228340 @1
  tabl = map (reverse . fst) $ iterate f ([1], [1,0]) where
     f (us, vs'@ (_ : vs@ (v : _))) = (vs', ws) where
       ws = 1 : (v + 1) : zipWith (+) us (map (* (v + 2)) vs)

instance Table 228351 where
  rowT 0 = []
  rowT n = (oeisIx @1511 . pred) n : (rowT @228351) (n `div` 2^ (oeisIx @1511 $ pred n))
instance OEIS 228351 where
  oeis = concatMap (rowT @228351) [1..]

instance OEIS 228369 where
  oeis = concatMap (rowT @228369) [1..]
instance Table 228369 where
  rowT 0 = []
  rowT n
    | 2^k == 2 * n + 2 = [k - 1]
    | otherwise        = (rowT @228369) (n `div` 2^k) ++ [k] where
      k = (oeisIx @7814.pred) (n + 1) + 1

instance OEIS 228643 where
  oeis = tablList @228643
instance Table 228643 where
  rowCol = rowCol_off @228643 @1 @1
  rowT   = rowT_off   @228643 @1
  tabl = map fst $ iterate
     (\ (row, x) -> (scanl (+) (x * (x - 1) + 1) row, x + 1)) ([1], 2)

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

instance OEIS 229037 where
  oeis = f 0 M.empty  where
     f i m = y : f (i + 1) (M.insert (i + 1) y m) where
       y = head [z | z <- [1..],
                     all (\k -> z + m M.! (i - k) /= 2 * m M.! (i - k `div` 2))
                         [1, 3 .. i - 1]]

instance OEIS 229109 where
  oeisIx (succ->n)  = (oeisIx @1221 . pred) n + n

instance OEIS 229362 where
  oeis = 1 : 2 : 3 : f 4 [1,2,3] where
     f x ys = y : f (x + 1) (ys ++ [y]) where y = p ys x
     p _          0 = 1
     p []         _ = 0
     p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 230091 where
  oeis = map succ $ elemIndices 2 $ tail $ oeis @228085

instance OEIS 230092 where
  oeis = map succ $ elemIndices 3 $ tail $ oeis @228085

instance OEIS 230093 where
  oeisIx n = length $ filter ((== n) . oeisIx @62028)
    [n - 9 * oeisIx @55642 n .. n]

instance OEIS 230094 where
  oeis = elemIndices 2 (oeis @230093)

instance OEIS 230631 where
  oeisIx n = (oeisIx @53737) n + n

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

instance OEIS 231330 where
  oeis = tablList @231330
instance Table 231330 where
  tabf = map (sort . nub) (tabf @230871)

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

instance OEIS 231897 where
  oeisIx n = head [x | x <- [0..], (oeisIx @159918) x == n]

instance OEIS 231900 where
  oeis = filter (> 1) (oeis @134744)

instance OEIS 232054 where
  oeis = c [1..] (oeis @56875) where
     c (u:us) vs'@ (v:vs) = if u == v then c us vs else u : c us vs'

instance OEIS 232221 where
  oeis = scanl1 (+) (oeis @232342)

instance OEIS 232244 where
  oeis = 1 : concat (zipWith w (oeis @2193) $ tail (oeis @2193))
     where w v u | v > u     = [v - 1, v - 2 .. u]
                 | v < u     = [v + 1 .. u]
                 | otherwise = [v]

instance OEIS 232246 where
  oeisIx n = (rowCol @110440) (2 * n) n

instance OEIS 232342 where
  oeis = zipWith (-) (oeis @77068) (oeis @77065)

instance OEIS 232991 where
  oeisIx = (0 ^) . subtract 1 . gcd 6 . (+ 1)
  oeis = cycle [1,0,0,0,1,0]

instance OEIS 234575 where
  oeis = tablList @234575
instance Table 234575 where
  rowCol = rowCol_off @234575 @1 @1
  rowT   = rowT_off   @234575 @1
  tabl = zipWith (zipWith (+)) (tabl @48158) (tabl @10766)

instance OEIS 234950 where
  oeis = tablList @234950
instance Table 234950 where
  rowCol n k = sum [rowCol @7318 s k * (rowCol @9766) n s | s <- [k..n]]
  rowT n = map (rowCol @234950 n) [0..n]
  tabl = map (rowT @234950) [0..]

instance OEIS 234959 where
  oeisIx = f 1 . succ where
     f y x
      | (x', 0) <- divMod x 6 = f (y * 6) x'
      | let                   = y

instance OEIS 235168 where
  oeis = tablList @235168
instance Table 235168 where
  rowT 0 = [0]
  rowT n = t n $ reverse $ takeWhile (<= n) (oeis @2110)
     where t 0 []     = []
           t x (b:bs) = x' : t m bs where (x', m) = divMod x b
  tabf = map (rowT @235168) [0..]

instance OEIS 235711 where
  oeisIx = (oeisIx @3415) . (oeisIx @2620)

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

instance OEIS 236076 where
  oeis = tablList @236076
instance Table 236076 where
  tabl = [1] : [0, 2] : f [1] [0, 2] where
     f us vs = ws : f vs ws where
       ws = [0] ++ zipWith (+) (zipWith (+) ([0] ++ us) (us ++ [0])) vs

instance OEIS 237424 where
  oeisIx = flip div 3 . (+ 1) . (oeisIx @52216)

instance OEIS 238453 where
  oeis = tablList @238453
instance Table 238453 where
  tabl = [1] : f [1] (oeis @10) where
     f xs (z:zs) = (map (div y) $ zipWith (*) ys $ reverse ys) : f ys zs
       where ys = y : xs; y = head xs * z

instance OEIS 238497 where
  oeis = filter ((== 1) . (oeisIx @212793).pred) $ tail (oeis @45)

instance OEIS 238498 where
  oeis = tablList @238498
instance Table 238498 where
  tabl = [1] : f [1] (oeis @1615) where
     f xs (z:zs) = (map (div y) $ zipWith (*) ys $ reverse ys) : f ys zs
       where ys = y : xs; y = head xs * z

instance OEIS 238985 where
  oeis = filter ((== 1) . (oeisIx @168046)) $ f $ S.singleton 1 where
     f s = x : f (s' `S.union` S.fromList
                 (filter ((> 0) . (`mod` 10)) $ map (* x) [2,3,5,7]))
                 where (x, s') = S.deleteFindMin s

instance OEIS 239690 where
  oeisIx = (oeisIx @53737) . (oeisIx @40)

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

instance OEIS 239878 where
  oeis = elemIndices 1 (oeis @240752)

instance OEIS 239965 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @240024)) . (oeisIx @18252)

instance OEIS 240024 where
  oeis = 1 : ekg 4 (oeis @2808) where
     ekg x zs = f zs where
         f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

instance OEIS 240025 where
  oeisIx n = max (oeisIx @5369 n) (oeisIx @10052 n)

instance OEIS 240236 where
  oeis = tablList @240236
instance Table 240236 where
  rowCol = rowCol_off @240236 @1 @1
  rowT   = rowT_off   @240236 @1
  tabl = zipWith (map . flip q)
                         [2..] (map tail $ tail (tabl @2260)) where
     q b n = if n < b then n else q b n' + d where (n', d) = divMod n b

instance OEIS 240595 where
  oeis = tablList @240595
instance Table 240595 where
  rowCol = rowCol_off @240595 @1 @1
  rowT   = rowT_off @240595 @1
  tabf = iterate f [1] where
     f xs = concat [map length zss, map head zss]
            where zss = group $ sort xs

instance OEIS 240694 where
  oeis = tablList @240694
instance Table 240694 where
  rowCol = rowCol_off @240694 @1 @1
  rowT   = rowT_off @240694 @1
  tabf = map (scanl1 (*)) (tabf @27750)

instance OEIS 240698 where
  oeis = tablList @240698
instance Table 240698 where
  rowCol = rowCol_off @240698 @1 @1
  rowT   = rowT_off @240698 @1
  tabf = map (scanl1 (+)) (tabf @27750)

instance OEIS 240752 where
  oeis = zipWith (-) (tail (oeis @4159)) (oeis @4159)

instance OEIS 240754 where
  oeis = elemIndices (-1) (oeis @240752)

instance OEIS 240769 where
  oeis = tablList @240769
instance Table 240769 where
  rowCol = rowCol_off @240769 @1 @1
  rowT   = rowT_off   @240769 @1
  tabl = iterate (\ (x:xs) -> xs ++ [2*x, 2*x- 1]) [1]

instance OEIS 240857 where
  oeis = tablList @240857
instance Table 240857 where
  tabl = iterate (\ (x:xs) -> xs ++ [x, x + 1]) [0]

instance OEIS 241241 where
  oeis = 0 : 1 : f (S.singleton 2) where
     f s = m : f (S.insert (oeisIx @290 m) $ S.insert (oeisIx @217 m) s')
           where (m, s') = S.deleteFindMin s

instance OEIS 241759 where
  oeisIx = p $ tail (oeis @1047) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 241783 where
  oeis = elemIndices 0 (oeis @241759)

instance OEIS 242114 where
  oeis = tablList @242114
instance Table 242114 where
  rowCol = rowCol_off @242114 @1 @1
  rowT   = rowT_off   @242114 @1
  tabl = map (map (oeisIx @18805 . pred)) (tabl @10766)

instance OEIS 242179 where
  oeis = tablList @242179
instance Table 242179 where
  tabf = iterate (concatMap (\x -> [-x, x])) [1]

instance OEIS 242312 where
  oeis = tablList @242312
instance Table 242312 where
  tabl = map (map (oeisIx @10888)) (tabl @7318)

instance OEIS 242357 where
  oeis = concatMap f $ tail $ inits [1..] where
     f us = (init us) ++ (take v [v, v ..]) ++ vs
            where (v:vs) = reverse us

instance OEIS 242399 where
  oeisIx n = foldr (\t v -> 3 * v + t) 0 $
                    map (flip mod 3) $ zipWith (+) ([0] ++ ts) (ts ++ [0])
              where ts = (rowT @30341) n

instance OEIS 242400 where
  oeisIx (n) = (oeisIx @8586) n - (oeisIx @242399) n

instance OEIS 242407 where
  oeis = elemIndices 0 (oeis @242400)

instance OEIS 242614 where
  oeis = tablList @242614
instance Table 242614 where
  rowCol n k = (rowT @242614) n !! (k- 1)
  rowT n = filter ((== n) . (oeisIx @7953)) [n .. (oeisIx @2275) n]
  tabf = map (rowT @242614) [0..]

instance OEIS 242885 where
  oeisIx (succ->n) = head [k | let nn = n ^ n, k <- [1..], mod (k ^ k + nn) (k + n) == 0]

instance OEIS 243758 where
  oeis = scanl (*) 1 (oeis @234959)

instance OEIS 243987 where
  oeis = tablList @243987
instance Table 243987 where
  rowCol = rowCol_off @243987 @1 @1
  rowT   = rowT_off   @243987 @1
  tabl = map (scanl1 (+)) (tabl @51731)

instance OEIS 244040 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @244040) (2 * n') + t where (n', t) = divMod n 3

instance OEIS 245022 where
  oeis = elemIndices 3 (oeis @2635)

instance OEIS 245093 where
  oeis = tablList @245093
instance Table 245093 where
  rowCol = rowCol_off @245093 @1 @1
  rowT   = rowT_off   @245093 @1
  tabl = tail $ inits $ (oeis @203)

instance OEIS 245180 where
  oeisIx = flip div 8 . (oeisIx @160239) . succ

instance OEIS 245334 where
  oeis = tablList @245334
instance Table 245334 where
  tabl = iterate (\row@ (h:_) -> (h + 1) : map (* h) row) [1]

instance OEIS 245542 where
  oeis = scanl1 (+) (oeis @160239)

instance OEIS 245550 where
  oeis = 0 : zipWith (-) (tail (oeis @6046)) (h (oeis @6046))
                 where h (x:xs) = (2 * x) : (2 * x) : h xs

instance OEIS 245563 where
  oeis = tablList @245563
instance Table 245563 where
  tabf = [0] : map
     (map length . (filter ((== 1) . head)) . group) (tail (tabf @30308))

instance OEIS 245575 where
  oeis = f 0 [] $ tail (oeis @2620) where
     f u vs ws'@ (w:ws)
       | u < w     = (sum $ map (oeisIx @240025 . (u -)) vs) : f (u + 1) vs ws'
       | otherwise = f u (w : vs) ws

instance OEIS 245585 where
  oeis = elemIndices 0 (oeis @245575)

instance OEIS 245717 where
  oeis = tablList @245717
instance Table 245717 where
  rowCol = rowCol_off @245717 @1 @1
  rowT   = rowT_off   @245717 @1
  tabl = zipWith (zipWith gcd) (tabl @2024) (tabl @133819)

instance OEIS 245826 where
  oeis = tablList @245826
instance Table 245826 where
  rowCol n k = n * k * (2 * n^2 * k^2 - n^2 - k^2) `div` 6
  rowT n = map (rowCol @245826 n) [1..n]
  tabl = map (rowT @245826) [1..]

instance OEIS 246438 where
  oeis = elemIndices 0 (oeis @164349)

instance OEIS 246439 where
  oeis = elemIndices 1 (oeis @164349)

instance OEIS 246694 where
  oeis = tablList @246694
instance Table 246694 where
  tabl = [1] : [1,2] : f 1 2 [1,2] where
     f i z xs = ys : f j (z + 1) ys where
       ys = take (z + 1) $ map (+ 1) (xs !! (z - i) : xs !! (z - j) : ys)
       j = 3 - i

instance OEIS 246695 where
  oeis = scanl1 (+) (oeis @257083)

instance OEIS 246830 where
  oeis = tablList @246830
instance Table 246830 where
  tabl = zipWith (zipWith f) (tabl @51162) (tabl @25581) where
     f x y = foldr (\b v -> 2 * v + b) 0 $ x |+| y
     (|+|) = (++) `on` (rowT @30308)

instance OEIS 247023 where
  oeis = tablList @247023
instance Table 247023 where
  tabl = map reverse (tabl @201634)

instance OEIS 247073 where
  oeis = tablList @247073
instance Table 247073 where
  rowCol = rowCol_off @247073 @1 @1
  tabl = map (rowT @247073) [1..]
  rowT n = map length $ groupBy ((==) `on` fst) $ sort $
     takeWhile ((<= 2^n). snd) $ tail $ zip (oeis @25474) (oeis @961)

instance OEIS 247108 where
  oeis = tablList @247108
instance Table 247108 where
  tabl = iterate (\row -> scanl (+) (- last row) row) [1]

instance OEIS 247143 where
  oeis = map fi $ [0..10] ++ f 11 (map show [11..]) where
     f x zss = (read ys :: Int) : f (x + 1) (delete ys zss) where
               ys = fromJust $ find (elem $ ds !! x) zss
     ds = concatMap show (oeis @247143)

instance OEIS 247149 where
  oeis = map (fi.digitToInt) $ concatMap (show.fi) (oeis @247143)

instance OEIS 247358 where
  oeis = tablList @247358
instance Table 247358 where
  rowCol = rowCol_off @247358 @1 @1
  rowT   = rowT_off   @247358 @1
  tabl = map sort (tabl @51129)

instance OEIS 247364 where
  oeis = tablList @247364
instance Table 247364 where
  tabl = [1] : (map reverse (tabf @34928))

instance OEIS 247365 where
  oeisIx (succ->n) = (rowCol @102473) (2 * n - 1) n

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

instance OEIS 247379 where
  oeisIx n = gcd (n+1) $ (oeisIx @64413) n

instance OEIS 247383 where
  oeis = f 1 1 M.empty where
     f x z m | M.member x m = m M.! x : f (x + 1) (x + 1) m
             | M.member y m = f x (z + 1) m
             | otherwise  = f x (z + 1) (M.insert y z m)
             where y = (oeisIx @247379.pred) z

instance OEIS 247453 where
  oeis = tablList @247453
instance Table 247453 where
  tabl = zipWith (zipWith (*)) (tabl @109449) (tabl @97807)

instance OEIS 247462 where
  oeisIx 0 = 1
  oeisIx (succ->n) = fst $ until ((== 1) . denominator . snd)
                          (\ (i, x) -> (i + 1, f x)) (0, 1 % n) where
     f x = (oeisIx @8472 . pred) x' % (oeisIx @1221 . pred) x' where x' = numerator x + denominator x

instance OEIS 247500 where
  oeis = tablList @247500
instance Table 247500 where
  tabl = zipWith (zipWith div) (tabl @105278) (tabl @4736)

instance OEIS 247560 where
  oeis = 1 : 1 : zipWith (-) (map (* 3) $ tail (oeis @247560))
                                     (map (* 4) (oeis @247560))

instance OEIS 247563 where
  oeis = 2 : 3 : zipWith (-) (map (* 3) $ tail (oeis @247563))
                                     (map (* 4) (oeis @247563))

instance OEIS 247564 where
  oeis = [2,1,3,1] ++ zipWith (-) (map (* 3) $ drop 2 (oeis @247564))
                                          (map (* 4) $ (oeis @247564))

instance OEIS 247648 where
  oeis = f $ S.singleton 1 where
     f s = x : f (S.insert (4 * x + 1) $ S.insert (2 * x + 1) s')
           where (x, s') = S.deleteFindMin s

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

instance OEIS 247765 where
  oeis = tablList @247765
instance Table 247765 where
  rowCol = rowCol_off @247765 @1 @1
  tabf = map (rowT @247765) [1..]
  rowT n = f (map recip [2..]) (n % (n + 1)) where
     f es x | numerator x == 1 = [denominator x]
            | otherwise        = g es
            where g (u:us) | u <= x    = (denominator u) : f us (x - u)
                           | otherwise =  g us

instance OEIS 247795 where
  oeis = tablList @247795
instance Table 247795 where
  rowCol = rowCol_off @247795 @1 @1
  rowT   = rowT_off @247795 @1
  tabf = map (map (flip mod 2)) (tabf @27750)

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

instance OEIS 248110 where
  oeis = tablList @248110
instance Table 248110 where
  rowCol = rowCol_off @248110 @1 @1
  rowT   = rowT_off @248110 @1
  tabf = map (\x -> [x + 1 .. x + (oeisIx @7953) x]) [1 ..]

instance OEIS 248122 where
  oeisIx 0 = 0
  oeisIx 1 = 0
  oeisIx n = 3 * r (n - 1) + 3^ceiling (n % 2) - r (ceiling (n % 2))
    where r = oeisIx @248122

instance OEIS 248141 where
  oeis = tablList @248141
instance Table 248141 where
  rowCol = rowCol_off @248141 @1 @1
  rowT   = rowT_off @248141 @1
  tabf = map concat usss where
     usss = iterate f [[1]] where
       f vss = group [1 .. last (last vss) + 1] ++
               map (\ws -> ws ++ [last ws + 1]) vss

instance OEIS 248147 where
  oeis = tablList @248147
instance Table 248147 where
  rowCol = rowCol_off @248147 @1 @1
  rowT   = rowT_off @248147 @1
  tabf = map concat psss where
     psss = iterate f [[2]] where
        f pss = group (h $ last pss) ++ map h pss
        h ws = ws ++ [oeisIx @151800 $ last ws]

instance OEIS 248164 where
  oeis = tablList @248164
instance Table 248164 where
  rowCol = rowCol_off @248164 @1 @1
  rowT   = rowT_off @248164 @1
  tabf = map (map product) psss where
     psss = iterate f [[2]] where
        f pss = group (h $ last pss) ++ map h pss
        h ws = ws ++ [oeisIx @151800 $ last ws]

instance OEIS 248378 where
  oeis = concat $ transpose [oeis @1704, tail (oeis @127423)]

instance OEIS 248737 where
  oeis = 0 : f 1 [0] where
     f x ys = y : f (x + 1) (y : ys) where
       y = (+ 1) $ fromMaybe (x - 1) $ findIndex (\z -> gcd z x /= 1) ys

instance OEIS 248756 where
  oeis = f 1 [] where
     f x yvs = fst yw : f (x + 1) (yw:yvs) where
       yw = g 1 yvs
       g _ []          = (x, h)
       g k ((z,w):zws) = if w == h then (k, (oeisIx @120) k) else g (k + 1) zws
       h = (oeisIx @120) x

instance OEIS 248910 where
  oeis = iterate f 1 where
     f x = 1 + if r < 5 then x else 6 * f x'  where (x', r) = divMod x 6

instance OEIS  248939 where
  oeis = tablList @248939
instance Table 248939 where
  tabf = map (rowT @248939) [0..]
  rowT n = n : wBall 1 n (S.singleton n) where
     wBall _ 0 _ = []
     wBall k x s = y : wBall (k + 1) y (S.insert y s) where
                       y = x + (if (x - j) `S.member` s then j else -j)
                       j = k * signum x

instance OEIS 248940 where
  oeis = (rowT @248939) 7

instance OEIS 248941 where
  oeis = (rowT @248939) 17

instance OEIS 248942 where
  oeis = (rowT @248939) 20

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

instance OEIS 248973 where
  oeis = tablList @248973
instance Table 248973 where
  tabf = map (scanl1 (+)) (tabf @248939)

instance OEIS 249031 where
  oeis = f [1..] where
     f ws@ (u:v:_) = u : v : f (ws \\ [u, v, u + v])

instance OEIS 249043 where
  oeis = iterate (oeisIx @62028) 42

instance OEIS 249044 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) $ tail (oeis @3052)

instance OEIS 249045 where
  oeis = filter ((== 0) . flip mod 3) (oeis @3052)

instance OEIS 249046 where
  oeis = filter ((> 0) . flip mod 9) (oeis @249045)

instance OEIS 249047 where
  oeis = filter ((> 0) . flip mod 3) (oeis @3052)

instance OEIS 249048 where
  oeis = filter ((== 0) . flip mod 9) (oeis @3052)

instance OEIS 249053 where
  oeis = 1 : f 1 1 (oeis @2808) (M.singleton 1 1) where
     f x z cs m
       | k == x    = p : f (x + 1) p cs (M.insert (x + p) 0 $ M.delete x m)
       | otherwise = c : f (x + 1) c cs' (M.insert (x + c) 0 m)
       where p = (oeisIx @7918) z
             (c:cs') = dropWhile (<= z) cs
             (k,_) = M.findMin m

instance OEIS 249054 where
  oeis = 1 : f 1 (oeis @40) (oeis @2808) (M.singleton 1 1) where
     f x ps'@ (p:ps) cs'@ (c:cs) m
       | k == x    = p : f (x + 1) ps cs' (M.insert (x + p) 0 $ M.delete x m)
       | otherwise = c : f (x + 1) ps' cs (M.insert (x + c) 0 m)
       where (k,_) = M.findMin m

instance OEIS 249095 where
  oeis = tablList @249095
instance Table 249095 where
  tabf = [1] : map (concat . transpose)
     (zipWith ((. return) . (:)) (tail (tabl @7318)) (tabl @7318))

instance OEIS 249111 where
  oeis = tablList @249111
instance Table 249111 where
  tabf = map (scanl1 (+)) (tabf @249095)

instance OEIS 249129 where
  oeis = map fi $ 1 : 0 : 2 : f 3 2 [3..] where
     f :: Int -> Int -> [Int] -> [Int]
     f k x zs'@ (z:zs)
       | r == 0 = y : f (k+1) y (delete (x + y) $ delete y zs')
       | r == 1 = z : f (k+1) z (delete (x + z) zs)
       where y = (oeisIx @249129) k' + (oeisIx @249129) (k' + 1)
             (k', r) = divMod k 2

instance OEIS 249133 where
  oeis = tablList @249133
instance Table 249133 where
  tabf = map (map (flip mod 2)) (tabf @249095)

instance OEIS 249307 where
  oeis = tablList @249307
instance Table 249307 where
  tabf = map (zipWith (*) (oeis @79)) (tabf @249095)

instance OEIS 249484 where
  oeis = 1 : concat (zipWith (++) ([2] : f [5,2,7]) (f [4,3])) where
     f = iterate (\row -> [g $ head row] ++ row ++ [g $ last row])
     g x = x + ((5 -) . (* 2) . flip mod 2) x

instance OEIS 249571 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . succ

instance OEIS 249594 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . (oeisIx @40)

instance OEIS 249595 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . (oeisIx @18252)

instance OEIS 249629 where
  oeisIx = a where
    a 0 = 0; a 1 = 0
    a n = 4 * a (n - 1) + 4^ceiling (n % 2) - a (ceiling (n % 2))

instance OEIS 249638 where
  oeisIx = a where
    a 0 = 0; a 1 = 0
    a n = 5 * a (n - 1) + 5^ceiling (n % 2) - a (ceiling (n % 2))

instance OEIS 249830 where
  oeis = elemIndices 0 (oeis @249832)

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

instance OEIS 250402 where
  oeis = elemIndices 0 (oeis @247149)

instance OEIS 250403 where
  oeis = elemIndices 1 (oeis @247149)

instance OEIS 251138 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @98550)

instance OEIS 251139 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @98548)

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

instance OEIS 251635 where
  oeis = tablList @251635
instance Table 251635 where
  tabl = [1] : iterate (0 :) [-2, 1]

instance OEIS 253146 where
  oeis = tablList @253146
instance Table 253146 where
  rowCol = rowCol_off @253146 @1 @1
  rowT   = rowT_off   @253146 @1
  tabl = [1] : [2,3] : f [1] [2,3] where
     f us vs@ (v:_) = ws : f vs ws where
                     ws = [v + 2] ++ us ++ [v + 3]

instance OEIS 253170 where
  oeisIx = sum . (rowT @30717) . succ

instance OEIS 253580 where
  oeis = tablList @253580
instance Table 253580 where
  tabf = [0] : [1,0,2] : f [1,0,2] where
     f xs@ (x:_) = ys : f ys where ys = [x + 2] ++ xs ++ [x + 3]

instance OEIS 253672 where
  oeis = tablList @253672
instance Table 253672 where
  tabf = [0,1,2] : f [] [0,1,2] [] (iterate (map (+ 3)) [3..5]) where
     f as bs cs (uvws:uvwss) = (as' ++ uvws ++ cs') : f as' uvws cs' uvwss
       where as' = as ++ [u,v]; cs' = [w] ++ cs
             [u,v,w] = bs

instance OEIS 254143 where
  oeis = f (oeis @237424) [] S.empty where
     f xs'@ (x:xs) zs s
       | S.null s || x < y = f xs zs' (S.union s $ S.fromList $ map (* x) zs')
       | otherwise           = y : f xs' zs s'
       where zs' = x : zs
             (y, s') = S.deleteFindMin s

instance OEIS 254524 where
  oeis = f 1 M.empty where
     f x m = y : f (x + 1) (M.insert q (y + 1) m) where
             y = M.findWithDefault 1 q m; q = (oeisIx @7953) x

instance OEIS 254730 where
  oeis = tablList @254730
instance Table 254730 where
  tabl = zipWith (map . div)
     (oeis @243758) $ zipWith (zipWith (*)) xss $ map reverse xss
     where xss = tail $ inits (oeis @243758)

instance OEIS 255420 where
  oeis = iterate (oeisIx @3309) 1

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

instance OEIS 255879 where
  oeis = scanl1 (+) (oeis @256188)

instance OEIS 256015 where
  oeis = tablList @256015
instance Table 256015 where
  rowCol = rowCol_off @256015 @1 @1
  rowT   = rowT_off @256015 @1
  tabf = map (sort . filter ((== 1) . (oeisIx @10051 . pred)) . nub .
                  map sum . tail . subsequences) (tail $ inits (oeis @40))

instance OEIS 256113 where
  oeis = tablList @256113
instance Table 256113 where
  rowCol = rowCol_off @256113 @1 @1
  rowT   = rowT_off @256113 @1
  tabf = map (rowT @27748) $ tail (oeis @1142)

instance OEIS 256184 where
  oeis = 0 : concat (transpose [map (subtract 2) (oeis @256184),
                                        map (subtract 1) (oeis @256184),
                                        map negate $ tail (oeis @256184)])

instance OEIS 256185 where
  oeis = 0 : concat (transpose [map (subtract 3) (oeis @256185),
                                        map (-2 -) (oeis @256185),
                                        map negate $ tail (oeis @256185)])

instance OEIS 256188 where
  oeis = f 0 [1..] (tabl @2260) where
     f k xs (zs:zss) = us ++ zs ++ f (k + 1) vs zss
                       where (us, v:vs) = splitAt k xs

instance OEIS 256285 where
  oeis = f (tail (oeis @127423)) [] where
     f (x:xs) ds = y : f xs (insert y ds) where
                   y = head (rowT @27750 x `O.minus` ds)

instance OEIS 256393 where
  oeis = 2 : zipWith ($) (cycle [oeisIx @70229.pred, oeisIx @61228.pred]) (oeis @256393)

instance OEIS 256617 where
  oeis = f (S.singleton (6, 2, 3)) $ tail (oeis @40) where
     f s ps@ (p : ps'@ (p':_))
       | m < p * p' = m : f (S.insert (m * q, q, q')
                            (S.insert (m * q', q, q') s')) ps
       | otherwise  = f (S.insert (p * p', p, p') s) ps'
       where ((m, q, q'), s') = S.deleteFindMin s

instance OEIS 256970 where
  oeisIx = (oeisIx @20639) . pred . (oeisIx @53755) . succ

instance OEIS 256971 where
  oeis = scanl1 (+) (oeis @256970)

instance OEIS 257001 where
  oeis = filter f [1..] where
     f x = h > 0 && mod x h == 0 where h = (oeisIx @30) $ (oeisIx @230959) x

instance OEIS 257078 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @248756)) . succ

instance OEIS 257083 where
  oeis = scanl1 (+) (oeis @257088)

instance OEIS 257088 where
  oeis = concat $ transpose [oeis @8574, (oeis @5408)]

instance OEIS 257232 where
  oeis = tablList @257232
instance Table 257232 where
  rowCol = rowCol_off @257232 @1 @1
  rowT   = rowT_off   @257232 @1
  tabl = iterate
                 (\xs@ (x:_) -> map (+ 1) xs ++ [1 - (oeisIx @10051 . pred) (x + 1)]) [1]

instance OEIS 257241 where
  oeis = tablList @257241
instance Table 257241 where
  rowCol = rowCol_off @257241 @1 @1
  rowT   = rowT_off @257241 @1
  tabf = iterate stifel [1] where
     stifel xs@ (x:_) = if odd x then xs' else xs' ++ [last xs']
                       where xs' = zipWith (+) xs (1 : xs)

instance OEIS 257244 where
  oeis = zipWith gcd (oeis @256393) $ tail (oeis @256393)

instance OEIS 257265 where
  oeisIx = genericLength . us where
     us n = if (oeisIx @79559) n == 0
               then [] else () : zipWith (const $ const ())
                                 (us $ (oeisIx @213723) n) (us $ (oeisIx @213724) n)

instance OEIS 257278 where
  oeis = f (S.singleton (4, 2)) 27 (tail (oeis @40)) where
     f s pp ps@ (p:ps'@ (p':_))
       | qq > pp   = pp : f (S.insert (pp * p, p) s) (p' ^ p') ps'
       | otherwise = qq : f (S.insert (qq * q, q) s') pp ps
       where ((qq, q), s') = S.deleteFindMin s

instance OEIS 257502 where
  oeisIx = fromJust . (`elemIndex` (oeis @78783))

instance OEIS 257508 where
  oeis = elemIndices 1 (oeis @257265)

instance OEIS 257509 where
  oeis = elemIndices 2 (oeis @257265)

instance OEIS 257719 where
  oeis = filter f [1..] where
     f x = sx >= x && (oeisIx @1065.pred) sx < sx where sx = (oeisIx @1065.pred) x

instance OEIS 257720 where
  oeis = filter f [1..] where
     f x = sx > 0 && sx < x && (oeisIx @1065.pred) sx >= sx where sx = (oeisIx @1065.pred) x

instance OEIS 257836 where
  oeis = f $ S.singleton (15, 3, 5) where
     f s = y : f (S.insert (w, u, v') $ S.insert (w `div` u, u + 2, v') s')
           where w = y * v'; v' = v + 2
                 ((y, u, v), s') = S.deleteFindMin s

instance OEIS 257851 where
  oeis = tablList @257851
instance Table 257851 where
  tabl = map
     (\x -> take (x + 1) $ filter ((== x) . (oeisIx @46660 . pred)) [1..]) [0..]

instance OEIS 257891 where
  oeis = f $ S.singleton (30, 2, 5) where
     f s = y : f (S.insert (w, p, q') $ S.insert (w `div` p, (oeisIx @151800) p, q') s')
           where w = y * q'; q' = (oeisIx @151800) q
                 ((y, p, q), s') = S.deleteFindMin s

instance OEIS 257997 where
  oeis = O.unionAll [oeis @3586, (oeis @3592), (oeis @3593)]

instance OEIS 257998 where
  oeis = scanl1 (+) (oeis @188967)

instance OEIS 258023 where
  oeis = O.union (oeis @3586) (oeis @3593)

instance OEIS 258197 where
  oeis = tablList @258197
instance Table 258197 where
  tabl = map (map (oeisIx @3415)) (tabl @7318)

instance OEIS 258318 where
  oeis = f 2 (tabl @258197) $ S.singleton 0 where
     f k (xs:xss) zs = g (take (div k 2) xs) zs where
       g []     ys = fi (S.size ys) : f (k + 1) xss ys
       g (x:xs) ys = g xs (S.insert x ys)

instance OEIS 258708 where
  oeis = tablList @258708
instance Table 258708 where
  rowCol = rowCol_off @258708 @1 @0
  rowT   = rowT_off   @258708 @1
  tabl = zipWith (zipWith ((round .) . ((/) `on` fi)))
                         (tabl @258993) (tabl @158405)

instance OEIS 258865 where
  oeis = tail $ f (S.singleton 1) 1 [] [] (oeis @30078) where
     f s z vs qcs pcs'@ (pc:pcs)
       | m < z = m : f s' z vs qcs pcs'
       | otherwise = f (S.union s $ S.fromList $ map (+ pc) ws)
                       pc ws (pc:qcs) pcs
       where ws = O.union vs $ map (+ pc) (pc : qcs)
             (m, s') = S.deleteFindMin s

instance OEIS 258993 where
  oeis = tablList @258993
instance Table 258993 where
  rowCol = rowCol_off @258993 @1 @0
  rowT   = rowT_off   @258993 @1
  tabl = zipWith (zipWith (rowCol @7318)) (tabl @94727) (tabl @4736)

instance OEIS 259024 where
  oeis = concat (transpose [drop 2 cs, [0, 0 ..], drop 7 cs, (oeis @259024)])
    where cs = map negate (oeis @259022)

instance OEIS 259029 where
  oeis = scanl1 (+) (oeis @259024)

instance OEIS 259031 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (map abs (oeis @259029)))

instance OEIS 259260 where
  oeis = 1 : f 1 [3, 5 ..] where
     f x zs = g zs where
       g (y:ys) = if (oeisIx @10052) ((x + y) `div` 2) == 1
                     then y : f y (delete y zs) else g ys

instance OEIS 259429 where
  oeis = 1 : f 1 [3, 5 ..] where
     f x zs = g zs where
       g (y:ys) = if (oeisIx @10057) ((x + y) `div` 2) == 1
                     then y : f y (delete y zs) else g ys

instance OEIS 259431 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @183209)) . succ

instance OEIS 259526 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @259260)) . subtract 1 . (* 2) . succ

instance OEIS 259537 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @259429)) . subtract 1 . (* 2) . succ

instance OEIS 259542 where
  oeis = 1 : f 1 [3, 5 ..] where
     f x zs = g zs where
       g (y:ys) = if (oeisIx @10054) ((x + y) `div` 2) == 1
                     then y : f y (delete y zs) else g ys

instance OEIS 259543 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @259542)) . subtract 1 . (* 2) . succ

instance OEIS 259565 where
  oeis = 1 : f 1 [3, 5 ..] where
     f x zs = g zs where
       g (y:ys) = if (oeisIx @8966 . pred) ((x + y) `div` 2) == 1
                     then y : f y (delete y zs) else g ys

instance OEIS 259570 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @259565)) . subtract 1 . (* 2) . succ

instance OEIS 259602 where
  oeis = zipWith ((flip div 2 .) . (+))
                         (oeis @259260) $ tail (oeis @259260)

instance OEIS 259603 where
  oeis = zipWith ((flip div 2 .) . (+))
                         (oeis @259429) $ tail (oeis @259429)

instance OEIS 259604 where
  oeis = zipWith ((flip div 2 .) . (+))
                         (oeis @259542) $ tail (oeis @259542)

instance OEIS 259605 where
  oeis = zipWith ((flip div 2 .) . (+))
                         (oeis @259565) $ tail (oeis @259565)

instance OEIS 259644 where
  oeis = map numerator $
                 scanl1 (+) $ map (recip . fi) (oeis @112373)

instance OEIS 259645 where
  oeis = (oeis @5574) `O.isect` (oeis @87370) `O.isect` (oeis @56561)

instance OEIS 259730 where
  oeis = (oeis @63908) `O.isect` (oeis @88878)

instance OEIS 259758 where
  oeisIx n = (2 * p - 3) * (3 * p - 2)  where p = (oeisIx @259730) n

instance OEIS 259823 where
  oeis = scanl (+) 0 (oeis @3586)

instance OEIS 260254 where
  oeisIx n = sum $ map (oeisIx @136522 . (n -)) $
                 takeWhile (<= n `div` 2) (oeis @2113)

instance OEIS 260255 where
  oeis = filter ((> 0) . (oeisIx @260254)) [0..]

instance OEIS 260672 where
  oeis = tablList @260672
instance Table 260672 where
  tabf = map (takeWhile (>= 0) . flip map (oeis @1318) . (-)) [0..]

instance OEIS 260689 where
  oeis = tablList @260689
instance Table 260689 where
  rowCol = rowCol_off @260689 @2 @1
  rowT n = [m | m <- [1, 3 .. 2 * n - 3],
                       (oeisIx @10051 . pred) (2*n + m) == 1, (oeisIx @10051 . pred) (2*n - m) == 1]
  tabf = map (rowT @260689) [2..]

instance OEIS 260798 where
  oeis = map (subtract 1 . pMemo 2) (oeis @40) where
     pMemo = memo2 integral integral p
     p _ 0 = 1
     p k m | m < k     = 0
           | otherwise = pMemo k (m - k) + pMemo (k + 1) m

instance OEIS 260910 where
  oeis = tablList @260910
instance Table 260910 where
  rowCol = rowCol_off @260910 @1 @1
  rowT   = rowT_off   @260910 @1
  tabl = zipWith (map . sylvester) [1..] (tabl @77664) where
     sylvester u v = u * v - u - v

instance OEIS 261012 where
  oeisIx = signum . pred
  oeis = -1 : 0 : [1, 1 ..]

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

instance OEIS 261189 where
  oeis = (oeis @52382) `O.isect` (oeis @52413)

instance OEIS 261351 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @84385)) . succ

instance OEIS 261363 where
  oeis = tablList @261363
instance Table 261363 where
  tabl = map (scanl1 (+)) (tabl @47999)

instance OEIS 261423 where
  oeis = tail (oeis @261914)

instance OEIS 261897 where
  oeis = tablList @261897
instance Table 261897 where
  rowCol = rowCol_off @261897 @0 @1
  tabl = [1] : f 1 0 [1] where
     f t h xs | t <= (h + 1) ^ 2  = ys : f (t + 1) h ys
              | otherwise         = ys' : f (t + 1) (h + 1) ys'
              where ys = zipWith (+) ([0] ++ xs) (xs ++ [0])
                    ys' = zipWith (+) ([0] ++ xs) (us ++ (0:vs) ++ [0])
                    (us, _:vs) = splitAt h xs

instance OEIS 261914 where
  oeis = (0:).(0:) . drop 2 $ f 0 (oeis @2113) where
     f n ps@ (p:ps'@ (p':_)) = p : f (n + 1) (if n < p' then ps else ps')

instance OEIS 262065 where
  oeis = O.union us vs where
     us = [val60 $ bs ++ reverse bs | bs <- bss]
     vs = [0..59] ++ [val60 $ bs ++ cs ++ reverse bs |
            bs <- tail bss, cs <- take 60 bss]
     bss = iterate s [0] where
           s [] = [1]; s (59:ds) = 0 : s ds; s (d:ds) = (d + 1) : ds
     val60 = foldr (\b v -> 60 * v + b) 0

instance OEIS 262069 where
  oeis = O.isect (oeis @2113) (oeis @262065)

instance OEIS 262188 where
  oeis = tablList @262188
instance Table 262188 where
  tabf = map (sort . nub . map (foldr (\d v -> 10 * v + d) 0) .
     filter (\xs -> length xs == 1 || last xs > 0 && reverse xs == xs) .
            concatMap (tail . inits) . tails) (tabf @31298)

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

instance OEIS 263017 where
  oeis = f 1 M.empty where
     f x m = y : f (x + 1) (M.insert h (y + 1) m) where
             y = M.findWithDefault 1 h m
             h = (oeisIx @120) x

instance OEIS 263109 where
  oeis = f 1 M.empty where
     f x m = y : f (x + 1) (M.insert q (y + 1) m) where
             y = M.findWithDefault 1 q m; q = (oeisIx @53832) x

instance OEIS 263110 where
  oeis = f 1 M.empty where
     f x m = y : f (x + 1) (M.insert q (y + 1) m) where
             y = M.findWithDefault 1 q m; q = (oeisIx @53836) x

instance OEIS 263451 where
  oeis = iterate (oeisIx @4186 . (* 2)) 1

instance OEIS 263838 where
  oeis = O.union (oeis @257719) (oeis @257720)

instance OEIS 264662 where
  oeis = tablList @264662
instance Table 264662 where
  rowCol = rowCol_off @264662 @1 @1
  rowT   = rowT_off   @264662 @1
  tabl = map (sortBy (compare `on` (reverse . show . fi . (oeisIx @7088)))) $
                     tail $ inits (oeis @40)

instance OEIS 264666 where
  oeis = tablList @264666
instance Table 264666 where
  rowCol = rowCol_off @264666 @1 @1
  rowT   = rowT_off   @264666 @1
  tabl = map (scanl1 (*)) (tabl @264662)

instance OEIS 264717 where
  oeisIx n = (rowCol @100326) (2 * n) n

instance OEIS 264739 where
  oeisIx (succ->n) = if (oeis @2202) `O.has` n then 1 else 0

instance OEIS 264784 where
  oeis = map length $ filter ((== 0) . head) $ group (oeis @265158)

instance OEIS 264810 where
  oeis = (1:) . tail $ scanl1 (+) (oeis @264739)

instance OEIS 264893 where
  oeis = zipWith (-) (tail (oeis @155043)) (oeis @155043)

instance OEIS 264898 where
  oeis = elemIndices 0 (oeis @264893)

instance OEIS 264959 where
  oeisIx n = rowCol @257851 n n

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

instance OEIS 265008 where
  oeisIx (succ->n) = genericLength [ () | let cs = (rowT @165416) n, c <- cs,
              let as = takeWhile (<= c) cs, a <- as, b <- as, a * b == c]

instance OEIS 265110 where
  oeis = tablList @265110
instance Table 265110 where
  rowCol = rowCol_off @265110 @1 @1
  rowT   = rowT_off @265110 @1
  tabf   = map (scanl1 (*)) (tabf @27746)

instance OEIS 265111 where
  oeis = f 1 [] 0 1 where
     f u [] w x = f 1 (reverse $ (rowT @27746) (u * x)) w (x + 1)
     f u (v:vs) w x | v == w    = f (u * v) vs w x
                    | otherwise = v : f u vs v x

instance OEIS 265125 where
  oeis = scanl1 (*) (oeis @265111)

instance OEIS 265158 where
  oeis = 1 : concat
     (transpose [map (* 2) (oeis @265158), map (flip div 2) (oeis @265158)])

instance OEIS 265182 where
  oeisIx (succ->n) = genericLength [ () | let cs = dropWhile (== 0) $ (rowT @218978) n, c <- cs,
              let as = takeWhile (<= c) cs, a <- as, b <- as, a * b == c]

instance OEIS 265183 where
  oeisIx n = genericLength [ () | let cs = (rowT @218978) n, a <- cs, b <- cs, c <- cs,
                           a * b == c || c == 0 && a * b == 0]

instance OEIS 265236 where
  oeisIx n = genericLength [ () | let cs = (rowT @119709) n, a <- cs, b <- cs, c <- cs,
                           a * b == c || c == 0 && a * b == 0]

instance OEIS 265327 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @238324)) . succ

instance OEIS 265377 where
  oeis = f (S.singleton (1 + 2^3, (1, 2))) (-1) where
     f s z = if y /= z then y : f s'' y else f s'' y
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 265509 where
  oeis = f (tail (tabf @30308)) [[]] where
     f (bs:_:bss) pss = y : f bss pss' where
       y = foldr (\d v -> 2 * v + d) 0 ys
       (ys:_) = dropWhile (\ps -> not $ and $ zipWith (<=) ps bs) pss'
       pss' = if bs /= reverse bs then pss else bs : pss

instance OEIS 265510 where
  oeisIx = (oeisIx @7088) . (oeisIx @265509)

instance OEIS 265525 where
  oeis = f (tabf @31298) [[]] where
     f (ds:dss) pss = y : f dss pss' where
       y = foldr (\d v -> 10 * v + d) 0 ys
       (ys:_) = dropWhile (\ps -> not $ and $ zipWith (<=) ps ds) pss'
       pss' = if ds /= reverse ds then pss else ds : pss

instance OEIS 265652 where
  oeis = tablList @265652
instance Table 265652 where
  rowCol = rowCol_off @265652 @1 @1
  rowT   = rowT_off   @265652 @1
  tabl = zipWith (zipWith (-))
     (zipWith (map . (+)) (oeis @203) (tabl @245093)) (tabl @132442)

instance OEIS 265668 where
  oeis = tablList @265668
instance Table 265668 where
  rowCol = rowCol_off @265668 @1 @1
  rowT   = rowT_off @265668 @1
  tabf   = map (map fi) $ [1] : mapMaybe f ([2..] :: [Int]) where
     f x = if all (== 1) es then Just ps else Nothing
           where (map unPrime->ps, es) = unzip $ factorise x

instance OEIS 265675 where
  oeis = map (\ (x:xs) -> length $ filter ((== 1) . gcd x) xs) $
                     map reverse $ tail $ inits (oeis @5117)

instance OEIS 265845 where
  oeis = f (S.singleton (1, (1, 1))) 0 0 where
     f s z z' = if y == z && z' /= z then y : f s'' y z else f s'' y z
                where s'' = (S.insert (y', (i, j')) $
                             S.insert (y' - i ^ 3 , (i + 1, j')) s')
                      y' = y + j' ^ 3; j' = j + 1
                      ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 265848 where
  oeis = tablList @265848
instance Table 265848 where
  tabl = zipWith (++) ([] : (tabf @14413)) (tabf @34868)

instance OEIS 265912 where
  oeisIx = fromJust . (flip findIndex (tabl @7318)) . elem . (oeisIx @14631)

instance OEIS 276165 where
  oeisIx = minimax . (rowT @66099)

instance OEIS 276374 where
  oeis = map succ $ filter (\i -> (oeisIx @240024) i == (oeisIx @2808) i) [1..]

instance OEIS 276375 where
  oeis = (1 :) . map succ $ filter (\i -> (oeisIx @240024) (i + 1) == (oeisIx @2808) i) [1..]

instance OEIS 288208 where
  oeisIx = a . succ where
    pairs l = zip l (drop 1 l)
    d n = filter (all (uncurry (/=)) . zip [1..]) $ Data.List.permutations [1..n]
    a n = length $ filter (all ((1<) . abs . uncurry (-)) . pairs) $ d n

instance OEIS 308576 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @308576 (n - 1)) + (oeisIx @308576 (oeisIx @308576 (n - 1) `mod` n)) + 1

