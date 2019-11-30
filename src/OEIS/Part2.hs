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

rhonda b x = (oeisIx @1414.pred) x * b == product (unfoldr
    (\z -> if z == 0 then Nothing else Just $ swap $ divMod z b) x)


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

((oeis1597), (oeis25478), (oeis25479)) =
    unzip3 $ (1, 1, 2) : f 9 (3, 2) (M.singleton 4 (2, 2)) where
      f zz (bz, ez) m
        | xx < zz = (xx, bx, ex) :
                    f zz (bz, ez+1) (M.insert (bx*xx) (bx, ex+1) $ M.deleteMin m)
        | xx > zz = (zz, bz, 2) :
                    f (zz+2*bz+1) (bz+1, 2) (M.insert (bz*zz) (bz, 3) m)
        | otherwise = f (zz+2*bz+1) (bz+1, 2) m
        where (xx, (bx, ex)) = M.findMin m

instance OEIS 25478 where
  oeis = oeis25478
instance OEIS 25479 where
  oeis = oeis25479

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

instance OEIS 1751 where
  oeis = 2 : filter (\n -> ((oeisIx @10051) . pred $ div n $ gcd 2 n) == 1) [1..]

instance OEIS 1764 where
 oeis = 1 : [rowCol @258708 (2 * n) n | n <- [1..]]

instance OEIS 1783 where
  oeisIx = product . (rowT @38566) . succ

instance OEIS 1792 where
  oeis = scanl1 (+) (oeis @45623)

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

instance OEIS 3137 where
  oeis = tablList @3137
instance Table 3137 where
  rowCol = rowCol_off @3137 @1 @0
  rowT   = rowT_off @3137 @1
  tabf   = map reverse $ tail (tabf @30341)

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

instance OEIS 5214 where
  oeis = tail $ O.union (oeis @290) (oeis @217)

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

instance OEIS 5318 where
  oeis = 0 : 1 : zipWith (-)
     (map (* 2) $ tail (oeis @5318)) (map (oeisIx @5318) (oeis @83920))

instance OEIS 5382 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (subtract 1) . (* 2)) (oeis @40)

instance OEIS 5383 where
  oeis = [p | p <- (oeis @65091), (oeisIx @10051 . pred) ((p + 1) `div` 2) == 1]

instance OEIS 5385 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (`div` 2)) (oeis @40)

instance OEIS 5409 where
  oeis = 1 : scanl1 (+) (tail (oeis @1333))

instance OEIS 5412 where
  oeis = 1 : f 2 [1] where
     f v ws@ (w:_) = y : f (v + 2) (y : ws) where
                    y = v * w + (sum $ zipWith (*) ws $ reverse ws)

instance OEIS 5418 where
  oeisIx = sum . rowT @34851

instance OEIS 5473 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ map (+ 4) (oeis @290)

instance OEIS 5528 where
  oeis = filter (\x -> 2 * x <= (oeisIx @6530 . pred) (x ^ 2 + 1)) [1..]

instance OEIS 5574 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (+ 1) . (^ 2)) [0..]

instance OEIS 5592 where
  oeis = (1:) . map (subtract 1) $ tail $ zipWith (+) (oeis @1519) $ tail (oeis @1519)

instance OEIS 5598 where
  oeisIx n = 1 + sum (zipWith (*) [n, n - 1 .. 1] (oeis @10))

instance OEIS 5599 where
  oeis = scanl (+) 0 $ f (oeis @106400)
     where f (x:_:_:xs) = x : f xs

instance OEIS 5614 where
  oeis = map (1 -) (oeis @3849)

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

instance OEIS 5836 where
  oeis = filter ((== 1) . (oeisIx @39966)) [0..]

instance OEIS 5867 where
  oeis = scanl (*) 1 (oeis @6093)

instance OEIS 5900 where
  oeisIx n = sum $ zipWith (*) odds $ reverse odds
    where odds = take n (oeis @5408)
  oeis = scanl (+) 0 (oeis @1844)

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

instance OEIS 6206 where
  oeisIx (succ->n) = sum (map f $ (rowT @27750) n) `div` n where
     f d = (oeisIx @8683 . pred) (n `div` d) * (oeisIx @45 (d - 1) + (oeisIx @45) (d + 1))

instance OEIS 6252 where
  oeisIx 0 = 1
  oeisIx n = sum $ (rowT @48594) n

instance OEIS 6256 where
  oeis = f (tail (oeis @5809)) [1] where
     f (x:xs) zs = (sum $ zipWith (*) zs (oeis @5809)) : f xs (x : zs)

instance OEIS 6431 where
  oeis = elemIndices 1 (oeis @2635)

instance OEIS 6446 where
  oeis = filter (\x -> x `mod` (oeisIx @196) x == 0) [1..]

instance OEIS 6449 where
  oeisIx = sum . (rowT @45995)

instance OEIS 6450 where
  oeisIx = (oeisIx @40 . pred) . (oeisIx @40)
  oeis   = map (oeisIx @40 . pred) (oeis @40)

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

instance OEIS 6521 where
  oeis = filter (\x -> (oeisIx @51) x `mod` x == 0) [1..]

instance OEIS 6562 where
  oeis = filter ((== 1) . (oeisIx @10051).pred) (oeis @75540)

instance OEIS 6566 where
  oeisIx n = n * (3 * n - 1) * (3 * n - 2) `div` 2
  oeis = scanl (+) 0 (oeis @93485)

instance OEIS 6567 where
  oeis = filter f (oeis @40) where
     f p = (oeisIx @10051 . pred) q == 1 && q /= p  where q = (oeisIx @4086) p

instance OEIS 6659 where
  oeisIx (succ->n) = 2 * (rowCol @7318) (2 * n + 2) (n - 1)

instance OEIS 6666 where
  oeisIx = length . filter even . takeWhile (> 1) . (iterate (oeisIx @6370)) . succ

instance OEIS 6769 where
  -- oeisIx n = (oeis @50512) !! n
  oeis = 0 : 1 : 1 : (-1) : 1 : zipWith div (zipWith (+) (zipWith (*)
     (drop 4 (oeis @6769)) (drop 2 (oeis @6769)))
       (map (^ 2) (drop 3 (oeis @6769)))) (tail (oeis @6769))

instance OEIS 6881 where
  oeis = filter chi [1..] where
     chi n = p /= q && (oeisIx @10051) (pred q) == 1 where
        p = (oeisIx @20639) $ pred n
        q = n `div` p

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

instance OEIS 6960 where
  oeis = iterate (oeisIx @56964) 196

instance OEIS 6985 where
  oeis = 1 : map (oeisIx @45 . (+ 2)) (oeis @6985)

instance OEIS 6995 where
  oeis = 0 : filter ((== 1) . (oeisIx @178225)) (oeis @5408)

instance OEIS 6996 where
  oeisIx n = (rowCol @83093) (2 * n) n

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

instance OEIS 7066 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = if x `elem` zs then z + 2 else z + 3

instance OEIS 7068 where
  oeis = 1 : 3 : zipWith (+)
     (tail (oeis @7068)) (zipWith (*) (oeis @34) (oeis @7068))

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

instance OEIS 7456 where
  oeisIx 0 = 0
  oeisIx (succ->n) = (oeisIx @523 . pred) (n - 1) + mod n 2 + 1

instance OEIS 7459 where
  oeis = f 1 (oeis @40) where
    f q (p:ps) = if mod q (p - 1) == 0 then p : f (q * p ^ 2) ps else f q ps

instance OEIS 7466 where
  oeisIx (succ->n) = (rowCol @228643) n n

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

instance OEIS 7528 where
  oeis = [x | k <- [0..], let x = 6 * k + 5, (oeisIx @10051 . pred) x == 1]

instance OEIS 7556 where
  oeisIx 0 = 1
  oeisIx n = (rowCol @7318) (8 * n) (n - 1) `div` n

instance OEIS 7602 where
  oeis = map succ $ elemIndices 1 $ map (oeisIx @188642) [1..]

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

instance OEIS 7689 where
  oeisIx n = (oeisIx @79) n + (oeisIx @244) n

instance OEIS 7692 where
  oeis = findIndices (> 1) (oeis @25426)

instance OEIS 7694 where
  oeis = 1 : filter even (oeis @3586)

instance OEIS 7697 where
  oeisIx (succ->n) = 2 * (fromJust $ findIndex (>= n) (oeis @46921)) + 1

instance OEIS 7733 where
  oeisIx = (oeisIx @2326) . flip div 2 . subtract 1 . (oeisIx @265)

instance OEIS 7770 where
  oeis = filter ((== 1) . (oeisIx @103369) . pred) [1..]

instance OEIS 7774 where
  oeis = filter ((== 2) . (oeisIx @1221) . pred) [1..]

instance OEIS 7775 where
  oeis = 1 : filter ((> 5) . oeisIx @20639 . pred) [7..]

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

instance OEIS 7947 where
  oeisIx = product . (rowT @27748) . succ

instance OEIS 7948 where
  oeisIx = last . filter ((== 1) . (oeisIx @212793.pred)) . (rowT @27750).succ

instance OEIS 7956 where
  oeisIx = product . (rowT @27751) . succ

instance OEIS 7969 where
  oeis = filter ((== 1) . oeisIx @7968) [0..]

instance OEIS 7970 where
  oeis = filter ((== 2) . oeisIx @7968) [0..]

instance OEIS 7978 where
  oeisIx = head . ([1..] \\) . (rowT @27750) . succ

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

instance OEIS 8836 where
  oeisIx = (1 -) . (* 2) . (oeisIx @66829)

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

instance OEIS 8905 where
  oeisIx = (oeisIx @30) . (oeisIx @142)

instance OEIS 8908 where
  oeisIx = length . (rowT @70165) . succ

instance OEIS 8919 where
  oeis = [x | x <- [1..], let (x',m) = divMod (oeisIx @4086 x) x, m == 0, x' > 1]

instance OEIS 8937 where
  oeis = tail $ scanl1 (+) (oeis @73)

instance OEIS 8949 where
  oeis = tablList @8949
instance Table 8949 where
  tabl = map (scanl1 (+)) (tabl @7318)

instance OEIS 8957 where
  oeis = tablList @8957
instance Table 8957 where
  rowCol = rowCol_off @8957 @1 @1
  rowT   = rowT_off   @8957 @1
  tabl = map reverse (tabl @36969)

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

instance OEIS 10704 where
  oeisIx = (* 3) . (oeisIx @34)
  oeis = cycle [3,6]

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

instance OEIS 11540 where
  oeis = elemIndices 0 (oeis @168046)

instance OEIS 11769 where
  oeis = 1 : zipWith (-) (map (* 3) (oeis @11769)) (oeis @59727)

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

instance OEIS 13613 where
  oeis = tablList @13613
instance Table 13613 where
  tabl = zipWith (zipWith (*))
                 (tail $ inits (oeis @400)) (tabl @7318)

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

instance OEIS 14683 where
  oeisIx (succ->n) = n + (oeisIx @10051 . pred) n

instance OEIS 14688 where
  oeis = zipWith (+) [1..] (oeis @40)

instance OEIS 14980 where
  oeis = iterate (oeisIx @2620) 5

instance OEIS 15614 where
  oeisIx = (subtract 1) . (oeisIx @2088) . succ

instance OEIS 16052 where
  oeis = iterate (oeisIx @62028) 3

instance OEIS 16067 where
  oeis = map (+ 1)
       . findIndices (> 1)
       . zipTail (-)
       . scanl max 0
       $ oeis @46920

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

instance OEIS 18900 where
  oeis = elemIndices 2 (oeis @73267)

instance OEIS 19312 where
  oeisIx = t . enumFromTo 1.succ where
     t xs = foldl max 0 [z + t (xs \\ ds) | z <- xs,
               let ds = (rowT @27750) z, not $ null $ intersect xs $ init ds]

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

instance OEIS 23890 where
  oeisIx (succ->n) = sum $ zipWith (*) divs $ map ((1 -) . (oeisIx @10051 . pred)) divs
              where divs = (rowT @27750) n

(oeis5211, oeis24014) = unzip $ (1, 1) :  f 1 1
  where
    f i x | y > x     = (y, i) : f (i + 1) y
          | otherwise = f (i + 1) x
          where y = (oeisIx @5210) i

instance OEIS 5211 where
  oeis = oeis5211

instance OEIS 24206 where
  oeisIx (succ->n) = (n - 1) * (n + 3) `div` 4
  oeis = scanl (+) 0 $ tail (oeis @8619)

instance OEIS 24362 where
  oeisIx (succ->n) = sum [oeisIx @10052 y | x <- takeWhile (< nn) $ tail (oeis @290),
                               let y = nn - x, y <= x, gcd x y == 1]
              where nn = n ^ 2

instance OEIS 25475 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @961)

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

instance OEIS 26430 where
  oeis = scanl (+) 0 (oeis @1285)

instance OEIS 26532 where
  oeis = scanl (*) 1 $ (oeis @176059)

instance OEIS 26549 where
  oeis = scanl (*) 1 $ (oeis @10693)

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

instance OEIS 27926 where
  oeis = tablList @27926
instance Table 27926 where
  tabf = iterate (\xs -> zipWith (+) ([0] ++ xs ++ [0]) ([1,0] ++ xs)) [1]
  -- oeisIx_tabf' = zipWith (++) (tabl @104763) (map tail (tabl @105809))

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

instance OEIS 28263 where
  oeis = tablList @28263
instance Table 28263 where
  tabl = zipWith (zipWith (+)) (tabl @7318) (tabl @14410)

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

instance OEIS 28871 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) $ tail (oeis @8865)

instance OEIS 28906 where
  oeisIx = (oeisIx @4186) . (oeisIx @40)

instance OEIS 28982 where
  oeis = tail $ O.union (oeis @290) (oeis @1105)

instance OEIS 29578 where
  oeisIx n =  (n - n `mod` 2) `div` (2 - n `mod` 2)
  oeis = concat $ transpose [oeis @1477, (oeis @5843)]

instance OEIS 29609 where
  oeisIx n = (rowCol @29600) (2*n) n

instance OEIS 29742 where
  oeis = filter ((== 0) . (oeisIx @136522)) [1..]

instance OEIS 29743 where
  oeis = filter ((== 1) . (oeisIx @10051).pred) (oeis @10784)

instance OEIS 29837 where
  oeis = scanl1 (+) (oeis @209229)

instance OEIS 29854 where
  oeis = zipWith gcd (oeis @1043) $ tail (oeis @1043)

instance OEIS 29931 where
  oeisIx = sum . zipWith (*) [1..] . (rowT @30308)

instance OEIS 30078 where
  oeisIx = (oeisIx @578) . (oeisIx @40)
  oeis = map (oeisIx @578) (oeis @40)

instance OEIS 30096 where
  oeis = filter f (oeis @40) where
     f x = odd d && (x < 10 || f x') where (x', d) = divMod x 10

instance OEIS 30102 where
  oeisIx = foldl (\v d -> 3 * v + d) 0 . (rowT @30341)

instance OEIS 30109 where
  oeisIx = flip div 2 . subtract 1 . (oeisIx @30101) . succ

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

instance OEIS 30190 where
  oeis = concatMap reverse (tabf @30308)

instance OEIS 30230 where
  oeis = filter (odd . (oeisIx @1221) . pred) [1..]

instance OEIS 30231 where
  oeis = filter (even . (oeisIx @1221) . pred) [1..]

instance OEIS 30237 where
  oeis = tablList @30237
instance Table 30237 where
  tabl = map init $ tail (tabl @9766)

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

instance OEIS 32742 where
  oeisIx n = succ n `div` (oeisIx @20639) n

instance OEIS 32759 where
  oeis = map fi (2 : map read (zipWith (++) vs (tail us)) :: [Integer])
     where (us,vs) = unzip $ map ((splitAt 1) . show) (oeis @40)

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

instance OEIS 33493 where
  oeisIx = sum . (rowT @70165) . succ

instance OEIS 33496 where
  oeis = 1 : filter f [2, 4 ..] where
     f x = x == maximum (takeWhile (/= 1) $ iterate (oeisIx @6370) x)

instance OEIS 33648 where
  oeis = iterate (oeisIx @56964) 3

instance OEIS 33649 where
  oeis = iterate (oeisIx @56964) 5

instance OEIS 33650 where
  oeis = iterate (oeisIx @56964) 7

instance OEIS 33676 where
  oeisIx (succ->n) = last $ takeWhile (<= (oeisIx @196) n) $ (rowT @27750) n

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

instance OEIS 35306 where
  oeis = tablList @35306
instance Table 35306 where
  rowCol n k = (rowT @35306) n !! (k - 1)
  rowT 1 = [1,1]
  rowT n = concat $ transpose [rowT @27748 n, (rowT @124010) n]
  tabf = map (rowT @35306) [1..]

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

instance OEIS 36234 where
  oeisIx = (+ 1) . (oeisIx @720)

instance OEIS 36441 where
  oeis = tail (oeis @76271)

instance OEIS 36447 where
  oeis = iterate (oeisIx @4093) 1

instance OEIS 36467 where
  oeis = 1 : zipWith (-) (oeis @40) (oeis @36467)

instance OEIS 36561 where
  oeis = tablList @36561
instance Table 36561 where
  tabf = iterate (\xs@ (x:_) -> x * 2 : map (* 3) xs) [1]

instance OEIS 36581 where
  oeis = zipWith (\u v -> if u /= v then 2 * u + v - 1 else 2)
                         (oeis @10060) $ tail (oeis @10060)

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

instance OEIS 38137 where
  oeis = tablList @38137
instance Table 38137 where
  tabl = map reverse (tabl @37027)

instance OEIS 38194 where
  oeisIx = flip mod 9 . (oeisIx @40)

instance OEIS 38255 where
  oeis = tablList @38255
instance Table 38255 where
  tabl = map reverse (tabl @13613)

instance OEIS 38505 where
  oeis = zipWith (-) (tail (oeis @749)) (oeis @749)

instance OEIS 38548 where
  oeisIx (succ->n) = genericLength $ takeWhile (<= (oeisIx @196) n) $ (rowT @27750) n

instance OEIS 38566 where
  oeis = tablList @38566
instance Table 38566 where
  rowCol = rowCol_off @38566 @1 @1
  rowT   = rowT_off @38566 @1
  tabf   = zipWith (\v ws -> filter ((== 1) . (gcd v)) ws) [1..] (tabl @2260)

instance OEIS 38699 where
  oeisIx 0 = 3
  oeisIx n = until ((== 1) . (oeisIx @10051 . pred)) ((+ 1) . (* 2)) $ n

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

instance OEIS 39702 where
  oeisIx = (`mod` 4) . (oeisIx @40)

instance OEIS 39913 where
  oeis = tablList @39913
instance Table 39913 where
  tabl = [[0], [1, 1]] ++ f [0] [1, 1] where
     f us@ (u:us') vs@ (v:vs') = ws : f vs ws where
       ws = [u + v, u + v + v] ++ zipWith (+) us vs'

instance OEIS 39955 where
  oeis = filter ((== 1) . (`mod` 4)) (oeis @5117)

instance OEIS 39956 where
  oeis = filter even (oeis @5117)

instance OEIS 39957 where
  oeis = filter ((== 3) . (`mod` 4)) (oeis @5117)

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

instance OEIS 43096 where
  oeis = elemIndices 1 (oeis @196368)

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

instance OEIS 45844 where
  oeis = iterate (oeisIx @95815 . pred) 1

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

instance OEIS 46132 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) $ map (+ 4) (oeis @40)

instance OEIS 46301 where
  oeis = zipWith3 (((*) .) . (*))
                 (oeis @40) (tail (oeis @40)) (drop 2 (oeis @40))

--   oeisIx 0 = 0
--   oeisIx 1 = 0
--   oeisIx n = mexp (oeisIx' n)
--   oeisIx' n = (map trymove [0.. (div (n - 1) 2)])
--     where trymove k = nimSum (oeis !! k) (oeis !! (n-k-1))

instance OEIS 46669 where
 oeis = scanl1 (+) (oeis @20639)

instance OEIS 46670 where
  oeis = scanl1 (+) (oeis @6530)

instance OEIS 46704 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @7953)) (oeis @40)

instance OEIS 46897 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ zipWith
              (\p e -> if p == 2 then 3 else div (p ^ (e + 1) - 1) (p - 1))
              (rowT @27748 n) (rowT @124010 n)

instance OEIS 46899 where
  oeis = tablList @46899
instance Table 46899 where
  tabl = zipWith take [1..] $ transpose (tabl @7318)

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

instance OEIS 47242 where
  oeis = elemIndices 0 (oeis @214090)

instance OEIS 47355 where
  oeis = scanl (+) 0 (oeis @10702)

instance OEIS 47520 where
  oeisIx n = sum $ zipWith (*) (reverse $ genericTake n $ tail (oeis @290)) (oeis @79)

instance OEIS 47781 where
  oeisIx n = (rowCol @49600) (2 * n) n

instance OEIS 47791 where
  oeis = filter ((== 1) . (oeisIx @10051).pred . (oeisIx @62028)) [1..]

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

instance OEIS 48673 where
  oeisIx = (`div` 2) . (+ 1) . (oeisIx @45965)

instance OEIS 48760 where
  oeisIx = (^ 2) . (oeisIx @196)

instance OEIS 48772 where
  oeis = scanl1 (+) (oeis @48696)

instance OEIS 48803 where
  oeis = scanl (*) 1 (oeis @7947)

instance OEIS 48853 where
  oeisIx (succ->n) = (sum $ map (oeisIx @10051 . pred . fi . (read :: String -> Integer)) $ tail $ nub $ concat $ zipWith
    (\its tls -> map ((\xs ys d -> xs ++ (d:ys)) its tls) "0123456789")
      (map init $ tail $ inits $ show $ fi n) (tail $ tails $ show $ fi n)) -
        (oeisIx @10051 . pred) n

instance OEIS 48865 where
  oeisIx (succ->n) = sum [oeisIx @10051 (pred t) | t <- [1..n], gcd n t == 1]

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

instance OEIS 48973 where
  oeis = [1..] `O.minus` (oeis @5243)

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

instance OEIS 49343 where
  oeis = map fi $ elemIndices 0
     $ zipWith ((-) `on` (oeisIx @7953)) (oeis @5843) (oeis @290)

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

instance OEIS 50536 where
  oeis = iterate (oeisIx @217) 8

instance OEIS 50542 where
  oeis = iterate (oeisIx @217) 5

instance OEIS 50548 where
  oeis = iterate (oeisIx @217) 7

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

instance OEIS 51023 where
  oeisIx n = (rowCol @70950) n n

instance OEIS 51046 where
  oeis = filter
     (\x -> fi (oeisIx @720 $ pred x) > hs !! (x - 1)) [1..]
     where hs = zipWith (/)
                [1..] $ map (subtract 1.5) $ scanl1 (+) $ map (1 /) [1..]

instance OEIS 51064 where
  oeisIx = (+ 1) . length . takeWhile (== 3) . dropWhile (== 2) . (rowT @27746) . succ

instance OEIS 51129 where
  oeis = tablList @51129
instance Table 51129 where
  rowCol n k = k ^ (n - k)
  rowT   = rowT_off   @51129 @1
  tabl = zipWith (zipWith (^)) (tabl @2260) $ map reverse (tabl @2260)

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

instance OEIS 51352 where
  oeis = 0 : zipWith (+)
     (oeis @51352) (zipWith (*) [1..] $ map ((1 -) . (* 2)) (oeis @10051))

instance OEIS 51353 where
  oeis = 0 : zipWith (+) (oeis @51353)
     (zipWith (\chi x -> x * (chi * (x + 1) - 1)) (oeis @10051) [1..])

instance OEIS 51417 where
  oeis = zipWith div (tail (oeis @25547)) (oeis @25547)

instance OEIS 51451 where
  oeis = scanl1 lcm (oeis @961)

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

instance OEIS 51599 where
  oeis = tablList @51599
instance Table 51599 where
  tabl = map fst $ iterate f ([2], (oeis @1223)) where
     f (row, (d:ds)) =  (zipWith (+) ([d] ++ row) (row ++ [d]), ds)

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

instance OEIS 51634 where
  oeis = f (oeis @40) where
     f (p:qs@ (q:r:ps)) = if 2 * q > (p + r) then q : f qs else f qs

instance OEIS 51635 where
  oeis = g (oeis @40) where
     g (p:qs@ (q:r:ps)) = if 2 * q < (p + r) then q : g qs else g qs

instance OEIS 51638 where
  oeisIx = sum . (rowT @83093)

instance OEIS 51683 where
  oeis = tablList @51683
instance Table 51683 where
  rowCol = rowCol_off @51683 @1 @1
  rowT   = rowT_off   @51683 @1
  tabl = map fst $ iterate f ([1], 2) where
     f (row, n) = (row' ++ [head row' + last row'], n + 1) where
       row' = map (* n) row

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

instance OEIS 51802 where
  oeisIx 0 = 1
  oeisIx n = until (< 10) (oeisIx @51801) n

instance OEIS 51838 where
  oeis = (1:) . (3:) . map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @2110) $ tail (oeis @7504)

instance OEIS 51841 where
  oeisIx (succ->n) = (sum $ zipWith (\u v -> gcd 2 u * (oeisIx @8683.pred) u * 2 ^ v)
               ds $ reverse ds) `div` (2 * n) where ds = (rowT @27750) n

instance OEIS 51884 where
  oeis =  1 : f 1 (oeis @2808) where
     f x cs = y : f y (dropWhile (<= y) cs) where
       y = head [z | z <- cs, x `gcd` z == 1]

instance OEIS 51924 where
  oeis = zipWith (-) (tail (oeis @984)) (oeis @984)

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

instance OEIS 52294 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (oeisIx @120)) [1..]

instance OEIS 52343 where
  oeisIx = (flip div 2) . (+ 1) . (oeisIx @8441)

instance OEIS 52582 where
  oeis =  0 : 2 : zipWith
     div (zipWith (*) (tail (oeis @52582)) (drop 2 (oeis @290))) [1..]

instance OEIS 52849 where
  oeisIx n = if n == 0 then 0 else 2 * (oeisIx @142) n
  oeis = 0 : fs where fs = 2 : zipWith (*) [2..] fs

instance OEIS 52952 where
  oeis = 1 : 1 : zipWith (+)
     (oeis @59841) (zipWith (+) (oeis @52952) $ tail (oeis @52952))

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

instance OEIS 53650 where
  oeisIx = (oeisIx @51953 . pred) . (oeisIx @290) . succ

instance OEIS 53661 where
  oeis = filter (> 0) (oeis @175880)

instance OEIS 53669 where
  oeisIx (succ->n) = head $ dropWhile ((== 0) . (mod n)) (oeis @40)

instance OEIS 53671 where
  oeisIx (succ->n) = f $ drop 2 (oeis @40) where
     f (p:ps) | (n `mod` p) * ((n+1) `mod` p) * ((n+2) `mod` p) > 0 = p
              | otherwise = f ps

instance OEIS 53685 where
  oeis = dropWhile (<= 7) $ i (oeis @47211) (oeis @5382) where
     i xs'@ (x:xs) ys'@ (y:ys) | x < y     = i xs ys'
                             | x > y     = i xs' ys
                             | otherwise = x : i xs ys

instance OEIS 53735 where
  oeisIx = sum . (rowT @30341)

instance OEIS 53754 where
  oeis = 0 : filter (even . (oeisIx @70939)) [1..]

instance OEIS 53767 where
  oeis = scanl (+) 0 (oeis @2808)

instance OEIS 53810 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (oeisIx @100995) . pred) $ tail (oeis @961)

instance OEIS 53868 where
  oeis = filter (odd . (oeisIx @1065) . pred) [1..]

instance OEIS 53869 where
  oeis = filter (even . (oeisIx @1065) . pred) [1..]
  -- oeis = map (+ 1) $ findIndices even $ map (oeisIx @1065) [1..]

instance OEIS 53989 where
  oeisIx 0=3
  oeisIx n = head [k | k <- [1..], (oeisIx @10051) (k * n - 1) == 1]

instance OEIS 54008 where
  oeisIx (succ->n) = n `mod` (oeisIx @5 . pred) n

instance OEIS 54024 where
  oeisIx (succ->n) = mod (oeisIx @203 $ pred n) n

instance OEIS 54025 where
  oeis = zipWith mod (oeis @203) (oeis @5)

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

instance OEIS 54978 where
  oeis = map head $ iterate
                 (\lds -> map abs $ zipWith (-) (tail lds) lds) (oeis @959)

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

instance OEIS 55653 where
  oeisIx = sum . map (oeisIx @10 . pred) . (rowT @77610) . succ

instance OEIS 55654 where
  oeis = zipWith (-) [1..] (oeis @55653)

instance OEIS 55944 where
  oeisIx n = n + (oeisIx @30101) n

instance OEIS 55948 where
  oeisIx n = n + (oeisIx @30103) n

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

instance OEIS 56561 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @202018)) [0..]

instance OEIS 56924 where
  oeisIx = (`div` 2) . (oeisIx @5)

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

instance OEIS 57147 where
  oeisIx n = (oeisIx @7953) n * n

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

instance OEIS 58006 where
  oeis = scanl1 (+) (oeis @133942)

instance OEIS 58254 where
  oeis = scanl1 lcm (oeis @6093)

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

instance OEIS 58698 where
  oeis = map (pMemo 1) (oeis @40) where
     pMemo = memo2 integral integral p
     p _ 0 = 1
     p k m | m < k     = 0
           | otherwise = pMemo k (m - k) + pMemo (k + 1) m

instance OEIS 58841 where
  oeis =
     0 : (map length $ filter ((== 0) . head) $ group (oeis @58840))

instance OEIS 58922 where
  oeisIx (succ->n) = (n - 1) * 2 ^ n
  oeis = zipWith (*) [0..] $ tail (oeis @79)

instance OEIS 59015 where
  oeis = scanl1 (+) $ map (oeisIx @23416) [0..]

instance OEIS 59169 where
  oeis = map abs $ zipWith (-) (tail (oeis @178804)) (oeis @178804)

instance OEIS 59175 where
  oeisIx n = f [n % 1] where
     f xs@ (x:_) | denominator y == 1 = numerator y
                | y `elem` xs        = 0
                | otherwise          = f (y : xs)
                where y = (numerator x * denominator x) %
                          (oeisIx @7953 (numerator x) + (oeisIx @7953) (denominator x))

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

instance OEIS 59893 where
  oeisIx = foldl (\v b -> v * 2 + b) 1 . init . (rowT @30308) . succ

instance OEIS 59942 where
  oeis = map (foldr (\d v -> v * 2 + d) 0) $ f (tabf @30341) where
     f (xs:xss)
       | 0 `elem` xs = f xss
       | otherwise = map (fi . fromEnum) (zipWith (==)
                     (tail $ inits xs) (reverse $ init $ tails xs)) : f xss

instance OEIS 59943 where
  oeisIx = (* 2) . (oeisIx @59942)

instance OEIS 60036 where
  oeis = tablList @60036
instance Table 60036 where
  rowCol = rowCol_off @60036 @2 @1
  rowT = rowT_off @60036 @2
  tabl = map init $ tail (tabl @48152)

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

instance OEIS 60819 where
  oeisIx (succ->n) = n `div` (oeisIx @109008) n

instance OEIS 60984 where
  oeis = iterate (\x -> x + (oeisIx @48760) x) 1

instance OEIS 60985 where
  oeis = iterate (oeisIx @61885) 1

instance OEIS 61019 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map negate $ (rowT @27746) n

instance OEIS 61020 where
  oeisIx = sum . map (oeisIx @61019.pred) . (rowT @27750) . succ

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

instance OEIS 61357 where
  oeisIx (succ->n) = sum $
     zipWith (\u v -> (oeisIx @10051.pred) u * (oeisIx @10051.pred) v) [n+1..] $ reverse [1..n - 1]

instance OEIS 61397 where
  oeisIx (succ->n) = ((oeisIx @10051 . pred) n) * n

instance OEIS 61470 where
  oeis = elemIndices 1 (oeis @225693)

instance OEIS 61561 where
  oeis = iterate (oeisIx @55944) 22

instance OEIS 61673 where
  oeis = filter bothComp [4,6..] where
     bothComp n = (1 - (oeisIx @10051.pred) (n - 1)) * (1 - (oeisIx @10051.pred) (n+1)) > 0

instance OEIS 61883 where
  oeis = 1 : zipWith (-) (tail (oeis @60985)) (oeis @60985)

instance OEIS 61885 where
  oeisIx n = n + (oeisIx @57944) n

instance OEIS 61886 where
  oeis = 1 : zipWith (-) (tail (oeis @60984)) (oeis @60984)

instance OEIS 62028 where
  oeisIx n = oeisIx @7953 n + n

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

instance OEIS 62326 where
  oeisIx = (oeisIx @40) . pred . (oeisIx @137291)
  oeis = map (oeisIx @40) $
                 elemIndices 1 $ map (oeisIx @10051.pred) $ (oeis @49001)

instance OEIS 62509 where
  oeisIx (succ->n) = n ^ (oeisIx @1221 . pred) n

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

instance OEIS 62974 where
  oeis = map (+ 1) $ findIndices (< 0) $
     zipWith (-) (tail (oeis @1221)) $ map (* 2) (oeis @1221)

instance OEIS 63007 where
  oeis = tablList @63007
instance Table 63007 where
  tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @46899)

instance OEIS 63433 where
  oeis = iterate (oeisIx @56964) 10577

instance OEIS 63662 where
  oeis = iterate (oeisIx @63660) 0

instance OEIS 63908 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (subtract 3) . (* 2)) (oeis @40)

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

instance OEIS 64415 where
  oeisIx 0 = 0
  oeisIx (succ->n) = (oeisIx @3434 . pred) n - n `mod` 2

instance OEIS 64426 where
  oeis = zipWith (-) (tail (oeis @64413)) (oeis @64413)

instance OEIS 64437 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = if x `elem` zs then z + 3 else z + 2

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

instance OEIS 64664 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @64413)) . succ

instance OEIS 64806 where
  oeisIx n = 1 + n + (oeisIx @10888 . succ) n

instance OEIS 64847 where
  oeis = 1 : f [1,1] where
     f xs'@ (x:xs) = y : f (y : xs') where y = x * sum xs

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

instance OEIS 66339 where
  oeis = (0:) . scanl1 (+) $ map (oeisIx @79260) [1..]

instance OEIS 66459 where
  oeisIx = product . map (oeisIx @142) .
     unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 10)

instance OEIS 66490 where
  oeis = scanl1 (+) $ map (oeisIx @79261) [0..]

instance OEIS 66520 where
  oeis = scanl1 (+) $ map (negate . (oeisIx @151763)) [0..]

instance OEIS 66522 where
  oeis = filter f [1..] where
     f x = genericLength ds == maximum ds where ds = (rowT @161906) x

instance OEIS 66664 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) $ tail (oeis @66522)

instance OEIS 66681 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @66680)

instance OEIS 66710 where
  oeis = iterate (oeisIx @36839) 3

instance OEIS 66711 where
  oeis = iterate (oeisIx @36839) 9

instance OEIS 66713 where
  oeisIx = (oeisIx @36839) . (2 ^)

instance OEIS 66721 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @66720)

instance OEIS 66829 where
  oeisIx = (`mod` 2) . (oeisIx @1222)

instance OEIS 66839 where
  oeisIx = sum . (rowT @161906) . succ

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

instance OEIS 72047 where
  oeis = map (oeisIx @1221 . pred) $ (oeis @5117)

instance OEIS 72048 where
  oeisIx = (2 ^) . (oeisIx @72047)

instance OEIS 72055 where
  oeisIx = (+ 1) . (* 2) . (oeisIx @40)

instance OEIS 72057 where
  oeisIx = (oeisIx @203 . pred) . (oeisIx @72055)

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

instance OEIS 73579 where
  oeisIx n = p * (2 - p `mod` 4) where p = (oeisIx @40) n

instance OEIS 73703 where
  oeisIx (n) = head [p | p <- (oeis @40), (oeisIx @10051.pred) (p + 2 * (oeisIx @40) n) == 1]

instance OEIS 73734 where
  oeis = zipWith gcd (oeis @64413) $ tail (oeis @64413)

instance OEIS 73736 where
  oeis = scanl1 (+) (oeis @73737)

instance OEIS 73737 where
  oeis =
     1 : 1 : zipWith (-) (oeis @65091)
                         (zipWith (+) (oeis @73737) $ tail (oeis @73737))

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

instance OEIS 76052 where
  oeis = scanl1 (+) $ map (oeisIx @6460) [0..]

instance OEIS 76271 where
  oeis = iterate (oeisIx @70229 . pred) 1

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

instance OEIS 76479 where
  oeisIx = (oeisIx @8683) . pred . (oeisIx @7947)

instance OEIS 76489 where
  oeis = map (length . nub) $
                 zipWith intersect (tail (tabf @31298)) (tabf @31298)

instance OEIS 76644 where
  oeis = scanl1 (+) (oeis @122196)

instance OEIS 76805 where
  oeis = filter (not . ("13" `isInfixOf`) . show . fi) (oeis @40)

instance OEIS 77039 where
  oeis = scanl1 (+) (oeis @73579)

instance OEIS 77065 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (`div` 2) . succ) (oeis @6093)

instance OEIS 77068 where
  oeis = filter ((== 1) . (oeisIx @10051) . pred . (`div` 2)) (oeis @8864)

instance OEIS 77581 where
  oeis = tablList @77581
instance Table 77581 where
  rowCol = rowCol_off @77581 @1 @1
  rowT   = rowT_off   @77581 @1
  tabl = map (\x -> take x [z | z <- [1..], gcd x z == 1]) [1..]

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

instance OEIS 78898 where
  oeis = 0 : 1 : f M.empty 2 where
     f m x = y : f (M.insert p y m) (x + 1) where
             y = M.findWithDefault 0 p m + 1
             p = (oeisIx @20639.pred) x

instance OEIS 79066 where
  oeisIx (succ->n) = length $ filter (`isInfixOf` (primesDec !! n)) $ take n primesDec
    where
      primesDec = "_" : map (show.fi) (oeis @40)

instance OEIS 79079 where
  oeis = map (`div` 4) $
                 zipWith (*) (oeis @8864) $ tail (oeis @8864)

instance OEIS 79085 where
  oeisIx = (oeisIx @1221 . pred) . (oeisIx @79079)

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

instance OEIS 80237 where
  oeis = tablList @80237
instance Table 80237 where
  rowCol = rowCol_off @80237 @1 @1
  rowT   = rowT_off @80237 @1
  tabf = [1] : f (tabf @80237) where
     f [[]] =[]
     f (xs:xss) = concatMap (enumFromTo 1 . (+ 1)) xs : f xss

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

instance OEIS 80715 where
  oeis = 1 : filter (\x -> all ((== 1) . (oeisIx @10051.pred)) $
     zipWith (+) (rowT @27750 x) (reverse $ (rowT @27750) x)) [2,4..]

instance OEIS 80737 where
  oeis = 0 : (map f [2..]) where
    f n | mod n 4 == 2 = (oeisIx @80737 . pred) $ div n 2
        | otherwise = (oeisIx @67240 . pred) n

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

instance OEIS 84345 where
  oeis = filter ((== 0) . (oeisIx @10051.pred) . (oeisIx @120)) [0..]

instance OEIS 84888 where
  oeisIx = (oeisIx @25426) . (oeisIx @578)

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

instance OEIS 87112 where
  oeis = tablList @87112
instance Table 87112 where
  rowCol = rowCol_off @87112 @1 @1
  rowT n = map (* last ps) ps where ps = take n (oeis @40)
  tabl = map (rowT @87112) [1..]

instance OEIS 87226 where
  oeisIx = foldl1 lcm . (rowT @70165) . succ

instance OEIS 87279 where
  oeis = 0 : 2 : f (drop 2 (oeis @290))
     where f (x:xs) = x - 1 : x+1 : f xs

instance OEIS 87349 where
  oeisIx (succ->n) = (oeisIx @20639 n) + n

instance OEIS 87370 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . subtract 1 . (* 3)) [0..]

instance OEIS 87624 where
  oeisIx n = if (oeisIx @10051) n == 1 then 0 else (oeisIx @1221) n

instance OEIS 87695 where
  oeis = filter
     (\x -> (oeisIx @10051.pred) (x - 3) == 1 && (oeisIx @10051.pred) (x + 3) == 1) [2, 4 ..]

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

instance OEIS 88580 where
  oeisIx = (+ 1) . (oeisIx @203)
  oeis   = map succ $ oeis @203

instance OEIS 88732 where
  oeisIx n = head [q | q <- [2 * n + 1, 3 * n + 2 ..], (oeisIx @10051.pred) q == 1]

instance OEIS 88733 where
  oeisIx (succ->n) = last $ take n $
              [q | q <- [2 * n + 1, 3 * n + 2 ..], (oeisIx @10051.pred) q == 1]

instance OEIS 88878 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . subtract 2 . (* 3)) (oeis @40)

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

instance OEIS 90582 where
  oeis = tablList @90582
instance Table 90582 where
  rowCol = rowCol_off @90582 @1 @1
  rowT   = rowT_off   @90582 @1
  tabl = map reverse (tabl @19538)

instance OEIS 90636 where
  oeis = iterate (oeisIx @3415) 15

instance OEIS 90824 where
  oeis = tablList @90824
instance Table 90824 where
  tabl = zipWith (zipWith p)
     (map (\x -> map (`enumFromTo` x) [1..x+1]) [0..]) (tabl @7318)
     where p _          0 = 1
           p []         _ = 0
           p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 91441 where
  oeis = tablList @91441
instance Table 91441 where
  rowCol = rowCol_off @91441 @1 @1
  rowT   = rowT_off   @91441 @1
  tabl = iterate f [1] where
     f xs = zipWith (+)
       (zipWith (*) ([0] ++ xs) ks) (zipWith (*) (xs ++ [0]) (reverse ks))
       where ks = [1 .. 1 + genericLength xs]

instance OEIS 91633 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @136333)

instance OEIS 92391 where
  oeisIx n = n + oeisIx @120 n

instance OEIS 92392 where
  oeis = tablList @92392
instance Table 92392 where
  rowCol = rowCol_off @92392 @1 @1
  rowT   = rowT_off   @92392 @1
  tabl = map reverse (tabl @46899)

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

instance OEIS 93573 where
  oeis = tablList @93573
instance Table 93573 where
  rowCol n k = (rowT @93573) n !! (k- 1)
  rowT n = take n $ elemIndices n (oeis @20986)
  tabl = map (rowT @93573) [1..]

instance OEIS 93771 where
  oeis = [oeisIx @1597 x | x <- [1..], (oeisIx @10051.pred) (oeisIx @25479 x) == 1]

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

instance OEIS 94727 where
  oeis = tablList @94727
instance Table 94727 where
  rowCol n k = n + k
  rowT   = rowT_off   @94727 @1
  tabl = iterate (\row@ (h:_) -> (h + 1) : map (+ 2) row) [1]

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

instance OEIS 95259 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @95258)) . succ

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

instance OEIS 97062 where
  oeis = concat $ transpose [oeis @5408, (-1) : (oeis @5408)]

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

instance OEIS 98424 where
  oeisIx (succ->n) = genericLength [ (p,q,r) | p <- takeWhile (<= n) (oeis @40),
              let r = p + 6, (oeisIx @10051.pred) r == 1, q <- [p+1..r - 1], (oeisIx @10051.pred) q == 1]

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

instance OEIS 99036 where
  oeis = zipWith (-) (oeis @79) (oeis @45)

instance OEIS 99047 where
  oeis = [m | m <- [1..],
                      (oeisIx @10051.pred) (m - 1) == 0 && (oeisIx @10051.pred) (m + 1) == 0]

instance OEIS 99244 where
  oeisIx (succ->n) = gcd (oeisIx @70939 n) (oeisIx @120 n)

instance OEIS 99247 where
  oeis = filter ((== 1) . (oeisIx @99244 . pred)) [1..]

instance OEIS 99248 where
  oeis = map (+1) $ filter ((> 1) . (oeisIx @99244)) [1..]

instance OEIS 99249 where
  oeis = scanl1 (+) $ map ((0 ^) . (subtract 1)) (oeis @99244)

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

instance OEIS 99964 where
  oeis = tablList @99964
instance Table 99964 where
  tabf = scanl f [1] $ tail (oeis @10054) where
     f row t = if t == 1 then row' ++ [last row'] else row'
             where row' = scanl1 (+) $ reverse row

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

instance OEIS 100484 where
  oeis = map (* 2) (oeis @40)

instance OEIS 100587 where
  oeisIx = (subtract 1) . (2 ^) . (oeisIx @5)

instance OEIS 100717 where
  oeis = map (+1) $ elemIndices 0 (oeis @203908)

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

instance OEIS 102661 where
  oeis = tablList @102661
instance Table 102661 where
  rowCol = rowCol_off @102661 @1 @1
  rowT   = rowT_off   @102661 @1
  tabl = map (scanl1 (+) . tail) $ tail (tabl @48993)

instance OEIS 102696 where
  oeisIx (succ->n) = genericLength $ nub
     [p + q | p <- genericTake n (oeis @65091), q <- takeWhile (<= p) (oeis @65091)]

instance OEIS 102820 where
  oeis =  map (sum . (map (oeisIx @10051.pred))) $
     zipWith enumFromTo (oeis @100484) (tail (oeis @100484))

instance OEIS 103128 where
  oeisIx = (oeisIx @196) . (subtract 1) . (* 2) . succ

instance OEIS 103369 where
  oeisIx = until (`elem` (oeis @39943)) (oeisIx @3132) . succ

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

instance OEIS 106244 where
  oeis = map (p' 1) [0..] where
     p' = memo2 integral integral p
     p _ 0 = 1
     p k m = if m < pp then 0 else p' (k + 1) (m - pp) + p' (k + 1) m
             where pp = oeisIx @961 $ pred k

instance OEIS 107430 where
  oeis = tablList @107430
instance Table 107430 where
  tabl = map sort (tabl @7318)

instance OEIS 107715 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @7090)

instance OEIS 107743 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred) . (oeisIx @62028)) [1..]

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

instance OEIS 108561 where
  oeis = tablList @108561
instance Table 108561 where
  tabl = map reverse (tabl @112465)

instance OEIS 108617 where
  oeis = tablList @108617
instance Table 108617 where
  tabl = [0] : iterate f [1,1] where
     f row@ (u:v:_) = zipWith (+) ([v - u] ++ row) (row ++ [v - u])

instance OEIS 108731 where
  oeis = tablList @108731
instance Table 108731 where
  rowT 0 = [0]
  rowT n = t n $ reverse $ takeWhile (<= n) $ tail (oeis @142)
     where t 0 []     = []
           t x (b:bs) = x' : t m bs where (x',m) = divMod x b
  tabf = map (rowT @108731) [0..]

instance OEIS 108872 where
  oeis = tablList @108872
instance Table 108872 where
  rowCol = rowCol_off @108872 @1 @1
  rowT   = rowT_off   @108872 @1
  tabl = map (\x -> [x + 1 .. 2 * x]) [1..]

instance OEIS 109043 where
  oeisIx = (lcm 2)
  oeis = zipWith (*) [0..] (oeis @34)

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

instance OEIS 110654 where
  oeisIx = (`div` 2) . (+ 1)
  oeis = tail (oeis @4526)

instance OEIS 111060 where
  oeisIx 0 = 0
  oeisIx n = sum $ (rowT @265668 . succ) n

instance OEIS 111244 where
  oeis = scanl1 (+) (oeis @84385)

instance OEIS 111282 where
  oeis = 1 : (oeis @25169)

instance OEIS 112465 where
  oeis = tablList @112465
instance Table 112465 where
  tabl = iterate f [1] where
     f xs'@ (x:xs) = zipWith (+) ([-x] ++ xs ++ [0]) ([0] ++ xs')

instance OEIS 112632 where
  oeis = scanl1 (+) $ map negate (oeis @134323)

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

instance OEIS 114327 where
  oeis = tablList @114327
instance Table 114327 where
  tabl = zipWith (zipWith (-)) (tabl @25581) (tabl @2262)

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

instance OEIS 117128 where
  oeis = 1 : f 1 (oeis @40) (S.singleton 1) where
     f x (p:ps) s | x' > 0 && x' `S.notMember` s = x' : f x' ps (S.insert x' s)
                  | otherwise                  = xp : f xp ps (S.insert xp s)
                  where x' = x - p; xp = x + p

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

instance OEIS 118668 where
  oeisIx = (oeisIx @43537.pred) . (oeisIx @217)
  oeis = map (oeisIx @43537.pred) (oeis @217)

instance OEIS 118914 where
  oeis = tablList @118914
instance Table 118914 where
  rowCol = rowCol_off @118914 @2 @1
  rowT   = rowT_off @118914 @2
  tabf = map sort $ tail (tabf @124010)

instance OEIS 119345 where
  oeis = elemIndices 1 (oeis @52343)

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

instance OEIS 121281 where
  oeis = tablList @121281
instance Table 121281 where
  tabl = [1] : f [1] (oeis @40) where
     f xs@ (x:_) (p:ps) = ys : f ys ps where ys = (map (* p) xs) ++ [x]

instance OEIS 121539 where
  oeis = elemIndices 1 (oeis @35263)

instance OEIS 121573 where
  oeis = scanl1 (+) $ map (oeisIx @36263 . pred) [1, 3 ..]

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

instance OEIS 125022 where
  oeis = elemIndices 1 (oeis @161)

instance OEIS 125308 where
  oeis = 3 : h [1,3] where
     h (u:us) | null (show (fi v) `intersect` "245679") &&
                (oeisIx @10051 . pred) v == 1 = v : h (us ++ [v])
              | otherwise       = h (us ++ [v])
              where v = u + 10

instance OEIS 125855 where
  oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051 . pred)) $
     iterate (zipWith (+) [1, 1, 1, 1]) [1, 3, 7, 9]

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

instance OEIS 127118 where
  oeisIx n = (oeisIx @40) n * (oeisIx @18252) n

instance OEIS 127446 where
  oeis = tablList @127446
instance Table 127446 where
  rowCol = rowCol_off @127446 @1 @1
  rowT   = rowT_off   @127446 @1
  tabl = zipWith (\v ws -> map (* v) ws) [1..] (tabl @51731)

instance OEIS 127542 where
  oeisIx = genericLength . filter ((== 1) . (oeisIx @10051 . pred) . sum) .
                            subsequences . enumFromTo 1 . succ

instance OEIS 127739 where
  oeis = tablList @127739
instance Table 127739 where
  rowCol = rowCol_off @127739 @1 @1
  rowT   = rowT_off   @127739 @1
  tabl = zipWith ($) (map replicate [1..]) $ tail (oeis @217)

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

instance OEIS 129511 where
  oeis = filter (f [] . (rowT @27750)) [1..] where
     f _ [_] = True
     f zs (d:ds) = null (dds `O.isect` zs) && f (dds `O.union` zs) ds
                   where dds = map (subtract d) ds

instance OEIS 129512 where
  oeis = O.minus [1..] (oeis @129511)

instance OEIS 129521 where
  oeisIx n = p * (2 * p - 1) where p = (oeisIx @5382) n

instance OEIS 129805 where
  oeis = [x | x <- (oeis @56020), (oeisIx @10051 . pred) x == 1]

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

instance OEIS 130665 where
  oeisIx = sum . map (3 ^) . (`take` (oeis @120)) . (+ 1)

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

instance OEIS 131816 where
  oeis = tablList @131816
instance Table 131816 where
  tabl = map (map (subtract 1)) $
     zipWith (zipWith (+)) (tabl @130321) (tabl @59268)

instance OEIS 132159 where
  oeis = tablList @132159
instance Table 132159 where
  tabl = map reverse (tabl @121757)

instance OEIS 132213 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051 . pred) $
              nub $ genericTake n $ map (`mod` n) $ tail (oeis @290)

instance OEIS 132231 where
  oeis = [x | k <- [0..], let x = 30 * k + 7, (oeisIx @10051 . pred) x == 1]

instance OEIS 132240 where
  oeis = [x | x <- (oeis @175887), (oeisIx @10051 . pred) x == 1]

instance OEIS 132393 where
  oeis = tablList @132393
instance Table 132393 where
  tabl = map (map abs) (tabl @48994)

instance OEIS 132442 where
  oeis = tablList @132442
instance Table 132442 where
  rowCol = rowCol_off @132442 @1 @1
  rowT   = rowT_off   @132442 @1
  tabl   = map (map (oeisIx @203 . pred)) (tabl @50873)

instance OEIS 132678 where
  oeis = elemIndices 1 (oeis @96535)

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

instance OEIS 134640 where
  oeis = tablList @134640
instance Table 134640 where
  rowCol = rowCol_off @134640 @1 @1
  rowT n = sort $
     map (foldr (\dig val -> val * n + dig) 0) $ permutations [0 .. n - 1]
  tabf = map (rowT @134640) [1..]

instance OEIS 134941 where
  oeis = elemIndices 1 (oeis @178333)

instance OEIS 135837 where
  oeis = tablList @135837
instance Table 135837 where
  rowCol = rowCol_off @135837 @1 @1
  rowT   = rowT_off   @135837 @1
  tabl = [1] : [1, 2] : f [1] [1, 2] where
     f xs ys = ys' : f ys ys' where
       ys' = zipWith3 (\u v w -> 2 * u - v + 2 * w)
                      (ys ++ [0]) (xs ++ [0, 0]) ([0, 0] ++ xs)

instance OEIS 136522 where
  oeisIx n = fi . fromEnum $ n == (oeisIx @4086) n

instance OEIS 136798 where
  oeis = tail $ map (+ 1) $ elemIndices 1 $
     zipWith (*) (0 : (oeis @10051)) $ map (1 -) $ tail (oeis @10051)

instance OEIS 137291 where
  oeis = filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @49001).pred) [1..]

instance OEIS 137581 where
  oeisIx = (oeisIx @55641) . (oeisIx @4154)

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

instance OEIS 139532 where
  oeis = [x | x <- [0..], (oeisIx @10051 . pred) (24 * x + 19) == 1]

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

instance OEIS 144328 where
  oeis = tablList @144328
instance Table 144328 where
  rowCol = rowCol_off @144328 @1 @1
  rowT   = rowT_off   @144328 @1
  tabl = [1] : map (\xs@ (x:_) -> x : xs) (tabl @2260)

instance OEIS 144394 where
  oeis = tablList @144394
instance Table 144394 where
  rowCol = rowCol_off @144394 @4 @0
  rowT = rowT_off @144394 @4
  tabl = map (drop 2 . reverse . drop 2) $ drop 4 (tabl @7318)

instance OEIS 144757 where
  oeisIx (succ->n) = (oeisIx @108) (oeisIx @1222 n - 1) * (oeisIx @8480) n

instance OEIS 144907 where
  oeisIx (succ->x)
    | (oeisIx @10051 . pred) x == 1 = 1
    | x `mod` 4 == 0 = 2 * rad
    | otherwise      = rad  where rad = (oeisIx @7947.pred) x

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

instance OEIS 158405 where
  oeis = tablList @158405
instance Table 158405 where
  -- rowCol = rolCol_off @158405 @0 @1
  rowT   = rowT_off   @158405 @1
  tabl = map reverse (tabl @99375)

instance OEIS 158582 where
  oeis = [x | x <- [0..], (oeisIx @23416) x > 1]

instance OEIS 159477 where
  oeis = 1 : concat
     (zipWith (\p q -> genericReplicate (q - p) q)
              (oeis @8578) $ tail (oeis @8578))

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

instance OEIS 161187 where
  oeis = tail $ findIndices ((== 1) . (oeisIx @10052)) (oeis @89237)

instance OEIS 161188 where
  oeis = map (+ 1) $ findIndices ((== 1) . (oeisIx @10051 . pred)) $ tail (oeis @89237)

instance OEIS 161385 where
  oeisIx = (+ 1) . (oeisIx @161382)

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

instance OEIS 168396 where
  oeis = tablList @168396
instance Table 168396 where
  rowCol = rowCol_off @168396 @1 @1
  rowT   = rowT_off   @168396 @1
  tabl = [1] : f [[1]] where
     f xss = ys : f (ys : xss) where
       ys = (map sum $ zipWith take [2..] xss) ++ [1]

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

instance OEIS 174863 where
  oeis = scanl1 (+) (oeis @76479)

instance OEIS 175130 where
  oeis = map (+ 1) $ findIndices ((== 0) . (oeisIx @212793.pred)) $ tail (oeis @45)

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

instance OEIS 178943 where
  oeis = 2 : h (oeis @40) where
     h (p:qs@ (q:r:ps)) = if 2 * q /= (p + r) then q : h qs else h qs

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

instance OEIS 180149 where
  oeis = elemIndices 2 (oeis @2635)

instance OEIS 180639 where
  oeis = (0:) . scanl1 (+) $ map ((1 -) . (oeisIx @264739)) [1..]

instance OEIS 180662 where
  oeis = tablList @180662
instance Table 180662 where
  tabl = tail $ inits (oeis @1654)

instance OEIS 181717 where
  oeis = 0 : 1 : fc 1 0 where
     fc x x' = y : fc y x where y = (oeisIx @6370) (x + x')

instance OEIS 181900 where
  oeisIx n = (oeisIx @22998) n * n

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

instance OEIS 182579 where
  oeis = tablList @182579
instance Table 182579 where
  tabl = [1] : iterate (\row ->
    zipWith (+) ([0] ++ row) (zipWith (*) (row ++ [0]) (oeis @59841))) [1,2]

instance OEIS 183209 where
  oeis = tablList @183209
instance Table 183209 where
  rowCol = rowCol_off @183209 @1 @1
  rowT   = rowT_off @183209 @1
  tabf = [1] : iterate (\xs -> concat $
     transpose [map (oeisIx @32766) xs, map (oeisIx @16789 . subtract 1) xs]) [2]

instance OEIS 185550 where
  oeis = [0..] `O.minus` (oeis @185549)

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

instance OEIS 188068 where
  oeisIx = (subtract 2) . (oeisIx @7538)

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

instance OEIS 192545 where
  oeis = map (+2) $ elemIndices 0 $ map (oeisIx @48853) [1..]

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

instance OEIS 193711 where
  oeis = scanl1 (+) (oeis @5214)

instance OEIS 193854 where
  oeis = elemIndices 1 (oeis @62039)

instance OEIS 193926 where
  oeis = zipWith (-) (tail (oeis @62039)) (oeis @62039)

instance OEIS 195238 where
  oeis = filter (\x -> (oeisIx @1221 . pred) x `elem` [2,3] &&
                               (oeisIx @6530 . pred) x `elem` [5,7] &&
                               (mod x 7 == 0 || mod x 15 == 0)) [1..]

instance OEIS 195376 where
  oeis = map (`mod` 2) (oeis @64413)

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

instance OEIS 201208 where
  oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @34)

instance OEIS 201266 where
  oeisIx n = [d | d <- [1..], (oeisIx @175755) n `mod` d == 0] !! 6

instance OEIS 201629 where
  oeisIx = (* 2) . (oeisIx @4524) . (+ 1)

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

instance OEIS 205667 where
  oeis = map succ $ elemIndices 1 (oeis @39997)

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

instance OEIS 211520 where
  oeis = 0 : 0 : 0 : scanl1 (+) (oeis @178804)

instance OEIS 211538 where
  oeis = scanl (+) 0 (oeis @29578)

instance OEIS 212210 where
  oeis = tablList @212210
instance Table 212210 where
  rowCol = rowCol_off @212210 @1 @1
  rowT   = rowT_off   @212210 @1
  tabl = f $ tail $ zip (inits pis) (tails pis) where
     f ((xs,ys) : zss) = (zipWith (-) (map (+ last xs) (xs)) ys) : f zss
     pis = (oeis @720)

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

instance OEIS 216022 where
  oeisIx = genericLength .
     takeWhile (== 0) . zipWith (-) [1..] . sort . (rowT @70165) . succ

instance OEIS 216059 where
  oeisIx  (succ->n) = head $ enumFromTo 1 (maximum ts + 1) \\ ts
     where ts = (rowT @70165) n

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

instance OEIS 225761 where
  oeisIx = numerator . sum . map (recip . fi) . (rowT @70165) . succ

instance OEIS 225784 where
  oeisIx = denominator . sum . map (recip . fi) . (rowT @70165) . succ

instance OEIS 225817 where
  oeis = tablList @225817
instance Table 225817 where
  rowCol = rowCol_off @225817 @1 @1
  rowT   = rowT_off @225817 @1
  tabf = map (map (oeisIx @8683 . pred)) (tabf @27750)

instance OEIS 225843 where
  oeisIx = floor . sum . map (recip . fi) . (rowT @70165) . succ

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

instance OEIS 227915 where
  oeis = map succ $ elemIndices 4 $ tail $ oeis @228085

instance OEIS 228053 where
  oeis = tablList @228053
instance Table 228053 where
  tabl = iterate (\row@ (i:_) -> zipWith (+)
     ([- i] ++ tail row ++ [0]) ([0] ++ init row ++ [- i])) [- 1]

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

instance OEIS 231330 where
  oeis = tablList @231330
instance Table 231330 where
  tabf = map (sort . nub) (tabf @230871)

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

instance OEIS 242312 where
  oeis = tablList @242312
instance Table 242312 where
  tabl = map (map (oeisIx @10888)) (tabl @7318)

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

instance OEIS 243758 where
  oeis = scanl (*) 1 (oeis @234959)

instance OEIS 243987 where
  oeis = tablList @243987
instance Table 243987 where
  rowCol = rowCol_off @243987 @1 @1
  rowT   = rowT_off   @243987 @1
  tabl = map (scanl1 (+)) (tabl @51731)

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

instance OEIS 246438 where
  oeis = elemIndices 0 (oeis @164349)

instance OEIS 246439 where
  oeis = elemIndices 1 (oeis @164349)

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

instance OEIS 248110 where
  oeis = tablList @248110
instance Table 248110 where
  rowCol = rowCol_off @248110 @1 @1
  rowT   = rowT_off @248110 @1
  tabf = map (\x -> [x + 1 .. x + (oeisIx @7953) x]) [1 ..]

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

instance OEIS 248756 where
  oeis = f 1 [] where
     f x yvs = fst yw : f (x + 1) (yw:yvs) where
       yw = g 1 yvs
       g _ []          = (x, h)
       g k ((z,w):zws) = if w == h then (k, (oeisIx @120) k) else g (k + 1) zws
       h = (oeisIx @120) x

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

instance OEIS 249571 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . succ

instance OEIS 249594 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . (oeisIx @40)

instance OEIS 249595 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . (oeisIx @18252)

instance OEIS 249830 where
  oeis = elemIndices 0 (oeis @249832)

instance OEIS 249832 where
  oeisIx = (0 ^) . flip mod 10 . (oeisIx @93017)

instance OEIS 249943 where
  oeis = scanl1 max $ map (oeisIx @98551) [0..]

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

instance OEIS 251417 where
  oeis = map length $ group (oeis @251416)

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

instance OEIS 262069 where
  oeis = O.isect (oeis @2113) (oeis @262065)

instance OEIS 262188 where
  oeis = tablList @262188
instance Table 262188 where
  tabf = map (sort . nub . map (foldr (\d v -> 10 * v + d) 0) .
     filter (\xs -> length xs == 1 || last xs > 0 && reverse xs == xs) .
            concatMap (tail . inits) . tails) (tabf @31298)

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

instance OEIS 1481 where
  oeis = [x | x <- [0..], oeisIx @161 x > 0]

instance OEIS 1690 where
  oeis = elemIndices 0 (oeis @10056)

instance Table 228351 where
  rowT 0 = []
  rowT n = (oeisIx @1511 . pred) n : (rowT @228351) (n `div` 2^ (oeisIx @1511 $ pred n))
instance OEIS 228351 where
  oeis = concatMap (rowT @228351) [1..]

instance Table 80738 where
  tabf = f 3 (drop 2 (oeis @80737)) 3 (M.singleton 0 [2,1]) where
     f i xs'@ (x:xs) till m
       | i > till  = (reverse row) : f i xs' (3 * head row) m'
       | otherwise = f (i + 1) xs till (M.insertWith (++) (div x 2) [i] m)
       where ((_,row),m')  = M.deleteFindMin m
instance OEIS 80738 where
  oeis = tablList @80738

instance Table 66099 where
  tabf = map (rowT @66099) [1..]
  rowT n = reverse $ (rowT @228351) n
instance OEIS 66099 where
  oeis = tablList @66099

instance OEIS 7966 where
  oeisIx = (\(_,x,_,_,_) -> x) . happy

instance OEIS 7967 where
  oeisIx = (\(_,_,x,_,_) -> x) . happy

instance OEIS 7968 where
  oeisIx = (\(x,_,_,_,_) -> x) . happy


instance OEIS 100970 where
  oeis = filter (rhonda 8) (oeis @255805)

instance OEIS 100973 where
  oeis = filter (rhonda 9) (oeis @255808)

instance OEIS 255872 where
  oeisIx n = head $ filter (rhonda b) $ iterate zeroless 1 where
      zeroless x = 1 + if r < b - 1 then x else b * zeroless x'
                    where (x', r) = divMod x b
      b = (oeisIx @2808) n

instance OEIS 255880 where
  oeisIx n = (filter (rhonda b) $ iterate zeroless 1) !! (n - 1) where
      zeroless x = 1 + if r < b - 1 then x else b * zeroless x'
                    where (x', r) = divMod x b
      b = (oeisIx @2808) n

instance OEIS 255731 where
  oeis = filter (rhonda 60) $ iterate z 1 where
     z x = 1 + if r < 59 then x else 60 * z x' where (x', r) = divMod x 60

instance OEIS 255732 where
  oeis = filter (rhonda 20) $ iterate z 1 where
     z x = 1 + if r < 29 then x else 30 * z x' where (x', r) = divMod x 30

instance OEIS 255735 where
  oeis = filter (rhonda 18) $ iterate z 1 where
     z x = 1 + if r < 17 then x else 18 * z x' where (x', r) = divMod x 18

instance OEIS 255736 where
  oeis = filter (rhonda 30) $ iterate z 1 where
     z x = 1 + if r < 29 then x else 30 * z x' where (x', r) = divMod x 30

instance OEIS 100971 where
  oeis = filter (rhonda 12) $ iterate z 1 where
     z x = 1 + if r < 11 then x else 12 * z x' where (x', r) = divMod x 12

instance OEIS 100972 where
  oeis = filter (rhonda 14) $ iterate z 1 where
     z x = 1 + if r < 13 then x else 14 * z x' where (x', r) = divMod x 14

instance OEIS 100974 where
  oeis = filter (rhonda 15) $ iterate z 1 where
     z x = 1 + if r < 14 then x else 15 * z x' where (x', r) = divMod x 15

instance OEIS 100975 where
  oeis = filter (rhonda 16) $ iterate z 1 where
     z x = 1 + if r < 15 then x else 16 * z x' where (x', r) = divMod x 16

instance OEIS 99542 where
  oeis = filter (rhonda 10) [1..]

instance OEIS 100968 where
  oeis = filter (rhonda 4) (oeis @23705)

instance OEIS 100969 where
  oeis = filter (rhonda 6) (oeis @248910)

instance OEIS 264618 where
  oeisIx n = foldr (\b v -> 2 * v + b) 0 $ (reverse bs ++ (0 : bs))
              where bs = map fi $ (rowT @30308) n

instance OEIS 263847 where
  oeis = 0 : zipWith (-)
     (zipWith (-) (tail qs) qs) (drop 2 (oeis @41))
     where qs = es $ tail (oeis @41)
           es [] = []; es [x] = []; es (_:x:xs) = x : es xs

instance OEIS 258059 where
  oeisIx = f 0 . (rowT @30386) . succ where
     f i [] = i
     f i (t:ts) = if t == 1 then f (i + 1) ts else i

instance OEIS 258144 where
  oeisIx = sum . zipWith (*) (cycle [1, -1]) . (rowT @257241) . succ

instance OEIS 260022 where
  oeisIx = (oeisIx @6921) . (* 2)

instance OEIS 51431 where
  oeisIx = (flip div 3628800) . (oeisIx @142) . (+ 10)

instance OEIS 52485 where
  oeis = map (+2) $ elemIndices 0 $ tail $ oeis @112526

instance OEIS 52548 where
  oeisIx = (+ 2) . (oeisIx @79)
  oeis = iterate ((subtract 2) . (* 2)) 3

instance OEIS 56868 where
  oeis = filter (any (== 1) . pks) [1..] where
     pks x = [p ^ k `mod` q | let fs = (rowT @27748) x, q <- fs,
                              (p,e) <- zip fs $ (rowT @124010) x, k <- [1..e]]

instance OEIS 57945 where
  oeisIx n = g n $ reverse $ takeWhile (<= n) $ tail (oeis @217) where
     g 0 _      = 0
     g x (t:ts) = g r ts + a where (a,r) = divMod x t

instance OEIS 58071 where
  oeis = tablList @58071
instance Table 58071 where
  tabl = map (\fs -> zipWith (*) fs $ reverse fs) (tabl @104763)

instance OEIS 59941 where
  oeis = map (foldr (\d v -> v * 10 + d) 0) $ f (tabf @30341) where
     f (xs:xss)
       | 0 `elem` xs = f xss
       | otherwise = map (fi.fromEnum) (zipWith (==)
                     (tail $ inits xs) (reverse $ init $ tails xs)) : f xss

instance OEIS 60054 where
  oeis = -1 : map (numerator . sum) (tail $ zipWith (zipWith (%))
     (zipWith (map . (*)) (oeis @142) (tabf @242179)) (tabf @106831))

instance OEIS 64986 where
  oeisIx = p (tail (oeis @142)) where
     p _          0             = 1
     p fs'@ (f:fs) m | m < f     = 0
                    | otherwise = p fs' (m - f) + p fs m

instance OEIS 67078 where
  oeis = scanl (+) 1 (oeis @142)

instance OEIS 70960 where
  oeisIx (succ->n) = if n == 1 then 1 else 3 * (oeisIx @142) n `div` 2
  oeis = map (flip div 2) fs where fs = 3 : zipWith (*) [2..] fs

instance OEIS 75180 where
  oeis = map (denominator . sum) $ zipWith (zipWith (%))
     (zipWith (map . (*)) (oeis @142) (tabf @242179)) (tabf @106831)

instance OEIS 92246 where
  oeis = filter odd (oeis @69)

instance OEIS 92495 where
  oeisIx (succ->n) = fromJust $ find ((== 0) . (`mod` n)) $ (oeis @142)

instance OEIS 100732 where
  oeisIx = (oeisIx @142) . (oeisIx @8585)

instance OEIS 109681 where
  oeis = map (foldr (\d v -> 3 * v + d) 0) $ f (tabf @30341) where
     f vss = (g 0 vss) : f (tail vss)
     g k (ws:wss) = if k < length ws then ws !! k : g (k + 1) wss else []

instance OEIS 109682 where
  oeis = compl (oeis @109681) [0..] where
     compl us'@ (u:us) vs'@ (v:vs)
         | u == v    = compl us vs
         | u > 3 * v = v : compl us (delete u vs)
         | otherwise = compl us (delete u vs')

instance OEIS 109683 where
  oeisIx = (oeisIx @7089) . (oeisIx @109681)

instance OEIS 109735 where
  oeis = scanl1 (+) (oeis @109890)

instance OEIS 109736 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @109890)) . succ

instance OEIS 109890 where
  oeis = 1 : 2 : 3 : f (4, []) 6 where
     f (m,ys) z = g $ dropWhile (< m) $ (rowT @27750) z where
       g (d:ds) | elem d ys = g ds
                | otherwise = d : f (ins [m, m + 1 ..] (insert d ys)) (z + d)
       ins (u:us) vs'@ (v:vs) = if u < v then (u, vs') else ins us vs

instance OEIS 109906 where
  oeis = tablList @109906
instance Table 109906 where
  tabl = zipWith (zipWith (*)) (tabl @58071) (tabl @7318)

instance OEIS 115944 where
  oeisIx = p (tail (oeis @142)) where
     p _      0             = 1
     p (f:fs) m | m < f     = 0
                | otherwise = p fs (m - f) + p fs m

instance OEIS 117930 where
  oeisIx n = p (tail (oeis @142)) $ 2*n where
     p _          0             = 1
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 129893 where
  oeis = 1 : zipWith div (tail fs) fs where
     fs = map (oeisIx @142) (oeis @124)

instance OEIS 161466 where
  oeis = (rowT @27750) $ (oeisIx @142) 10

instance OEIS 262411 where
  oeis = 1 : f [1] (drop 2 (tabf @30341)) where
     f xs tss = g tss where
       g (ys:yss) | null (intersect its $ tail $ inits ys) &&
                    null (intersect tis $ init $ tails ys) = g yss
                  | otherwise = (foldr (\t v -> 3 * v + t) 0 ys) :
                                f ys (delete ys tss)
       its = init $ tails xs; tis = tail $ inits xs

instance OEIS 262412 where
  oeis = 1 : f [1] (drop 2 (tabf @30341)) where
     f xs tss = g tss where
       g (ys:yss) | null (intersect its $ tail $ inits ys) &&
                    null (intersect tis $ init $ tails ys) = g yss
                  | otherwise = (foldr (\t v -> 10 * v + t) 0 ys) :
                                f ys (delete ys tss)
       its = init $ tails xs; tis = tail $ inits xs

instance OEIS 265885 where
  oeisIx (succ->n) = n `bimpl` (oeisIx @40.pred) n where
     bimpl 0 0 = 0
     bimpl p q = 2 * bimpl p' q' + if u <= v then 1 else 0
                 where (p', u) = divMod p 2; (q', v) = divMod q 2

instance OEIS 187769 where
  oeis = tablList @187769
instance Table 187769 where
  tabf = [0] : [elemIndices (b, len - b) $
     takeWhile ((<= len) . uncurry (+)) $ zip (oeis @120) (oeis @23416) |
     len <- [1 ..], b <- [1 .. len]]

instance OEIS 187786 where
  oeis = tablList @187786
instance Table 187786 where
  rowT n = fromJust $ find (elem n) (tabf @187769)
  tabf   = map (rowT @187786) [0..]

instance OEIS 232642 where
  oeis = tablList @232642
instance Table 232642 where
  rowCol = rowCol_off @232642 @1 @1
  rowT   = rowT_off   @232642 @1
  tabf = f (tabf @82560) [] where
     f (xs:xss) zs = ys : f xss (sort (ys ++ zs)) where
       ys = [v | v <- xs, not $ O.member v zs]


instance OEIS 1694 where
  oeis = map succ $ elemIndices 1 $ oeis @112526

instance OEIS 38555 where
  oeisIx n = foldr (\d v -> v * 3 + d) 0 $
     zipWith (\x y -> (x + y) `mod` 3) ts $ tail ts
     where ts = (rowT @30341) n

instance OEIS 49388 where
  oeisIx = (flip div 5040) . (oeisIx @142) . (+ 7)

instance OEIS 49389 where
  oeisIx = (flip div 40320) . (oeisIx @142) . (+ 8)

instance OEIS 1178 where
  oeisIx = f 0 . succ where
     f j x = if x == y then j else f (j + 1) y  where y = oeisIx @1175 $ pred x


instance OEIS 6549 where
  oeis = [1,2,3,4,7,8] ++ f (drop 4 (oeis @40)) where
     f (p:ps) | (oeisIx @10055.pred) (p - 1) == 1 = (p - 1) : f ps
              | (oeisIx @10055.pred) (p + 1) == 1 = p : f ps
              | otherwise            = f ps

instance OEIS 32810 where
  oeisIx = f 0 . (+ 1) . succ where
     f y 1 = (oeisIx @4086) y
     f y x = f (10 * y + m + 2) x' where (x', m) = divMod x 2

instance OEIS 33294 where
  oeis = filter chi (oeis @290) where
    chi m = m `mod` 10 > 0 && head ds `elem` [1,4,5,6,9] &&
            (oeisIx @10052) (foldl (\v d -> 10 * v + d) 0 ds) == 1 where
      ds = unfoldr
           (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 10) m

instance OEIS 34020 where
  oeis = f [0..] (oeis @3136) where
     f (x:xs) ys'@ (y:ys) | x < y = x : f xs ys'
                         | otherwise = f xs ys

instance OEIS 36491 where
  oeisIx n = f z z where
     f x y | x `mod` 2401 == 0 = f (x `div` 49) (y `div` 7)
           | x `mod` 343 == 0  = y `div` 7
           | otherwise         = y
     z = (oeisIx @36490) n

instance OEIS 37800 where
  oeisIx = f 0 . (rowT @30308) where
     f c [_]          = c
     f c (1 : 0 : bs) = f (c + 1) bs
     f c (_ : bs)     = f c bs

instance OEIS 38528 where
  oeis = gen ([1], 1) where
     gen (_, 10) = []
     gen (ds, len)
        | len `elem` ds && chi ds
          = foldr (\u v -> u + 10*v) 0 ds : gen (succ (ds, len))
        | otherwise = gen (succ (ds, len))
     chi xs = null ys || ys /= xs && chi ys where
              ys = tr $ filter (/= length xs) xs
              tr zs = if null zs || last zs > 0 then zs else tr $ init zs
     succ ([], len)   = ([1], len + 1)
     succ (d : ds, len)
         | d < len = (head (dropWhile (<= d) (oeis @2024) \\ ds) : ds, len)
         | otherwise = (0 : ds', len') where (ds', len') = succ (ds, len)

instance OEIS 50264 where
  oeis = filter chi [2..] where
     chi n = f (oeis @40) where
        f (p:ps) | p*p > n   = True
                 | otherwise = 2 * abs (2 * (n `mod` p) - p) <= p && f ps

instance OEIS 55975 where
  oeisIx (succ->n) = (oeisIx @3188) n - (oeisIx @3188) (n - 1)
  oeis = zipWith (-) (tail (oeis @3188)) (oeis @3188)

instance OEIS 56970 where
  oeisIx n = p (oeis @47261) n where
     p _  0     = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 59481 where
  oeis = tablList @59481
instance Table 59481 where
  tabl = map reverse (tabl @100100)

instance OEIS 62010 where
  oeis = filter f [1..] where
     f x = any (== 0) $ map (mod x) lower where
         lower = map bas [1 + (oeisIx @54055) x .. 9]
         bas b = foldl (\v d -> b*v + d) 0 bas10
         bas10 = reverse $ unfoldr dig x where
            dig n = if n== 0 then Nothing else Just $ swap $ divMod n 10

instance OEIS 62320 where
  oeisIx = (^ 2) . (oeisIx @13929)

instance OEIS 63054 where
  oeis = iterate (oeisIx @56964) 1997

instance OEIS 63057 where
  oeis = iterate (oeisIx @56964) 7059

instance OEIS 63060 where
  oeis = iterate (oeisIx @56964) 10553

instance OEIS 63063 where
  oeis = iterate (oeisIx @56964) 10563

instance OEIS 68636 where
  oeisIx (succ->n) = min n $ (oeisIx @4086) n

instance OEIS 68637 where
  oeisIx (succ->n) = max n $ (oeisIx @4086) n

instance OEIS 69104 where
  oeis = map (+ 1) $ elemIndices 0 $ zipWith mod (drop 2 (oeis @45)) [1..]

instance OEIS 69905 where
  oeis = scanl (+) 0 (oeis @8615)

instance OEIS 72504 where
  oeisIx = foldl1 lcm . (rowT @161906) . succ

instance OEIS 73729 where
  oeisIx (succ->n) = 10 * (oeisIx @30) n + (oeisIx @10879) n

instance OEIS 73730 where
  oeisIx (succ->n) = 10 * (oeisIx @54055) n + (oeisIx @54054) n

instance OEIS 100100 where
  oeis = tablList @100100
instance Table 100100 where
  tabl = [1] : f (tabl @92392) where
     f (us : wss'@ (vs : wss)) = (vs !! 1 : us) : f wss'

instance OEIS 49398 where
  oeisIx = (flip div 362880) . (oeisIx @142) . (+ 9)

instance OEIS 50925 where
  oeis = 1 : -1 : (tail $ map (numerator . sum) $
     zipWith (zipWith (%))
     (zipWith (map . (*)) (drop 2 (oeis @142)) (tabf @242179)) (tabf @106831))

instance OEIS 50932 where
  oeis = 1 : map (denominator . sum) (zipWith (zipWith (%))
     (zipWith (map . (*)) (drop 2 (oeis @142)) (tabf @242179)) (tabf @106831))

instance OEIS 51035 where
  oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @14091)

instance OEIS 51135 where
  oeis = map length $ group (oeis @4001)

instance OEIS 51139 where
  oeisIx n = (oeisIx @994) (n + 2) - (oeisIx @995) (n + 2)

instance OEIS 100428 where
  oeis = f (oeis @2) where f (u:_:us) = u : f us

instance OEIS 100429 where
  oeis = g (oeis @2) where g (_:v:vs) = v : g vs

instance OEIS 3277 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @9195)

instance OEIS 3624 where
  oeis = filter ((== 1) . (oeisIx @9194.pred)) (oeis @2808)

instance OEIS 3990 where
  oeis = tablList @3990
instance Table 3990 where
  rowCol = rowCol_off @3990 @0 @1
  rowT   = rowT_off @3990 @1
  tabl   = zipWith (zipWith lcm) (tabl @2260) $ map reverse (tabl @2260)

instance OEIS 4613 where
  oeis = 1 : filter (all (== 1) . map (oeisIx @79260 . pred) . (rowT @27748)) [1..]

instance OEIS 4614 where
  oeis = 1 : filter (all (== 1) . map (oeisIx @79261 . pred) . (rowT @27748)) [1..]

instance OEIS 4957 where
  oeis = findIndices even (oeis @60142)

instance OEIS 5846 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @202018)

instance OEIS 6532 where
  oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @203) . pred) [1..]

instance OEIS 6577 where
  oeisIx (succ->n) = fromJust $ findIndex (n `elem`) (tabf @127824)

instance OEIS 6601 where
  oeis = map (+ 1) $ elemIndices 0 $
     zipWith3 (((+) .) . (+)) ds (tail ds) (drop 2 ds) where
     ds = map abs $ zipWith (-) (tail (oeis @5)) (oeis @5)

instance OEIS 6696 where
  oeis = 0 : f 1 [0] where
     f u vs = w : f (u + 1) (w : vs) where
       w = minimum $ zipWith (+)
           (reverse vs) (zipWith (*) (tail (oeis @79)) (map (+ u) vs))

instance OEIS 7015 where
  oeisIx (succ->n) = 1 + (fromJust $
              elemIndex 0 $ zipWith (-) (oeis @10) $ drop n (oeis @10))

instance OEIS 7340 where
  oeis = filter ((== 0) . (oeisIx @54025) . pred) (oeis @1599)

instance OEIS 7620 where
  oeis = 1 : filter (\x -> all (p $ (rowT @27751) x) [1..x]) [2..]
     where p _  0 = True
           p [] _ = False
           p ds'@ (d:ds) m = d <= m && (p ds (m - d) || p ds m)

instance OEIS 7691 where
  oeis = map succ $ elemIndices 1 $ oeis @17666

instance OEIS 7755 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @3434))

instance OEIS 7957 where
  oeis = findIndices (> 0) (oeis @196564)

instance OEIS 8365 where
  oeis = 1 : filter ((> 11) . (oeisIx @20639) . pred) [1..]

instance OEIS 8810 where
  oeisIx = ceiling . (/ 3) . fi . (oeisIx @290)
  oeis = [0,1,2,3,6] ++ zipWith5
                 (\u v w x y -> 2 * u - v + w - 2 * x + y)
     (drop 4 (oeis @8810)) (drop 3 (oeis @8810)) (drop 2 (oeis @8810))
     (tail (oeis @8810)) (oeis @8810)

instance OEIS 8833 where
  oeisIx (succ->n) = head $ filter ((== 0) . (mod n)) $
     reverse $ takeWhile (<= n) $ tail (oeis @290)

instance OEIS 10096 where
  oeisIx = genericLength . takeWhile (/= 0) . iterate (oeisIx @523 . pred) . succ

instance OEIS 11379 where
  oeisIx n = (oeisIx @290) n + (oeisIx @578) n

instance OEIS 11756 where
  oeis = map (oeisIx @40 . pred) $ tail (oeis @217)

instance OEIS 14320 where
  oeis = nub $ (oeis @1223)

instance OEIS 14567 where
  oeis = map succ $ elemIndices 1 $ oeis @9194

instance OEIS 14689 where
  oeisIx (succ->n) = (oeisIx @40 . pred) n - n

instance OEIS 15976 where
  oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @56964)) [1..]

instance OEIS 16035 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ map (oeisIx @10.pred) $ init $ tail $ (rowT @27750) n

instance OEIS 16726 where
  oeis = [1,2,6,9] ++ (f 5 $ drop 4 (oeis @1751)) where
     f n qs'@ (q:qs) | q < 2*n   = f n qs
                     | otherwise = q : f (n+1) qs'

instance OEIS 18825 where
  oeis = tail $ elemIndices 0 (oeis @25426)

instance OEIS 19302 where
  oeisIx = sum . zipWith (*) (oeis @10060) . rowT @7318

instance OEIS 20330 where
  oeisIx (succ->n) = foldr (\d v -> 2 * v + d) 0 (bs ++ bs) where
     bs = (rowT @30308) n

instance OEIS 20475 where
  oeis = 0 : map (sum . map (0 ^)) (tail (tabl @53200))

instance OEIS 20482 where
  oeisIx = last . (rowT @171637) . succ . succ

instance OEIS 20653 where
  oeis = concat $ map reverse $ tail (tabf @38566)

instance OEIS 23610 where
  oeis = f [1] $ drop 3 (oeis @45) where
     f us (v:vs) = (sum $ zipWith (*) us $ tail (oeis @45)) : f (v:us) vs

instance OEIS 23888 where
  oeisIx = sum . (rowT @210208) . succ

instance OEIS 23896 where
  oeisIx = sum . (rowT @38566) . succ

instance OEIS 23900 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (1 -) $ (rowT @27748) n

instance OEIS 23998 where
  oeis = 1 : f 2 [1] (tabl @132813) where
     f x ys (zs:zss) = y : f (x + 1) (ys ++ [y]) zss where
                       y = sum $ zipWith (*) ys zs

instance OEIS 24166 where
  oeisIx n = sum $ zipWith (*) [n+1,n..0] (oeis @578)

instance OEIS 24352 where
  oeis = 3 : drop 4 (oeis @42965)

instance OEIS 24359 where
  oeis = f 0 1 (oeis @20884) where
     f c u vs'@ (v:vs) | u == v = f (c + 1) u vs
                      | u /= v = c : f 0 (u + 1) vs'

instance OEIS 24409 where
  oeis = map (+ 1) $ findIndices (> 1) (oeis @24362)

instance OEIS 24431 where
  oeis = 1 : 2 : f [2, 1] [2 ..] where
     f ks@ (k:_) (j:js) =
       x : y : f (y : x : ks) ((js \\ map (y -) ks) \\ map (x -) ks)
       where y = x + j; x = 2 * k + 2

instance OEIS 24619 where
  oeis = map succ $ elemIndices 0 $ oeis @10055

instance OEIS 24620 where
  oeis = map succ $ elemIndices 1 $ oeis @25474

instance OEIS 24816 where
  oeisIx = sum . (rowT @173541) . succ

instance OEIS 24894 where
  oeisIx = flip div 5 . subtract 1 . (oeisIx @30430)

instance OEIS 24939 where
  oeisIx = p (oeis @65091) where
     p _  0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 26351 where
  oeis = findIndices odd (oeis @60142)

instance OEIS 26375 where
  oeisIx n = (rowCol @26374) (2 * n) n

instance OEIS 26424 where
  oeis = filter (odd . (oeisIx @1222) . pred) [1..]

instance OEIS 26465 where
  oeis = map length $ group (oeis @10060)

instance OEIS 27306 where
  oeisIx n = (rowCol @8949) n (n `div` 2)

instance OEIS 27810 where
  oeisIx n = (n + 1) * (rowCol @7318) (n + 5) 5

instance OEIS 27818 where
  oeisIx n = (n + 1) * (rowCol @7318) (n + 6) 6

instance OEIS 30293 where
  oeis = filter ((<= 2) . (oeisIx @43537)) (oeis @578)

instance OEIS 34262 where
  oeisIx n = (oeisIx @578) n + n

instance OEIS 46530 where
  oeisIx (succ->n)
    = genericLength $ nub $ map (`mod` n) $ take n $ tail (oeis @578)

instance OEIS 46642 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @9191)

instance OEIS 46712 where
  oeis = filter ((`elem` [1,2]) . (`mod` 4)) (oeis @22544)

instance OEIS 48762 where
  oeisIx n = last $ takeWhile (<= n) (oeis @578)

instance OEIS 48763 where
  oeisIx 0 = 0
  oeisIx n = head $ dropWhile (< n) (oeis @578)

instance OEIS 52248 where
  oeis = f (oeis @65091) where
     f (p:ps'@ (p':ps)) = (maximum $ map (oeisIx @6530.pred) [p+1..p'- 1]) : f ps'

instance OEIS 56001 where
  oeisIx n = (n + 1) * (rowCol @7318) (n + 7) 7

instance OEIS 56003 where
  oeisIx n = (n + 1) * (rowCol @7318) (n + 8) 8

instance OEIS 56114 where
  oeisIx n = (n + 1) * (rowCol @7318) (n + 9) 9

instance OEIS 74989 where
  oeisIx 0 = 0
  oeisIx n = min (n - last xs) (head ys - n) where
     (xs,ys) = span (< n) (oeis @578)

instance OEIS 77063 where
  oeisIx = (oeisIx @7947 . pred) . (oeisIx @6093)

instance OEIS 79229 where
  oeis = f (oeis @7947) where
     f (x:xs) = ((+ 1) . length $ takeWhile (<= x) xs) : f xs

instance OEIS 80170 where
  oeis = filter f [1..] where
     f x = foldl1 gcd (map (flip (rowCol @7318) x) [2*x, 3*x .. x* (x+1)]) == 1

instance OEIS 94178 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @125203)

instance OEIS 94328 where
  oeis = iterate (oeisIx @6369) 4

instance OEIS 94329 where
  oeis = iterate (oeisIx @6369) 16

instance OEIS 119629 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @14631)) . succ

instance OEIS 122953 where
  oeisIx = genericLength . (rowT @165416) . succ

instance OEIS 123581 where
  oeis = iterate (oeisIx @70229 . pred) 3

instance OEIS 123976 where
  oeis = map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @45) [1..]

instance OEIS 124056 where
  oeis = 1 : f [1] where
     f xs@ (x:_) = y : f (y : xs) where
       y = length $ filter (flip elem $ (rowT @27750) x) xs

instance OEIS 124134 where
  oeis = filter ((> 0) . (oeisIx @161) . (oeisIx @45)) [1..]

instance OEIS 124837 where
  oeisIx (succ->n) = (rowCol @213998) (n + 2) (n - 1)

instance OEIS 124838 where
  oeisIx (succ->n) = (rowCol @213999) (n + 2) (n - 1)

instance OEIS 124934 where
  oeis = map (+ 1) $ findIndices (> 0) (oeis @125203)

instance OEIS 124978 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (tail (oeis @2635))) . succ

instance OEIS 125086 where
  oeis = f [0, 2 ..] (oeis @36990) where
     f (u:us) vs'@ (v:vs) = if u == v then f us vs else u : f us vs'

instance OEIS 125217 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @125203)

instance OEIS 125218 where
  oeis = map (+ 1) $ findIndices (> 1) (oeis @125203)

instance OEIS 137948 where
  oeis = tablList @137948
instance Table 137948 where
  tabl = zipWith (zipWith div) (tabl @245334) (tabl @7318)

instance OEIS 157996 where
  oeis = map (+ 1) $ filter f (oeis @6093) where
     f x = g $ takeWhile (< x) (oeis @65091) where
       g []  = False
       g [_] = False
       g (p:ps@ (_:qs)) = (x - p) `elem` qs || g ps

instance OEIS 160113 where
  oeisIx = (oeisIx @60431 . pred) . (2 ^)

instance OEIS 160516 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75075)) . succ

instance OEIS 168223 where
  oeisIx n = (oeisIx @6369) n - (oeisIx @6368) n

instance OEIS 168559 where
  oeis = scanl (+) 0 $ drop 2 (oeis @290)

instance OEIS 169718 where
  oeisIx = p [1,5,10,25,50,100] where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 169834 where
  oeis = f (oeis @51950) [1..] where
     f (0:0:ws) (x:y:zs) = y : f (0:ws) (y:zs)
     f (_:v:ws) (_:y:zs) = f (v:ws) (y:zs)

instance OEIS 185080 where
  oeisIx (succ->n) = 6 * (rowCol @7318) (2 * n) (n - 1) + (rowCol @7318) (2 * n - 1) n

instance OEIS 185154 where
  oeis = catMaybes $ map f (oeis @6093) where
     f x = g $ takeWhile (< x) (oeis @65091) where
       g []  = Nothing
       g [_] = Nothing
       g (p:ps@ (_:qs)) | (x - p) `elem` qs = Just p
                       | otherwise         = g ps

instance OEIS 185212 where
  oeisIx = (+ 1) . (* 4) . (oeisIx @567)

instance OEIS 185589 where
  oeis = iterate (oeisIx @6369) 144

instance OEIS 185590 where
  oeis = iterate (oeisIx @6369) 44

instance OEIS 185635 where
  oeis = filter (> 0) $
     zipWith (\x y -> if x == y then y else 0) [1..] (oeis @75075)

instance OEIS 190311 where
  oeisIx n = g n $ reverse $ takeWhile (<= n) $ tail (oeis @578) where
    g _ []                 = 0
    g m (x:xs) | x > m     = g m xs
               | otherwise = signum m' + g r xs where (m',r) = divMod m x

instance OEIS 247233 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75323)) . (oeisIx @65091)

instance OEIS 261893 where
  oeisIx n = n * (n * (n + 2) + 3) + 1
  oeis = zipWith (-) (tail (oeis @578)) (oeis @290)

instance OEIS 2100 where
  oeisIx = p (oeis @6881) . succ where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m


instance OEIS 2294 where
  oeis = 1 : [rowCol @258708 (3 * n) (2 * n) | n <- [1..]]

instance OEIS 2457 where
  oeisIx n = (rowCol @116666) (2 * n + 1) (n + 1)

instance OEIS 2654 where
  oeisIx n = product $ zipWith f ((rowT @27748) m) ((rowT @124010) m) where
     f p e | p `mod` 4 == 1 = e + 1
           | otherwise      = (e + 1) `mod` 2
     m = (oeisIx @265) n

instance OEIS 2690 where
  oeisIx (n) = (rowCol @245334) (2 * n) n

instance OEIS 2694 where
  oeisIx (succ.succ->n) = (rowCol @7318) (2 * n) (n - 2)

instance OEIS 3116 where
  oeisIx 0 = 1
  oeisIx 1 = 1
  oeisIx (pred->n) = (rowCol @168396) (2 * n + 1) n

instance OEIS 3169 where
  oeisIx = flip (rowCol @100326) 0 . succ

instance OEIS 46101 where
  oeis = filter ((> 3) . (oeisIx @51903) . pred) [1..]

instance OEIS 46315 where
  oeis = filter odd (oeis @1358)

instance OEIS 46665 where
  oeisIx n = (oeisIx @6530) n - (oeisIx @20639) n

instance OEIS 46711 where
  oeis = [x | x <- (oeis @42963), (oeisIx @161) x > 0]

instance OEIS 46727 where
  oeis = 0 : f (tail (oeis @1652)) (tail (oeis @46090)) where
     f (x:_:xs) (_:y:ys) = x : y : f xs ys

instance OEIS 46818 where
  oeisIx = (oeisIx @120) . (oeisIx @16777)

instance OEIS 46831 where
  oeis = filter ((> 0) . (`mod` 10)) (oeis @18834)

instance OEIS 46970 where
  oeisIx 0 = 1
  oeisIx n = product . map ((1 -) . (^ 2)) . (rowT @27748) $ n + 1

instance OEIS 47160 where
  oeisIx (succ->n) = if null ms then -1 else head ms
              where ms = [m | m <- [0 .. n - 1],
                              (oeisIx @10051) (n - m) == 1, (oeisIx @10051) (n + m) == 1]

instance OEIS 36998 where
  oeisIx (succ->n) = p (rowT @38566 n) n where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 38163 where
  oeis = map
      (sum . zipWith (*) (intersperse 0 $ tail (oeis @217)) . reverse) $
      tail $ inits $ tail (oeis @217) where

instance OEIS 38444 where
  oeis = 11 : f [11] 90 where
     f xs@ (x:_) z = ys ++ f ys (10 * z) where
                    ys = (x + z) : map (* 10) xs

instance OEIS 38507 where
  oeisIx = (+ 1) . (oeisIx @142)
  oeis = 2 : f 1 2 where
     f x y = z : f (x + 1) z where z = x * (y - 1) + 1

instance OEIS 38509 where
  oeis = [x | x <- (oeis @2808), gcd x 6 == 1]

instance OEIS 38529 where
  oeisIx n = (oeisIx @40) n - (oeisIx @2808) n

instance OEIS 38549 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @38548)) . succ

instance OEIS 38550 where
  oeis = map succ $ elemIndices 2 $ oeis @1227

instance OEIS 38561 where
  oeisIx = head . (rowT @46937)

instance OEIS 38610 where
  oeisIx = foldl lcm 1 . (rowT @38566) . succ

instance OEIS 38618 where
  oeis = filter ((== 1) . (oeisIx @168046)) (oeis @40)

instance OEIS 38720 where
  oeis = (transpose $ map reverse (tabl @38719)) !! 1

instance OEIS 38721 where
  oeis = (transpose (tabl @38719)) !! 2

instance OEIS 38731 where
  oeis = c [1] $ tail (oeis @45) where
     c us vs'@ (v:vs) = (sum $ zipWith (*) us vs') : c (v:us) vs

instance OEIS 39701 where
  oeisIx = (`mod` 3) . (oeisIx @40)
  oeis   = map (`mod` 3) (oeis @40)

instance OEIS 39833 where
  oeis = f (oeis @6881) where
     f (u : vs@ (v : w : xs))
       | v == u+1 && w == v+1 = u : f vs
       | otherwise            = f vs

instance OEIS 40027 where
  oeisIx n = head $ (rowT @46936) (n + 1)

instance OEIS 40040 where
  oeisIx = flip div 2 . (oeisIx @14574)

instance OEIS 41013 where
  oeis = 1 : f 1 where
     f x | rev <= x  = (2*x) : f (2*x)
         | otherwise = rev : f rev where rev = (oeisIx @4086) x

instance OEIS 43276 where
  oeisIx = maximum . (rowT @101211) . succ

instance OEIS 43545 where
  oeisIx = (1 -) . (oeisIx @36987)

instance OEIS 44051 where
  oeisIx = (`div` 2) . (+ 1) . (oeisIx @6995) . (+ 1)

instance OEIS 44813 where
  oeis = filter p [1..] where
     p x = nub xs == xs where
           xs = map length $ group $ (rowT @30308) x

instance OEIS 45572 where
  oeis = filter ((/= 0) . (`mod` 5)) (oeis @5408)

instance OEIS 46092 where
  oeisIx = (* 2) . (oeisIx @2378)

instance OEIS 46100 where
  oeis = filter ((< 4) . (oeisIx @51903)) [1..]

----- aaa


instance OEIS 264619 where
  oeisIx 0 = 1
  oeisIx n = foldr (\b v -> 2 * v + b) 0 $ (reverse bs ++ (1 : bs))
              where bs = map fi $ (rowT @30308) n


instance OEIS 263845 where
  oeis = filter (> 0) (oeis @258059)


instance OEIS 263766 where
  oeis = scanl (*) 1 (oeis @8865)


instance OEIS 262703 where
  oeis = zipWith (-) (tail (oeis @252001)) (oeis @252001)


instance OEIS 262604 where
  oeis = zipWith (-) (tail (oeis @252022)) (oeis @252022)


instance OEIS 262460 where
  oeis = 1 : f [1] (drop 2 (tabf @262437)) where
     f xs tss = g tss where
       g (ys:yss) | null (intersect its $ tail $ inits ys) &&
                    null (intersect tis $ init $ tails ys) = g yss
                  | otherwise = (foldr (\t v -> 16 * v + t) 0 ys) :
                                f ys (delete ys tss)
       its = init $ tails xs; tis = tail $ inits xs


instance OEIS 262438 where
  oeisIx = genericLength . (rowT @262437)


instance OEIS 262223 where
  oeisIx n = n + (oeisIx @47813) n


instance OEIS 262079 where
  oeis = zipWith (-) (tail (oeis @262065)) (oeis @262065)


instance OEIS 262038 where
  oeis = f 0 (oeis @2113) where
     f n ps'@ (p:ps) = p : f (n + 1) (if p > n then ps' else ps)


instance OEIS 261930 where
  oeisIx = sum . (rowT @261897)


instance OEIS 261890 where
  oeis = zipWith (-) (tail (oeis @261869)) (oeis @261869)


instance OEIS 261869 where
  oeis = zipWith (-) (tail (oeis @55615)) (oeis @55615)


instance OEIS 261723 where
  oeis = concat $ transpose [tail (oeis @52548), tail (oeis @51)]


instance OEIS 261607 where
  oeisIx = last . (rowT @261575)


instance OEIS 261598 where
  oeisIx = product . (rowT @261575)


instance OEIS 261587 where
  oeisIx = sum . (rowT @261575)


instance OEIS 261585 where
  oeisIx = genericLength . (rowT @261575)


instance OEIS 261366 where
  oeisIx = sum . map ((1 -) . flip mod 2) . (rowT @261363)


instance OEIS 261294 where
  oeisIx = (oeisIx @65650) . (oeisIx @65650)


instance OEIS 261293 where
  oeisIx = (oeisIx @65649) . (oeisIx @65649)


instance OEIS 261279 where
  oeis = [x | x <- [0..], (oeisIx @65649) x == x]


instance OEIS 261255 where
  oeisIx n = fromJust (findIndex (== (oeisIx @7335) n) (oeis @3586)) + 1


instance OEIS 261089 where
  oeisIx = fromJust . (`elemIndex` (oeis @155043))


instance OEIS 261009 where
  oeisIx = (oeisIx @53735) . (oeisIx @79)


instance OEIS 260933 where
  oeis = f 1 [1..] where
     f x zs = g zs where
       g (y:ys) = if (oeisIx @10051 . pred) (x + y) == 0 && (oeisIx @10051 . pred) (x + y + 1) == 0
                     then y : f (x + 1) (delete y zs) else g ys


instance OEIS 260895 where
  oeisIx = sum . map (oeisIx @10051 . pred) . (rowT @77664) . succ


instance OEIS 260822 where
  oeis = f 1 [1..] where
     f x zs = g zs where
       g (y:ys) = if y /= x && (oeisIx @10051 . pred) (x + y) == 0
                     then y : f (x + 1) (delete y zs) else g ys


instance OEIS 260706 where
  oeisIx = sum . (rowT @260672)


instance OEIS 260682 where
  oeis = filter ((== 1) . flip mod 6) (oeis @3136)


instance OEIS 260669 where
  oeisIx = flip div 2 . (oeisIx @54440) . succ

instance OEIS 260664 where
  oeisIx = sum . zipWith (*) (oeis @87960) . map (oeisIx @133042) . (rowT @260672)

instance OEIS 259966 where
  oeis = 0 : 0 : 2 : 7 : zipWith (+)
     (zipWith3 (((+) .) . (+))
               (oeis @259966) (drop 2 (oeis @259966)) (drop 3 (oeis @259966)))
     (drop 2 $ zipWith (+)
               (map (* 2) $ drop 2 (oeis @5251)) (map (* 3) (oeis @5251)))

instance OEIS 259315 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @52382)

instance OEIS 259046 where
  oeisIx = fromJust . (`elemIndex` (map (oeisIx @259043) [0..]))

instance OEIS 258765 where
  oeisIx = fromJust . (`elemIndex` (map abs (oeis @258057)))

instance OEIS 258565 where
  oeisIx = (oeisIx @3415) . (oeisIx @1694)

instance OEIS 258469 where
  oeis = map (oeisIx @258432) $ (map succ $ elemIndices 1 $ tail $ oeis @258383)

instance OEIS 258432 where
  oeis = map (snd . head) $
                      groupBy ((==) `on` fst) $ zip (oeis @62234) [1..]

instance OEIS 258383 where
  oeis = map length $ group (oeis @62234)

instance OEIS 258317 where
  oeisIx = sum . (rowT @258197)

instance OEIS 258290 where
  oeisIx = (oeisIx @3415) . (oeisIx @984)

instance OEIS 258143 where
  oeisIx = sum . (rowT @257241) . succ

instance OEIS 258087 where
  oeis = f 0 [0] $
     map (\i -> take (i + 1) (repeat 0) ++ replicate (i + 2) i) [0..] where
     f i ys@ (y:_) (xs:xss) = (ys !! i) :
                             f (i + 1) (zipWith (+) (ys ++ repeat 0) xs) xss

instance OEIS 258062 where
  oeis = map length $ group (oeis @188967)

instance OEIS 258057 where
  oeis = zipWith (-) (tail (oeis @3415)) (oeis @3415)

instance OEIS 258051 where
  oeis = f (tail (oeis @258033)) where
     f xs = (0 : (delete (maximum ys) ys)) ++ f zs
            where (ys, (_ : zs)) = span (> 0) xs

instance OEIS 258033 where
  oeis = f (tail (oeis @22328)) where
     f xs = (0 : (delete (maximum ys) ys)) ++ f zs
            where (ys, (_ : zs)) = span (> 0) xs

instance OEIS 258032 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . flip div 10. (^ 3)) (oeis @40)

instance OEIS 257971 where
  oeis = zipWith (-) (tail (oeis @6921)) (oeis @6921)

instance OEIS 257956 where
  oeisIx = sum . (rowT @232642) . succ

instance OEIS 257831 where
  oeisIx = foldr (\b v -> 10 * v + b) 0 .
             concat . mapMaybe (flip lookup bin) . (rowT @31298)
              where bin = zip [0..9] (tabf @30308)

instance OEIS 257644 where
  oeis = scanl (+) 1 (oeis @7503)

instance OEIS 257585 where
  oeisIx = flip mod 2 . (oeisIx @254077)

instance OEIS 257279 where
  oeis = filter ((== 1) . (oeisIx @168046)) (oeis @257278)

instance OEIS 257173 where
  oeisIx = fromJust . (`elemIndex` (oeis @248737))

instance OEIS 256841 where
  oeis = [x | x <- map (+ 28561) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256840 where
  oeis = [x | x <- map (+ 20736) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256839 where
  oeis = [x | x <- map (+ 14641) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256838 where
  oeis = [x | x <- map (+ 10000) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256837 where
  oeis = [x | x <- map (+ 6561) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256836 where
  oeis = [x | x <- map (+ 4096) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256835 where
  oeis = [x | x <- map (+ 2401) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256834 where
  oeis = [x | x <- map (+ 1296) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256777 where
  oeis = [x | x <- map (+ 625) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256776 where
  oeis = [x | x <- map (+ 256) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256775 where
  oeis = [x | x <- map (+ 81) (oeis @290), (oeisIx @10051 . pred) x == 1]

instance OEIS 256673 where
  oeis = filter odd (oeis @157037)

instance OEIS 256563 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @166133)) . (oeisIx @1358)

instance OEIS 256561 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @166133)) . (oeisIx @40)

instance OEIS 256541 where
  oeis = zipWith (-) (tail (oeis @166133)) (oeis @166133)

instance OEIS 256415 where
  oeisIx (succ->n) | (oeisIx @10051 . pred) n == 1 = 2 * n
            | r == 0 && (oeisIx @10051 . pred) n' == 1 = 2 * n'
            | otherwise = n
            where (n', r) = divMod n 3

instance OEIS 256414 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121217)) . (oeisIx @40)

instance OEIS 256283 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257905))

instance OEIS 256213 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @254077).pred) [1..]

instance OEIS 256187 where
  oeis = zipWith (-) (tail (oeis @4718)) (oeis @4718)

instance OEIS 256012 where
  oeisIx = p (oeis @13929) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 255878 where
  oeis = zipWith (-) (tail (oeis @256188)) (oeis @256188)

instance OEIS 255481 where
  oeis = zipWith gcd (oeis @255582) $ tail (oeis @255582)

instance OEIS 255480 where
  oeis = zipWith gcd (oeis @255582) $ drop 2 (oeis @255582)

instance OEIS 254398 where
  oeisIx = flip mod 10 . (oeisIx @237424)

instance OEIS 254397 where
  oeisIx = (oeisIx @30) . (oeisIx @237424)

instance OEIS 254339 where
  oeisIx = flip mod 10 . (oeisIx @254143)

instance OEIS 254338 where
  oeisIx = (oeisIx @30) . (oeisIx @254143)

instance OEIS 254323 where
  oeisIx = (oeisIx @137564) . (oeisIx @254143)

instance OEIS 253607 where
  oeis = zipWith (-) (tail (oeis @253580)) (oeis @253580)

instance OEIS 253297 where
  oeis = f (oeis @98550) where
     f (u:vs@ (_:v:_)) = if (oeisIx @10051 . pred) v == 1 && div u v > 2
                           then v : f vs else f vs

instance OEIS 253074 where
  oeis = 0 : f 0 [1..] where
     f u vs = g vs where
       g (w:ws) | (oeisIx @10051 . pred) (u + w) == 1 = g ws
                | otherwise = w : f w (delete w vs)

instance OEIS 253073 where
  oeis = 0 : f 0 (oeis @18252) where
     f u vs = g vs where
       g (w:ws) | (oeisIx @10051 . pred) (u + w) == 1 = g ws
                | otherwise = w : f w (delete w vs)

instance OEIS 252895 where
  oeis = filter (odd . (oeisIx @46951).pred) [1..]

instance OEIS 252865 where
  oeis = 1 : 2 : 3 : f 2 3 (drop 3 (oeis @5117)) where
     f u v ws = g ws where
       g (x:xs) = if gcd x u > 1 && gcd x v == 1
                     then x : f v x (delete x ws) else g xs

instance OEIS 252849 where
  oeis = filter (even . (oeisIx @46951).pred) [1..]

instance OEIS 252837 where
  oeis = f (oeis @98550) where
     f us = (h 0 vs) : f vs where
       (_:vs) = dropWhile ((== 0) . (oeisIx @10051 . pred)) us
       h e (w:_:ws) = if even w then h (e + 1) ws else e

instance OEIS 252022 where
  oeis = 1 : f [1] (drop 2 (tabf @31298)) where
     f xs zss = g zss where
       g (ds:dss) = if all (<= 9) $ zipWith (+) xs ds
         then (foldr (\d v -> 10 * v + d) 0 ds) : f ds (delete ds zss)
         else g dss

instance OEIS 252001 where
  oeis = 1 : f [1] (drop 2 (tabf @31298)) where
     f xs zss = g zss where
       g (ds:dss) = if any (> 9) $ zipWith (+) xs ds
         then (foldr (\d v -> 10 * v + d) 0 ds) : f ds (delete ds zss)
         else g dss

instance OEIS 251756 where
  oeis = 0 : f 0 (oeis @2808) where
     f x zs = g zs where
       g (y:ys) | d == 1 || (oeisIx @10051 . pred) d == 1 = g ys
                | otherwise = y : f y (delete y zs)
                where d = gcd x y

instance OEIS 251725 where
  oeisIx 0 = 1
  oeisIx (succ->n) = if length ps == 1 then 1 else head $ filter f [2..]  where
    f b = all (== len) lbs where len:lbs = map (length . d b) ps
    ps = (rowT @27748) n
    d b = unfoldr (\z -> if z == 0 then Nothing else Just $ swap $ divMod z b)

instance OEIS 251621 where
  oeis = map length $ group (oeis @249943)

instance OEIS 251620 where
  oeis = map head $ group (oeis @249943)

instance OEIS 251618 where
  oeisIx n = fromJust $
              find (\x -> mod x (fi $ (oeisIx @40) n) == 0) (oeis @98550)

instance OEIS 251608 where
  oeis = 2 : 3 : f 2 3 (drop 5 (oeis @45)) where
     f u v (w:ws) = if gcd u w > 1 && gcd v w == 1
                       then w : f v w ws else f u v ws

instance OEIS 251595 where
  oeis = map head $ group (oeis @251416)

instance OEIS 251557 where
  oeis = map (+ 2) $ tail $ scanl maxEven 0 (oeis @98550)
                 where maxEven u v = if even v then max u v else u

instance OEIS 251553 where
  oeis = filter ((== 0) . flip mod 3 . (oeisIx @98550).pred) [1..]

instance OEIS 251549 where
  oeisIx (succ->n) = head $ [1, 3 ..] \\ filter odd (take n (oeis @98550))

instance OEIS 251546 where
  oeisIx (succ->n) = head $ [2, 4 ..] \\ filter even (take n (oeis @98550))

instance OEIS 251542 where
  oeis = [div u v | (u, v) <- zip (drop 2 (oeis @98550)) (oeis @98550),
                            (oeisIx @10051 . pred) v == 1]

instance OEIS 251538 where
  oeis = filter (\x -> (oeisIx @98548) (2*x+3) > (oeisIx @98548) (2*x+1) + 6) [1..]

instance OEIS 251537 where
  oeis = filter (\x -> (oeisIx @98548) (x + 2) > (oeisIx @98548) x + 6) [1, 3 ..]

instance OEIS 251239 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @98550).pred) [1..]

instance OEIS 251102 where
  oeis = zipWith gcd (drop 2 (oeis @98550)) (oeis @98550)

instance OEIS 249951 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @113630)) [1..]

instance OEIS 249900 where
  oeis = [1..4] ++ concatMap (uncurry (++))
            (f [2] [3,4] (drop 2 (oeis @40)) (tail (oeis @2808))) where
     f us@ (u:_) vs ps'@ (p:p':ps) cs'@ (c:c':cs)
       | (oeisIx @10051 . pred) u == 1 = g ([c] ++ us ++ [c']) vs ps' cs
       | otherwise      = g ([p] ++ us ++ [p']) vs ps cs'
     g us vs@ (v:_) (p:ps) (c:cs) = (us, ws) : f us ws ps cs where
       ws = if (oeisIx @10051 . pred) v == 1 then [c] ++ vs ++ [p] else [p] ++ vs ++ [c]

instance OEIS 249858 where
  oeisIx n = (oeisIx @249857) n - (oeisIx @249856) n

instance OEIS 249694 where
  oeis = zipWith gcd (drop 3 (oeis @84937)) (oeis @84937)

instance OEIS 249648 where
  oeisIx = fromJust . (`elemIndex` (oeis @249626)) . (oeisIx @11540)

instance OEIS 249626 where
  oeis = f (zip [0,0..] [0..9]) (tabf @31298) where
     f acds@ ((_,dig):_) zss = g zss where
       g (ys:yss) = if dig `elem` ys
                       then y : f acds' (delete ys zss) else g yss
         where y = foldr (\d v -> 10 * v + d) 0 ys
               acds' = sortBy (compare `on` fst) $
                      addd (sortBy (compare `on` snd) acds)
                           (sortBy (compare `on` snd) $
                                   zip (map length gss) (map head gss))
               addd cds [] = cds
               addd []   _ = []
               addd ((c, d) : cds) yys'@ ((cy, dy) : yys)
                    | d == dy  = (c + cy, d) : addd cds yys
                    | otherwise = (c, d) : addd cds yys'
               gss = sortBy compare $ group ys

instance OEIS 249603 where
  oeisIx = flip mod 3 . (oeisIx @84937)

instance OEIS 249411 where
  oeis = filter ((== 1) . (oeisIx @5369)) (oeis @249407)

instance OEIS 249408 where
  oeis = filter ((== 0) . (oeisIx @5369)) (oeis @249406)

instance OEIS 249407 where
  oeis = f [2..] where
     f ws@ (u:v:_) = u : v : f (ws \\ [u, v, u * v])

instance OEIS 249406 where
  oeis = 1 : f [2..] where
     f ws@ (u:v:_) = y : f (ws \\ [u, v, y]) where y = u * v

instance OEIS 249304 where
  oeisIx n = if n == 0 then 0 else (oeisIx @48967) n + (oeisIx @48967) (n - 1)

instance OEIS 249184 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 . (rowT @249133)

instance OEIS 249183 where
  oeisIx = foldr (\b v -> 10 * v + b) 0 . (rowT @249133)

instance OEIS 249167 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v ws = g ws where
       g (x:xs) | null (intersect fdx $ (rowT @213925) u) ||
                  not (null $ intersect fdx $ (rowT @213925) v) = g xs
                | otherwise =  x : f v x (delete x ws)
                where fdx = (rowT @213925) x

instance OEIS 249041 where
  oeis = tail $ scanl (\i j -> i + mod j 2) 0 (oeis @249039)

instance OEIS 249040 where
  oeis = tail $ scanl (\i j -> i + 1 - mod j 2) 0 (oeis @249039)

instance OEIS 249034 where
  oeis = filter odd (oeis @171946)

instance OEIS 249032 where
  oeis = zipWith (-) (tail (oeis @75326)) (oeis @75326)

instance OEIS 248918 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247665)) . (oeisIx @961) . (+ 1)

instance OEIS 248574 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @27306) (n - 1) + (oeisIx @27306) n

instance OEIS 248519 where
  oeisIx = p $ tail (oeis @52383) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 248518 where
  oeisIx = p $ tail (oeis @52383) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 248387 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247665)) . (oeisIx @40)

instance OEIS 248353 where
  oeis = filter k [1..] where
     k x = elem x $ map (uncurry (+)) $
           takeWhile ((> 0) . fst) $ map (divMod (x ^ 2)) (oeis @11557)

instance OEIS 248045 where
  oeisIx (succ->n) = (oeisIx @891) (n - 1) * (oeisIx @142) n

instance OEIS 248025 where
  oeis = 1 : f 1 [2..] where
    f x zs = g zs where
      g (y:ys) = if (oeisIx @30) y == (oeisIx @10888) x
                 then y : f y (delete y zs) else g ys

instance OEIS 248014 where
  oeis = filter (\x -> (oeisIx @247796) x < x) [0..]

instance OEIS 248013 where
  oeis = filter (\x -> (oeisIx @247796) x == x) [0..]

instance OEIS 248012 where
  oeisIx = foldr1 (^) . (rowT @27748) . succ . succ

instance OEIS 247894 where
  oeisIx = (oeisIx @196) . (oeisIx @10807)

instance OEIS 247824 where
  oeis = f ips where
     f ((x, p) : xps) = head
       [y | (y, q) <- ips, (p + q) `mod` (x + y) == 0] : f xps
     ips = zip [1..] (oeis @40)

instance OEIS 247815 where
  oeisIx = sum . map (oeisIx @10051 . pred) . (rowT @77581) . succ

instance OEIS 247799 where
  oeis = 0 : f 1 [0] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
                    y = z + maybe x id (elemIndex x $ reverse zs)

instance OEIS 247797 where
  oeis = f 1 $ zip (oeis @40) (oeis @7605) where
     f q' vws = g vws where
       g  ((p,q):pqs) = if gcd q q' == 1
                           then p : f q (delete (p,q) vws) else g pqs

instance OEIS 247657 where
  oeis = f 0 $ drop 2 (oeis @40) where
     f z (p:ps) | (oeisIx @10051 . pred) z' == 1 = z' : f z' (delete z' ps)
                | otherwise        = f z' ps
                where z' = z + p

instance OEIS 247647 where
  oeisIx = (oeisIx @7088) . (oeisIx @247648)

instance OEIS 247499 where
  oeisIx = sum . (rowT @247500)

instance OEIS 247419 where
  oeis = concat $
                 transpose [map (subtract 1) (oeis @3256), (oeis @3256)]

instance OEIS 247414 where
  oeis = zipWith (-) (tail (oeis @24431)) (oeis @24431)

instance OEIS 247367 where
  oeisIx n = sum $ map ((1 -) . (oeisIx @10052) . (n -)) $
                    takeWhile (<= n) (oeis @290)

instance OEIS 247199 where
  oeis = filter f [1..] where
     f x = 1 == denominator
           (sum [v % w | (v:ws) <- tails $ reverse $ (rowT @27750) x, w <- ws])

instance OEIS 247167 where
  oeis = filter ((zipWith (==) [0..] (oeis @247143)) !!) [0..]

instance OEIS 247144 where
  oeisIx = fromJust . (`elemIndex` (oeis @247143))

instance OEIS 246878 where
  oeis = 1 : f [1] (oeis @523) where
     f xs (k:ks) = y : f (xs ++ [y]) ks where y = sum $ genericDrop k xs

instance OEIS 246701 where
  oeis = zipWith (-) (tail (oeis @246520)) (oeis @246520)

instance OEIS 246558 where
  oeisIx = (oeisIx @7954) . (oeisIx @45)

instance OEIS 246520 where
  oeisIx = maximum . (rowT @246830)

instance OEIS 246436 where
  oeisIx (succ->n) = genericLength $ [1..n] \\ genericIndex (tabf @220237) (n - 1)

instance OEIS 246430 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @212300)) . (oeisIx @40)

instance OEIS 245836 where
  oeisIx = sum . (rowT @53398) . succ

instance OEIS 245718 where
  oeisIx n = (oeisIx @245677) n `div` (oeisIx @245678) n

instance OEIS 245508 where
  oeis = f (oeis @40) (oeis @1105) where
     f ps'@ (p:ps) xs'@ (x:xs) = if p <= x then x : f ps xs' else f ps' xs

instance OEIS 245305 where
  oeis = map ((`div` 4) . (subtract 1) . head) $
     filter (all (== 1) . map (oeisIx @10051 . pred)) $
            iterate (zipWith (+) [4, 4, 6]) [1, 3, 5]

instance OEIS 245057 where
  oeisIx = fromJust . (`elemIndex` (oeis @249129))

instance OEIS 244724 where
  oeis = 1 : f 1 [2..] where
     f x xs = f' xs where
       f' (u:us) | (oeisIx @10051 . pred) (x + u) == 1 = g u (delete u xs)
                 | otherwise             = f' us where
          g y ys = g' ys where
            g' (v:vs) | (oeisIx @10051 . pred) (y + v) == 0 = u : v : f v (delete v ys)
                      | otherwise        = g' vs

instance OEIS 244479 where
  oeisIx = (`div` 2) . (oeisIx @244478)

instance OEIS 243451 where
  oeis = [x | x <- (oeis @241751), (oeisIx @10051 . pred) x == 1]

instance OEIS 242535 where
  oeis = f [1..] where
     f xs'@ (x:xs) = x : f (xs \\ [z, 2 * z]) where z = xs' !! x

instance OEIS 242408 where
  oeis = filter ((> 0) . (oeisIx @242400)) [0..]

instance OEIS 242401 where
  oeis = filter ((== 0) . (oeisIx @10054)) (oeis @37)

instance OEIS 242314 where
  oeisIx = maximum . (rowT @242312)

instance OEIS 242311 where
  oeisIx = maximum . (rowT @96145)

instance OEIS 242094 where
  oeis = c [1..] (oeis @3249) where
     c (v:vs) ws'@ (w:ws) = if v == w then c vs ws else v : c vs ws'

instance OEIS 241944 where
  oeisIx = sum . (rowT @27420)

instance OEIS 241772 where
  oeis = zipWith (-) (tail (oeis @65094)) (oeis @65094)

instance OEIS 241766 where
  oeisIx = p $ tail (oeis @1047) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 241673 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @229037)) [1..]

instance OEIS 241418 where
  oeis = zipWith (-) (tail (oeis @99054)) (oeis @99054)

instance OEIS 240993 where
  oeisIx n = (oeisIx @142) (n + 1) * (oeisIx @2109) n

instance OEIS 240844 where
  oeisIx = p $ drop 3 (oeis @73) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 240508 where
  oeisIx = genericLength . (rowT @174382) .succ

instance OEIS 240400 where
  oeis = filter ((> 0) . (oeisIx @241759)) [0..]

instance OEIS 240277 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @7456))

instance OEIS 240052 where
  oeisIx = (oeisIx @68346) . (oeisIx @6094)

instance OEIS 239968 where
  oeis = unfoldr c (1, 1, (oeis @18252)) where
     c (i, z, xs'@ (x:xs)) | i == x = Just (z, (i + 1, z + 1, xs))
                          | i /= x = Just (0, (i + 1, z, xs'))

instance OEIS 239930 where
  oeisIx = sum . map (oeisIx @240025) . (rowT @27750) . succ

instance OEIS 239826 where
  oeisIx (succ->n) = sum $
              filter ((flip isPrefixOf `on` (rowT @30308)) n) $ (rowT @27750) n

instance OEIS 239740 where
  oeisIx (succ->n) = gcd (sum fs) (product fs)
              where fs = take n $ tail (oeis @45)

instance OEIS 239656 where
  oeis = zipWith (-) (tail (oeis @7304)) (oeis @7304)

instance OEIS 239639 where
  oeis = map length $ group (oeis @239634)

instance OEIS 239636 where
  oeisIx = subtract 1 . (* 2) . (oeisIx @14689)

instance OEIS 239634 where
  oeisIx = (oeisIx @30) . (oeisIx @1358)

instance OEIS 239509 where
  oeisIx = p (oeis @469) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 239508 where
  oeisIx = p (oeis @469) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 239070 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @132995)) . (oeisIx @5117)

instance OEIS 238778 where
  oeisIx (succ.succ->n) = sum $ filter ((== 1) . (oeisIx @10051 . pred)) $
     map (2 * n -) $ takeWhile (<= 2 * n) (oeis @40)

instance OEIS 238704 where
  oeis = 1 : f 1 [2..] where
     f x zs = g zs where
       g (y:ys) =
         if y `mod` 2 /= m then g ys else y : f y (delete y zs)
       m = (oeisIx @30) x `mod` 2

instance OEIS 238327 where
  oeis = iterate ((+ 2) . (oeisIx @151800)) 1

instance OEIS 238248 where
  oeis = map succ $ elemIndices 7 $ oeis @72219

instance OEIS 238247 where
  oeis = map succ $ elemIndices 5 $ oeis @72219

instance OEIS 238246 where
  oeis = map succ $ elemIndices 3 $ oeis @72219

instance OEIS 237347 where
  oeis = zipWith (-) (tail (oeis @78633)) (oeis @78633)

instance OEIS 236473 where
  oeisIx = p (oeis @7422) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 235992 where
  oeis = filter (even . (oeisIx @3415)) [0..]

instance OEIS 235991 where
  oeis = filter (odd . (oeisIx @3415)) [0..]

instance OEIS 235224 where
  oeisIx n = genericLength $ takeWhile (<= n) (oeis @2110)

instance OEIS 234814 where
  oeis = filter (\x -> x `mod` (oeisIx @7953) x == 0 &&
                               x `mod` (oeisIx @10888) x /= 0) [1..]

instance OEIS 233836 where
  oeis = map length $ group (oeis @4539)

instance OEIS 231335 where
  oeisIx = genericLength . filter ((== 1) . (oeisIx @10056)) . (rowT @231330)

instance OEIS 231331 where
  oeisIx = genericLength . (rowT @231330)

instance OEIS 230873 where
  oeis = f [0..] (oeis @230872) where
     f (u:us) vs'@ (v:vs) = if u == v then f us vs else u : f us vs'

instance OEIS 230872 where
  oeis = f [] (tabf @231330) where
     f ws (xs:xss) = us ++ f (merge vs xs) xss where
       (us,vs) = span (< head xs) ws
     merge us [] = us
     merge [] vs = vs
     merge us'@ (u:us) vs'@ (v:vs)
          | u < v = u : merge us vs'
          | u > v = v : merge us' vs
          | otherwise = u : merge us vs

instance OEIS 230709 where
  oeis = filter (\x -> (oeisIx @10060) x * x `mod` 2 == 0) [0..]

instance OEIS 230641 where
  oeisIx n = (oeisIx @53735) n + n

instance OEIS 230585 where
  oeisIx 0 = 3
  oeisIx (succ->n) = (rowCol @7318) (2*n) n - (rowCol @7318) (2*n) (n+2)

instance OEIS 230584 where
  oeis = 2 : 3 : concat
                 (transpose [drop 2 (oeis @59100), drop 2 (oeis @8865)])

instance OEIS 230504 where
  oeisIx (succ->n) = head $ filter ((== 1) . (oeisIx @10051 . pred)) rs where
                     rs = n : zipWith (+) rs (zipWith gcd rs [2..])

instance OEIS 230287 where
  oeis = zipWith (-) (tail (oeis @230286)) (oeis @230286)

instance OEIS 230286 where
  oeisIx = (flip div 3) . (oeisIx @16052)

instance OEIS 230116 where
  oeisIx = foldr (\u v-> 2*v + u) 0 . (rowT @166360) . succ

instance OEIS 230097 where
  oeis = 0 : f 0 0 where
     f i m = if v > m then i : f (i + 1) v else f (i + 1) m
             where v = (oeisIx @159918) i

instance OEIS 228078 where
  oeisIx = subtract 1 . (oeisIx @99036)

instance OEIS 228057 where
  oeis = filter odd (oeis @228056)

instance OEIS 227928 where
  oeis = 1 : f 0 0 (tail (oeis @79)) (tail (oeis @244)) where
     f x y us'@ (u:us) vs'@ (v:vs)
       | x > 0     = u : f 0 (u - x + y) us vs'
       | y > v - u = v : f (v + x - y) 0 us' vs
       | otherwise = u : f 0 (u + y) us vs'

instance OEIS 227878 where
  oeis = f (oeis @51701) where
     f (p:ps@ (_:p':_)) = if p == p' then p : f ps else f ps

instance OEIS 227876 where
  oeisIx n = fst $ until (null . snd) h (0, (rowT @31298) n) where
              h (s, ds) = (s + sum ds, map abs $ zipWith (-) ds $ tail ds)

instance OEIS 227481 where
  oeisIx = sum . map (oeisIx @10052) . (rowT @69011)

instance OEIS 227455 where
  oeis = 1 : f [2..] [1] where
     f (v:vs) ws = if any (`notElem` ws) $ map (subtract 1) $ (rowT @27748) v
                      then v : f vs (v : ws) else f vs ws

instance OEIS 227389 where
  oeis = map length $ group (oeis @226390)

instance OEIS 227378 where
  oeisIx = fromJust . (`elemIndex` (oeis @217928))

instance OEIS 227325 where
  oeisIx n = (oeisIx @272) (n + 1) * (oeisIx @984) n

instance OEIS 227288 where
  oeis = zipWith gcd (tail (oeis @227113)) (oeis @227113)

instance OEIS 227190 where
  oeisIx (succ->n) = n - (oeisIx @167489) n

instance OEIS 227068 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @56924)) . succ

instance OEIS 226898 where
  oeisIx = maximum . map length .
     map (\ds@ (d:_) -> takeWhile (<= e' d) ds) . init . tails . (rowT @27750) . succ
     where e' = floor . (* e) . fi; e = exp 1

instance OEIS 226749 where
  oeisIx = p (oeis @53012) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 226748 where
  oeisIx = p (oeis @53012) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 226482 where
  oeisIx = genericLength . (rowT @226481)

instance OEIS 226047 where
  oeisIx = maximum . (rowT @226078) . succ

instance OEIS 226029 where
  oeis = zipWith (-) (tail (oeis @182402)) (oeis @182402)

instance OEIS 226025 where
  oeis = filter ((/= 2) . (oeisIx @100995).pred) (oeis @71904)

instance OEIS 225793 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @230093

instance OEIS 225481 where
  oeisIx n = product [p | p <- takeWhile (<= n + 1) (oeis @40),
                           mod n (p - 1) == 0 || mod (n + 1) p == 0]

instance OEIS 225228 where
  oeis = filter f [1..] where
     f x = length es == 3 && sum es `elem` [3,5,7] &&
                             maximum es - minimum es <= 1
           where es = (rowT @124010) x

instance OEIS 225105 where
  oeis = filter
     ((== 1) . (oeisIx @10051 . pred) . maximum . filter odd . (rowT @70165)) (oeis @5408)

instance OEIS 225078 where
  oeis = elemIndices 1 $
     zipWith ((*) `on` (oeisIx @10051 . pred)) (oeis @2522) (oeis @8865)

instance OEIS 225045 where
  oeisIx = p (oeis @14132) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 225044 where
  oeisIx = p (oeis @14132) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 224839 where
  oeis = f [1..] [] where
     f (x:xs) ys = if all ((== 0) . (oeisIx @10052)) $ map (x -) ys
                      then x : f xs (x:ys) else f xs ys

instance OEIS 224076 where
  oeisIx = genericLength . (rowT @224075) . succ

instance OEIS 223491 where
  oeisIx = last . (rowT @213925) . succ

instance OEIS 223490 where
  oeisIx = head . (rowT @213925) . succ

instance OEIS 222493 where
  oeisIx = (oeisIx @133500) . (oeisIx @221221)

instance OEIS 220812 where
  oeisIx = (oeisIx @11557) . (oeisIx @79)

instance OEIS 219922 where
  oeisIx (succ->n) = (fromJust $ findIndex (n `elem`) (tabl @26835)) + 1

instance OEIS 219762 where
  oeisIx = subtract 1 . (oeisIx @99054) . subtract 1 . succ

instance OEIS 219609 where
  oeis = map (`div` 2) $ zipWith (-) (tail (oeis @219608)) (oeis @219608)

instance OEIS 219608 where
  oeis = filter odd (oeis @60142)

instance OEIS 219607 where
  oeisIx = p (oeis @47221) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 219603 where
  oeisIx n = (oeisIx @40) n * (oeisIx @31368) n

instance OEIS 219462 where
  oeisIx = sum . zipWith (*) (oeis @1906) . (rowT @34870)

instance OEIS 218534 where
  oeisIx n = (oeisIx @176352) (n + 1) `div` (oeisIx @218535) n
  oeis = map denominator $ zipWith (%) (oeis @176352) $ tail (oeis @176352)

instance OEIS 218533 where
  oeisIx n = (oeisIx @176352) n `div` (oeisIx @218535) n
  oeis = map numerator $ zipWith (%) (oeis @176352) $ tail (oeis @176352)

instance OEIS 217921 where
  oeisIx (succ->n) = fst $ until (all (== 1) . snd) f (0, (rowT @30308) n) where
     f (i, xs)  = (i + 1, map genericLength $ group xs)

instance OEIS 217575 where
  oeisIx = subtract 1 . (oeisIx @63657)

instance OEIS 217398 where
  oeis = map succ $ elemIndices 5 $ tail $ oeis @30

instance OEIS 217218 where
  oeis = iterate (oeisIx @6368) 44

instance OEIS 217122 where
  oeis = f 1 [0..] where
     f x zs = y : f (x + 1) (delete y zs) where
       y = zs !! (oeisIx @120) x

instance OEIS 216602 where
  oeisIx n | n <= 5    = 0
            | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 6 - 1)

instance OEIS 216601 where
  oeisIx n | n <= 4    = 0
            | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 5 - 1)

instance OEIS 216600 where
  oeisIx (succ->n) | n <= 3    = 0
            | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 4 - 1)

instance OEIS 216599 where
  oeisIx (succ->n) | n <= 2    = 0
            | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 3 - 1)

instance OEIS 216407 where
  oeisIx = (45 -) . (oeisIx @217928)

instance OEIS 216237 where
  oeis = filter ((== 1) . (oeisIx @136522)) (oeis @7770)

instance OEIS 216176 where
  oeisIx (succ->n) = sum $ zipWith (*) zs $ reverse zs
     where zs = (rowT @189920) n

instance OEIS 214958 where
  oeis = [x | x <- [0..], (oeisIx @214949) x == 1]

instance OEIS 214957 where
  oeis = [x | x <- [0..], (oeisIx @214950) x == 1]

instance OEIS 214855 where
  oeisIx = (oeisIx @45) . (oeisIx @8597) . subtract 1 . succ

instance OEIS 214848 where
  oeis = zipWith (-) (tail (oeis @22846)) (oeis @22846)

instance OEIS 214777 where
  oeis = findIndices (> 0) (oeis @214772)

instance OEIS 214772 where
  oeisIx = p [6, 9, 20] where
     p _      0 = 1
     p []     _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 214583 where
  oeis = filter (p 3 1) [2..] where
     p i k2 x = x <= k2 || (gcd k2 x > 1 || (oeisIx @10051 . pred) (x - k2) == 1) &&
                           p (i + 2) (k2 + i) x

instance OEIS 214560 where
  oeisIx = (oeisIx @23416) . (oeisIx @290)

instance OEIS 214546 where
  oeis = zipWith (-) (tail (oeis @140472)) (oeis @140472)

instance OEIS 214489 where
  oeis = [x | x <- [0..], (oeisIx @70939) x == (oeisIx @103586) x]

instance OEIS 214433 where
  oeis = [x | x <- [0..], (oeisIx @105025) x == (oeisIx @105027) x]

instance OEIS 214360 where
  oeis = [x | k <- [0..], let x = 3120613860*k+23, (oeisIx @10051 . pred) x == 1]

instance OEIS 214295 where
  oeisIx (succ->n) = (oeisIx @10052) n - (oeisIx @10052) (3*n)

instance OEIS 213912 where
  oeis = 1 : f [1] where
     f xs@ (x:_) = y : f (y : xs) where
       y = if z `notElem` xs then z else 3 * x where z = (oeisIx @196) x

instance OEIS 213087 where
  oeis = f (oeis @30190) where
     f xs = foldl1 (\v d -> 10 * v + d) (ys ++ [0]) : f zs where
            (ys, _:zs) = span (/= 0) xs

instance OEIS 213025 where
  oeis = f (oeis @1358) where
     f (x:sps'@ (y:z:sps)) | 2 * y == (x + z) = y : f sps'
                          | otherwise        = f sps'

instance OEIS 212529 where
  oeisIx = (oeisIx @39724) . negate . succ

instance OEIS 212444 where
  oeis = iterate (oeisIx @212439) 0

instance OEIS 212441 where
  oeis = filter (odd . (oeisIx @181935)) [0..]

instance OEIS 212440 where
  oeis = filter (even . (oeisIx @181935)) [0..]

instance OEIS 212439 where
  oeisIx n = 2 * n + (oeisIx @212412) n

instance OEIS 212412 where
  oeisIx = (`mod` 2) . (oeisIx @181935)

instance OEIS 212300 where
  oeis = f 1 (2 : (oeis @40)) [1] $ tail (oeis @6530) where
     f x ps'@ (_ : ps@ (_ : p : _)) gpfs (q : qs) =
       y : f (x + 1) (if y == p then ps else ps') (q : gpfs) qs where
       y = head [z | z <- ps', length (filter (> z) gpfs) <= div x 2]

instance OEIS 212192 where
  oeis = filter ((== 1) . (oeisIx @10054)) (oeis @14311)

instance OEIS 212191 where
  oeis = map (oeisIx @196) (oeis @212190)

instance OEIS 212190 where
  oeis = filter ((== 1) . (oeisIx @10052)) (oeis @14311)

instance OEIS 212177 where
  oeis = filter (> 0) (oeis @56170)

instance OEIS 212168 where
  oeis = map (+ 1) $ findIndices (> 0) (oeis @225230)

instance OEIS 212167 where
  oeis = map (+ 1) $ findIndices (>= 0) (oeis @225230)

instance OEIS 212166 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @225230)

instance OEIS 212165 where
  oeis = map (+ 1) $ findIndices (<= 0) (oeis @225230)

instance OEIS 212164 where
  oeis = map (+ 1) $ findIndices (< 0) (oeis @225230)

instance OEIS 211996 where
  oeisIx (succ->n) = genericLength [x | x <- [1..n], let (y, m) = divMod n x,
                          m == 0, (oeisIx @10052) (x + y) == 1]

instance OEIS 211868 where
  oeisIx (succ->n) = f (oeis @5408) 1 nn 0 where
     f (o:os) l nl xx
       | yy > nl   = 0
       | yy < nl   = f os (l + 1) (nl + nn) yy + f os l nl xx
       | otherwise = if w == n then 1 else 0
       where w = if r == 0 then (oeisIx @196) m else 0
             (m, r) = divMod yy l
             yy = xx + o * o
     nn = n ^ 2

instance OEIS 211863 where
  oeisIx n = p 0 [] [1..8] n where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p _ _  []     _ = 0
     p m ms ks'@ (k:ks) x
       | x < k       = 0
       | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
       | m `elem` ms = p (m + 1) ms ks' (x - k)
       | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

instance OEIS 211862 where
  oeisIx n = p 0 [] [1..7] n where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p _ _  []     _ = 0
     p m ms ks'@ (k:ks) x
       | x < k       = 0
       | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
       | m `elem` ms = p (m + 1) ms ks' (x - k)
       | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

instance OEIS 211861 where
  oeisIx n = p 0 [] [1..6] n where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p _ _  []     _ = 0
     p m ms ks'@ (k:ks) x
       | x < k       = 0
       | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
       | m `elem` ms = p (m + 1) ms ks' (x - k)
       | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

instance OEIS 211860 where
  oeisIx n = p 0 [] [1..5] n where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p _ _  []     _ = 0
     p m ms ks'@ (k:ks) x
       | x < k       = 0
       | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
       | m `elem` ms = p (m + 1) ms ks' (x - k)
       | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

instance OEIS 211859 where
  oeisIx n = p 0 [] [1..4] n where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p _ _  []     _ = 0
     p m ms ks'@ (k:ks) x
       | x < k       = 0
       | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
       | m `elem` ms = p (m + 1) ms ks' (x - k)
       | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

instance OEIS 211858 where
  oeisIx n = p 0 [] [1..3] n where
     p m ms _      0 = if m `elem` ms then 0 else 1
     p _ _  []     _ = 0
     p m ms ks'@ (k:ks) x
       | x < k       = 0
       | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
       | m `elem` ms = p (m + 1) ms ks' (x - k)
       | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

instance OEIS 211316 where
  oeisIx (succ.succ->n) | not $ null ps = n * (head ps + 1) `div` (3 * head ps)
            | m == 0        = n'
            | otherwise     = (n - 1) `div` 3
            where ps = [p | p <- (rowT @27748) n, mod p 3 == 2]
                  (n',m) = divMod n 3

instance OEIS 211201 where
  oeisIx = fromJust . (`elemIndex` (oeis @50493))

instance OEIS 211111 where
  oeisIx (succ->n) = p (tail $ (rowT @27750) n) n where
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m | m < k     = 0
                 | otherwise = p ks (m - k) + p ks m

instance OEIS 211110 where
  oeisIx n = p (tail $ (rowT @27750) n) n where
     p _      0 = 1
     p []     _ = 0
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 211005 where
  oeis = map length $ group (oeis @69754)

instance OEIS 210719 where
  oeis = f (zip [1..] (oeis @10)) [] where
     f ((i,x):ixs) phis | x `elem` phis = f ixs phis
                        | otherwise     = i : f ixs (x : phis)

instance OEIS 210490 where
  oeis = filter chi [1..] where
     chi x = all (== 1) es || all even es where es = (rowT @124010) x

instance OEIS 210454 where
  oeisIx = (`div` 3) . (subtract 1) . (4 ^) . (oeisIx @40) . (+ 2)

instance OEIS 209211 where
  oeis = filter (\x -> (x - 1) `gcd` (oeisIx @10) x == 1) [1..]

instance OEIS 208976 where
  oeis = map (subtract 1) $ tail (oeis @50168)

instance OEIS 208768 where
  oeis = nub (oeis @70198)

instance OEIS 208450 where
  oeis = map denominator $
     zipWith (%) (tail (oeis @10786)) (oeis @10786)

instance OEIS 208449 where
  oeis = map numerator $
     zipWith (%) (tail (oeis @10786)) (oeis @10786)

instance OEIS 208448 where
  oeis = zipWith gcd (oeis @10786) $ tail (oeis @10786)

instance OEIS 208355 where
  oeisIx n = (rowCol @208101) n n
  oeis = map last (tabl @208101)

instance OEIS 208280 where
  oeisIx = genericLength . nub . (rowT @8975)

instance OEIS 208278 where
  oeisIx = sum . (rowT @8975)

instance OEIS 208260 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @208259)

instance OEIS 208259 where
  oeis = 1 : map ((+ 1) . (* 10)) (oeis @131835)

instance OEIS 208238 where
  oeisIx = genericIndex (oeis @208238)
  oeis = f nns $ filter ((== 1) . (oeisIx @10051 . pred) . fst) nns where
     f mms'@ ((m,ms):mms) pps'@ ((p,ps):pps) =
       if m == p then f mms' pps else q : f mms pps'
       where q = fst $ fromJust $ find ((ms `isInfixOf`) . snd) pps'
     nns = zip [0..] (tabf @30308)

instance OEIS 208178 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) [1,257..]

instance OEIS 208177 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) [1,129..]

instance OEIS 208134 where
  oeisIx = genericLength . filter (== 0) . (rowT @8975)

instance OEIS 208131 where
  oeis = scanl (*) 1 $ (oeis @52901)

instance OEIS 207954 where
  oeis = filter ((== 1) . (oeisIx @136522)) (oeis @33075)

instance OEIS 207675 where
  oeis = filter
     (\x -> (rowT @27750) x `intersect` (rowT @70165) x /= (rowT @27750) x) [1..]

instance OEIS 207674 where
  oeis = filter
     (\x -> (rowT @27750) x `intersect` (rowT @70165) x == (rowT @27750) x) [1..]

instance OEIS 207337 where
  oeis = f (oeis @2522) where
     f (x:xs) | m == 0 && (oeisIx @10051 . pred) y == 1 = y : f xs
              | otherwise                = f xs
              where (y,m) = divMod x 10

instance OEIS 206787 where
  oeisIx = sum . filter odd . (rowT @206778) . succ

instance OEIS 206553 where
  oeisIx (succ->n) = head [p | p <- drop 2 (oeis @40),
                        (oeisIx @10051 . pred) (2^n + p*2^ (div (n+1) 2) - 1) == 1]

instance OEIS 206245 where
  oeisIx = p (oeis @83278) where
     p _      0 = 1
     p rps'@ (rp:rps) n = if n < rp then 0 else p rps' (n - rp) + p rps n

instance OEIS 206244 where
  oeisIx = p $ tail (oeis @2275) where
     p _             0 = 1
     p rus'@ (ru:rus) n = if n < ru then 0 else p rus' (n - ru) + p rus n

instance OEIS 205959 where
  oeisIx (succ->n) = product $ map (div n) $ (rowT @27748) n

instance OEIS 205745 where
  oeisIx (succ->n) = sum $ map ((`mod` 2) . (n `div`))
     [p | p <- takeWhile (<= n) (oeis @40), n `mod` p == 0]

instance OEIS 205217 where
  oeisIx = p $ tail (oeis @5836) where
     p _ 0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 205216 where
  oeisIx = p $ tail (oeis @5836) where
     p _ 0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 204879 where
  oeis = map (+ 1) $ findIndices (> 0) (oeis @97796)

instance OEIS 204878 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @97796)

instance OEIS 204558 where
  oeisIx = sum . (rowT @45975) . succ

instance OEIS 204557 where
  oeisIx = last . (rowT @45975) . succ

instance OEIS 204556 where
  oeisIx = head . (rowT @45975) . succ

instance OEIS 204389 where
  oeisIx = p (oeis @2808) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 204138 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ drop 6 (oeis @1945)

instance OEIS 204095 where
  oeis = map (* 8) (oeis @7088)

instance OEIS 204094 where
  oeis = map (* 7) (oeis @7088)

instance OEIS 204093 where
  oeis = map (* 6) (oeis @7088)

instance OEIS 203069 where
  oeis = 1 : f 1 [2..] where
     f u vs = g vs where
       g (w:ws) | odd z && (oeisIx @10051 . pred) z == 0 = w : f w (delete w vs)
                | otherwise = g ws
                where z = u + w

instance OEIS 202822 where
  oeis = filter ((== 1) . flip mod 3) (oeis @3136)

instance OEIS 202337 where
  oeis = f (oeis @62723) where
     f (x:xs'@ (x':xs)) = if x == x' then f xs' else x : f xs'

instance OEIS 202262 where
  oeis = [4,6,8,9] ++ [x | u <- (oeis @202262), v <- [4,6,8,9],
                         let x = 10 * u + v, v /= 9 || (oeisIx @10051 . pred) x == 0]

instance OEIS 200742 where
  oeis = f (tabl @200741) where
     f (rs:rss'@ (rs':rss)) =
       (length $ takeWhile (== EQ) $ zipWith compare rs rs') : f rss'

instance OEIS 200738 where
  oeis = f (tabl @200737) where
     f (rs:rss'@ (rs':rss)) =
       (length $ takeWhile (== EQ) $ zipWith compare rs rs') : f rss'

instance OEIS 200379 where
  oeisIx (succ->n) = (tabl @56230) !! n !! (n - 1)

instance OEIS 199968 where
  oeisIx = head . (rowT @173540) . succ

instance OEIS 199771 where
  oeisIx  = sum . (rowT @199332) . succ

instance OEIS 199696 where
  oeisIx n = product . (take (n `div` 2 + 1)) $ (rowT @199333) n

instance OEIS 199582 where
  oeisIx n = (rowT @199333) n !! (n `div` 2)

instance OEIS 199425 where
  oeis = f [] (tabl @199333) where
     f ps (ts:tts) =  (length ps') : f ps' tts where
       ps' = ps `union` (take ((length ts - 1) `div` 2) $ tail ts)

instance OEIS 199424 where
  oeisIx n = fromJust $ findIndex (elem $ (oeisIx @40) n) (tabl @199333)

instance OEIS 199262 where
  oeisIx n = (fromJust $ elemIndex n (oeis @199238)) + 1

instance OEIS 199238 where
  oeis = zipWith mod [1..] $ tail (oeis @120)

instance OEIS 199123 where
  oeisIx = p (oeis @1857) where
     p _  0 = 1
     p (u:us) m | m < u = 0
                | otherwise = p us (m - u) + p us m

instance OEIS 199122 where
  oeisIx = p (oeis @1857) where
     p _ 0 = 1
     p us'@ (u:us) m | m < u     = 0
                    | otherwise = p us' (m - u) + p us m

instance OEIS 199120 where
  oeisIx = p (oeis @3666) where
     p _ 0 = 1
     p us'@ (u:us) m | m < u     = 0
                    | otherwise = p us' (m - u) + p us m

instance OEIS 199119 where
  oeisIx = p (oeis @2859) where
     p _  0 = 1
     p (u:us) m | m < u = 0
                | otherwise = p us (m - u) + p us m

instance OEIS 199118 where
  oeisIx = p (oeis @2859) where
     p _ 0 = 1
     p us'@ (u:us) m | m < u     = 0
                    | otherwise = p us' (m - u) + p us m

instance OEIS 199017 where
  oeisIx = p (oeis @2858) where
     p _  0 = 1
     p (u:us) m | m < u = 0
                | otherwise = p us (m - u) + p us m

instance OEIS 198385 where
  oeis = map (^ 2) (oeis @198389)

instance OEIS 197911 where
  oeis = scanl (+) 0 (oeis @56832)

instance OEIS 197877 where
  oeis = map (fromJust . (`elemIndex` (oeis @96535))) [0..]

instance OEIS 197183 where
  oeisIx = (oeisIx @115944) . (oeisIx @290)

instance OEIS 197182 where
  oeisIx = (oeisIx @64986) . (oeisIx @290)

instance OEIS 196871 where
  oeis = filter
     (all (== 0) . map (oeisIx @10051 . pred) . takeWhile (> 2) . iterate (oeisIx @6370)) [1..]

instance OEIS 196175 where
  oeis = map (+ 2) $ elemIndices True $
     zipWith (\x y -> x < 0 && y > 0) (oeis @36263) $ tail (oeis @36263)

instance OEIS 195610 where
  oeis = catMaybes $ map k [1..] where
     k x = elemIndex 0 $ map (`mod` x) $ take (fromInteger x) (oeis @51)

instance OEIS 195324 where
  oeis = filter p [2,4..] where
     p n = all ((== 0) . (oeisIx @10051 . pred)) $ takeWhile (> 1) $ map (n -) (oeis @5385)

instance OEIS 195145 where
  oeis = scanl (+) 0 (oeis @113801)

instance OEIS 195143 where
  oeis = scanl (+) 0 (oeis @91998)

instance OEIS 195142 where
  oeis = scanl (+) 0 (oeis @90771)

instance OEIS 195118 where
  oeis = filter f [3,5..] where
     f x = last pfs - head pfs == 6 where pfs = (rowT @27748) x

instance OEIS 195085 where
  oeis = map (+ 1) $ elemIndices 2 (oeis @57918)

instance OEIS 195066 where
  oeis = filter (\x -> (oeisIx @36044) x >= x) [0,2..]

instance OEIS 195065 where
  oeis = filter (\x -> (oeisIx @36044) x > x) [0,2..]

instance OEIS 195064 where
  oeis = filter (\x -> (oeisIx @36044) x <= x) [0,2..]

instance OEIS 195063 where
  oeis = filter (\x -> (oeisIx @36044) x < x) [0,2..]

instance OEIS 195045 where
  oeis = scanl (+) 0 (oeis @175886)

instance OEIS 195043 where
  oeis = scanl (+) 0 (oeis @175885)

instance OEIS 195042 where
  oeis = scanl (+) 0 (oeis @56020)

instance OEIS 195041 where
  oeis = scanl (+) 0 (oeis @47336)

instance OEIS 194597 where
  oeisIx (succ->n) = [1,6,6,1,9,3,1,3,9] !! (oeisIx @10878) (n - 1)

instance OEIS 194233 where
  oeisIx (succ->n) =
     fromMaybe (10*n) $ find (== (oeisIx @4186) n) $ map (oeisIx @4186) [n+1..10*n]

instance OEIS 194189 where
  oeisIx (succ->n) = sum $ map (oeisIx @10051 . pred) [n* (n+1) `div` 2 + 1 .. n^2 - 1]

instance OEIS 194081 where
  oeis = map (fromJust . (`elemIndex` (oeis @5375))) [0..]

instance OEIS 193928 where
  oeis = findIndices (0 <=) (oeis @193926)

instance OEIS 193749 where
  oeisIx = p (oeis @5214) where
     p _      0    = 1
     p (k:ks) m
       | m < k     = 0
       | otherwise = p ks (m - k) + p ks m

instance OEIS 193748 where
  oeisIx = p (oeis @5214) where
     p _          0 = 1
     p ks'@ (k:ks) m
       | m < k      = 0
       | otherwise  = p ks' (m - k) + p ks m

instance OEIS 193715 where
  oeis =
     map ((+ 1) . fromJust . (`elemIndex` (oeis @5214))) $ tail (oeis @217)

instance OEIS 193714 where
  oeis =
     map ((+ 1) . fromJust . (`elemIndex` (oeis @5214))) $ tail (oeis @290)

instance OEIS 193422 where
  oeis = map (fromJust . (`elemIndex` (oeis @193358))) [1..]


instance OEIS 193213 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @99267)

instance OEIS 192977 where
  oeis = f 0 $ group (oeis @68395) where
     f n xss'@ (xs:xss)
       | head xs `div` 9 == n = length xs : f (n+1) xss
       | otherwise            = 0 : f (n+1) xss'

instance OEIS 192504 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @3309)

instance OEIS 192503 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @3309)

instance OEIS 192134 where
  oeisIx n = (oeisIx @961) n - (oeisIx @192015) n



instance OEIS 192109 where
  oeis = map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @918) [1..]

instance OEIS 192066 where
  oeisIx = sum . filter odd . (rowT @77610) . succ

instance OEIS 192016 where
  oeisIx = (oeisIx @68346) . (oeisIx @961)

instance OEIS 192015 where
  oeisIx = (oeisIx @3415) . (oeisIx @961)

instance OEIS 191610 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ takeWhile (> 0) $ map ((n - 1) `div`) (oeis @351)

instance OEIS 190944 where
  oeisIx = (oeisIx @7088) . (* 3)

instance OEIS 190641 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @56170)

instance OEIS 190321 where
  oeisIx n = g n $ reverse $ takeWhile (<= n) $ tail (oeis @290) where
    g _ []                 = 0
    g m (x:xs) | x > m     = g m xs
               | otherwise = signum m' + g r xs where (m',r) = divMod m x

instance OEIS 190136 where
  oeisIx n = maximum $ map (oeisIx @6530) [n..n+3]

instance OEIS 190018 where
  oeis = 0 : drop 2 (merge (merge fibs $
      map (^ 2) fibs) $ zipWith (*) fibs (drop 2 fibs))
      where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
            merge xs'@ (x:xs) ys'@ (y:ys)
               | x < y     = x : merge xs ys'
               | x == y    = x : merge xs ys
               | otherwise = y : merge xs' ys

instance OEIS 189835 where
  oeisIx n = (oeisIx @1157) n - (oeisIx @38040) n

instance OEIS 189710 where
  oeis = elemIndices 0 $
     zipWith (-) (map (oeisIx @3415) (oeis @3415)) (map pred (oeis @3415))

instance OEIS 188917 where
  oeis = filter ((== 1) . (oeisIx @209229). (oeisIx @188915)) [0..]

instance OEIS 188916 where
  oeis = filter ((== 1) . (oeisIx @10052). (oeisIx @188915)) [0..]

instance OEIS 188654 where
  oeis = map (+ 1) $ findIndices (/= 0) (oeis @225230)

instance OEIS 188264 where
  oeis =
     map (+ 1) $ elemIndices 0 $ zipWith mod [1..] $ map (oeisIx @66459) [1..]

instance OEIS 187744 where
  oeis = filter ((== 1) . (oeisIx @10054) . (oeisIx @7953)) [0..]

instance OEIS 187098 where
  oeis = 1 : 2 : map (`div` 2) (oeis @187085)

instance OEIS 187085 where
  oeisIx  (succ->n) = (oeis @187085) !! (n - 1)
  oeis = zipWith (+) (oeis @187072) $ tail (oeis @187072)

instance OEIS 186771 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @186711)

instance OEIS 186712 where
  oeisIx n = (+ 1) $ fromJust $ findIndex (== (oeisIx @3586) n) (oeis @186711)

instance OEIS 185694 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where
            y = sum $ zipWith (*) xs $ map negate (oeis @8683)

instance OEIS 185086 where
  oeis = filter (\p -> any ((== 1) . (oeisIx @10052)) $
                 map (p -) $ takeWhile (<= p) (oeis @1248)) (oeis @40)

instance OEIS 183091 where
  oeisIx = product . (rowT @210208) . succ

instance OEIS 183063 where
  oeisIx = sum . map (1 -) . (rowT @247795) . succ

instance OEIS 182472 where
  oeisIx = fromJust . (`elemIndex` (oeis @182458))

instance OEIS 182402 where
  oeis = map (sum . map (oeisIx @55642)) $ t 1 [1..] where
     t i xs = ys : t (i + 1) zs where
       (ys, zs) = splitAt i xs

instance OEIS 182205 where
  oeis = iterate (oeisIx @6368) 40

instance OEIS 182126 where
  oeis = zipWith3 (\p p' p'' -> mod (p * p') p'')
                    (oeis @40) (tail (oeis @40)) (drop 2 (oeis @40))

instance OEIS 181894 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ (rowT @213925) n

instance OEIS 181741 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @81118)

instance OEIS 181511 where
  oeis = tablList @181511
instance Table 181511 where
  rowCol = rowCol_off @181511 @1 @0
  rowT   = rowT_off   @181511 @1
  tabl = tail $ map init (tabl @8279)

instance OEIS 180864 where
  oeis = iterate (oeisIx @6368) 13

instance OEIS 180853 where
  oeis = iterate (oeisIx @6368) 4

instance OEIS 180663 where
  oeis = tablList @180663
instance Table 180663 where
  tabl = map reverse (tabl @180662)

instance OEIS 180197 where
  oeis = f 1 where
     f x = if length ps == 3 && nub ps == ps
           then (2 ^ (ps!!0 * ps!!1) `mod` ps!!2) : f (x+2) else f (x+2)
           where ps = (rowT @27746) x

instance OEIS 180191 where
  oeisIx (succ->n) = if n == 1 then 0 else sum $ (rowT @116853) (n - 1)

instance OEIS 179242 where
  oeis = concatMap h $ drop 3 $ inits $ drop 2 (oeis @45) where
     h is = reverse $ map (+ f) fs where
            (f:_:fs) = reverse is

instance OEIS 178799 where
  oeis = zipWith (-) (tail (oeis @25487)) (oeis @25487)

instance OEIS 178361 where
  oeis = [x | x <- [1..], (oeisIx @7953) x <= (oeisIx @55642) x]

instance OEIS 177000 where
  oeis = filter (all (\x -> even x || (oeisIx @10051 . pred) x == 1) .
                         (init . (rowT @70165))) (oeis @40)

instance OEIS 176995 where
  oeis = filter ((> 0) . (oeisIx @230093)) [1..]

instance OEIS 175911 where
  oeisIx (succ->n) = foldl1 (\v d -> b * v + d) rls where
     b = maximum rls + 1
     rls = (rowT @101211) n

instance OEIS 175872 where
  oeisIx = f . (rowT @30308) . succ where
     f xs | all (== 1) xs = length xs
          | otherwise     = f $ map genericLength $ group xs

instance OEIS 175592 where
  oeis = filter (z 0 0 . (rowT @27746)) $ [1..] where
     z u v []     = u == v
     z u v (p:ps) = z (u + p) v ps || z u (v + p) ps

instance OEIS 175499 where
  oeis = zipWith (-) (tail (oeis @175498)) (oeis @175498)

instance OEIS 175119 where
  oeis = map (+ 1) $ zipWith (-) (tail (oeis @175118)) (oeis @175118)

instance OEIS 175118 where
  oeis = 2 : f 2 (oeis @40) where
     f x ps = g $ dropWhile (<= x) ps where
       g (q:qs) | (oeisIx @10051 . pred) (q - x + 1) == 1 = g qs
                | otherwise                 = q : f q qs

instance OEIS 175048 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 . concatMap
     (\bs@ (b:_) -> if b == 1 then 1 : bs else bs) . group . (rowT @30308) . succ

instance OEIS 175047 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 . concatMap
     (\bs@ (b:_) -> if b == 0 then 0 : bs else bs) . group . (rowT @30308) . succ

instance OEIS 175046 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 .
            concatMap (\bs@ (b:_) -> b : bs) . group . (rowT @30308) . succ

instance OEIS 174956 where
  oeis = unfoldr x (1, 1, (oeis @1358)) where
     x (i, z, ps'@ (p:ps)) | i == p = Just (z, (i + 1, z + 1, ps))
                          | i /= p = Just (0, (i + 1, z, ps'))

instance OEIS 174429 where
  oeisIx = (oeisIx @45) . (oeisIx @8908)

instance OEIS 174375 where
  oeisIx n = n ^ 2 - (oeisIx @169810) n

instance OEIS 174332 where
  oeisIx = (oeisIx @208238) . (oeisIx @40)

instance OEIS 173732 where
  oeis = f $ tail (oeis @25480) where f (x : _ : _ : xs) = x : f xs

instance OEIS 173557 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (subtract 1) $ (rowT @27748) n

instance OEIS 173517 where
  oeisIx n = (1 - (oeisIx @10052) n) * (oeisIx @28391) n

instance OEIS 173018 where
  oeis = tablList @173018
instance Table 173018 where
  tabl = map reverse (tabl @123125)

instance OEIS 172471 where
  oeisIx = (oeisIx @196) . (* 2)

instance OEIS 172287 where
  oeis = filter
     (\p -> (oeisIx @10051 . pred) (2 * p - 3) + (oeisIx @10051 . pred) (3 * p - 2) == 1) (oeis @40)

instance OEIS 171904 where
  oeisIx (succ->n) = head [m | m <- (oeis @171901), (oeisIx @196368) (m + n) == 0]

instance OEIS 171903 where
  oeis = elemIndices 0 $
                 zipWith (+) (oeis @196368) $ tail (oeis @196368)

instance OEIS 171886 where
  oeis = elemIndices 1 $ map (oeisIx @209229) $ concat (tabl @8949)

instance OEIS 171865 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @181391)

instance OEIS 171862 where
  oeisIx n = 1 + fromJust (elemIndex n (oeis @181391))

instance OEIS 171746 where
  oeisIx = (+ 1) . length . takeWhile (== 0) .
                             map (oeisIx @10052) . tail . iterate (oeisIx @28392) . succ

instance OEIS 169835 where
  oeis = f [] (tail (oeis @217)) (tail (oeis @290)) where
     f ts us'@ (u:us) vs'@ (v:vs)
       | u <= v = f (u : ts) us vs'
       | any p $ map (divMod v) ts = v : f ts us' vs
       | otherwise = f ts us' vs
       where p (q, r) = r == 0 && (oeisIx @10054) q == 1

instance OEIS 169669 where
  oeisIx n = (oeisIx @30) n * mod n 10

instance OEIS 169630 where
  oeisIx n = (oeisIx @7598) n * n

instance OEIS 168036 where
  oeisIx n = (oeisIx @3415) n - n

instance OEIS 167700 where
  oeisIx = p (oeis @16754) where
     p _  0 = 1
     p (q:qs) m = if m < q then 0 else p qs (m - q) + p qs m

instance OEIS 167489 where
  oeisIx = product . map length . group . (rowT @30308)

instance OEIS 167008 where
  oeisIx = sum . (rowT @219206)

instance OEIS 166920 where
  oeis = scanl (+) 0 (oeis @14551)

instance OEIS 166474 where
  oeis = 1 : 2 : zipWith (+)
     (tail (oeis @166474)) (zipWith (*) (oeis @166474) $ drop 2 (oeis @217))

instance OEIS 166251 where
  oeis = concat $ (filter ((== 1) . length)) $
     map (filter ((== 1) . (oeisIx @10051 . pred))) $
     zipWith enumFromTo (oeis @100484) (tail (oeis @100484))

instance OEIS 166133 where
  oeis = 1 : 2 : 4 : f (3:[5..]) 4 where
     f zs x = y : f (delete y zs) y where
              y = head $ O.isect (rowT @27750 (x ^ 2 - 1)) zs

instance OEIS 165930 where
  oeis = 1 : zipWith (-) (tail (oeis @64491)) (oeis @64491)

instance OEIS 165413 where
  oeisIx = genericLength . nub . map length . group . (rowT @30308) . succ

instance OEIS 165157 where
  oeis = scanl (+) 0 (oeis @133622)

instance OEIS 165153 where
  oeisIx = product . (rowT @165416) . succ

instance OEIS 164861 where
  oeis = filter ((== 0) . (oeisIx @178225)) (oeis @5408)

instance OEIS 164297 where
  oeisIx (succ->n) = genericLength [m | let ts = (rowT @38566) n, m <- ts,
                          any ((> 1) . gcd m) (ts \\ [m])]

instance OEIS 164296 where
  oeisIx (succ->n) = genericLength [m | let ts = (rowT @38566) n, m <- ts,
                          all ((== 1) . gcd m) (ts \\ [m])]

instance OEIS 163974 where
  oeisIx (succ->n) = f (oeis @40) 1 nn 0 where
     f (p:ps) l nl xx
       | yy > nl   = 0
       | yy < nl   = f ps (l + 1) (nl + nn) yy + f ps l nl xx
       | otherwise = if w == n then 1 else 0
       where w = if r == 0 then (oeisIx @196) m else 0
             (m, r) = divMod yy l
             yy = xx + p * p
     nn = n ^ 2

instance OEIS 163926 where
  oeisIx = genericLength . (rowT @163925) . succ

instance OEIS 163271 where
  oeisIx = sum . (rowT @128966) . (subtract 1) . succ

instance OEIS 160967 where
  oeis = m (oeis @79) (oeis @2450) where
     m xs'@ (x:xs) ys'@ (y:ys) | x < y     = x : m xs ys'
                             | x == y    = x : m xs ys
                             | otherwise = y : m xs' ys

instance OEIS 160855 where
  oeis = 1 : f 2 1 [2..] where
     f x sum zs = g zs where
       g (y:ys) = if binSub x (sum + y)
                     then y : f (x + 1) (sum + y) (delete y zs) else g ys
     binSub u = sub where
        sub w = mod w m == u || w > u && sub (div w 2)
        m = (oeisIx @62383) u

instance OEIS 160588 where
  oeis = concat $ transpose [oeis @53645, oeis @27]

instance OEIS 159780 where
  oeisIx n = sum $ zipWith (*) bs $ reverse bs
     where bs = (rowT @30308) n

instance OEIS 159765 where
  oeis = (rowT @27750) 1000000

instance OEIS 159051 where
  oeis = map (+ 2) $ elemIndices 0 $ zipWith mod (oeis @45) [2..]

instance OEIS 158036 where
  oeisIx = (\x -> (4^x - 2^x + 8*x^2 - 2) `div` (2*x* (2*x + 1))) . (oeisIx @158034)

instance OEIS 157793 where
  oeis = f [head (oeis @23416)] $ tail (oeis @23416) where
     f zs (x:xs) = (sum $ zipWith (*) zs (oeis @120)) : f (x:zs) xs

instance OEIS 157729 where
  oeisIx = (+ 5) . (oeisIx @45)
  oeis = 5 : 6 : map (subtract 5)
                         (zipWith (+) (oeis @157729) $ tail (oeis @157729))

instance OEIS 157727 where
  oeisIx = (+ 4) . (oeisIx @45)
  oeis = 4 : 5 : map (subtract 4)
                         (zipWith (+) (oeis @157727) $ tail (oeis @157727))

instance OEIS 157726 where
  oeisIx = (+ 3) . (oeisIx @45)
  oeis = 3 : 4 : map (subtract 3)
                         (zipWith (+) (oeis @157726) $ tail (oeis @157726))

instance OEIS 157725 where
  oeisIx = (+ 2) . (oeisIx @45)
  oeis = 2 : 3 : map (subtract 2)
                         (zipWith (+) (oeis @157725) $ tail (oeis @157725))

instance OEIS 157104 where
  oeisIx = (oeisIx @3415) . (oeisIx @4709)

instance OEIS 156689 where
  oeis = f 1 1 where
     f u v | v > uu `div` 2        = f (u + 1) (u + 2)
           | gcd u v > 1 || w == 0 = f u (v + 2)
           | otherwise             = (u + v - w) `div` 2 : f u (v + 2)
           where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

instance OEIS 156679 where
  oeis = f 1 1 where
     f u v | v > uu `div` 2        = f (u + 1) (u + 2)
           | gcd u v > 1 || w == 0 = f u (v + 2)
           | otherwise             = w : f u (v + 2)
           where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

instance OEIS 156678 where
  oeis = f 1 1 where
     f u v | v > uu `div` 2        = f (u + 1) (u + 2)
           | gcd u v > 1 || w == 0 = f u (v + 2)
           | otherwise             = v : f u (v + 2)
           where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

instance OEIS 156144 where
  oeisIx (succ->n) = p [x | x <- [1..n], (oeisIx @10888) x == (oeisIx @10888) n] n where
     p _  0 = 1
     p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 155046 where
  oeis = concat $ transpose [tail (oeis @1333), tail (oeis @129)]

instance OEIS 154691 where
  oeis = 1 : zipWith (+)
                     (oeis @154691) (drop 2 $ map (* 2) (oeis @45))

instance OEIS 152271 where
  oeisIx = (oeisIx @57979) . (+ 2)
  oeis = concat $ transpose [repeat 1, [1..]]

instance OEIS 151949 where
  oeisIx n = (oeisIx @4186) n - (oeisIx @4185) n

instance OEIS 151910 where
  oeis = zipWith (-) (tail (oeis @1682)) (oeis @1682)

instance OEIS 151821 where
  oeis = x : xs where (x : _ : xs) = (oeis @79)

instance OEIS 151799 where
  oeisIx = (oeisIx @7917) . (subtract 1) . succ

instance OEIS 145829 where
  oeis = map (oeisIx @196) $ filter ((== 1) . (oeisIx @10052)) $ tail (oeis @145768)

instance OEIS 145799 where
  oeisIx = maximum . map (foldr (\b v -> 2 * v + b) 0) .
                      filter (\bs -> bs == reverse bs && head bs == 1) .
                      substr . bin . succ where
     substr [] = []
     substr us'@ (_:us) = sub us' ++ substr us where
        sub [] = []; sub (v:vs) = [v] : [v : ws | ws <- sub vs ]
     bin 0 = []; bin n = b : bin n' where (n', b) = divMod n 2

instance OEIS 145445 where
  oeis = f (oeis @40) $ drop 2 (oeis @290) where
     f ps'@ (p:ps) xs'@ (x:xs) = if p < x then x : f ps xs' else f ps' xs

instance OEIS 145108 where
  oeis = filter ((== 0) . (`mod` 4)) (oeis @133809)

instance OEIS 144925 where
  oeisIx = genericLength . (rowT @163870) . succ

instance OEIS 144624 where
  oeisIx n = (oeisIx @78823) n - fi n

instance OEIS 144582 where
  oeis = [x | x <- [0..], (oeisIx @30) x == (oeisIx @30) (x ^ 3)]

instance OEIS 143967 where
  oeisIx = f 0 . (+ 1) . succ where
     f y 1 = (oeisIx @4086) y
     f y x = f (10 * y + 3 + 4 * r) x' where (x', r) = divMod x 2

instance OEIS 143667 where
  oeis = f (oeis @3849) where
     f (0:0:ws) = 0 : f ws; f (0:1:ws) = 1 : f ws; f (1:0:ws) = 2 : f ws

instance OEIS 143344 where
  oeis = zipWith (-) (tail (oeis @22941)) (oeis @22941)

instance OEIS 143206 where
  oeis = (3*7) : f (oeis @40) where
     f (p:ps@ (p':_)) | p'-p == 4 = (p*p') : f ps
                     | otherwise = f ps

instance OEIS 143205 where
  oeis = filter f [1,3..] where
     f x = length pfs == 2 && last pfs - head pfs == 6 where
         pfs = (rowT @27748) x

instance OEIS 143203 where
  oeis = filter f [1,3..] where
     f x = length pfs == 2 && last pfs - head pfs == 4 where
         pfs = (rowT @27748) x

instance OEIS 143201 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (+ 1) $ zipWith (-) (tail pfs) pfs
     where pfs = (rowT @27748) n

instance OEIS 143072 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @23416)) (oeis @143071)

instance OEIS 143071 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @120)) [1..]

instance OEIS 143070 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @23416)) [1..]

instance OEIS 142987 where
  oeis = 1 : 10 : zipWith (+)
                          (map (* 10) $ tail (oeis @142987))
                          (zipWith (*) (drop 2 (oeis @2378)) (oeis @142987))

instance OEIS 142986 where
  oeis = 1 : 8 : zipWith (+)
                         (map (* 8) $ tail (oeis @142986))
                         (zipWith (*) (drop 2 (oeis @2378)) (oeis @142986))

instance OEIS 142985 where
  oeis = 1 : 6 : zipWith (+)
                         (map (* 6) $ tail (oeis @142985))
                         (zipWith (*) (drop 2 (oeis @2378)) (oeis @142985))

instance OEIS 142984 where
  oeis = 1 : 4 : zipWith (+)
                         (map (* 4) $ tail (oeis @142984))
                         (zipWith (*) (drop 2 (oeis @2378)) (oeis @142984))

instance OEIS 142983 where
  oeis = 1 : 2 : zipWith (+)
                         (map (* 2) $ tail (oeis @142983))
                         (zipWith (*) (drop 2 (oeis @2378)) (oeis @142983))

instance OEIS 140434 where
  oeis = 1 : zipWith (-) (tail (oeis @18805)) (oeis @18805)

instance OEIS 140119 where
  oeisIx = sum . (rowT @95195) . succ

instance OEIS 139764 where
  oeisIx = head . (rowT @35517) . succ

instance OEIS 139544 where
  oeis = 1 : 2 : 4 : tail (oeis @16825)

instance OEIS 137488 where
  oeis = m (map (^ 24) (oeis @40)) (map (^ 4) (oeis @6881)) where
     m xs'@ (x:xs) ys'@ (y:ys) | x < y = x : m xs ys'
                             | otherwise = y : m xs' ys

instance OEIS 137409 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @24362)

instance OEIS 136655 where
  oeisIx = product . (rowT @182469)

instance OEIS 136446 where
  oeis = map (+ 1) $ findIndices (> 1) (oeis @211111)

instance OEIS 136183 where
  oeisIx (succ->n) = sum $ zipWith lcm ps $ tail ps where ps = (rowT @27750) n

instance OEIS 136119 where
  oeis = f [1..] where
     f zs@ (y:xs) = y : f (delete (zs !! y) xs)

instance OEIS 135581 where
  oeisIx n = [d | d <- [1..], (oeisIx @137488) n `mod` d == 0] !! 4

instance OEIS 135440 where
  oeis = zipWith (-) (tail (oeis @14551)) (oeis @14551)

instance OEIS 134451 where
  oeisIx = until (< 3) (oeisIx @53735)

instance OEIS 134204 where
  oeis = 2 : f 1 2 (tail (oeis @40)) where
     f x q ps = p' : f (x + 1) p' (delete p' ps) where
       p' = head [p | p <- ps, mod (p + q) x == 0]

instance OEIS 133811 where
  oeis = 1 : filter f [2..] where
     f x = (and $ zipWith (<) eps $ tail eps) &&
           (all (== 1) $ zipWith (-) (tail ips) ips)
       where ips = map (oeisIx @49084) $ (rowT @27748) x
             eps = (rowT @124010) x

instance OEIS 133466 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @57918)

instance OEIS 133042 where
  oeisIx = (^ 3) . (oeisIx @41)

instance OEIS 133008 where
  oeisIx (succ->n) = genericLength [x | x <- takeWhile (< n) (oeis @28),
                      n `mod` x == 0, let y = n `div` x, x < y,
                      y `elem` takeWhile (<= n) (oeis @28)]

instance OEIS 132995 where
  oeis = tail $ f (oeis @40) 0 1 where
     f (p:ps) u v = (gcd u v) : f ps (p + u) (p * v)

instance OEIS 132199 where
  oeis = zipWith (-) (tail (oeis @106108)) (oeis @106108)

instance OEIS 132157 where
  oeis = (map length) (group (oeis @63882))

instance OEIS 131577 where
  oeisIx = (`div` 2) . (oeisIx @79)
  oeis = 0 : (oeis @79)

instance OEIS 130887 where
  oeisIx = sum . map (oeisIx @225) . (rowT @27750) . succ

instance OEIS 130883 where
  oeisIx = (oeisIx @128918) . (* 2)

instance OEIS 130534 where
  oeis = tablList @130534
instance Table 130534 where
  tabl = map (map abs) (tabl @8275)

instance OEIS 129871 where
  oeis = 1 : (oeis @58)

instance OEIS 129300 where
  oeis = 1 : f [1] 1 where
     f xs@ (x:_) k = y : f (y:xs) (k+1) where
       y = x + sum [z | z <- xs, z <= k]

instance OEIS 129299 where
  oeis = 1 : f [1] 2 where
     f xs@ (x:_) k = y : f (y:xs) (k+1) where
       y = x + sum [z | z <- xs, z <= k]

instance OEIS 129286 where
  oeis = iterate (oeisIx @129283) 5

instance OEIS 129283 where
  oeisIx n = (oeisIx @3415) n + n

instance OEIS 129152 where
  oeis = iterate (oeisIx @3415) 15625

instance OEIS 129151 where
  oeis = iterate (oeisIx @3415) 81

instance OEIS 129150 where
  oeis = iterate (oeisIx @3415) 8

instance OEIS 128543 where
  oeisIx = sum . (rowT @134239) . subtract 1 . succ

instance OEIS 128218 where
  oeis = zipWith (-) (tail (oeis @128217)) (oeis @128217)

instance OEIS 127367 where
  oeisIx n | even n    = n - m + 1
            | otherwise = n + m
            where m = length $ takeWhile (<= n) (oeis @2378)

instance OEIS 127366 where
  oeisIx n | even n'   = n'
            | otherwise = 2*n - n'
            where n' = n + (oeisIx @196) n

instance OEIS 126869 where
  oeisIx n = (rowT @204293) (2*n) !! n

instance OEIS 126768 where
  oeis = map length $ group (oeis @117872)

instance OEIS 126684 where
  oeis = tail $ m (oeis @695) $ map (* 2) (oeis @695) where
     m xs'@ (x:xs) ys'@ (y:ys) | x < y     = x : m xs ys'
                             | otherwise = y : m xs' ys

instance OEIS 126596 where
  oeisIx n = (oeisIx @5810) n * (oeisIx @5408) n `div` (oeisIx @16777) n

instance OEIS 126027 where
  oeisIx = genericLength . (rowT @30717) . succ

instance OEIS 126024 where
  oeisIx = genericLength . filter ((== 1) . (oeisIx @10052) . sum) .
                            subsequences . enumFromTo 1

instance OEIS 125886 where
  oeis = 1 : f 1 [10..] where
     f u vs = g vs where
       g (w:ws) = if (oeisIx @10879) w == iu then w : f w (delete w vs) else g ws
       iu = (oeisIx @30) u

instance OEIS 123345 where
  oeis = filter
    (\x -> all (`isInfixOf` b x) $ map b $ (rowT @27750) x) [1..] where
    b = unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 122797 where
  oeis  = 1 : zipWith (+) (oeis @122797) (map ((1 -) . (oeisIx @10054)) [1..])

instance OEIS 122768 where
  oeis = 0 : f (tail (oeis @41)) [1] where
     f (p:ps) rs = (sum $ zipWith (*) rs $ tail (oeis @41)) : f ps (p : rs)

instance OEIS 120960 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @24362)

instance OEIS 118965 where
  oeisIx = sum . map (0 ^) . (rowT @128924) . succ

instance OEIS 118959 where
  oeis = filter
     (\x -> let x' = (oeisIx @4086) x in x' /= x && x `mod` x' == 0) [1..]

instance OEIS 118882 where
  oeis = findIndices (> 1) (oeis @161)

instance OEIS 118532 where
  oeis = iterate ((+ 15) . (oeisIx @4086)) 1

instance OEIS 118416 where
  oeis = tablList @118416
instance Table 118416 where
  rowCol = rowCol_off @118416 @1 @1
  rowT 1 = [1]
  rowT n = (map (* 2) $ (rowT @118416) (n - 1)) ++ [oeisIx @14480 (n- 1)]
  tabl = map (rowT @118416) [1..]

instance OEIS 117989 where
  oeis = tail $ zipWith (-)
                        (map (* 2) (oeis @41)) $ tail (oeis @41)

instance OEIS 117890 where
  oeis = [x | x <- [1..], let z = (oeisIx @23416) x, z > 0, mod x z == 0]

instance OEIS 117872 where
  oeisIx = flip mod 2 . (oeisIx @7501)

instance OEIS 117767 where
  oeisIx = (+ 1) . (* 2) . (oeisIx @6)

instance OEIS 117704 where
  oeis = 1 : zipWith (-) (tail (oeis @5214)) (oeis @5214)

instance OEIS 117591 where
  oeis = zipWith (+) (oeis @79) (oeis @45)

instance OEIS 117546 where
  oeisIx = p $ drop 3 (oeis @73) where
     p _  0     = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 117366 where
  oeisIx = (oeisIx @151800) . (oeisIx @6530)

instance OEIS 116966 where
  oeis = zipWith (+) [0..] $ drop 2 (oeis @140081)

instance OEIS 116942 where
  oeisIx = fromJust . (`elemIndex` (oeis @116941))

instance OEIS 116941 where
  oeis = f 0 1 (zip (oeis @116939) [0..]) [] where
     f u v xis'@ ((x,i):xis) ws
       | x == u    = i : f u v xis ws
       | x == v    = f u v xis (i : ws)
       | otherwise = reverse ws ++ f v x xis' []

instance OEIS 116940 where
  oeisIx n = last $ elemIndices n $ takeWhile (<= n + 1) (oeis @116939)

instance OEIS 116939 where
  oeis = 0 : f [0] where
     f xs@ (x : _) = ys ++ f ys where
       ys = if odd x then (x + 1 : x : map (+ 1) xs) else map (+ 1) xs

instance OEIS 116590 where
  oeis = 1 : zipWith (+) (oeis @5185) (drop 2 (oeis @5185))

instance OEIS 116549 where
  oeisIx = genericIndex (oeis @116549)
  oeis = 1 : zipWith ((+) `on` (oeisIx @116549)) (oeis @523) (oeis @53645)

instance OEIS 116371 where
  oeisIx (succ->n) = p (oeis @17173) n where
     p _  0 = 1
     p [] _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 115945 where
  oeis = elemIndices 0 $ map (oeisIx @115944) [0..]

instance OEIS 115671 where
  oeisIx = p [x | x <- [0..], (mod x 32) `notElem` [0,2,12,14,16,18,20,30]]
     where p _          0 = 1
           p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 115300 where
  oeisIx (succ->n) = (oeisIx @54054) n * (oeisIx @54055) n

instance OEIS 114374 where
  oeisIx = p (oeis @13929) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 114334 where
  oeis = (rowT @27750) (6 ^ 6)

instance OEIS 114266 where
  oeisIx n = head [m | m <- [1..],
                        (oeisIx @10051 . pred) (2 * (oeisIx @40) n + (oeisIx @40) (n + m)) == 1]

instance OEIS 114265 where
  oeisIx (succ->n) = head [p | let (q:qs) = drop (n - 1) (oeis @40), p <- qs,
                        (oeisIx @10051 . pred) (2 * q + p) == 1]

instance OEIS 114183 where
  oeis = 1 : f [1] where
     f xs@ (x:_) = y : f (y : xs) where
       y = if z `notElem` xs then z else 2 * x where z = (oeisIx @196) x

instance OEIS 114102 where
  oeisIx (succ->n) = genericLength $ filter (== 1) $
              map (length . nub . (map (oeisIx @10888))) $ ps 1 n
     where ps x 0 = [[]]
           ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

instance OEIS 113646 where
  oeisIx n = if n < 3 then 4 else (oeisIx @14683) n

instance OEIS 113215 where
  oeis = concat $ zipWith take
                          [1, 3 ..] $ map (repeat . (oeisIx @6218)) [0 ..]

instance OEIS 112988 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @89088)) . (oeisIx @40)

instance OEIS 111711 where
  oeis = 1 : zipWith (+) (oeis @111711) (oeis @80512)

instance OEIS 111546 where
  oeis = 1 : f 2 [1] where
     f v ws@ (w:_) = y : f (v + 1) (y : ws) where
                    y = v * w + (sum $ zipWith (*) ws $ reverse ws)

instance OEIS 111418 where
  oeis = tablList @111418
instance Table 111418 where
  tabl = map reverse (tabl @122366)

instance OEIS 111208 where
  oeisIx n = genericLength $ takeWhile (<= (oeisIx @217) n) (oeis @40)

instance OEIS 111192 where
  oeis = f (oeis @40) where
     f (p:ps@ (q:r:_)) | q - p == 6 = (p*q) : f ps
                      | r - p == 6 = (p*r) : f ps
                      | otherwise  = f ps

instance OEIS 111178 where
  oeisIx = p $ tail (oeis @5563) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 111133 where
  oeisIx = subtract 1 . (oeisIx @9)

instance OEIS 111046 where
  oeisIx = (* 2) . (oeisIx @54735)

instance OEIS 110765 where
  oeisIx = product . zipWith (^) (oeis @40) .  reverse . (rowT @30308) . succ

instance OEIS 110475 where
  oeisIx 0 = 0
  oeisIx (succ->n) = genericLength us - 1 + 2 * length vs where
              (us, vs) = span (== 1) $ (rowT @118914) n

instance OEIS 110353 where
  oeisIx (succ->n) = (+ 1) $ fromJust $
     findIndex ((== 0) . (`mod` t)) $ dropWhile (<= t) (oeis @217)
     where t = (oeisIx @217) n

instance OEIS 110240 where
  oeisIx = foldl (\v d -> 2 * v + d) 0 . (rowT @70950)

instance OEIS 109984 where
  oeisIx = sum . zipWith (*) [0..] . (rowT @109983)

instance OEIS 109983 where
  oeis = tablList @109983
instance Table 109983 where
  tabf = zipWith (++) (map (flip take (repeat 0)) [0..]) (tabl @63007)

instance OEIS 109400 where
  oeis = concat $ zipWith (++) (tabl @2260) (tabl @37126)

instance OEIS 109303 where
  oeis = filter ((> 0) . (oeisIx @107846)) [0..]

instance OEIS 108906 where
  oeis = zipWith (-) (tail (oeis @6899)) (oeis @6899)

instance OEIS 108804 where
  oeis = f [head (oeis @10060)] $ tail (oeis @10060) where
     f xs (z:zs) = (sum $ zipWith (*) xs (reverse xs)) : f (z : xs) zs

instance OEIS 108348 where
  oeis = 1 : f [2..] where
     f (x:xs) = g (oeis @40) where
       g (p:ps) = h 0 $ map ((`div` (p - 1)) . subtract 1) $
                            iterate (* p) (p ^ 2) where
         h i (pp:pps) | pp > x    = if i == 0 then f xs else g ps
                      | pp < x    = h 1 pps
                      | otherwise = x : f xs

instance OEIS 108309 where
  oeisIx = sum . (map (oeisIx @10051 . pred)) . (rowT @176271) . succ

instance OEIS 107782 where
  oeisIx n = (oeisIx @23416) n - (oeisIx @87116) n

instance OEIS 107750 where
  oeis = 0 : f 0 where
     f x = y : f y where
       y = head [z | z <- [x + 1 ..], (oeisIx @23416) z /= (oeisIx @23416) x]

instance OEIS 107741 where
  oeisIx n = if null ms then 0 else head ms  where
     ms = [m | let p = (oeisIx @40) n,
               m <- [max 0 (p - 9 * (oeisIx @55642) p) .. p - 1], (oeisIx @62028) m == p]

instance OEIS 106404 where
  oeisIx (succ->n) = genericLength [d | d <- takeWhile (<= n) (oeis @100484), mod n d == 0]

instance OEIS 106151 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 . concatMap
     (\bs'@ (b:bs) -> if b == 0 then bs else bs') . group . (rowT @30308) . succ

instance OEIS 106146 where
  oeisIx = flip mod 10 . (oeisIx @1358)

instance OEIS 105612 where
  oeisIx = (subtract 1) . (oeisIx @224)

instance OEIS 105321 where
  oeisIx n = if n == 0 then 1 else (oeisIx @1316) n + (oeisIx @1316) (n - 1)

instance OEIS 105213 where
  oeis = 932 : map
        (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105213)

instance OEIS 105212 where
  oeis = 668 : map
        (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105212)

instance OEIS 105211 where
  oeis = 412 : map
        (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105211)

instance OEIS 105210 where
  oeis = 393 : map
        (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105210)

instance OEIS 105154 where
  oeisIx n = t [n] where
     t xs@ (x:_) | y `elem` xs = length xs
                | otherwise   = t (y : xs) where y = (oeisIx @105027) x

instance OEIS 105153 where
  oeisIx n = t [n] where
     t xs@ (x:_) | y `elem` xs = length xs
                | otherwise   = t (y : xs) where y = (oeisIx @105025) x

instance OEIS 105082 where
  oeis = scanl (+) 5 $ tail (oeis @48696)

instance OEIS 104777 where
  oeisIx = (^ 2) . (oeisIx @7310)

instance OEIS 104315 where
  oeis = filter (\x -> (oeisIx @168046) x == 0 && (oeisIx @168046) (x ^ 2) == 1) [1..]

instance OEIS 104126 where
  oeisIx n = p ^ (p + 1) where p = (oeisIx @40) n

instance OEIS 103371 where
  oeis = tablList @103371
instance Table 103371 where
  tabl = map reverse (tabl @132813)

instance OEIS 103285 where
  oeisIx = last . (rowT @103284)

instance OEIS 103147 where
  oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @47160))

instance OEIS 102376 where
  oeisIx = (4 ^) . (oeisIx @120)

instance OEIS 102364 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ filter (== 0) $ (rowT @213676) n

instance OEIS 101624 where
  oeisIx = sum . zipWith (*) (oeis @79) . map (flip mod 2) . (rowT @11973)

instance OEIS 101461 where
  oeisIx = maximum . (rowT @53121)

instance OEIS 101404 where
  oeisIx n = (oeisIx @101403) n * n

instance OEIS 101403 where
  oeis = map length $ group (oeis @101402)

instance OEIS 101402 where
  oeisIx = genericIndex (oeis @101402)
  oeis = 0 : 1 : zipWith ((+) `on` (oeisIx @101402))
                         (tail (oeis @53644)) (oeis @53645)

instance OEIS 101300 where
  oeisIx = (oeisIx @151800) . (oeisIx @151800)

instance OEIS 101082 where
  oeis = filter ((> 0) . (oeisIx @49502)) [0..]

instance OEIS 101048 where
  oeisIx = p (oeis @1358) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 100861 where
  oeis = tablList @100861
instance Table 100861 where
  tabf = zipWith take (oeis @8619) (tabl @144299)

instance OEIS 100795 where
  oeis = f 0 (oeis @2024) where
     f x ws = v : f v (us ++ vs) where (us, v:vs) = span (== x) ws

instance OEIS 100708 where
  oeis = map abs $ zipWith (-) (tail (oeis @100707)) (oeis @100707)

instance OEIS 100440 where
  oeisIx = genericLength . (rowT @200741) . succ

instance OEIS 99966 where
  oeis = map (last . init) $ tail (tabf @99964)

instance OEIS 99909 where
  oeis = map (flip div 2) $ tail $ zipWith (+)
     (zipWith (*) (oeis @40) $ map (subtract 1) $ tail (oeis @40))
     (zipWith (*) (map (subtract 1) (oeis @40)) $ tail (oeis @40))

instance OEIS 99848 where
  oeis = concat $ zipWith replicate (oeis @8480) [1..]

instance OEIS 99425 where
  oeisIx = sum . (rowT @102413)

instance OEIS 99188 where
  oeisIx = (* 2) . (oeisIx @49474)

instance OEIS 98884 where
  oeisIx = p (oeis @7310) where
     p _  0     = 1
     p (k:ks) m = if k > m then 0 else p ks (m - k) + p ks m

instance OEIS 98842 where
  oeis = map length $ group (oeis @60384)

instance OEIS 98430 where
  oeisIx n = (oeisIx @302) n * (oeisIx @984) n

instance OEIS 98096 where
  oeisIx n = (oeisIx @1248) n * (oeisIx @34785) n

instance OEIS 97944 where
  oeisIx = (oeisIx @55642) . (oeisIx @40)

instance OEIS 97796 where
  oeisIx = p (oeis @396) . succ where
     p _ 0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 97602 where
  oeis = 1 : f 1 1 where
     f x c = y : f y (c + (oeisIx @10052) y) where y = x + c

instance OEIS 97451 where
  oeisIx n = p (oeis @47228) n where
     p _  0         = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 97256 where
  oeis = map (* 9) (oeis @7088)

instance OEIS 96981 where
  oeisIx = p $ tail (oeis @47273) where
     p _  0         = 1
     p ks'@ (k:ks) m = if k > m then 0 else p ks' (m - k) + p ks m

instance OEIS 96824 where
  oeis = 0 : 1 : 2 : zipWith (-)
     (map (* 2) $ drop 2 (oeis @96824)) (map (oeisIx @96824) $ tail (oeis @122797))

instance OEIS 96796 where
  oeis = 0 : 1 : zipWith (-)
     (map (* 2) $ tail (oeis @96796)) (map (oeisIx @96796) $ tail (oeis @83920))

instance OEIS 96494 where
  oeisIx = (* 2) . (oeisIx @6)

instance OEIS 96165 where
  oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @1222)) $ tail (oeis @961)

instance OEIS 96138 where
  oeis = 1 : g 2 1 where
     g x y = z : g (x + 1) z where z = (oeisIx @4086) (x * y)

instance OEIS 95874 where
  oeisIx (succ->n) | y == n    = length xs + 1
            | otherwise = 0
            where (xs, y:ys) = span (< n) (oeis @961)

instance OEIS 95381 where
  oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @209229) (oeis @25586)

instance OEIS 95114 where
  oeis = 1 : f [1] 1 where
     f xs@ (x:_) k = y : f (y:xs) (k+1) where
       y = x + length [z | z <- xs, z <= k]

instance OEIS 95072 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @31444)

instance OEIS 94784 where
  oeis = [x | x <- [0..], (oeisIx @10052) x == 0, (oeisIx @10057) x == 0]

instance OEIS 94638 where
  oeis = tablList @94638
instance Table 94638 where
  rowCol = rowCol_off @94638 @1 @1
  rowT   = rowT_off   @94638 @1
  tabl = map reverse (tabl @130534)

instance OEIS 94588 where
  oeis = 0 : zipWith (+) (tail (oeis @45)) (zipWith (*) [1..] (oeis @45))

instance OEIS 94524 where
  oeisIx = (+ 2) . (* 3) . (oeisIx @23208)

instance OEIS 94015 where
  oeisIx = sum . (rowT @152842)

instance OEIS 93903 where
  oeis = 1 : f [1] (oeis @40) where
     f xs@ (x:_) ps = g ps where
       g (q:qs) | x <= q         = h ps
                | y `notElem` xs = y : f (y:xs) (delete q ps)
                | otherwise      = g qs where
         y = x - q
         h (r:rs) | z `notElem` xs = z : f (z:xs) (delete r ps)
                  | otherwise      = h rs where
           z = x + r

instance OEIS 93642 where
  oeis = filter
    (\x -> not $ all (`isInfixOf` b x) $ map b $ (rowT @27750) x) [1..] where
    b = unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 93446 where
  oeisIx = maximum . (rowT @93445) . succ

instance OEIS 93096 where
  oeis = f [3,3] where
     f (u : vs@ (v : _)) = u : f (vs ++
       if w < 10 then [w] else uncurry ((. return) . (:)) $ divMod w 10)
          where w = u * v

instance OEIS 93095 where
  oeis = f [2,3] where
     f (u : vs@ (v : _)) = u : f (vs ++
       if w < 10 then [w] else uncurry ((. return) . (:)) $ divMod w 10)
          where w = u * v

instance OEIS 93094 where
  oeis = f [2,2] where
     f (u : vs@ (v : _)) = u : f (vs ++
       if w < 10 then [w] else uncurry ((. return) . (:)) $ divMod w 10)
          where w = u * v

instance OEIS 92954 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @92953))

instance OEIS 91072 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @14707)

instance OEIS 91067 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @14707)

instance OEIS 90826 where
  oeis = map (sum . zipWith (*) (oeis @45) . reverse) $
                     tail $ inits (oeis @108)

instance OEIS 90503 where
  oeis = f [1..] where
     f (x:xs) = g $ tail (oeis @961) where
       g (q:pps) = h 0 $ map ((`div` (q - 1)) . subtract 1) $
                             iterate (* q) (q ^ 3) where
         h i (qy:ppys) | qy > x    = if i == 0 then f xs else g pps
                       | qy < x    = h 1 ppys
                       | otherwise = x : f xs

instance OEIS 90127 where
  oeis = f (oeis @10) [] where
    f (x:xs) phis | x `elem` phis = f xs phis
                  | otherwise     = x : f xs (x : phis)

instance OEIS 90079 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 . map head . group . (rowT @30308)

instance OEIS 90076 where
  oeis = zipWith (*) (oeis @40) $ drop 2 (oeis @40)

instance OEIS 89999 where
  oeisIx = (oeisIx @217) . (oeisIx @211201)

instance OEIS 89951 where
  oeis = [x | x <- [0..], (oeisIx @30) x == (oeisIx @30) (x ^ 2)]

instance OEIS 89648 where
  oeis = filter ((<= 1) . abs . (oeisIx @37861)) [0..]

instance OEIS 89625 where
  oeisIx (succ->n) = f n 0 (oeis @40) where
     f 0 y _      = y
     f x y (p:ps) = f x' (y + p * r) ps where (x',r) = divMod x 2

instance OEIS 89224 where
  oeisIx = (oeisIx @23416) . (oeisIx @23416)

instance OEIS 88864 where
  oeisIx 0 = 0
  oeisIx (succ->n) = maximum $ zipWith ((*) `on` foldr (\d v -> v * 2 + d) 0)
              (init $ tail $ inits bs) (init $ tail $ tails bs)
              where bs = (rowT @30308) n

instance OEIS 88763 where
  oeisIx = flip div 2 . (oeisIx @87695)

instance OEIS 88723 where
  oeis = filter f [2..] where
     f x = 1 `elem` (zipWith (-) (tail divs) divs)
           where divs = tail $ (rowT @27750) x

instance OEIS 88707 where
  oeisIx = (+ 1) . (oeisIx @1358)

instance OEIS 88670 where
  oeisIx = p $ tail (oeis @10785) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 88517 where
  oeisIx n = (oeisIx @1462) (n + 1) - (oeisIx @1462) n
  oeis = zipWith (-) (tail (oeis @1462)) (oeis @1462)



instance OEIS 88442 where
  oeisIx = (+ 1) . (oeisIx @4514)

instance OEIS 88382 where
  oeis = [x | x <- [1..], x <= (oeisIx @20639) x ^ 4]

instance OEIS 88359 where
  oeis = map succ $ elemIndices 1 (oeis @51135)

instance OEIS 88209 where
  oeis = zipWith (+) (oeis @45) $ tail (oeis @45925)

instance OEIS 87980 where
  oeis = 1 : filter f [2..] where
     f x = isPrefixOf ps (oeis @40) && all (< 0) (zipWith (-) (tail es) es)
           where ps = (rowT @27748) x; es = (rowT @124010) x

instance OEIS 87897 where
  oeisIx = p [3,5..] where
     p [] _ = 0
     p _  0 = 1
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 87686 where
  oeis = map succ $ findIndices (> 1) (oeis @51135)

instance OEIS 87188 where
  oeisIx = p (oeis @5117) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 87039 where
  oeisIx (succ->n) | null ps   = 1
            | otherwise = head ps
            where ps = tail $ reverse $ (rowT @27746) n

instance OEIS 86892 where
  oeis = tail $ zipWith gcd (oeis @225) (oeis @3462)

instance OEIS 86862 where
  oeis = zipWith (-) (tail (oeis @2113)) (oeis @2113)

instance OEIS 86518 where
  oeis = zipWith ((flip div 2 .) . (+))
                         (oeis @86517) $ tail (oeis @86517)

instance OEIS 85983 where
  oeisIx = count9 0 . (oeisIx @40) where
     count9 c x | d == 9    = if x < 10 then c + 1 else count9 (c + 1) x'
                | otherwise = if x < 10 then c else count9 c x'
                where (x', d) = divMod x 10

instance OEIS 85982 where
  oeisIx = count8 0 . (oeisIx @40) where
     count8 c x | d == 8    = if x < 10 then c + 1 else count8 (c + 1) x'
                | otherwise = if x < 10 then c else count8 c x'
                where (x', d) = divMod x 10

instance OEIS 85981 where
  oeisIx = count7 0 . (oeisIx @40) where
     count7 c x | d == 7    = if x < 10 then c + 1 else count7 (c + 1) x'
                | otherwise = if x < 10 then c else count7 c x'
                where (x', d) = divMod x 10

instance OEIS 85980 where
  oeisIx = count6 0 . (oeisIx @40) where
     count6 c x | d == 6    = if x < 10 then c + 1 else count6 (c + 1) x'
                | otherwise = if x < 10 then c else count6 c x'
                where (x', d) = divMod x 10

instance OEIS 85979 where
  oeisIx = count5 0 . (oeisIx @40) where
     count5 c x | d == 5    = if x < 10 then c + 1 else count5 (c + 1) x'
                | otherwise = if x < 10 then c else count5 c x'
                where (x', d) = divMod x 10

instance OEIS 85978 where
  oeisIx = count4 0 . (oeisIx @40) where
     count4 c x | d == 4    = if x < 10 then c + 1 else count4 (c + 1) x'
                | otherwise = if x < 10 then c else count4 c x'
                where (x', d) = divMod x 10

instance OEIS 85977 where
  oeisIx = count3 0 . (oeisIx @40) where
     count3 c x | d == 3    = if x < 10 then c + 1 else count3 (c + 1) x'
                | otherwise = if x < 10 then c else count3 c x'
                where (x', d) = divMod x 10

instance OEIS 85976 where
  oeisIx = count2 0 . (oeisIx @40) where
     count2 c x | d == 2    = if x < 10 then c + 1 else count2 (c + 1) x'
                | otherwise = if x < 10 then c else count2 c x'
                where (x', d) = divMod x 10

instance OEIS 85975 where
  oeisIx = count1 0 . (oeisIx @40) where
     count1 c x | d == 1    = if x < 10 then c + 1 else count1 (c + 1) x'
                | otherwise = if x < 10 then c else count1 c x'
                where (x', d) = divMod x 10

instance OEIS 85974 where
  oeisIx = count0 0 . (oeisIx @40) where
     count0 c x | d == 0    = if x < 10 then c + 1 else count0 (c + 1) x'
                | otherwise = if x < 10 then c else count0 c x'
                where (x', d) = divMod x 10

instance OEIS 85787 where
  oeis = scanl (+) 0 (oeis @80512)

instance OEIS 85731 where
  oeisIx (succ->n) = gcd n $ (oeisIx @3415) n

instance OEIS 85238 where
  oeisIx n = e (mod x 2 + 2) x where
     x = (oeisIx @6899) n
     e b p = if p == 1 then 0 else 1 + e b (p `div` b)

instance OEIS 84558 where
  oeisIx n = (oeisIx @90529) (n + 1) - 1

instance OEIS 84114 where
  oeisIx = g 0 1 . tail . (rowT @27750) . succ where
     g c _ []     = c
     g c x (d:ds) = if r > 0 then g c (x * d) ds else g (c + 1) x' ds
                    where (x', r) = divMod x d

instance OEIS 84113 where
  oeisIx = f 0 1 . (rowT @27750) . succ where
     f c _ []     = c
     f c x (d:ds) = if r == 0 then f c x' ds else f (c + 1) (x * d) ds
                    where (x', r) = divMod x d

instance OEIS 83534 where
  oeis = zipWith (-) (tail (oeis @7617)) (oeis @7617)

instance OEIS 83533 where
  oeis = zipWith (-) (tail (oeis @2202)) (oeis @2202)

instance OEIS 83479 where
  oeis = m [0..] (oeis @33638) where
     m xs'@ (x:xs) ys'@ (y:ys) | x <= y    = x : m xs ys'
                             | otherwise = y : m xs' ys

instance OEIS 83348 where
  oeis = filter ((> 0) . (oeisIx @168036)) [1..]

instance OEIS 83347 where
  oeis = filter ((< 0) . (oeisIx @168036)) [1..]

instance OEIS 83207 where
  oeis = filter (z 0 0 . (rowT @27750)) $ [1..] where
     z u v []     = u == v
     z u v (p:ps) = z (u + p) v ps || z u (v + p) ps

instance OEIS 83025 where
  oeisIx 0 = 0
  oeisIx (succ->n) = genericLength [x | x <- (rowT @27746) n, mod x 4 == 1]

instance OEIS 82587 where
  oeis = concat $ transpose [tail (oeis @204), (oeis @204)]

instance OEIS 82582 where
  oeis = 1 : 1 : f [1,1] where
     f xs'@ (x:_:xs) = y : f (y : xs') where
       y = x + sum (zipWith (*) xs' $ reverse xs)

instance OEIS 82416 where
  oeis = map (`mod` 2) (oeis @73941)

instance OEIS 81848 where
  oeis = 3 : tail (zipWith (-) (tail (oeis @70885)) (oeis @70885))

instance OEIS 81834 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = z + (if x `elem` zs then 4 else 3)

instance OEIS 81827 where
  oeis = zipWith (-) (tail (oeis @5185)) (oeis @5185)

instance OEIS 81729 where
  oeisIx n = (oeisIx @209229) n + (oeisIx @33999) (n)

instance OEIS 80982 where
  oeisIx (succ->n) = (+ 1) $ fromJust $
     findIndex ((== 0) . (`mod` (n ^ 2))) $ tail (oeis @217)

instance OEIS 80983 where
  oeisIx = (oeisIx @217) . (oeisIx @80982)

instance OEIS 80944 where
  oeis = filter ((<= 2) . (oeisIx @80942)) [1..]

instance OEIS 80942 where
  oeisIx (succ->n) = genericLength $
              filter ((flip isPrefixOf `on` (rowT @30308)) n) $ (rowT @27750) n

instance OEIS 80764 where
  oeis = tail $ zipWith (-) (tail (oeis @49472)) (oeis @49472)

instance OEIS 80719 where
  oeisIx = foldr (\b v -> 2 * v + b) 0 .
             concat . mapMaybe (flip lookup bin) . (rowT @31298)
              where bin = zip [0..9] (tabf @30308)

instance OEIS 80709 where
  oeis = iterate (oeisIx @3132) 4

instance OEIS 80590 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = z + (if x `elem` zs then 3 else 4)

instance OEIS 80579 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = if x `elem` zs then z + 1 else z + 4

instance OEIS 80578 where
  oeis = 1 : f 2 [1] where
     f x zs@ (z:_) = y : f (x + 1) (y : zs) where
       y = if x `elem` zs then z + 1 else z + 3

instance OEIS 80257 where
  oeis = m (oeis @24619) (oeis @33942) where
     m xs'@ (x:xs) ys'@ (y:ys) | x < y  = x : m xs ys'
                             | x == y = x : m xs ys
                             | x > y  = y : m xs' ys

instance OEIS 80225 where
  oeisIx (succ->n) = genericLength [d | d <- takeWhile (<= n) (oeis @396), mod n d == 0]

instance OEIS 79579 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ zipWith (*) pfs $ map (subtract 1) pfs
     where pfs = (rowT @27746) n

instance OEIS 79094 where
  oeisIx = (oeisIx @3415) . (oeisIx @79079)

instance OEIS 79082 where
  oeis = zipWith div (tail (oeis @23523)) (oeis @79080)

instance OEIS 79081 where
  oeis = zipWith div (oeis @79079) (oeis @79080)

instance OEIS 79080 where
  oeisIx n = (oeisIx @79079) n `gcd` (oeisIx @23523) (n + 1)

instance OEIS 78894 where
  oeis = sieve (oeis @40) where
     sieve (p:ps) = p : sieve [q | (i,q) <- zip [2..] ps, mod i p > 0]

instance OEIS 78833 where
  oeisIx = last . (rowT @225243) . succ

instance OEIS 78832 where
  oeisIx = head . (rowT @225243) . succ

instance OEIS 78829 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @78826

instance OEIS 78826 where
  oeisIx n | n <= 1 = 0
            | otherwise = length $ (rowT @225243) n

instance OEIS 78823 where
  oeisIx = sum . (rowT @119709)

instance OEIS 78779 where
  oeis = m (oeis @5117) $ map (* 2) (oeis @5117) where
     m xs'@ (x:xs) ys'@ (y:ys) | x < y     = x : m xs ys'
                             | x == y    = x : m xs ys
                             | otherwise = y : m xs' ys

instance OEIS 78730 where
  oeisIx (succ->n) = sum $ zipWith (*) ps $ tail ps where ps = (rowT @27750) n

instance OEIS 78701 where
  oeisIx n = if null odds then 1 else head odds
              where odds = tail $ (rowT @182469) n

instance OEIS 78649 where
  oeis = map (+ 1) $ (map succ $ elemIndices 0 $ tail $ oeis @54354)

instance OEIS 78440 where
  oeis = filter notbp (oeis @196871) where
     notbp x = m > 0 && x > 1 || m == 0 && notbp x' where
        (x',m) = divMod x 2

instance OEIS 78430 where
  oeisIx = sum . (rowT @245717) . succ

instance OEIS 78323 where
  oeisIx = (oeisIx @3415) . (oeisIx @78310)

instance OEIS 78147 where
  oeis = zipWith (-) (tail (oeis @13929)) (oeis @13929)

instance OEIS 78134 where
  oeisIx = (p $ drop 2 (oeis @290)) . succ where
     p _          0 = 1
     p ks'@ (k:ks) x = if x < k then 0 else p ks' (x - k) + p ks x

instance OEIS 78065 where
  oeis = 1 : zipWith (*) (cycle [-1, 1])
     (zipWith (+) (map (* 2) (oeis @5251)) (map (* 3) $ drop 2 (oeis @5251)))

instance OEIS 78057 where
  oeisIx = sum . (rowT @35607)

instance OEIS 77957 where
  oeisIx = sum . (rowT @204293)

instance OEIS 77800 where
  oeis = concat $ zipWith (\p q -> if p == q+2 then [q,p] else [])
                                  (tail (oeis @40)) (oeis @40)

instance OEIS 77666 where
  oeisIx = sum . (rowT @77664) . succ

instance OEIS 77665 where
  oeisIx = last . (rowT @77664) . succ

instance OEIS 77582 where
  oeisIx = sum . (rowT @77581) . succ

instance OEIS 77267 where
  oeisIx n = (oeisIx @79978) n + if n < 3 then 0 else (oeisIx @77267) (n `div` 3)

instance OEIS 77221 where
  oeis = scanl (+) 0 (oeis @47522)

instance OEIS 77077 where
  oeis = iterate (oeisIx @55944) 775

instance OEIS 77076 where
  oeis = iterate (oeisIx @55944) 537

instance OEIS 77043 where
  oeis = scanl (+) 0 (oeis @1651)

instance OEIS 76948 where
  oeisIx 0 = 1
  oeisIx (succ->n) = if null qs then 0 else head qs
              where qs = filter ((> 0) . (oeisIx @37213) . subtract 1 . (* n)) [1..n]

instance OEIS 76654 where
  oeis = f (oeis @67251) 1 where
    f xs z = g xs where
      g (y:ys) = if (oeisIx @30) y == mod z 10 then y : f (delete y xs) y else g ys

instance OEIS 76641 where
  oeisIx = (oeisIx @4086) . (oeisIx @67251)

instance OEIS 76627 where
  oeisIx n = (oeisIx @5) n * (oeisIx @49820) n

instance OEIS 76446 where
  oeis = zipWith (-) (tail (oeis @1694)) (oeis @1694)

instance OEIS 76405 where
  oeis = 1 : f (tail $ zip (oeis @1597) (oeis @25478)) where
     f ((p, r) : us) = g us where
       g ((q, r') : vs) = if r' == r then q : f us else g vs

instance OEIS 76404 where
  oeisIx = (`mod` 2) . (oeisIx @1597)

instance OEIS 76259 where
  oeis = zipWith (-) (tail (oeis @5117)) (oeis @5117)

instance OEIS 76217 where
  oeis = 1 : zipWith (+) (oeis @76217)
     (zipWith (*) [2..] $ map (oeisIx @57427) $ zipWith (-) [2..] (oeis @76217))

instance OEIS 76191 where
  oeis = zipWith (-) (tail (oeis @1222)) (oeis @1222)

instance OEIS 75322 where
  oeisIx = (oeisIx @75323) . subtract 1 . (* 2) . succ

instance OEIS 75321 where
  oeisIx = (oeisIx @75323) . (* 2)

instance OEIS 75268 where
  oeis = iterate (oeisIx @55944) 442

instance OEIS 75254 where
  oeisIx n = n + (oeisIx @1414) n + 1

instance OEIS 75253 where
  oeis = iterate (oeisIx @55944) 77

instance OEIS 75158 where
  oeisIx = fromJust . (`elemIndex` (oeis @75157))

instance OEIS 75157 where
  oeisIx 0 = 0
  oeisIx n = product (zipWith (^) (oeis @40) rs') - 1 where
     rs' = reverse $ r : map (subtract 1) rs
     (r:rs) = reverse $ map length $ group $ (rowT @30308) n

instance OEIS 75119 where
  oeisIx (succ->n) = denominator $ n % (oeisIx @196) n

instance OEIS 75109 where
  oeis = filter odd (oeis @1597)

instance OEIS 75105 where
  oeisIx (succ.succ->n) = numerator $ n % (oeisIx @523) n

instance OEIS 75104 where
  oeisIx (succ->n) = gcd n $ (oeisIx @523) n

instance OEIS 75090 where
  oeis = filter even (oeis @1597)

instance OEIS 74985 where
  oeisIx = (oeisIx @290) . (oeisIx @1358)

instance OEIS 74819 where
  oeis = map succ $ elemIndices 0 $ oeis @92410

instance OEIS 74235 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @227481

instance OEIS 73890 where
  oeisIx (succ->n) = numerator $ n % (oeisIx @196) n

instance OEIS 73783 where
  oeis = zipWith (-) (tail (oeis @2808)) (oeis @2808)

instance OEIS 73740 where
  oeis = tail $ f (oeis @73739) [] where
     f (x:xs) ys = (sum $ zipWith (*) ys (oeis @73739)) : f xs (x : ys)

instance OEIS 73739 where
  oeis = concat $ transpose [1 : 1 : repeat 0, tail (oeis @36467)]

instance OEIS 73710 where
  oeis = conv (oeis @73709) [] where
     conv (v:vs) ws = (sum $ zipWith (*) ws' $ reverse ws') : conv vs ws'
                      where ws' = v : ws

instance OEIS 73709 where
  oeis = 1 : zipWith (-) (tail (oeis @73708)) (oeis @73708)

instance OEIS 73708 where
  oeis = conv (oeis @73707) [] where
     conv (v:vs) ws = (sum $ zipWith (*) ws' $ reverse ws') : conv vs ws'
                      where ws' = v : ws

instance OEIS 73642 where
  oeisIx = sum . zipWith (*) [0..] . (rowT @30308)

instance OEIS 73576 where
  oeisIx = p (oeis @5117) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 73445 where
  oeis = zipWith (-) (tail (oeis @73783)) (oeis @73783)

instance OEIS 73395 where
  oeisIx n = (oeisIx @8472) n * (oeisIx @1414) n

instance OEIS 73334 where
  oeisIx 0 = 3
  oeisIx n = (oeisIx @45) $ (oeisIx @5811) n + 4

instance OEIS 72905 where
  oeisIx (succ->n) = head [k | k <- [n + 1 ..], (oeisIx @10052) (k * n) == 1]

instance OEIS 72603 where
  oeis = filter ((> 0) . (oeisIx @37861)) [1..]

instance OEIS 72602 where
  oeis = filter ((>= 0) . (oeisIx @37861)) [1..]

instance OEIS 72601 where
  oeis = filter ((<= 0) . (oeisIx @37861)) [0..]

instance OEIS 72600 where
  oeis = filter ((< 0) . (oeisIx @37861)) [0..]

instance OEIS 72544 where
  oeis = [x | x <- [0..], (oeisIx @54054) x == (oeisIx @30) x]

instance OEIS 72543 where
  oeis = [x | x <- [0..], (oeisIx @54055) x == (oeisIx @30) x]

instance OEIS 72541 where
  oeis = concat $ transpose
                 [map (+ 1) (oeis @23200), map (+ 5) (oeis @23200)]

instance OEIS 72511 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @61357))

instance OEIS 72452 where
  oeis = 0 : map (oeisIx @4086) (zipWith (+) (oeis @72452) [1..])

instance OEIS 72219 where
  oeisIx = (+ 1) . (* 2) . (oeisIx @33264) . subtract 1

instance OEIS 72084 where
  oeisIx = product . map (oeisIx @120) . (rowT @27746) . succ

instance OEIS 72010 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map f $ (rowT @27746) n where
     f 2 = 2
     f p = p + 2 * (2 - p `mod` 4)

instance OEIS 71904 where
  oeis = filter odd (oeis @2808)

instance OEIS 71888 where
  oeisIx 0 = 2
  oeisIx (succ->n) = head [m | m <- dropWhile (<= n) (oeis @5117), gcd m n > 1]

instance OEIS 71786 where
  oeisIx = product . map (oeisIx @4086) . (rowT @27746) . succ

instance OEIS 71521 where
  oeisIx (succ->n) = genericLength $ takeWhile (<= n) (oeis @3586)

instance OEIS 71321 where
  oeisIx 0 = 0
  oeisIx (succ->n) = sum $ zipWith (*) (oeis @33999) $ (rowT @27746) n

instance OEIS 71295 where
  oeisIx n = (oeisIx @120) n * (oeisIx @23416) n

instance OEIS 70760 where
  oeis = [x | x <- [0..], let y = (oeisIx @61205) x,
                      y /= x ^ 2, (oeisIx @10052) y == 1]

instance OEIS 70750 where
  oeisIx = (2 -) . (`mod` 4) . (oeisIx @40)

instance OEIS 70216 where
  oeis = tablList @70216
instance Table 70216 where
  rowCol = rowCol_off @70216 @1 @1
  rowT   = rowT_off   @70216 @1
  tabl = zipWith (zipWith (\u v -> (u + v) `div` 2))
                         (tabl @215630) (tabl @215631)

instance OEIS 70215 where
  oeisIx = (oeisIx @586) . (oeisIx @40)

instance OEIS 70168 where
  oeis = tablList @70168
instance Table 70168 where
  rowCol = rowCol_off @70168 @1 @1
  tabf = map (rowT @70168) [1..]
  rowT n = (takeWhile (/= 1) $ iterate (oeisIx @14682) n) ++ [1]

instance OEIS 70048 where
  oeisIx = p (oeis @42968) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 69545 where
  oeis = map length $ group (oeis @8836)

instance OEIS 69213 where
  oeisIx = last . (rowT @77581) . succ

instance OEIS 69158 where
  oeisIx = product . (rowT @225817) . succ

instance OEIS 68720 where
  oeisIx = (oeisIx @3415) . (oeisIx @290) . succ

instance OEIS 68500 where
  oeis = h 0 (oeis @4090) (oeis @45) where
     h r (q:qs) (f:fs) = if q <= r then h r qs fs else f : h q qs fs

instance OEIS 68396 where
  oeisIx n = p - (oeisIx @4086) p  where p = (oeisIx @40) n

instance OEIS 68395 where
  oeis = zipWith (-) (oeis @40) (oeis @7605)

instance OEIS 68346 where
  oeisIx = (oeisIx @3415) . (oeisIx @3415)

instance OEIS 68328 where
  oeisIx = (oeisIx @3415) . (oeisIx @5117)

instance OEIS 68319 where
  oeisIx n = if n <= spf ^ 2 then spf else (oeisIx @68319) $ spf + div n spf
              where spf = (oeisIx @20639) n

instance OEIS 68312 where
  oeisIx = (oeisIx @3415) . (oeisIx @217)

instance OEIS 68068 where
  oeisIx = genericLength . filter odd . (rowT @77610) . succ

instance OEIS 67970 where
  oeis = zipWith (-) (tail (oeis @14076)) (oeis @14076)

instance OEIS 67962 where
  oeis = 1 : zipWith (*) (oeis @67962) (drop 2 (oeis @1654))

instance OEIS 67872 where
  oeisIx (succ->n) = (until ((== 1) . (oeisIx @10052) . (+ 1)) (+ nn) nn) `div` nn
              where nn = n ^ 2

instance OEIS 67815 where
  oeisIx (succ->n) = gcd n $ (oeisIx @196) n

instance OEIS 67722 where
  oeisIx n = head [k | k <- [1..], (oeisIx @10052) (n * (n + k)) == 1]

instance OEIS 67391 where
  oeisIx (succ->n) | n <= 2    = 1
            | otherwise = foldl lcm 1 $ (rowT @173540) n

instance OEIS 67046 where
  oeisIx = (`div` 6) . (oeisIx @33931)

instance OEIS 66955 where
  oeisIx (succ->n) = genericLength [ (x,y,z) | x <- [1 .. (oeisIx @196) (div n 3)],
                                y <- [x .. div n x],
                                z <- [y .. div (n - x*y) (x + y)],
                                x * y + (x + y) * z == n]

instance OEIS 66729 where
  oeisIx (succ->n) = if pds == [1] then n else product pds
              where pds = (rowT @27751) n

instance OEIS 66658 where
  oeis = map denominator
     (1 : (concat $ tail $ zipWith (\u vs -> map (% u) vs)
                                   (oeis @66720) (inits (oeis @66720))))

instance OEIS 66657 where
  oeis = map numerator
     (1 : (concat $ tail $ zipWith (\u vs -> map (% u) vs)
                                   (oeis @66720) (inits (oeis @66720))))

instance OEIS 66638 where
  oeisIx n = (oeisIx @7947) n ^ (oeisIx @51903) n

instance OEIS 66360 where
  oeisIx (succ->n) = genericLength [ (x,y,z) | x <- [1 .. (oeisIx @196) n],
                                y <- [x .. div n x],
                                z <- [y .. n - x*y],
                                x*y+ (x+y)*z == n, gcd (gcd x y) z == 1]

instance OEIS 66246 where
  oeis = unfoldr x (1, 1, (oeis @2808)) where
     x (i, z, cs'@ (c:cs)) | i == c = Just (z, (i + 1, z + 1, cs))
                          | i /= c = Just (0, (i + 1, z, cs'))

instance OEIS 66195 where
  oeisIx n = fromJust $ find ((== n) . (oeisIx @23416)) (oeis @40)

instance OEIS 66054 where
  oeis = iterate (oeisIx @56964) 10583

instance OEIS 65730 where
  oeisIx = (oeisIx @48760) . (oeisIx @40)

instance OEIS 65650 where
  oeisIx = fromJust . (`elemIndex` (oeis @65649))

instance OEIS 65649 where
  oeis = zipWith (+)
                 (map ((* 10) . subtract 1) (oeis @65648)) (0 : (oeis @33307))

instance OEIS 65648 where
  oeis = f (0 : (oeis @33307)) $ take 10 $ repeat 1 where
     f (d:ds) counts = y : f ds (xs ++ (y + 1) : ys) where
                             (xs, y:ys) = splitAt d counts

instance OEIS 65516 where
  oeis = zipWith (-) (tail (oeis @1358)) (oeis @1358)

instance OEIS 65515 where
  oeisIx (succ->n) = genericLength $ takeWhile (<= n) (oeis @961)

instance OEIS 65500 where
  oeisIx n = (oeisIx @3418) n + n - signum n

instance OEIS 65383 where
  oeisIx n = head $ dropWhile (< (oeisIx @217) n) (oeis @40)

instance OEIS 65350 where
  oeis = zipWith mod (tail (oeis @984)) (drop 2 (oeis @290))

instance OEIS 65339 where
  oeisIx 0 = 0
  oeisIx (succ->n) = genericLength [x | x <- (rowT @27746) n, mod x 4 == 3]

instance OEIS 65306 where
  oeis = map (subtract 2) $ f (concat (tabl @65305)) [] where
     f (x:xs) ys = if x `elem` ys then f xs ys else x : f xs (x:ys)

instance OEIS 65206 where
  oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @56964)) (oeis @29742)

instance OEIS 65090 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @151763)

instance OEIS 65003 where
  oeis = elemIndices 0 $ map (oeisIx @214772) [0..43]

instance OEIS 64959 where
  oeis = map ((+ 1) . fromJust . (`elemIndex` (oeis @64419))) [1..]

instance OEIS 64834 where
  oeisIx n = sum $ take (length nds `div` 2) $
                    map abs $ zipWith (-) nds $ reverse nds
     where nds = (rowT @31298) n

instance OEIS 64807 where
  oeis = filter (\x -> x `mod` (oeisIx @10888) x == 0) [1..]

instance OEIS 64700 where
  oeis = filter f [1..] where
     f x = mdr > 0 && x `mod` mdr == 0 where mdr = (oeisIx @31347) x

instance OEIS 64689 where
  oeisIx = fromJust . (`elemIndex` (oeis @64672))

instance OEIS 64672 where
  oeis = 0 : 1 : f (drop 2 (oeis @196)) 1 1 (tail (oeis @64672))
     where f (r:rs) r' u (v:vs)
             | r == r' = (u + v) : f rs r u vs
             | r /= r' = u' : f rs r u' (tail (oeis @64672))
             where u' = (oeisIx @64672) $ fromInteger r

instance OEIS 64654 where
  oeis = map length $ group (oeis @195376)

instance OEIS 64649 where
  oeisIx = sum . (rowT @47916) . succ

instance OEIS 64614 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map f $ (rowT @27746) n where
     f 2 = 3; f 3 = 2; f p = p

instance OEIS 64150 where
  oeis = filter (\x -> x `mod` (oeisIx @53735) x == 0) [1..]

instance OEIS 64113 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @36263)

instance OEIS 63993 where
  oeisIx n = genericLength [ () | let ts = takeWhile (< n) $ tail (oeis @217),
                      x <- ts, y <- takeWhile (<= x) ts,
                      let z = n - x - y, 0 < z, z <= y, (oeisIx @10054) z == 1]

instance OEIS 63936 where
  oeis = map (+ 1) $
                 findIndices (\x -> x > 1 && (oeisIx @10052) x == 1) (oeis @34460)

instance OEIS 63919 where
  oeisIx 0 = 1
  oeisIx (succ->n) = sum $ init $ (rowT @77610) n

instance OEIS 63905 where
  oeis =
     concat $ zipWith ($) (map replicate (oeis @40)) (oeis @40)

instance OEIS 63114 where
  oeisIx (succ->n) = n + (oeisIx @51801) n

instance OEIS 62992 where
  oeisIx = sum . (rowT @234950)

instance OEIS 62759 where
  oeisIx n = (oeisIx @7947) n ^ (oeisIx @51904) n

instance OEIS 62730 where
  oeis =  filter f $ [3..] where
     f x = not $ all null $ zipWith
           (\us (v:vs) -> map (v -) us `intersect` map (subtract v) vs)
           (tail $ init $ inits bns) (tail $ init $ tails bns)
           where bns = (rowT @34868) x

instance OEIS 62550 where
  oeisIx 0 = 0
  oeisIx n = sum $ (rowT @13942) n

instance OEIS 62503 where
  oeisIx = (oeisIx @290) . (oeisIx @5117)

instance OEIS 62332 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @208259)

instance OEIS 62285 where
  oeis = filter (even . (oeisIx @30)) (oeis @30141)

instance OEIS 62279 where
  oeisIx 0 = 0
  oeisIx n = until ((== 1) . (oeisIx @136522) . (oeisIx @4151)) (+ n) n

instance OEIS 62162 where
  oeisIx = abs . sum . (rowT @247453)

instance OEIS 62119 where
  oeisIx (succ->n) = (n - 1) * (oeisIx @142) n

instance OEIS 61674 where
  oeisIx n = until ((== 1) . (oeisIx @136522) . (oeisIx @4151) . (* n)) (+ 1) 1

instance OEIS 61467 where
  oeisIx 0 = 0
  oeisIx n = mod (max n n') (min n n') where n' = (oeisIx @4086) n

instance OEIS 61417 where
  oeisIx = sum . (rowT @47917) . succ

instance OEIS 61383 where
  oeis = filter (\x -> mod (oeisIx @7953 x) (oeisIx @55642 x) == 0) [0..]

instance OEIS 61227 where
  oeisIx n = p + (oeisIx @4086) p  where p = (oeisIx @40) n

instance OEIS 61205 where
  oeisIx n = (oeisIx @4086) n * n

instance OEIS 60756 where
  oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @60715)) [0..]

instance OEIS 60646 where
  oeisIx (succ->n) = (fromJust $ findIndex ((n+1) <) (oeis @14688)) + 1

instance OEIS 60432 where
  oeisIx (succ->n) = sum $ zipWith (*) [n,n - 1..1] (oeis @10054)

instance OEIS 60418 where
  oeisIx = (oeisIx @54055) . (oeisIx @40)

instance OEIS 60374 where
  oeisIx n = f $ dropWhile (< n) (oeis @5836) where
     f (p:ps) | (oeisIx @39966) (p-n) == 1 && (oeisIx @39966) (2*p-n) == 1 = 2*p - n
              | otherwise                                  = f ps

instance OEIS 60373 where
  oeisIx n = (oeisIx @60372) n - n

instance OEIS 60372 where
  oeisIx n = (oeisIx @60374 n + n) `div` 2

instance OEIS 60308 where
  oeisIx = (oeisIx @7917) . (oeisIx @5843)

instance OEIS 60226 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @312) n - n * (oeisIx @312) (n - 1)

instance OEIS 59983 where
  oeis = mapMaybe (`elemIndex` (oeis @7599)) [0..]

instance OEIS 59590 where
  oeis = elemIndices 1 $ map (oeisIx @115944) [0..]

instance OEIS 59497 where
  oeis = (oeis @40) \\  (oeis @59496)

instance OEIS 59448 where
  oeisIx = (`mod` 2) . (oeisIx @23416) . succ

instance OEIS 59316 where
  oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @35250)) [1..]

instance OEIS 59011 where
  oeis = filter (odd . (oeisIx @71295)) [0..]

instance OEIS 59010 where
  oeis = filter (even . (oeisIx @23416)) [1..]

instance OEIS 58369 where
  oeis =
     elemIndices 0 $ zipWith ((-) `on` (oeisIx @7953)) [0..] (oeis @290)

instance OEIS 58331 where
  oeisIx = (+ 1) . (oeisIx @1105)

instance OEIS 58277 where
  oeis = map length $ group (oeis @7614)

instance OEIS 58042 where
  oeisIx = (oeisIx @7088) . (oeisIx @61561)

instance OEIS 57890 where
  oeis = 0 : filter ((== 1) . (oeisIx @178225) . (oeisIx @265)) [1..]

instance OEIS 57828 where
  oeisIx (succ->x) = genericLength $ filter ((== 1) . (gcd x)) $
                       takeWhile (<= x) $ tail (oeis @290)

instance OEIS 57705 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @57588)

instance OEIS 57588 where
  oeisIx = (subtract 1) . product . (flip take (oeis @40)) . succ

instance OEIS 57212 where
  oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @35)

instance OEIS 57211 where
  oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @59841)

instance OEIS 56965 where
  oeisIx n = n - (oeisIx @4086) n

instance OEIS 56789 where
  oeisIx = sum . (rowT @51537) . succ

instance OEIS 56768 where
  oeisIx = (oeisIx @607) . (oeisIx @40)

instance OEIS 56595 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n], mod n d == 0, (oeisIx @10052) d == 0]

instance OEIS 56577 where
  oeisIx = head . (rowT @227048)

instance OEIS 56576 where
  oeisIx = subtract 1 . (oeisIx @20914)

instance OEIS 56526 where
  oeis = zipWith (-) (tail (oeis @960)) (oeis @960)

instance OEIS 56062 where
  oeis = map length $ group (oeis @30190)

instance OEIS 55217 where
  oeisIx n = sum $ take (n + 1) $ (rowT @27907) (n + 1)

instance OEIS 55212 where
  oeisIx = subtract 1 . (oeisIx @33273)

instance OEIS 55205 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n^2], n^2 `mod` d == 0, (oeisIx @10052) d == 0]

instance OEIS 55099 where
  oeisIx n = (oeisIx @7481) (2 * n + 1) - (oeisIx @7481) (2 * n)

instance OEIS 54735 where
  oeisIx = (+ 2) . (* 2) . (oeisIx @1359)

instance OEIS 54686 where
  oeis = merge (oeis @290) (oeis @217) where
     merge xs'@ (x:xs) ys'@ (y:ys)
       | x <= y    = x : merge xs ys'
       | otherwise = y : merge xs' ys

instance OEIS 54440 where
  oeisIx = sum . zipWith (*) (oeis @87960) . map (oeisIx @1255) . (rowT @260672)

instance OEIS 53210 where
  oeisIx = sum . (rowT @51599)

instance OEIS 53127 where
  oeisIx = (* 2) . (oeisIx @53132)

instance OEIS 52474 where
  oeisIx (succ->n) = (tabl @56230) !! (n - 1) !! 0

instance OEIS 52008 where
  oeisIx n = (oeisIx @4185) n + (oeisIx @4186) n

instance OEIS 51701 where
  oeis = f 2 $ 1 : (oeis @40) where
     f d (q:ps@ (p:p':_)) = (if d <= d' then q else p') : f d' ps
       where d' = p' - p

instance OEIS 51656 where
  oeisIx = sum . zipWith (*) (oeis @1906) . (rowT @47999)

instance OEIS 51611 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @53603

instance OEIS 51466 where
  oeis = f [head (oeis @25487)] $ tail (oeis @25487) where
     f us (v:vs) = fromJust (find (\x -> mod v x == 0) us) : f (v : us) vs

instance OEIS 51178 where
  oeis = filter (\x -> (oeisIx @27423) x `mod` x == 0) [1..]

instance OEIS 51012 where
  oeisIx (succ->n) = denominator $ (sum $ (rowT @51010) n) % n

instance OEIS 51011 where
  oeisIx (succ->n) = numerator $ (sum $ (rowT @51010) n) % n

instance OEIS 51000 where
  oeisIx = sum . map (^ 3) . (rowT @182469)

instance OEIS 50999 where
  oeisIx = sum . map (^ 2) . (rowT @182469)

instance OEIS 50488 where
  oeisIx n = sum $ zipWith (*) (oeis @79) (reverse $ take n (oeis @5408))

instance OEIS 50320 where
  oeisIx (succ->n) = h n $ tail $ (rowT @206778) n where
     h 1 _          = 1
     h _ []         = 0
     h m fs'@ (f:fs) =
       if f > m then 0 else if r > 0 then h m fs else h m' fs' + h m fs
       where (m', r) = divMod m f

instance OEIS 49445 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @199238)

instance OEIS 49354 where
  oeis = filter f [1..] where
     f n = t0 == (oeisIx @62756) n && t0 == (oeisIx @81603) n where t0 = (oeisIx @77267) n

instance OEIS 48985 where
  oeisIx = foldr (\d v -> 2 * v + d) 0 . concatMap
     (unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2))
     . reverse . (rowT @27746) . succ

instance OEIS 48883 where
  oeisIx = (oeisIx @244) . (oeisIx @120)

instance OEIS 48728 where
  oeisIx n = (oeisIx @8585) n - (oeisIx @48724) n

instance OEIS 48701 where
  oeisIx (succ->n) = foldr (\d v -> 2 * v + d) 0 (reverse bs ++ bs) where
     bs = (rowT @30308) (n - 1)

instance OEIS 48669 where
  oeisIx (succ->n) = maximum $ zipWith (-) (tail ts) ts where
     ts = (rowT @38566) n ++ [n + 1]

instance OEIS 48398 where
  oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @33075)

instance OEIS 48344 where
  oeis = filter f (oeis @29742) where
     f x = (oeisIx @136522) (x * (oeisIx @4086) x) == 1

instance OEIS 48298 where
  oeisIx n = (oeisIx @209229) n * n

instance OEIS 48250 where
  oeisIx = sum . (rowT @206778) . succ

instance OEIS 48153 where
  oeisIx = sum . (rowT @48152) . succ

instance OEIS 48050 where
  oeisIx 0 = 0
  oeisIx (succ->n) = (subtract 1) $ sum $ (rowT @27751) n

instance OEIS 47983 where
  oeisIx n = genericLength [x | x <- [1..n - 1], (oeisIx @5) x == (oeisIx @5) n]

instance OEIS 47813 where
  oeisIx = last . (rowT @262188)

instance OEIS 45920 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @76191)

instance OEIS 45798 where
  oeis = filter (odd . (`mod` 10) . (`div` 10)) (oeis @45572)

instance OEIS 45797 where
  oeis = filter (even . (`mod` 10) . (`div` 10)) (oeis @45572)

instance OEIS 43548 where
  oeisIx (succ->n) = f 1 where
     f k = if distinct $ (map (div k)) [n, n - 1 .. 1] then k else f (k + 1)
     distinct [_] = True; distinct (u:vs@ (v:_)) = u /= v && distinct vs

instance OEIS 37888 where
  oeisIx (succ->n) = div (sum $ map abs $ zipWith (-) bs $ reverse bs) 2
     where bs = (rowT @30308) n

instance OEIS 37861 where
  oeisIx n = (oeisIx @23416) n - (oeisIx @120) n

instance OEIS 37268 where
  oeis = filter ((== 1) . (oeisIx @168046)) $
                        takeWhile (<= 999999999) (oeis @214959)

instance OEIS 37264 where
  oeis = filter ((== 1) . (oeisIx @168046)) $
                        takeWhile (<= 999999999) (oeis @214958)

instance OEIS 37019 where
  oeisIx = product .
     zipWith (^) (oeis @40) . reverse . map (subtract 1) . (rowT @27746)
     . succ

instance OEIS 36554 where
  oeisIx = (+ 1) . (oeisIx @79523)

instance OEIS 35959 where
  oeisIx = p (oeis @47201) where
     p _      0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 35928 where
  oeis = filter (\x -> (oeisIx @36044) x == x) [0,2..]

instance OEIS 35526 where
  oeisIx = (oeisIx @7088) . (oeisIx @35522)

instance OEIS 35191 where
  oeisIx n = (oeisIx @1817) n + (oeisIx @1822) n

instance OEIS 35103 where
  oeisIx = (oeisIx @23416) . (oeisIx @40)

instance OEIS 34891 where
  oeisIx = genericLength . (rowT @212721)

instance OEIS 34886 where
  oeisIx = (oeisIx @55642) . (oeisIx @142)

instance OEIS 34883 where
  oeisIx = maximum . (rowT @51010) . succ

instance OEIS 34874 where
  oeis = 1 : f 2 1 where
     f x y = z : f (x + 1) z where z = (x * (oeisIx @4086) y)

instance OEIS 34838 where
  oeis = filter f (oeis @52382) where
     f u = g u where
       g v = v == 0 || mod u d == 0 && g v' where (v',d) = divMod v 10

instance OEIS 34837 where
  oeis = filter (\i -> i `mod` (oeisIx @30 i) == 0) [1..]

instance OEIS 34794 where
  oeis = 2 : f 2 (tail  (oeis @40)) where
     f x (p:ps) = if elem x $ (rowT @46071) p then p : f p ps else f x ps

instance OEIS 34785 where
  oeisIx = (2 ^) . (oeisIx @40)

instance OEIS 34708 where
  oeis = filter ((== 1) . (oeisIx @168046)) (oeis @214957)

instance OEIS 34699 where
  oeisIx = last . (rowT @210208) . succ

instance OEIS 34676 where
  oeisIx = sum . map (^ 2) . (rowT @77610) . succ

instance OEIS 34460 where
  oeisIx = sum . init . (rowT @77610) . succ

instance OEIS 34444 where
  oeisIx = genericLength . (rowT @77610) . succ

instance OEIS 33989 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @33307) $ (oeisIx @185950) n

instance OEIS 33491 where
  oeisIx = head . (rowT @127824)

instance OEIS 32448 where
  oeisIx n = head [q | q <- (oeis @40), let p = (oeisIx @40) n,
                        q `mod` p == p - 1]

instance OEIS 31448 where
  oeis = filter ((== -1) . (oeisIx @37861)) [1..]

instance OEIS 31444 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @37861

instance OEIS 31076 where
  oeis = concat $ map reverse $ tail (tabf @31087)

instance OEIS 30338 where
  oeis = map length $ filter ((== 2) . head) $ group (oeis @3137)

instance OEIS 30337 where
  oeis = map length $ filter ((== 1) . head) $ group (oeis @3137)

instance OEIS 30336 where
  oeis = map length $ filter ((== 0) . head) $ group (oeis @3137)

instance OEIS 30334 where
  oeis = map (+ 1) $ elemIndices 2 (oeis @3137)

instance OEIS 30333 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @3137)

instance OEIS 30332 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @3137)

instance OEIS 30152 where
  oeis = filter ((== 1) . (oeisIx @228710)) (oeis @290)

instance OEIS 30147 where
  oeis = filter ((== 1) . (oeisIx @228710)) (oeis @2113)

instance OEIS 29907 where
  oeis = 0 : 1 : zipWith (+) (tail (oeis @45))
                        (zipWith (+) (tail (oeis @29907)) (oeis @29907))

instance OEIS 29730 where
  oeis = map (foldr (\h v -> 16 * v + h) 0) $
                     filter (\xs -> xs == reverse xs) (tabf @262437)

instance OEIS 27988 where
  oeisIx = maximum . (rowT @27926)

instance OEIS 27914 where
  oeisIx n = sum $ take (n + 1) $ (rowT @27907) n

instance OEIS 27868 where
  oeisIx n = sum $ takeWhile (> 0) $ map (n `div`) $ tail (oeis @351)

instance OEIS 27837 where
  oeis = f 1 [] where
     f x ys = y : f (x + 1) (y : ys) where
              y = (oeisIx @1044) x * x - sum (zipWith (*) ys $ tail (oeis @1044))

instance OEIS 26490 where
  oeis = map length $ group (oeis @26465)

instance OEIS 26238 where
  oeisIx (succ->n) = (oeisIx @49084) n + (oeisIx @66246) n

instance OEIS 26233 where
  oeisIx n = (oeisIx @49084) n + (oeisIx @239968) n

instance OEIS 18819 where
  oeis = 1 : f (tail (oeis @8619)) where
     f (x:xs) = (sum $ take x (oeis @18819)) : f xs

instance OEIS 14597 where
  oeis = tail $ elemIndices 1 $ map (oeisIx @197183) [0..]

instance OEIS 14481 where
  oeisIx n = (oeisIx @9445) n `div` (oeisIx @1147) n

instance OEIS 14454 where
  oeisIx (succ->n) = sum $ zipWith gcd kfs $ map (div nf) kfs
     where (nf:kfs) = reverse $ (rowT @166350) n

instance OEIS 14417 where
  oeisIx 0 = 0
  oeisIx n = foldl (\v z -> v * 10 + z) 0 $ (rowT @189920) n

instance OEIS 14342 where
  oeis= f (tail (oeis @40)) [head (oeis @40)] 1 where
     f (p:ps) qs k = sum (zipWith (*) qs $ reverse qs) :
                     f ps (p : qs) (k + 1)

instance OEIS 11784 where
  oeisIx = last . (rowT @12257) . succ

instance OEIS 11754 where
  oeisIx = (oeisIx @120) . (oeisIx @244)

instance OEIS 11539 where
  oeis = filter ((> 0) . (oeisIx @102683)) [1..]

instance OEIS 10683 where
  oeisIx = sum . (rowT @144944)

instance OEIS 9023 where
  oeis = filter ((> 1) . (oeisIx @227481)) [1..]

instance OEIS 6338 where
  oeis = tail (oeis @214848)

instance OEIS 4780 where
  oeis = filter ((> 1) . (oeisIx @48728)) [1..]

instance OEIS 4615 where
  oeis = filter (all (== 1) . (map (`mod` 5) . (rowT @27748))) [1..]

instance OEIS 4302 where
  oeisIx 0 = 0
  oeisIx n = (rowCol @103371) (n + 1) 2

instance OEIS 3256 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @242094)) . (oeisIx @1950)

instance OEIS 2733 where
  oeisIx = oeisIx @196 . (subtract 1) . (* 10) . (oeisIx @207337)

instance OEIS 2503 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @65350)

instance OEIS 1614 where
  oeis = f 0 0 (oeis @57211) where
     f c z (x:xs) = z' : f x z' xs where z' = z + 1 + 0 ^ abs (x - c)

instance OEIS 1480 where
  oeisIx n = (oeisIx @196) $ (`div` 3) $ ((oeisIx @7645) n) - ((oeisIx @1479) n) ^ 2

instance OEIS 1479 where
  oeisIx n = (oeisIx @196) $ head $
     filter ((== 1) . (oeisIx @10052)) $ map ((oeisIx @7645) n -) $ tail (oeis @33428)

instance OEIS 105441 where
  oeis = filter ((> 2) . (oeisIx @1227)) [1..]

instance OEIS 64097 where
  oeis = 0 : f 2 where
     f x | x == spf  = 1 + (oeisIx @64097) (spf - 1) : f (x + 1)
         | otherwise = (oeisIx @64097) spf + (oeisIx @64097) (x `div` spf) : f (x + 1)
         where spf = (oeisIx @20639) x

instance OEIS 63994 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (gcd (n - 1) . subtract 1) $ (rowT @27748) n
instance OEIS 60901 where
  oeisIx = (oeisIx @38500) . (oeisIx @45)

instance OEIS 59966 where
  oeisIx (succ->n) = sum (map (\x -> (oeisIx @8683) (n `div` x) * (oeisIx @225) x)
                       [d | d <- [1..n], mod n d == 0]) `div` n


instance OEIS 51402 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` ms) where
     ms = map (abs . (oeisIx @2321)) [1..]

instance OEIS 48395 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @199771) (2 * n)

instance OEIS 35612 where
  oeisIx = (oeisIx @7814) . (oeisIx @22340)

instance OEIS 14963 where
  oeisIx 1 = 1
  oeisIx n | until ((> 0) . (`mod` spf)) (`div` spf) n == 1 = spf
           | otherwise = 1
           where spf = (oeisIx @20639) n

instance OEIS 6992 where
  oeis = iterate (oeisIx @7917 . (* 2)) 2

instance OEIS 258613 where
  oeis = map succ $ elemIndices 1 $ oeis @74695

instance OEIS 198386 where
  oeis = map (^ 2) (oeis @198390)

instance OEIS 187208 where
  oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @187203.pred) [1..]

instance OEIS 187205 where
  oeis = map (+ 1) $ elemIndices 0 $ map (oeisIx @187203.pred) [1..]

instance OEIS 182834 where
  oeisIx (succ->n) = (oeisIx @196) (2 * n - 2) + n
