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
