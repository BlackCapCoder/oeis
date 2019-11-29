module OEIS.Part1 () where

import OEIS.Common
import OEIS.Simple

import Data.Bits
import Data.List
import qualified Data.Set as S
import Data.Numbers.Primes hiding (isPrime)
import Math.NumberTheory.Primes.Testing
import Math.NumberTheory.Powers.Squares.Internal
import Data.Ratio
import Control.Monad
import Data.Function (fix, on)
import Data.Char (intToDigit, digitToInt)
import Math.NumberTheory.Moduli (powMod)
import Math.NumberTheory.Primes.Factorisation (factorise)
import qualified Math.NumberTheory.ArithmeticFunctions as A
import Math.NumberTheory.Recurrences
import Data.Maybe (fromJust)
import Data.Ord (Down (..))
import Data.Tuple
import qualified Data.List.Ordered as O
import qualified Data.Map as M


instance OEIS 5 where
  oeisIx = genericLength . divisors . succ
  -- product . map (+1) . (rowT @124010)

-- TODO: This one is interesting
instance OEIS 6 where
  oeisIx = oeisIx @196 . oeisIx @40

instance OEIS 8 where
  oeisIx = p [1,2,5,10] where
    p _          0 = 1
    p []         _ = 0
    p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 13 where
  oeisIx 0 = 1
  oeisIx n = sum (zipWith (*)
   (map (oeisIx @10 . pred . (* 2)) ds) (map (2 ^) $ reverse ds)) `div` (2 * n)
    where ds = (rowT @27750) n

instance OEIS 15 where
  oeis = 1 : concat do zipTail f $ oeis @961
    where
      f pp qq = replicate (fi (pp - qq)) pp

instance OEIS 21 where
  oeisIx n = genericLength [ () | k <- [1..2^n],
        sum [oeisIx @10052 (k - 12*y^2) | y <- [0..oeisIx @196 (k `div` 12)]] > 0]

instance OEIS 26 where
  oeisIx (succ->n) = f primes n 1 (0^ (n - 1)) 1 where
    f _  1 q e y  = y * e * q
    f ps'@ (p:ps) x q e y
      | m == 0    = f ps' x' p (e+1) y
      | e > 0     = f ps x q 0 (y * e * q)
      | x < p * p = f ps' 1 x 1 y
      | otherwise = f ps x 1 0 y
      where (x', m) = divMod x p

instance OEIS 28 where
  oeis = filter (odd . sum . map (oeisIx @120) . rowT @124010) [1..]

instance OEIS 31 where
  oeisIx = f
    where
      f 0 = 1
      f n = (`div` n) $ sum $
        zipWith (*) (map (oeisIx @10 . pred) divs) (map (oeisIx @79) $ reverse divs)
        where divs = (rowT @27750) n

instance OEIS 33 where
  oeisIx (succ->n) = sum $ map f [2..n]
    where f k = g k `div` h k
          g k = (-1)^k * n * fac (2*n - k - 1) * fac (n - k)
          h k = fac (2*n - 2*k) * fac (k - 2)
          fac = (oeisIx @142)

instance OEIS 37 where
  oeisIx (succ->n) = n + f (n + f n)
    where f = oeisIx @196

instance OEIS 50 where
  oeisIx n = foldl f 0 [1..2^n]
    where
      f i j | g j > 0 = i + 1 | let = i
      g k = foldl f 0 (h k)
        where f i y = g y + i
                where g y = oeisIx @10052 (k - y^2)
              h k = [0..oeisIx @196 k]

instance OEIS 51 where
  oeisIx = succ . oeisIx @79
  oeis   = iterate (subtract 1 . (* 2)) 2

instance OEIS 58 where
  oeisIx = f
    where
      f 0 = 2
      f n = f m ^ 2 - f m + 1 where m = n - 1
  oeis = iterate (oeisIx @2061) 2

instance OEIS 69 where
  oeis = [x | x <- [0..], odd $ oeisIx @120 x]

instance OEIS 70 where
  oeisIx = p (oeis @28310) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 71 where
  oeis = map (subtract 1) $ tail $ oeis @45

instance OEIS 81 where
  oeis = 0 : 1 : f 1 [1,0] where
   f x ys = y : f (x + 1) (y : ys) where
     y = sum (zipWith (*) (map h [1..x]) ys) `div` x
     h = sum . map (\d -> d * (oeisIx @81) d) . (rowT @27750)

instance OEIS 93 where
  oeisIx = oeisIx @196 . oeisIx @578

instance OEIS 96 where
  oeisIx n = n * (n + 3) `div` 2
-- (oeis @96) = [x | x <- [0..], (oeisIx @23531) x == 1]

instance OEIS 100 where
  oeis = f (tail $ oeis @45) [head $ oeis @45] where
   f (x:xs) ys = (sum . zipWith (*) ys $ oeis @73) : f xs (x:ys)

instance OEIS 106 where
  oeis = drop 2 $ conv (oeis @81) [] where
    conv (v:vs) ws = (sum $ zipWith (*) ws' $ reverse ws') : conv vs ws'
      where ws' = v : ws

instance OEIS 110 where
  oeisIx = sum . rowT @48993

instance OEIS 111 where
  oeisIx 0 = 1
  oeisIx n = sum $ (rowT @8280) (n - 1)

instance OEIS 116 where
  oeis = bis $ oeis @13 where bis (x:_:xs) = x : bis xs

instance OEIS 119 where
  oeisIx = p $ drop 2 $ oeis @45 where
    p _      0 = 1
    p (f:fs) m = if m < f then 0 else p fs (m - f) + p fs m

instance OEIS 124 where
  oeisIx = succ . oeisIx @217

instance OEIS 127 where
  oeisIx = sum . take 5 . rowT @7318

instance OEIS 141 where
  oeisIx 0 = 1
  oeisIx (pred->n) = 16 * (oeisIx @50470) n - 4 * (oeisIx @2173) n

instance OEIS 172 where
  oeisIx = sum . map (oeisIx @578) . rowT @7318

instance OEIS 193 where
  oeisIx = round . log . succ . fi
  oeis   = concat [ replicate n i
                  | n <- 1 : do zipTail (-) $ oeis @219092
                  | i <- [0..] ]

instance OEIS 208 where
  oeis = map (`div` 2) $ concat $ transpose
   [zipWith (+) (oeis @116) $ bis (oeis @116), bis $ tail (oeis @116)]
   where bis (x:_:xs) = x : bis xs

instance OEIS 216 where
  oeis = iterate (oeisIx @3132) 2

instance OEIS 218 where
  oeis = iterate (oeisIx @3132) 3

instance OEIS 221 where
  oeis = iterate (oeisIx @3132) 5

instance OEIS 246 where
  oeis' (A r) = 1 : 1 : zipWith (+) (tail r) (zipWith (*) r $ oeis @2378)

instance OEIS 267 where
  oeisIx = oeisIx @196 . oeisIx @16813

instance OEIS 292 where
  oeisIx n = n * (n + 1) * (n + 2) `div` 6
  oeis = scanl1 (+) $ oeis @217

instance OEIS 301 where
  oeisIx      = oeisIx @79 . oeisIx @45
  oeis' (A r) = 1 : scanl (*) 2 r

instance OEIS 325 where
  oeis = zipWith (-) (oeis @79) [0..]
  oeisIx n = 2 ^ n - n

instance OEIS 330 where
  oeis = scanl1 (+) $ oeis @290
  oeisIx n = n * (n + 1) * (2 * n + 1) `div` 6

instance OEIS 350 where
  oeis = map fi . elemIndices True $ zipWith (isSuffixOf `on` show) [0..] $ oeis @45

instance OEIS 379 where
  oeis = filter (even . sum . map (oeisIx @120) . rowT @124010) [1..]

instance OEIS 384 where
  oeisIx n = n * (2 * n - 1)
  -- oeis = scanl (+) 0 (oeis @16813)

instance OEIS 389 where
  oeis = 0 : 0 : f [] (oeis @217)
    where
      f xs (t:ts) = (sum $ zipWith (*) xs $ oeis @217) : f (t:xs) ts

instance OEIS 430 where
  oeis = m (oeis @40) (oeis @1248)
    where
      m (x:xs) (y:ys)
        | x < y = x : m xs (y:ys)
        | x > y = y : m (x:xs) ys

instance OEIS 433 where
  oeisIx 0 = 0
  oeisIx n = fi . read $ map intToDigit $ t n $ reverse $ takeWhile (<= n) $ tail $ oeis @578
    where
      t _ []          = []
      t m (x:xs)
          | x > m     = 0 : t m xs
          | otherwise = (fi m') : t r xs
          where (m',r) = divMod m x

instance OEIS 447 where
  oeis = (0 :) . scanl1 (+) $ oeis @16754

instance OEIS 462 where
  oeisIx (succ->fi->n) = fi . g [] n $ reverse $ takeWhile (<= n) $ tail (oeis @217)
    where
      g as 0 []     = read $ concat $ map show $ reverse as :: Integer
      g as x (t:ts) = g (a:as) r ts where (a,r) = divMod x t

instance OEIS 537 where
  oeisIx = oeisIx @290 . oeisIx @217

instance OEIS 540 where
  oeis = scanl1 (+) $ oeis @1014

instance OEIS 566 where
  oeisIx n = n * (5 * (n - 1) + 2) `div` 2
  -- oeis = scanl (+) 0 $ oeis @16861

instance OEIS 583 where
  oeisIx = (^ 4)
  -- oeis = scanl (+) 0 $ oeis @5917

instance OEIS 586 where
  oeisIx = p $ oeis @40 where
    p _ 0 = 1
    p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 587 where
  oeis =  1 : f (tabl @7318) [1] where
   f (bs:bss) xs = y : f bss (y : xs)
     where y = - sum (zipWith (*) xs bs)

instance OEIS 593 where
  oeisIx = sum . rowT @182469

instance OEIS 607 where
  oeisIx = p $ oeis @40 where
    p _      0 = 1
    p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 657 where
  oeisIx n = (rowCol @8280) (2 * n) n

instance OEIS 670 where
  oeis = 1 : f [1] (map tail $ tail (tabl @7318)) where
    f xs (bs:bss) = y : f (y : xs) bss where y = sum $ zipWith (*) xs bs

instance OEIS 697 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : tail (oeis @290))

instance OEIS 720 where
  oeis = scanl1 (+) $ oeis @10051

instance OEIS 726 where
  oeisIx n = p (oeis @1651) n where
     p _  0 = 1
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 734 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : (oeis @79))

instance OEIS 736 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : (oeis @108))

instance OEIS 737 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) [1..]

instance OEIS 738 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (oeis @45)

instance OEIS 744 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) $ tail (oeis @45)

instance OEIS 745 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) $ tail (oeis @290)

instance OEIS 746 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) $ tail (oeis @217)

instance OEIS 747 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) $ oeis @40

instance OEIS 752 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) $ oeis @79

instance OEIS 753 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) $ oeis @108

instance OEIS 754 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) [1, 3 ..]

instance OEIS 756 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : 1 : [0, 0 ..])

instance OEIS 788 where
  oeis = scanl1 (+) $ oeis @120
  oeisIx 0 = 0
  oeisIx n = (oeisIx @788) n2 + (oeisIx @788) (n - n2 - 1) + (n - n2)
    where n2 = n `div` 2

instance OEIS 925 where
  oeisIx n = sum $ map (oeisIx @10052 . (n -)) $ takeWhile (<= n) $ oeis @290

instance OEIS 957 where
  oeis = fix \r -> 0 : 1 : (map (`div` 2) $ tail $ zipWith (-) (oeis @108) r)

instance OEIS 961 where
  oeis = 1 : g (S.singleton 2) (tail primes)
    where
      g s (p:ps) = m : g (S.insert (m * (oeisIx @20639 . pred) m) $ S.insert p s') ps
        where
          (m, s') = S.deleteFindMin s

instance OEIS 984 where
  oeisIx (fi->n) = rowCol @7318 (2*n) n

-- instance OEIS 1006 where
--   oeisIx n = (oeis @1006) !! n
--  oeis = zipWith (+) (oeis @5043) $ tail (oeis @5043)

instance OEIS 1043 where
  oeis = zipTail (+) (oeis @40)

instance OEIS 1044 where
  oeis = fix \r -> 1 : zipWith (*) (tail (oeis @290)) r

instance OEIS 1045 where
  oeisIx = (`div` 3) . (+ 1) . oeisIx @79
  oeis   = fix \r -> 0 : 1 : zipWith (+) (map (2 *) r) (tail r)

instance OEIS 1088 where
  oeis = scanl1 (*) $ oeis @10

instance OEIS 1142 where
  oeisIx = product . rowT @7318

instance OEIS 1156 where
  oeisIx = p (tail $ oeis @290) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 1157 where
  oeisIx (succ->n) = s n 1 1 $ oeis @40 where
     s 1 1 y _          = y
     s m x y ps'@ (p:ps)
       | m `mod` p == 0 = s (m `div` p) (x * p^2) y ps'
       | x > 1          = s m 1 (y * (x * p^2 - 1) `div` (p^2 - 1)) ps
       | otherwise      = s m 1 y ps

instance OEIS 1177 where
  oeisIx (succ->n) = head [k | k <- [1..], oeisIx @45 k `mod` n == 0]

instance OEIS 1179 where
  oeisIx = f . succ
    where
      f 1 = 0
      f n = if p == n then ll (p `div` 24) 1 else f p
              where p = oeisIx @1175 (pred n)
                    ll x k = if x == 1 then k else ll (x `div` 5) (k + 1)

instance OEIS 1223 where
  oeis = zipWith (-) (tail $ oeis @40) $ oeis @40

instance OEIS 1248 where
  oeis = map (^ 2) $ oeis @40

instance OEIS 1250 where
  oeisIx n = if n < 2 then 1 else 2 * oeisIx @111 n

instance Table 1263 where
  tabl = zipWith dt (tabl @7318) (tail (tabl @7318)) where
     dt us vs = zipWith (-) (zipWith (*) us (tail vs)) (zipWith (*) (tail us ++ [0]) (init vs))

  rowT   = rowT_off   @1263 @1
  rowCol = rowCol_off @1263 @1 @1

instance OEIS 1263 where
  oeis = tablList @1263

instance OEIS 1274 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $ zipWith (-) (tail $ oeis @10) $ oeis @10

instance OEIS 1285 where
 oeis = map (+ 1) (oeis @10060)

instance OEIS 1299 where
  oeisIx = p [1,5,10,25] where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 1300 where
  oeisIx = p [1,5,10,25,50] where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 1316 where
  -- oeisIx = sum . (rowT @47999)
  oeis = 1 : zs where
     zs = 2 : (concat $ transpose [zs, map (* 2) zs])

instance OEIS 1318 where
  oeis = scanl1 (+) (oeis @26741)

instance OEIS 1358 where
  oeis = map (+1) $ filter ((== 2) . oeisIx @1222) [1..]

instance OEIS 1370 where
  oeisIx = (oeisIx @7953) . (oeisIx @79)

instance OEIS 1399 where
  oeisIx = p [1,2,3] where
     p _      0 = 1
     p []     _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 1405 where
  oeisIx (fi->n) = rowCol @7318 n (n `div` 2)

instance OEIS 1463 where
  oeis = scanl1 (+) $ oeis @1462

instance OEIS 1494 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $ zipWith (-) (drop 2 (oeis @10)) (oeis @10)

instance Table 1497 where
  tabl = [1] : f [1] 1 where
    f xs z = ys : f ys (z + 2) where
        ys = zipWith (+) ([0] ++ xs) (zipWith (*) [z, z - 1 ..] (xs ++ [0]))

instance OEIS 1497 where
  oeis = tablList @1497

instance Table 1498 where
  tabl = map reverse $ tabl @1497

instance OEIS 1498 where
  oeis = tablList @1498

instance OEIS 1511 where
  oeisIx (succ->n) = genericLength $ takeWhile ((== 0) . (mod n)) $ oeis @79
  -- oeisIx n | odd n = 1 | otherwise = 1 + (oeisIx @1511) (n `div` 2)

instance OEIS 1515 where
  oeisIx = sum . rowT @1497

instance OEIS 1521 where
  oeis = fix \r -> 1 : (map (oeisIx @196) $ zipWith (*) (map (* 2) r) (map (+ 1) r))

instance OEIS 1563 where
  oeis = zipWith (-) (tail $ oeis @142) $ oeis @142

instance OEIS 1602 where
  oeisIx n = fi . (+ 1) . fromJust $ findIndex ((== 0) . (`mod` (oeisIx @40) n)) $ tail $ oeis @45

instance OEIS 1611 where
  oeisIx = (+ 1) . oeisIx @45
  oeis = fix \r -> 1 : 2 : do map (subtract 1) $ zipTail (+) r

instance OEIS 1616 where
  oeisIx (succ->n) = sum $ map (oeisIx @10 . pred) $ zipWith gcd ds $ reverse ds
    where ds = (rowT @27750) n

instance OEIS 1629 where
  oeis = f [] $ tail $ oeis @45 where
     f us (v:vs) = (sum $ zipWith (*) us $ oeis @45) : f (v:us) vs

instance OEIS 1654 where
  oeis = zipWith (*) (tail (oeis @45)) (oeis @45)

instance OEIS 1697 where
  oeis = 1 : 1 : f [1,1] where
     f xs@ (x:_) = y : f (y : xs) where y = x * sum xs

instance OEIS 1700 where
  oeisIx (fi->n) = rowCol @7318 (2*n+1) (n+1)

instance OEIS 1715 where
  oeisIx = (flip div 6) . oeisIx @142 . (+3)

instance OEIS 1720 where
  oeisIx  = (flip div 24) . oeisIx @142 . (+4)

instance OEIS 1725 where
  oeisIx  = (flip div 120) . oeisIx @142 . (+5)

instance OEIS 1730 where
  oeisIx  = (flip div 720) . oeisIx @142 . (+6)

instance OEIS 1748 where
  oeisIx = (* 3) . oeisIx @40

instance OEIS 1749 where
  oeisIx = (* 4) . oeisIx @40

instance OEIS 1787 where
  oeisIx 0 = 0
  oeisIx n = n * 2 ^ (n - 1)
  oeis = zipWith (*) [0..] $ 0 : oeis @79

instance OEIS 1788 where
  oeisIx n = if n < 2 then n else n * (n + 1) * 2 ^ (n - 2)
  oeis = zipWith (*) (oeis @217) $ 1 : oeis @79

instance OEIS 1789 where
  -- oeisIx n = (oeisIx @7318) (fi n) 3 * 2 ^ (n - 3)
  oeis     = 1 : zipWith (+) (map (* 2) $ oeis @1789) (drop 2  $ oeis @1788)

instance OEIS 1836 where
  oeis = f (oeis @10) 1 where
     f (u:v:ws) x = if u < v then x : f ws (x + 1) else f ws (x + 1)

instance OEIS 1838 where
  oeis = map (fi . (+ 1)) $ elemIndices 2 $ zipWith (-) (drop 2 $ oeis @10) $ oeis @10

instance OEIS 1840 where
  oeis = scanl (+) 0 $ oeis @8620

instance OEIS 1844 where
  oeisIx n = 2 * n * (n + 1) + 1
  oeis = zipWith (+) (oeis @290) $ tail (oeis @290)

instance OEIS 1870 where
  oeis = uncurry c $ splitAt 1 $ tail (oeis @45) where
     c us vs'@ (v:vs) = (sum $ zipWith (*) us vs') : c (v:us) vs

instance OEIS 1923 where
  oeis = scanl (+) 0 $ tail $ oeis @312

instance OEIS 1924 where
  oeis = drop 3 $ zipWith (-) (tail $ oeis @45) [0..]

instance OEIS 1950 where
  oeisIx n = oeisIx @201 n + n + 1

instance OEIS 1969 where
  oeis = [x | x <- [0..], even $ oeisIx @120 x]

instance Table 2024 where
  tabl   = iterate (\xs@ (x:_) -> map (+ 1) (x : xs)) [1]
  rowT   = rowT_off   @2024 @1
  rowCol = rowCol_off @2024 @1 @1

instance OEIS 2062 where
  oeisIx n = oeisIx @45 n + n
  oeis = fix \r -> 0 : 2 : 3 : (map (subtract 1) $ zipWith (-) (map (* 2) $ drop 2 r) r)

instance OEIS 2088 where
  oeis = scanl (+) 0 $ oeis @10

instance OEIS 2109 where
  oeis = scanl1 (*) $ oeis @312

instance OEIS 2110 where
  oeisIx n = product . genericTake n $ oeis @40
  oeis = scanl (*) 1 $ oeis @40

instance OEIS 2144 where
  oeis = map succ $ filter ((== 1) . oeisIx @10051) [0,4..]

instance OEIS 2173 where
  oeisIx n = (oeisIx @50450) n - (oeisIx @50453 n)

instance OEIS 2183 where
  -- oeis = nub $ map (oeisIx @5 . pred . oeisIx @61799) [1..]
  oeis = 1 : unfoldr f (1, map (oeisIx @5 . pred) (oeis @61799))
    where
      f (m, (dropWhile (m>=)->(x:xs)))
        = Just (x, (x, xs))

instance Table 2260 where
  tabl       = iterate (\row -> map (+ 1) (0 : row)) [1]
  rowT   n   = [1..n]
  rowCol _ k = k

instance OEIS 2260 where
  oeis = tablList @2260

instance Table 2262 where
  tabl = map (enumFromTo 0) [0..]

instance OEIS 2262 where
  oeis = tablList @2262

instance OEIS 2311 where
  -- oeis = filter f [1..] where
  --    f x = not $ null $ intersect txs $ map (tx -) $ txs where
  --        (txs,tx:_) = splitAt (fi x) $ oeis @292
  oeis =
    [ i
    | (i,tx,txs) <- ap (zip3 [0..]) inits $ oeis @292
    , not $ null $ intersect txs $ map (tx -) $ txs
    ]

instance OEIS 2313 where
  oeis = filter ((`elem` [1,2]) . (`mod` 4)) $ oeis @40

instance OEIS 2321 where
  oeis = scanl1 (+) (oeis @8683)

instance OEIS 2324 where
  oeisIx n = oeisIx @1817 n - oeisIx @1822 n

instance OEIS 2326 where
  oeisIx n = fi . (+ 1) $ fromJust $ findIndex ((== 0) . (`mod` (2 * n + 1))) $ tail $ oeis @225

instance OEIS 2411 where
  oeisIx n = n * oeisIx @217 n

instance OEIS 2450 where
  -- oeisIx = (`div` 3) . (oeisIx @24036)
  oeis = iterate ((+ 1) . (* 4)) 0

instance OEIS 2476 where
  oeis = filter ((== 1) . (`mod` 6)) $ oeis @40

instance OEIS 2635 where
  oeisIx = p (tail $ oeis @290) 4 where
    p ks'@ (k:ks) c m = if m == 0 then 1 else
      if c == 0 || m < k then 0 else p ks' (c - 1) (m - k) + p ks c m

instance OEIS 2662 where
  oeis = map (sum . drop 3) (tabl @7318)

instance OEIS 2663 where
  oeis = map (sum . drop 4) (tabl @7318)

instance OEIS 2664 where
  oeis = map (sum . drop 5) (tabl @7318)

instance OEIS 2782 where
 oeis = tail $ f 1 1 (map fi $ tail (oeis @7376)) where
     f x y (d:ds) | mod y x == 0 = y : f y d ds
                  | otherwise    = f x (10*y + d) ds

instance OEIS 2796 where
  oeis = filter f [1..] where
     f x = all ((== 0) . mod x) ds where
       ds = map (fi.digitToInt) (if c == '0' then cs else cs')
       cs'@ (c:cs) = nub $ sort $ (show :: Int -> String) (fi x)

instance OEIS 2808 where
  oeis = map (+1) $ filter ((== 1) . (oeisIx @66247)) [2..]

instance OEIS 2814 where
  oeis = 1 : zipWith div (tail xs) xs
     where xs = map (oeisIx @45) (oeis @244)

instance OEIS 2822 where
  oeis = f $ oeis @40 where
     f (q:ps'@ (p:ps)) | p > q + 2 || r > 0 = f ps'
                      | otherwise = y : f ps where (y,r) = divMod (q + 1) 6

instance OEIS 2878 where
  oeis = zipWith (+) (tail $ oeis @1906) $ oeis @1906

instance OEIS 2940 where
  oeis = 1 : 4 : 11 : zipWith (+)
     (zipWith (-) (map (* 2) $ drop 2 (oeis @2940)) (oeis @2940))
     (drop 5 (oeis @45))

instance OEIS 2941 where
  oeis = 1 : 7 : 29 : zipWith (+)
     (zipWith (-) (map (* 2) $ drop 2 (oeis @2941)) (oeis @2941))
     (drop 2 $ zipWith (+) (tail (oeis @2940)) (oeis @2940))

instance OEIS 2942 where
  oeisIx = (oeisIx @4086) . (oeisIx @290) . succ

instance OEIS 2965 where
  oeis = concat $ transpose [oeis @129, (oeis @1333)]

instance OEIS 2984 where
  oeis = iterate (\x -> x + oeisIx @196 x) 1

instance OEIS 2993 where
  oeisIx = (oeisIx @30) . (oeisIx @290)

instance OEIS 2994 where
  oeisIx = (oeisIx @30) . (oeisIx @578)

instance OEIS 3016 where
  oeisIx n = fi . sum $ map (fromEnum . (== n)) $
                        concat $ genericTake (fi n + 1) (tabl @7318)

instance OEIS 3044 where
  oeis = 1 : 2 : 3 : 4 : f [4,3..1] where
     f xs@ (x:_) = y : f (y : xs) where
       y = head [w | w <- [x + 1 ..],
           length [ () | v <- xs, (w - v) `elem` dropWhile (>= v) xs] == 2]

instance OEIS 3046 where
  oeis = scanl1 (*) (oeis @108)

instance Table 3056 where
  tabl   = map (rowT @3056) [0..]
  rowT n = genericReplicate (n + 1) n

instance OEIS 3056 where
  oeisIx = fi . floor . (/ 2) . (subtract 1) . sqrt . (+ 1) . (* 8) . fi
  oeis   = tablList @3056
instance OEIS 3108 where
  oeisIx = p $ tail (oeis @578) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 3148 where
  oeis = 1 : 1 : zipWith (+) (tail (oeis @3148))
                            (zipWith (*) (tail (oeis @2943)) (oeis @3148))

instance OEIS 3168 where
  oeisIx 0 = 1
  oeisIx n = sum (zipWith (*)
     (tail $ (tabl @7318) !! fi n)
     ((transpose $ take (3 * fi n + 1) (tabl @7318)) !! (2 * fi n + 1)))
     `div` fi n

instance OEIS 3233 where
  oeis = [x+1 | x <- [0..], (oeisIx @3231 . pred) (oeisIx @1950 x) == (oeisIx @1950 . pred) (oeisIx @3231 x)]

instance OEIS 3234 where
  oeis = [x+1 | x <- [1..], (oeisIx @3231 . pred) (oeisIx @1950 x) == (oeisIx @1950 . pred) (oeisIx @3231 x) - 1]

instance OEIS 3238 where
  oeis = 1 : f 1 where
     f x = (sum (map (oeisIx @3238 . pred) $ (rowT @27750) x)) : f (x + 1)

instance OEIS 3249 where
  oeisIx = (+ 1) . (oeisIx @1950 . pred) . (oeisIx @3234)

instance OEIS 3266 where
  oeis = (1 :) . scanl1 (*) $ tail (oeis @45)

instance OEIS 3275 where
  oeisIx = (oeisIx @10) . fi . (oeisIx @1274)

instance OEIS 3415 where
--   oeisIx 0 = 0
  oeisIx n = ad n (oeis @40) where
    ad n _ | n < 2     = 0
    ad n ps'@ (p:ps)
       | n < p * p     = 1
       | r > 0         = ad n ps
       | otherwise     = n' + p * ad n' ps' where
         (n',r) = divMod n p

instance OEIS 3422 where
  oeis = scanl (+) 0 (oeis @142)

instance OEIS 3434 where
  oeisIx n = fst $ until ((== 1) . snd)
                          (\ (i, x) -> (i + 1, (oeisIx @10 . pred) x)) (0, succ n)

instance Table 3506 where
  tabl   = scanl1 (\xs ys -> zipWith (+) (zipWith (+) ([0] ++ xs) (xs ++ [0])) ys) (tabl @7318)
  rowT   = rowT_off   @3506 @1
  rowCol = rowCol_off @3506 @1 @1

instance OEIS 3506 where
  oeis = tablList @3506

instance OEIS 3622 where
  oeis = filter ((elem 1) . (rowT @35516)) [1..]

instance OEIS 3627 where
  -- oeisIx n = (oeis @3627) !! (n - 1)
  oeis = filter ((== 2) . (`mod` 3)) (oeis @40)

instance OEIS 3817 where
--   oeisIx n = if n == 0 then 0 else 2 * (oeisIx @53644) n - 1
  oeis = map fi (scanl (.|.) 0 [1..] :: [Integer])

instance Table 3983 where
  tabl = map (rowT @3983) [1..]
  rowT n = hs ++ drop (fi m) (reverse hs)
     where hs = [1..n' + m]
           (n',m) = divMod n 2
  rowCol = rowCol_off @3983 @1 @1

instance OEIS 3983 where
  oeis = tablList @3983

instance Table 3986 where
  rowCol (fi->n) (fi->k) = fi $ ((n - k) .|. k :: Int)
  rowT n = map (rowCol @3986 n) [0..n]
  tabl = map (rowT @3986) [0..]

instance OEIS 3986 where
  oeis = tablList @3986

instance Table 3988 where
  rowCol n k = fi $ (n + 1 - k) `div` k
  rowT n = zipWith div [n,n - 1..1] [1..n]
  tabl = map (rowT @3988) [1..]

instance OEIS 3988 where
  oeis = tablList @3988

instance OEIS 3995 where
  oeis = filter (p (oeis @290)) [0..]
     where p (q:qs) m = m == 0 || q <= m && (p qs (m - q) || p qs m)

instance OEIS 4006 where
  oeisIx 0 = 0
  oeisIx (pred->n) = (oeisIx @292) n + n + 1

instance OEIS 4019 where
  oeis = iterate (oeisIx @290 . (+ 1)) 0

instance OEIS 4087 where
  oeis = map (oeisIx @4086) (oeis @40)

instance OEIS 4090 where
  oeisIx = (oeisIx @7953) . (oeisIx @45)

instance OEIS 4091 where
  oeisIx = (oeisIx @4086) . (oeisIx @45)

instance OEIS 4093 where
  oeisIx = (oeisIx @4086) . (* 2)

instance OEIS 4094 where
  oeisIx = (oeisIx @4086) . (oeisIx @79)

instance OEIS 4111 where
  oeis = 0 : 1 : f 1 [1] where
     f x zs = y : f (x + 1) (y : zs) where
              y = (sum $ zipWith (*) zs $ map g [1..]) `div` x
     g k = sum $ zipWith (*) (map (((-1) ^) . (+ 1)) $ reverse divs)
                             (zipWith (*) divs $ map (oeisIx @4111) divs)
                             where divs = (rowT @27750) k

instance OEIS 4148 where
  oeis = 1 : f [1] where
    f xs'@ (x:xs) = y : f (y : xs') where
      y = x + sum (zipWith (*) xs $ reverse $ tail xs)

instance OEIS 4154 where
  oeisIx = (oeisIx @4151 . pred) . (oeisIx @142)
  -- oeis = scanl (\u v -> (oeisIx @4151) $ u * v) 1 [1..]

instance OEIS 4159 where
  oeisIx = (oeisIx @7953) . (oeisIx @290)

instance OEIS 4170 where
  oeis = 0 : 1 : f (S.fromList us) vs where
     f s (x:xs) = m : f (S.insert x s') xs
       where (m,s') = S.deleteFindMin s
     (us,vs) = splitAt 120 $ drop 2 $ oeis @4091

instance OEIS 4171 where
  oeisIx = (* 2) . (oeisIx @302)
  oeis = iterate (* 4) 2

instance Table 4197 where
  rowT n = hs ++ drop (1 - fi n `mod` 2) (reverse hs) where hs = [0..n `div` 2]
  tabl = map (rowT @4197) [0..]

instance OEIS 4197 where
  oeis = tablList @4197

instance OEIS 4432 where
  oeis = filter (p 3 $ tail (oeis @290)) [1..] where
     p k (q:qs) m = k == 0 && m == 0 ||
                    q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

instance OEIS 4433 where
  oeis = filter (p 4 $ tail (oeis @290)) [1..] where
     p k (q:qs) m = k == 0 && m == 0 ||
                    q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

instance OEIS 4434 where
  oeis = filter (p 5 $ tail (oeis @290)) [1..] where
     p k (q:qs) m = k == 0 && m == 0 ||
                    q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

instance OEIS 4442 where
  oeisIx = fi . (xor 1 :: Integer -> Integer) . fi
--   oeis = concat $ transpose [oeis, (oeis @5843)]

instance OEIS 4648 where
  oeis = zipWith mod (oeis @40) [1..]

instance Table 4736 where
  rowCol n k = n - k + 1
  rowT = rowT @4736
  tabl = map reverse (tabl @2260)

instance OEIS 4736 where
  oeis = tablList @4736

instance OEIS 5013 where
  oeis = alt (oeis @45) (oeis @32) where
     alt (f:_:fs) (_:l:ls) = f : l : alt fs ls

instance OEIS 5041 where
  oeis = 1 : f 1 1 (tail ts) where
     f y i gs'@ ((j,a):gs) | i < j  = y : f y (i+1) gs'
                          | i == j = a : f a (i+1) gs
     ts = [ (6*k + 3*k* (k - 1) `div` 2 + r* (k+2), 3*k+r+1) |
           k <- [0..], r <- [0,1,2]]

instance OEIS 5044 where
  oeisIx = p [2,3,4] . (subtract 3) where
    p _ 0 = 1
    p [] _ = 0
    p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 5087 where
  oeisIx (succ->n) = (oeisIx @1221 . pred) n + n `mod` 2 - 1

instance OEIS 5165 where
  oeis = 0 : zipWith (-) (tail (oeis @142)) (oeis @5165)

instance OEIS 5171 where
  oeisIx = (1 -) . (oeisIx @10051)

instance OEIS 5179 where
  oeisIx (succ->n) = fi . succ $ fromJust $ elemIndex n $ map (oeisIx @5) [0..]

instance OEIS 5238 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $ zipWith (+) ds $ tail ds where
     ds = map abs $ zipWith (-) (tail (oeis @5)) (oeis @5)

instance OEIS 5247 where
  oeis = f (oeis @32) (oeis @45) where
     f (x:_:xs) (_:y:ys) = x : y : f xs ys

instance OEIS 5250 where
  oeis = f 0 (oeis @1223)
     where f m (x:xs) = if x <= m then f m xs else x : f x xs

instance OEIS 5252 where
  oeisIx (fi->n) = sum $ map (\x -> rowCol @7318 (n - x) x) [0, 2 .. 2 * div n 4]

instance OEIS 5258 where
  oeisIx (fi->n) = sum [rowCol @7318 n k ^ 2 * rowCol @7318 (n + k) k | k <- [0..n]]

instance OEIS 5352 where
  oeisIx = (oeisIx @5351) . negate . succ

instance OEIS 5361 where
  oeisIx 0 = 1
  oeisIx n = product . (rowT @124010) . succ $ n

instance OEIS 5369 where
  oeisIx = (oeisIx @10052) . (+ 1) . (* 4)

instance OEIS 5732 where
  oeis = 1 : 8 : f (drop 5 (tabl @7318)) where
     f (us:pss@ (vs:_:ws:_)) = (us !! 5 + vs !! 5 + ws !! 6) : f pss

instance OEIS 5809 where
  oeisIx (fi->n) = rowCol @7318 (3*n) n

instance OEIS 5810 where
  oeisIx (fi->n) = rowCol @7318 (4*n) n

instance OEIS 6003 where
  oeisIx n = n * (n ^ 2 + 1) `div` 2
  oeis = scanl (+) 0 (oeis @5448)

instance OEIS 6013 where
  oeisIx (fi->n) = fi $ rowCol @7318 (3 * n + 1) n `div` (n + 1)
  -- oeisIx' n = (oeisIx @258708) (2 * n + 1) n

instance OEIS 6049 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $
     zipWith (-) (tail (oeis @1221)) (oeis @1221)

instance OEIS 6261 where
  oeisIx = sum . take 6 . rowT @7318

instance OEIS 6364 where
  oeis = filter (even . (oeisIx @120). (`div` 2)) [0..]

instance OEIS 6463 where
  oeis = 0 : scanl1 (+) (oeis @3056)

instance OEIS 6478 where
  oeis = scanl1 (+) $ drop 2 (oeis @1629)

instance OEIS 6508 where
  oeis = iterate (oeisIx @2808 . pred) 1

instance OEIS 6899 where
  oeis = 1 : m (tail (oeis @79)) (tail (oeis @244)) where
     m us'@ (u:us) vs'@ (v:vs) = if u < v then u : m us vs' else v : m us' vs

instance OEIS 6918 where
  oeis = scanl (+) 0 (oeis @8805)

instance OEIS 7097 where
  oeis = iterate (oeisIx @40.pred) 1

instance OEIS 7290 where
  oeisIx (fi->n) = if n < 3 then 0 else 2 * rowCol @7318 n 3

instance OEIS 7294 where
  oeisIx = p $ tail (oeis @217) where
     p _      0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 7302 where
  oeisIx (fi->n) = fi do (oeisIx @120) $ xor n (3 * n) :: Integer

instance Table 7318 where
  -- tabl = iterate (\row -> zipWith (+) (0 : row) (row ++ [0])) [1]
  tabl = binomial

instance OEIS 7318 where
  oeis = tablList @7318

instance OEIS 7406 where
  oeis = map numerator $ scanl1 (+) $ map (1 %) $ tail (oeis @290)

instance OEIS 7407 where
  oeis = map denominator $ scanl1 (+) $ map (1 %) $ tail (oeis @290)

instance OEIS 7421 where
  oeisIx = (2 -) . (`mod` 2) . (oeisIx @1222)

instance OEIS 7423 where
  oeisIx = (+ 1) . (oeisIx @8683)

instance OEIS 7489 where
  oeis = scanl (+) 0 $ tail (oeis @142)

instance OEIS 7501 where
  oeis = iterate (oeisIx @217) 2

instance OEIS 7502 where
  oeis = zipWith (+) (oeis @45925) $ tail (oeis @45)

instance OEIS 7504 where
  oeis = scanl (+) 0 (oeis @40)

instance OEIS 7510 where
  oeis = map (fi . (+ 1)) $ elemIndices (0, 1, 0) $
    zip3 (drop 2 (oeis @10051)) (oeis @10051) (0 : 0 : (oeis @10051))

instance OEIS 7513 where
  oeisIx (succ->n) = (oeis @40) !! (fromJust $ elemIndex 0 $
     zipWith ((-) `on` (oeisIx @7953)) (oeis @40) $ drop (fi n - 1) (oeis @40))

instance OEIS 7519 where
  oeis = map succ $ filter ((== 1) . (oeisIx @10051)) [0,8..]

instance OEIS 7522 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @4771)

instance OEIS 7559 where
  oeis = scanl (*) 1 (oeis @16777)

instance OEIS 7583 where
  oeisIx = (`div` 3) . (+ 1) . (oeisIx @4171)

instance OEIS 7598 where
  oeisIx = (^ 2) . (oeisIx @45)

instance OEIS 7604 where
  oeis = concat $ map tail $ tail (tabl @46936)

instance OEIS 7605 where
  oeis = map (oeisIx @7953) (oeis @40)

instance OEIS 7675 where
  oeis = f 1 (oeis @8966) where
     f n (u:xs'@ (v:w:x:xs)) | u == 1 && w == 1 && v == 1 = n : f (n+4) xs
                            | otherwise = f (n+1) xs'

instance OEIS 7805 where
  oeisIx = (`div` 2) . (oeisIx @45) . (* 3) . (+ 1) . (* 2)

instance OEIS 7895 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ rowT @35516 n

instance OEIS 7916 where
  oeis = filter ((== 1) . foldl1 gcd . (rowT @124010)) [2..]

instance OEIS 7955 where
  oeisIx = product . (rowT @27750) . succ

instance OEIS 8280 where
  oeis = tablList @8280
instance Table 8280 where
  tabl = ox True (tabl @8281) where
    ox turn (xs:xss) = (if turn then reverse xs else xs) : ox (not turn) xss

instance OEIS 8544 where
  oeis = scanl (*) 1 (oeis @16789)

instance OEIS 8545 where
  oeis = scanl (*) 1 (oeis @4767)

instance OEIS 8578 where
  oeis = 1 : (oeis @40)

instance OEIS 8805 where
  oeisIx = (oeisIx @217) . (`div` 2) . (+ 2)
  oeis = drop 2 $ concat $ transpose [oeis @217, oeis @217]

instance OEIS 8859 where
  oeisIx = sum . take 7 . rowT @7318 . fi

instance OEIS 8860 where
  oeisIx = sum . take 8 . rowT @7318 . fi

instance OEIS 8861 where
  oeisIx = sum . take 9 . rowT @7318 . fi

instance OEIS 8862 where
  oeisIx = sum . take 10 .rowT @7318 . fi

instance OEIS 8863 where
  oeisIx = sum . take 11 .rowT @7318 . fi

instance OEIS 8963 where
  oeisIx = (oeisIx @30) . (oeisIx @45)

instance OEIS 8966 where
  oeisIx = abs . (oeisIx @8683)

instance OEIS 10051 where
  oeis = unfoldr ch (1, (oeis @40)) where
     ch (i, ps'@ (p:ps))
       = Just (fi $ fromEnum (i == p), (i + 1, if i == p then ps else ps'))
  oeisIx = fi . fromEnum . isPrime . fi . succ

instance OEIS 10052 where
  oeis     = concat (iterate (\xs -> xs ++ [0,0]) [1])
  oeisIx n = fi . fromEnum $ oeisIx @196 n ^ 2 == n

instance OEIS 10888 where
  oeisIx = until (< 10) (oeisIx @7953)

instance OEIS 11371 where
  oeisIx n = n - (oeisIx @120) n

instance OEIS 11772 where
  oeisIx (succ->n) = fi . (+ 1) $ fromJust $
     findIndex ((== 0) . (`mod` n)) $ tail (oeis @217)

instance OEIS 11848 where
  oeisIx (fi->n) = if n < 2 then 0 else flip div 2 $ rowCol @7318 n 2

instance OEIS 13589 where
  oeis = iterate (oeisIx @217) 4

instance OEIS 13929 where
  oeis = map (fi . (+1)) $ filter ((== 0) . (oeisIx @8966)) [1..]

instance OEIS 14105 where
  oeisIx n = n * (2 * n + 1)
  oeis = scanl (+) 0 (oeis @4767)

instance OEIS 14284 where
  oeis = scanl1 (+) (oeis @8578)

instance OEIS 14499 where
  oeisIx = (oeisIx @120) . (oeisIx @40)

instance OEIS 16754 where
  oeis = scanl (+) 1 $ tail (oeis @8590)

instance OEIS 20914 where
  oeisIx = (oeisIx @70939) . (oeisIx @244)

instance OEIS 20915 where
  oeisIx = (oeisIx @81604) . (oeisIx @79)

instance OEIS 20986 where
  oeis = scanl1 (+) (oeis @20985)

instance OEIS 20987 where
  oeisIx = (`div` 2) . (1 -) . (oeisIx @20985)

instance OEIS 20990 where
  oeis = scanl1 (+) $ zipWith (*) (oeis @33999) (oeis @20985)

instance OEIS 22155 where
  oeis = map fi $ elemIndices (- 1) (oeis @20985)

instance OEIS 22290 where
  oeisIx 0 = 0
  oeisIx n = h n 0 $ drop 2 (oeis @45) where
     h 0 y _      = y
     h x y (f:fs) = h x' (y + f * r) fs where (x',r) = divMod x 2

instance OEIS 22340 where
  oeisIx = (* 2) . (oeisIx @3714)

instance OEIS 22342 where
  oeis = filter ((notElem 1) . (rowT @35516)) [0..]

instance OEIS 22449 where
  oeisIx = (oeisIx @2808 . pred) . (oeisIx @8578)
  oeis = map (oeisIx @2808 . pred) (oeis @8578)

instance OEIS 22998 where
  oeisIx n = (oeisIx @34) (n + 1) * n
  oeis = zipWith (*) [0..] $ tail (oeis @34)

instance OEIS 23201 where
  oeis = filter ((== 1) . (oeisIx @10051) . (+ 5)) (oeis @40)

instance OEIS 23431 where
  oeis = 1 : 1 : f [1,1] where
     f xs'@ (x:_:xs) = y : f (y : xs') where
       y = x + sum (zipWith (*) xs $ reverse $ xs')

instance OEIS 23432 where
  oeis = 1 : 1 : f [1,1] where
     f xs'@ (x:_:xs) = y : f (y : xs') where
       y = x + sum (zipWith (*) xs $ reverse $ tail xs)

instance OEIS 23895 where
  oeisIx = p (oeis @2808) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 24023 where
  oeisIx = subtract 1 . (oeisIx @244)

instance OEIS 24036 where
  oeisIx = (subtract 1) . (oeisIx @302)
  oeis = iterate ((+ 3) . (* 4)) 0

instance OEIS 24450 where
  oeis = scanl1 (+) (oeis @1248)

instance OEIS 24483 where
  oeisIx (succ->n) = (rowCol @51631) (2* n) n

instance OEIS 24697 where
  oeis = f (tail (oeis @40)) [head (oeis @40)] 2 where
     f (p:ps) qs k = sum (take (div k 2) $ zipWith (*) qs $ reverse qs) :
                     f ps (p : qs) (k + 1)

instance OEIS 24940 where
  oeisIx = p $ tail (oeis @217) where
     p _  0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 25065 where
  oeisIx = p (1:[2,4..]) where
     p [] _ = 0
     p _  0 = 1
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 25129 where
  oeis= f (tail (oeis @40)) [head (oeis @40)] 1 where
     f (p:ps) qs k = sum (take (div k 2) $ zipWith (*) qs $ reverse qs) :
                     f ps (p : qs) (k + 1)

instance OEIS 25284 where
  oeis = map fi $ elemIndices 1 (oeis @25426)

instance OEIS 25285 where
  oeis = filter ((== 2) . (oeisIx @25426)) [1..]

instance OEIS 25286 where
  oeis = filter ((== 3) . (oeisIx @25426)) [1..]

instance OEIS 25294 where
  oeis = filter ((> 2) . (oeisIx @25426)) [1..]

instance OEIS 25302 where
  oeis = [x | x <- [1..], (oeisIx @25441) x == 1]

instance OEIS 25414 where
  oeisIx = fi . fromJust . (`elemIndex` (oeis @25427)) . succ

instance OEIS 25426 where
  oeisIx n = sum $ map (oeisIx @10052 . (n -)) $
                        takeWhile (<= n `div` 2) $ tail (oeis @290)
  oeis = map (oeisIx @25426) [0..]

instance OEIS 25427 where
  oeisIx n = sum $ map f zs where
     f x = sum $ map (oeisIx @10052 . (n - x -)) $
                     takeWhile (<= div (n - x) 2) $ dropWhile (< x) zs
     zs = takeWhile (< n) $ tail (oeis @290)

instance OEIS 25435 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @10052) n + sum
     (map (oeisIx @10052 . (n -)) $ takeWhile (< n `div` 2) $ tail (oeis @290))

instance OEIS 25441 where
  oeisIx n = sum $ map (oeisIx @10052 . (n -)) $
                        takeWhile (< n `div` 2) $ tail (oeis @290)

instance OEIS 25473 where
  oeisIx = (oeisIx @20639) . pred . (oeisIx @961)

instance OEIS 25474 where
  oeisIx = (oeisIx @1222) . pred . (oeisIx @961)

instance OEIS 25543 where
  oeis = scanl lcm 1 (oeis @2808)

instance OEIS 25555 where
  oeis = scanl1 lcm $ tail (oeis @217)

instance Table 27748 where
  rowCol = rowCol_off @27748 @1 @1
  tabl = rowT @27748 <$> [1..]
  rowT 1 = [1]
  rowT n = unfoldr fact n where
     fact 1 = Nothing
     fact x = Just (p, until ((> 0) . (`mod` p)) (`div` p) x)
              where p = (oeisIx @20639) $ pred x

instance OEIS 27748 where
  oeis = 1 : (map (fi.fst) do factorise =<< [2..])

instance OEIS 28905 where
  oeisIx = (oeisIx @4185) . (oeisIx @40)

instance OEIS 30229 where
--   oeisIx n = (oeis @30229) !! (n - 1)
  oeis = map (fi . succ) $ elemIndices 1 (oeis @8683)

instance OEIS 31287 where
  oeis = tail $ map fi $ elemIndices 0 (oeis @7376)

instance OEIS 31288 where
  oeis = map fi $ elemIndices 1 (oeis @7376)

instance OEIS 31289 where
  oeis = map fi $ elemIndices 2 (oeis @7376)

instance OEIS 31290 where
  oeis = map fi $ elemIndices 3 (oeis @7376)

instance OEIS 31291 where
  oeis = map fi $ elemIndices 4 (oeis @7376)

instance OEIS 31292 where
  oeis = map fi $ elemIndices 5 (oeis @7376)

instance OEIS 31293 where
  oeis = map fi $ elemIndices 6 (oeis @7376)

instance OEIS 31294 where
  oeis = map fi $ elemIndices 7 (oeis @7376)

instance OEIS 31295 where
  oeis = map fi $ elemIndices 8 (oeis @7376)

instance OEIS 31296 where
  oeis = map fi $ elemIndices 9 (oeis @7376)

instance OEIS 31347 where
  oeisIx = until (< 10) (oeisIx @7954)

instance OEIS 33264 where
  oeisIx = f 0 . (rowT @30308) . succ where
     f c [] = c
     f c (0 : 1 : bs) = f (c + 1) bs
     f c (_ : bs) = f c bs

instance OEIS 33860 where
  oeis = iterate (oeisIx @70196) 1

instance OEIS 33942 where
  oeis = map (+1) $ filter ((> 2) . (oeisIx @1222)) [1..]

instance OEIS 33992 where
  oeis = map (+1) $ filter ((== 3) . (oeisIx @1221)) [1..]

instance OEIS 34690 where
  oeisIx = sum . map (oeisIx @7953) . (rowT @27750) . succ

instance OEIS 34953 where
  oeis = map (oeisIx @217) (oeis @40)

instance OEIS 35038 where
  oeis = map (sum . drop 6)  (tabl @7318)

instance OEIS 35039 where
  oeis = map (sum . drop 7)  (tabl @7318)

instance OEIS 35040 where
  oeis = map (sum . drop 8)  (tabl @7318)

instance OEIS 35041 where
  oeis = map (sum . drop 9)  (tabl @7318)

instance OEIS 35042 where
  oeis = map (sum . drop 10) (tabl @7318)

instance OEIS 35516 where
  oeis = tablList @35516
instance Table 35516 where
  tabf = map (rowT @35516) [0..]
  rowT 0 = [0]
  rowT n = z n $ reverse $ takeWhile (<= n) (oeis @45) where
     z 0 _              = []
     z x (f:fs'@ (_:fs)) = if f <= x then f : z (x - f) fs else z x fs'

instance OEIS 36263 where
  oeis = zipWith (-) (tail (oeis @1223)) (oeis @1223)

instance OEIS 36289 where
  oeisIx n = n * 2 ^ n
  oeis = zipWith (*) [0..] (oeis @79)

instance OEIS 36555 where
  oeisIx = (oeisIx @120) . (* 3)

instance OEIS 36988 where
  oeisIx = (oeisIx @63524) . (oeisIx @36989)

instance OEIS 36990 where
  oeis = filter ((== 1) . (oeisIx @36989)) [0..]

instance OEIS 36991 where
  oeis = filter ((p 1) . (rowT @30308)) [0..] where
     p _    [_]    = True
     p ones (0:bs) = ones > 1 && p (ones - 1) bs
     p ones (1:bs) = p (ones + 1) bs

instance OEIS 36992 where
  oeis = c (oeis @36990) (tail (oeis @36991)) [0..] where
     c us'@ (u:us) vs'@ (v:vs) (w:ws)
       | w == u    = c us vs' ws
       | w == v    = c us' vs ws
       | otherwise = w : c us' vs' ws

instance OEIS 36993 where
  oeis = filter ((p 0) . (rowT @30308)) [0..] where
     p _     []     = True
     p zeros (0:bs) = p (zeros + 1) bs
     p zeros (1:bs) = zeros > 1 && p (zeros - 1) bs

instance OEIS 36994 where
  oeis = filter ((p 0) . (rowT @30308)) [1, 3 ..] where
     p ones []    = ones > 0
     p ones (0:bs) = ones > 1 && p (ones - 1) bs
     p ones (1:bs) = p (ones + 1) bs

instance OEIS 37015 where
  oeis = filter (all (> 0) . ds) [0..] where
     ds x = zipWith (-) (tail gs) gs where
        gs = map length $ group $ (rowT @30308) x

instance OEIS 37201 where
  oeis = f (oeis @1223) where
     f (x:xs@ (x':_)) | x == x'   = f xs
                     | otherwise = x : f xs

instance OEIS 38664 where
  oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @1223)) . (* 2) . succ

instance OEIS 39995 where
  oeisIx n
     = fi . sum
     . map (oeisIx @10051 . pred)
     . filter (>0)
     . nub
     . map (read :: String -> Integer)
     . tail
     . subsequences
     . show . fi . succ $ n

instance OEIS 45776 where
  oeis = iterate f 1 where
     f x = head $ dropWhile (<= x) [q,2*q..] where q = (oeisIx @7953) x

instance OEIS 45925 where
  oeis = zipWith (*) [0..] (oeis @45)

instance OEIS 46523 where
  oeisIx = product . zipWith (^) (oeis @40) . reverse . sort . (rowT @124010) . succ

instance OEIS 46933 where
  oeis = map (subtract 1) (oeis @1223)

instance OEIS 46935 where
  oeis = concatMap tail $ tail (tabl @46934)

instance OEIS 48158 where
  oeis = tablList @48158
instance Table 48158 where
  rowCol = mod
  rowT   = rowT_off @48158 @1
  tabl   = zipWith (map . mod) [1..] (tabl @2260)

instance OEIS 49001 where
  oeisIx = subtract 2 . (oeisIx @1248)

instance OEIS 49341 where
  oeis = 3 : 6 : map (oeisIx @7953) (zipWith (+) (oeis @49341) $ tail (oeis @49341))

instance OEIS 50450 where
  oeisIx = sum . map (^ 2) . filter ((== 1) . (`mod` 4)) . (rowT @27750) . succ

instance OEIS 50453 where
  oeisIx = sum . map (^ 2) . filter ((== 3) . (`mod` 4)) . (rowT @27750) . succ

instance OEIS 50461 where
  oeisIx (succ->n) = sum [d ^ 2 | d <- (rowT @27750) n, mod (div n d) 4 == 1]

instance OEIS 50465 where
  oeisIx (succ->n) = sum [d ^ 2 | d <- (rowT @27750) n, mod (div n d) 4 == 3]

instance OEIS 50470 where
  oeisIx n = (oeisIx @50461) n - (oeisIx @50465) n

instance OEIS 50493 where
  oeisIx = (oeisIx @120) . (oeisIx @217)

instance OEIS 51674 where
  oeis = map (\p -> p ^ p) (oeis @40)

instance OEIS 51731 where
  oeis = tablList @51731
instance Table 51731 where
  rowCol n k = 0 ^ mod n k
  rowT   = rowT_off @51731 @1
  tabl = map (map (oeisIx @7)) (tabl @48158)

instance OEIS 51903 where
  oeisIx 0 = 0
  oeisIx n = maximum $ (rowT @124010 . succ) n

instance OEIS 51904 where
  oeisIx 0 = 0
  oeisIx n = minimum $ (rowT @124010 . succ) n

instance OEIS 52217 where
  oeis = filter ((== 3) . (oeisIx @7953)) [0..]

instance OEIS 52218 where
  oeis = filter ((== 4) . (oeisIx @7953)) [0..]

instance OEIS 52219 where
  oeis = filter ((== 5) . (oeisIx @7953)) [0..]

instance OEIS 52220 where
  oeis = filter ((== 6) . (oeisIx @7953)) [0..]

instance OEIS 52221 where
  oeis = filter ((== 7) . (oeisIx @7953)) [0..]

instance OEIS 52222 where
  oeis = filter ((== 8) . (oeisIx @7953)) [0..]

instance OEIS 52223 where
  oeis = filter ((== 9) . (oeisIx @7953)) [0..]

instance OEIS 52224 where
  oeis = filter ((== 10) . (oeisIx @7953)) [0..]

instance OEIS 52409 where
  oeisIx 0 = 0
  oeisIx n = foldr1 gcd $ (rowT @124010 . succ) n

instance OEIS 53645 where
  -- oeisIx 0 = 0
  -- oeisIx n = 2 * (oeisIx @53645) n' + b  where (n', b) = divMod n 2
  oeis = concatMap (0 `enumFromTo`) (oeis @225)

instance OEIS 53696 where
  oeis = filter ((> 1) . (oeisIx @88323) . subtract 2) [2..]

instance OEIS 54868 where
  oeisIx = (oeisIx @120) . (oeisIx @120)

instance OEIS 55612 where
  oeisIx = product . map (+ 1) . tail . rowT @7318 . fi

instance OEIS 56169 where
  oeisIx = genericLength . filter (== 1) . (rowT @124010 . succ)

instance OEIS 56170 where
  oeisIx = genericLength . filter (> 1) . (rowT @124010 . succ)

instance OEIS 57168 where
  oeis = f 2 $ tail (oeis @120) where
     f x (z:zs) = (x + genericLength (takeWhile (/= z) zs)) : f (x + 1) zs

instance OEIS 58084 where
  oeisIx (succ->n) = fi . fromJust $ findIndex (elem n) (tabl @7318)

instance OEIS 61712 where
  oeisIx (succ->n) = fromJust $ find ((== n) . (oeisIx @120)) (oeis @40)

instance OEIS 61799 where
  oeisIx 0 = 1
  oeisIx (succ->n) = fi . (+1) $ fromJust $ findIndex (n <=) $ oeis @5

  oeis = 1 : unfoldr f (1,1,oeis @5)
    where
      f (i, x, ns)
        | (l,r) <- break (x <) ns
        , i'    <- i + genericLength l
        = Just (i', (i', x+1, r))

instance OEIS 65075 where
  oeis = 1 : 1 : f 2 where
     f x = y : f (x + y) where y = (oeisIx @7953) x

instance OEIS 65076 where
  oeis = 0 : 1 : zipWith (+)
                  (oeis @65076) (map (oeisIx @7953) $ tail (oeis @65076))

instance OEIS 65877 where
  oeis = map (fi . (+2)) $ findIndices (> 0) $ map (oeisIx @70635) [1..]

instance OEIS 66247 where
  oeisIx 0 = 0
  oeisIx n = 1 - (oeisIx @10051) n

instance OEIS 66301 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @51903) n - 1

instance OEIS 67029 where
  oeisIx = head . (rowT @124010) . succ

instance OEIS 67259 where
  oeis = map (+1) $ filter ((== 2) . (oeisIx @51903)) [1..]

instance OEIS 68498 where
  oeis = f [0..] (oeis @45) where
     f (u:us) (v:vs) = if u < (oeisIx @7953) v then v : f us vs else f us vs

instance OEIS 69482 where
  oeis = zipWith (-) (tail (oeis @1248)) (oeis @1248)

instance OEIS 70196 where
  oeisIx n = n + (oeisIx @4185) n

instance OEIS 70635 where
  oeisIx (succ->n) = n `mod` (oeisIx @7953 n)

instance OEIS 71178 where
  oeisIx = last . (rowT @124010) . succ

instance OEIS 71364 where
  oeisIx = product . zipWith (^) (oeis @40) . (rowT @124010) . succ

instance OEIS 72502 where
  oeis = f (S.singleton 9) $ drop 2 (oeis @1248) where
     f s (x:xs) = m : f (S.insert (2 * m) $ S.insert x s') xs where
                  (m,s') = S.deleteFindMin s

instance OEIS 72587 where
  oeis = tail $ filter (any even . (rowT @124010)) [1..]

instance OEIS 72588 where
  oeis = filter f [1..] where
     f x = any odd es && any even es  where es = (rowT @124010) x

instance OEIS 73776 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where y = sum $ zipWith (*) xs ms
     ms = map negate $ tail (oeis @8683)

instance OEIS 75311 where
  oeis = 1 : f 2 [1] where
     f x ys = if (oeisIx @120) x `elem` ys then f (x + 1) ys
                                     else x : f (x + 1) (x : ys)

instance OEIS 75517 where
  oeis = [0..9] ++ f 1 [0..9] where
     f x ys = if (oeisIx @7953) x `elem` ys then f (x + 1) ys
                                     else x : f (x + 1) (x : ys)

instance OEIS 76467 where
  oeis = 1 : filter ((> 2) . foldl1 gcd . (rowT @124010)) [2..]

instance OEIS 77436 where
  oeis = map fi $ elemIndices 0
     $ zipWith ((-) `on` (oeisIx @120)) [0..] (oeis @290)

instance OEIS 79704 where
  oeisIx = (* 2) . (oeisIx @1248)

instance OEIS 83542 where
  oeisIx n = (oeisIx @10) n * (oeisIx @10) (n + 1)
  oeis = zipWith (*) (tail (oeis @10)) (oeis @10)

instance OEIS 83652 where
  oeis = scanl1 (+) (oeis @70939)

instance OEIS 84228 where
  oeis = 1 : 2 : f 3 where
     f x = y : f (x + y) where y = (oeisIx @7953) x

instance OEIS 84920 where
  oeisIx n = (p - 1) * (p + 1) where p = (oeisIx @40) n

instance OEIS 84921 where
  oeisIx n = lcm (p - 1) (p + 1)  where p = (oeisIx @40) n

instance OEIS 85084 where
  oeis = 1 : f 1 (oeis @2808) where
     f x cs = y : f y (delete y cs) where
              y = fromJust $ find ((== 1) . (gcd x)) cs

instance OEIS 85104 where
  oeis = filter ((> 1) . (oeisIx @88323 . subtract 2)) (oeis @40)

instance OEIS 86849 where
  oeis = scanl1 (+) (oeis @37)

instance OEIS 87153 where
  oeisIx = p (oeis @37) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 87154 where
  oeisIx = p (oeis @37) where
     p _      0 = 1
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 87172 where
  oeisIx = head . (rowT @35516) . succ

instance OEIS 88705 where
  oeis = 0 : zipWith (-) (tail (oeis @120)) (oeis @120)

instance OEIS 92410 where
  oeis = zipWith (+) (oeis @8683) $ tail (oeis @8683)

instance OEIS 94510 where
  oeis = f 1 [1..] where
     f x zs = g zs where
       g (y:ys) = if y /= x && (oeisIx @120) y == wt
                     then y : f (x + 1) (delete y zs) else g ys
       wt = (oeisIx @120) x

instance OEIS 94511 where
  oeis = f 1 [1..] where
     f x zs = g zs where
       g (y:ys) = if y /= x && (oeisIx @120) y <= wt
                     then y : f (x + 1) (delete y zs) else g ys
       wt = (oeisIx @120) x

instance OEIS 94512 where
  oeisIx = fi . (+ 1) . fromJust . (`elemIndex` oeis @94511) . succ

instance OEIS 94513 where
  oeisIx = oeisIx @94511 . pred . oeisIx @94511

instance OEIS 94514 where
  oeisIx = oeisIx @94512 . pred . oeisIx @94512

instance OEIS 94535 where
  oeis = map (fi . (+ 1) . fromJust . (`elemIndex` (oeis @39995))) [0..]

instance OEIS 94677 where
  oeis = filter ((== 0) . flip mod 10 . (oeisIx @7953)) [1..]

instance OEIS 97764 where
  oeis = f 0 (S.singleton (4, 2, 2)) $
                   tail $ zip (oeis @51674) (oeis @40) where
     f m s ppps'@ ((pp, p) : ppps)
       | pp < qq   = f m (S.insert (pp, p, 2) s) ppps
       | qq == m   = f m (S.insert ((k * q) ^ q, q, k + 1) s') ppps'
       | otherwise = qq : f qq (S.insert ((k * q) ^ q, q, k + 1) s') ppps'
       where ((qq, q, k), s') = S.deleteFindMin s

instance OEIS 103586 where
  oeisIx n = (oeisIx @70939) (n + (oeisIx @70939) n)
  oeis = 1 : concat
     (zipWith (replicate . fromInteger) (tail (oeis @225)) [2..])

instance OEIS 105317 where
  oeis = 0 : 1 : h 1 (drop 4 (oeis @45)) (S.singleton (2, 2)) where
    h y xs'@ (x:xs) s
      | x < ff    = h y xs (S.insert (x, x) s)
      | ff == y   = h y xs' s'
      | otherwise = ff : h ff xs' (S.insert (f * ff, f) s')
      where ((ff, f), s') = S.deleteFindMin s

instance OEIS 106530 where
  oeisIx = foldr lcm 1 . (rowT @35516) . succ

instance OEIS 107015 where
  oeisIx = genericLength . filter even . (rowT @35516) . succ

instance OEIS 107016 where
  oeisIx = genericLength . filter odd . (rowT @35516) . succ

instance OEIS 107227 where
  oeis = filter ((all even) . (rowT @35516)) [1..]

instance OEIS 107228 where
  oeis = filter ((all odd) . (rowT @35516)) [1..]

instance OEIS 109449 where
  oeis = tablList @109449
instance Table 109449 where
  rowT n = zipWith (*) (rowT @7318 n) (reverse $ genericTake (n + 1) (oeis @111))
  tabl = map (rowT @109449) [0..]

instance OEIS 109614 where
  oeis = concat $ transpose [tail (oeis @578), (oeis @27), tail (oeis @290)]

instance OEIS 112526 where
  oeisIx 0 = 1
  oeisIx n = fi . fromEnum $ (> 1) $ minimum $ (rowT @124010 . succ) n

instance OEIS 112963 where
  oeisIx (succ->n) = sum $ zipWith (*)
     (oeis @8683) $ reverse $ genericTake (n - 1) (oeis @5)

instance OEIS 112966 where
  oeisIx (succ->n) = sum $ zipWith (*)
     (oeis @8683) $ reverse $ genericTake (n - 1) (oeis @1221)

instance OEIS 112968 where
  oeisIx (succ->n) = sum $ zipWith (*)
     (oeis @8683) $ reverse $ genericTake (n - 1) (oeis @1222)

instance OEIS 116385 where
  oeisIx n = (rowCol @51631) (n+1) $ (n+1) `div` 2

instance OEIS 116470 where
  oeis = 0 : 1 : 2 : concat (transpose [drop 4 (oeis @45), drop 3 (oeis @32)])

instance OEIS 116472 where
  oeisIx = (oeisIx @149) . (* 2)

instance OEIS 123684 where
  oeis = concat $ transpose [oeis @16777, (oeis @27)]

instance OEIS 124010 where
  oeis = tablList @124010
instance Table 124010 where
  rowCol = rowCol_off @124010 @1 @1
  rowT 1 = [0]
  rowT n = f n primes where
   f 1 _      = []
   f u (p:ps) = h u 0 where
     h v e
      | (v',m) <- divMod v p
      , m == 0 = h v' (e + 1)
      | e  > 0 = e : f v ps
      | let    = f v ps
  tabf = map (rowT @124010) [1..]

instance OEIS 131524 where
  oeis = concat $ transpose [tail (oeis @71), tail (oeis @71)]

instance OEIS 134734 where
  oeis = zipWith (-) (tail (oeis @84662)) (oeis @84662)

instance OEIS 134735 where
  oeis = concat $ transpose [oeis @40, oeis @1223]

instance OEIS 134743 where
  oeis = zipWith (-) (tail (oeis @134736)) (oeis @134736)

instance OEIS 134744 where
  oeis = zipWith (-) (tail (oeis @84663)) (oeis @84663)

instance OEIS 138791 where
  oeisIx n = (fi . fromJust $ elemIndex n (oeis @70635)) + 1

instance OEIS 143164 where
  oeis = filter ((== 13) . (oeisIx @7953)) [0..]

instance OEIS 153158 where
  oeis = filter ((== 2) . foldl1 gcd . (rowT @124010)) [2..]

instance OEIS 155587 where
  oeis = scanl (+) 1 (oeis @108)

instance OEIS 163866 where
  oeis = scanl1 (+) $ concat (tabl @7318)

instance OEIS 164514 where
  oeis = 1 : (oeis @37)

instance OEIS 166370 where
  oeis = filter ((== 17) . (oeisIx @7953)) [0..]

instance OEIS 166459 where
  oeis = filter ((== 19) . (oeisIx @7953)) [0..]

instance OEIS 173639 where
  oeis = filter (odd . (oeisIx @7953) . (* 11)) [0..]

instance OEIS 182469 where
  oeis = tablList @182469
instance Table 182469 where
  rowCol = rowCol_off @182469 @1 @1
  rowT = rowT @27750 . oeisIx @265
  tabf = map (rowT @182469) [0..]

instance OEIS 188641 where
  oeisIx = (1 -) . signum . (oeisIx @70635)

instance OEIS 188642 where
  oeisIx n
     | (oeisIx @168046) n == 1 = 1 - signum (n `mod` (oeisIx @7954) n)
     | otherwise      = 0

instance OEIS 188643 where
  oeis = map (fi.succ) $ elemIndices 0 $ map (oeisIx @188642) [1..]

instance OEIS 188649 where
  oeisIx (succ->n) = foldl lcm 1 $ map (oeisIx @4086) $ filter ((== 0) . mod n) [1..n]

instance OEIS 188650 where
  oeis = map (fi.succ) $ elemIndices 0 $ zipWith (-) [1..] $ map (oeisIx @188649) [0..]

instance OEIS 188652 where
  oeis = zipWith (-) (tail (oeis @463)) (oeis @463)

instance OEIS 188653 where
  oeis = zipWith (-) (tail (oeis @188652)) (oeis @188652)

instance OEIS 193431 where
  oeis = tail $ f (oeis @7376) where
     f (x:xs'@ (x':x'':xs)) = 10* (10*x + x') + x'' : f xs'

instance OEIS 193492 where
  oeis = tail $ f (oeis @7376) where
     f xs'@ (x:xs) = ((foldl (\u v -> 10*u + v) 0) $ take 4 xs') : f xs

instance OEIS 193493 where
  oeis = tail $ f (oeis @7376) where
     f xs'@ (x:xs) = ((foldl (\u v -> 10*u + v) 0) $ take 5 xs') : f xs

instance OEIS 193551 where
  oeisIx (succ->n) = fi $ (fromJust $ elemIndex n (oeis @26)) + 1

instance OEIS 193581 where
  oeisIx n = n - (oeisIx @4185) n
--   oeis = map (oeisIx @193581) [0..]

instance OEIS 194459 where
  oeisIx = sum . map (signum . flip mod 5) . rowT @7318 . fi

instance OEIS 219531 where
  oeisIx = sum . take 12 . rowT @7318

instance OEIS 219606 where
  oeis = concat $ transpose [oeis @1223, oeis @40]

instance OEIS 226538 where
  oeis = concat $ transpose [drop 2 (oeis @71), tail (oeis @1911)]

instance OEIS 228056 where
  oeis = filter f [1..] where
     f x = length us == 1 && (head us > 1 || not (null vs)) where
           (us,vs) = Data.List.partition odd $ (rowT @124010) x

instance OEIS 230099 where
  oeisIx n = (oeisIx @7954) n + n

instance OEIS 230102 where
  oeis = iterate (oeisIx @230099) 1

instance OEIS 235151 where
  oeis = filter ((== 12) . (oeisIx @7953)) [0..]

instance OEIS 235225 where
  oeis = filter ((== 14) . (oeisIx @7953)) [0..]

instance OEIS 235226 where
  oeis = filter ((== 15) . (oeisIx @7953)) [0..]

instance OEIS 235227 where
  oeis = filter ((== 16) . (oeisIx @7953)) [0..]

instance OEIS 235228 where
  oeis = filter ((== 18) . (oeisIx @7953)) [0..]

instance OEIS 235229 where
  oeis = filter ((== 20) . (oeisIx @7953)) [0..]

instance OEIS 242216 where
  oeisIx = p [1,2,3,7,11,19,43,67,163] where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 254649 where
  oeis = f (oeis @7376) [0] where
     f (x:xs) ys = g x xs where
       g y zs'@ (z:zs) | y `elem` ys = g (y + z) zs
                      | otherwise   = y : f zs' (y:ys)

instance OEIS 254719 where
  oeis = g (0 : drop 2 (oeis @45)) (oeis @105317) where
     g fs'@ (f:fs) (x:xs) = if x == f then g fs xs else x : g fs' xs

instance OEIS 254784 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 5

instance OEIS 254790 where
  oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @254788)) . succ

instance OEIS 254792 where
  oeis = map abs $ zipWith (-) (oeis @254788) $ tail (oeis @254788)

instance OEIS 254793 where
  oeis = zipWith (+) (oeis @254788) $ tail (oeis @254788)

instance OEIS 254967 where
  oeis = tablList @254967
instance Table 254967 where
  tabl = diags [] $
     iterate (\lds -> map abs $ zipWith (-) (tail lds) lds) (oeis @959)
     where diags uss (vs:vss) = (map head wss) : diags (map tail wss) vss
                                where wss = vs : uss

instance OEIS 254969 where
  oeisIx n = (rowCol @254967) (2 * n) n

instance OEIS 255134 where
  oeis = zipWith (-) (tail (oeis @97764)) (oeis @97764)

(oeis255136, (oeis255137)) = unzip $ f [1..] (oeis @255134) (-1) where
     f (x:xs) (y:ys) r = if y > r then (y, x) : f xs ys y else f xs ys r

instance OEIS 255136 where
  oeis = oeis255136

instance OEIS 255137 where
  oeis = oeis255137

instance OEIS 256100 where
  oeis = f (tail $ oeis @7376) $ replicate 10 1 where
     f (d:ds) counts = y : f ds (xs ++ (y + 1) : ys) where
                             (xs, y:ys) = splitAt d counts

instance OEIS 256523 where
  oeis = [x | x <- [0..], let i = (oeisIx @30) x,
                      (oeisIx @30) (x ^ 2) == i, (oeisIx @30) (x ^ 3) == i]

instance OEIS 259525 where
  oeis = zipWith (-) (tail pascal) pascal
    where pascal = concat (tabl @7318)

instance OEIS 260416 where
  oeis = f 1 (oeis @40) where
     f x (p:ps) = g ps where
         g (q:qs) = if (q - x) `mod` p == 0 then q : f (x + 1) ps else g qs

instance OEIS 261470 where
  oeis = zipWith (-) (drop 2 (oeis @1223)) (oeis @1223)

instance OEIS 264749 where
  oeisIx n = div n $ (oeisIx @70939) n

instance OEIS 265736 where
  oeisIx = sum . (rowT @265705)

instance OEIS 266089 where
  oeis = 0 : f 0 (zip [1..] $ tail (oeis @120)) where
     f x zws = g zws where
       g (yw@ (y, w) : yws) | abs (x - w) /= 1 = g yws
                           | otherwise = y : f w (delete yw zws)

instance OEIS 266154 where
  oeisIx = fi . fromJust . (`elemIndex` (oeis @266089))

instance OEIS 266161 where
  oeisIx = (oeisIx @120) . (oeisIx @266089)

instance OEIS 273156 where
  oeisIx = product . (rowT @35516)

