module OEIS.Prime () where

import OEIS.Common
import OEIS.Part2

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

drop          = genericDrop
(!!)          = genericIndex
length        = genericLength
replicate     = genericReplicate
splitAt       = genericSplitAt
take          = genericTake
findIndices f = fmap fi . L.findIndices f
elemIndices x = fmap fi . L.elemIndices x
findIndex f   = fmap fi . L.findIndex f
elemIndex x   = fmap fi . L.elemIndex x


-- instance OEIS 171942 where
--   oeisIx 1 = 0
--   oeisIx n = head [m | m <- [1..], (oeisIx @120) (m + n - 1) == (oeisIx @120) (n - 1)]

-- instance OEIS 173694 where
--   oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @2322)) [1..]

-- instance OEIS 175944 where
--   oeis = concat $ zipWith ($) (map replicate (oeis @18252)) (oeis @18252)

-- instance OEIS 181522 where
--   oeisIx = genericLength . filter ((== 1) . (oeisIx @64911) . sum) .
--                             subsequences . enumFromTo 1 . succ

-- instance OEIS 182426 where
--   oeis = concatMap f $ group $ zipWith (-) (tail ips) ips where
--      f xs | head xs == 1 = reverse $ enumFromTo 2 $ length xs + 1
--           | otherwise    = take (length xs) $ repeat 1
--      ips = map (oeisIx @49084) (oeis @166251)

-- instance OEIS 184162 where
--   oeis = 1 : g 2 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then 2 * (oeisIx @184162) t + 1 else (oeisIx @184162) r + (oeisIx @184162) s - 1
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 187090 where
--   oeisIx n = until ((== 9) . (oeisIx @30)) (+ n) n

-- instance OEIS 187204 where
--   oeis = map (+ 1) $ elemIndices 0 $ map (oeisIx @187202.pred) [1..]

-- instance OEIS 187285 where
--   oeisIx n = until ((== 1) . (oeisIx @30)) (+ n) n

-- instance OEIS 188069 where
--   oeis = (map succ $ elemIndices 2 $ tail $ oeis @7538)

-- instance OEIS 188070 where
--   oeis = map succ $ elemIndices 3 $ tail $ oeis @7538

-- instance OEIS 188145 where
--   oeis = elemIndices 0 $ zipWith3 (\x y z -> x - y - z)
--      (map (oeisIx @3415) (oeis @3415)) (oeis @3415) [0..]

-- instance OEIS 188163 where
--   oeisIx n = succ $ fromJust $ elemIndex n (oeis @4001)

-- instance OEIS 188226 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (map (oeisIx @188172) [1..]))) [0..]

-- instance OEIS 188968 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @188967

-- instance OEIS 188969 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @188967

-- instance OEIS 189639 where
--   oeisIx (succ->n) = (oeis @189710) !! (n - 1)
--   oeis = elemIndices 0 $
--      zipWith (-) (map (oeisIx @3415) (oeis @3415)) (map pred (oeis @3415))

-- instance OEIS 191967 where
--   oeisIx n = n * (oeisIx @1651) n

-- instance OEIS 193671 where
--   oeis = map (+ 1) $ findIndices (> 0) $ map (oeisIx @187202) [1..]

-- instance OEIS 193672 where
--   oeis = map (+ 1) $ findIndices (< 0) $ map (oeisIx @187202) [1..]

-- instance OEIS 193738 where
--   oeis = tablList @193738
-- instance Table 193738 where
--   tabl = map reverse (tabl @193739)

-- instance OEIS 193739 where
--   oeis = tablList @193739
-- instance Table 193739 where
--   rowCol n k = (tabl @193738) !! n !! k
--   rowT n = (tabl @193738) !! n
--   tabl = map reverse (tabl @193739)

-- instance OEIS 193927 where
--   oeis = findIndices (< 0) (oeis @193926)

-- instance OEIS 195942 where
--   oeis = filter (\x -> (oeisIx @10051 . pred) x == 0 && (oeisIx @10055) x == 1) (oeis @52382)

-- instance OEIS 195943 where
--   oeis = filter ((== 1) . (oeisIx @10055)) (oeis @52382)

-- instance OEIS 196047 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then (oeisIx @196047) t + (oeisIx @61775) t else (oeisIx @196047) r + (oeisIx @196047) s
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196048 where
--   oeis = 0 : 1 : g 3 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then (oeisIx @196048) t + (oeisIx @109129) t else (oeisIx @196048) r + (oeisIx @196048) s
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196049 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196049) t + (oeisIx @64911) t
--          | otherwise = (oeisIx @196049) r + (oeisIx @196049) s + (oeisIx @64911) s
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196050 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then (oeisIx @196050) t + 1 else (oeisIx @196050) r + (oeisIx @196050) s
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196051 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196051) t + (oeisIx @196047) t + (oeisIx @196050) t + 1
--          | otherwise = (oeisIx @196051) r + (oeisIx @196051) s +
--                        (oeisIx @196047) r * (oeisIx @196050) s + (oeisIx @196047) s * (oeisIx @196050) r
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196052 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then (oeisIx @1222) t + 1 else (oeisIx @196052) r + (oeisIx @196052) s
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196053 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196053) t + 2 + 2 * (oeisIx @1222) t
--          | otherwise = (oeisIx @196053) r + (oeisIx @196053) s -
--                        (oeisIx @1222) r ^ 2  - (oeisIx @1222) s ^ 2 + (oeisIx @1222) x ^ 2
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196057 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196057) t
--          | otherwise = (oeisIx @196057) r + (oeisIx @196057) s + (oeisIx @1222) r * (oeisIx @1222) s
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196062 where
--   oeis = 0 : 1 : g 3 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196062) t
--          | otherwise = (oeisIx @196062) r + (oeisIx @196062) s - 0 ^ (x `mod` 4)
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196068 where
--   oeis = 1 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196068) t + (oeisIx @61775) t + 1
--          | otherwise = (oeisIx @196068) r + (oeisIx @196068) s - 1
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 198332 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @198332) t + 2 * (oeisIx @1222) t
--          | otherwise = (oeisIx @198332) r + (oeisIx @198332) s + 2 * (oeisIx @1222) r * (oeisIx @1222) s
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 203814 where
--   oeisIx n = genericLength [x | x <- [0..n], (oeisIx @43537) x == (oeisIx @43537) n]

-- instance OEIS 203967 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @9087)

-- instance OEIS 206498 where
--   oeisIx 0 = 0
--   oeisIx 1 = 2
--   oeisIx (succ->x) = if t > 0 then (oeisIx @196062) t + t `mod` 2 else (oeisIx @196062) x
--               where t = (oeisIx @49084) x

-- instance OEIS 208662 where
--   oeisIx n = head [m | m <- [1..], let p = (oeisIx @65091) n,
--      let q = 2 * m - p, (oeisIx @10051 . pred) q == 1,
--      all ((== 0) . (oeisIx @10051 . pred)) $ map (2 * m -) $ take (n - 1) (oeis @65091)]

-- instance OEIS 212211 where
--   oeis = tablList @212211
-- instance Table 212211 where
--   rowCol = rowCol_off @212211 @2 @2
--   tabl = map (rowT @212211) [2..]
--   rowT n = zipWith (-)
--      (map (+ (oeisIx @720) n) $ take (n - 1) $ tail (oeis @720))
--      (drop (n + 1) (oeis @720))

-- instance OEIS 214567 where
--   oeis = 1 : g 2 where
--     g x = y : g (x + 1) where
--       y | t > 0     = (oeisIx @214567) t + 1
--         | otherwise = 1 + sum (map ((subtract 1) . (oeisIx @214567)) $ (rowT @27748) x)
--          where t = (oeisIx @49084) x

-- instance OEIS 218494 where
--   oeisIx = p (tail (oeis @290)) . (^ 3) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 219908 where
--   oeisIx (succ->n) = (oeis @219907) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @219907)

-- instance OEIS 220096 where
--   oeisIx (succ->n) = if z == 1 then n - 1 else z  where z = (oeisIx @32742) n

-- instance OEIS 220264 where
--   oeisIx n = fromJust $ find ((== n) . (oeisIx @86971)) (oeis @220423)

-- instance OEIS 222581 where
--   oeis = map length $ group (oeis @93796)

-- instance OEIS 222623 where
--   oeis = filter (\x -> (oeisIx @113966) x == x) [1..]

-- instance OEIS 224458 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @224458) t + (oeisIx @1222) t
--          | otherwise = (oeisIx @224458) r + (oeisIx @224458) s + (oeisIx @1222) r * (oeisIx @1222) s
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 224981 where
--   oeis = filter (p 6 $ tail (oeis @290)) [1..] where
--      p k (q:qs) m = k == 0 && m == 0 ||
--                     q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

-- instance OEIS 224982 where
--   oeis = filter (p 7 $ tail (oeis @290)) [1..] where
--      p k (q:qs) m = k == 0 && m == 0 ||
--                     q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

-- instance OEIS 224983 where
--   oeis = filter (p 8 $ tail (oeis @290)) [1..] where
--      p k (q:qs) m = k == 0 && m == 0 ||
--                     q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

-- instance OEIS 225395 where
--   oeisIx n = product $ zipWith (^)
--       (map (oeisIx @49084) $ (rowT @27748) n) (map (oeisIx @225395) $ (rowT @124010) n)

-- instance OEIS 227296 where
--   oeisIx n = p [1 .. (oeisIx @10) n] n where
--      p _          0 = 1
--      p []         _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 227413 where
--   oeis = 1 : concat (transpose [map (oeisIx @40) (oeis @227413),
--                                         map (oeisIx @2808) (oeis @227413)])

-- instance OEIS 230780 where
--   oeis = filter (all (/= 1) . map (flip mod 6) . (rowT @27748)) [1..]

-- instance OEIS 235249 where
--   oeisIx n = if y == n then n else (oeisIx @235249) y  where y = (oeisIx @1175) n

-- instance OEIS 238525 where
--   oeisIx n = mod n $ (oeisIx @1414.succ) n

-- instance OEIS 239433 where
--   oeis = filter
--      (\z -> any (== z) $ map (oeisIx @3415) $ takeWhile (<= (oeisIx @2620) z) (oeis @13929)) [2..]

-- instance OEIS 239586 where
--   oeisIx n = (oeisIx @78972) n `div` (oeisIx @239585) n

-- instance OEIS 239943 where
--   oeis = [x | x <- [1..], (oeisIx @239965) x == x]

-- instance OEIS 240923 where
--   oeisIx (succ->n) = numerator sq - (oeisIx @203) (denominator sq)
--      where sq = (oeisIx @203) n % n

-- instance OEIS 241917 where
--   oeisIx (succ->n) = i - j where
--               (i:j:_) = map (oeisIx @49084) $ reverse (1 : (rowT @27746) n)

-- instance OEIS 241919 where
--   oeisIx 0 = 0
--   oeisIx (succ->n) = i - j where
--               (i:j:_) = map (oeisIx @49084) $ reverse (1 : (rowT @27748) n)

-- instance OEIS 242411 where
--   oeisIx 0 = 0
--   oeisIx (succ->n) = i - j where
--               (i:j:_) = map (oeisIx @49084) $ ps ++ [p]
--               ps@ (p:_) = reverse $ (rowT @27748) n

-- instance OEIS 245543 where
--   oeis = scanl1 (+) (oeis @160239)

-- instance OEIS 246517 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @141036)) [0..]

-- instance OEIS 246518 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ (oeis @141036)

-- instance OEIS 246704 where
--   oeis = filter (\x -> (oeisIx @113963) x == x) [1..]

-- instance OEIS 247104 where
--   oeis = filter ((== 1) . (oeisIx @8966)) $ tail (oeis @3052)

-- instance OEIS 247180 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @67029

-- instance OEIS 247204 where
--   oeis = filter ((zipWith (==) [1..] (oeis @250552)) !!) [1..]

-- instance OEIS 247253 where
--   oeis = zipWith (-) (tail (oeis @251239)) (oeis @251239)

-- instance OEIS 247793 where
--   oeis = 2 : f (zip [2..] $ tail (oeis @40)) where
--      f ((x, p) : xps) = m : f xps where
--        m = head [y | y <- [1..], (p + (oeisIx @40) y) `mod` (oeisIx @720) (x * y) == 0]

-- instance OEIS 247892 where
--   oeisIx (succ->n) = n - (oeisIx @247815) n

-- instance OEIS 249575 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @84937)) [1..]

-- instance OEIS 249602 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @84937)) [1..]

-- instance OEIS 249684 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @249777

-- instance OEIS 251548 where
--   oeis = map length $ group $ map (oeisIx @251546) [1..]

-- instance OEIS 251561 where
--   oeisIx 1 = 1
--   oeisIx n | q == 1                    = 2 * p
--             | p == 2 && (oeisIx @10051 . pred) q == 1 = q
--             | otherwise                 = n
--             where q = div n p; p = (oeisIx @20639) n

-- instance OEIS 251728 where
--   oeis = filter f [1..] where
--                         f x = q < p ^ 2 && (oeisIx @10051 . pred) q == 1
--                               where q = div x p; p = (oeisIx @20639) x

-- instance OEIS 251767 where
--   oeis = map head $ group (oeis @251539)

-- instance OEIS 251768 where
--   oeis = map length $ group (oeis @251539)

-- instance OEIS 252078 where
--   oeis = [x | x <- [1..], (oeisIx @252001) x == x]

-- instance OEIS 252079 where
--   oeis = [x | x <- [1..], (oeisIx @252022) x == x]

-- instance OEIS 252458 where
--   oeis = [x | x <- [1..], (oeisIx @249990) x == x]

-- instance OEIS 252912 where
--   oeis = filter (\x -> (oeisIx @98550.pred) x == (oeisIx @251555) x) [1..]

-- instance OEIS 252939 where
--   oeis = tail $ zipWith (-) (tail (oeis @252912)) (oeis @252912)

-- instance OEIS 252940 where
--   oeis = map length $ group (oeis @252939)

-- instance OEIS 253046 where
--   oeisIx n | i == 0 || p > 3 = n
--             | p == 2          = 3 * (oeisIx @40) (i + 1)
--             | otherwise       = 2 * (oeisIx @40) (i - 1)
--               where i = (oeisIx @49084) (div n p);  p = (oeisIx @20639) n

-- instance OEIS 253106 where
--   oeis = filter f [1..] where
--      f x = p <= 3 && (oeisIx @10051 . pred) (div x p) == 1  where p = (oeisIx @20639) x

-- instance OEIS 253138 where
--   oeisIx n = sum $ map (oeisIx @64911) $
--      takeWhile (> 0) $ map (2 * p -) $ dropWhile (< p) (oeis @1358)
--      where p = (oeisIx @40) n

-- instance OEIS 253425 where
--   oeis = map length $ group (oeis @253415)

-- instance OEIS 254656 where
--   oeis = [x | x <- [1..], (oeisIx @254649) x == x]

-- instance OEIS 255482 where
--   oeisIx n = (oeisIx @64664) n - (oeisIx @255479) n

-- instance OEIS 255678 where
--   oeisIx n = head $ filter ((== n) . (oeisIx @118668)) [0..]

-- instance OEIS 256406 where
--   oeis = f (oeis @166133) where
--      f (u:vs'@ (v:ws)) | u > v || v /= u ^ 2 - 1 = f vs'
--                       | otherwise               = u : f ws

-- instance OEIS 256489 where
--   oeis = zipWith (-) (tail (oeis @257509)) (oeis @257509)

-- instance OEIS 256543 where
--   oeis = [x | x <- [1..], abs (oeisIx @256541 x) == 1]

-- instance OEIS 256786 where
--   oeis = filter f (oeis @52382) where
--      f x = g x where
--        g z = z == 0 || x `mod` (oeisIx @40) d == 0 && g z'
--              where (z', d) = divMod z 10

-- instance OEIS 256885 where
--   oeisIx n = (oeisIx @217) n - (oeisIx @720) n

-- instance OEIS 257053 where
--   oeis = tablList @257053
-- instance Table 257053 where
--   rowCol = rowCol_off @257053 @1 @0
--   rowT   = rowT_off @257053 @1
--   tabf = map (rowT @256913) (oeis @40)

-- instance OEIS 257070 where
--   oeisIx = last . (rowT @257053) . succ.succ

-- instance OEIS 257071 where
--   oeisIx = genericLength . (rowT @257053) . succ.succ

-- instance OEIS 257646 where
--   oeisIx n = fromJust $ findIndex (elem n) (tabl @103284)

-- instance OEIS 257762 where
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 2 $ tail $ oeis @258383)

-- instance OEIS 257892 where
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 4 $ tail $ oeis @258383)


-- instance OEIS 257951 where
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 5 $ tail $ oeis @258383)

-- instance OEIS 258095 where
--   oeis = filter
--                  (\x -> (oeisIx @258091) x `notElem` [3, 5, 7, 13, 19, 37, 73]) [1..]

-- instance OEIS 258115 where
--   oeisIx (succ->n) = (oeisIx @208570) n `div` n

-- instance OEIS 258125 where
--   oeis = 2 : 2 : zipWith (+)
--                  (map (oeisIx @6530) (oeis @258125)) (tail (oeis @258125))

-- instance OEIS 258227 where
--   oeis = f 12 1 (map fi $ tail (oeis @7376)) where
--      f x y (d:ds) | gcd x y > 1 = y : f y d ds
--                   | otherwise   = f x (10 * y + d) ds

-- instance OEIS 258262 where
--   oeis = filter ((== 1) . (oeisIx @10057)) (oeis @258865)

-- instance OEIS 258324 where
--   oeisIx (succ->n) = foldl lcm 1 $ map (n -) $ (rowT @27751) n

-- instance OEIS 258353 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @212306

-- instance OEIS 258437 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258383)).succ

-- instance OEIS 258449 where
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 3 $ tail $ oeis @258383)

-- instance OEIS 258768 where
--   oeis = tail [x | x <- [1..], (oeisIx @258767) x == x]

-- instance OEIS 259969 where
--   oeisIx n = (oeis @259967) !! n
--   oeis = 3 : 2 : 2 : 5 : zipWith3 (((+) .) . (+))
--      (oeis @259967) (drop 2 (oeis @259967)) (drop 3 (oeis @259967))

-- instance OEIS 260797 where
--   oeisIx = (oeisIx @98743.pred) . (oeisIx @142) . succ

-- instance OEIS 260936 where
--   oeis = [x | x <- [1..], (oeisIx @260933) x == x]

-- instance OEIS 261121 where
--   oeisIx = (oeisIx @98743.pred) . (oeisIx @2110).succ

-- instance OEIS 261335 where
--   oeis = [x | x <- [0..], (oeisIx @261333) x == x]


-- instance OEIS 261525 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @31131)) . (* 2)





-- instance OEIS 261969 where
--   oeisIx n = product $ map fst $ filter ((== emax) . snd) $ zip ps es
--       where emax = maximum es
--             ps = (rowT @27748) n; es = (rowT @124010) n

-- instance OEIS 262435 where
--   oeis = [x | x <- [1..], (oeisIx @262411) x == x]

-- instance OEIS 262481 where
--   oeis = filter (\x -> (oeisIx @120) x == (oeisIx @20639) x) [1..]

-- instance OEIS 263837 where
--   oeis = filter (\x -> (oeisIx @1065) x <= x) [1..]

-- instance OEIS 264782 where
--   oeisIx (succ->n) = sum $ zipWith (^) (map (oeisIx @8683) divs) (reverse divs)
--               where divs = (rowT @27750) n

-- instance OEIS 264856 where
--   oeisIx n = fromJust $ findIndex (elem n) (tabl @125605)

-- instance OEIS 276127 where
--   oeisIx 0 = 1
--   oeisIx (succ->n) = (oeisIx @1414) $ (oeisIx @64413) n

-- instance OEIS 226950 where
--   oeis = f (oeis @76467) S.empty where
--      f (x:xs) s | S.size s'' <= 1 = f xs (x `S.insert` s)
--                 | otherwise     = x : f xs (x `S.insert` s)
--                 where s'' = S.filter ((`S.member` s) . (x -)) s'
--                       (s', _) = S.split (x `div` 2) s

-- instance OEIS 1158 where
--   oeisIx n = product $ zipWith (\p e -> (p^ (3*e + 3) - 1) `div` (p^3 - 1))
--                         ((rowT @27748) n) ((rowT @124010) n)

-- instance OEIS 5276 where
--   oeis = filter p [1..] where
--      p z = p' z [0, z] where
--        p' x ts = if y `notElem` ts then p' y (y:ts) else y == z
--                  where y = (oeisIx @48050) x

-- instance OEIS 5349 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @70635

-- instance OEIS 5413 where
--   oeis = 1 : zipWith (*) [1 ..] (zipWith (+) (tail (oeis @5412)) (zipWith (*) [4, 6 ..] (oeis @5413)))

-- instance OEIS 7367 where
--   oeis = map fst $ filter ((== 3) . snd) $ zip (oeis @2202) (oeis @58277)

-- instance OEIS 7434 where
--   oeisIx (succ->n) = sum $ zipWith3 (\x y z -> x * y * z)
--                     tdivs (reverse tdivs) (reverse divs)
--                     where divs = (rowT @27750) n;  tdivs = map (oeisIx @10) divs

-- instance OEIS 7439 where
--   oeis = 1 : 1 : f 2 where
--      f x = (sum $ map (oeisIx @7439) $ (rowT @27750) (x - 1)) : f (x + 1)

-- instance OEIS 7457 where
--   oeisIx n = genericLength [k | k <- [1..n - 1], gcd k n == 1, (oeisIx @8966) k == 1,
--                           let j = n - k, gcd j n == 1, (oeisIx @8966) j == 1]

-- instance OEIS 7554 where
--   oeis = 1 : f 1 where
--      f x = (sum $ zipWith (*) (map (oeisIx @8683) divs)
--                               (map (oeisIx @7554) $ reverse divs)) : f (x + 1)
--             where divs = (rowT @27750) x

-- instance OEIS 7811 where
--   oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051 . pred)) $
--      iterate (zipWith (+) [10, 10, 10, 10]) [1, 3, 7, 9]

-- instance OEIS 8847 where
--   oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @203) . (oeisIx @290)) [1..]

-- instance OEIS 9087 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

-- instance OEIS 13638 where
--   oeisIx n = (oeisIx @151799) n * (oeisIx @151800) n

-- instance OEIS 14701 where
--   -- oeisIx 1 = 0
--   oeisIx n = (oeisIx @7953) $ (oeisIx @7931) (n - 1)

-- instance OEIS 20486 where
--   oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @5) x == 0) [1..]

-- instance OEIS 20487 where
--   oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @203) x == 0) [1..]

-- instance OEIS 24556 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) $ tail (oeis @56911)

-- instance OEIS 26239 where
--   oeisIx 1 = 1
--   oeisIx n | (oeisIx @10051) n == 1 = (oeisIx @2808) $ (oeisIx @49084) n
--             | otherwise      = (oeisIx @40) $ (oeisIx @66246) n

-- instance OEIS 26274 where
--   oeis = map (subtract 1) $ tail $ (map succ $ elemIndices 1 $ tail $ oeis @35612)

-- instance OEIS 30664 where
--   oeisIx n = (oeisIx @7917) n * (oeisIx @7918) n

-- instance OEIS 31877 where
--   oeis = [x | x <- [1..], x `mod` 10 > 0,
--                       let x' = (oeisIx @4086) x, x' /= x && x `mod` x' == 0]

-- instance OEIS 33556 where
--   oeis = iterate (\x -> 2*x - (oeisIx @151799 x)) 3


-- instance OEIS 33620 where
--   oeis = filter chi [1..] where
--      chi n = (oeisIx @136522) spf == 1 && (n' == 1 || chi n') where
--         n' = n `div` spf
--         spf = (oeisIx @20639) n

-- instance OEIS 33632 where
--   oeis = filter (\x -> (oeisIx @62401) x == (oeisIx @62402) x) [1..]

-- instance OEIS 33950 where
--   oeis = [x - 1 | x <- [1..], x `mod` (oeisIx @5) x == 0]

-- instance OEIS 34710 where
--   oeis = map succ . elemIndices 0 $ map (\x -> (oeisIx @7953) x - (oeisIx @7954) x) [1..]

-- instance OEIS 36262 where
--   oeis = tablList @36262
-- instance Table 36262 where
--   rowCol n k = delta !! (n - k) !! (k - 1)
--     where
--       delta = iterate
--         (\pds -> zipWith (\x y -> abs (x - y)) (tail pds) pds) (oeis @40)

-- instance OEIS 36454 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

-- instance OEIS 36537 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @5)) [1..]

-- instance OEIS 36786 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x < (oeisIx @55642) x]

-- instance OEIS 36787 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x == (oeisIx @55642) x]

-- instance OEIS 36788 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x <= (oeisIx @55642) x]

-- instance OEIS 36844 where
--   oeis = filter ((== 0). (oeisIx @238525)) [2..]

-- instance OEIS 37992 where
--   oeisIx n = head [x | x <- [1..], (oeisIx @5) x == 2 ^ n]

-- instance OEIS 38186 where
--   oeis = map succ $ elemIndices 1
--                  $ zipWith (*) (map (oeisIx @188641) [1..]) (map (oeisIx @188642) [1..])

-- instance OEIS 38567 where
--   oeis = concatMap (\x -> genericTake (oeisIx @10 x) $ repeat x) [1..]

-- instance OEIS 38572 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @53645) n * m + n' where (n', m) = divMod n 2

-- instance OEIS 45967 where
--   oeisIx 1 = 4
--   oeisIx n = product $ zipWith (^)
--               (map (oeisIx @151800) $ (rowT @27748) n) (map (+ 1) $ (rowT @124010) n)

-- instance OEIS 46099 where
--   oeis = filter ((== 1) . (oeisIx @212793.pred)) [1..]

-- instance OEIS 46930 where
--   oeisIx 1 = 1
--   oeisIx n = subtract 2 $ (oeisIx @31131) n

-- instance OEIS 47994 where
--   oeisIx n = f n 1 where
--      f 1 uph = uph
--      f x uph = f (x `div` sppf) (uph * (sppf - 1)) where sppf = (oeisIx @28233) x

-- instance OEIS 48055 where
--   oeis = [x | x <- (oeis @2808)
--          , let (us,vs) = Data.List.partition
--                           ((== 1) . (oeisIx @10051.pred)) $ (rowT @27751) x
--          , sum us + x == sum vs]

-- instance OEIS 48098 where
--   oeis = [x | x <- [1..], (oeisIx @6530) x ^ 2 <= x]

-- instance OEIS 48272 where
--   oeisIx n = (oeisIx @1227) n - (oeisIx @183063) n

-- instance OEIS 48411 where
--   oeis = 0 : filter ((== 1) . (oeisIx @10052)) (oeis @33075)

-- instance OEIS 48890 where
--   oeis = filter f (oeis @40) where
--      f x = all (`elem` [0,1,6,8,9]) ds && x' /= x && (oeisIx @10051) x' == 1
--        where x' = foldl c 0 ds
--              c v 6 = 10*v + 9; c v 9 = 10*v + 6; c v d = 10*v + d
--              ds = unfoldr d x
--              d z = if z == 0 then Nothing else Just $ swap $ divMod z 10

-- instance OEIS 49098 where
--   oeis = filter ((== 0) . (oeisIx @8966) . (+ 1)) (oeis @40)

-- instance OEIS 49613 where
--   oeisIx n = 2 * n - (oeisIx @7917) (2 * n - 2)

-- instance OEIS 50001 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @50000)) + 1

-- instance OEIS 50435 where
--   oeisIx = (oeisIx @2808) . (oeisIx @2808)
--   oeis = map (oeisIx @2808) (oeis @2808)

-- instance OEIS 50931 where
--   oeis = map pred $ filter (any (== 1) . map (flip mod 6) . (rowT @27748) . succ) [1..]

-- instance OEIS 51004 where
--   oeis =  [x | x <- (oeis @5349),
--                        x == head (dropWhile (< x) (oeis @34838))]

-- instance OEIS 51015 where
--   oeis = filter zeisel [3, 5 ..] where
--      zeisel x = 0 `notElem` ds && length ds > 2 &&
--            all (== 0) (zipWith mod (tail ds) ds) && all (== q) qs
--            where q:qs = (zipWith div (tail ds) ds)
--                  ds = zipWith (-) (tail ps) ps
--                  ps = 1 : (rowT @27746) x

-- instance OEIS 51144 where
--   oeis = filter ((== 0) . (oeisIx @8966)) (oeis @37)

-- instance OEIS 51250 where
--   oeis = filter (all ((== 1) . (oeisIx @10055)) . (rowT @38566)) [1..]

-- instance OEIS 51283 where
--   oeis = map succ $ filter (\x -> (oeisIx @34699 x) ^ 2 < x) [1..]

-- instance OEIS 51487 where
--   oeis = [x | x <- [2..], let t = (oeisIx @10) x, t == (oeisIx @10) (x - t)]

-- instance OEIS 51488 where
--   oeis = map pred [x | x <- [2..], let t = (oeisIx @10) x, t < (oeisIx @10) (x - t)]

-- instance OEIS 51532 where
--   oeis = filter ((== 1) . (oeisIx @212793.pred)) (oeis @56867)

-- instance OEIS 52011 where
--   oeis = c 0 0 $ drop 2 (oeis @45) where
--     c x y fs'@ (f:fs) | x < f     = c (x+1) (y + (oeisIx @10051) x) fs'
--                       | otherwise = y : c (x+1) 0 fs

-- instance OEIS 52012 where
--   oeis = c 1 0 $ tail (oeis @204) where
--     c x y ls'@ (l:ls) | x < l     = c (x+1) (y + (oeisIx @10051) x) ls'
--                       | otherwise = y : c (x+1) 0 ls

-- instance OEIS 52021 where
--   oeis = tail $ filter (\x -> (oeisIx @7953) x == (oeisIx @6530) x) [1..]

-- instance OEIS 53636 where
--   oeisIx 0 = 0
--   oeisIx n = sum . zipWith (*) (map (oeisIx @10) ods)
--                  $ map ((2 ^) . (div n)) ods
--     where ods = rowT @182469 n

-- instance OEIS 54646 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @70167) $ (oeisIx @302) n

-- instance OEIS 54841 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ zipWith (*)
--                     (map ((10 ^) . subtract 1 . (oeisIx @49084)) $ (rowT @27748) n)
--                     (map fi $ (rowT @124010) n)

-- instance OEIS 55983 where
--   oeis = iterate (oeisIx @102487 . (+ 1)) 10

-- instance OEIS 56867 where
--   oeis = filter (\x -> gcd x (oeisIx @173557 x) == 1) [1..]

-- instance OEIS 57533 where
--   oeis = filter (\z -> p z [z]) [1..] where
--      p x ts = y > 0 && (y `elem` ts || p y (y:ts)) where y = (oeisIx @48050) x

-- instance OEIS 57661 where
--   oeisIx (succ->n) = (oeisIx @51193) n `div` n

-- instance OEIS 57891 where
--   oeis = filter ((== 0) . (oeisIx @178225) . (oeisIx @265)) [1..]

-- instance OEIS 58199 where
--   oeisIx n = fromJust $ findIndex (n <=) $ map negate (oeis @51950)

-- instance OEIS 58529 where
--   oeis = filter (\x -> all (`elem` (takeWhile (<= x) (oeis @1132)))
--                                    $ (rowT @27748) x) [1..]

-- instance OEIS 58972 where
--   oeis = 1 : do map numerator $ filter ((f [])) [1..] where
--      f ys q = denominator y == 1 || not (y `elem` ys) && f (y : ys) y
--               where y = (oeisIx @1065) q' % (oeisIx @5) q'
--                     q' = numerator q + denominator q

-- instance OEIS 58977 where
--   oeisIx = numerator . until ((== 1) . denominator) f . f . fi
--      where f x = (oeisIx @8472) z % (oeisIx @1221) z
--                  where z = numerator x + denominator x

-- instance OEIS 58988 where
--   oeisIx n = numerator $ fst $
--     until ((== 1) . denominator . fst) f $ f (fi n, []) where
--     f (x, ys) = if y `elem` ys then (0, []) else (y, y:ys) where
--      y = numerator x * denominator x % (oeisIx @5) (numerator x + denominator x)

-- instance OEIS 59009 where
--   oeis = 0 : filter (odd . (oeisIx @23416)) [1..]

-- instance OEIS 60355 where
--   oeis = map (oeisIx @1694) $ (map succ $ elemIndices 1 $ tail $ oeis @76446)

-- instance OEIS 60681 where
--   oeisIx n = div n p * (p - 1) where p = (oeisIx @20639) n

-- instance OEIS 61775 where
--   oeis = 1 : g 2 where
--      g x = y : g (x + 1) where
--         y = if t > 0 then (oeisIx @61775) t + 1 else (oeisIx @61775) u + (oeisIx @61775) v - 1
--             where t = (oeisIx @49084) x; u = (oeisIx @20639) x; v = x `div` u

-- instance OEIS 63108 where
--   oeis = iterate (oeisIx @63114) 1

-- instance OEIS 63574 where
--   oeisIx n = fst $ until ((== 1) . flip mod 4 . snd)
--                           (\ (u, v) -> (u + 1, (oeisIx @7494) v)) (0, n)

-- instance OEIS 63637 where
--   oeis = filter ((== 1) . (oeisIx @64911) . (+ 2)) (oeis @40)

-- instance OEIS 63638 where
--   oeis = map (+ 2) $ filter ((== 1) . (oeisIx @64911)) (oeis @40976)

-- instance OEIS 63776 where
--   oeisIx (succ->n) = (oeisIx @53636) n `div` n

-- instance OEIS 63947 where
--   oeis = filter ((== 1) . denominator . hm . (rowT @77609)) [1..]
--      where hm xs = genericLength xs / sum (map (recip . fi) xs)

-- instance OEIS 63962 where
--   oeisIx 0 = 0
--   oeisIx (succ->n) = genericLength [p | p <- (rowT @27748) n, p ^ 2 <= n]

-- instance OEIS 64372 where
--   oeisIx 0 = 1
--   oeisIx n = sum $ map (oeisIx @64372) $ (rowT @124010 . succ) n

-- instance OEIS 64427 where
--   oeisIx 0 = 1
--   oeisIx (succ->n) = (oeisIx @720) (n - 1) + n

-- instance OEIS 64553 where
--   oeisIx 0 = 1
--   oeisIx (succ->n) = product $ map ((+ 1) . (oeisIx @49084)) $ (rowT @27746) n

-- instance OEIS 64702 where
--   oeis = filter (\x -> (oeisIx @10888) x == (oeisIx @31347) x) [1..]

-- instance OEIS 64745 where
--   oeisIx n = fromJust (elemIndex n (oeis @64736)) + 1

-- instance OEIS 64956 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @64417)) + 1

-- instance OEIS 64987 where
--   oeisIx n = (oeisIx @203) n * n

-- instance OEIS 65307 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @65306)) + 1

-- instance OEIS 65338 where
--   oeisIx 1 = 1
--   oeisIx n = (spf `mod` 4) * (oeisIx @65338) (n `div` spf) where spf = (oeisIx @20639) n

-- instance OEIS 66527 where
--   oeis = filter ((== 1) . (oeisIx @10054) . succ) (oeis @7504)

-- instance OEIS 67953 where
--   oeisIx n = p [1..n] $ (oeisIx @40) n where
--      p _  0 = 1
--      p [] _ = 0
--      p (k:ks) m | m < k = 0 | otherwise = p ks (m - k) + p ks m

-- instance OEIS 68074 where
--   oeisIx n | odd n     = - (oeisIx @48691) n
--             | otherwise = 2 * (oeisIx @48691) (n `div` 2) - (oeisIx @48691) n

-- instance OEIS 68494 where
--   oeisIx n = mod n $ (oeisIx @10) n

-- instance OEIS 68781 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @261869

-- instance OEIS 68901 where
--   oeisIx (succ->n) = head $
--      filter ((== 0) . (`mod` fi n) . (+ (oeisIx @40) n)) $ [0..]

-- instance OEIS 68919 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @56868)

-- instance OEIS 68997 where
--   oeis = filter (\x -> mod x (oeisIx @173557 x) == 0) [1..]

-- instance OEIS 69056 where
--   oeis = filter (\x -> x ^ 2 `mod` (oeisIx @46970) x == 0) [1..]

-- instance OEIS 69059 where
--   oeis = filter ((> 1) . (oeisIx @9194)) [1..]

-- instance OEIS 69283 where
--   oeisIx 0 = 0
--   oeisIx n = genericLength $ tail $ (rowT @182469) n

-- instance OEIS 69288 where
--   oeisIx n = genericLength $ takeWhile (<= (oeisIx @196) n) $ (rowT @182469) n

-- instance OEIS 69289 where
--   oeisIx n = sum $ takeWhile (<= (oeisIx @196) n) $ (rowT @182469) n

-- instance OEIS 69488 where
--   oeis = filter f $ dropWhile (<= 100) (oeis @38618) where
--      f x = x < 10 || (oeisIx @10051) (x `mod` 100) == 1 && f (x `div` 10)

-- instance OEIS 69513 where
--   oeisIx 1 = 0
--   oeisIx n = (oeisIx @10055) n

-- instance OEIS 69715 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @52423

-- instance OEIS 70005 where
--   oeis = filter ((== 0) . (oeisIx @10055)) (oeis @78174)

-- instance OEIS 70072 where
--   oeisIx n = genericLength [ () | x <- [1..n], y <- [1..x], (oeisIx @8966) (x*y) == 1]

-- instance OEIS 71139 where
--   oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) [2..]

-- instance OEIS 71140 where
--   oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) (oeis @24619)

-- instance OEIS 71892 where
--   oeisIx (succ->n) = lcm n $ (oeisIx @71888) n

-- instance OEIS 71931 where
--   oeis = filter f (oeis @2312) where
--      f x = 2 * gpf <= (oeisIx @6530) (gpf ^ 2 + 1) where gpf = (oeisIx @6530) x

-- instance OEIS 72086 where
--   oeisIx (succ->n) = fst $
--      until ((== 1) . snd) (\ (i, x) -> (i + 1, (oeisIx @72084) x)) (0, n)

-- instance OEIS 72087 where
--   oeisIx 0 = 1
--   oeisIx (succ->n) = product $ map (oeisIx @61712) $ (rowT @27746) n

-- instance OEIS 72202 where
--   oeis = [x | x <- [1..], (oeisIx @83025) x == (oeisIx @65339) x]

-- instance OEIS 72211 where
--   oeis = 1 : zipWith div (tail (oeis @217863)) (oeis @217863)

-- instance OEIS 72403 where
--   oeis = map denominator $ scanl1 (-) $
--     map ((1 %) . (oeisIx @244)) $ (oeis @29837)

-- instance OEIS 72513 where
--   oeisIx (succ->n) = product $ map (n -) $ (rowT @27751) n

-- instance OEIS 72941 where
--   oeisIx n = product $ zipWith (^) ps $ map (max 1) es where
--               (ps, es) = unzip $ dropWhile ((== 0) . snd) $
--                          zip (oeis @40) $ (rowT @67255) n

-- instance OEIS 73121 where
--   oeisIx n = (oeisIx @53644) n * (fi n + 2 * (oeisIx @53645) n)

-- instance OEIS 73483 where
--   oeisIx n = product $ filter ((> 0) . (mod m)) $
--      dropWhile (<= (oeisIx @20639) m) $ takeWhile (<= (oeisIx @6530) m) (oeis @40)
--      where m = (oeisIx @5117) n

-- instance OEIS 73490 where
--   oeisIx = f . succ where
--     f (succ->n) = genericLength $ filter (> 1) $ zipWith (-) (tail ips) ips
--       where ips = map (oeisIx @49084) $ (rowT @27748) n

-- instance OEIS 73491 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @73490

-- instance OEIS 73492 where
--   oeis = filter ((> 0) . (oeisIx @73490)) [1..]

-- instance OEIS 73493 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @73490

-- instance OEIS 73494 where
--   oeis = map succ $ elemIndices 2 $ tail $ oeis @73490

-- instance OEIS 73495 where
--   oeis = map succ $ elemIndices 3 $ tail $ oeis @73490

-- instance OEIS 75106 where
--   oeisIx (succ.succ->n) = denominator $ n % (oeisIx @523) n

-- instance OEIS 77609 where
--   oeis = tablList @77609
-- instance Table 77609 where
--   rowCol n k = (rowT @77609) n !! (k- 1)
--   rowT n = filter
--      (\d -> d == 1 || null (rowT @77609 d \\ (rowT @213925) n)) $ (rowT @27750) n
--   tabf = map (rowT @77609) [1..]

-- instance OEIS 78179 where
--   oeisIx (succ.succ->n) = n ^ (oeisIx @78178 n) + n - 1

-- instance OEIS 79062 where
--   oeis = 2 : f 2 (tail (oeis @40)) where
--      f x ps = q : f q qs where
--        (q:qs) = dropWhile (\p -> (oeisIx @75802) (p - x) == 0 || p - x == 1) ps

-- instance OEIS 80788 where
--   oeis = filter f (oeis @40) where
--      f x = all (`elem` [0,1,6,8,9]) ds && x' /= x && (oeisIx @10051) x' == 1
--        where x' = foldl c 0 ds
--              c v 6 = 10*v + 9; c v 9 = 10*v + 6; c v d = 10*v + d
--              ds = unfoldr d x
--              d z = if z == 0 then Nothing else Just $ swap $ divMod z 10

-- instance OEIS 81238 where
--   oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
--                            (oeisIx @8683) u * (oeisIx @8683) v == -1]

-- instance OEIS 81239 where
--   oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
--                            (oeisIx @8683) u * (oeisIx @8683) v == 0]

-- instance OEIS 81240 where
--   oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
--                            (oeisIx @8683) u * (oeisIx @8683) v == 1]

-- instance OEIS 81382 where
--   oeisIx 1 = 1
--   oeisIx n = head [x | let sopf = (oeisIx @8472) n, x <- [n+1..], (oeisIx @8472) x == sopf]

-- instance OEIS 81619 where
--   oeis = filter ((== 1) . (oeisIx @10054) . (oeisIx @5)) [1..]

-- instance OEIS 81770 where
--   oeis = filter ((== 1) . (oeisIx @8966) . (`div` 4)) (oeis @17113)

-- instance OEIS 82763 where
--   oeis = filter (containsL . (oeisIx @61493)) [1..3999] where
--      containsL x = d == 4 || x > 0 && containsL x' where
--                    (x',d) = divMod x 10

-- instance OEIS 83752 where
--   oeisIx n = head [k | k <- [n+1..], (oeisIx @10052) (12* (k+n)^2 + k*n) == 1]

-- instance OEIS 84116 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @84115

-- instance OEIS 84933 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @84937)) + 1

-- instance OEIS 85239 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @6899) n `mod` 2 + 2

-- instance OEIS 85604 where
--   oeis = tablList @85604
-- instance Table 85604 where
--   rowCol = rowCol_off @85604 @2 @1
--   rowT 1 = [0]
--   rowT n = (rowT @115627) n ++ (take $ (oeisIx @62298) $ fi n) [0,0..]
--   tabl = map (rowT @85604) [1..]

-- instance OEIS 85730 where
--   oeisIx 0 = 1
--   oeisIx (succ->n) = (p - 1) * p ^ (e - 1)
--      where p =  (oeisIx @25473) n; e =  (oeisIx @25474) n

-- instance OEIS 87875 where
--   oeis = 1 : 1 : zipWith (+)
--      (map (oeisIx @87875) $ zipWith (-) [3..] (oeis @87875))
--      (map (oeisIx @720) $ zipWith (-) [3..] $ tail (oeis @720))

-- instance OEIS 88380 where
--   oeisIx (succ->n) = (oeis @88382) !! (n - 1)
--   oeis = [x | x <- [1..], x <= (oeisIx @20639) x ^ 3]

-- instance OEIS 88381 where
--   oeis = filter f [1..] where
--                         f x = p ^ 2 < div x p  where p = (oeisIx @20639) x

-- instance OEIS 88631 where
--   oeisIx n = (oeisIx @60265) n - n

-- instance OEIS 89341 where
--   oeis = filter (\x -> (oeisIx @6530) x < 2 * (oeisIx @20639) x) (oeis @24619)

-- instance OEIS 90050 where
--   oeis = [x | x <- [1..], (oeisIx @87117) x == (oeisIx @38374) x]

-- instance OEIS 91191 where
--   oeis = filter f [1..] where
--      f x = sum pdivs > x && all (<= 0) (map (\d -> (oeisIx @203) d - 2 * d) pdivs)
--            where pdivs = (rowT @27751) x

-- instance OEIS 91376 where
--   oeis = [x | x <- (oeis @2808), (oeisIx @1222.pred) x == (oeisIx @20639) x]

-- instance OEIS 92206 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @214295

-- instance OEIS 92693 where
--   oeisIx 0 = 0
--   oeisIx (succ->n) = (+ 1) $ sum $ takeWhile (/= 1) $ iterate (oeisIx @10) $ (oeisIx @10) n

-- instance OEIS 93074 where
--   oeisIx 0 = 2
--   oeisIx (succ->n) = maximum $ map (oeisIx @6530) [n - 1..n+1]

-- instance OEIS 93641 where
--   oeis = filter ((<= 2) . (oeisIx @1227)) [1..]

-- instance OEIS 93703 where
--   oeis = filter
--      ((`elem` map (oeisIx @61493) [1..3999]) . (oeisIx @4086) . (oeisIx @61493)) [1..]

-- instance OEIS 93796 where
--   oeis = concatMap (reverse . unfoldr r) $ map (oeisIx @61493) [1..3999]
--      where r 0 = Nothing
--            r x = Just ([0,1,5,10,50,100,500,1000] !! fromInteger d, x')
--                  where (x', d) = divMod x 10

-- instance OEIS 98962 where
--   oeis = 1 : f [2..] (tail (oeis @175944)) where
--      f xs'@ (x:xs) ps'@ (p:ps)
--        | (oeisIx @10051.pred) x == 1    = x : f xs (delete x ps')
--        | u == q && v == q' = x : f xs' zs
--        | otherwise         = f xs ps'
--        where q = (oeisIx @20639) x; q' = div x q
--              (us, u:us') = span (< q) ps'
--              (vs, v:vs') = span (< q') us'
--              zs@ (z:_) = us ++ vs ++ vs'
--              xs' = if z == p then xs else filter ((> 0) . (`mod` p)) xs

-- instance OEIS 99009 where
--   oeis = [x | x <- [0..], (oeisIx @151949) x == x]

-- instance OEIS 99302 where
--   oeisIx n = genericLength $ filter (== n) $ map (oeisIx @3415) [1 .. (oeisIx @2620) n]

-- instance OEIS 99619 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98962)) . (oeisIx @40).succ

-- instance OEIS 100962 where
--   oeis = filter ((== 0) . (oeisIx @64911)) (oeis @14092)

-- instance OEIS 101594 where
--   oeis = filter ((== 2) . (oeisIx @43537)) (oeis @52382)

-- instance OEIS 102466 where
--   oeis = tail [x | x <- [1..], (oeisIx @5) x == (oeisIx @1221) x + (oeisIx @1222.pred) x]

-- instance OEIS 102467 where
--   oeis = [x | x <- [1..], (oeisIx @5) x /= (oeisIx @1221) x + (oeisIx @1222.pred) x]

-- instance OEIS 103889 where
--   oeisIx n = n - 1 + 2 * mod n 2
--   oeis = concat $ transpose [tail (oeis @5843), (oeis @5408)]

-- instance OEIS 105271 where
--   oeis = [x | x <- [0..], (oeisIx @105025) x == x]

-- instance OEIS 105571 where
--   oeis = [x | x <- [3..], (oeisIx @64911) (x - 2) == 1, (oeisIx @64911) (x + 2) == 1]

-- instance OEIS 106315 where
--   oeisIx n = n * (oeisIx @5) n `mod` (oeisIx @203) n

-- instance OEIS 106372 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @106370))

-- instance OEIS 109129 where
--   oeis = 0 : 1 : g 3 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then (oeisIx @109129) t else (oeisIx @109129) r + (oeisIx @109129) s
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 109373 where
--   oeis = filter ((== 1) . (oeisIx @64911)) (oeis @88707)

-- instance OEIS 110085 where
--   oeis = filter (\x -> (oeisIx @51612) x < (oeisIx @110088) x) [1..]

-- instance OEIS 110086 where
--   oeis = filter (\x -> (oeisIx @51612) x <= (oeisIx @110088) x) [1..]

-- instance OEIS 110087 where
--   oeis = filter (\x -> (oeisIx @51612) x > (oeisIx @110088) x) [1..]

-- instance OEIS 112798 where
--   oeis = tablList @112798
-- instance Table 112798 where
--   rowCol = rowCol_off @112798 @2 @1
--   rowT   = rowT_off @112798 @2
--   tabf = map (map (oeisIx @49084)) $ tail (tabf @27746)

-- instance OEIS 114180 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @261890

-- instance OEIS 114228 where
--   oeisIx n = head [m | m <- [1..], (oeisIx @10051 . pred) ((oeisIx @40) n + 2 * (oeisIx @40) m) == 1]

-- instance OEIS 114229 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (map (oeisIx @114228) [2..])).succ

-- instance OEIS 114262 where
--   oeisIx n = head [q | let (p:ps) = drop (n - 1) (oeis @40),
--                         q <- ps, (oeisIx @10051 . pred) (p + 2 * q) == 1]

-- instance OEIS 115627 where
--   oeis = tablList @115627
-- instance Table 115627 where
--   rowCol = rowCol_off @115627 @2 @1
--   rowT = map (oeisIx @100995) . (rowT @141809) . (oeisIx @142)
--   tabf = map (rowT @115627) [2..]

-- instance OEIS 116933 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10051 . pred) (n + k * (oeisIx @79578) n) == 1]

-- instance OEIS 116934 where
--   oeisIx n = head [q | k <- [1..], let q = n + k * (oeisIx @79578) n,
--                         (oeisIx @10051 . pred) q == 1]

-- instance OEIS 117214 where
--   oeisIx n = product $
--      filter ((> 0) . (mod m)) $ takeWhile (< (oeisIx @6530) m) (oeis @40)
--      where m = (oeisIx @5117) n

-- instance OEIS 119797 where
--   oeis = f [0..] (oeis @43537) where
--      f (u:us) (v:vs@ (v':_)) | v /= v'   = f us vs
--                             | otherwise = u : f us vs

-- instance OEIS 119798 where
--   oeis = f [0..] (oeis @43537) (oeis @43537) where
--      f (z:zs) (u:us) (v:_:vs) | u /= v   = f zs us vs
--                               | otherwise = z : f zs us vs

-- instance OEIS 119799 where
--   oeis = i (oeis @119797) (oeis @119798) where
--      i xs'@ (x:xs) ys'@ (y:ys) | x < y     = i xs ys'
--                              | x > y     = i xs' ys
--                              | otherwise = x : i xs ys

-- instance OEIS 120007 where
--   oeisIx 1 = 0
--   oeisIx n | until ((> 0) . (`mod` spf)) (`div` spf) n == 1 = spf
--             | otherwise = 0
--             where spf = (oeisIx @20639) n

-- instance OEIS 120944 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @2808)

-- instance OEIS 122426 where
--   oeis = [x | x <- [1..], (oeisIx @122425) x < x]

-- instance OEIS 122427 where
--   oeisIx (succ->n) = (oeis @122426) !! (n - 1)
--   oeis = [x | x <- [1..], (oeisIx @122425) x == x]

-- instance OEIS 122428 where
--   oeis = [x | x <- [1..], (oeisIx @122425) x == (oeisIx @6530) x]

-- instance OEIS 122631 where
--   oeis =
--      1 : 2 : map (oeisIx @6530) (zipWith (+) (map ((2 *) . (oeisIx @40)) (oeis @122631))
--                                       (map (oeisIx @40) (tail (oeis @122631))))

-- instance OEIS 123087 where
--   oeis = scanl (+) 0 (oeis @96268)

-- instance OEIS 125290 where
--   oeis = filter ((> 1) . (oeisIx @43537)) (oeis @52382)

-- instance OEIS 125639 where
--   oeis = filter f [1..] where
--      f x = sx > x && (oeisIx @1065) sx > sx where sx = (oeisIx @1065) x

-- instance OEIS 126949 where
--   oeis = filter h [1..] where
--      h m = not $ null [ (x, e) | x <- [2 .. m - 2], gcd x m == 1,
--                                 e <- [2 .. (oeisIx @10) m `div` 2],
--                                 x ^ e `mod` m == m - 1]

-- instance OEIS 127626 where
--   oeis = tablList @127626
-- instance Table 127626 where
--   rowCol = rowCol_off @127626 @1 @1
--   rowT   = rowT_off   @127626 @1
--   tabl = map (map (\x -> if x == 0 then 0 else (oeisIx @18804) x)) (tabl @127093)

-- instance OEIS 129284 where
--   oeisIx 0 = 2
--   oeisIx n = (oeisIx @129151) n `div` 27

-- instance OEIS 132350 where
--   oeisIx 1 = 1
--   oeisIx n = 1 - (oeisIx @75802) n

-- instance OEIS 132431 where
--   oeisIx (succ->n) = (oeisIx @60226) n - (oeisIx @62119) n + (oeisIx @2378) (n - 1)

-- instance OEIS 133810 where
--   oeis = 1 : filter f [2..] where
--      f x = (and $ zipWith (<=) eps $ tail eps) &&
--            (all (== 1) $ zipWith (-) (tail ips) ips)
--        where ips = map (oeisIx @49084) $ (rowT @27748) x
--              eps = (rowT @124010) x

-- instance OEIS 133813 where
--   oeis = 1 : filter f [2..] where
--      f x = isPrefixOf ps (dropWhile (< (oeisIx @20639) x) (oeis @40)) &&
--              all (< 0) (zipWith (-) (tail es) es)
--            where ps = (rowT @27748) x; es = (rowT @124010) x

-- instance OEIS 135093 where
--   oeisIx 0 = 4
--   oeisIx n = (+ 1) $ fromJust $ (`elemIndex` (oeis @46665)) $ (oeisIx @30173) n

-- instance OEIS 135282 where
--   oeisIx = (oeisIx @7814) . head . filter ((== 1) . (oeisIx @209229)) . (rowT @70165).succ

-- instance OEIS 135499 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @225693

-- instance OEIS 136414 where
--   oeis = zipWith (+) (tail (oeis @7376)) $ map (10 *) (oeis @7376)

-- instance OEIS 136480 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @7814) $ n + mod n 2

-- instance OEIS 136495 where
--   oeisIx n = (fromJust $ n `elemIndex` tail (oeis @5374)) + 1

-- instance OEIS 137613 where
--   oeis =  filter (> 1) (oeis @132199)

-- instance OEIS 140470 where
--   oeis = filter
--      (\x -> all (== 0) $ map ((mod x) . (+ 1)) $ (rowT @27748) x) [1..]

-- instance OEIS 140480 where
--   oeis = filter
--       ((== 1) . (oeisIx @10052) . (\x -> (oeisIx @1157) x `div` (oeisIx @5) x)) (oeis @20486)

-- instance OEIS 141164 where
--   oeis = map succ $ elemIndices 1 $ map (oeisIx @188172) [1..]

-- instance OEIS 141707 where
--   oeisIx n = head [k | k <- [1, 3 ..], (oeisIx @178225) (k * (2 * n - 1)) == 1]

-- instance OEIS 141708 where
--   oeisIx n = (oeisIx @141707) n * (2 * n - 1)

-- instance OEIS 141709 where
--   oeisIx (succ->n) = until ((== 1) . (oeisIx @178225) . (oeisIx @265)) (+ n) n

-- instance OEIS 141766 where
--   oeis = filter f [1..] where
--      f x = all (== 0) $ map (mod x) $ (map pred ps) ++ (map succ ps)
--            where ps = (rowT @27748.succ) x

-- instance OEIS 141767 where
--   oeis = filter f [1..] where
--      f x = all (== 0) $
--            map (mod x) $ zipWith (*) (map pred ps) (map succ ps)
--            where ps = (rowT @27748.succ) x

-- instance OEIS 143202 where
--   oeis = filter (\x -> (oeisIx @6530) x - (oeisIx @20639) x == 2) [1,3..]

-- instance OEIS 143215 where
--   oeisIx n = (oeisIx @40) n * (oeisIx @7504) n

-- instance OEIS 143792 where
--   oeisIx (succ->n) = genericLength $ (rowT @225243) n `intersect` (rowT @27748) (fi n)

-- instance OEIS 144100 where
--   oeis = filter (\x -> (oeisIx @144907) x < x) [1..]

-- instance OEIS 145784 where
--   oeis = filter ((== 0) . (oeisIx @10872) . (oeisIx @1222.pred)) [1..]

-- instance OEIS 156596 where
--   oeisIx (succ->n) = (oeis @143667) !! (n - 1)
--   oeis = f (oeis @3849) where
--      f (0:0:ws) = 0 : f ws; f (0:1:ws) = 1 : f ws; f (1:0:ws) = 2 : f ws

-- instance OEIS 157931 where
--   oeis = filter ((== 1) . (oeisIx @64911)) (oeis @14091)

-- instance OEIS 160676 where
--   oeis = filter (\x -> (oeisIx @6968) x == (oeisIx @6968) (2 * x)) [1..]

-- instance OEIS 161764 where
--   oeisIx n = n - (oeisIx @199238) n

-- instance OEIS 162643 where
--   oeis = filter ((== 0) . (oeisIx @209229) . (oeisIx @5)) [1..]

-- instance OEIS 164652 where
--   oeis = tablList @164652
-- instance Table 164652 where
--   tabl = [0] : tail (zipWith (zipWith (*)) (tabl @128174) $
--      zipWith (map . flip div) (tail (oeis @217)) (map init $ tail (tabl @130534)))

-- instance OEIS 165712 where
--   oeisIx (succ->n)
--     = head [x | x <- [n + 1 ..], (oeisIx @1222.pred) x == (oeisIx @1222.pred) n]

-- instance OEIS 165713 where
--   oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1221) x == (oeisIx @1221) n]

