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


{-

instance OEIS 171462 where
  oeisIx n = div n p * (p - 1) where p = (oeisIx @6530) n

instance OEIS 171942 where
  oeisIx 1 = 0
  oeisIx n = head [m | m <- [1..], (oeisIx @120) (m + n - 1) == (oeisIx @120) (n - 1)]

instance OEIS 173525 where
  oeisIx = (+ 1) . (oeisIx @53824) . (subtract 1)

instance OEIS 173694 where
  oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @2322)) [1..]

instance OEIS 175944 where
  oeis = concat $ zipWith ($) (map replicate (oeis @18252)) (oeis @18252)

instance OEIS 177904 where
  oeis = 1 : 1 : 1 : (map (oeisIx @6530) $ zipWith (+)
     (oeis @177904) (tail $ zipWith (+) (oeis @177904) $ tail (oeis @177904)))

instance OEIS 178609 where
  oeisIx (succ.succ->n) = head [k | k <- [n - 1, n - 2 .. 0], let p2 = 2 * (oeisIx @40) n,
                        (oeisIx @40) (n - k) + (oeisIx @40) (n + k) == p2]

instance OEIS 178953 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @178609

instance OEIS 179248 where
  oeis = map succ $ elemIndices 9 $ tail $ oeis @7895

instance OEIS 179249 where
  oeis = map succ $ elemIndices 9 $ tail $ oeis @7895

instance OEIS 179250 where
  oeis = map succ $ elemIndices 10 $ tail $ oeis @7895

instance OEIS 179251 where
  oeis = map succ $ elemIndices 11 $ tail $ oeis @7895

instance OEIS 179253 where
  oeis = map succ $ elemIndices 13 $ tail $ oeis @7895

instance OEIS 179627 where
  oeisIx = (+ 1) . (oeisIx @6666).pred . (oeisIx @40).succ

instance OEIS 181424 where
  oeisIx = (oeisIx @40) . (+ 2) . (oeisIx @64113)

instance OEIS 181522 where
  oeisIx = genericLength . filter ((== 1) . (oeisIx @64911.pred) . sum) .
                            subsequences . enumFromTo 1

instance OEIS 181819 where
  oeisIx = product . map (oeisIx @40.pred) . (rowT @124010) . succ

instance OEIS 182061 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @61373))

instance OEIS 182426 where
  oeis = concatMap f $ group $ zipWith (-) (tail ips) ips where
     f xs | head xs == 1 = reverse $ enumFromTo 2 $ length xs + 1
          | otherwise    = take (length xs) $ repeat 1
     ips = map (oeisIx @49084) (oeis @166251)

instance OEIS 182850 where
  oeisIx n = genericLength $ takeWhile (`notElem` [1,2]) $ iterate (oeisIx @181819) n

instance OEIS 184162 where
  oeis = 1 : g 2 where
     g x = y : g (x + 1) where
       y = if t > 0 then 2 * (oeisIx @184162) t + 1 else (oeisIx @184162) r + (oeisIx @184162) s - 1
           where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 185038 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @90895)

instance OEIS 186253 where
  oeis = map succ $ elemIndices 0 $ oeis @261301

instance OEIS 187059 where
  oeisIx = (oeisIx @7814) . (oeisIx @1142)

instance OEIS 187090 where
  oeisIx n = until ((== 9) . (oeisIx @30)) (+ n) n

instance OEIS 187204 where
  oeis = map (+ 1) $ elemIndices 0 $ map (oeisIx @187202.pred) [1..]

instance OEIS 187285 where
  oeisIx n = until ((== 1) . (oeisIx @30)) (+ n) n

instance OEIS 188069 where
  oeis = (map succ $ elemIndices 2 $ tail $ oeis @7538)

instance OEIS 188070 where
  oeis = map succ $ elemIndices 3 $ tail $ oeis @7538

instance OEIS 188145 where
  oeis = elemIndices 0 $ zipWith3 (\x y z -> x - y - z)
     (map (oeisIx @3415) (oeis @3415)) (oeis @3415) [0..]

instance OEIS 188163 where
  oeisIx n = succ $ fromJust $ elemIndex n (oeis @4001)

instance OEIS 188226 where
  oeis =
     map (succ . fromJust . (`elemIndex` (map (oeisIx @188172) [1..]))) [0..]

instance OEIS 188968 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @188967

instance OEIS 188969 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @188967

instance OEIS 189419 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @114183))

instance OEIS 189639 where
  oeisIx (succ->n) = (oeis @189710) !! (n - 1)
  oeis = elemIndices 0 $
     zipWith (-) (map (oeisIx @3415) (oeis @3415)) (map pred (oeis @3415))

instance OEIS 191967 where
  oeisIx n = n * (oeisIx @1651) n

instance OEIS 193169 where
  oeisIx = genericLength . filter odd . (rowT @27750) . (oeisIx @2322)

instance OEIS 193671 where
  oeis = map (+ 1) $ findIndices (> 0) $ map (oeisIx @187202) [1..]

instance OEIS 193672 where
  oeis = map (+ 1) $ findIndices (< 0) $ map (oeisIx @187202) [1..]

instance OEIS 193738 where
  oeis = tablList @193738
instance Table 193738 where
  tabl = map reverse (tabl @193739)

instance OEIS 193739 where
  oeis = tablList @193739
instance Table 193739 where
  rowCol n k = (tabl @193738) !! n !! k
  rowT n = (tabl @193738) !! n
  tabl = map reverse (tabl @193739)

instance OEIS 193927 where
  oeis = findIndices (< 0) (oeis @193926)

instance OEIS 194626 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @81827)

instance OEIS 195069 where
  oeis = map succ $ elemIndices 10 $ tail $ oeis @46660

instance OEIS 195086 where
  oeis = map succ $ elemIndices 2 $ tail $ oeis @46660

instance OEIS 195087 where
  oeis = map succ $ elemIndices 3 $ tail $ oeis @46660

instance OEIS 195088 where
  oeis = map succ $ elemIndices 4 $ tail $ oeis @46660

instance OEIS 195089 where
  oeis = map succ $ elemIndices 5 $ tail $ oeis @46660

instance OEIS 195090 where
  oeis = map succ $ elemIndices 6 $ tail $ oeis @46660

instance OEIS 195091 where
  oeis = map succ $ elemIndices 7 $ tail $ oeis @46660

instance OEIS 195092 where
  oeis = map succ $ elemIndices 8 $ tail $ oeis @46660

instance OEIS 195093 where
  oeis = map succ $ elemIndices 9 $ tail $ oeis @46660

instance OEIS 195106 where
  oeis = filter (\x -> (oeisIx @6530) x - (oeisIx @20639) x == 4) [1,3..]

instance OEIS 195758 where
  oeisIx = (oeisIx @20639) . (oeisIx @16105)

instance OEIS 195759 where
  oeisIx = (oeisIx @6530) . (oeisIx @16105)

instance OEIS 195942 where
  oeis = filter (\x -> (oeisIx @10051 . pred) x == 0 && (oeisIx @10055) x == 1) (oeis @52382)

instance OEIS 195943 where
  oeis = filter ((== 1) . (oeisIx @10055)) (oeis @52382)

instance OEIS 196047 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y = if t > 0 then (oeisIx @196047) t + (oeisIx @61775) t else (oeisIx @196047) r + (oeisIx @196047) s
           where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196048 where
  oeis = 0 : 1 : g 3 where
     g x = y : g (x + 1) where
       y = if t > 0 then (oeisIx @196048) t + (oeisIx @109129) t else (oeisIx @196048) r + (oeisIx @196048) s
           where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196049 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @196049) t + (oeisIx @64911) t
         | otherwise = (oeisIx @196049) r + (oeisIx @196049) s + (oeisIx @64911) s
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196050 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y = if t > 0 then (oeisIx @196050) t + 1 else (oeisIx @196050) r + (oeisIx @196050) s
           where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196051 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @196051) t + (oeisIx @196047) t + (oeisIx @196050) t + 1
         | otherwise = (oeisIx @196051) r + (oeisIx @196051) s +
                       (oeisIx @196047) r * (oeisIx @196050) s + (oeisIx @196047) s * (oeisIx @196050) r
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196052 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y = if t > 0 then (oeisIx @1222) t + 1 else (oeisIx @196052) r + (oeisIx @196052) s
           where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196053 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @196053) t + 2 + 2 * (oeisIx @1222) t
         | otherwise = (oeisIx @196053) r + (oeisIx @196053) s -
                       (oeisIx @1222) r ^ 2  - (oeisIx @1222) s ^ 2 + (oeisIx @1222) x ^ 2
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196057 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @196057) t
         | otherwise = (oeisIx @196057) r + (oeisIx @196057) s + (oeisIx @1222) r * (oeisIx @1222) s
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196062 where
  oeis = 0 : 1 : g 3 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @196062) t
         | otherwise = (oeisIx @196062) r + (oeisIx @196062) s - 0 ^ (x `mod` 4)
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196068 where
  oeis = 1 : g 2 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @196068) t + (oeisIx @61775) t + 1
         | otherwise = (oeisIx @196068) r + (oeisIx @196068) s - 1
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 198332 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @198332) t + 2 * (oeisIx @1222) t
         | otherwise = (oeisIx @198332) r + (oeisIx @198332) s + 2 * (oeisIx @1222) r * (oeisIx @1222) s
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 198384 where
  oeis = map (^ 2) (oeis @198388)

instance OEIS 198387 where
  oeis = zipWith (-) (oeis @198385) (oeis @198384)

instance OEIS 198409 where
  oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @8966) $
     zipWith gcd (oeis @198384) $ zipWith gcd (oeis @198385) (oeis @198386)

instance OEIS 198435 where
  oeis = map (oeisIx @198384) (oeis @198409)

instance OEIS 198436 where
  oeis = map (oeisIx @198385) (oeis @198409)

instance OEIS 198437 where
  oeis = map (oeisIx @198386) (oeis @198409)

instance OEIS 198438 where
  oeis = map (oeisIx @198387) (oeis @198409)

instance OEIS 198439 where
  oeis = map (oeisIx @198388) (oeis @198409)

instance OEIS 198440 where
  oeis = map (oeisIx @198389) (oeis @198409)

instance OEIS 198441 where
  oeis = map (oeisIx @198390) (oeis @198409)

instance OEIS 199581 where
  oeisIx n = (rowT @199333) (2*n) !! n

instance OEIS 199694 where
  oeisIx = sum . (rowT @199333) . succ

instance OEIS 199695 where
  oeisIx = product . (rowT @199333) . succ

instance OEIS 200087 where
  oeisIx n = (fromJust $ elemIndex n (oeis @79878)) + 1

instance OEIS 202014 where
  oeisIx n = (fromJust $ elemIndex n (oeis @63882)) + 1

instance OEIS 202016 where
  oeis = map (+ 1) $ elemIndices 1 (oeis @132157)

instance OEIS 203814 where
  oeisIx n = genericLength [x | x <- [0..n], (oeisIx @43537) x == (oeisIx @43537) n]

instance OEIS 203967 where
  oeisIx n = genericLength $ takeWhile (<= n) (oeis @9087)

instance OEIS 205666 where
  oeis = [x | x <- [1..], (oeisIx @65306) x == fi x]

instance OEIS 206498 where
  oeisIx 0 = 0
  oeisIx 1 = 2
  oeisIx (succ->x) = if t > 0 then (oeisIx @196062) t + t `mod` 2 else (oeisIx @196062) x
              where t = (oeisIx @49084) x

instance OEIS 206941 where
  oeisIx = (oeisIx @10) . (oeisIx @2322)

instance OEIS 207193 where
  oeisIx 0 = 1
  oeisIx (succ->n) | p == 2 && e > 2 = 2 ^ (e - 2)
            | otherwise       = (p - 1) * p ^ (e - 1)
            where p = (oeisIx @25473) n; e = (oeisIx @25474) n

instance OEIS 207852 where
  oeisIx n = (fromJust $ elemIndex n (oeis @178830)) + 1

instance OEIS 208083 where
  oeisIx = sum . map (oeisIx @10051 . pred) . (rowT @81118) . succ

instance OEIS 208091 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @208083))

instance OEIS 208570 where
  oeisIx n = lcm n $ (oeisIx @7978) n

instance OEIS 208662 where
  oeisIx n = head [m | m <- [1..], let p = (oeisIx @65091) n,
     let q = 2 * m - p, (oeisIx @10051 . pred) q == 1,
     all ((== 0) . (oeisIx @10051 . pred)) $ map (2 * m -) $ take (n - 1) (oeis @65091)]

instance OEIS 208852 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @90895))

instance OEIS 208981 where
  oeisIx = genericLength . takeWhile ((== 0) . (oeisIx @209229)) . (rowT @70165)

instance OEIS 210771 where
  oeisIx n = fromJust (elemIndex n (oeis @210770)) + 1

instance OEIS 212211 where
  oeis = tablList @212211
instance Table 212211 where
  rowCol = rowCol_off @212211 @2 @2
  tabl = map (rowT @212211) [2..]
  rowT n = zipWith (-)
     (map (+ (oeisIx @720) n) $ take (n - 1) $ tail (oeis @720))
     (drop (n + 1) (oeis @720))

instance OEIS 213911 where
  oeisIx = genericLength . filter ((== 0) . head) . group . (rowT @213676)

instance OEIS 213913 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @213912))

instance OEIS 214320 where
  oeis = 1 : 1 : 1 : (map (oeisIx @6530) $
     zipWith (+) (oeis @214320) (drop 2 $ (oeis @214320)))

instance OEIS 214511 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @45698))

instance OEIS 214567 where
  oeis = 1 : g 2 where
    g x = y : g (x + 1) where
      y | t > 0     = (oeisIx @214567) t + 1
        | otherwise = 1 + sum (map ((subtract 1) . (oeisIx @214567)) $ (rowT @27748) x)
         where t = (oeisIx @49084) x

instance OEIS 217863 where
  oeisIx = (oeisIx @10) . (oeisIx @3418)

instance OEIS 218454 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @176352))

instance OEIS 218494 where
  oeisIx = p (tail (oeis @290)) . (^ 3) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 219908 where
  oeisIx (succ->n) = (oeis @219907) !! (n - 1)
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @219907)

instance OEIS 220096 where
  oeisIx (succ->n) = if z == 1 then n - 1 else z  where z = (oeisIx @32742) n

instance OEIS 220264 where
  oeisIx n = fromJust $ find ((== n) . (oeisIx @86971)) (oeis @220423)

instance OEIS 222208 where
  oeis = 1 : 3 : f 3 (2 : [4 ..]) where
     f u vs = g vs where
       g (w:ws) = if all (== 0) $ map ((mod w) . (oeisIx @222208)) $ (rowT @27751) u
                     then w : f (u + 1) (delete w vs) else g ws

instance OEIS 222209 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @222208))

instance OEIS 222581 where
  oeis = map length $ group (oeis @93796)

instance OEIS 222622 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @113966))

instance OEIS 222623 where
  oeis = filter (\x -> (oeisIx @113966) x == x) [1..]

instance OEIS 224363 where
  oeisIx = (oeisIx @40) . (oeisIx @221056)

instance OEIS 224401 where
  oeisIx = (+ 1) . fromJust . (`findIndex` (tabf @85612)) . elem

instance OEIS 224458 where
  oeis = 0 : g 2 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @224458) t + (oeisIx @1222) t
         | otherwise = (oeisIx @224458) r + (oeisIx @224458) s + (oeisIx @1222) r * (oeisIx @1222) s
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 224981 where
  oeis = filter (p 6 $ tail (oeis @290)) [1..] where
     p k (q:qs) m = k == 0 && m == 0 ||
                    q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

instance OEIS 224982 where
  oeis = filter (p 7 $ tail (oeis @290)) [1..] where
     p k (q:qs) m = k == 0 && m == 0 ||
                    q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

instance OEIS 224983 where
  oeis = filter (p 8 $ tail (oeis @290)) [1..] where
     p k (q:qs) m = k == 0 && m == 0 ||
                    q <= m && k >= 0 && (p (k - 1) qs (m - q) || p k qs m)

instance OEIS 225047 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121216))

instance OEIS 225124 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @25586)) . (oeisIx @79)

instance OEIS 225395 where
  oeisIx n = product $ zipWith (^)
      (map (oeisIx @49084) $ (rowT @27748) n) (map (oeisIx @225395) $ (rowT @124010) n)

instance OEIS 225589 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @217122))

instance OEIS 226030 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226029))

instance OEIS 226245 where

instance OEIS 226387 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @85612))

instance OEIS 226483 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @224909))

instance OEIS 226778 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @55483

instance OEIS 227114 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @227113))

instance OEIS 227289 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @227288))

instance OEIS 227296 where
  oeisIx n = p [1 .. (oeisIx @10) n] n where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 227388 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226390))

instance OEIS 227413 where
  oeis = 1 : concat (transpose [map (oeisIx @40) (oeis @227413),
                                        map (oeisIx @2808) (oeis @227413)])

instance OEIS 227633 where

instance OEIS 227836 where
  oeisIx = (oeisIx @7814) . (oeisIx @214551)

instance OEIS 227837 where
  oeisIx = (oeisIx @7949) . (oeisIx @214551)

instance OEIS 227838 where
  oeisIx = (oeisIx @7814) . (oeisIx @5132)

instance OEIS 227839 where
  oeisIx = (oeisIx @7949) . (oeisIx @5132)

instance OEIS 227944 where
  oeisIx n = fst $
              until ((== 1) . snd) (\ (i, x) -> (i + 1, (oeisIx @53575) x)) (0, n)

instance OEIS 227946 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @227944))

instance OEIS 230720 where
  oeisIx = (oeisIx @3071) . (oeisIx @92246)
  oeis = filter even (oeis @3071)

instance OEIS 230721 where
  oeisIx = (oeisIx @3071) . (oeisIx @230709)
  oeis = filter odd (oeis @3071)

instance OEIS 230780 where
  oeis = filter (all (/= 1) . map (flip mod 6) . (rowT @27748).pred) [1..]

instance OEIS 232361 where

instance OEIS 232643 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @232642))

instance OEIS 233281 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @1177).pred) [1..]

instance OEIS 234098 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $
                        map ((flip div 2) . (+ 1)) (oeis @46388)

instance OEIS 234587 where

instance OEIS 235249 where
  oeisIx n = if y == n then n else (oeisIx @235249) y  where y = (oeisIx @1175) n

instance OEIS 235540 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @158034)

instance OEIS 236246 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @229037

instance OEIS 236341 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @160855))

instance OEIS 237739 where
  oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @71574))

instance OEIS 238525 where
  oeisIx (succ->n) = mod n $ (oeisIx @1414) n

instance OEIS 238718 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @238704))

instance OEIS 238862 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @239965))

instance OEIS 239122 where
  oeis = drop 2 $ scanl1 (+) (oeis @61019)

instance OEIS 239324 where
  oeis = scanl (+) 0 (oeis @90431)

instance OEIS 239433 where
  oeis = filter
     (\z -> any (== z) $ map (oeisIx @3415) $ takeWhile (<= (oeisIx @2620) z) (oeis @13929)) [2..]

instance OEIS 239585 where
  oeisIx = (oeisIx @20639) . (oeisIx @78972)

instance OEIS 239586 where
  oeisIx n = (oeisIx @78972) n `div` (oeisIx @239585) n

instance OEIS 239943 where
  oeis = [x | x <- [1..], (oeisIx @239965) x == x]

instance OEIS 240923 where
  oeisIx (succ->n) = numerator sq - (oeisIx @203) (denominator sq)
     where sq = (oeisIx @203) n % n

instance OEIS 240952 where
  oeisIx = fromJust . (`elemIndex` (oeis @245575))

instance OEIS 240960 where
  oeis = filter (\x -> (oeisIx @51612) x == (oeisIx @110088) x) [1..]

instance OEIS 241012 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @109465))

instance OEIS 241218 where
  oeisIx = fromJust . (`elemIndex` (oeis @240808))

instance OEIS 241235 where
  oeis = map length $ group (oeis @6949)

instance OEIS 241752 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @229037))

instance OEIS 241917 where
  oeisIx (succ->n) = i - j where
              (i:j:_) = map (oeisIx @49084) $ reverse (1 : (rowT @27746) n)

instance OEIS 241919 where
  oeisIx 0 = 0
  oeisIx (succ->n) = i - j where
              (i:j:_) = map (oeisIx @49084) $ reverse (1 : (rowT @27748) n)

instance OEIS 242411 where
  oeisIx 0 = 0
  oeisIx (succ->n) = i - j where
              (i:j:_) = map (oeisIx @49084) $ ps ++ [p]
              ps@ (p:_) = reverse $ (rowT @27748) n

instance OEIS 242622 where
  oeisIx = genericLength . (rowT @242614)

instance OEIS 242901 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @242885))

instance OEIS 244080 where
  oeisIx = (oeisIx @6530) . (oeisIx @166133)

instance OEIS 244731 where
  oeis = [x | x <- [1..], (oeisIx @244724) x == x]

instance OEIS 244732 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @244724))

instance OEIS 245304 where
  oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051 . pred)) $
     iterate (zipWith (+) [1, 1, 1, 1, 1]) [1, 3, 7, 9, 13]

instance OEIS 245543 where
  oeis = scanl1 (+) (oeis @160239)

instance OEIS 246431 where
  oeisIx = fromJust . (`elemIndex` (oeis @101403))

instance OEIS 246433 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @113963))

instance OEIS 246517 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @141036)) [0..]

instance OEIS 246518 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ (oeis @141036)

instance OEIS 246704 where
  oeis = filter (\x -> (oeisIx @113963) x == x) [1..]

instance OEIS 247104 where
  oeis = filter ((== 1) . (oeisIx @8966)) $ tail (oeis @3052)

instance OEIS 247180 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @67029

instance OEIS 247204 where
  oeis = filter ((zipWith (==) [1..] (oeis @250552)) !!) [1..]

instance OEIS 247253 where
  oeis = zipWith (-) (tail (oeis @251239)) (oeis @251239)

instance OEIS 247451 where
  oeis = map (oeisIx @7947) (oeis @25487)

instance OEIS 247468 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247462))

instance OEIS 247503 where
  oeisIx = product . filter (odd . (oeisIx @49084)) . (rowT @27746) . succ

instance OEIS 247793 where
  oeis = 2 : f (zip [2..] $ tail (oeis @40)) where
     f ((x, p) : xps) = m : f xps where
       m = head [y | y <- [1..], (p + (oeisIx @40) y) `mod` (oeisIx @720) (x * y) == 0]

instance OEIS 247879 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @248025))

instance OEIS 247892 where
  oeisIx (succ->n) = n - (oeisIx @247815) n

instance OEIS 248101 where
  oeisIx = product . filter (even . (oeisIx @49084)) . (rowT @27746) . succ

instance OEIS 249343 where
  oeisIx = (oeisIx @7949) . (oeisIx @1142)

instance OEIS 249346 where
  oeisIx = (oeisIx @122841) . (oeisIx @1142)

instance OEIS 249575 where
  oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @84937)) [1..]

instance OEIS 249602 where
  oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @84937)) [1..]

instance OEIS 249680 where
  oeisIx = (oeisIx @84937) . (+ 1) . (* 3)

instance OEIS 249681 where
  oeisIx = (oeisIx @84937) . (+ 2) . (* 3)

instance OEIS 249682 where
  oeisIx = (oeisIx @84937) . (* 3)

instance OEIS 249683 where
  oeisIx = flip div 2 . (oeisIx @249681)

instance OEIS 249684 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @249777

instance OEIS 249918 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @203069))

instance OEIS 249920 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @55266))

instance OEIS 250007 where
  oeis = map length $ group $ map (oeisIx @100618) [1..]

instance OEIS 250552 where
  oeisIx = (oeisIx @49084) . (oeisIx @247797)

instance OEIS 250553 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @250552)) . succ

instance OEIS 251045 where
  oeisIx = (oeisIx @7913) . (oeisIx @98550)

instance OEIS 251046 where
  oeisIx = (oeisIx @7913) . (oeisIx @98548)

instance OEIS 251089 where
  oeisIx = (oeisIx @7947) . (oeisIx @98550)

instance OEIS 251090 where
  oeisIx = (oeisIx @7947) . (oeisIx @98548)

instance OEIS 251101 where
  oeisIx = (oeisIx @20639) . (oeisIx @98550)

instance OEIS 251103 where
  oeisIx = (oeisIx @6530) . (oeisIx @98550)

instance OEIS 251104 where
  oeisIx = (oeisIx @6530) . (oeisIx @98548)

instance OEIS 251140 where
  oeisIx = (oeisIx @1222) . (oeisIx @98550)

instance OEIS 251141 where
  oeisIx = (oeisIx @1222) . (oeisIx @98548)

instance OEIS 251240 where
  oeis = filter ((== 2) . (oeisIx @62799) . (oeisIx @98550).pred) [1..]

instance OEIS 251241 where
  oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @98550).pred) [1..]

instance OEIS 251391 where
  oeis = filter ((== 1) . (oeisIx @8966) . (oeisIx @98550).pred) [1..]

instance OEIS 251392 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred) . (oeisIx @98550)) [1..]

instance OEIS 251393 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @98550)) [1..]

instance OEIS 251394 where
  oeis = filter ((== 1) . gcd 6 . (oeisIx @98550)) [1..]

instance OEIS 251412 where
  oeis = iterate (oeisIx @98550) 11

instance OEIS 251539 where
  oeis = zipWith (-) (tail (oeis @251538)) (oeis @251538)

instance OEIS 251540 where
  oeis = filter ((> 0) . flip mod 3 . (oeisIx @98548)) [1, 3 ..]

instance OEIS 251547 where
  oeisIx = flip div 2 . (oeisIx @251546) . succ

instance OEIS 251548 where
  oeis = map length $ group $ map (oeisIx @251546) [1..]

instance OEIS 251550 where
  oeisIx = flip div 2 . subtract 1 . (oeisIx @251549) . succ

instance OEIS 251551 where
  oeisIx (succ->n) = (oeisIx @251546) n - (oeisIx @251549) n

instance OEIS 251552 where
  oeisIx = flip div 2 . subtract 1 . (oeisIx @251551) . succ

instance OEIS 251561 where
  oeisIx 1 = 1
  oeisIx n | q == 1                    = 2 * p
            | p == 2 && (oeisIx @10051 . pred) q == 1 = q
            | otherwise                 = n
            where q = div n p; p = (oeisIx @20639) n

instance OEIS 251619 where
  oeisIx = (oeisIx @20639) . (oeisIx @251618)

instance OEIS 251728 where
  oeis = filter f [1..] where
                        f x = q < p ^ 2 && (oeisIx @10051 . pred) q == 1
                              where q = div x p; p = (oeisIx @20639) x

instance OEIS 251767 where
  oeis = map head $ group (oeis @251539)

instance OEIS 251768 where
  oeis = map length $ group (oeis @251539)

instance OEIS 252002 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @252001))

instance OEIS 252023 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @252022))

instance OEIS 252078 where
  oeis = [x | x <- [1..], (oeisIx @252001) x == x]

instance OEIS 252079 where
  oeis = [x | x <- [1..], (oeisIx @252022) x == x]

instance OEIS 252448 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249990))

instance OEIS 252458 where
  oeis = [x | x <- [1..], (oeisIx @249990) x == x]

instance OEIS 252912 where
  oeis = filter (\x -> (oeisIx @98550) x == (oeisIx @251555) x) [1..]

instance OEIS 252939 where
  oeis = tail $ zipWith (-) (tail (oeis @252912)) (oeis @252912)

instance OEIS 252940 where
  oeis = map length $ group (oeis @252939)

instance OEIS 253046 where
  oeisIx n | i == 0 || p > 3 = n
            | p == 2          = 3 * (oeisIx @40) (i + 1)
            | otherwise       = 2 * (oeisIx @40) (i - 1)
              where i = (oeisIx @49084) (div n p);  p = (oeisIx @20639) n

instance OEIS 253106 where
  oeis = filter f [1..] where
     f x = p <= 3 && (oeisIx @10051 . pred) (div x p) == 1  where p = (oeisIx @20639) x

instance OEIS 253138 where
  oeisIx n = sum $ map (oeisIx @64911) $
     takeWhile (> 0) $ map (2 * p -) $ dropWhile (< p) (oeis @1358)
     where p = (oeisIx @40) n

instance OEIS 253169 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @256188))

instance OEIS 253425 where
  oeis = map length $ group (oeis @253415)

instance OEIS 254650 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @254649))

instance OEIS 254656 where
  oeis = [x | x <- [1..], (oeisIx @254649) x == x]

instance OEIS 255479 where
  oeisIx = (+ 1) . fromJust. (`elemIndex` (oeis @255582))

instance OEIS 255482 where
  oeisIx n = (oeisIx @64664) n - (oeisIx @255479) n

instance OEIS 255646 where
  oeisIx = flip mod 10 . (oeisIx @46316)

instance OEIS 255678 where
  oeisIx n = head $ filter ((== n) . (oeisIx @118668)) [0..]

instance OEIS 255833 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @166133))

instance OEIS 255940 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249167))

instance OEIS 255972 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @251604))

instance OEIS 256406 where
  oeis = f (oeis @166133) where
     f (u:vs'@ (v:ws)) | u > v || v /= u ^ 2 - 1 = f vs'
                      | otherwise               = u : f ws

instance OEIS 256417 where
  oeisIx = (oeisIx @256415) . (oeisIx @64413)

instance OEIS 256419 where
  oeisIx = (oeisIx @256415) . (oeisIx @121217)

instance OEIS 256489 where
  oeis = zipWith (-) (tail (oeis @257509)) (oeis @257509)

instance OEIS 256542 where
  oeisIx = (oeisIx @5) . (oeisIx @166133)

instance OEIS 256543 where
  oeis = [x | x <- [1..], abs (oeisIx @256541 x) == 1]

instance OEIS 256564 where
  oeisIx = (oeisIx @20639) . (oeisIx @166133)

instance OEIS 256578 where
  oeisIx = (oeisIx @32742) . (oeisIx @166133)

instance OEIS 256607 where
  oeisIx = (oeisIx @7733) . (oeisIx @7733)

instance OEIS 256618 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121217))

instance OEIS 256628 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @251622))

instance OEIS 256757 where
  oeisIx (succ->n) = fst $ until ((== 1) . snd)
              (\ (i, x) -> (i + 1, fi $ (oeisIx @7733) x)) (0, n)

instance OEIS 256758 where
  oeisIx = (+ 1) . fromJust . (`elemIndex`  (oeis @256757)) . succ

instance OEIS 256786 where
  oeis = filter f (oeis @52382) where
     f x = g x where
       g z = z == 0 || x `mod` (oeisIx @40) d == 0 && g z'
             where (z', d) = divMod z 10

instance OEIS 256885 where
  oeisIx n = (oeisIx @217) n - (oeisIx @720) n

instance OEIS 257053 where
  oeis = tablList @257053
instance Table 257053 where
  rowCol = rowCol_off @257053 @1 @0
  rowT   = rowT_off @257053 @1
  tabf = map (rowT @257053) (oeis @40)

instance OEIS 257070 where
  oeisIx = last . (rowT @257053) . succ

instance OEIS 257071 where
  oeisIx = genericLength . (rowT @257053) . succ

instance OEIS 257465 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @175498))

instance OEIS 257572 where
  oeisIx = (oeisIx @20639) . (oeisIx @257278)

instance OEIS 257573 where
  oeisIx = (oeisIx @1222) . (oeisIx @257278)

instance OEIS 257646 where
  oeisIx n = fromJust $ findIndex (elem n) (tabl @103284)

instance OEIS 257762 where
  oeis = map (oeisIx @258432) $ (map succ $ elemIndices 2 $ tail $ oeis @258383)

instance OEIS 257815 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @64364))

instance OEIS 257892 where
  oeis = map (oeisIx @258432) $ (map succ $ elemIndices 4 $ tail $ oeis @258383)


instance OEIS 257951 where
  oeis = map (oeisIx @258432) $ (map succ $ elemIndices 5 $ tail $ oeis @258383)

instance OEIS 257999 where
  oeis = filter (odd . flip mod 2 . (oeisIx @1222)) (oeis @3586)

instance OEIS 258063 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258062))

instance OEIS 258091 where
  oeisIx = (oeisIx @20639) . (oeisIx @258073)

instance OEIS 258095 where
  oeis = filter
                 (\x -> (oeisIx @258091) x `notElem` [3, 5, 7, 13, 19, 37, 73]) [1..]

instance OEIS 258115 where
  oeisIx (succ->n) = (oeisIx @208570) n `div` n

instance OEIS 258125 where
  oeis = 2 : 2 : zipWith (+)
                 (map (oeisIx @6530) (oeis @258125)) (tail (oeis @258125))

instance OEIS 258227 where
  oeis = f 12 1 (map fi $ tail (oeis @7376)) where
     f x y (d:ds) | gcd x y > 1 = y : f y d ds
                  | otherwise   = f x (10 * y + d) ds

instance OEIS 258262 where
  oeis = filter ((== 1) . (oeisIx @10057)) (oeis @258865)

instance OEIS 258324 where
  oeisIx (succ->n) = foldl lcm 1 $ map (n -) $ (rowT @27751) n

instance OEIS 258353 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @212306

instance OEIS 258354 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @212306))

instance OEIS 258437 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258383))

instance OEIS 258449 where
  oeis = map (oeisIx @258432) $ (map succ $ elemIndices 3 $ tail $ oeis @258383)

instance OEIS 258567 where
  oeisIx = (oeisIx @20639) . (oeisIx @1694)

instance OEIS 258568 where
  oeisIx = (oeisIx @20639) . (oeisIx @36966)

instance OEIS 258569 where
  oeisIx = (oeisIx @20639) . (oeisIx @36967)

instance OEIS 258570 where
  oeisIx = (oeisIx @20639) . (oeisIx @69492)

instance OEIS 258571 where
  oeisIx = (oeisIx @20639) . (oeisIx @69493)

instance OEIS 258599 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258567)) . (oeisIx @40)

instance OEIS 258600 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258568)) . (oeisIx @40)

instance OEIS 258601 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258569)) . (oeisIx @40)

instance OEIS 258602 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258570)) . (oeisIx @40)

instance OEIS 258603 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258571)) . (oeisIx @40)

instance OEIS 258767 where
  oeis = 1 : f 1 [2..] where
     f x zs = g zs where
       g (y:ys) | (oeisIx @8966) (x^2 + y^2) == 1 = g ys
                | otherwise = y : f y (delete y zs)

instance OEIS 258768 where
  oeis = tail [x | x <- [1..], (oeisIx @258767) x == x]

instance OEIS 258827 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258767))

instance OEIS 259969 where
  oeisIx n = (oeis @259967) !! n
  oeis = 3 : 2 : 2 : 5 : zipWith3 (((+) .) . (+))
     (oeis @259967) (drop 2 (oeis @259967)) (drop 3 (oeis @259967))

instance OEIS 260797 where
  oeisIx = (oeisIx @98743) . (oeisIx @142) . succ

instance OEIS 260936 where
  oeis = [x | x <- [1..], (oeisIx @260933) x == x]

instance OEIS 261121 where
  oeisIx = (oeisIx @98743) . (oeisIx @2110)


instance OEIS 261333 where
  oeis = zipWith (+) (map ((* 10) . subtract 1) (oeis @256100)) (oeis @7376)

instance OEIS 261334 where
  oeisIx = fromJust . (`elemIndex` (oeis @261333))

instance OEIS 261335 where
  oeis = [x | x <- [0..], (oeisIx @261333) x == x]


instance OEIS 261525 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @31131)) . (* 2)





instance OEIS 261969 where
  oeisIx n = product $ map fst $ filter ((== emax) . snd) $ zip ps es
      where emax = maximum es
            ps = (rowT @27748) n; es = (rowT @124010) n



instance OEIS 262095 where
  oeisIx = sum . map ((1 -) . (oeisIx @64911)) . (rowT @27750) . succ

instance OEIS 262429 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @262411))

instance OEIS 262435 where
  oeis = [x | x <- [1..], (oeisIx @262411) x == x]



instance OEIS 262461 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @262460))

instance OEIS 262481 where
  oeis = filter (\x -> (oeisIx @120) x == (oeisIx @20639) x) [1..]


instance OEIS 262663 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75348))

instance OEIS 263837 where
  oeis = filter (\x -> (oeisIx @1065) x <= x) [1..]


instance OEIS 263922 where
  oeisIx = (oeisIx @51903) . (oeisIx @984)

instance OEIS 264782 where
  oeisIx (succ->n) = sum $ zipWith (^) (map (oeisIx @8683) divs) (reverse divs)
              where divs = (rowT @27750) n

instance OEIS 264856 where
  oeisIx n = fromJust $ findIndex (elem n) (tabl @125605)

instance OEIS 276127 where
  oeisIx 0 = 1
  oeisIx (succ->n) = (oeisIx @1414) $ (oeisIx @64413) n

instance OEIS 226950 where
  oeis = f (oeis @76467) S.empty where
     f (x:xs) s | S.size s'' <= 1 = f xs (x `S.insert` s)
                | otherwise     = x : f xs (x `S.insert` s)
                where s'' = S.filter ((`S.member` s) . (x -)) s'
                      (s', _) = S.split (x `div` 2) s

instance OEIS 1158 where
  oeisIx n = product $ zipWith (\p e -> (p^ (3*e + 3) - 1) `div` (p^3 - 1))
                        ((rowT @27748) n) ((rowT @124010) n)

instance OEIS 2322 where
  oeisIx n = foldl lcm 1 $ map ((oeisIx @207193) . (oeisIx @95874)) $
                            zipWith (^) ((rowT @27748) n) ((rowT @124010) n)

instance OEIS 2616 where
  oeisIx = flip div 2 . oeisIx @2322

instance OEIS 2964 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (map (oeisIx @2963 . pred) [1..3888]))

instance OEIS 5276 where
  oeis = filter p [1..] where
     p z = p' z [0, z] where
       p' x ts = if y `notElem` ts then p' y (y:ts) else y == z
                 where y = (oeisIx @48050) x

instance OEIS 5349 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @70635

instance OEIS 5413 where
  oeis = 1 : zipWith (*) [1 ..] (zipWith (+) (tail (oeis @5412)) (zipWith (*) [4, 6 ..] (oeis @5413)))

instance OEIS 7367 where
  oeis = map fst $ filter ((== 3) . snd) $ zip (oeis @2202) (oeis @58277)

instance OEIS 7434 where
  oeisIx (succ->n) = sum $ zipWith3 (\x y z -> x * y * z)
                    tdivs (reverse tdivs) (reverse divs)
                    where divs = (rowT @27750) n;  tdivs = map (oeisIx @10) divs

instance OEIS 7439 where
  oeis = 1 : 1 : f 2 where
     f x = (sum $ map (oeisIx @7439) $ (rowT @27750) (x - 1)) : f (x + 1)

instance OEIS 7457 where
  oeisIx n = genericLength [k | k <- [1..n - 1], gcd k n == 1, (oeisIx @8966) k == 1,
                          let j = n - k, gcd j n == 1, (oeisIx @8966) j == 1]

instance OEIS 7554 where
  oeis = 1 : f 1 where
     f x = (sum $ zipWith (*) (map (oeisIx @8683) divs)
                              (map (oeisIx @7554) $ reverse divs)) : f (x + 1)
            where divs = (rowT @27750) x

instance OEIS 7811 where
  oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051 . pred)) $
     iterate (zipWith (+) [10, 10, 10, 10]) [1, 3, 7, 9]

instance OEIS 8847 where
  oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @203) . (oeisIx @290)) [1..]

instance OEIS 9087 where
  oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

instance OEIS 10120 where
  oeisIx = (oeisIx @70167) . (oeisIx @79)

instance OEIS 10554 where
  oeisIx = (oeisIx @10) . (oeisIx @10)

instance OEIS 13638 where
  oeisIx n = (oeisIx @151799) n * (oeisIx @151800) n

instance OEIS 14701 where
  -- oeisIx 1 = 0
  oeisIx n = (oeisIx @7953) $ (oeisIx @7931) (n - 1)

instance OEIS 20486 where
  oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @5) x == 0) [1..]

instance OEIS 20487 where
  oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @203) x == 0) [1..]

instance OEIS 24556 where
  oeis = filter ((== 0) . (oeisIx @10051 . pred)) $ tail (oeis @56911)

instance OEIS 26239 where
  oeisIx 1 = 1
  oeisIx n | (oeisIx @10051) n == 1 = (oeisIx @2808) $ (oeisIx @49084) n
            | otherwise      = (oeisIx @40) $ (oeisIx @66246) n

instance OEIS 26274 where
  oeis = map (subtract 1) $ tail $ (map succ $ elemIndices 1 $ tail $ oeis @35612)

instance OEIS 30664 where
  oeisIx n = (oeisIx @7917) n * (oeisIx @7918) n

instance OEIS 31359 where
  oeisIx = (oeisIx @1615) . (subtract 1) . (* 2) . succ

instance OEIS 31877 where
  oeis = [x | x <- [1..], x `mod` 10 > 0,
                      let x' = (oeisIx @4086) x, x' /= x && x `mod` x' == 0]

instance OEIS 32358 where
  oeisIx = genericLength . takeWhile (/= 2) . (iterate (oeisIx @10))

instance OEIS 32447 where
  oeis = f [1..] (oeis @2110) [] where
     f xs'@ (x:xs) ps'@ (p:ps) us
       | x < p = f xs ps' $ O.insertBag (oeisIx @10 x, x) us
       | otherwise = map snd vs ++ f xs' ps ws
       where (vs, ws) = span ((<= (oeisIx @10) x) . fst) us

instance OEIS 32741 where
  oeisIx n = if n < 2 then 0 else (oeisIx @5.pred) n - 1

instance OEIS 33549 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @90431

instance OEIS 33556 where
  oeis = iterate (\x -> 2*x - (oeisIx @151799 x)) 3


instance OEIS 33620 where
  oeis = filter chi [1..] where
     chi n = (oeisIx @136522) spf == 1 && (n' == 1 || chi n') where
        n' = n `div` spf
        spf = (oeisIx @20639) n

instance OEIS 33632 where
  oeis = filter (\x -> (oeisIx @62401) x == (oeisIx @62402) x) [1..]

instance OEIS 33950 where
  oeis = [x - 1 | x <- [1..], x `mod` (oeisIx @5) x == 0]

instance OEIS 34380 where
  oeisIx n = (oeisIx @10) n `div` (oeisIx @2322) n

instance OEIS 34404 where
  oeisIx = (oeisIx @292) . (oeisIx @2311)

instance OEIS 34684 where
  oeisIx = minimum . (rowT @141809)

instance OEIS 34710 where
  oeis = map succ . elemIndices 0 $ map (\x -> (oeisIx @7953) x - (oeisIx @7954) x) [1..]

instance OEIS 36262 where
  oeis = tablList @36262
instance Table 36262 where
  rowCol n k = delta !! (n - k) !! (k - 1)
    where
      delta = iterate
        (\pds -> zipWith (\x y -> abs (x - y)) (tail pds) pds) (oeis @40)

instance OEIS 36454 where
  oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

instance OEIS 36537 where
  oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @5)) [1..]

instance OEIS 36667 where
  oeis = filter (even . flip mod 2 . (oeisIx @1222)) (oeis @3586)

instance OEIS 36763 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @51521

instance OEIS 36786 where
  oeis = [x | x <- [1..], (oeisIx @6968) x < (oeisIx @55642) x]

instance OEIS 36787 where
  oeis = [x | x <- [1..], (oeisIx @6968) x == (oeisIx @55642) x]

instance OEIS 36788 where
  oeis = [x | x <- [1..], (oeisIx @6968) x <= (oeisIx @55642) x]

instance OEIS 36844 where
  oeis = filter ((== 0). (oeisIx @238525)) [2..]

instance OEIS 37992 where
  oeisIx n = head [x | x <- [1..], (oeisIx @5) x == 2 ^ n]

instance OEIS 38186 where
  oeis = map succ $ elemIndices 1
                 $ zipWith (*) (map (oeisIx @188641) [1..]) (map (oeisIx @188642) [1..])

instance OEIS 38567 where
  oeis = concatMap (\x -> genericTake (oeisIx @10 x) $ repeat x) [1..]

instance OEIS 38572 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @53645) n * m + n' where (n', m) = divMod n 2

instance OEIS 38575 where
  oeisIx n = if n == 0 then 0 else (oeisIx @1222) $ (oeisIx @45) n

instance OEIS 45967 where
  oeisIx 1 = 4
  oeisIx n = product $ zipWith (^)
              (map (oeisIx @151800) $ (rowT @27748) n) (map (+ 1) $ (rowT @124010) n)

instance OEIS 46099 where
  oeis = filter ((== 1) . (oeisIx @212793.pred)) [1..]

instance OEIS 46316 where
  oeis = filter ((== 3) . (oeisIx @1222)) [1, 3 ..]

instance OEIS 46388 where
  oeis = 15 : filter ((== 2) . (oeisIx @1221 . pred)) (oeis @56911)

instance OEIS 46930 where
  oeisIx 1 = 1
  oeisIx n = subtract 2 $ (oeisIx @31131) n

instance OEIS 47994 where
  oeisIx n = f n 1 where
     f 1 uph = uph
     f x uph = f (x `div` sppf) (uph * (sppf - 1)) where sppf = (oeisIx @28233) x

instance OEIS 48055 where
  oeis = [x | x <- (oeis @2808)
         , let (us,vs) = Data.List.partition
                          ((== 1) . (oeisIx @10051.pred)) $ (rowT @27751) x
         , sum us + x == sum vs]

instance OEIS 48098 where
  oeis = [x | x <- [1..], (oeisIx @6530) x ^ 2 <= x]

instance OEIS 48272 where
  oeisIx n = (oeisIx @1227) n - (oeisIx @183063) n

instance OEIS 48411 where
  oeis = 0 : filter ((== 1) . (oeisIx @10052)) (oeis @33075)

instance OEIS 48691 where
  oeisIx = product . map (oeisIx @5408 . fi) . (rowT @124010)

instance OEIS 48890 where
  oeis = filter f (oeis @40) where
     f x = all (`elem` [0,1,6,8,9]) ds && x' /= x && (oeisIx @10051) x' == 1
       where x' = foldl c 0 ds
             c v 6 = 10*v + 9; c v 9 = 10*v + 6; c v d = 10*v + d
             ds = unfoldr d x
             d z = if z == 0 then Nothing else Just $ swap $ divMod z 10

instance OEIS 49076 where
  oeisIx = (+ 1) . (oeisIx @78442)

instance OEIS 49098 where
  oeis = filter ((== 0) . (oeisIx @8966) . (+ 1)) (oeis @40)

instance OEIS 49599 where
  oeisIx = product . map ((+ 1) . (oeisIx @5) . fi) . (rowT @124010) . succ

instance OEIS 49613 where
  oeisIx n = 2 * n - (oeisIx @7917) (2 * n - 2)

instance OEIS 50001 where
  oeisIx n = (fromJust $ elemIndex n (oeis @50000)) + 1

instance OEIS 50360 where
  oeisIx = (oeisIx @688) . (oeisIx @25487)

instance OEIS 50361 where
  oeisIx = product . map (oeisIx @9) . (rowT @124010)

instance OEIS 50382 where
  oeisIx = (oeisIx @8480) . (oeisIx @25487)

instance OEIS 50435 where
  oeisIx = (oeisIx @2808) . (oeisIx @2808)
  oeis = map (oeisIx @2808) (oeis @2808)

instance OEIS 50931 where
  oeis = map pred $ filter (any (== 1) . map (flip mod 6) . (rowT @27748) . succ) [1..]

instance OEIS 51004 where
  oeis =  [x | x <- (oeis @5349),
                       x == head (dropWhile (< x) (oeis @34838))]

instance OEIS 51015 where
  oeis = filter zeisel [3, 5 ..] where
     zeisel x = 0 `notElem` ds && length ds > 2 &&
           all (== 0) (zipWith mod (tail ds) ds) && all (== q) qs
           where q:qs = (zipWith div (tail ds) ds)
                 ds = zipWith (-) (tail ps) ps
                 ps = 1 : (rowT @27746) x

instance OEIS 51144 where
  oeis = filter ((== 0) . (oeisIx @8966)) (oeis @37)

instance OEIS 51250 where
  oeis = filter (all ((== 1) . (oeisIx @10055)) . (rowT @38566)) [1..]

instance OEIS 51278 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @51521

instance OEIS 51279 where
  oeis = map succ $ elemIndices 2 $ tail $ oeis @51521

instance OEIS 51282 where
  oeisIx = (oeisIx @7814) . (oeisIx @25487)

instance OEIS 51283 where
  oeis = map succ $ filter (\x -> (oeisIx @34699 x) ^ 2 < x) [1..]

instance OEIS 51487 where
  oeis = [x | x <- [2..], let t = (oeisIx @10) x, t == (oeisIx @10) (x - t)]

instance OEIS 51488 where
  oeis = map pred [x | x <- [2..], let t = (oeisIx @10) x, t < (oeisIx @10) (x - t)]

instance OEIS 51521 where
  oeisIx n = genericLength [k | k <- [1..4*n^2],
                          let d = (oeisIx @5) k, divMod k d == (n,0)]

instance OEIS 51532 where
  oeis = filter ((== 1) . (oeisIx @212793.pred)) (oeis @56867)

instance OEIS 52011 where
  oeis = c 0 0 $ drop 2 (oeis @45) where
    c x y fs'@ (f:fs) | x < f     = c (x+1) (y + (oeisIx @10051) x) fs'
                      | otherwise = y : c (x+1) 0 fs

instance OEIS 52012 where
  oeis = c 1 0 $ tail (oeis @204) where
    c x y ls'@ (l:ls) | x < l     = c (x+1) (y + (oeisIx @10051) x) ls'
                      | otherwise = y : c (x+1) 0 ls

instance OEIS 52021 where
  oeis = tail $ filter (\x -> (oeisIx @7953) x == (oeisIx @6530) x) [1..]

instance OEIS 53636 where
  oeisIx 0 = 0
  oeisIx n = sum . zipWith (*) (map (oeisIx @10) ods)
                 $ map ((2 ^) . (div n)) ods
    where ods = rowT @182469 n

instance OEIS 54646 where
  oeisIx 1 = 1
  oeisIx n = (oeisIx @70167) $ (oeisIx @302) n

instance OEIS 54841 where
  oeisIx 1 = 0
  oeisIx n = sum $ zipWith (*)
                    (map ((10 ^) . subtract 1 . (oeisIx @49084)) $ (rowT @27748) n)
                    (map fi $ (rowT @124010) n)

instance OEIS 55396 where
  oeisIx = (oeisIx @49084) . (oeisIx @20639)

instance OEIS 55768 where
  oeisIx = (oeisIx @1221) . (oeisIx @5867)

instance OEIS 55769 where
  oeisIx = (oeisIx @6530) . (oeisIx @5867)

instance OEIS 55983 where
  oeis = iterate (oeisIx @102487 . (+ 1)) 10

instance OEIS 56240 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @1414)) . succ

instance OEIS 56606 where
  oeisIx = (oeisIx @7947) . (oeisIx @1142) . succ

instance OEIS 56608 where
  oeisIx = (oeisIx @20639) . (oeisIx @2808)

instance OEIS 56867 where
  oeis = filter (\x -> gcd x (oeisIx @173557 x) == 1) [1..]

instance OEIS 56911 where
  oeis = filter ((== 1) . (oeisIx @8966)) [1,3..]

instance OEIS 57226 where
  oeisIx = (oeisIx @43537) . (oeisIx @61493)

instance OEIS 57533 where
  oeis = filter (\z -> p z [z]) [1..] where
     p x ts = y > 0 && (y `elem` ts || p y (y:ts)) where y = (oeisIx @48050) x

instance OEIS 57661 where
  oeisIx (succ->n) = (oeisIx @51193) n `div` n

instance OEIS 57891 where
  oeis = filter ((== 0) . (oeisIx @178225) . (oeisIx @265)) [1..]

instance OEIS 58197 where
  oeisIx n = (+ 1) $ fromJust $ findIndex (n <=) $ tail (oeis @51950)

instance OEIS 58198 where
  oeisIx = (+ 1) . (oeisIx @58197)

instance OEIS 58199 where
  oeisIx n = fromJust $ findIndex (n <=) $ map negate (oeis @51950)

instance OEIS 58529 where
  oeis = filter (\x -> all (`elem` (takeWhile (<= x) (oeis @1132)))
                                   $ (rowT @27748) x) [1..]

instance OEIS 58972 where
  oeis = 1 : do map numerator $ filter ((f [])) [1..] where
     f ys q = denominator y == 1 || not (y `elem` ys) && f (y : ys) y
              where y = (oeisIx @1065) q' % (oeisIx @5) q'
                    q' = numerator q + denominator q

instance OEIS 58977 where
  oeisIx = numerator . until ((== 1) . denominator) f . f . fi
     where f x = (oeisIx @8472) z % (oeisIx @1221) z
                 where z = numerator x + denominator x

instance OEIS 58988 where
  oeisIx n = numerator $ fst $
    until ((== 1) . denominator . fst) f $ f (fi n, []) where
    f (x, ys) = if y `elem` ys then (0, []) else (y, y:ys) where
     y = numerator x * denominator x % (oeisIx @5) (numerator x + denominator x)

instance OEIS 59009 where
  oeis = 0 : filter (odd . (oeisIx @23416)) [1..]

instance OEIS 60355 where
  oeis = map (oeisIx @1694) $ (map succ $ elemIndices 1 $ tail $ oeis @76446)

instance OEIS 60681 where
  oeisIx n = div n p * (p - 1) where p = (oeisIx @20639) n

instance OEIS 60687 where
  oeis = map succ $ elemIndices 1 $ oeis @46660

instance OEIS 60968 where
  oeisIx 1 = 1
  oeisIx n = (if p == 2 then (if e == 1 then 2 else 2^ (e+1)) else 1) *
     (product $ zipWith (*) (map (\q -> q - 2 + mod q 4) ps'')
                            (zipWith (^) ps'' (map (subtract 1) es'')))
     where (ps'', es'') = if p == 2 then (ps, es) else (ps', es')
           ps'@ (p:ps) = (rowT @27748) n; es'@ (e:es) = (rowT @124010) n

instance OEIS 61258 where
  oeisIx (succ->n) = sum $ zipWith (*) ds $ map (oeisIx @2322) ds
              where ds = (rowT @27750) n

instance OEIS 61338 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @6519) n + (oeisIx @120) n - 1

instance OEIS 61373 where
  oeisIx 0 = 1
  oeisIx (succ->n) = genericIndex (oeis @61373) (n - 1)
  oeis = 1 : f 2 where
     f x | x == spf  = 1 + (oeisIx @61373) (spf - 1) : f (x + 1)
         | otherwise = (oeisIx @61373) spf + (oeisIx @61373) (x `div` spf) : f (x + 1)
         where spf = (oeisIx @20639) x

instance OEIS 61389 where
  oeisIx = product . map ((+ 1) . (oeisIx @10) . fi) . (rowT @124010) . succ

instance OEIS 61394 where
  oeisIx = fromJust . (`elemIndex` (oeis @2110)) . (oeisIx @247451) . succ

instance OEIS 61395 where
  oeisIx = (oeisIx @49084) . (oeisIx @6530)

instance OEIS 61775 where
  oeis = 1 : g 2 where
     g x = y : g (x + 1) where
        y = if t > 0 then (oeisIx @61775) t + 1 else (oeisIx @61775) u + (oeisIx @61775) v - 1
            where t = (oeisIx @49084) x; u = (oeisIx @20639) x; v = x `div` u

instance OEIS 62115 where
  oeis = filter ((== 0) . (oeisIx @39997)) (oeis @84984)

instance OEIS 62373 where
  oeis = map succ $ elemIndices 2 $ tail $ oeis @34380

instance OEIS 62401 where
  oeisIx = (oeisIx @10) . (oeisIx @203)

instance OEIS 62402 where
  oeisIx = (oeisIx @203) . (oeisIx @10)

instance OEIS 63108 where
  oeis = iterate (oeisIx @63114) 1

instance OEIS 63453 where
  oeisIx = product . map ((1 -) . (^ 3)) . (rowT @27748) . succ

instance OEIS 63574 where
  oeisIx n = fst $ until ((== 1) . flip mod 4 . snd)
                          (\ (u, v) -> (u + 1, (oeisIx @7494) v)) (0, n)

instance OEIS 63637 where
  oeis = filter ((== 1) . (oeisIx @64911) . (+ 2)) (oeis @40)

instance OEIS 63638 where
  oeis = map (+ 2) $ filter ((== 1) . (oeisIx @64911)) (oeis @40976)

instance OEIS 63639 where
  oeis = [p | p <- (oeis @40), (oeisIx @1222) (p+1) == 3]

instance OEIS 63776 where
  oeisIx (succ->n) = (oeisIx @53636) n `div` n

instance OEIS 63947 where
  oeis = filter ((== 1) . denominator . hm . (rowT @77609)) [1..]
     where hm xs = genericLength xs / sum (map (recip . fi) xs)

instance OEIS 63962 where
  oeisIx 0 = 0
  oeisIx (succ->n) = genericLength [p | p <- (rowT @27748) n, p ^ 2 <= n]

instance OEIS 64275 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @32447))

instance OEIS 64372 where
  oeisIx 0 = 1
  oeisIx n = sum $ map (oeisIx @64372) $ (rowT @124010 . succ) n

instance OEIS 64427 where
  oeisIx 0 = 1
  oeisIx (succ->n) = (oeisIx @720) (n - 1) + n

instance OEIS 64549 where
  oeisIx (succ->n) = (oeisIx @7947) n * n

instance OEIS 64553 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map ((+ 1) . (oeisIx @49084)) $ (rowT @27746) n

instance OEIS 64702 where
  oeis = filter (\x -> (oeisIx @10888) x == (oeisIx @31347) x) [1..]

instance OEIS 64745 where
  oeisIx n = fromJust (elemIndex n (oeis @64736)) + 1

instance OEIS 64800 where
  oeisIx n = (oeisIx @1222) n + n

instance OEIS 64956 where
  oeisIx n = (fromJust $ elemIndex n (oeis @64417)) + 1

instance OEIS 64987 where
  oeisIx n = (oeisIx @203) n * n

instance OEIS 65037 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @36552))

instance OEIS 65307 where
  oeisIx n = (fromJust $ elemIndex n (oeis @65306)) + 1

instance OEIS 65330 where
  oeisIx = (oeisIx @38502) . (oeisIx @265)

instance OEIS 65338 where
  oeisIx 1 = 1
  oeisIx n = (spf `mod` 4) * (oeisIx @65338) (n `div` spf) where spf = (oeisIx @20639) n

instance OEIS 65642 where
  oeisIx 1 = 1
  oeisIx n = head [x | let rad = (oeisIx @7947) n, x <- [n+1..], (oeisIx @7947) x == rad]

instance OEIS 66197 where
  oeisIx n = (oeisIx @7947) $ (oeisIx @33286) n * (oeisIx @14688) n

instance OEIS 66527 where
  oeis = filter ((== 1) . (oeisIx @10054) . succ) (oeis @7504)

instance OEIS 67953 where
  oeisIx n = p [1..n] $ (oeisIx @40) n where
     p _  0 = 1
     p [] _ = 0
     p (k:ks) m | m < k = 0 | otherwise = p ks (m - k) + p ks m

instance OEIS 68074 where
  oeisIx n | odd n     = - (oeisIx @48691) n
            | otherwise = 2 * (oeisIx @48691) (n `div` 2) - (oeisIx @48691) n

instance OEIS 68101 where
  oeisIx = sum . map (oeisIx @8683) . (rowT @161906) . succ

instance OEIS 68494 where
  oeisIx n = mod n $ (oeisIx @10) n

instance OEIS 68781 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @261869

instance OEIS 68901 where
  oeisIx (succ->n) = head $
     filter ((== 0) . (`mod` fi n) . (+ (oeisIx @40) n)) $ [0..]

instance OEIS 68919 where
  oeis = filter ((== 1) . (oeisIx @8966)) (oeis @56868)

instance OEIS 68936 where
  oeis = [x | x <- [1..], (oeisIx @8472) x <= (oeisIx @1222) x]

instance OEIS 68997 where
  oeis = filter (\x -> mod x (oeisIx @173557 x) == 0) [1..]

instance OEIS 69056 where
  oeis = filter (\x -> x ^ 2 `mod` (oeisIx @46970) x == 0) [1..]

instance OEIS 69059 where
  oeis = filter ((> 1) . (oeisIx @9194)) [1..]

instance OEIS 69283 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ tail $ (rowT @182469) n

instance OEIS 69288 where
  oeisIx n = genericLength $ takeWhile (<= (oeisIx @196) n) $ (rowT @182469) n

instance OEIS 69289 where
  oeisIx n = sum $ takeWhile (<= (oeisIx @196) n) $ (rowT @182469) n

instance OEIS 69352 where
  oeisIx = (oeisIx @1222) . (oeisIx @3586)

instance OEIS 69488 where
  oeis = filter f $ dropWhile (<= 100) (oeis @38618) where
     f x = x < 10 || (oeisIx @10051) (x `mod` 100) == 1 && f (x `div` 10)

instance OEIS 69513 where
  oeisIx 1 = 0
  oeisIx n = (oeisIx @10055) n

instance OEIS 69715 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @52423

instance OEIS 70005 where
  oeis = filter ((== 0) . (oeisIx @10055)) (oeis @78174)

instance OEIS 70072 where
  oeisIx n = genericLength [ () | x <- [1..n], y <- [1..x], (oeisIx @8966) (x*y) == 1]

instance OEIS 70647 where
  oeisIx = (oeisIx @6530) . (oeisIx @6881)

instance OEIS 71139 where
  oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) [2..]

instance OEIS 71140 where
  oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) (oeis @24619)

instance OEIS 71188 where
  oeisIx = (oeisIx @6530) . (oeisIx @5)

instance OEIS 71249 where
  oeis = filter ((> 1) . (oeisIx @55483)) [1..]

instance OEIS 71574 where
  oeisIx 1 = 0
  oeisIx n = 2 * (oeisIx @71574) (if j > 0 then j + 1 else (oeisIx @49084) n) + 1 - signum j
              where j = (oeisIx @66246) n

instance OEIS 71681 where
  oeisIx n = sum $ map (oeisIx @10051) $
     takeWhile (> 0) $ map (2 * (oeisIx @40) n -) $ drop n (oeis @40)

instance OEIS 71889 where
  oeisIx (succ->n) = gcd n $ (oeisIx @71888) n

instance OEIS 71890 where
  oeisIx (succ->n) = (oeisIx @71888) n - n

instance OEIS 71891 where
  oeisIx (succ->n) = (oeisIx @71890) n `div` (oeisIx @71889) n

instance OEIS 71892 where
  oeisIx (succ->n) = lcm n $ (oeisIx @71888) n

instance OEIS 71893 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @71891))

instance OEIS 71931 where
  oeis = filter f (oeis @2312) where
     f x = 2 * gpf <= (oeisIx @6530) (gpf ^ 2 + 1) where gpf = (oeisIx @6530) x

instance OEIS 72085 where
  oeisIx = (oeisIx @72084) . (oeisIx @72084) . succ

instance OEIS 72086 where
  oeisIx (succ->n) = fst $
     until ((== 1) . snd) (\ (i, x) -> (i + 1, (oeisIx @72084) x)) (0, n)

instance OEIS 72087 where
  oeisIx 0 = 1
  oeisIx (succ->n) = product $ map (oeisIx @61712) $ (rowT @27746) n

instance OEIS 72202 where
  oeis = [x | x <- [1..], (oeisIx @83025) x == (oeisIx @65339) x]

instance OEIS 72211 where
  oeis = 1 : zipWith div (tail (oeis @217863)) (oeis @217863)

instance OEIS 72403 where
  oeis = map denominator $ scanl1 (-) $
    map ((1 %) . (oeisIx @244)) $ (oeis @29837)

instance OEIS 72513 where
  oeisIx (succ->n) = product $ map (n -) $ (rowT @27751) n

instance OEIS 72941 where
  oeisIx n = product $ zipWith (^) ps $ map (max 1) es where
              (ps, es) = unzip $ dropWhile ((== 0) . snd) $
                         zip (oeis @40) $ (rowT @67255) n

instance OEIS 73121 where
  oeisIx n = (oeisIx @53644) n * (fi n + 2 * (oeisIx @53645) n)

instance OEIS 73311 where
  oeisIx = sum . map (oeisIx @8966) . (rowT @38566) . succ

instance OEIS 73353 where
  oeisIx n = n + (oeisIx @7947) n

instance OEIS 73481 where
  oeisIx = (oeisIx @20639) . (oeisIx @5117)

instance OEIS 73482 where
  oeisIx = (oeisIx @6530) . (oeisIx @5117)

instance OEIS 73483 where
  oeisIx n = product $ filter ((> 0) . (mod m)) $
     dropWhile (<= (oeisIx @20639) m) $ takeWhile (<= (oeisIx @6530) m) (oeis @40)
     where m = (oeisIx @5117) n

instance OEIS 73490 where
  oeisIx = f . succ where
    f (succ->n) = genericLength $ filter (> 1) $ zipWith (-) (tail ips) ips
      where ips = map (oeisIx @49084) $ (rowT @27748) n

instance OEIS 73491 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @73490

instance OEIS 73492 where
  oeis = filter ((> 0) . (oeisIx @73490)) [1..]

instance OEIS 73493 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @73490

instance OEIS 73494 where
  oeis = map succ $ elemIndices 2 $ tail $ oeis @73490

instance OEIS 73495 where
  oeis = map succ $ elemIndices 3 $ tail $ oeis @73490

instance OEIS 75106 where
  oeisIx (succ.succ->n) = denominator $ n % (oeisIx @523) n

instance OEIS 76396 where
  oeisIx = (oeisIx @20639) . (oeisIx @25478)

instance OEIS 76397 where
  oeisIx = (oeisIx @6530) . (oeisIx @25478)

instance OEIS 76403 where
  oeisIx = (oeisIx @7947) . (oeisIx @25478)

instance OEIS 76566 where
  oeisIx = (oeisIx @6530) . (* 3) . (+ 1)

instance OEIS 77066 where
  oeisIx = (oeisIx @7947) . (oeisIx @8864)

instance OEIS 77609 where
  oeis = tablList @77609
instance Table 77609 where
  rowCol n k = (rowT @77609) n !! (k- 1)
  rowT n = filter
     (\d -> d == 1 || null (rowT @77609 d \\ (rowT @213925) n)) $ (rowT @27750) n
  tabf = map (rowT @77609) [1..]

instance OEIS 78175 where
  oeis = filter (\x -> (oeisIx @1414 x) `mod` (oeisIx @1222 x) == 0) [2..]

instance OEIS 78179 where
  oeisIx (succ.succ->n) = n ^ (oeisIx @78178 n) + n - 1

instance OEIS 78311 where
  oeisIx = (oeisIx @20639) . (oeisIx @78310)

instance OEIS 78312 where
  oeisIx = (oeisIx @6530) . (oeisIx @78310)

instance OEIS 78314 where
  oeisIx = (oeisIx @1222) . (oeisIx @78310)

instance OEIS 78315 where
  oeisIx = (oeisIx @51904) . (oeisIx @78310)

instance OEIS 78316 where
  oeisIx = (oeisIx @51903) . (oeisIx @78310)

instance OEIS 78317 where
  oeisIx = (oeisIx @5) . (oeisIx @78310)

instance OEIS 78318 where
  oeisIx = (oeisIx @203) . (oeisIx @78310)

instance OEIS 78319 where
  oeisIx = (oeisIx @8472) . (oeisIx @78310)

instance OEIS 78320 where
  oeisIx = (oeisIx @1414) . (oeisIx @78310)

instance OEIS 78321 where
  oeisIx = (oeisIx @10) . (oeisIx @78310)

instance OEIS 78322 where
  oeisIx = (oeisIx @7947) . (oeisIx @78310)

instance OEIS 78442 where
  oeisIx (succ->n) = fst $ until ((== 0) . snd)
                (\ (i, p) -> (i + 1, (oeisIx @49084) p)) (-2, (oeisIx @40) n)

instance OEIS 78613 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @5094

instance OEIS 78637 where
  oeisIx n = (oeisIx @7947) $ product [n..n+2]

instance OEIS 78715 where
  oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @61493)) [1..3999]



instance OEIS 78972 where
  oeis = filter brilliant (oeis @1358) where
     brilliant x = (on (==) (oeisIx @55642)) p (x `div` p) where p = (oeisIx @20639) x

instance OEIS 79062 where
  oeis = 2 : f 2 (tail (oeis @40)) where
     f x ps = q : f q qs where
       (q:qs) = dropWhile (\p -> (oeisIx @75802) (p - x) == 0 || p - x == 1) ps

instance OEIS 79083 where
  oeisIx = (oeisIx @78701) . (oeisIx @79079)

instance OEIS 79084 where
  oeisIx = (oeisIx @6530) . (oeisIx @79079)

instance OEIS 79086 where
  oeisIx = (oeisIx @1222) . (oeisIx @79079)

instance OEIS 79087 where
  oeisIx = (oeisIx @51903) . (oeisIx @79079)

instance OEIS 79088 where
  oeisIx = (oeisIx @5) . (oeisIx @79079)

instance OEIS 79089 where
  oeisIx = (oeisIx @203) . (oeisIx @79079)

instance OEIS 79090 where
  oeisIx = (oeisIx @8472) . (oeisIx @79079)

instance OEIS 79091 where
  oeisIx = (oeisIx @1414) . (oeisIx @79079)

instance OEIS 79092 where
  oeisIx = (oeisIx @10) . (oeisIx @79079)

instance OEIS 79093 where
  oeisIx = (oeisIx @7947) . (oeisIx @79079)

instance OEIS 79095 where
  oeis = filter ((== 1) . (oeisIx @8966)) (oeis @79079)

instance OEIS 79124 where
  oeisIx n = p [1 .. (oeisIx @10) n] n where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 79228 where
  oeisIx n = head [k | k <- [n+1..], (oeisIx @7947) k > (oeisIx @7947) n]

instance OEIS 79890 where
  oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1222) x == 1 + (oeisIx @1222) n]

instance OEIS 80672 where
  oeis = filter ((<= 7) . (oeisIx @20639)) [2..]

instance OEIS 80736 where
  oeisIx n = if n `mod` 4 == 2 then 0 else (oeisIx @10) n

instance OEIS 80788 where
  oeis = filter f (oeis @40) where
     f x = all (`elem` [0,1,6,8,9]) ds && x' /= x && (oeisIx @10051) x' == 1
       where x' = foldl c 0 ds
             c v 6 = 10*v + 9; c v 9 = 10*v + 6; c v d = 10*v + d
             ds = unfoldr d x
             d z = if z == 0 then Nothing else Just $ swap $ divMod z 10

instance OEIS 80941 where
  oeisIx (succ->n) = if null ds then 0 else head ds  where
              ds = filter ((flip isPrefixOf `on` (rowT @30308)) n) $
                          reverse $ (rowT @27751) n

instance OEIS 80943 where
  oeis = map succ $ elemIndices 2 $ tail $ oeis @80942

instance OEIS 80945 where
  oeis = filter ((> 2) . (oeisIx @80942)) [1..]

instance OEIS 80946 where
  oeis = map succ $ elemIndices 3 $ tail $ oeis @80942

instance OEIS 80947 where
  oeis = filter ((> 3) . (oeisIx @80942)) [1..]

instance OEIS 81146 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @81145))

instance OEIS 81238 where
  oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
                           (oeisIx @8683) u * (oeisIx @8683) v == -1]

instance OEIS 81239 where
  oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
                           (oeisIx @8683) u * (oeisIx @8683) v == 0]

instance OEIS 81240 where
  oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
                           (oeisIx @8683) u * (oeisIx @8683) v == 1]

instance OEIS 81382 where
  oeisIx 1 = 1
  oeisIx n = head [x | let sopf = (oeisIx @8472) n, x <- [n+1..], (oeisIx @8472) x == sopf]

instance OEIS 81408 where
  oeis = 1 : 1 : 1 : 1 : zipWith (*) [5..] (oeis @81407)

instance OEIS 81619 where
  oeis = filter ((== 1) . (oeisIx @10054) . (oeisIx @5)) [1..]

instance OEIS 81770 where
  oeis = filter ((== 1) . (oeisIx @8966) . (`div` 4)) (oeis @17113)

instance OEIS 81828 where
  oeis = map (+ 1) $ elemIndices 0 (oeis @81827)

instance OEIS 81829 where
  oeis = map (+ 1) $ findIndices (< 0) (oeis @81827)

instance OEIS 81830 where
  oeis = map (+ 1) $ findIndices (> 0) (oeis @81827)

instance OEIS 82763 where
  oeis = filter (containsL . (oeisIx @61493)) [1..3999] where
     containsL x = d == 4 || x > 0 && containsL x' where
                   (x',d) = divMod x 10

instance OEIS 83752 where
  oeisIx n = head [k | k <- [n+1..], (oeisIx @10052) (12* (k+n)^2 + k*n) == 1]

instance OEIS 84115 where
  oeisIx (succ->n) = (oeisIx @84113) n - (oeisIx @84114) n

instance OEIS 84116 where
  oeis = map succ $ elemIndices 1 $ tail $ oeis @84115

instance OEIS 84126 where
  oeisIx = (oeisIx @20639) . (oeisIx @1358)

instance OEIS 84127 where
  oeisIx = (oeisIx @6530) . (oeisIx @1358)

instance OEIS 84933 where
  oeisIx n = (fromJust $ elemIndex n (oeis @84937)) + 1

instance OEIS 85239 where
  oeisIx 1 = 1
  oeisIx n = (oeisIx @6899) n `mod` 2 + 2

instance OEIS 85392 where
  oeisIx = (oeisIx @6530) . (oeisIx @32742)

instance OEIS 85423 where
  oeisIx = (oeisIx @523) . (oeisIx @8585)

instance OEIS 85604 where
  oeis = tablList @85604
instance Table 85604 where
  rowCol = rowCol_off @85604 @2 @1
  rowT 1 = [0]
  rowT n = (rowT @115627) n ++ (take $ (oeisIx @62298) $ fi n) [0,0..]
  tabl = map (rowT @85604) [1..]

instance OEIS 85730 where
  oeisIx 0 = 1
  oeisIx (succ->n) = (p - 1) * p ^ (e - 1)
     where p =  (oeisIx @25473) n; e =  (oeisIx @25474) n

instance OEIS 85809 where

instance OEIS 86005 where
  oeis = tail $ filter
     (\x -> (oeisIx @64911) (x - 1) == 1 && (oeisIx @64911) (x + 1) == 1) (oeis @100484)

instance OEIS 86006 where
  oeisIx = flip div 2 . (oeisIx @86005) . succ

instance OEIS 86971 where
  oeisIx = sum . map (oeisIx @64911) . (rowT @27750) . succ

instance OEIS 87713 where
  oeisIx = (oeisIx @6530) . (oeisIx @84920)

instance OEIS 87875 where
  oeis = 1 : 1 : zipWith (+)
     (map (oeisIx @87875) $ zipWith (-) [3..] (oeis @87875))
     (map (oeisIx @720) $ zipWith (-) [3..] $ tail (oeis @720))

instance OEIS 88380 where
  oeisIx (succ->n) = (oeis @88382) !! (n - 1)
  oeis = [x | x <- [1..], x <= (oeisIx @20639) x ^ 3]

instance OEIS 88381 where
  oeis = filter f [1..] where
                        f x = p ^ 2 < div x p  where p = (oeisIx @20639) x

instance OEIS 88631 where
  oeisIx n = (oeisIx @60265) n - n

instance OEIS 89341 where
  oeis = filter (\x -> (oeisIx @6530) x < 2 * (oeisIx @20639) x) (oeis @24619)

instance OEIS 90050 where
  oeis = [x | x <- [1..], (oeisIx @87117) x == (oeisIx @38374) x]

instance OEIS 90431 where
  oeisIx n = (oeisIx @7953) n - (oeisIx @7605) n

instance OEIS 91050 where
  oeisIx = sum . map (oeisIx @75802) . (rowT @27750) . succ

instance OEIS 91191 where
  oeis = filter f [1..] where
     f x = sum pdivs > x && all (<= 0) (map (\d -> (oeisIx @203) d - 2 * d) pdivs)
           where pdivs = (rowT @27751) x

instance OEIS 91376 where
  oeis = [x | x <- (oeis @2808), (oeisIx @1222) x == (oeisIx @20639) x]

instance OEIS 92206 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @214295

instance OEIS 92693 where
  oeisIx 0 = 0
  oeisIx (succ->n) = (+ 1) $ sum $ takeWhile (/= 1) $ iterate (oeisIx @10) $ (oeisIx @10) n

instance OEIS 93074 where
  oeisIx 0 = 2
  oeisIx (succ->n) = maximum $ map (oeisIx @6530) [n - 1..n+1]

instance OEIS 93641 where
  oeis = filter ((<= 2) . (oeisIx @1227)) [1..]

instance OEIS 93703 where
  oeis = filter
     ((`elem` map (oeisIx @61493) [1..3999]) . (oeisIx @4086) . (oeisIx @61493)) [1..]

instance OEIS 93796 where
  oeis = concatMap (reverse . unfoldr r) $ map (oeisIx @61493) [1..3999]
     where r 0 = Nothing
           r x = Just ([0,1,5,10,50,100,500,1000] !! fromInteger d, x')
                 where (x', d) = divMod x 10

instance OEIS 94379 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @66955))

instance OEIS 95960 where
  oeisIx (succ->n) = genericLength [x | x <- (rowT @27750) n, x < (oeisIx @7947) n]

instance OEIS 96268 where
  oeisIx = (subtract 1) . (oeisIx @56832) . (+ 1)

instance OEIS 96363 where
  oeisIx = (oeisIx @1175) . (10 ^)

instance OEIS 96460 where
  oeis = 1 : iterate (\x -> x + (oeisIx @8472) x) 2

instance OEIS 96461 where
  oeis = 1 : iterate (oeisIx @75254) 2

instance OEIS 96780 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75383))

instance OEIS 96916 where
  oeisIx = (oeisIx @20639) . (oeisIx @6881)

instance OEIS 97974 where
  oeisIx (succ->n) = sum [p | p <- (rowT @27748) n, p ^ 2 <= n]

instance OEIS 98312 where
  oeisIx = (oeisIx @98311) . (oeisIx @98311)

instance OEIS 98313 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98311))

instance OEIS 98314 where
  oeisIx = (oeisIx @98313) . (oeisIx @98313)

instance OEIS 98549 where
  oeisIx = (oeisIx @98548) . (oeisIx @98548)

instance OEIS 98553 where
  oeisIx = (oeisIx @98551) . (oeisIx @98551)

instance OEIS 98962 where
  oeis = 1 : f [2..] (tail (oeis @175944)) where
     f xs'@ (x:xs) ps'@ (p:ps)
       | (oeisIx @10051) x == 1    = x : f xs (delete x ps')
       | u == q && v == q' = x : f xs' zs
       | otherwise         = f xs ps'
       where q = (oeisIx @20639) x; q' = div x q
             (us, u:us') = span (< q) ps'
             (vs, v:vs') = span (< q') us'
             zs@ (z:_) = us ++ vs ++ vs'
             xs' = if z == p then xs else filter ((> 0) . (`mod` p)) xs

instance OEIS 99009 where
  oeis = [x | x <- [0..], (oeisIx @151949) x == x]

instance OEIS 99302 where
  oeisIx n = genericLength $ filter (== n) $ map (oeisIx @3415) [1 .. (oeisIx @2620) n]

instance OEIS 99543 where
  oeisIx = (oeisIx @1414) . (oeisIx @99542) . succ

instance OEIS 99619 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98962)) . (oeisIx @40)

instance OEIS 100678 where
  oeisIx = genericLength . (rowT @247765)

instance OEIS 100695 where
  oeisIx = last . (rowT @247765)

instance OEIS 100962 where
  oeis = filter ((== 0) . (oeisIx @64911)) (oeis @14092)

instance OEIS 101438 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @101369))

instance OEIS 101594 where
  oeis = filter ((== 2) . (oeisIx @43537)) (oeis @52382)

instance OEIS 102466 where
  oeis = tail [x | x <- [1..], (oeisIx @5) x == (oeisIx @1221) x + (oeisIx @1222) x]

instance OEIS 102467 where
  oeis = [x | x <- [1..], (oeisIx @5) x /= (oeisIx @1221) x + (oeisIx @1222) x]

instance OEIS 102478 where
  oeisIx = flip div 2 . (oeisIx @68700)

instance OEIS 103889 where
  oeisIx n = n - 1 + 2 * mod n 2
  oeis = concat $ transpose [tail (oeis @5843), (oeis @5408)]

instance OEIS 104324 where
  oeisIx = genericLength . map length . group . (rowT @213676)

instance OEIS 105271 where
  oeis = [x | x <- [0..], (oeisIx @105025) x == x]

instance OEIS 105571 where
  oeis = [x | x <- [3..], (oeisIx @64911) (x - 2) == 1, (oeisIx @64911) (x + 2) == 1]

instance OEIS 106315 where
  oeisIx n = n * (oeisIx @5) n `mod` (oeisIx @203) n

instance OEIS 106372 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @106370))

instance OEIS 106799 where
  oeisIx = (oeisIx @1222) . (oeisIx @65330)

instance OEIS 109129 where
  oeis = 0 : 1 : g 3 where
     g x = y : g (x + 1) where
       y = if t > 0 then (oeisIx @109129) t else (oeisIx @109129) r + (oeisIx @109129) s
           where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 109373 where
  oeis = filter ((== 1) . (oeisIx @64911)) (oeis @88707)

instance OEIS 110085 where
  oeis = filter (\x -> (oeisIx @51612) x < (oeisIx @110088) x) [1..]

instance OEIS 110086 where
  oeis = filter (\x -> (oeisIx @51612) x <= (oeisIx @110088) x) [1..]

instance OEIS 110087 where
  oeis = filter (\x -> (oeisIx @51612) x > (oeisIx @110088) x) [1..]

instance OEIS 112798 where
  oeis = tablList @112798
instance Table 112798 where
  rowCol = rowCol_off @112798 @2 @1
  rowT   = rowT_off @112798 @2
  tabf = map (map (oeisIx @49084)) $ tail (tabf @27746)

instance OEIS 112990 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @89088))

instance OEIS 114180 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @261890

instance OEIS 114228 where
  oeisIx n = head [m | m <- [1..],
                        (oeisIx @10051 . pred) (oeisIx @40 n + 2 * (oeisIx @40) m) == 1]

instance OEIS 114229 where
  oeisIx = (+ 2) . fromJust . (`elemIndex` (map (oeisIx @114228) [2..]))

instance OEIS 114262 where
  oeisIx n = head [q | let (p:ps) = drop (n - 1) (oeis @40),
                        q <- ps, (oeisIx @10051 . pred) (p + 2 * q) == 1]

instance OEIS 115627 where
  oeis = tablList @115627
instance Table 115627 where
  rowCol = rowCol_off @115627 @2 @1
  rowT = map (oeisIx @100995) . (rowT @141809) . (oeisIx @142)
  tabf = map (rowT @115627) [2..]

instance OEIS 116619 where
  oeisIx = (+ 1) . (oeisIx @71681)

instance OEIS 116933 where
  oeisIx n = head [k | k <- [1..], (oeisIx @10051 . pred) (n + k * (oeisIx @79578) n) == 1]

instance OEIS 116934 where
  oeisIx n = head [q | k <- [1..], let q = n + k * (oeisIx @79578) n,
                        (oeisIx @10051 . pred) q == 1]

instance OEIS 117214 where
  oeisIx n = product $
     filter ((> 0) . (mod m)) $ takeWhile (< (oeisIx @6530) m) (oeis @40)
     where m = (oeisIx @5117) n

instance OEIS 117922 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @55265))

instance OEIS 119354 where
  oeisIx = fromJust . (`elemIndex` (oeis @119352))

instance OEIS 119797 where
  oeis = f [0..] (oeis @43537) where
     f (u:us) (v:vs@ (v':_)) | v /= v'   = f us vs
                            | otherwise = u : f us vs

instance OEIS 119798 where
  oeis = f [0..] (oeis @43537) (oeis @43537) where
     f (z:zs) (u:us) (v:_:vs) | u /= v   = f zs us vs
                              | otherwise = z : f zs us vs

instance OEIS 119799 where
  oeis = i (oeis @119797) (oeis @119798) where
     i xs'@ (x:xs) ys'@ (y:ys) | x < y     = i xs ys'
                             | x > y     = i xs' ys
                             | otherwise = x : i xs ys

instance OEIS 120005 where
  oeisIx = fromJust . (`elemIndex` (oeis @120004))

instance OEIS 120007 where
  oeisIx 1 = 0
  oeisIx n | until ((> 0) . (`mod` spf)) (`div` spf) n == 1 = spf
            | otherwise = 0
            where spf = (oeisIx @20639) n

instance OEIS 120511 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (tail (oeis @6949)))

instance OEIS 120944 where
  oeis = filter ((== 1) . (oeisIx @8966)) (oeis @2808)

instance OEIS 121369 where
  oeis = 1 : 1 : zipWith ((+) `on` (oeisIx @7947))
                         (oeis @121369) (tail (oeis @121369))

instance OEIS 122426 where
  oeis = [x | x <- [1..], (oeisIx @122425) x < x]

instance OEIS 122427 where
  oeisIx (succ->n) = (oeis @122426) !! (n - 1)
  oeis = [x | x <- [1..], (oeisIx @122425) x == x]

instance OEIS 122428 where
  oeis = [x | x <- [1..], (oeisIx @122425) x == (oeisIx @6530) x]

instance OEIS 122535 where
  oeisIx = (oeisIx @40) . (oeisIx @64113)

instance OEIS 122631 where
  oeis =
     1 : 2 : map (oeisIx @6530) (zipWith (+) (map ((2 *) . (oeisIx @40)) (oeis @122631))
                                      (map (oeisIx @40) (tail (oeis @122631))))

instance OEIS 123087 where
  oeis = scanl (+) 0 (oeis @96268)

instance OEIS 125290 where
  oeis = filter ((> 1) . (oeisIx @43537)) (oeis @52382)

instance OEIS 125639 where
  oeis = filter f [1..] where
     f x = sx > x && (oeisIx @1065) sx > sx where sx = (oeisIx @1065) x

instance OEIS 126949 where
  oeis = filter h [1..] where
     h m = not $ null [ (x, e) | x <- [2 .. m - 2], gcd x m == 1,
                                e <- [2 .. (oeisIx @10) m `div` 2],
                                x ^ e `mod` m == m - 1]

instance OEIS 127626 where
  oeis = tablList @127626
instance Table 127626 where
  rowCol = rowCol_off @127626 @1 @1
  rowT   = rowT_off   @127626 @1
  tabl = map (map (\x -> if x == 0 then 0 else (oeisIx @18804) x)) (tabl @127093)

instance OEIS 129284 where
  oeisIx 0 = 2
  oeisIx n = (oeisIx @129151) n `div` 27

instance OEIS 132090 where
  oeisIx = (oeisIx @720) . (oeisIx @720)

instance OEIS 132350 where
  oeisIx 1 = 1
  oeisIx n = 1 - (oeisIx @75802) n

instance OEIS 132431 where
  oeisIx (succ->n) = (oeisIx @60226) n - (oeisIx @62119) n + (oeisIx @2378) (n - 1)

instance OEIS 133810 where
  oeis = 1 : filter f [2..] where
     f x = (and $ zipWith (<=) eps $ tail eps) &&
           (all (== 1) $ zipWith (-) (tail ips) ips)
       where ips = map (oeisIx @49084) $ (rowT @27748) x
             eps = (rowT @124010) x

instance OEIS 133813 where
  oeis = 1 : filter f [2..] where
     f x = isPrefixOf ps (dropWhile (< (oeisIx @20639) x) (oeis @40)) &&
             all (< 0) (zipWith (-) (tail es) es)
           where ps = (rowT @27748) x; es = (rowT @124010) x

instance OEIS 135093 where
  oeisIx 0 = 4
  oeisIx n = (+ 1) $ fromJust $ (`elemIndex` (oeis @46665)) $ (oeisIx @30173) n

instance OEIS 135282 where
  oeisIx = (oeisIx @7814) . head . filter ((== 1) . (oeisIx @209229)) . (rowT @70165)

instance OEIS 135499 where
  oeis = map succ $ elemIndices 0 $ tail $ oeis @225693

instance OEIS 136414 where
  oeis = zipWith (+) (tail (oeis @7376)) $ map (10 *) (oeis @7376)

instance OEIS 136480 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx @7814) $ n + mod n 2

instance OEIS 136495 where
  oeisIx n = (fromJust $ n `elemIndex` tail (oeis @5374)) + 1

instance OEIS 137613 where
  oeis =  filter (> 1) (oeis @132199)

instance OEIS 139555 where
  oeisIx = sum . map (oeisIx @10055) . (rowT @38566) . succ

instance OEIS 140470 where
  oeis = filter
     (\x -> all (== 0) $ map ((mod x) . (+ 1)) $ (rowT @27748) x) [1..]

instance OEIS 140480 where
  oeis = filter
      ((== 1) . (oeisIx @10052) . (\x -> (oeisIx @1157) x `div` (oeisIx @5) x)) (oeis @20486)

instance OEIS 141164 where
  oeis = map succ $ elemIndices 1 $ map (oeisIx @188172) [1..]

instance OEIS 141258 where
  oeisIx = sum . map (oeisIx @2322) . (rowT @27750) . succ

instance OEIS 141707 where
  oeisIx n = head [k | k <- [1, 3 ..], (oeisIx @178225) (k * (2 * n - 1)) == 1]

instance OEIS 141708 where
  oeisIx n = (oeisIx @141707) n * (2 * n - 1)

instance OEIS 141709 where
  oeisIx (succ->n) = until ((== 1) . (oeisIx @178225) . (oeisIx @265)) (+ n) n

instance OEIS 141766 where
  oeis = filter f [1..] where
     f x = all (== 0) $ map (mod x) $ (map pred ps) ++ (map succ ps)
           where ps = (rowT @27748.succ) x

instance OEIS 141767 where
  oeis = filter f [1..] where
     f x = all (== 0) $
           map (mod x) $ zipWith (*) (map pred ps) (map succ ps)
           where ps = (rowT @27748.succ) x

instance OEIS 143202 where
  oeis = filter (\x -> (oeisIx @6530) x - (oeisIx @20639) x == 2) [1,3..]

instance OEIS 143215 where
  oeisIx n = (oeisIx @40) n * (oeisIx @7504) n

instance OEIS 143691 where
  oeis = f 1 [1..] where
     f m xs = g xs where
       g (z:zs) = if m + m' /= 1 then g zs else z : f m' (delete z xs)
                  where m' = (oeisIx @1222) z `mod` 2

instance OEIS 143692 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @143691))

instance OEIS 143792 where
  oeisIx (succ->n) = genericLength $ (rowT @225243) n `intersect` (rowT @27748) (fi n)

instance OEIS 144100 where
  oeis = filter (\x -> (oeisIx @144907) x < x) [1..]

instance OEIS 145784 where
  oeis = filter ((== 0) . (oeisIx @10872) . (oeisIx @1222)) [1..]

instance OEIS 146288 where
  oeisIx = (oeisIx @5) . (oeisIx @25487)

instance OEIS 151764 where
  oeisIx = (oeisIx @71786) . (oeisIx @71786)

instance OEIS 151765 where
  oeisIx = (oeisIx @71786) . (oeisIx @4086)

instance OEIS 156596 where
  oeisIx (succ->n) = (oeis @143667) !! (n - 1)
  oeis = f (oeis @3849) where
     f (0:0:ws) = 0 : f ws; f (0:1:ws) = 1 : f ws; f (1:0:ws) = 2 : f ws

instance OEIS 157728 where
  oeisIx = subtract 4 . (oeisIx @45) . (+5)

instance OEIS 157931 where
  oeis = filter ((== 1) . (oeisIx @64911)) (oeis @14091)

instance OEIS 160180 where
  oeisIx = (oeisIx @32742) . (oeisIx @2808)

instance OEIS 160676 where
  oeis = filter (\x -> (oeisIx @6968) x == (oeisIx @6968) (2 * x)) [1..]

instance OEIS 161764 where
  oeisIx n = n - (oeisIx @199238) n

instance OEIS 162643 where
  oeis = filter ((== 0) . (oeisIx @209229) . (oeisIx @5)) [1..]

instance OEIS 163753 where
  oeis = filter ((> 0) . (oeisIx @39997)) [0..]

instance OEIS 164283 where
  oeisIx (succ->n) = f [1..] 1 nn 0 where
     f (k:ks) l nl xx
       | yy > nl  = 0
       | yy < nl  = f ks (l + 1) (nl + nn) yy + f ks l nl xx
       | otherwise = if w == n then 1 else 0
       where w = if r == 0 then (oeisIx @196) m else 0
             (m, r) = divMod yy l
             yy = xx + k * k
     nn = n ^ 2

instance OEIS 164652 where
  oeis = tablList @164652
instance Table 164652 where
  tabl = [0] : tail (zipWith (zipWith (*)) (tabl @128174) $
     zipWith (map . flip div) (tail (oeis @217)) (map init $ tail (tabl @130534)))

instance OEIS 165712 where
  oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1222) x == (oeisIx @1222) n]

instance OEIS 165713 where
  oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1221) x == (oeisIx @1221) n]

instance OEIS 169611 where
  oeisIx = (oeisIx @1222) . (oeisIx @65331)


----- aaa

instance OEIS 223456 where
  oeis = filter ((== 1 ) . (oeisIx @10051 . pred) . (oeisIx @32741) . (oeisIx @32741)) (oeis @2808)

instance OEIS 182140 where
  oeis = [x | x <- [1..], (oeisIx @60968) x == (oeisIx @201629) x]

instance OEIS 81407 where
  oeisIx n = (oeis @81408) !! n
  oeis = 1 : 1 : 1 : 1 : zipWith (*) [5..] (oeis @81407)

instance OEIS 258614 where
  oeis = filter ((> 1) . (oeisIx @74695) . pred) [1..]

instance OEIS 33548 where
  oeis = filter ((== 0) . (oeisIx @90431) . (oeisIx @49084)) (oeis @40)

-}
