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



-- instance OEIS 16 where
--   oeisIx 0 = 1
--   oeisIx n =  (`div` (2 * n)) $ sum $
--    zipWith (*) (map (oeisIx @10) oddDivs) (map ((2 ^) . (div n)) $ oddDivs)
--     where oddDivs = rowT @182469 n

-- instance OEIS 82 where
--   oeisIx n = product $ zipWith (\p e -> p ^ (2*e - 1) * (p + 1))
--                               (rowT @27748 n) (rowT @124010 n)

-- instance OEIS 86 where
--   oeisIx n = if n `mod` 9 == 0 then 0
--   else product $ map ((* 2) . (oeisIx @79978) . (+ 2)) $ rowT @27748 $ oeisIx @38502 n

-- instance OEIS 89 where
--   oeisIx (succ->n) = product $ zipWith f (rowT @27748 n) (rowT @124010 n) where
--    f 2 e = if e == 1 then 1 else 0
--    f p _ = if p `mod` 4 == 1 then 2 else 0

-- instance OEIS 95 where
--   oeisIx n = product $ zipWith f ((rowT @27748) n) ((rowT @124010) n) where
--    f 2 e = if e == 1 then 2 else 0
--    f p _ = if p `mod` 4 == 1 then 2 else 0

-- instance OEIS 118 where
--   oeisIx 0 = 1
--   oeisIx n = 8 * (oeisIx @46897) n

-- instance OEIS 139 where
--   oeis   0 = 2
--   oeisIx n = ((3 * n) `a007318` (2 * n + 1)) `div` (oeisIx @217) n

-- instance OEIS 224 where
--   oeisIx (succ->n) = product $ zipWith f ((rowT @27748) n) ((rowT @124010) n) where
--    f 2 e = 2 ^ e `div` 6 + 2
--    f p e = p ^ (e + 1) `div` (2 * p + 2) + 1

-- instance OEIS 419 where
--   oeis = filter ((== 3) . oeisIx @2828) [1..]

-- instance OEIS 658 where
--   oeisIx (succ->n) = sum $ map c3 [0..n]
--     where
--       c3 k = (rowCol @7318    n     k)^2
--            * (rowCol @7318 (2*k)    k)^2
--            * (rowCol @7318 (2*k) (n-k))

-- instance OEIS 667 where
--   oeisIx n = if x == 1 then last xs else x
--               where xs@ (x:_) = rowT @227862 n

-- instance OEIS 712 where
--   oeis = p $ oeis @8619 where
--     p _          0 = 1
--     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 790 where
--   oeisIx n = head [c | c <- oeis @2808, powMod n c c == mod n c]

-- instance OEIS 796 where
--   oeisIx len = map (fi . digitToInt) $ show . fi $ machin' `div` (10 ^ 10) where
--      machin' = 4 * (4 * arccot 5 unity - arccot 239 unity)
--      unity = 10 ^ (len + 10)
--      arccot x unity = arccot' x unity 0 (unity `div` x) 1 1 where
--        arccot' x unity summa xpow n sign
--         | term == 0 = summa
--         | otherwise = arccot'
--           x unity (summa + sign * term) (xpow `div` x ^ 2) (n + 2) (- sign)
--         where term = xpow `div` n

-- instance OEIS 891 where
--   oeisIx n = (oeisIx @1263) (2 * n - 1) n

-- instance OEIS 894 where
--   oeisIx n = (oeisIx @132813) (2 * n) n

-- instance OEIS 914 where
--   oeis = scanl1 (+) $ oeis @6002

-- instance OEIS 967 where
--   oeisIx n = round $ sum $
--               zipWith ((/) `on` fi) ((rowT @258993) n) [1, 3 ..]

-- instance OEIS 970 where
--   oeisIx n = (oeisIx @258708) n (n - 5)

-- instance OEIS 971 where
--   oeisIx n = (oeisIx @258708) n (n - 6)

-- instance OEIS 972 where
--   oeisIx n = (oeisIx @258708) n (n - 7)

-- instance OEIS 973 where
--   oeisIx n = (oeisIx @258708) n (n - 8)

-- instance OEIS 989 where
--   oeisIx = (oeisIx @7949) . (oeisIx @984)

-- instance OEIS 994 where
--   oeis = 1 : 0 : us where
--      us = 1 : 1 : f 2 where
--        f x = (1 + sum (zipWith (*) (map ((oeisIx @7318) x) [2 .. x]) us)) : f (x + 1)

-- instance OEIS 995 where
--   oeis = 0 : 1 : vs where
--      vs = 0 : 1 : g 2 where
--        g x = (x + sum (zipWith (*) (map ((oeisIx @7318) x) [2 .. x]) vs)) : g (x + 1)

-- instance OEIS 1003 where
--   oeisIx = last . (rowT @144944)

-- instance OEIS 1013 where
  -- import Data.Set (empty, fromList, deleteFindMin, union)
--   import qualified Data.Set as Set (null)
--   oeisIx n = (oeis @1013) !! (n - 1)
--  oeis = 1 : h 0 empty [1] (drop 2 (oeis @142)) where
--      h z s mcs xs'@ (x:xs)
--       | Set.null s || x < m = h z (union s (fromList $ map (* x) mcs)) mcs xs
--       | m == z = h m s' mcs xs'
--       | otherwise = m : h m (union s' (fromList (map (* m) $ init (m:mcs)))) (m:mcs) xs'
--       where (m, s') = deleteFindMin s

-- instance OEIS 1031 where
--   oeisIx n = sum (map (oeisIx @10051) gs) + fi (fromEnum (1 `elem` gs))
--      where gs = map (2 * n -) $ takeWhile (<= n) (oeis @8578)

-- instance OEIS 1037 where
--   oeisIx 0 = 1
--   oeisIx n = (sum $ map (\d -> ((oeisIx @79) d) * (oeisIx @8683) (n `div` d)) $
--                          (rowT @27750) n) `div` n

-- instance OEIS 1055 where
--   oeisIx = (map last (tabl @66032) !!) . (subtract 1)

-- instance OEIS 1066 where
--   import Data.Set (deleteFindMin, fromList, insert)
--   oeisIx n = (oeis @1066) !! (n - 1)
--  oeis = f (fromList [h, 2 * h]) $ tail (oeis @3038) where
--      h = head (oeis @3038)
--      f s (x:xs) = m : f (x `insert` (( 2 * x) `insert` s')) xs where
--        (m, s') = deleteFindMin s

-- instance OEIS 1097 where
--   oeis = filter ((== 1) . (oeisIx @164292)) [1..]

-- instance OEIS 1101 where
--  oeis = map succ $ findIndices p [1..] where
--      p n = m == 0 && (oeisIx @10051) n' == 1 where
--         (n', m) = divMod n ((oeisIx @7953) n)

-- instance OEIS 1102 where
--   oeis = filter (\x -> (oeisIx @10052) (x `div` (oeisIx @7953 x)) == 1) $ oeis @5349

-- instance OEIS 1103 where
--  oeis = filter f (oeis @52382) where
--      f x = m == 0 && (x' == 1 || (oeisIx @10051) x' == 1) where
--          (x',m) = divMod x $ (oeisIx @7954) x

-- instance OEIS 1113 where
--   oeis = eStream (1, 0, 1)
--      [ (n, a * d, d) | (n, d, a) <- map (\k -> (1, k, 1)) [1..]] where
--      eStream z xs'@ (x:xs)
--        | lb /= approx z 2 = eStream (mult z x) xs
--        | otherwise = lb : eStream (mult (10, -10 * lb, 1) z) xs'
--        where lb = approx z 1
--              approx (a, b, c) n = div (a * n + b) c
--              mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f)

-- instance OEIS 1127 where
--   oeis = iterate (oeisIx @56964) 1

-- instance OEIS 1129 where
--  oeis = 0 : 1 : zipWith (+) iccanobifs (tail iccanobifs)
--   where (succ->iccanobifs) = map (oeisIx @4086) (oeis @1129)

-- instance OEIS 1132 where
--  oeis = [x | x <- (oeis @47522), (oeisIx @10051) x == 1]

-- instance OEIS 1158 where
--   oeisIx n = product $ zipWith (\p e -> (p^ (3*e + 3) - 1) `div` (p^3 - 1))
--                         ((rowT @27748) n) ((rowT @124010) n)

-- instance OEIS 1178 where
--   oeisIx = f 0 where
--      f j x = if x == y then j else f (j + 1) y  where y = oeisIx @1175 x

-- instance OEIS 1181 where
--   oeisIx 0 = 0
--   oeisIx n =
--      (sum $ map (\k -> product $ map ((oeisIx @7318) (n+1)) [k - 1..k+1]) [1..n])
--       `div` ((oeisIx @6002) n)

-- instance OEIS 1220 where
--   oeis = map (oeis @40 . (+ 1)) $ elemIndices 1 (oeis @196202)

-- instance OEIS 1227 where
--   (oeisIx @1227) = sum . (rowT @247795)

-- instance OEIS 1317 where
--   (oeisIx @1317) = foldr (\u v-> 2*v + u) 0 . map toInteger . (rowT @47999)

-- instance OEIS 1479 where
--   oeisIx n = (oeisIx @196) $ head $
--      filter ((== 1) . (oeisIx @10052)) $ map ((oeisIx @7645) n -) $ tail (oeis @33428)

-- instance OEIS 1480 where
--   oeisIx n = (oeisIx @196) $ (`div` 3) $ ((oeisIx @7645) n) - ((oeisIx @1479) n) ^ 2

-- instance OEIS 1583 where
--  oeis = filter
--      (\p -> mod ((oeisIx @45) $ div (p - 1) 5) p == 0) (oeis @30430)

-- instance OEIS 1614 where
--   oeis = f 0 0 (oeis @57211) where
--      f c z (x:xs) = z' : f x z' xs where z' = z + 1 + 0 ^ abs (x - c)

-- instance OEIS 1633 where
--  oeis = filter (odd . (oeisIx @55642)) [0..]

-- instance OEIS 1637 where
--   oeis = filter (even . (oeisIx @55642)) [0..]

-- instance OEIS 1650 where
--   (oeisIx @1650) n k = (tabf @1650) !! (n - 1) !! (k-1)
--  (rowT @1650) n = (tabf @1650) !! (n - 1)
--  (tabf @1650) = iterate (\xs@ (x:_) -> map (+ 2) (x:x:xs)) [1]
--  oeis = concat (tabf @1650)

-- instance OEIS 1682 where
--   oeis = [k | k <- [0..], let m = 3^k, (oeisIx @55642) m == (oeisIx @55642) (9*m)]

-- instance OEIS 1690 where
--   oeis = filter ((== 0) . (oeisIx @10056)) [0..]

-- instance OEIS 1692 where
--   oeisIx n = flip div n $ sum $
--               zipWith (*) (map (oeisIx @8683) divs) (map (oeisIx @351) $ reverse divs)
--               where divs = (rowT @27750) n

-- instance OEIS 1694 where
--   oeis = filter ((== 1) . (oeisIx @112526)) [1..]

-- instance OEIS 1764 where
--  oeis = 1 : [a258708 (2 * n) n | n <- [1..]]

-- instance OEIS 1768 where
--   oeisIx n = n * (z - 1) - (2 ^ (z + 2) - 3 * z) `div` 6
--      where z = (oeisIx @85423) $ n + 1

-- instance OEIS 1783 where
--   oeisIx = product . (rowT @38566)

-- instance OEIS 1857 where
--  oeis = 2 : 3 : ulam 2 3 (oeis @1857)

-- instance OEIS 1935 where
--   (oeisIx @1935) = p (oeis @42968) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 1970 where
--  oeis = 1 : f 1 [1] where
--      f x ys = y : f (x + 1) (y : ys) where
--               y = sum (zipWith (*) ys (oeis @61259)) `div` x

-- instance OEIS 1983 where
--  oeis = [x | x <- [0..], (oeisIx @25435) x > 0]

-- instance OEIS 2034 where
--   oeisIx 1 = 1
--   oeisIx n = fromJust ((oeisIx @92495) n `elemIndex` (oeis @142))

-- instance OEIS 2035 where
--  oeis = filter (all odd . (rowT @124010)) [1..]

-- instance OEIS 2095 where
--   (oeisIx @2095) = p (oeis @18252) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 2100 where
--   (oeisIx @2100) = p (oeis @6881) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m


-- instance OEIS 2121 where
--   oeis = 1 : 0 : -1 : f 0 (-1) 3 where
--      f v w x = y : f w y (x + 1) where
--        y = sum (map ((oeisIx @2121) . (x -)) $ takeWhile (<= x) (oeis @65091)) - v

-- instance OEIS 2122 where
--   oeis = uncurry conv $ splitAt 1 $ oeis @2121 where
--      conv xs (z:zs) = sum (zipWith (*) xs $ reverse xs) : conv (z:xs) zs

-- instance OEIS 2123 where
--  oeis = 0 : 0 : f 3 where
--      f x = y : f (x + 1) where
--        y = (oeisIx @61397) x -
--            sum (map ((oeisIx @2123) . (x -)) $ takeWhile (< x) (oeis @65091))

-- instance OEIS 2125 where
--   oeis = uncurry conv $ splitAt 1 (oeis @2124) where
--      conv xs (z:zs) = sum (zipWith (*) xs $ reverse xs) : conv (z:xs) zs

-- instance OEIS 2145 where
--  oeis = filter ((== 1) . (oeisIx @10051)) [3, 7 ..]

-- instance OEIS 2180 where
--   oeisIx = flip div 2 . oeisIx @2202

-- instance OEIS 2294 where
--   oeis = [oeisIx @258708 (3 * n) (2 * n) | n <- [1..]]

-- instance OEIS 2296 where
--  oeis = [a258708 (4 * n) (3 * n) | n <- [1..]]


-- instance OEIS 2312 where
--   oeis = filter (\x -> 2 * x > (oeisIx @6530) (x ^ 2 + 1)) [1..]

-- instance OEIS 2322 where
--   oeisIx n = foldl lcm 1 $ map ((oeisIx @207193) . (oeisIx @95874)) $
--                             zipWith (^) ((rowT @27748) n) ((rowT @124010) n)

-- instance OEIS 2327 where
--  oeis = filter ((== 1) . (oeisIx @10051)') (oeis @28387)

-- instance OEIS 2348 where
--   oeisIx n = product (zipWith d ps es) * 4 ^ e0 `div` 8 where
--      d p e = (p ^ 2 - 1) * p ^ e
--      e0 = if even n then head $ (rowT @124010) n else 0
--      es = map ((* 2) . subtract 1) $
--               if even n then tail $ (rowT @124010) n else (rowT @124010) n
--      ps = if even n then tail $ (rowT @27748) n else (rowT @27748) n

-- instance OEIS 2372 where
--   oeisIx n = sum $ map ((oeisIx @10051) . (2*n -)) $ takeWhile (< 2*n) (oeis @65091)

-- instance OEIS 2373 where
--   oeisIx n = head $ dropWhile ((== 0) . (oeisIx @10051) . (2*n -)) (oeis @65091)

-- instance OEIS 2375 where
--   oeisIx n = sum $ map ((oeisIx @10051) . (2 * n -)) $ takeWhile (<= n) (oeis @65091)

-- instance OEIS 2385 where
--  oeis = filter ((== 1) . (oeisIx @136522)) (oeis @40)

-- instance OEIS 2457 where
--   oeisIx n = (oeisIx @116666) (2 * n + 1) (n + 1)

-- instance OEIS 2471 where
--   oeisIx n = sum $ map ((oeisIx @10051) . (n -)) $ takeWhile (< n) (oeis @290)

-- instance OEIS 2473 where
--   import Data.Set (singleton, deleteFindMin, fromList, union)
--   oeisIx n = (oeis @2473) !! (n - 1)
--  oeis = f $ singleton 1 where
--      f s = x : f (s' `union` fromList (map (* x) [2,3,5,7]))
--            where (x, s') = deleteFindMin s

-- instance OEIS 2479 where
--  oeis = 0 : filter f [1..] where
--      f x = all (even . snd) $ filter ((`elem` [5,7]) . (`mod` 8) . fst) $
--                               zip ((rowT @27748) x) ((rowT @124010) x)

-- instance OEIS 2496 where
--  oeis = filter ((== 1) . (oeisIx @10051)') (oeis @2522)


-- instance OEIS 2503 where
--  oeis = map (+ 1) $ elemIndices 0 (oeis @65350)

-- instance OEIS 2516 where
--  oeis = 0 : concat (transpose
--   [a004767_list, f (oeis @2516), (oeis @17089), g $ drop 2 (oeis @2516)])
--   where f [z] = []; f (_:z:zs) = 2 * z : f zs

-- instance OEIS 2616 where
--   oeisIx = flip div 2 . oeisIx @2322

-- instance OEIS 2618 where
--   oeisIx n = oeisIx @10 n * n

-- instance OEIS 2645 where
--  oeis = 2 : (map (oeisIx @40) $ filter ((> 1) . (oeisIx @256852)) [1..])

-- instance OEIS 2646 where
--  oeis = [hqp | x <- [1, 3 ..], y <- [1, 3 .. x - 1],
--                         let hqp = div (x ^ 4 + y ^ 4) 2, (oeisIx @10051)' hqp == 1]

-- instance OEIS 2654 where
--   oeisIx n = product $ zipWith f ((rowT @27748) m) ((rowT @124010) m) where
--      f p e | p `mod` 4 == 1 = e + 1
--            | otherwise      = (e + 1) `mod` 2
--      m = (oeisIx @265) n

-- instance OEIS 2658 where
--   oeis = 1 : 1 : f [1,1] where
--      f (x:xs) = y : f (y:x:xs') where y = x * sum xs + x * (x + 1) `div` 2

-- instance OEIS 2690 where
--   oeisIx n = (oeisIx @245334) (2 * n) n

-- instance OEIS 2694 where
--   oeisIx (fi->n) = (oeisIx @7318)' (2 * n) (n - 2)

-- instance OEIS 2731 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @982)) [1, 3 ..]

-- instance OEIS 2733 where
--   oeisIx = oeisIx @196 . (subtract 1) . (* 10) . (oeisIx @207337)

-- instance OEIS 2778 where
--  oeis = filter ((== 1) . (oeisIx @136522) . (^ 2)) [0..]

-- instance OEIS 2779 where
--  oeis = filter ((== 1) . (oeisIx @136522)) (oeis @290)

-- instance OEIS 2815 where
--   oeisIx 0 = 0
--   oeisIx n = oeisIx @46992 n + n

-- instance OEIS 2828 where
--   oeisIx 0 = 0
--   oeisIx n
--      | (oeisIx @10052) n == 1 = 1
--      | (oeisIx @25426) n > 0 = 2 | (oeisIx @25427) n > 0 = 3 | otherwise = 4

-- instance OEIS 2858 where
--  oeis = 1 : 2 : ulam 2 2 (oeis @2858)
--   ulam :: Int -> Integer -> [Integer] -> [Integer]
--   ulam n u us = u' : ulam (n + 1) u' us where
--      u' = f 0 (u+1) us'
--      f 2 z _                         = f 0 (z + 1) us'
--      f e z (v:vs) | z - v <= v       = if e == 1 then z else f 0 (z + 1) us'
--                   | z - v `elem` us' = f (e + 1) z vs
--                   | otherwise        = f e z vs
--      us' = take n us

-- instance OEIS 2859 where
--  oeis = 1 : 3 : ulam 2 3 (oeis @2859)

-- instance OEIS 2868 where
--   oeisIx n = if n == 0 then 1 else maximum $ map abs $ (rowT @8297) n

-- instance OEIS 2869 where
--   oeisIx 0 = 1
--   oeisIx n = maximum $ (rowT @19538) n

-- instance OEIS 2944 where
--   oeisIx n = oeisIx @3418 n `div` n

-- instance OEIS 2961 where
--   oeis = map (fi . (+ 1)) $ elemIndices 0 (oeis @53222)

-- instance OEIS 2963 where
--   oeisIx = ch 0 . oeisIx @61493 where
--        ch s 0 = s
--        ch s x = ch (s + [0,1,2,2,2,2,3,4] !! d') x'
--                 where  (x',d) = divMod x 10; d' = fromInteger d

-- instance OEIS 2964 where
--   oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (map (oeisIx @2963) [1..3888]))

-- instance OEIS 2996 where
--   oeisIx n = sum $ zipWith (*) (map (oeisIx @8683) divs) (map (oeisIx @108) $ reverse divs)
--      where divs = (rowT @27750) n

-- instance OEIS 2997 where
--   oeis = [x | x <- (oeis @24556),
--       all (== 0) $ map ((mod (x - 1)) . (subtract 1)) $ (rowT @27748) x]

-- instance OEIS 3038 where
--   import Data.Set (deleteFindMin, fromList, insert)
--   oeisIx n = (oeis @3038) !! (n - 1)
--   oeis = f (fromList (3 : [14, 52, 78, 133, 248]))
--      (drop 2 (oeis @5563)) (drop 4 (oeis @217)) where
--      f s (x:xs) (y:ys) = m : f (x `insert` (y `insert` s')) xs ys where
--        (m, s') = deleteFindMin s

-- instance OEIS 3052 where
  -- oeis = filter ((== 0) . (oeisIx @230093)) [1..]

-- instance OEIS 3071 where
--   oeisIx n = 1 - 2 ^ last es +
--      sum (zipWith (*) (zipWith (+) es [0..]) (map (2 ^) es))
--      where es = reverse $ (rowT @133457) n

-- instance OEIS 3072 where
--   oeis = filter c3 [1..] where
--      c3 x = any (== 1) $ map (oeisIx @10057 . fromInteger) $
--                          takeWhile (> 0) $ map (x -) $ (oeis @3325)

-- instance OEIS 3106 where
--   oeisIx = p (oeis @47221) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 3114 where
--   oeisIx = p (oeis @47209) where
--      p _      0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 3116 where
--   oeisIx n = (oeisIx @168396) (2 * n + 1) n

-- instance OEIS 3169 where
--   oeisIx = flip (oeisIx @100326) 0

-- instance OEIS 3226 where
--   oeis = filter (\x -> show x `isSuffixOf` show (x^2)) (oeis @8851)

-- instance OEIS 3242 where
--   oeis = 1 : f [1] where
--      f xs = y : f (y : xs) where
--             y = sum $ zipWith (*) xs (oeis @48272)

-- instance OEIS 3256 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @242094)) . (oeisIx @1950)

-- instance OEIS 3261 where
--   oeisIx = (subtract 1) . (oeisIx @36289)

-- instance OEIS 3271 where
  -- oeis = map ((+ 1) . fromJust . (`elemIndex` (oeis @49865))) [0..]

-- instance OEIS 3277 where
  -- oeis = map (+ 1) $ elemIndices 1 (oeis @9195)

-- instance OEIS 3325 where
--   oeis = filter c2 [1..] where
--      c2 x = any (== 1) $ map (oeisIx . fromInteger) $
--                          takeWhile (> 0) $ map (x -) $ tail (oeis @578)

-- instance OEIS 3459 where
--   oeis = filter isAbsPrime (oeis @40) where
--      isAbsPrime = all (== 1) . map (oeisIx . read) . permutations . show

-- instance OEIS 3508 where
  -- oeis = 1 : map
  --       (\x -> x + 1 + sum (takeWhile (< x) $ ((rowT @27748) x))) (oeis @3508)

-- instance OEIS 3557 where
--   oeisIx n = product $ zipWith (^)
--                         (oeisIx_row n) (map (subtract 1) $ (rowT @124010) n)

-- instance OEIS 3601 where
  -- oeis = filter ((== 1) . (oeisIx @245656)) [1..]

-- instance OEIS 3607 where
  -- oeis = map fi $ elemIndices 0 (oeis @30190)

-- instance OEIS 3624 where
--   oeis = filter ((== 1) . (oeisIx @9194)) (oeis @2808)

-- instance OEIS 3628 where
  -- oeis = filter ((== 1) . (oeisIx @10051)) (oeis @47566)

-- instance OEIS 3631 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @47221)

-- instance OEIS 3666 where
--   oeis = 1 : 4 : ulam 2 4 (oeis @3666)

-- instance OEIS 3667 where
--   oeis = 1 : 5 : ulam 2 5 (oeis @3667)

-- instance OEIS 3668 where
--   oeis = 2 : 7 : ulam 2 7 (oeis @3668)

-- instance OEIS 3669 where
--   oeis = 3 : 4 : ulam 2 4 (oeis @3669)

-- instance OEIS 3670 where
--   oeis = 4 : 7 : ulam 2 7 (oeis @3670)

-- instance OEIS 3958 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (subtract 1) $ (rowT @27746) n

-- instance OEIS 3963 where
--   oeisIx n = product $
--      zipWith (^) (map (oeisIx @49084) $ (rowT @27748) n) (oeisIx_row n)

-- instance OEIS 3990 where
--   oeisIx x y = (oeisIx @3990)_adiag x !! (y-1)
--   oeisIx_adiag n = (tabl @3990) !! (n - 1)
--   oeisIx_tabl = zipWith (zipWith lcm) (tabl @2260) $ map reverse (tabl @2260)

-- instance OEIS 4000 where
--   oeis = iterate (oeisIx @36839) 1

-- instance OEIS 4051 where
--   oeis = filter ((== 1) . (oeisIx @10051)'') (oeis @4050)

-- instance OEIS 4144 where
--   oeis = map (fi . succ) $ elemIndices 0 (oeis @5089)

-- instance OEIS 4207 where
--   oeis = 1 : iterate (oeisIx @62028) 1

-- instance OEIS 4214 where
--   oeis = filter ((== 0) . (oeisIx @25427)) [1..]

-- instance OEIS 4215 where
--   oeis = filter ((== 4) . (oeisIx @2828)) [1..]

-- instance OEIS 4290 where
--   oeisIx 0 = 0
--   oeisIx n = head [x | x <- tail (oeis @7088), mod x n == 0]

-- instance OEIS 4302 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @103371) (n + 1) 2

-- instance OEIS 4426 where
--   oeisIx n = (oeisIx @7953 n) `div` (oeisIx @55642 n)

-- instance OEIS 4431 where
--   oeis = findIndices (> 1) (oeis @63725)

-- instance OEIS 4435 where
--   oeis = [x | x <- [1..], (oeisIx @25435) x == 0]

-- instance OEIS 4514 where
--   oeisIx = (oeisIx @63695) . (+ 1) . (* 2)

-- instance OEIS 4613 where
--   oeis = filter (all (== 1) . map (oeisIx @79260) . (rowT @27748)) [1..]

-- instance OEIS 4614 where
--   oeis = filter (all (== 1) . map (oeisIx @79261) . (rowT @27748)) [1..]

-- instance OEIS 4615 where
--   oeis = filter (all (== 1) . (map (`mod` 5) . (rowT @27748))) [1..]

-- instance OEIS 4676 where
--   oeisIx = (oeisIx @7088) . (oeisIx @40)

-- instance OEIS 4709 where
--   oeis = filter ((== 1) . (oeisIx @212793)) [1..]

-- instance OEIS 4780 where
--   oeis = filter ((> 1) . (oeisIx @48728)) [1..]

-- instance OEIS 4788 where
--   oeisIx = (oeisIx @1221) . (oeisIx @1142)

-- instance OEIS 4789 where
--   oeisIx = fi . fromJust . (`elemIndex` (oeis @4788))

-- instance OEIS 4957 where
--   oeis = findIndices even (oeis @60142)

-- instance OEIS 4999 where
--   oeis = filter c2 [1..] where
--      c2 x = any (== 1) $ map (oeisIx @10057 . fi) $
--                          takeWhile (>= 0) $ map (x -) $ tail (oeis @578)

-- instance OEIS 5098 where
--   oeisIx = (`div` 4) . (subtract 1) . (oeisIx @2144)
--
-- instance OEIS 5100 where
--   oeis = filter (\x -> (oeisIx @1065) x < x) [1..]
--
-- instance OEIS 5101 where
--   oeis = filter (\x -> (oeisIx @1065) x > x) [1..]

-- instance OEIS 5145 where
--   oeis = tablList @5145
-- instance Table 5145 where
--   rowCol = rowCol_off @5145 @1 @1
--   rowT   = rowT_off   @5145 @1
--   tabl = zipWith ($) (map replicate [1..]) (oeis @40)

-- instance OEIS 5153 where
--   oeis = filter (\x -> all (p $ (rowT @27750) x) [1..x]) [1..]
--      where p _  0 = True
--            p [] _ = False
--            p ds'@ (d:ds) m = d <= m && (p ds (m - d) || p ds m)

-- instance OEIS 5169 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @168396) n 1

-- instance OEIS 5235 where
--   oeisIx n = head [m | m <- [3, 5 ..], (oeisIx @10051)'' (oeisIx n + m) == 1]

-- instance OEIS 5236 where
--   oeis = filter (\x -> all (<= x) $ map (oeisIx @229109) [1..x-1]) [2..]

-- instance OEIS 5245 where
--   oeis = 1 : f 2 [1] where
--      f x ys = y : f (x + 1) (y : ys) where
--        y = minimum $
--            (zipWith (+) (take (x `div` 2) ys) (reverse ys)) ++
--            (zipWith (+) (map (oeisIx @5245) $ tail $ (rowT @161906) x)
--                         (map (oeisIx @5245) $ reverse $ init $ (rowT @161908) x))

-- instance OEIS 5254 where
--   oeisIx = sum . (rowT @37254)

-- instance OEIS 5256 where
--   oeis = map (subtract 2) $ drop 3 (oeis @62178)

-- instance OEIS 5276 where
--   oeis = filter p [1..] where
--      p z = p' z [0, z] where
--        p' x ts = if y `notElem` ts then p' y (y:ts) else y == z
--                  where y = (oeisIx @48050) x

-- instance OEIS 5277 where
--   oeis = filter even (oeis @7617)

-- instance OEIS 5279 where
--   oeis = filter ((> 0) . (oeisIx @174903)) [1..]

-- instance OEIS 5282 where
--   import Data.Set (Set, empty, insert, member)
--   oeisIx n = (oeis @5282) !! (n - 1)
--   oeis = sMianChowla [] 1 empty where
--      sMianChowla :: [Integer] -> Integer -> Set Integer -> [Integer]
--      sMianChowla sums z s | s' == empty = sMianChowla sums (z+1) s
--                           | otherwise   = z : sMianChowla (z:sums) (z+1) s
--         where s' = try (z:sums) s
--               try :: [Integer] -> Set Integer -> Set Integer
--               try []     s                      = s
--               try (x:sums) s | (z+x) `member` s = empty
--                              | otherwise        = try sums $ insert (z+x) s

-- instance OEIS 5341 where
--   oeisIx = genericLength . (rowT @34002)

-- instance OEIS 5349 where
--   oeis = filter ((== 0) . (oeisIx @70635)) [1..]

-- instance OEIS 5374 where
--   oeis = 0 : 1 : zipWith (-)
--      [2..] (map (oeisIx . (oeisIx @5374)) $ tail (oeis @5374))

-- instance OEIS 5378 where
--   oeis = 1 : zipWith (-) [1..] (map (oeisIx @5379) (oeis @5378))

-- instance OEIS 5383 where
--   oeis = [p | p <- (oeis @65091), (oeisIx @10051) ((p + 1) `div` 2) == 1]

-- instance OEIS 5385 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (`div` 2)) (oeis @40)

-- instance OEIS 5413 where
--   oeis = 1 : zipWith (*) [1 ..] (zipWith (+) (tail (oeis @5412)) (zipWith (*) [4, 6 ..] (oeis @5413)))

-- instance OEIS 5418 where
--   oeisIx n = sum $ (rowT @34851) (n - 1)

-- instance OEIS 5428 where
--   oeis = (iterate j (1, 1)) where
--      j (a, s) = (a', (s + a') `mod` 2) where
--        a' = (3 * a + (1 - s) * a `mod` 2) `div` 2

-- instance OEIS 5473 where
--   oeis = filter ((== 1) . (oeisIx @10051)') $ map (+ 4) (oeis @290)

-- instance OEIS 5528 where
--   oeis = filter (\x -> 2 * x <= (oeisIx @6530) (x ^ 2 + 1)) [1..]

-- instance OEIS 5574 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (+ 1) . (^ 2)) [0..]

-- instance OEIS 5592 where
--   oeis = map (subtract 1) $
--                      tail $ zipWith (+) (oeis @1519) $ tail (oeis @1519)

-- instance OEIS 5598 where
--   oeisIx n = 1 + sum (zipWith (*) [n, n - 1 .. 1] (oeis @10))

-- instance OEIS 5658 where
--   import Data.Set (Set, fromList, insert, deleteFindMin)
--   oeisIx n = (oeis @5658) !! (n - 1)
--   oeis = klarner $ fromList [1,2] where
--      klarner :: Set Integer -> [Integer]
--      klarner s = m : (klarner $
--                       insert (2*m) $ insert (3*m+2) $ insert (6*m+3) s')
--         where (m,s') = deleteFindMin s

-- instance OEIS 5707 where
--   oeis = 1 : 1 : 1 : 1 : h 5 1 where
--      h x y = z : h (x + 1) z where z = (oeisIx @5707) y + (oeisIx @5707) (x - y)

-- instance OEIS 5773 where
--   oeis = 1 : f (oeis @1006) [] where
--      f (x:xs) ys = y : f xs (y : ys) where
--        y = x + sum (zipWith (*) (oeis @1006) ys)

-- instance OEIS 5774 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @38622) n 1

-- instance OEIS 5775 where
--   oeisIx = flip (oeisIx @38622) 2 . (subtract 1)

-- instance OEIS 5803 where
--   oeisIx n = 2 ^ n - 2 * n
--   oeis = 1 : f 1 [0, 2 ..] where
--      f x (z:zs@ (z':_)) = y : f y zs  where y = (x + z) * 2 - z'

-- instance OEIS 5811 where
--   oeisIx 0 = 0
--   oeisIx n = genericLength $ group $ (rowT @30308) n
--   oeis = 0 : f [1] where
--      f (x:xs) = x : f (xs ++ [x + x `mod` 2, x + 1 - x `mod` 2])

-- instance OEIS 5835 where
--   oeis = filter ((== 1) . (oeisIx @210455)) [1..]

-- instance OEIS 5845 where
--   oeis = filter (\x -> (oeisIx x - 1) `mod` x == 0) (oeis @2808)

-- instance OEIS 5846 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @202018)

-- instance OEIS 6022 where
--   oeisIx 1 = 0
--   oeisIx n = (+ 1) $ sum $ takeWhile (> 1) $
--             iterate (\x -> x `div` (oeisIx @20639) x) (oeisIx n)

-- instance OEIS 6036 where
--   oeis = filter (all (== 0) . map (oeisIx @210455) . (rowT @27751)) (oeis @5835)

-- instance OEIS 6037 where
--   oeis = filter ((== 0) . (oeisIx @210455)) (oeis @5101)

-- instance OEIS 6038 where
--   oeis = filter f [1, 3 ..] where
--      f x = sum pdivs > x && all (<= 0) (map (\d -> (oeisIx @203) d - 2 * d) pdivs)
--            where pdivs = (rowT @27751) x

-- instance OEIS 6068 where
--   oeisIx n = foldl xor 0 $
--                     map (div n) $ takeWhile (<= n) (oeis @79) :: Integer

-- instance OEIS 6086 where
--   oeis = filter ((== 1) . (oeisIx @103340)) [1..]

-- instance OEIS 6087 where
--   oeis = map numerator $ filter ((== 1) . denominator) $
--      map uhm [1..]  where uhm n = (n * (oeisIx @34444) n) % (oeisIx n)

-- instance OEIS 6127 where
--   oeisIx n = (oeisIx @79) n + n
--   oeis = s [1] where
--      s xs = last xs : (s $ zipWith (+) [1..] (xs ++ reverse xs))

-- instance OEIS 6128 where
--   oeisIx = genericLength . concat . ps 1 where
--      ps _ 0 = [[]]
--      ps i j = [t:ts | t <- [i..j], ts <- ps t (j - t)]

-- instance OEIS 6252 where
--   oeisIx 0 = 1
--   oeisIx n = sum $ (rowT @48594) n

-- instance OEIS 6256 where
--   oeis = f (tail (oeis @5809)) [1] where
--      f (x:xs) zs = (sum $ zipWith (*) zs (oeis @5809)) : f xs (x : zs)

-- instance OEIS 6285 where
--   oeis = filter ((== 0) . (oeisIx @109925)) [1, 3 ..]

-- instance OEIS 6336 where
--   oeis = 1 : h 2 1 0 where
--     h n last evens = x : h (n + 1) x (evens + 1 - x `mod` 2) where
--       x = last + (oeisIx @6336) (n - 1 - evens)

-- instance OEIS 6338 where
--   oeis = tail (oeis @214848)

-- instance OEIS 6343 where
--   oeisIx 0 = 1
--   oeisIx n = sum $ zipWith div
--      (zipWith (*) (map (oeisIx n) ks)
--                   (map (\k -> (oeisIx @7318) (2*n - 3*k - 4) (n - 2*k - 2)) ks))
--      (map (toInteger . (n - 1 -)) ks)
--      where ks = [0 .. (n - 2) `div` 2]

-- instance OEIS 6356 where
  -- oeisIx n = (oeis @6056) !! n
  -- oeis = 1 : 3 : 6 : zipWith (+) (map (2 *) $ drop 2 (oeis @6056))
  --    (zipWith (-) (tail (oeis @6056)) (oeis @6056))

-- instance OEIS 6378 where
--   oeis = map (oeisIx @40) $ filter ((== 0) . (oeisIx @107740)) [1..]

-- instance OEIS 6431 where
--   oeis = filter ((== 1) . (oeisIx @2635)) [0..]

-- instance OEIS 6446 where
--   oeis = filter (\x -> x `mod` (oeisIx @196) x == 0) [1..]

-- instance OEIS 6449 where
--   oeisIx = sum . (rowT @45995)

-- instance OEIS 6451 where
--   oeis = 0 : 2 : 5 : 15 : map (+ 2)
--      (zipWith (-) (map (* 6) (drop 2 (oeis @6451))) (oeis @6451))

-- instance OEIS 6489 where
--   oeis = filter
--      ((== 1) . (oeisIx @10051) . (subtract 6)) $ dropWhile (<= 6) (oeis @23201)

-- instance OEIS 6507 where
--   oeis = iterate (oeisIx @62028) 7

-- instance OEIS 6509 where
--   oeis = 1 : f [1] (oeis @40) where
--      f xs'@ (x:_) (p:ps) | x' > 0 && x' `notElem` xs = x' : f (x':xs) ps
--                         | x'' `notElem` xs          = x'' : f (x'':xs) ps
--                         | otherwise                 = 0 : f (0:xs) ps
--                         where x' = x - p; x'' = x + p

-- instance OEIS 6512 where
--   oeisIx = (+ 2) . (oeisIx @1359)

-- instance OEIS 6521 where
--   oeis = filter (\x -> (oeisIx @51) x `mod` x == 0) [1..]

-- instance OEIS 6532 where
--   oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @203)) [1..]

-- instance OEIS 6549 where
--   oeis = [1,2,3,4,7,8] ++ f (drop 4 (oeis @40)) where
--      f (p:ps) | (oeisIx @10055) (p - 1) == 1 = (p - 1) : f ps
--               | (oeisIx @10055) (p + 1) == 1 = p : f ps
--               | otherwise            = f ps

-- instance OEIS 6562 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @75540)

-- instance OEIS 6567 where
--   oeis = filter f (oeis @40) where
--      f p = (oeisIx @10051)' q == 1 && q /= p  where q = (oeisIx @4086) p

-- instance OEIS 6577 where
--   oeisIx n = fromJust $ findIndex (n `elem`) (tabf @127824)

-- instance OEIS 6580 where
--   oeis = map sum (tabl @3990)

-- instance OEIS 6590 where
--   oeisIx n = sum $ map f [1..n] where
--      f x = y + 1 - 0 ^ r where (y, r) = divMod n x

-- instance OEIS 6601 where
--   oeis = map (+ 1) $ elemIndices 0 $
--      zipWith3 (((+) .) . (+)) ds (tail ds) (drop 2 ds) where
--      ds = map abs $ zipWith (-) (tail (oeis @5)) (oeis @5)

-- instance OEIS 6659 where
--   oeisIx n = 2 * (oeisIx @7318)' (2 * n + 2) (n - 1)

-- instance OEIS 6696 where
--   oeis = 0 : f 1 [0] where
--      f u vs = w : f (u + 1) (w : vs) where
--        w = minimum $ zipWith (+)
--            (reverse vs) (zipWith (*) (tail (oeis @79)) (map (+ u) vs))

-- instance OEIS 6711 where
--   oeis = iterate (oeisIx . (oeisIx @4086)) 1

-- instance OEIS 6720 where
--   oeis = [1,1,1,1] ++
--      zipWith div (foldr1 (zipWith (+)) (map b [1..2])) (oeis @6720)
--      where b i = zipWith (*) (drop i (oeis @6720)) (drop (4-i) (oeis @6720))

-- instance OEIS 6721 where
--   oeis = [1,1,1,1,1] ++
--     zipWith div (foldr1 (zipWith (+)) (map b [1..2])) (oeis @6721)
--     where b i = zipWith (*) (drop i (oeis @6721)) (drop (5-i) (oeis @6721))

-- instance OEIS 6722 where
--   oeis = [1,1,1,1,1,1] ++
--     zipWith div (foldr1 (zipWith (+)) (map b [1..3])) (oeis @6722)
--     where b i = zipWith (*) (drop i (oeis @6722)) (drop (6-i) (oeis @6722))

-- instance OEIS 6723 where
--   oeis = [1,1,1,1,1,1,1] ++
--     zipWith div (foldr1 (zipWith (+)) (map b [1..3])) (oeis @6723)
--     where b i = zipWith (*) (drop i (oeis @6723)) (drop (7-i) (oeis @6723))

-- instance OEIS 6751 where
--   oeisIx = foldl1 (\v d -> 10 * v + d) . map toInteger . (rowT @88203)

-- instance OEIS 6753 where
--   oeis = [x | x <- (oeis @2808),
--                       oeisIx x == sum (map (oeisIx @7953) (oeisIx_row x))]

-- instance OEIS 6844 where
--   oeis = 4 : 5 : ulam 2 5 (oeis @6844)

-- instance OEIS 6872 where
--   oeis = filter (\x -> (oeisIx @10)' x == (oeisIx @10)' (oeisIx' x)) [1..]

-- instance OEIS 6884 where
--   oeis = f 1 0 (oeis @25586) where
--      f i r (x:xs) = if x > r then i : f (i + 1) x xs else f (i + 1) r xs

-- instance OEIS 6885 where
--   oeisIx = (oeisIx @25586) . (oeisIx @6884)

-- instance OEIS 6889 where
--   oeisIx = fromJust . (`elemIndex` (oeis @224782))

-- instance OEIS 6895 where
--   oeis = 1 : f 0 0 (tail (oeis @79)) (tail (oeis @244)) where
--      f x y us'@ (u:us) vs'@ (v:vs)
--        | x > 0     = (u - x) : f 0 (u - x + y) us vs'
--        | y > v - u = (v - y) : f (v + x - y) 0 us' vs
--        | otherwise =       u : f 0 (u + y) us vs'

-- instance OEIS 6921 where
--   oeisIx = sum . zipWith (*)
--                   (oeis @79) . map (flip mod 2) . reverse . (rowT @11973)

-- instance OEIS 6933 where
--   import Data.Text (Text); import qualified Data.Text as T (unpack)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @6933) !! (n - 1)
--   oeis = filter (T.all (/= 'e') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 6942 where
--   oeis = [6,2,5,5,4,5,6,3,7,6] ++ f 10 where
--      f x = (oeisIx x' + (oeisIx @6942) d) : f (x + 1)
--            where (x',d) = divMod x 10

-- instance OEIS 6960 where
--   oeis = iterate (oeisIx @56964) 196

-- instance OEIS 6992 where
--   oeis = iterate (oeisIx @7917 . (* 2)) 2

-- instance OEIS 7012 where
--   oeis = 1 : map (sum . map signum) (tail (tabl @53200))

-- instance OEIS 7015 where
--   oeisIx n = 1 + (fromJust $
--               elemIndex 0 $ zipWith (-) (oeis @10) $ drop n (oeis @10))

-- instance OEIS 7018 where
--   oeis = iterate (oeisIx @2378) 1

-- instance OEIS 7047 where
--   oeisIx = sum . (rowT @38719)

-- instance OEIS 7066 where
--   oeis = 1 : f 2 [1] where
--      f x zs@ (z:_) = y : f (x + 1) (y : zs) where
--        y = if x `elem` zs then z + 2 else z + 3

-- instance OEIS 7300 where
--   oeis = 2 : 5 : ulam 2 5 (oeis @7300)

-- instance OEIS 7304 where
--   oeis = filter f [1..] where
--   f u = p < q && q < w && (oeisIx @10051) w == 1 where
--   p = (oeisIx @20639) u; v = div u p; q = (oeisIx @20639) v; w = div v q

-- instance OEIS 7340 where
--   oeis = filter ((== 0) . (oeisIx @54025)) (oeis @1599)

-- instance OEIS 7367 where
--   oeis = map fst $
--                  filter ((== 3) . snd) $ zip (oeis @2202) (oeis @58277)

-- instance OEIS 7377 where
--   oeis = elemIndices 0 (oeis @27870)

-- instance OEIS 7401 where
--   oeis = [x | x <- [0..], (oeisIx @23531) x == 0]

-- instance OEIS 7412 where
--   oeisIx n = n + (oeisIx @48766) (n + (oeisIx @48766) n)

-- instance OEIS 7416 where
--   oeis = f 1 [] where
--      f x ts = if tau `elem` ts then f (x + 1) ts else x : f (x + 1) (tau:ts)
--               where tau = (oeisIx @5)' x

-- instance OEIS 7425 where
--   oeisIx = sum . map (oeisIx @5) . (rowT @27750)

-- instance OEIS 7427 where
--   oeisIx n = sum $ zipWith (*) mds $ reverse mds where
--      mds = (rowT @225817) n

-- instance OEIS 7428 where
--   oeisIx n = product
--      [oeisIx' 3 e * cycle [1,-1] !! fi e | e <- (rowT @124010) n]

-- instance OEIS 7430 where
--   oeisIx n = sum $ zipWith (*) (map (oeisIx @5) ds) (map (oeisIx @203) $ reverse ds)
--               where ds = (rowT @27750) n

-- instance OEIS 7431 where
--   oeisIx 0 = 0
--   oeisIx n = sum $ map (oeisIx @8683 . gcd n) [1..n]

-- instance OEIS 7434 where
--   oeisIx n = sum $ zipWith3 (\x y z -> x * y * z)
--                     tdivs (reverse tdivs) (reverse divs)
--                     where divs = (rowT @27750) n;  tdivs = map (oeisIx @10) divs

-- instance OEIS 7439 where
--   oeis = 1 : 1 : f 2 where
--      f x = (sum $ map (oeisIx @7439) $ (rowT @27750) (x - 1)) : f (x + 1)

-- instance OEIS 7457 where
--   oeisIx n = genericLength [k | k <- [1..n - 1], gcd k n == 1, (oeisIx @8966) k == 1,
--                           let j = n - k, gcd j n == 1, (oeisIx @8966) j == 1]

-- instance OEIS 7459 where
--   oeis = f 1 (oeis @40) where
--     f q (p:ps) = if mod q (p - 1) == 0 then p : f (q * p ^ 2) ps else f q ps

-- instance OEIS 7460 where
--   oeis = 1 : f [1,1] where
--      f xs = x : f (x:xs) where
--        x = sum $ zipWith (.|.) xs $ tail $ reverse xs :: Integer

-- instance OEIS 7461 where
--   oeis = 1 : f [1,1] where
--      f xs = x : f (x:xs) where
--        x = sum $ zipWith (.&.) xs $ tail $ reverse xs :: Integer

-- instance OEIS 7462 where
--   oeis = 0 : 1 : f [1,0] where
--      f xs = y : f (y : xs) where
--        y = sum $ zipWith xor xs $ reverse xs :: Integer

-- instance OEIS 7466 where
--   oeisIx n = (oeisIx @228643) n n

-- instance OEIS 7491 where
--   oeisIx = (oeisIx @7918) . (oeisIx @290)

-- instance OEIS 7500 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @4086)) (oeis @40)

-- instance OEIS 7503 where
--   oeisIx = sum . map (+ 1) . (rowT @27750)'

-- instance OEIS 7505 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @83329)

-- instance OEIS 7521 where
--   oeis = filter ((== 1). (oeisIx @10051)) (oeis @4770)

-- instance OEIS 7528 where
--   oeis = [x | k <- [0..], let x = 6 * k + 5, (oeisIx @10051)' x == 1]

-- instance OEIS 7534 where
--   import qualified Data.Set as Set (map, null)
--   import Data.Set (empty, insert, intersection)
--   oeisIx n = (oeis @7534) !! (n - 1)
--   oeis = f [2,4..] empty 1 (oeis @1097) where
--      f xs'@ (x:xs) s m ps'@ (p:ps)
--        | x > m = f xs' (insert p s) p ps
--        | Set.null (s `intersection` Set.map (x -) s) = x : f xs s m ps'
--        | otherwise = f xs s m ps'

-- instance OEIS 7535 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = head [m | m <- dropWhile (<= n) (oeis @2808),
--                         powerMod n (m - 1) m == 1]

-- instance OEIS 7542 where
--   oeis = iterate (oeisIx @203907) 2

-- instance OEIS 7547 where
--   oeis = tail $ elemIndices 2 $ map (oeisIx @6530) (oeis @7542)

-- instance OEIS 7554 where
--   oeis = 1 : f 1 where
--      f x = (sum $ zipWith (*) (map (oeisIx @8683) divs)
--                               (map (oeisIx @7554) $ reverse divs)) : f (x + 1)
--             where divs = (rowT @27750) x

-- instance OEIS 7556 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @7318)' (8 * n) (n - 1) `div` n

-- instance OEIS 7599 where
--   oeis = 1 : f 1 1  where
--      f x m = y : f y (m + 1) where
--        y = x + (iterate (oeisIx @7599) (m-2)) !! (m `div` 2)

-- instance OEIS 7602 where
--   oeis = map succ $ elemIndices 1 $ map (oeisIx @188642) [1..]

-- instance OEIS 7608 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @7608) n' * 10 + m where
--      (n', m) = if r < 0 then (q + 1, r + 4) else (q, r)
--                where (q, r) = quotRem n (negate 4)

-- instance OEIS 7614 where
--   import Data.List.Ordered (insertBag)
--   oeisIx n = (oeis @7614) !! (n - 1)
--   oeis = f [1..] (oeis @2110) [] where
--      f xs'@ (x:xs) ps'@ (p:ps) us
--        | x < p = f xs ps' $ insertBag (oeisIx' x) us
--        | otherwise = vs ++ f xs' ps ws
--        where (vs, ws) = span (<= (oeisIx @10)' x) us

-- instance OEIS 7617 where
--   oeis = [1..] `O.minus` (oeis @2202)

-- instance OEIS 7618 where
--   oeis = iterate (oeisIx @62028) 5

-- instance OEIS 7620 where
--   oeis = 1 : filter (\x -> all (p $ (rowT @27751) x) [1..x]) [2..]
--      where p _  0 = True
--            p [] _ = False
--            p ds'@ (d:ds) m = d <= m && (p ds (m - d) || p ds m)

-- instance OEIS 7623 where
--   oeisIx n | n <= 36287999 = read $ concatMap show (oeisIx_row n) :: Int
--             | otherwise     = error "representation would be ambiguous"

-- instance OEIS 7627 where
--   oeis = filter modest' [1..] where
--      modest' x = or $ zipWith m
--                  (map read $ (init $ tail $ inits $ show x) :: [Integer])
--                  (map read $ (tail $ init $ tails $ show x) :: [Integer])
--         where m u v = u < v && (x - u) `mod` v == 0 && gcd u v == 1

-- instance OEIS 7628 where
--   oeis = filter f (oeis @125308) where
--      f p = (oeisIx @10051)' q == 1 && q /= p  where q = (oeisIx @4086) p

-- instance OEIS 7629 where
--   oeis = filter isKeith [10..] where
--      isKeith n = repfigit $ reverse $ map digitToInt $ show n where
--         repfigit ns = s == n || s < n && (repfigit $ s : init ns) where
--            s = sum ns

-- instance OEIS 7632 where
--   oeis = filter ((== 1) . (oeisIx @178225)) (oeis @2113)

-- instance OEIS 7645 where
--   oeis = filter ((== 1) . (oeisIx @10051)) $ tail (oeis @3136)

-- instance OEIS 7651 where
--   oeisIx = foldl1 (\v d -> 10 * v + d) . map toInteger . (rowT @220424)

-- instance OEIS 7660 where
--   oeis = 0 : 0 : map (+ 1)
--                          (zipWith (*) (oeis @7660) $ tail (oeis @7660))

-- instance OEIS 7664 where
--   oeisIx = sum . map (oeisIx . (oeisIx @3056)) . enumFromTo 0 . subtract 1

-- instance OEIS 7689 where
--   oeisIx n = (oeisIx @79) n + (oeisIx @244) n

-- instance OEIS 7691 where
--   oeis = filter ((== 1) . (oeisIx @17666)) [1..]

-- instance OEIS 7692 where
--   oeis = findIndices (> 1) (oeis @25426)

-- instance OEIS 7694 where
--   oeis = 1 : filter even (oeis @3586)

-- instance OEIS 7697 where
--   oeisIx n = 2 * (fromJust $ findIndex (>= n) (oeis @46921)) + 1

-- instance OEIS 7733 where
--   oeisIx = (oeisIx @2326) . flip div 2 . subtract 1 . (oeisIx @265)

-- instance OEIS 7755 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @3434)) . (subtract 1)

-- instance OEIS 7770 where
--   oeis = filter ((== 1) . (oeisIx @103369)) [1..]

-- instance OEIS 7774 where
--   oeis = filter ((== 2) . (oeisIx @1221)) [1..]

-- instance OEIS 7811 where
--   oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051)') $
--      iterate (zipWith (+) [10, 10, 10, 10]) [1, 3, 7, 9]

-- instance OEIS 7862 where
--   oeisIx = sum . map (oeisIx @10054) . (rowT @27750)

-- instance OEIS 7875 where
--   oeisIx = genericLength . filter (> 0) . (rowT @225817)

-- instance OEIS 7921 where
--   oeis = filter ((== 0) . (oeisIx @10051)' . (+ 2)) [1, 3 ..]

-- instance OEIS 7928 where
--   oeis = findIndices (> 0) (oeis @196563)

-- instance OEIS 7931 where
--   oeisIx n = f (n + 1) where
--      f x = if x < 2 then 0 else (10 * f x') + m + 1
--        where (x', m) = divMod x 2

-- instance OEIS 7948 where
--   oeisIx = last . filter ((== 1) . (oeisIx @212793)) . (rowT @27750)

-- instance OEIS 7949 where
--   oeisIx n = if m > 0 then 0 else 1 + (oeisIx @7949) n'
--               where (n', m) = divMod n 3

-- instance OEIS 7957 where
--   oeis = findIndices (> 0) (oeis @196564)

-- instance OEIS 7961 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = read $ map intToDigit $
--     t n $ reverse $ takeWhile (<= n) $ tail (oeis @290) where
--       t _ []          = []
--       t m (x:xs)
--           | x > m     = 0 : t m xs
--           | otherwise = (fromInteger m') : t r xs
--           where (m',r) = divMod m x

-- instance OEIS 7966 where
--   oeis = map fst hCouples

-- instance OEIS 7967 where
--   oeis = map snd hCouples

-- instance OEIS 7968 where
--   oeisIx = (\ (hType,_,_,_,_) -> hType) . h
--   h 0 = (0, 0, 0, 0, 0)
--   h x = if a > 0 then (0, a, a, a, a) else h' 1 divs
--         where a = (oeisIx @37213) x
--               divs = (rowT @27750) x
--               h' r []                                = h' (r + 1) divs
--               h' r (d:ds)
--                | d' > 1 && rest1 == 0 && ss == s ^ 2 = (1, d, d', r, s)
--                | rest2 == 0 && odd u && uu == u ^ 2  = (2, d, d', t, u)
--                | otherwise                           = h' r ds
--                where (ss, rest1) = divMod (d * r ^ 2 + 1) d'
--                      (uu, rest2) = divMod (d * t ^ 2 + 2) d'
--                      s = (oeisIx @196) ss; u = (oeisIx @196) uu; t = 2 * r - 1
--                      d' = div x d
--   hs = map h [0..]
--   hCouples = map (\ (_, factor1, factor2, _, _) -> (factor1, factor2)) hs
--   sqrtPair n = genericIndex sqrtPairs (n - 1)
--   sqrtPairs = map (\ (_, _, _, sqrt1, sqrt2) -> (sqrt1, sqrt2)) hs

-- instance OEIS 7969 where
--   oeis = filter ((== 1) . (oeisIx @7968)) [0..]

-- instance OEIS 7970 where
--   oeis = filter ((== 2) . (oeisIx @7968)) [0..]

-- instance OEIS 7978 where
--   oeisIx = head . ([1..] \\) . (rowT @27750)

-- instance OEIS 7997 where
--   oeisIx n = ceiling $ (fi $ (n - 3) * (n - 4)) / 6
--   oeis = 0 : 0 : 1 : zipWith (+) (oeis @7997) [1..]

-- instance OEIS 8275 where
--   oeis = tablList @8275
-- instance Table 8275 where
--   rowCol = rowCol_off @8275 @1 @1
--   rowT   = rowT_off   @8275 @1
--   tabl = map tail $ tail (tabl @48994)

-- instance OEIS 8276 where
--   oeis = tablList @8276
-- instance Table 8276 where
--   rowCol = rowCol_off @8276 @1 @1
--   rowT   = rowT_off   @8276 @1
--   tabl = map init $ tail (tabl @54654)

-- instance OEIS 8277 where
--   oeis = tablList @8277
-- instance Table 8277 where
--   rowCol = rowCol_off @8277 @1 @1
--   rowT   = rowT_off   @8277 @1
--   tabl = map tail $ (tabl @48993)

-- instance OEIS 8278 where
--   oeis = tablList @8278
-- instance Table 8278 where
--   rowCol = rowCol_off @8278 @1 @1
--   rowT   = rowT_off   @8278 @1
--   tabl = iterate st2 [1] where
--     st2 row = zipWith (+) ([0] ++ row') (row ++ [0])
--               where row' = reverse $ zipWith (*) [1..] $ reverse row

-- instance OEIS 8279 where
--   oeis = tablList @8279
-- instance Table 8279 where
--   tabl = iterate f [1] where
--      f xs = zipWith (+) ([0] ++ zipWith (*) xs [1..]) (xs ++ [0])

-- instance OEIS 8284 where
--   oeis = tablList @8284
-- instance Table 8284 where
--   rowCol = rowCol_off @8284 @1 @1
--   rowT   = rowT_off   @8284 @1
--   tabl = [1] : f [[1]] where
--      f xss = ys : f (ys : xss) where
--        ys = (map sum $ zipWith take [1..] xss) ++ [1]

-- instance OEIS 8287 where
--   oeis = concat $ iterate ([1,1,1,1] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 8288 where
--   oeis = tablList @8288
-- instance Table 8288 where
--   tabl = map fst $ iterate
--       (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
--                          zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

-- instance OEIS 8290 where
--   oeis = tablList @8290
-- instance Table 8290 where
--   tabl = map reverse (tabl @98825)

-- instance OEIS 8292 where
--   oeis = tablList @8292
-- instance Table 8292 where
--   rowCol = rowCol_off @8292 @1 @1
--   rowT   = rowT_off   @8292 @1
--   tabl = iterate f [1] where
--      f xs = zipWith (+)
--        (zipWith (*) ([0] ++ xs) (reverse ks)) (zipWith (*) (xs ++ [0]) ks)
--        where ks = [1 .. 1 + genericLength xs]

-- instance OEIS 8297 where
--   oeis = tablList @8297
-- instance Table 8297 where
--   rowCol = rowCol_off @8297 @1 @1
--   rowT   = rowT_off   @8297 @1
--   tabl = [-1] : f [-1] 2 where
--      f xs i = ys : f ys (i + 1) where
--        ys = map negate $
--             zipWith (+) ([0] ++ xs) (zipWith (*) [i, i + 1 ..] (xs ++ [0]))

-- instance OEIS 8301 where
--   oeis = tablList @8301
-- instance Table 8301 where
--   tabf = iterate f [1] where
--      f zs = zs' ++ tail (reverse zs') where
--        zs' = (sum zs) : h (0 : take (length zs `div` 2) zs) (sum zs) 0
--        h []     _  _ = []
--        h (x:xs) y' y = y'' : h xs y'' y' where y'' = 2*y' - 2*x - y

-- instance OEIS 8306 where
--   oeis = tablList @8306
-- instance Table 8306 where
--   rowCol = rowCol_off @8306 @2 @1
--   rowT   = rowT_off @8306 @2
--   tabf = map (fst . fst) $ iterate f (([1], [2]), 3) where
--      f ((us, vs), x) =
--        ((vs, map (* x) $ zipWith (+) ([0] ++ us) (vs ++ [0])), x + 1)

-- instance OEIS 8313 where
--   oeis = tablList @8313
-- instance Table 8313 where
--   tabf = map (filter (> 0)) (tabl @53121)

-- instance OEIS 8315 where
--   oeis = tablList @8315
-- instance Table 8315 where
--   tabf = map reverse (tabf @8313)

-- instance OEIS 8365 where
--   oeis = 1 : filter ((> 11) . (oeisIx @20639)) [1..]

-- instance OEIS 8438 where
--   oeisIx = (oeisIx @203) . (oeisIx @5408)

-- instance OEIS 8441 where
--   oeisIx = (flip div 2) . (+ 1) . (oeisIx @8441)

-- instance OEIS 8473 where
  -- oeisIx n = product $ zipWith (+) (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 8474 where
--   oeisIx n = sum $ zipWith (+) (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 8475 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ (rowT @141809) n

-- instance OEIS 8477 where
--   oeisIx n = product $ zipWith (^) (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 8520 where
--   import Data.Text (Text); import qualified Data.Text as T (any)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @8520) !! (n - 1)
--   oeis = filter (T.any (== 'e') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 8521 where
--   import Data.Text (Text); import qualified Data.Text as T (all)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @8521) !! (n - 1)
--   oeis = filter (T.all (/= 'o') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 8523 where
--   import Data.Text (Text); import qualified Data.Text as T (all)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @8523) !! (n - 1)
--   oeis = filter (T.all (/= 't') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 8784 where
--   oeis = 1 : 2 : O.union (oeis @4613) (map (* 2) (oeis @4613))

-- instance OEIS 8810 where
--   oeisIx = ceiling . (/ 3) . fromInteger . (oeisIx @290)
--   oeis = [0,1,2,3,6] ++ zipWith5
--                  (\u v w x y -> 2 * u - v + w - 2 * x + y)
--      (drop 4 (oeis @8810)) (drop 3 (oeis @8810)) (drop 2 (oeis @8810))
--      (tail (oeis @8810)) (oeis @8810)

-- instance OEIS 8833 where
--   oeisIx n = head $ filter ((== 0) . (mod n)) $
--      reverse $ takeWhile (<= n) $ tail (oeis @290)

-- instance OEIS 8847 where
--   oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @203) . (oeisIx @290)) [1..]

-- instance OEIS 8905 where
--   oeisIx = (oeisIx @30) . (oeisIx @142)

-- instance OEIS 8957 where
--   oeis = tablList @8957
-- instance Table 8957 where
--   rowCol = rowCol_off @8957 @1 @1
--   rowT   = rowT_off   @8957 @1
--   tabl = map reverse (tabl @36969)

-- instance OEIS 8975 where
--   oeis = tablList @8975
-- instance Table 8975 where
--   tabl = iterate
--      (\row -> map (`mod` 10) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

-- instance OEIS 8996 where
--   oeis = 1 : f 0 (filter (> 1) $
--                           map length $ group $ drop 3 (oeis @10051))
--      where f m (u : us) = if u <= m then f m us else u : f u us

-- instance OEIS 9003 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @5089)

-- instance OEIS 9023 where
--   oeis = filter ((> 1) . (oeisIx @227481)) [1..]

-- instance OEIS 9087 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

-- instance OEIS 9191 where
--   oeisIx n = gcd n $ (oeisIx @5) n

-- instance OEIS 9194 where
--   oeisIx n = gcd (oeisIx n) n

-- instance OEIS 9195 where
--   oeisIx n = n `gcd` (oeisIx @10) n

-- instance OEIS 9223 where
--   oeisIx n = gcd (oeisIx n) (oeisIx n)

-- instance OEIS 9293 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @9293) !! (n - 1)
--   oeis = f [2] (singleton 2) where
--      f xs s = m : f xs' (foldl (flip insert) s' (map ((+ 1) . (* m)) xs'))
--        where xs' = m : xs
--              (m,s') = deleteFindMin s

-- instance OEIS 9299 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @9299) !! (n - 1)
--   oeis = f [2] (singleton 2) where
--      f xs s = m : f xs' (foldl (flip insert) s' (map ((+ 2) . (* m)) xs'))
--        where xs' = m : xs
--              (m,s') = deleteFindMin s

-- instance OEIS 9388 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @9388) !! (n - 1)
--   oeis = f [2] (singleton 2) where
--      f xs s = m : f xs' (foldl (flip insert) s' (map (pred . (* m)) xs'))
--        where xs' = m : xs
--              (m,s') = deleteFindMin s

-- instance OEIS 9445 where
--   oeisIx n = product [1..2*n+1]
--   T = taylor (sin (x^2), x, 0, 70)

-- instance OEIS 9994 where
--   import Data.Set (fromList, deleteFindMin, insert)
--   oeisIx n = (oeis @9994) !! n
--   oeis = 0 : f (fromList [1..9]) where
--      f s = m : f (foldl (flip insert) s' $ map (10*m +) [m `mod` 10 ..9])
--            where (m,s') = deleteFindMin s

-- instance OEIS 9995 where
--   import Data.Set (fromList, minView, insert)
--   oeisIx n = (oeis @9995) !! n
--   oeis = 0 : f (fromList [1..9]) where
--      f s = case minView s of
--            Nothing     -> []
--            Just (m,s') -> m : f (foldl (flip insert) s' $
--                                 map (10*m +) [0..m `mod` 10 - 1])

-- instance OEIS 9998 where
--   oeis = tablList @9998
--   rowCol n k = (k + 1) ^ (n - k)
--   rowT n = (tabl @9998) !! n
--   tabl = map reverse (tabl @9999)

-- instance OEIS 9999 where
--   oeis = tablList @9999
--   rowCol n k = (n + 1 - k) ^ k
--   rowT n = (tabl @9999) !! n
--   tabl = [1] : map snd (iterate f ([1,1], [1,1])) where
--      f (us@ (u:_), vs) = (us', 1 : zipWith (*) us' vs)
--                         where us' = (u + 1) : us

-- instance OEIS 10049 where
--   oeis = uncurry c $ splitAt 1 (oeis @45) where
--      c us (v:vs) = (sum $ zipWith (*) us (1 : reverse us)) : c (v:us) vs


-- instance OEIS 10056 where
--   oeisIx = genericIndex (oeis @10056)
--   oeis = 1 : 1 : ch [2..] (drop 3 (oeis @45)) where
--      ch (x:xs) fs'@ (f:fs) = if x == f then 1 : ch xs fs else 0 : ch xs fs'

-- instance OEIS 10061 where
--   oeis = filter ((== 0) . (oeisIx @228085)) [1..]

-- instance OEIS 10062 where
--   oeis = iterate (oeisIx @92391) 1

-- instance OEIS 10078 where
--   oeisIx = x . subtract 1 where
--      x m = if m == 0 then 1 else 2 * x m' + 1 - b
--               where (m',b) = divMod m 2

-- instance OEIS 10096 where
--   oeisIx = genericLength . takeWhile (/= 0) . iterate (oeisIx @523)

-- instance OEIS 10120 where
--   oeisIx = (oeisIx @70167) . (oeisIx @79)

-- instance OEIS 10371 where
--   oeis = [6,2,5,5,4,5,6,4,7,6] ++ f 10 where
--      f x = (oeisIx x' + (oeisIx @10371) d) : f (x + 1)
--            where (x',d) = divMod x 10

-- instance OEIS 10554 where
--   oeisIx = (oeisIx @10) . (oeisIx @10)

-- instance OEIS 10683 where
--   oeisIx = sum . (rowT @144944)

-- instance OEIS 10704 where
--   oeisIx n = (* 3) . (oeisIx @34)
--   oeis = cycle [3,6]

-- instance OEIS 10712 where
--   oeisIx = (+ 3) . (4 ^) . flip mod 2
--   oeis = cycle [4, 7]

-- instance OEIS 10766 where
--   rowCol = div
--   rowT = rowT_off @10766 @1
--   tabl = zipWith (map . div) [1..] (tabl @2260)

-- instance OEIS 10783 where
--   oeis = tablList @10783
-- instance Table 10783 where
--   rowCol n k = (n + 1 - k) `div` k
--   rowT   = rowT_off   @10783 @1
--   tabl = map reverse (tabl @10766)

-- instance OEIS 10784 where
--   oeis = filter ((== 1) . (oeisIx @178788)) [1..]

-- instance OEIS 11262 where
--   oeisIx n = product $ zipWith (^)
--                         (oeisIx_row n) (map (oeisIx @103889) $ (rowT @124010) n)

-- instance OEIS 11264 where
--   oeisIx n = product $ zipWith (^)
--                         (oeisIx_row n) (map (oeisIx @4442) $ (rowT @124010) n)

-- instance OEIS 11379 where
--   oeisIx n = (oeisIx @290) n + (oeisIx @578) n

-- instance OEIS 11539 where
--   oeis = filter ((> 0) . (oeisIx @102683)) [1..]

-- instance OEIS 11540 where
--   oeis = filter ((== 0) . (oeisIx @168046)) [0..]

-- instance OEIS 11754 where
--   oeisIx = (oeisIx @120) . (oeisIx @244)

-- instance OEIS 11756 where
--   oeis = map (oeisIx @40) $ tail (oeis @217)

-- instance OEIS 11773 where
--   oeisIx n = foldl lcm 1 $ map (oeisIx . (oeisIx @95874)) $
--                             zipWith (^) (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 11776 where
--   oeisIx 1 = 1
--   oeisIx n = genericLength $
--      takeWhile ((== 0) . (mod (oeisIx n))) $ iterate (* n) n

-- instance OEIS 11784 where
--   oeisIx = last . (rowT @12257)

-- instance OEIS 11971 where
--   oeis = tablList @11971
-- instance Table 11971 where
--   tabl = iterate (\row -> scanl (+) (last row) row) [1]

-- instance OEIS 11973 where
--   oeis = tablList @11973
-- instance Table 11973 where
--   tabf = zipWith (zipWith (oeisIx @7318)) (tabl @25581) (tabf @55087)

-- instance OEIS 12257 where
--   oeis = tablList @12257
-- instance Table 12257 where
--   rowCol = rowCol_off @12257 @1 @1
--   rowT   = rowT_off @12257 @1
--   tabf = iterate (\row -> concat $
--                           zipWith replicate (reverse row) [1..]) [1, 1]

-- instance OEIS 13609 where
--   oeis = concat $ iterate ([1,2] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 13610 where
--   oeis = tablList @13610
-- instance Table 13610 where
--   tabl = iterate (\row ->
--      zipWith (+) (map (* 1) (row ++ [0])) (map (* 3) ([0] ++ row))) [1]

-- instance OEIS 13613 where
--   oeis = tablList @13613
-- instance Table 13613 where
--   tabl = zipWith (zipWith (*))
--                  (tail $ inits (oeis @400)) (tabl @7318)

-- instance OEIS 13620 where
--   oeis = tablList @13620
-- instance Table 13620 where
--   tabl = iterate (\row ->
--      zipWith (+) (map (* 2) (row ++ [0])) (map (* 3) ([0] ++ row))) [1]

-- instance OEIS 13638 where
--   oeisIx n = (oeisIx @151799) n * (oeisIx @151800) n

-- instance OEIS 13918 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @7504)

-- instance OEIS 13942 where
--   oeis = tablList @13942
-- instance Table 13942 where
--   rowCol = rowCol_off @13942 @1 @1
--   rowT n = map (div (n * 2)) [1 .. 2 * n]
--   tabf = map (rowT @13942) [1 ..]

-- instance OEIS 14081 where
--   oeisIx n = (oeisIx @120) (n .&. div n 2)

-- instance OEIS 14082 where
--   oeisIx = sum . map (fromEnum . ([1,1,1] `isPrefixOf`)) .
--                       tails . (rowT @30308)

-- instance OEIS 14085 where
--   oeisIx n = sum $ map (oeisIx @10051) [n^2.. (n+1)^2]

-- instance OEIS 14091 where
--   oeis = filter (\x -> any ((== 1) . (oeisIx @10051)) $
--                         map (x -) $ takeWhile (< x) (oeis @40)) [1..]

-- instance OEIS 14092 where
--   oeis = filter (\x ->
--      all ((== 0) . (oeisIx @10051)) $ map (x -) $ takeWhile (< x) (oeis @40)) [1..]

-- instance OEIS 14132 where
--   oeisIx n = n + round (sqrt $ 2 * fromInteger n)
--   oeis = filter ((== 0) . (oeisIx @10054)) [0..]

-- instance OEIS 14237 where
--   oeisIx n = (oeisIx @40) n - (oeisIx @18252) n

-- instance OEIS 14313 where
--   oeisIx = f . (oeisIx @38447) where
--      f x = if x == 0 then 0 else 2 * f x' + b  where (x', b) = divMod x 10

-- instance OEIS 14320 where
--   oeis = nub $ (oeis @1223)

-- instance OEIS 14342 where
--   oeis= f (tail (oeis @40)) [head (oeis @40)] 1 where
--      f (p:ps) qs k = sum (zipWith (*) qs $ reverse qs) :
--                      f ps (p : qs) (k + 1)

-- instance OEIS 14413 where
--   oeis = tablList @14413
-- instance Table 14413 where
--   rowCol = rowCol_off @14413 @1 @1
--   rowT   = rowT_off @14413 @1
--   tabf = [1] : f 1 [1] where
--      f 0 us'@ (_:us) = ys : f 1 ys where
--                       ys = zipWith (+) us' (us ++ [0])
--      f 1 vs@ (v:_) = ys' : f 0 ys where
--                     ys@ (_:ys') = zipWith (+) (vs ++ [0]) ([v] ++ vs)

-- instance OEIS 14417 where
--   oeisIx 0 = 0
--   oeisIx n = foldl (\v z -> v * 10 + z) 0 $ (rowT @189920) n

-- instance OEIS 14430 where
--   oeis = tablList @14430
-- instance Table 14430 where
--   tabl = map (init . tail) $ drop 2 (tabl @14473)

-- instance OEIS 14454 where
--   oeisIx n = sum $ zipWith gcd kfs $ map (div nf) kfs
--      where (nf:kfs) = reverse $ (rowT @166350) n

-- instance OEIS 14462 where
--   oeis = tablList @14462
-- instance Table 14462 where
--   rowCol = rowCol_off @14462 @1 @1
--   rowT   = rowT_off @14462 @1
--   tabf = map reverse (tabf @14413)

-- instance OEIS 14473 where
--   oeis = tablList @14473
-- instance Table 14473 where
--   tabl = map (map (subtract 1)) (tabl @7318)

-- instance OEIS 14480 where
--   oeis = 1 : 6 : map (* 4)
--      (zipWith (-) (tail (oeis @14480)) (oeis @14480))

-- instance OEIS 14481 where
--   oeisIx n = (oeisIx @9445) n `div` (oeisIx @1147) n

-- instance OEIS 14563 where
--   oeis = 1 : f 1 (drop 2 (oeis @290)) where
--      f x (q:qs) | null $ xs \\ (show q) = y : f y qs
--                 | otherwise             = f x qs
--                 where y = (oeisIx @196) q; xs = show (x * x)

-- instance OEIS 14567 where
--   oeis = filter ((== 1) . (oeisIx @9194)) [1..]

-- instance OEIS 14574 where
--   oeis = [x | x <- [2,4..], (oeisIx @10051) (x - 1) == 1, (oeisIx @10051) (x+1) == 1]

-- instance OEIS 14597 where
--   oeis = tail $ elemIndices 1 $ map (oeisIx @197183) [0..]

-- instance OEIS 14657 where
--   oeis = map (+ 1) $ findIndices (> 0) $ map (oeisIx @195470) [1..]

-- instance OEIS 14661 where
--   oeis = 2 : map (+ 1) (elemIndices 0 $ map (oeisIx @195470) [1..])

-- instance OEIS 14682 where
--   oeisIx n = if r > 0 then div (3 * n + 1) 2 else n'
--               where (n', r) = divMod n 2

-- instance OEIS 14683 where
--   oeisIx n = n + (oeisIx @10051)' n

-- instance OEIS 14684 where
--   oeisIx n = n - fi (oeisIx n)

-- instance OEIS 14689 where
--   oeisIx n = (oeisIx @40) n - fi n

-- instance OEIS 14701 where
--   oeisIx 1 = 0
--   oeisIx n = (oeisIx @7953) $ (oeisIx @7931) (n - 1)

-- instance OEIS 14963 where
--   oeisIx 1 = 1
--   oeisIx n | until ((> 0) . (`mod` spf)) (`div` spf) n == 1 = spf
--            | otherwise = 1
--            where spf = (oeisIx @20639) n

-- instance OEIS 15910 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 n n

-- instance OEIS 15976 where
--   oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @56964)) [1..]

-- instance OEIS 16035 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ map (oeisIx @10) $ init $ tail $ (rowT @27750) n

-- instance OEIS 16052 where
--   oeis = iterate (oeisIx @62028) 3

-- instance OEIS 16096 where
--   oeis = iterate (oeisIx @62028) 9

-- instance OEIS 16105 where
--   import Data.Set (singleton, fromList, deleteFindMin, union)
--   oeisIx n = (oeis @16105) !! (n - 1)
--   oeis = f [3,7] (drop 2 (oeis @2145)) 21 (singleton 21) where
--      f qs (p:p':ps) t s
--        | m < t     = m : f qs (p:p':ps) t s'
--        | otherwise = m : f (p:qs) (p':ps) t' (s' `union` (fromList pqs))
--        where (m,s') = deleteFindMin s
--              t' = head $ dropWhile (> 3*p') pqs
--              pqs = map (p *) qs

-- instance OEIS 16726 where
--   oeis = [1,2,6,9] ++ (f 5 $ drop 4 (oeis @1751)) where
--      f n qs'@ (q:qs) | q < 2*n   = f n qs
--                     | otherwise = q : f (n+1) qs'

-- instance OEIS 17665 where
--   oeisIx = numerator . sum . map (1 %) . (rowT @27750)

-- instance OEIS 17666 where
--   oeisIx = denominator . sum . map (1 %) . (rowT @27750)

-- instance OEIS 18194 where
--   oeisIx n = 1 + length (takeWhile (/= 0) $ zipWith (-) ks $ tail ks)
--      where ks = iterate (oeisIx @2034) n

-- instance OEIS 18800 where
--   oeisIx n = read $ fromJust $
--               find (show n `isPrefixOf`) $ map show (oeis @40) :: Int

-- instance OEIS 18818 where
--   oeisIx n = p (init $ (rowT @27750) n) n + 1 where
--      p _      0 = 1
--      p []     _ = 0
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 18819 where
--   oeis = 1 : f (tail (oeis @8619)) where
--      f (x:xs) = (sum $ take x (oeis @18819)) : f xs

-- instance OEIS 18825 where
--   oeis = tail $ elemIndices 0 (oeis @25426)

-- instance OEIS 18856 where
--   oeisIx n =
--      fromJust $ findIndex (show n `isPrefixOf`) $ map show (oeis @79)

-- instance OEIS 18896 where
--   oeis = replicate 8 1 ++ f 8 where
--      f x = ((oeisIx (x - 1) * (oeisIx @18896) (x - 7) + (oeisIx @18896) (x - 4) ^ 2)
--            `div` (oeisIx @18896) (x - 8)) : f (x + 1)

-- instance OEIS 18900 where
--   oeis = elemIndices 2 (oeis @73267)

-- instance OEIS 19268 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @19269))

-- instance OEIS 19269 where
--   oeisIx n = snd $ until ((== 1) . (oeisIx @65333) . fst)
--                           (\ (x, i) -> (oeisIx x, i+1)) (n, 0)

-- instance OEIS 19294 where
--   oeisIx n = snd $ until ((== 0) . (`mod` n) . fst)
--                           (\ (x, i) -> (oeisIx x, i + 1)) (oeisIx n, 1)

-- instance OEIS 19302 where
--   oeisIx = sum . zipWith (*) (oeis @10060) . rowT @7318

-- instance OEIS 19312 where
--   oeisIx = t . enumFromTo 1 where
--      t xs = foldl max 0 [z + t (xs \\ ds) | z <- xs,
--                let ds = (rowT @27750) z, not $ null $ intersect xs $ init ds]

-- instance OEIS 19506 where
--   oeis = [x | x <- (oeis @2808),
--                       oeisIx x == sum (map (oeisIx @7953) (oeisIx_row x))]

-- instance OEIS 19518 where
--   oeis = map read $ scanl1 (++) $ map show (oeis @40) :: [Integer]

-- instance OEIS 19521 where
--   oeis = f "" $ tail (oeis @290) where
--      f xs (q:qs) = (read ys :: Integer) : f ys qs
--        where ys = xs ++ show q

-- instance OEIS 19523 where
--   oeisIx n = read $ concatMap show $ take n $ tail (oeis @45) :: Integer

-- instance OEIS 19538 where
--   oeis = tablList @19538
-- instance Table 19538 where
--   rowCol = rowCol_off @19538 @1 @1
--   rowT   = rowT_off   @19538 @1
--   tabl = iterate f [1] where
--      f xs = zipWith (*) [1..] $ zipWith (+) ([0] ++ xs) (xs ++ [0])

-- instance OEIS 19546 where
--   oeis = filter (all (`elem` "2357") . show )
--                         ([2,3,5] ++ (drop 2 (oeis @3631)))
--   oeis = filter ((== 1) . (oeisIx @10051)) $
--                         [2,3,5,7] ++ h ["3","7"] where
--      h xs = (map read xs') ++ h xs' where
--        xs' = concat $ map (f xs) "2357"
--        f xs d = map (d :) xs

-- instance OEIS 19554 where
--   oeisIx n = product $ zipWith (^)
--               (oeisIx_row n) (map ((`div` 2) . (+ 1)) $ (rowT @124010) n)

-- instance OEIS 19565 where
--   oeisIx n = product $ zipWith (^) (oeis @40) (oeisIx_row n)

-- instance OEIS 19587 where
--   oeisIx n = genericLength $ filter (<= nTau) $
--               map (snd . properFraction . (* tau) . fromInteger) [1..n]
--      where (_, nTau) = properFraction (tau * fromInteger n)
--            tau = (1 + sqrt 5) / 2

-- instance OEIS 19588 where
--   oeisIx n = genericLength $ filter (nTau <=) $
--               map (snd . properFraction . (* tau) . fromInteger) [1..n]
--      where (_, nTau) = properFraction (tau * fromInteger n)
--            tau = (1 + sqrt 5) / 2

-- instance OEIS 20330 where
--   oeisIx n = foldr (\d v -> 2 * v + d) 0 (bs ++ bs) where
--      bs = (rowT @30308) n

-- instance OEIS 20474 where
--   oeis = tablList @20474
-- instance Table 20474 where
--   rowCol = rowCol_off @20474 @2 @2
--   rowT = rowT_off @20474 @2
--   tabl = map fst $ iterate f ([1], [0,1]) where
--      f (us,vs) = (vs, scanl (+) 0 ws) where
--        ws = zipWith (+) (us ++ [0]) vs

-- instance OEIS 20475 where
--   oeis = 0 : map (sum . map (0 ^)) (tail (tabl @53200))

-- instance OEIS 20481 where
--   oeisIx n = head [p | p <- (oeis @40), (oeisIx @10051)' (2 * n - p) == 1]

-- instance OEIS 20482 where
--   oeisIx = last . (rowT @171637)

-- instance OEIS 20483 where
--   oeisIx n = head [p | p <- (oeis @40), (oeisIx @10051)' (p + 2 * n) == 1]

-- instance OEIS 20484 where
--   oeisIx n = head [q | p <- (oeis @40), let q = p + 2*n, (oeisIx @10051)' q == 1]

-- instance OEIS 20486 where
--   oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @5) x == 0) [1..]

-- instance OEIS 20487 where
--   oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @203) x == 0) [1..]

-- instance OEIS 20653 where
--   oeis = concat $ map reverse $ tail (tabf @38566)

-- instance OEIS 20696 where
--   oeisIx = product . map (+ 1) . (rowT @27750)'

-- instance OEIS 20756 where
--   oeis = filter ((> 0) . (oeisIx @52343)) [0..]

-- instance OEIS 20757 where
--   oeis = filter ((== 0) . (oeisIx @52343)) [0..]

-- instance OEIS 20884 where
--   oeis = f 1 1 where
--      f u v | v > uu `div` 2        = f (u + 1) (u + 2)
--            | gcd u v > 1 || w == 0 = f u (v + 2)
--            | otherwise             = u : f u (v + 2)
--            where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

-- instance OEIS 20893 where
--   oeis = filter (\x -> any (== 1) $ map (oeisIx . (x -)) $
--                                takeWhile (<= x) (oeis @290)) (oeis @5117)

-- instance OEIS 20899 where
--   oeis = filter (odd . (oeisIx @7895)) [1..]

-- instance OEIS 22307 where
--   oeisIx n = if n == 0 then 0 else (oeisIx @1221) $ (oeisIx @45) n

-- instance OEIS 22482 where
--   oeis = iterate (oeisIx @45918 . oeisIx @4086) 2

-- instance OEIS 22488 where
--   oeis = 2 : f [2] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22506 where
--   oeis = 0 : 10 : iterate (oeisIx @45918 . (oeisIx @4086)) 1011

-- instance OEIS 22507 where
--   oeis = iterate (oeisIx @45918 . oeisIx @4086) 3

-- instance OEIS 22514 where
--   oeis = 3 : f [3] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22515 where
--   oeis = 4 : f [4] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22516 where
--   oeis = 5 : f [5] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22517 where
--   oeis = 6 : f [6] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22518 where
--   oeis = 7 : f [7] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22519 where
--   oeis = 8 : f [8] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22520 where
--   oeis = 9 : f [9] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f ys where
--             ys = concat $ transpose [map head zss, map length zss]
--             zss = reverse $ group xs

-- instance OEIS 22544 where
--   oeis = elemIndices 0 (oeis @161)

-- instance OEIS 23143 where
--   oeis = 1 : map (+ 1) (elemIndices 1 (oeis @4648))

-- instance OEIS 23172 where
--   oeis =
--      map (+ 1) $ elemIndices 0 $ zipWith mod (tail (oeis @45)) [1..]

-- instance OEIS 23200 where
--   oeis = filter ((== 1) . (oeisIx @10051)') $
--                  map (subtract 4) $ drop 2 (oeis @40)

-- instance OEIS 23208 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 2) . (* 3)) (oeis @40)

-- instance OEIS 23531 where
--   oeis = concat $ iterate ([0,1] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 23610 where
--   oeis = f [1] $ drop 3 (oeis @45) where
--      f us (v:vs) = (sum $ zipWith (*) us $ tail (oeis @45)) : f (v:us) vs

-- instance OEIS 23758 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @23758) !! (n - 1)
--   oeis = 0 : f (singleton 1) where
--   f s = x : f (if even x then insert z s' else insert z $ insert (z+1) s')
--   where z = 2*x; (x, s') = deleteFindMin s

-- instance OEIS 23888 where
--   oeisIx = sum . (rowT @210208)

-- instance OEIS 23890 where
--   oeisIx n = sum $ zipWith (*) divs $ map ((1 -) . (oeisIx @10051)) divs
--               where divs = (rowT @27750) n

-- instance OEIS 23896 where
--   oeisIx = sum . (rowT @38566)

-- instance OEIS 23900 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (1 -) $ (rowT @27748) n

-- instance OEIS 23989 where
--   oeis = 2 : f [2] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f (ys) where
--             ys = concat $ transpose [map length zss, map head zss]
--             zss = group $ sort xs

-- instance OEIS 23998 where
--   oeis = 1 : f 2 [1] (tabl @132813) where
--      f x ys (zs:zss) = y : f (x + 1) (ys ++ [y]) zss where
--                        y = sum $ zipWith (*) ys zs

-- instance OEIS 24166 where
--   oeisIx n = sum $ zipWith (*) [n+1,n..0] (oeis @578)

-- instance OEIS 24316 where
--   oeisIx n = sum $ take (div (n + 1) 2) $ zipWith (*) zs $ reverse zs
--               where zs = take n $ tail (oeis @23531)

-- instance OEIS 24352 where
--   oeis = 3 : drop 4 (oeis @42965)

-- instance OEIS 24359 where
--   oeis = f 0 1 (oeis @20884) where
--      f c u vs'@ (v:vs) | u == v = f (c + 1) u vs
--                       | u /= v = c : f 0 (u + 1) vs'

-- instance OEIS 24409 where
--   oeis = map (+ 1) $ findIndices (> 1) (oeis @24362)

-- instance OEIS 24431 where
--   oeis = 1 : 2 : f [2, 1] [2 ..] where
--      f ks@ (k:_) (j:js) =
--        x : y : f (y : x : ks) ((js \\ map (y -) ks) \\ map (x -) ks)
--        where y = x + j; x = 2 * k + 2

-- instance OEIS 24556 where
--   oeis = filter ((== 0) . (oeisIx @10051)) $ tail (oeis @56911)

-- instance OEIS 24619 where
--   oeis = filter ((== 0) . (oeisIx @10055)) [1..]

-- instance OEIS 24620 where
--   oeis = filter ((== 1) . (oeisIx @25474)) [1..]

-- instance OEIS 24629 where
--   oeisIx 0 = 0
--   oeisIx n = 10 * (oeisIx @24629) (2 * n') + t where (n', t) = divMod n 3

-- instance OEIS 24770 where
--   oeis = filter (\x ->
--      all (== 1) $ map (oeisIx . read) $ tail $ inits $ show x) (oeis @38618)

-- instance OEIS 24785 where
--   oeis = filter (\x ->
--      all (== 1) $ map (oeisIx . read) $ init $ tails $ show x) (oeis @38618)

-- instance OEIS 24816 where
--   oeisIx = sum . (rowT @173541)

-- instance OEIS 24894 where
--   oeisIx = flip div 5 . subtract 1 . (oeisIx @30430)

-- instance OEIS 24939 where
--   oeisIx = p (oeis @65091) where
--      p _  0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 25475 where
--   oeis = filter ((== 0) . (oeisIx @10051)) (oeis @961)

-- instance OEIS 25478 where

-- instance OEIS 25479 where

-- instance OEIS 25480 where
--   interleave xs ys = concat . transpose $ [xs,ys]
--   oeisIx = interleave [0..] (oeisIx @25480)
--   oeisIx n k = (tabf @25480) !! n !! k
--   oeisIx_row n = (tabf @25480) !! n
--   oeisIx_tabf = iterate (\xs -> concat $
--      transpose [xs, [length xs .. 2 * length xs - 1]]) [0]
--   oeis = concat $ (tabf @25480)

-- instance OEIS 25487 where
--   import Data.Set (singleton, fromList, deleteFindMin, union)
--   oeisIx n = (oeis @25487) !! (n - 1)
--   oeis = 1 : h [b] (singleton b) bs where
--      (_ : b : bs) = (oeis @2110)
--      h cs s xs'@ (x:xs)
--        | m <= x    = m : h (m:cs) (s' `union` fromList (map (* m) cs)) xs'
--        | otherwise = x : h (x:cs) (s  `union` fromList (map (* x) (x:cs))) xs
--        where (m, s') = deleteFindMin s

-- instance OEIS 25492 where
--   oeisIx n = fst $ until (uncurry (==)) (\ (x,x') -> (oeisIx x, x)) (n,0)

-- instance OEIS 25530 where
--   oeisIx n = sum $ map (div (oeisIx $ fromInteger n))
--                         (zipWith (*) [1..n] (oeis @33999))

-- instance OEIS 25565 where
--   oeis = 1 : f (oeis @1006) [1] where
--      f (x:xs) ys = y : f xs (y : ys) where
--        y = x + sum (zipWith (*) (oeis @1006) ys)

-- instance OEIS 25583 where
--   oeis = filter f (oeis @2808) where
--      f x = all (== 0) $ map (oeisIx . (x -)) $ takeWhile (< x) (oeis @40)

-- instance OEIS 25586 where
--   oeisIx = last . (rowT @220237)

-- instance OEIS 25616 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @25616) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (3 * y, i + 1, j) $ insert (10 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 25620 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @25620) !! (n - 1)
--   oeis = f $ singleton 1 where
--      f s = y : f (insert (4 * y) $ insert (9 * y) s')
--                  where (y, s') = deleteFindMin s

-- instance OEIS 25632 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @25632) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (7 * y, i + 1, j) $ insert (10 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 25635 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @25635) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (9 * y, i + 1, j) $ insert (10 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 26150 where
--   oeis = 1 : 1 : map (* 2) (zipWith (+) (oeis @26150) (tail
--   oeis))

-- instance OEIS 26233 where
--   oeisIx n = (oeisIx @49084) n + (oeisIx @239968) n

-- instance OEIS 26238 where
--   oeisIx n = (oeisIx @49084) n + (oeisIx @66246) n

-- instance OEIS 26239 where
--   oeisIx 1 = 1
--   oeisIx n | (oeisIx @10051) n == 1 = (oeisIx @2808) $ (oeisIx @49084) n
--             | otherwise      = (oeisIx @40) $ (oeisIx @66246) n

-- instance OEIS 26274 where
--   oeis = map (subtract 1) $ tail $ filter ((== 1) . (oeisIx @35612)) [1..]

-- instance OEIS 26300 where
--   oeis = tablList @26300
-- instance Table 26300 where
--   tabl = iterate (\row -> zipWith (+) ([0,0] ++ row) $
--                                   zipWith (+) ([0] ++ row) (row ++ [0])) [1]

-- instance OEIS 26351 where
--   oeis = findIndices odd (oeis @60142)

-- instance OEIS 26374 where
--   oeis = tablList @26374
-- instance Table 26374 where
--   tabl = [1] : map fst (map snd $ iterate f (1, ([1, 1], [1]))) where
--      f (0, (us, vs)) = (1, (zipWith (+) ([0] ++ us) (us ++ [0]), us))
--      f (1, (us, vs)) = (0, (zipWith (+) ([0] ++ vs ++ [0]) $
--                                zipWith (+) ([0] ++ us) (us ++ [0]), us))

-- instance OEIS 26375 where
--   oeisIx n = (oeisIx @26374) (2 * n) n

-- instance OEIS 26381 where
--   oeisIx = flip (oeisIx @26374) 2

-- instance OEIS 26382 where
--   oeisIx = flip (oeisIx @26374) 3

-- instance OEIS 26424 where
--   oeis = filter (odd . (oeisIx @1222)) [1..]

-- instance OEIS 26465 where
--   oeis = map length $ group (oeis @10060)

-- instance OEIS 26490 where
--   oeis = map length $ group (oeis @26465)

-- instance OEIS 26637 where
--   oeis = tablList @26637
-- instance Table 26637 where
--   tabl = [1] : [1,1] : map (fst . snd)
--      (iterate f (0, ([1,2,1], [0,1,1,0]))) where
--      f (i, (xs, ws)) = (1 - i,
--        if i == 1 then (ys, ws) else (zipWith (+) ys ws, ws'))
--           where ys = zipWith (+) ([0] ++ xs) (xs ++ [0])
--                 ws' = [0,1,0,0] ++ drop 2 ws

-- instance OEIS 26807 where
--   oeis = tablList @26807
-- instance Table 26807 where
--   rowCol = rowCol_off @26807 @1 @1
--   rowT   = rowT_off   @26807 @1
--   tabl = map
--      (\row -> map (p $ last row) $ init $ tails row) (tabl @2260)
--      where p 0  _ = 1
--            p _ [] = 0
--            p m ks'@ (k:ks) = if m < k then 0 else p (m - k) ks' + p m ks

-- instance OEIS 26820 where
--   oeis = tablList @26820
-- instance Table 26820 where
--   rowCol = rowCol_off @26820 @1 @1
--   rowT   = rowT_off   @26820 @1
--   tabl = zipWith
--      (\x -> map (p x) . tail . inits) [1..] $ tail $ inits [1..] where
--      p 0 _ = 1
--      p _ [] = 0
--      p m ks'@ (k:ks) = if m < k then 0 else p (m - k) ks' + p m ks

-- instance OEIS 26832 where
--   oeisIx n = p 1 n where
--      p _ 0 = 1
--      p k m = if m < k then 0 else p (k+1) (m-k) + p (k+1+0^ (n-m)) m

-- instance OEIS 26835 where
--   oeis = tablList @26835
-- instance Table 26835 where
--   rowCol = rowCol_off @26835 @1 @1
--   rowT   = rowT_off   @26835 @1
--   tabl = map
--      (\row -> map (p $ last row) $ init $ tails row) (tabl @2260)
--      where p 0      _ = 1
--            p _     [] = 0
--            p m (k:ks) = if m < k then 0 else p (m - k) ks + p m ks

-- instance OEIS 27023 where
--   oeis = tablList @27023
-- instance Table 27023 where
--   rowCol = rowCol_off @27023 @1 @1
--   rowT   = rowT_off @27023 @1
--   tabf = [1] : iterate f [1, 1, 1] where
--      f row = 1 : 1 : 1 :
--              zipWith3 (((+) .) . (+)) (drop 2 row) (tail row) row ++ [1]

-- instance OEIS 27306 where
--   oeisIx n = (oeisIx @8949) n (n `div` 2)

-- instance OEIS 27383 where
--   oeis = concat $ transpose [oeis, drop 2 (oeis @918)]

-- instance OEIS 27420 where
--   oeis = tablList @27420
-- instance Table 27420 where
--   tabl = zipWith (zipWith z) (tabl @2262) (tabl @25581)
--                  where z u v = length $ nub $ [i * j | i <- zs, j <- zs]
--                                where zs = [min u v .. max u v]

-- instance OEIS 27423 where
--   oeisIx n = f 1 $ map (\p -> iterate (* p) p) (oeis @40) where
--      f y ((pps@ (p:_)):ppss)
--        | p <= n = f (y * (sum (map (div n) $ takeWhile (<= n) pps) + 1)) ppss
--        | otherwise = y

-- instance OEIS 27465 where
--   oeis = tablList @27465
-- instance Table 27465 where
--   tabl = iterate (\row ->
--      zipWith (+) (map (* 3) (row ++ [0])) (map (* 1) ([0] ++ row))) [1]

-- instance OEIS 27642 where
--   oeis = 1 : map (denominator . sum) (zipWith (zipWith (%))
--      (zipWith (map . (*)) (tail (oeis @142)) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 27749 where
--   oeis = tablList @27749
-- instance Table 27749 where
--   rowCol = rowCol_off @27749 @1 @1
--   tabf = [1] : map tail (tail (tabf @27750))

-- instance OEIS 27810 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 5) 5

-- instance OEIS 27818 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 6) 6

-- instance OEIS 27837 where
--   oeis = f 1 [] where
--      f x ys = y : f (x + 1) (y : ys) where
--               y = (oeisIx @1044) x * x - sum (zipWith (*) ys $ tail (oeis @1044))

-- instance OEIS 27861 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @1844)) [0..]

-- instance OEIS 27868 where
--   oeisIx n = sum $ takeWhile (> 0) $ map (n `div`) $ tail (oeis @351)

-- instance OEIS 27914 where
--   oeisIx n = sum $ take (n + 1) $ (rowT @27907) n

-- instance OEIS 27926 where
--   oeis = tablList @27926
-- instance Table 27926 where
--   tabf = iterate (\xs -> zipWith (+)
--                                  ([0] ++ xs ++ [0]) ([1,0] ++ xs)) [1]
--   oeisIx_tabf' = zipWith (++) (tabl @104763) (map tail (tabl @105809))

-- instance OEIS 27941 where
--   oeisIx = (subtract 1) . (oeisIx @45) . (+ 1) . (* 2)

-- instance OEIS 27988 where
--   oeisIx = maximum . (rowT @27926)

-- instance OEIS 27990 where
--   oeis = c [1] (oeis @45) where
--      c us (v:vs) = (sum $ zipWith (*) us vs) : c (v:us) vs

-- instance OEIS 28233 where
--   oeisIx = head . (rowT @141809)

-- instance OEIS 28234 where
--   oeisIx n = n `div` (oeisIx @28233) n

-- instance OEIS 28236 where
--   oeisIx n = sum $ map (div n) $ (rowT @141809) n

-- instance OEIS 28242 where
--   oeisIx n = n' + 1 - m where (n',m) = divMod n 2
--   oeis = concat $ transpose [oeis, (oeis @1477)]

-- instance OEIS 28260 where
--   oeis = filter (even . (oeisIx @1222)) [1..]

-- instance OEIS 28262 where
--   oeis = tablList @28262
-- instance Table 28262 where
--   tabl = [1] : [1,1] : iterate
--      (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,3,1]

-- instance OEIS 28263 where
--   oeis = tablList @28263
-- instance Table 28263 where
--   tabl = zipWith (zipWith (+)) (tabl @7318) (tabl @14410)

-- instance OEIS 28326 where
--   oeis = tablList @28326
-- instance Table 28326 where
--   tabl = iterate
--      (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [2]

-- instance OEIS 28391 where
--   oeisIx n = n - (oeisIx @196) n

-- instance OEIS 28392 where
--   oeisIx n = n + (oeisIx @196) n

-- instance OEIS 28422 where
--   oeisIx 1 = 0
--   oeisIx n = (map (last . init) (tabl @66032)) !! (n - 1)

-- instance OEIS 28834 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @7953)) [1..]

-- instance OEIS 28835 where
--   oeis = findIndices (`elem` [2,3,5,7]) $ map (oeisIx @10888) [0..]

-- instance OEIS 28859 where
--   oeis =
--      1 : 3 : map (* 2) (zipWith (+) (oeis @28859) (tail (oeis @28859)))

-- instance OEIS 28860 where
--   oeis =
--      -1 : 1 : map (* 2) (zipWith (+) (oeis @28860) (tail (oeis @28860)))

-- instance OEIS 28871 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @8865)

-- instance OEIS 28884 where
  -- oeisIx n = (n * (n + 6) + 1) `div` 4

-- instance OEIS 28897 where
--   oeisIx 0 = 0
--   oeisIx n = 2 * (oeisIx @28897) n' + d where (n', d) = divMod n 10

-- instance OEIS 28906 where
--   oeisIx = (oeisIx @4186) . (oeisIx @40)

-- instance OEIS 28916 where
--   oeis = map (oeisIx @40) $ filter ((> 0) . (oeisIx @256852)) [1..]

-- instance OEIS 28982 where
--   import Data.List.Ordered (union)
--   oeis = tail $ union (oeis @290) (oeis @1105)

-- instance OEIS 29549 where
--   oeis = [0,6,210] ++
--      zipWith (+) (oeis @29549)
--                  (map (* 35) $ tail delta)
--      where delta = zipWith (-) (tail (oeis @29549)) (oeis @29549)

-- instance OEIS 29579 where
--   oeisIx n = if m == 0 then n' + 1 else n  where (n', m) = divMod n 2
--   oeis = concat $ transpose [[1 ..], [1, 3 ..]]

-- instance OEIS 29600 where
--   oeis = tablList @29600
-- instance Table 29600 where
--   tabl = [1] : iterate
--      (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [2,3]

-- instance OEIS 29609 where
--   oeisIx n = (oeisIx @29600) (2*n) n

-- instance OEIS 29635 where
--   oeis = tablList @29635
-- instance Table 29635 where
--   tabl = [2] : iterate
--      (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,2]

-- instance OEIS 29653 where
--   oeis = tablList @29653
-- instance Table 29653 where
--   tabl = [1] : iterate
--                  (\xs -> zipWith (+) ([0] ++ xs) (xs ++ [0])) [2, 1]

-- instance OEIS 29730 where
--   oeis = map (foldr (\h v -> 16 * v + h) 0) $
--                      filter (\xs -> xs == reverse xs) (tabf @262437)

-- instance OEIS 29742 where
--   oeis = filter ((== 0) . (oeisIx @136522)) [1..]

-- instance OEIS 29743 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @10784)

-- instance OEIS 29783 where
--   oeis = filter (\x -> (oeisIx @258682) x == x ^ 2) [1..]

-- instance OEIS 29885 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) (1 : map fi (oeis @1285))

-- instance OEIS 29907 where
--   oeis = 0 : 1 : zipWith (+) (tail (oeis @45))
--                         (zipWith (+) (tail (oeis @29907)) (oeis @29907))

-- instance OEIS 29931 where
--   oeisIx = sum . zipWith (*) [1..] . (rowT @30308)

-- instance OEIS 29943 where
--   oeis = filter f [0..] where
--      f x = show x `isInfixOf` show (x^2) && show x `isInfixOf` show (x^3)

-- instance OEIS 30000 where
--   oeisIx n =
--      fromJust $ findIndex (show n `isInfixOf`) $ map show (oeis @79)

-- instance OEIS 30001 where
--   oeisIx n = head $ filter ((show n `isInfixOf`) . show) (oeis @79)

-- instance OEIS 30057 where
--   oeisIx n = head $ filter ((== 0) . p (oeisIx_row n)) [1..] where
--      p _      0 = 1
--      p []     _ = 0
--      p (k:ks) x = if x < k then 0 else p ks (x - k) + p ks x

-- instance OEIS 30078 where
--   oeisIx = (oeisIx @578) . (oeisIx @40)
--   oeis = map (oeisIx @578) (oeis @40)

-- instance OEIS 30079 where
--   oeis = filter f (oeis @40) where
--      f p = pd == pd `intersect` (nub $ show (p^2)) where
--          pd = nub $ show p

-- instance OEIS 30091 where
--   oeis =
--      filter (\x -> ((==) `on` (nub . sort . show)) x (x^2)) (oeis @40)

-- instance OEIS 30147 where
--   oeis = filter ((== 1) . (oeisIx @228710)) (oeis @2113)

-- instance OEIS 30152 where
--   oeis = filter ((== 1) . (oeisIx @228710)) (oeis @290)

-- instance OEIS 30173 where
--   import Data.List.Ordered (union)
--   oeis = union [2, 4 ..] $ tail (oeis @40976)

-- instance OEIS 30190 where
--   oeis = concatMap reverse (tabf @30308)
--
-- instance OEIS 30237 where
--   oeis = tablList @30237
-- instance Table 30237 where
--   tabl = map init $ tail (tabl @9766)

-- instance OEIS 30273 where
--   oeisIx n = p (map (^ 2) [1..]) (n^2) where
--      p _  0 = 1
--      p (k:ks) m | m < k     = 0
--                 | otherwise = p ks (m - k) + p ks m

-- instance OEIS 30284 where
--   oeis = f [] (oeis @40) where
--      f xs (p:ps) = if null $ intersect xs ys then p : f ys ps else f xs ps
--                    where ys = show p

-- instance OEIS 30293 where
--   oeis = filter ((<= 2) . (oeisIx @43537)) (oeis @578)

-- instance OEIS 30298 where
--   oeis = tablList @30298
-- instance Table 30298 where
--   rowCol n k = (tabf @30298) !! (n - 1) (k-1)
--   rowT = concat . sort . permutations . enumFromTo 1
--   tabf = map (rowT @30298) [1..]

-- instance OEIS 30303 where
--   oeis = elemIndices 1 (oeis @30190)

-- instance OEIS 30332 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @3137)

-- instance OEIS 30333 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @3137)

-- instance OEIS 30334 where
--   oeis = map (+ 1) $ elemIndices 2 (oeis @3137)

-- instance OEIS 30336 where
--   oeis = map length $ filter ((== 0) . head) $ group (oeis @3137)

-- instance OEIS 30337 where
--   oeis = map length $ filter ((== 1) . head) $ group (oeis @3137)

-- instance OEIS 30338 where
--   oeis = map length $ filter ((== 2) . head) $ group (oeis @3137)

-- instance OEIS 30339 where
--   oeis = scanl1
--      (\u v -> u + fromEnum (v == 1) - fromEnum (v == 0)) (oeis @3137)
--
-- instance OEIS 30340 where
--   oeis = scanl1
--      (\u v -> u + fromEnum (v == 1) - fromEnum (v == 2)) (oeis @3137)
--
-- instance OEIS 30386 where
--   oeis = tablList @30386
-- instance Table 30386 where
--   tabf = iterate succ [0] where
--      succ []     = [1]
--      succ (3:ts) = 0 : succ ts
--      succ (t:ts) = (t + 1) : ts

-- instance OEIS 30430 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @17281)

-- instance OEIS 30457 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @1704)) [1..]

-- instance OEIS 30461 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @45533)

-- instance OEIS 30514 where
--   oeisIx = (^ 4) . (oeisIx @40)
--   oeis = map (^ 4) (oeis @40)

-- instance OEIS 30516 where
--   oeisIx = (^ 6) . (oeisIx @40)
--   oeis = map (^ 6) (oeis @40)

-- instance OEIS 30658 where
--   oeisIx = fromEnum . (<= 0) . (oeisIx @95916)

-- instance OEIS 30664 where
--   oeisIx n = (oeisIx @7917) n * (oeisIx @7918) n

-- instance OEIS 31076 where
--   oeis = concat $ map reverse $ tail (tabf @31087)

-- instance OEIS 31087 where
--   oeis = tablList @31087
-- instance Table 31087 where
--   rowCol n k = (rowT @31087) n !! (k-1)
--   rowT n | n < 9     = [n]
--                 | otherwise = m : (rowT @31087) n' where (n',m) = divMod n 9
--   tabf = map (rowT @31087) [0..]

-- instance OEIS 31235 where
--   oeis = tablList @31235
-- instance Table 31235 where
--   tabf = iterate succ [0] where
--      succ []     = [1]
--      succ (4:ts) = 0 : succ ts
--      succ (t:ts) = (t + 1) : ts

-- instance OEIS 31359 where
--   oeisIx = (oeisIx @1615) . (subtract 1) . (* 2)

-- instance OEIS 31444 where
--   oeis = filter ((== 1) . (oeisIx @37861)) [1..]

-- instance OEIS 31448 where
--   oeis = filter ((== -1) . (oeisIx @37861)) [1..]

-- instance OEIS 31877 where
--   oeis = [x | x <- [1..], x `mod` 10 > 0,
--                       let x' = (oeisIx @4086) x, x' /= x && x `mod` x' == 0]

-- instance OEIS 31944 where
--   oeis = elemIndices 3 (oeis @212193)

-- instance OEIS 31955 where
--   oeis = filter ((== 2) . (oeisIx @43537)) [0..]

-- instance OEIS 31971 where
--   oeisIx = sum . (rowT @89072)

-- instance OEIS 32358 where
--   oeisIx = genericLength . takeWhile (/= 2) . (iterate (oeisIx @10))

-- instance OEIS 32447 where
--   import Data.List.Ordered (insertBag)
--   oeisIx n = (oeis @32447) !! (n - 1)
--   oeis = f [1..] (oeis @2110) [] where
--      f xs'@ (x:xs) ps'@ (p:ps) us
--        | x < p = f xs ps' $ insertBag (oeisIx' x, x) us
--        | otherwise = map snd vs ++ f xs' ps ws
--        where (vs, ws) = span ((<= (oeisIx @10)' x) . fst) us

-- instance OEIS 32448 where
--   oeisIx n = head [q | q <- (oeis @40), let p = (oeisIx @40) n,
--                         q `mod` p == p - 1]

-- instance OEIS 32741 where
--   oeisIx n = if n == 0 then 0 else (oeisIx @5) n - 1

-- instance OEIS 32760 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) (oeis @290)

-- instance OEIS 32761 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) (oeis @578)

-- instance OEIS 32762 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) [0..]

-- instance OEIS 32763 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) [0, 2 ..]

-- instance OEIS 32764 where
--   oeis = 1 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) [1, 3 ..]

-- instance OEIS 32810 where
--   oeisIx = f 0 . (+ 1) where
--      f y 1 = (oeisIx @4086) y
--      f y x = f (10 * y + m + 2) x' where (x', m) = divMod x 2

-- instance OEIS 32981 where
--   oeis = map read $ filter f $ map show [1..] :: [Int] where
--      f ps = all (`elem` neighbours) $ zipWith ((. return) . (:)) ps (tail ps)
--      neighbours = "09" : "90" : zipWith ((. return) . (:))
--         (digs ++ tail digs ++ init digs) (digs ++ init digs ++ tail digs)
--      digs = "0123456789"

-- instance OEIS 33075 where
--   -- import Data.Set (fromList, deleteFindMin, insert)
--   oeisIx n = (oeis @33075) !! (n - 1)
--   oeis = f (fromList [1..9]) where
--      f s | d == 0    = m : f (insert (10*m+1) s')
--          | d == 9    = m : f (insert (10*m+8) s')
--          | otherwise = m : f (insert (10*m+d-1) (insert (10*m+d+1) s'))
--          where (m,s') = deleteFindMin s
--                d = mod m 10

-- instance OEIS 33180 where
--   oeis = filter ((> 0) . (oeisIx @67109)) [1..]

-- instance OEIS 33184 where
--   oeis = tablList @33184
-- instance Table 33184 where
--   rowCol = rowCol_off @33184 @1 @1
--   rowT   = rowT_off   @33184 @1
--   tabl = map reverse (tabl @9766)

-- instance OEIS 33200 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @47471)

-- instance OEIS 33203 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @47476)

-- instance OEIS 33274 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 0 (oeis @79066)

-- instance OEIS 33286 where
--   oeisIx n = (oeisIx @40) n * n

-- instance OEIS 33291 where
--   oeis = tablList @33291
-- instance Table 33291 where
--   rowCol = rowCol_off @33291 @1 @1
--   rowT   = rowT_off   @33291 @1
--   tabl = f 1 [1..] where
--      f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
--        ys  = take k $ filter ((== 0) . (`mod` k)) xs
--   oeisIx n = head $ (tabl @33291) !! (n - 1)
--   oeisIx n = last $ (tabl @33291) !! (n - 1)

-- instance OEIS 33292 where
--   oeis = tablList @33292
-- instance Table 33292 where
--   rowCol = rowCol_off @33292 @1 @1
--   rowT   = rowT_off   @33292 @1
--   tabl = f 1 [1..] where
--      f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
--        ys  = take k $ filter ((== 0) . (`mod` 3) . (subtract k)) xs

-- instance OEIS 33293 where
--   oeis = tablList @33293
-- instance Table 33293 where
--   rowCol = rowCol_off @33293 @1 @1
--   rowT   = rowT_off   @33293 @1
--   tabl = f 1 [1..] where
--      f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
--        ys  = take k $ filter ((== 0) . (`mod` 8) . (subtract k)) xs

-- instance OEIS 33294 where
--   oeis = filter chi (oeis @290) where
--     chi m = m `mod` 10 > 0 && head ds `elem` [1,4,5,6,9] &&
--             (oeisIx @10052) (foldl (\v d -> 10 * v + d) 0 ds) == 1 where
--       ds = unfoldr
--            (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 10) m

-- instance OEIS 33308 where
--   oeis = concatMap (map (read . return) . show) (oeis @40) :: [Int]

-- instance OEIS 33428 where
--   oeisIx = (* 3) . (^ 2)
--   oeis = 0 : 3 : 12 : zipWith (+) (oeis @33428)
--      (map (* 3) $ tail $ zipWith (-) (tail (oeis @33428)) (oeis @33428))

-- instance OEIS 33484 where
--   oeisIx = (subtract 2) . (* 3) . (2 ^)
--   oeis = iterate ((subtract 2) . (* 2) . (+ 2)) 1

-- instance OEIS 33485 where
--   oeis = 1 : zipWith (+)
--      (oeis @33485) (concat $ transpose [oeis, (oeis @33485)])

-- instance OEIS 33491 where
--   oeisIx = head . (rowT @127824)

-- instance OEIS 33538 where
--   oeis =
--      1 : 1 : (map (+ 1) $ zipWith (+) (oeis @33538)
--                                       $ map (3 *) $ tail (oeis @33538))

-- instance OEIS 33539 where
--   oeis =
--      1 : 1 : 1 : (map (+ 1) $ zipWith (+) (tail (oeis @33539))
--                                           (map (2 *) $ drop 2 (oeis @33539)))

-- instance OEIS 33548 where
--   oeis = filter ((== 0) . (oeisIx @90431) . (oeisIx @49084)) (oeis @40)

-- instance OEIS 33549 where
--   oeis = filter ((== 0) . (oeisIx @90431)) [1..]

-- instance OEIS 33556 where
--   oeis = iterate (\x -> 2*x - (oeisIx @151799 x)) 3


-- instance OEIS 33619 where
--   import Data.Set (fromList, deleteFindMin, insert)
--   oeisIx n = (oeis @33619) !! (n - 1)
--   oeis = [0..9] ++ (f $ fromList [10..99]) where
--      f s = m : f (insert (m * 10 + h) s') where
--        h = div (mod m 100) 10
--        (m,s') = deleteFindMin s

-- instance OEIS 33620 where
--   oeis = filter chi [1..] where
--      chi n = (oeisIx @136522) spf == 1 && (n' == 1 || chi n') where
--         n' = n `div` spf
--         spf = (oeisIx @20639) n

-- instance OEIS 33630 where
--   oeisIx 0 = 1
--   oeisIx n = p (oeisIx_row n) n where
--      p _  0 = 1
--      p [] _ = 0
--      p (d:ds) m = if d > m then 0 else p ds (m - d) + p ds m

-- instance OEIS 33632 where
--   oeis = filter (\x -> (oeisIx @62401) x == (oeisIx @62402) x) [1..]

-- instance OEIS 33651 where
  -- oeisIx n = (oeis @63051) !! n
  -- oeis = iterate (oeisIx @56964) 879

-- instance OEIS 33664 where
--   oeis = filter (all ((== 1) . (oeisIx @10051). read) .
--                              init . tail . tails . show) (oeis @40)

-- instance OEIS 33676 where
--   oeisIx n = last $ takeWhile (<= (oeisIx @196) n) $ (rowT @27750) n

-- instance OEIS 33677 where
--   oeisIx n = head $
--      dropWhile ((< n) . (^ 2)) [d | d <- [1..n], mod n d == 0]

-- instance OEIS 33683 where
--   oeisIx n = fromEnum $ odd n && mod n 3 > 0 && (oeisIx @10052) n == 1

-- instance OEIS 33815 where
--   oeisIx n = (oeisIx @116854) (2 * n + 1) (n + 1)

-- instance OEIS 33845 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @33845) !! (n - 1)
--   oeis = f (singleton (2*3)) where
--      f s = m : f (insert (2*m) $ insert (3*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 33846 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @33846) !! (n - 1)
--   oeis = f (singleton (2*5)) where
--      f s = m : f (insert (2*m) $ insert (5*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 33847 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @33847) !! (n - 1)
--   oeis = f (singleton (2*7)) where
--      f s = m : f (insert (2*m) $ insert (7*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 33848 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @33848) !! (n - 1)
--   oeis = f (singleton (2*11)) where
--      f s = m : f (insert (2*m) $ insert (11*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 33849 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @33849) !! (n - 1)
--   oeis = f (singleton (3*5)) where
--      f s = m : f (insert (3*m) $ insert (5*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 33850 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @33850) !! (n - 1)
--   oeis = f (singleton (3*7)) where
--      f s = m : f (insert (3*m) $ insert (7*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 33851 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @33851) !! (n - 1)
--   oeis = f (singleton (5*7)) where
--      f s = m : f (insert (5*m) $ insert (7*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 33876 where
--   oeisIx n = sum $ zipWith (!!) zss [0..n] where
--      zss = take (n+1) $ g (take (n+1) (1 : [0,0..])) where
--          g us = (take (n+1) $ g' us) : g (0 : init us)
--          g' vs = last $ take (2 * n + 3) $
--                         map snd $ iterate h (0, vs ++ reverse vs)
--      h (p,ws) = (1 - p, drop p $ zipWith (+) ([0] ++ ws) (ws ++ [0]))

-- instance OEIS 33950 where
--   oeis = [x | x <- [1..], x `mod` (oeisIx @5) x == 0]

-- instance OEIS 33958 where

-- instance OEIS 33959 where
--   (oeis, (oeis @33958)) = unzip $ (0, 1) : f 1 1 where
--      f i x | y > x     = (y, 2 * i - 1) : f (i + 1) y
--            | otherwise = f (i + 1) x
--            where y = (oeisIx @75680) i

-- instance OEIS 33989 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @33307) $ (oeisIx @185950) n

-- instance OEIS 34017 where
--   oeis = 0 : filter ((> 0) . (oeisIx @86)) [1..]

-- instance OEIS 34020 where
--   oeis = f [0..] (oeis @3136) where
--      f (x:xs) ys'@ (y:ys) | x < y = x : f xs ys'
--                          | otherwise = f xs ys

-- instance OEIS 34182 where
--   oeis = 1 : 5 : (map (+ 4) $
--      zipWith (+) (oeis @34182) (map (* 2) $ tail (oeis @34182)))

-- instance OEIS 34262 where
--   oeisIx n = (oeisIx @578) n + n

-- instance OEIS 34302 where
--   oeis = filter f $ drop 4 (oeis @38618) where
--      f x = all (== 1) $ map (oeisIx . read) $
--                zipWith (++) (inits $ show x) (tail $ tails $ show x)

-- instance OEIS 34303 where
--   oeis = filter f $ drop 4 (oeis @38618) where
--      f x = all (== 0) $ map (oeisIx . read) $
--                zipWith (++) (inits $ show x) (tail $ tails $ show x)

-- instance OEIS 34304 where
--   oeis = map read $ filter (f "") $
--                  map show $ dropWhile (< 10) (oeis @259315) :: [Integer] where
--      f _ "" = True
--      f us (v:vs) = (oeisIx @10051)' (read (us ++ vs)) == 1 && f (us ++ [v]) vs

-- instance OEIS 34305 where
--   oeis = filter f $ drop 9 (oeis @52382) where
--     f x = (oeisIx @10051)' x == 0 &&
--           (all (== 0) $ map (oeisIx' . read) $
--            zipWith (++) (inits $ show x) (tail $ tails $ show x))

-- instance OEIS 34380 where
--   oeisIx n = (oeisIx @10) n `div` (oeisIx @2322) n

-- instance OEIS 34404 where
--   oeisIx = (oeisIx @292) . (oeisIx @2311)

-- instance OEIS 34444 where
--   oeisIx = genericLength . (rowT @77610)

-- instance OEIS 34460 where
--   oeisIx = sum . init . (rowT @77610)

-- instance OEIS 34676 where
--   oeisIx = sum . map (^ 2) . (rowT @77610)

-- instance OEIS 34684 where
--   oeisIx = minimum . (rowT @141809)

-- instance OEIS 34693 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10051) (k * n + 1) == 1]

-- instance OEIS 34694 where
--   oeisIx n = until ((== 1) . (oeisIx @10051)) (+ n) (n + 1)

-- instance OEIS 34698 where
--   oeis = f [2..] [1] where
--      f (x:xs) ys | (oeisIx @10051)' x == 1 &&
--                    (and $ map (isSquMod x) ys) = x : f xs (x:ys)
--                  | otherwise                   = f xs ys
--      isSquMod u v = v `mod` u `elem` (map ((`mod` u) . (^ 2)) [0..u-1])

-- instance OEIS 34699 where
--   oeisIx = last . (rowT @210208)

-- instance OEIS 34700 where
--   oeis = f [1,5..] [1] where
--      f (x:xs) ys | (oeisIx @10051)' x == 1 &&
--                    (and $ map (isSquMod x) ys) = x : f xs (x:ys)
--                  | otherwise                   = f xs ys
--      isSquMod u v = v `mod` u `elem` (map ((`mod` u) . (^ 2)) [0..u-1])

-- instance OEIS 34705 where
--   import Data.Set (deleteFindMin, union, fromList); import Data.List (inits)
--   oeisIx n = (oeis @34705) !! (n - 1)
--   oeis = f 0 (tail $ inits $ (oeis @290)) (fromList [0]) where
--      f x vss'@ (vs:vss) s
--        | y < x = y : f x vss' s'
--        | otherwise = f w vss (union s $ fromList $ scanl1 (+) ws)
--        where ws@ (w:_) = reverse vs
--              (y, s') = deleteFindMin s

-- instance OEIS 34706 where
--   -- import Data.Set (deleteFindMin, union, fromList); import Data.List (inits)
--   oeisIx n = (oeis @34706) !! (n - 1)
--   oeis = f 0 (tail $ inits $ (oeis @217)) (fromList [0]) where
--      f x vss'@ (vs:vss) s
--        | y < x = y : f x vss' s'
--        | otherwise = f w vss (union s $ fromList $ scanl1 (+) ws)
--        where ws@ (w:_) = reverse vs
--              (y, s') = deleteFindMin s

-- instance OEIS 34708 where
--   oeis = filter ((== 1) . (oeisIx @168046)) (oeis @214957)

-- instance OEIS 34709 where
--   oeis =
--      filter (\i -> i `mod` 10 > 0 && i `mod` (i `mod` 10) == 0) [1..]

-- instance OEIS 34710 where
--   oeis = map fi $ elemIndices 0 $ map (\x -> (oeisIx @7953) x - (oeisIx @7954) x) [1..]

-- instance OEIS 34757 where
--   oeisIx = (subtract 1) . (* 2) . (oeisIx @5282)

-- instance OEIS 34785 where
--   oeisIx = (2 ^) . (oeisIx @40)

-- instance OEIS 34794 where
--   oeis = 2 : f 2 (tail  (oeis @40)) where
--      f x (p:ps) = if elem x $ (rowT @46071) p then p : f p ps else f x ps

-- instance OEIS 34837 where
--   oeis = filter (\i -> i `mod` (oeisIx @30 i) == 0) [1..]

-- instance OEIS 34838 where
--   oeis = filter f (oeis @52382) where
--      f u = g u where
--        g v = v == 0 || mod u d == 0 && g v' where (v',d) = divMod v 10

-- instance OEIS 34844 where
--   oeis = filter (not . any  (`elem` "2357") . show ) (oeis @40)

-- instance OEIS 34851 where
--   oeis = tablList @34851
-- instance Table 34851 where
--   rowCol n k = (rowT @34851) n !! k
--   rowT 0 = [1]
--   rowT 1 = [1,1]
--   oeisIx_row n = zipWith (-) (zipWith (+) ([0] ++ losa) (losa ++ [0]))
--                               ([0] ++ (rowT @204293) (n - 2) ++ [0])
--      where losa = (rowT @34851) (n - 1)
--   tabl = map (rowT @34851) [0..]

-- instance OEIS 34852 where
--   oeis = tablList @34852
-- instance Table 34852 where
--   tabl = zipWith (zipWith (-)) (tabl @7318) (tabl @34851)

-- instance OEIS 34870 where
--   oeis = tablList @34870
-- instance Table 34870 where
--   tabf = map (rowT @7318) [0, 2 ..]

-- instance OEIS 34871 where
--   oeis = concat $ map ([1,1] ^) [1,3..]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 34874 where
--   oeis = 1 : f 2 1 where
--      f x y = z : f (x + 1) z where z = (x * (oeisIx @4086) y)

-- instance OEIS 34877 where
--   oeis = tablList @34877
-- instance Table 34877 where
--   tabl = map (init . tail) $ drop 2 (tabl @34852)

-- instance OEIS 34883 where
--   oeisIx = maximum . (rowT @51010)

-- instance OEIS 34886 where
--   oeisIx = (oeisIx @55642) . (oeisIx @142)

-- instance OEIS 34891 where
--   oeisIx = genericLength . (rowT @212721)

-- instance OEIS 34928 where
--   oeis = tablList @34928
-- instance Table 34928 where
--   tabf = iterate f [1,1] where
--      f us = vs ++ [last vs] where
--             vs = zipWith (+) us (0 : scanl (+) 0 us)

-- instance OEIS 34930 where
--   oeis = tablList @34930
-- instance Table 34930 where
--   tabl = iterate
--      (\ws -> zipWith (\u v -> mod (u + v) 8) ([0] ++ ws) (ws ++ [0])) [1]

-- instance OEIS 34931 where
--   oeis = tablList @34931
-- instance Table 34931 where
--   tabl = iterate
--      (\ws -> zipWith ((flip mod 4 .) . (+)) ([0] ++ ws) (ws ++ [0])) [1]

-- instance OEIS 34932 where
--   oeis = tablList @34932
-- instance Table 34932 where
--   tabl = iterate
--      (\ws -> zipWith ((flip mod 16 .) . (+)) ([0] ++ ws) (ws ++ [0])) [1]

-- instance OEIS 34970 where
--   oeis = 2 : 3 : (map (oeisIx . (subtract 1)) $
--                           zipWith (*) (oeis @34970) $ tail (oeis @34970))

-- instance OEIS 35026 where
--   oeisIx n = sum $ map (oeisIx @10051 . (2 * n -)) $
--      takeWhile (< 2 * n) (oeis @40)

-- instance OEIS 35103 where
--   oeisIx = (oeisIx @23416) . (oeisIx @40)

-- instance OEIS 35106 where
--   import Data.List.Ordered (union)
--   oeisIx n = (oeis @35106) !! (n - 1)
--   oeis = 1 : tail (union (oeis @2378) (oeis @5563))

-- instance OEIS 35116 where
--   oeisIx = (^ 2) . (oeisIx @5)'

-- instance OEIS 35137 where
--   oeis = filter ((== 0) . (oeisIx @260254)) [0..]

-- instance OEIS 35191 where
--   oeisIx n = (oeisIx @1817) n + (oeisIx @1822) n

-- instance OEIS 35250 where
--   oeisIx n = sum $ map (oeisIx @10051) [n..2*n]

-- instance OEIS 35302 where
--   oeis = 0 : 1 : (-2) :
--      zipWith (+) (drop 2 $ map (* 2) (oeis @35302))
--                  (map (* 4) $ zipWith (-) (oeis @35302) $ tail (oeis @35302))

-- instance OEIS 35306 where
--   oeis = tablList @35306
-- instance Table 35306 where
--   rowCol n k = (rowT @35306) n !! (k-1)
--   rowT 1 = [1,1]
--   rowT n = concat $ transpose [oeisIx_row n, (rowT @124010) n]
--   tabf = map (rowT @35306) [1..]

-- instance OEIS 35316 where
--   oeisIx n = product $
--      zipWith (\p e -> (p ^ (e + 2 - mod e 2) - 1) `div` (p ^ 2 - 1))
--              (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 35317 where
--   oeis = tablList @35317
-- instance Table 35317 where
--   tabl = map snd $ iterate f (0, [1]) where
--      f (i, row) = (1 - i, zipWith (+) ([0] ++ row) (row ++ [i]))

-- instance OEIS 35324 where
--   oeis = tablList @35324
-- instance Table 35324 where
--   rowCol = rowCol_off @35324 @1 @1
--   rowT   = rowT_off   @35324 @1
--   tabl = map snd $ iterate f (1, [1]) where
--      f (i, xs)  = (i + 1, map (`div` (i + 1)) $
--         zipWith (+) ((map (* 2) $ zipWith (*) [2 * i + 1 ..] xs) ++ [0])
--                     ([0] ++ zipWith (*) [2 ..] xs))

-- instance OEIS 35327 where
--   oeisIx n = if n <= 1 then 1 - n else 2 * (oeisIx @35327) n' + 1 - b
--               where (n',b) = divMod n 2

-- instance OEIS 35336 where
--   oeis = elemIndices 0 (oeis @5713)

-- instance OEIS 35342 where
--   oeis = tablList @35342
-- instance Table 35342 where
--   rowCol = rowCol_off @35342 @1 @1
--   rowT   = rowT_off   @35342 @1
--   tabl = map fst $ iterate (\ (xs, i) -> (zipWith (+)
--      ([0] ++ xs) $ zipWith (*) [i..] (xs ++ [0]), i + 2)) ([1], 3)

-- instance OEIS 35514 where
--   oeis = map (read . concatMap show) (tabf @35516) :: [Integer]

-- instance OEIS 35515 where
--   oeis = map (read . concatMap show) (tabf @35517) :: [Integer]

-- instance OEIS 35517 where
--   oeis = tablList @35517
-- instance Table 35517 where
--   tabf = map reverse (tabf @35516)

-- instance OEIS 35526 where
--   oeisIx = (oeisIx @7088) . (oeisIx @35522)

-- instance OEIS 35532 where
--   oeisIx 1 = 1
--   oeisIx n = if (oeisIx @10051)' n == 0 then phi2 else phi2 - (oeisIx @120) n + 1
--               where phi2 = 2 * (oeisIx @10) n

-- instance OEIS 35607 where
--   oeis = tablList @35607
-- instance Table 35607 where
--   tabl = map fst $ iterate
--      (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
--                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 2])

-- instance OEIS 35612 where
--   oeisIx = (oeisIx @7814) . (oeisIx @22340)

-- instance OEIS 35614 where
--   oeisIx = (oeisIx @122840) . (oeisIx @14417) . (+ 1)

-- instance OEIS 35928 where
--   oeis = filter (\x -> (oeisIx @36044) x == x) [0,2..]

-- instance OEIS 35930 where
--   oeisIx n | n < 10    = 0
--            | otherwise = maximum $ zipWith (*)
--               (map read $ init $ tail $ inits $ show $ fi n)
--               (map read $ tail $ init $ tails $ show $ fi n)

-- instance OEIS 35959 where
--   oeisIx = p (oeis @47201) where
--      p _      0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 36044 where
--   oeisIx 0 = 1
--   oeisIx n = foldl (\v d -> 2 * v + d) 0 (unfoldr bc n) where
--      bc 0 = Nothing
--      bc x = Just (1 - m, x') where (x',m) = divMod x 2

-- instance OEIS 36059 where
--   oeis = map (read . concatMap show) fss :: [Integer] where
--      fss = [1] : [1] : zipWith h (tail fss) fss where
--            h vs ws = concatMap (\us -> [length us, head us]) $
--                      group $ reverse $ sort $ vs ++ ws

-- instance OEIS 36103 where
--   oeis = 0 : 1 : map (read . concatMap say . group . reverse . sort)
--                  (zipWith ((++) `on` show) (oeis @36103) $ tail (oeis @36103))
--                  where say w = (show $ length w) ++ [head w]

-- instance OEIS 36106 where
--   oeis = 1 : 2 : map (read . concatMap say . reverse . group . sort)
--                  (zipWith ((++) `on` show) (oeis @36106) $ tail (oeis @36106))
--                  where say ws = (show $ length ws) ++ [head ws]

-- instance OEIS 36241 where
--   import qualified Data.Set as Set (null, map)
--   import Data.Set (empty, fromList, toList, intersect, union)
--   oeisIx n = (oeis @36241) !! (n - 1)
--   oeis = f [1..] [] empty empty where
--      f (x:xs) ys s2 s3
--       | null (s2' `intersect` y2s) && null (s3' `intersect` y3s)
--         = x : f xs (x:ys) (fromList s2' `union` s2) (fromList s3' `union` s3)
--       | otherwise = f xs ys s2 s3
--       where s2' = sort $ map (x +) ys
--             s3' = sort $ map (x +) y2s
--             y2s = toList s2
--             y3s = toList s3

-- instance OEIS 36262 where
--   oeis = tablList @36262
-- instance Table 36262 where
--   rowCol n k = delta !! (n - k) !! (k - 1) where delta = iterate
--      (\pds -> zipWith (\x y -> abs (x - y)) (tail pds) pds) (oeis @40)

-- instance OEIS 36288 where
--   oeisIx n = 1 + sum (zipWith (*)
--               (oeisIx_row n) (map fi $ (rowT @124010) n))

-- instance OEIS 36299 where
--   oeis = map read rabbits :: [Integer] where
--      rabbits = "1" : "10" : zipWith (++) (tail rabbits) rabbits

-- instance OEIS 36355 where
--   oeis = tablList @36355
-- instance Table 36355 where
--   tabl = [1] : f [1] [1,1] where
--      f us vs = vs : f vs (zipWith (+)
--                          (zipWith (+) ([0,0] ++ us) (us ++ [0,0]))
--                          (zipWith (+) ([0] ++ vs) (vs ++ [0])))

-- instance OEIS 36391 where
--   oeisIx = sum . (rowT @139366)

-- instance OEIS 36433 where
--   oeis = filter f [1..] where
--      f x = d < 10 && ("0123456789" !! d) `elem` show x where d = (oeisIx @5) x

-- instance OEIS 36441 where
--   oeis = tail (oeis @76271)

-- instance OEIS 36449 where
--   oeis = map fst listsOfValsAndDiffs
--   oeisIx n = (oeis @189475) !! (n - 1)
--   oeis = tail $ map snd listsOfValsAndDiffs
--   listsOfValsAndDiffs = (0,1) : f (0,1) where
--      f (x,y) = (u,v) : f (u,v) where
--        u = x + v
--        v = head $ dropWhile ((== 0) . (oeisIx @10052) . (+ x)) $ tail (oeis @217)

-- instance OEIS 36454 where
--   oeisIx n = (oeis @9087) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

-- instance OEIS 36490 where
--   import Data.Set (Set, fromList, insert, deleteFindMin)
--   oeisIx n = (oeis @36490) !! (n - 1)
--   oeis = f $ fromList [5,7,11] where
--      f s = m : (f $ insert (5 * m) $ insert (7 * m) $ insert (11 * m) s')
--            where (m, s') = deleteFindMin s

-- instance OEIS 36491 where
--   oeisIx n = f z z where
--      f x y | x `mod` 2401 == 0 = f (x `div` 49) (y `div` 7)
--            | x `mod` 343 == 0  = y `div` 7
--            | otherwise         = y
--      z = (oeisIx @36490) n

-- instance OEIS 36537 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @5)) [1..]

-- instance OEIS 36554 where
--   oeisIx = (+ 1) . (oeisIx @79523)

-- instance OEIS 36561 where
--   oeis = tablList @36561
-- instance Table 36561 where
--   tabf = iterate (\xs@ (x:_) -> x * 2 : map (* 3) xs) [1]

-- instance OEIS 36585 where
--   oeis = 3 : concat (map f (oeis @36585))
--   where f 1 = [1,2,3]; f 2 = [1,3]; f 3 = [2]

-- instance OEIS 36667 where
--   oeis = filter (even . flip mod 2 . (oeisIx @1222)) (oeis @3586)

-- instance OEIS 36692 where
--   oeisIx n = (oeisIx @36355) (2 * n) n

-- instance OEIS 36746 where
--   oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @61493))
--      (map (read . reverse) $ tail $ inits $ reverse $ show $ (oeisIx @61493) 3888)

-- instance OEIS 36763 where
--   oeis = filter ((== 0) . (oeisIx @51521)) [1..]

-- instance OEIS 36786 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x < (oeisIx @55642) x]

-- instance OEIS 36787 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x == (oeisIx @55642) x]

-- instance OEIS 36788 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x <= (oeisIx @55642) x]

-- instance OEIS 36829 where
--   oeisIx n = sum $ map
--      (\k -> (oeisIx (3*k) k) * (oeisIx (3*n - 3*k-2) (n-k-1))) [0..n-1]

-- instance OEIS 36844 where
--   oeis = filter ((== 0). (oeisIx @238525)) [2..]

-- instance OEIS 36916 where
--   oeisIx n = sum $ map
--      (\k -> (oeisIx (2*n - 2*k) (n - k))^2 * (oeisIx @7318 n k)^2) [0..n]

-- instance OEIS 36917 where
--   oeisIx n = sum $ map
--      (\k -> (oeisIx (2*n - 2*k) (n - k))^2 * (oeisIx @7318 (2*k) k)^2) [0..n]

-- instance OEIS 36966 where
--   import Data.Set (singleton, deleteFindMin, fromList, union)
--   oeisIx n = (oeis @36966) !! (n - 1)
--   oeis = 1 : f (singleton z) [1, z] zs where
--      f s q3s p3s'@ (p3:p3s)
--        | m < p3 = m : f (union (fromList $ map (* m) ps) s') q3s p3s'
--        | otherwise = f (union (fromList $ map (* p3) q3s) s) (p3:q3s) p3s
--        where ps = (rowT @27748) m
--              (m, s') = deleteFindMin s
--      (z:zs) = (oeis @30078)

-- instance OEIS 36967 where
--   import Data.Set (singleton, deleteFindMin, fromList, union)
--   oeisIx n = (oeis @36967) !! (n - 1)
--   oeis = 1 : f (singleton z) [1, z] zs where
--      f s q4s p4s'@ (p4:p4s)
--        | m < p4 = m : f (union (fromList $ map (* m) ps) s') q4s p4s'
--        | otherwise = f (union (fromList $ map (* p4) q4s) s) (p4:q4s) p4s
--        where ps = (rowT @27748) m
--              (m, s') = deleteFindMin s
--      (z:zs) = (oeis @30514)

-- instance OEIS 36969 where
--   oeis = tablList @36969
-- instance Table 36969 where
--   rowCol = rowCol_off @36969 @1 @1
--   rowT   = rowT_off   @36969 @1
--   tabl = iterate f [1] where
--      f row = zipWith (+)
--        ([0] ++ row) (zipWith (*) (tail (oeis @290)) (row ++ [0]))

-- instance OEIS 36998 where
--   oeisIx n = p (rowT @38566 n) n where
--      p _      0 = 1
--      p []     _ = 0
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 37019 where
--   oeisIx = product .
--      zipWith (^) (oeis @40) . reverse . map (subtract 1) . (rowT @27746)

-- instance OEIS 37020 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @1065)) [1..]

-- instance OEIS 37027 where
--   oeis = tablList @37027
-- instance Table 37027 where
--   tabl = [1] : [1,1] : f [1] [1,1] where
--      f xs ys = ys' : f ys ys' where
--        ys' = zipWith3 (\u v w -> u + v + w) (ys ++ [0]) (xs ++ [0,0]) ([0] ++ ys)

-- instance OEIS 37074 where
--   oeisIx = subtract 1 . (oeisIx @75369)

-- instance OEIS 37126 where
--   oeis = tablList @37126
-- instance Table 37126 where
--   rowCol = rowCol_off @37126 @1 @1
--   rowT   = rowT_off   @37126 @1
--   tabl = map (`take` (oeis @40)) [1..]

-- instance OEIS 37143 where
--   oeis = 1 : merge (oeis @40) (oeis @1358) where
--      merge xs'@ (x:xs) ys'@ (y:ys) =
--            if x < y then x : merge xs ys' else y : merge xs' ys

-- instance OEIS 37254 where
--   oeis = tablList @37254
-- instance Table 37254 where
--   rowCol = rowCol_off @37254 @1 @1
--   rowT   = rowT_off   @37254 @1
--   tabl = map fst $ iterate f ([1], drop 2 (oeis @2083)) where
--      f (row, (x:xs)) = (map (+ x) (0 : row), xs)

-- instance OEIS 37264 where
--   oeis = filter ((== 1) . (oeisIx @168046)) $
--                         takeWhile (<= 999999999) (oeis @214958)

-- instance OEIS 37268 where
--   oeis = filter ((== 1) . (oeisIx @168046)) $
--                         takeWhile (<= 999999999) (oeis @214959)

-- instance OEIS 37271 where
--   oeisIx = genericLength . takeWhile ((== 0) . (oeisIx @10051)'') .
--                                iterate (oeisIx @37276) . (oeisIx @2808)

-- instance OEIS 37273 where
--   oeisIx 1 = -1
--   oeisIx n = genericLength $ takeWhile ((== 0) . (oeisIx @10051)) $
--      iterate (\x -> read $ concatMap show $ (rowT @27746) x :: Integer) n

-- instance OEIS 37276 where
--   oeisIx = read . concatMap show . (rowT @27746)

-- instance OEIS 37277 where
--   oeisIx 1 = 0
--   oeisIx n = read $ concat $ map show $ tail $ (rowT @27750) n

-- instance OEIS 37278 where
--   oeisIx = read . concatMap show . (rowT @27750) :: Integer -> Integer

-- instance OEIS 37283 where
--   oeisIx = read . concat . (map show) . (rowT @182469) :: Integer -> Integer

-- instance OEIS 37284 where
--   oeisIx n
--      | (oeisIx @209229) n == 1 = 0
--      | otherwise      = read $ concat $ (map show) $ tail $ (rowT @182469) n

-- instance OEIS 37285 where
--   oeisIx n
--   | (oeisIx @209229) n == 1 = 0
--   | (oeisIx @10051) n == 1 = 0
--   | otherwise = read $ concat $ (map show) $ delete n $ tail $ (rowT @182469) n

-- instance OEIS 37306 where
--   oeis = tablList @37306
-- instance Table 37306 where
--   rowCol n k = div (sum $ map f $ (rowT @27750) $ gcd n k) n where
--      f d = (oeisIx @10) d * (oeisIx @7318)' (div n d) (div k d)
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @37306) [1..]

-- instance OEIS 37444 where
--   oeisIx n = p (map (^ 2) [1..]) (n^2) where
--      p _      0 = 1
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 37445 where
--   oeisIx = product . map (oeisIx . (oeisIx @120)) . (rowT @124010)

-- instance OEIS 37800 where
--   oeisIx = f 0 . (rowT @30308) where
--      f c [_]          = c
--      f c (1 : 0 : bs) = f (c + 1) bs
--      f c (_ : bs)     = f c bs

-- instance OEIS 37834 where
--   oeisIx n = sum $ map fromEnum $ zipWith (/=) (tail bs) bs
--               where bs = (rowT @30308) n

-- instance OEIS 37861 where
--   oeisIx n = (oeisIx @23416) n - (oeisIx @120) n

-- instance OEIS 37888 where
--   oeisIx n = div (sum $ map abs $ zipWith (-) bs $ reverse bs) 2
--      where bs = (rowT @30308) n

-- instance OEIS 37992 where
--   oeisIx n = head [x | x <- [1..], (oeisIx @5) x == 2 ^ n]

-- instance OEIS 38044 where
--   oeis = 1 : f 1 [1] where
--      f x ys = y : f (x + 1) (y:ys) where
--        y = sum $ zipWith ((*) `on` (oeisIx @38044)) divs $ reverse divs
--            where divs = (rowT @27750) x

-- instance OEIS 38135 where
--   oeis = 0 : 1 : 1 : 1 : f 1 1 1 4 where
--      f u v w x = y : f v w y (x + 1) where
--        y = q (x - u) + q (x - v) + q (x - w)
--        q z = if abs z >= x then 0 else (oeisIx @38135) $ abs z

-- instance OEIS 38137 where
--   oeis = tablList @38137
-- instance Table 38137 where
--   tabl = map reverse (tabl @37027)

-- instance OEIS 38163 where
--   oeis = map
--       (sum . zipWith (*) (intersperse 0 $ tail (oeis @217)) . reverse) $
--       tail $ inits $ tail (oeis @217) where

-- instance OEIS 38186 where
--   oeis = map succ $ elemIndices 1
--                  $ zipWith (*) (map (oeisIx @188641) [1..]) (map (oeisIx @188642) [1..])

-- instance OEIS 38194 where
--   oeisIx = flip mod 9 . (oeisIx @40)

-- instance OEIS 38199 where
--   oeisIx n = sum [oeisIx (n `div` d) * (oeisIx d)| d <- (rowT @27750) n]

-- instance OEIS 38207 where
--   oeis = concat $ iterate ([2,1] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 38220 where
--   oeis = tablList @38220
-- instance Table 38220 where
--   tabl = iterate (\row ->
--      zipWith (+) (map (* 3) (row ++ [0])) (map (* 2) ([0] ++ row))) [1]

-- instance OEIS 38221 where
--   oeis = concat $ iterate ([3,3] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 38255 where
--   oeis = tablList @38255
-- instance Table 38255 where
--   tabl = map reverse (tabl @13613)

-- instance OEIS 38374 where
--   oeisIx = maximum . map length . filter ((== 1) . head) . group .
--      unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

-- instance OEIS 38394 where
--   oeis = f "" (oeis @40) where
--      f xs (q:qs) = (read ys :: Integer) : f ys qs
--        where ys = show q ++ xs

-- instance OEIS 38397 where
--   oeis = f "" $ tail (oeis @290) where
--      f xs (q:qs) = (read ys :: Integer) : f ys qs
--        where ys = show q ++ xs

-- instance OEIS 38399 where
--   oeis = h "" $ tail (oeis @45) where
--      h xs (f:fs) = (read ys :: Integer) : h ys fs
--        where ys = show f ++ xs

-- instance OEIS 38444 where
--   oeis = 11 : f [11] 90 where
--      f xs@ (x:_) z = ys ++ f ys (10 * z) where
--                     ys = (x + z) : map (* 10) xs

-- instance OEIS 38447 where
--   import Data.Set (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @38447) !! (n - 1)
--   oeis = f $ fromList [11111] where
--      f s = m : f (union s' $ fromList $ g [] $ show m) where
--           (m, s') = deleteFindMin s
--      g _  []       = []
--      g us ('0':vs) = g (us ++ ['0']) vs
--      g us ('1':vs) = (read (us ++ "10" ++ vs)) : g (us ++ ['1']) vs

-- instance OEIS 38500 where
--   oeisIx = f 1 where
--      f y x = if m == 0 then f (y * 3) x' else y  where (x', m) = divMod x 3

-- instance OEIS 38502 where
--   oeisIx (succ->n) = if m > 0 then n else (oeisIx @38502) n'  where (n', m) = divMod n 3

-- instance OEIS 38507 where
--   oeisIx = (+ 1) . (oeisIx @142)
--   oeis = 2 : f 1 2 where
--      f x y = z : f (x + 1) z where z = x * (y - 1) + 1

-- instance OEIS 38509 where
--   oeis = [x | x <- (oeis @2808), gcd x 6 == 1]

-- instance OEIS 38528 where
--   oeis = gen ([1], 1) where
--      gen (_, 10) = []
--      gen (ds, len)
--         | len `elem` ds && chi ds
--           = foldr (\u v -> u + 10*v) 0 ds : gen (succ (ds, len))
--         | otherwise = gen (succ (ds, len))
--      chi xs = null ys || ys /= xs && chi ys where
--               ys = tr $ filter (/= length xs) xs
--               tr zs = if null zs || last zs > 0 then zs else tr $ init zs
--      succ ([], len)   = ([1], len + 1)
--      succ (d : ds, len)
--          | d < len = (head (dropWhile (<= d) (oeis @2024) \\ ds) : ds, len)
--          | otherwise = (0 : ds', len') where (ds', len') = succ (ds, len)

-- instance OEIS 38529 where
--   oeisIx n = (oeisIx @40) n - (oeisIx @2808) n

-- instance OEIS 38547 where
--   oeisIx n = fromJust $ find ((== n) . length . divisors) [1,3..]
--      where divisors m = filter ((== 0) . mod m) [1..m]

-- instance OEIS 38548 where
--   oeisIx n = genericLength $ takeWhile (<= (oeisIx @196) n) $ (rowT @27750) n

-- instance OEIS 38549 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @38548))

-- instance OEIS 38550 where
--   oeis = filter ((== 2) . (oeisIx @1227)) [1..]

-- instance OEIS 38554 where
--   oeisIx n = foldr (\d v -> v * 2 + d) 0 $ zipWith xor bs $ tail bs
--      where bs = (rowT @30308) n

-- instance OEIS 38555 where
--   oeisIx n = foldr (\d v -> v * 3 + d) 0 $
--      zipWith (\x y -> (x + y) `mod` 3) ts $ tail ts
--      where ts = (rowT @30341) n

-- instance OEIS 38556 where
--   oeisIx (fi->n) = fi do n `xor` (oeisIx $ 2 * n + 1) :: Integer

-- instance OEIS 38558 where
--   oeisIx = fromJust . (`elemIndex` (oeis @38554))

-- instance OEIS 38561 where
--   oeisIx = head . (rowT @46937)

-- instance OEIS 38567 where
--   oeis = concatMap (\x -> genericTake (oeisIx @10 x) $ repeat x) [1..]

-- instance OEIS 38570 where
--   oeisIx = (oeisIx @38554) . (oeisIx @38554)

-- instance OEIS 38571 where
--   oeisIx n = snd $ until
--      ((== 0) . fst) (\ (x, i) -> (fi $ (oeisIx @38554) x, i + 1)) (n, 0)

-- instance OEIS 38572 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @53645) n * m + n' where (n', m) = divMod n 2

-- instance OEIS 38573 where
--   oeisIx 0 = 0
--   oeisIx n = (m + 1) * (oeisIx n') + m where (n', m) = divMod n 2

-- instance OEIS 38575 where
--   oeisIx n = if n == 0 then 0 else (oeisIx @1222) $ (oeisIx @45) n

-- instance OEIS 38585 where
--   oeisIx 0 = 0
--   oeisIx n = (9 * m + 1) * (oeisIx n') + m where (n', m) = divMod n 2

-- instance OEIS 38610 where
--   oeisIx = foldl lcm 1 . (rowT @38566)

-- instance OEIS 38618 where
--   oeis = filter ((== 1) . (oeisIx @168046)) (oeis @40)

-- instance OEIS 38622 where
--   oeis = tablList @38622
-- instance Table 38622 where
--   tabl = iterate (\row -> map sum $
--      transpose [tail row ++ [0,0], row ++ [0], [head row] ++ row]) [1]

-- instance OEIS 38670 where
--   oeis = elemIndices 2 $ map (oeisIx @193095) [0..]

-- instance OEIS 38680 where
--   oeis = filter (any ((== 1) . (oeisIx @10051). read) .
--                              init . tail . tails . show) (oeis @40)

-- instance OEIS 38699 where
--   oeisIx = until ((== 1) . (oeisIx @10051)) ((+ 1) . (* 2)) . (subtract 1)

-- instance OEIS 38719 where
--   oeis = tablList @38719
-- instance Table 38719 where
--   tabl = iterate f [1] where
--      f row = zipWith (+) (zipWith (*) [0..] $ [0] ++ row)
--                          (zipWith (*) [2..] $ row ++ [0])

-- instance OEIS 38720 where
--   oeis = (transpose $ map reverse (tabl @38719)) !! 1

-- instance OEIS 38721 where
--   oeis = (transpose (tabl @38719)) !! 2

-- instance OEIS 38722 where
--   oeis = concat (tabl @38722)
--   oeisIx_tabl = map reverse (tabl @27)
--   oeisIx_row n = (tabl @38722) !! (n - 1)

-- instance OEIS 38731 where
--   oeis = c [1] $ tail (oeis @45) where
--      c us vs'@ (v:vs) = (sum $ zipWith (*) us vs') : c (v:us) vs

-- instance OEIS 38754 where
--   oeis = concat $ transpose [oeis, (oeis @8776)]

-- instance OEIS 38769 where
--   oeisIx n = genericLength $ filter (== 0)
--               $ map ((mod n) . digitToInt) $ filter (> '0') $ show n

-- instance OEIS 38772 where
--   oeis = filter p [1..] where
--      p n = all (> 0) $ map ((mod n) . digitToInt) $ filter (> '0') $ show n

-- instance OEIS 39634 where
--   oeisIx 1 = 1
--   oeisIx n = until ((== 1) . (oeisIx @10051)) (flip div 2) n

-- instance OEIS 39635 where
--   oeisIx 1 = 1
--   oeisIx n = until ((== 1) . (oeisIx @10051)) ((flip div 2) . (+ 1)) n

-- instance OEIS 39636 where
--   oeisIx 1 = 1
--   oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
--                           (\ (x, i) -> (x `div` 2 , i + 1)) (n, 1)

-- instance OEIS 39637 where
--   oeisIx 1 = 1
--   oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
--                           (\ (x, i) -> ((x + 1) `div` 2 , i + 1)) (n, 1)

-- instance OEIS 39638 where
--   oeisIx 1 = 1
--   oeisIx n = until ((== 1) . (oeisIx @10051)) (flip div 2) (oeisIx n - 1)

-- instance OEIS 39639 where
--   oeisIx = until ((== 1) . (oeisIx @10051)) (flip div 2) . (+ 1) . (oeisIx @40)

-- instance OEIS 39640 where
--   oeisIx 1 = 1
--   oeisIx n = until ((== 1) . (oeisIx @10051)) (flip div 2 . (+ 1)) (oeisIx n - 1)

-- instance OEIS 39641 where
--   oeisIx = until ((== 1) . (oeisIx @10051)) (flip div 2 . (+ 1)) . (+ 1) . (oeisIx @40)

-- instance OEIS 39642 where
--   oeisIx 1 = 1
--   oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
--                     (\ (x, i) -> (x `div` 2 , i + 1)) (oeisIx n - 1, 1)

-- instance OEIS 39643 where
--   oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
--                     (\ (x, i) -> (x `div` 2 , i + 1)) (oeisIx n + 1, 1)

-- instance OEIS 39644 where
--   oeisIx 1 = 1
--   oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
--               (\ (x, i) -> ((x + 1) `div` 2 , i + 1)) (oeisIx n - 1, 1)

-- instance OEIS 39645 where
--   oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
--               (\ (x, i) -> ((x + 1) `div` 2 , i + 1)) (oeisIx n + 1, 1)

-- instance OEIS 39686 where
--   oeis = filter ((== 1) . (oeisIx @10052)) (oeis @191933)

-- instance OEIS 39701 where
--   oeisIx = (`mod` 3) . (oeisIx @40)
--   oeis = map (`mod` 3) (oeis @40)

-- instance OEIS 39702 where
--   oeisIx = (`mod` 4) . (oeisIx @40)

-- instance OEIS 39723 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @39723) n' * 10 + m where
--      (n',m) = if r < 0 then (q + 1, r + 10) else qr where
--               qr@ (q, r) = quotRem n (negate 10)

-- instance OEIS 39724 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @39724) n' * 10 + m where
--      (n', m) = if r < 0 then (q + 1, r + 2) else (q, r)
--                where (q, r) = quotRem n (negate 2)

-- instance OEIS 39833 where
--   oeis = f (oeis @6881) where
--      f (u : vs@ (v : w : xs))
--        | v == u+1 && w == v+1 = u : f vs
--        | otherwise            = f vs

-- instance OEIS 39913 where
--   oeis = tablList @39913
-- instance Table 39913 where
--   tabl = [[0], [1, 1]] ++ f [0] [1, 1] where
--      f us@ (u:us') vs@ (v:vs') = ws : f vs ws where
--        ws = [u + v, u + v + v] ++ zipWith (+) us vs'

-- instance OEIS 39955 where
--   oeis = filter ((== 1) . (`mod` 4)) (oeis @5117)

-- instance OEIS 39956 where
--   oeis = filter even (oeis @5117)

-- instance OEIS 39957 where
--   oeis = filter ((== 3) . (`mod` 4)) (oeis @5117)

-- instance OEIS 39999 where
--   oeisIx n = genericLength $ filter ((== 1) . (oeisIx @10051))
--                      (map read (nub $ permutations $ show n) :: [Integer])

-- instance OEIS 40026 where
--   oeisIx n = f 1 where
--      f t | (1 - t*r) `mod` s == 0 = t*r
--          | (1 + t*r) `mod` s == 0 = - t*r
--          | otherwise              = f (t + 1)
--      (r,s) = split n 1
--      split x y | m == 0 = split x' (2 * y)
--                | m == 1 = (x,y) where (x',m) = divMod x 2

-- instance OEIS 40027 where
--   oeisIx n = head $ (rowT @46936) (n + 1)

-- instance OEIS 40040 where
--   oeisIx = flip div 2 . (oeisIx @14574)

-- instance OEIS 40051 where
--   oeisIx n = p 1 n :: Int where
--      p _ 0 = 1
--      p k m | k <= m = p k (m - k) `xor` p (k+1) m | k > m  = 0

-- instance OEIS 40081 where
--   oeisIx = genericLength . takeWhile ((== 0) . (oeisIx @10051)) .
--                          iterate  ((+ 1) . (* 2)) . (subtract 1)

-- instance OEIS 40976 where
--   oeisIx n = (oeisIx @40) n - 2
--   oeis = map (subtract 2) (oeis @40)

-- instance OEIS 41013 where
--   oeis = 1 : f 1 where
--      f x | rev <= x  = (2*x) : f (2*x)
--          | otherwise = rev : f rev where rev = (oeisIx @4086) x

-- instance OEIS 42939 where
--   oeisIx = (oeisIx @40997) . (oeisIx @40)

-- instance OEIS 42965 where
--   oeisIx =  (`div` 3) . (subtract 3) . (* 4)
--   oeis = 0 : 1 : 3 : map (+ 4) (oeis @42965)

-- instance OEIS 42968 where
--   oeisIx = (`div` 3) . (subtract 1) . (* 4)
--   oeis = filter ((/= 0) . (`mod` 4)) [1..]

-- instance OEIS 42974 where
--   oeis =  1 : 2 :
--      concat (zipWith replicate (tail (oeis @42974)) (oeis @42974))

-- instance OEIS 43096 where
--   oeis = elemIndices 1 (oeis @196368)

-- instance OEIS 43276 where
--   oeisIx = maximum . (rowT @101211)

-- instance OEIS 43545 where
--   oeisIx = (1 -) . (oeisIx @36987)

-- instance OEIS 43548 where
--   oeisIx n = f 1 where
--      f k = if distinct $ (map (div k)) [n, n - 1 .. 1] then k else f (k + 1)
--      distinct [_] = True; distinct (u:vs@ (v:_)) = u /= v && distinct vs

-- instance OEIS 44051 where
--   oeisIx = (`div` 2) . (+ 1) . (oeisIx @6995) . (+ 1)

-- instance OEIS 44813 where
--   oeis = filter p [1..] where
--      p x = nub xs == xs where
--            xs = map length $ group $ (rowT @30308) x

-- instance OEIS 45323 where
--   oeis = filter ((== 1). (oeisIx @10051)) (oeis @4776)

-- instance OEIS 45468 where
--   oeis = [x | x <- (oeis @47209), (oeisIx @10051) x == 1]

-- instance OEIS 45472 where
--   oeis = [x | x <- (oeis @47336), (oeisIx @10051) x == 1]

-- instance OEIS 45532 where
--   oeisIx n = read $ show n ++ show (oeisIx n) :: Integer

-- instance OEIS 45533 where
--   oeis = f $ map show (oeis @40) :: [Integer] where
--      f (t:ts@ (t':_)) = read (t ++ t') : f ts

-- instance OEIS 45537 where
--   oeisIx n = 2 + length
--      (takeWhile (not . ((show n) `isInfixOf`) . show) $ iterate (* n) (n^2))

-- instance OEIS 45541 where
--   oeis = 2 : f 2 where
--      f x = x' : f x'
--          where x' = read $ filter (`notElem` show x) $ show (x^2)

-- instance OEIS 45572 where
--   oeis = filter ((/= 0) . (`mod` 5)) (oeis @5408)

-- instance OEIS 45616 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = (oeis @45616) !! (n - 1)
--   oeis = filter
--                  (\p -> powerMod 10 (p - 1) (p ^ 2) == 1) (oeis @40)'

-- instance OEIS 45636 where
--   oeis = findIndices (> 0) (oeis @45698)

-- instance OEIS 45698 where
--   oeisIx n = genericLength $ filter (\x -> x > 0 && (oeisIx @10051)' x == 1) $
--   map (oeisIx . (n -)) $
--   takeWhile (<= div n 2) (oeis @1248)

-- instance OEIS 45797 where
--   oeis = filter (even . (`mod` 10) . (`div` 10)) (oeis @45572)

-- instance OEIS 45798 where
--   oeis = filter (odd . (`mod` 10) . (`div` 10)) (oeis @45572)

-- instance OEIS 45910 where
--   oeis =  [x | x <- takeWhile (<= 999999999) $ (oeis @9994),
--                        oeisIx x == 1]

-- instance OEIS 45917 where
--   oeisIx n = sum $ map (oeisIx . (2 * n -)) $ takeWhile (<= n) (oeis @40)

-- instance OEIS 45920 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @76191)

-- instance OEIS 45953 where
--   oeis = filter chi (oeis @8851) where
--      chi n = (x == y && xs `isSub'` ys) where
--         x:xs = show $ div n 10
--         y:ys = show $ div (n^2) 10
--         isSub' us vs = any id $ zipWith (&&)
--                                 (map (`isPrefixOf` vs) $ inits us)
--                                 (map (`isSuffixOf` vs) $ tails us)

-- instance OEIS 45966 where
--   oeisIx 1 = 3
--   oeisIx n = product $ zipWith (^)
--               (map (oeisIx @101300) $ (rowT @27748) n) (oeisIx_row n)

-- instance OEIS 45967 where
--   oeisIx 1 = 4
--   oeisIx n = product $ zipWith (^)
--               (map (oeisIx @151800) $ (rowT @27748) n) (map (+ 1) $ (rowT @124010) n)

-- instance OEIS 45974 where
--   oeisIx n = g n n where
--      g x y = product [oeisIx (oeisIx pi + (oeisIx @49084) pj) ^ (ei * ej) |
--                       (pi,ei) <- zip (oeisIx_row x) (oeisIx_row x),
--                       (pj,ej) <- zip (oeisIx_row y) (oeisIx_row y)]

-- instance OEIS 45975 where
--   oeis = tablList @45975
-- instance Table 45975 where
--   rowCol = rowCol_off @45975 @1 @1
--   rowT   = rowT_off   @45975 @1
--   tabl = f 1 [1..] where
--      f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
--        ys | even k    = take k ms
--           | otherwise = take k $ filter odd ms
--        ms = filter ((== 0) . (`mod` k)) xs

-- instance OEIS 45980 where
--   oeis = 0 : filter f [1..] where
--      f x = g $ takeWhile ((<= 4 * x) . (^ 3)) $ (rowT @27750) x where
--        g [] = False
--        g (d:ds) = r == 0 && (oeisIx @10052) (d ^ 2 - 4 * y) == 1 || g ds
--          where (y, r) = divMod (d ^ 2 - div x d) 3

-- instance OEIS 45981 where
--   oeis = 1 : f 1 [] where
--      f x zs = y : f y zs' where
--        y = read (concatMap show zs')
--        zs' = zs ++ [oeisIx x]

-- instance OEIS 45982 where
--   oeis = 1 : f [1] where
--      f xs = y : f (xs ++ [y]) where
--        y = (oeisIx @45918) $ read (concatMap show xs)

-- instance OEIS 45985 where
--   oeisIx n = head [k | (k, x) <- zip [1..] (oeis @7504),
--                         let (y, r) = divMod x n, r == 0, (oeisIx @10051)' y == 1]

-- instance OEIS 45995 where
--   oeis = tablList @45995
-- instance Table 45995 where
--   tabl = map (map (oeisIx . fromInteger)) (tabl @7318)

-- instance OEIS 46028 where
--   oeis = f 1 where
--      f x | null zs   = f (x + 1)
--          | otherwise = (fst $ head zs) : f (x + 1)
--          where zs = reverse $ filter ((> 1) . snd) $
--                     zip (oeisIx_row x) (oeisIx_row x)

-- instance OEIS 46042 where
--   oeisIx = p $ tail (oeis @583) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 46071 where
--   oeis = tablList @46071
-- instance Table 46071 where
--   rowCol n k = genericIndex (tabf @46071) (n - 2) !! (k-1)
--   rowT n = genericIndex (tabf @46071) (n - 2)
--   tabf = f [1] 2 3 where
--      f qs@ (q:_) i j = ys : f ((q + j) : qs) (i + 1) (j + 2) where
--                       ys = nub $ sort $ filter (> 0) $ map (flip mod i) qs

-- instance OEIS 46075 where
--   import Data.Set (fromList, deleteFindMin, insert)
--   oeisIx n = (oeis @46075) !! (n - 1)
--   oeis = f $ fromList
--                  [100 * a + 10 * b + a | a <- [1..9], b <- [0..9], b /= a]
--      where f s = m : f (insert (10 * m + div (mod m 100) 10) s')
--                  where (m, s') = deleteFindMin s

-- instance OEIS 46090 where
--   oeis = 1 : 4 : map (subtract 2)
--      (zipWith (-) (map (* 6) (tail (oeis @46090))) (oeis @46090))

-- instance OEIS 46092 where
--   oeisIx = (* 2) . (oeisIx @2378)

-- instance OEIS 46099 where
--   oeis = filter ((== 1) . (oeisIx @212793)) [1..]

-- instance OEIS 46100 where
--   oeis = filter ((< 4) . (oeisIx @51903)) [1..]

-- instance OEIS 46101 where
--   oeis = filter ((> 3) . (oeisIx @51903)) [1..]

-- instance OEIS 46132 where
--   oeis = filter ((== 1) . (oeisIx @10051)') $ map (+ 4) (oeis @40)

-- instance OEIS 46315 where
--   oeis = filter odd (oeis @1358)

-- instance OEIS 46316 where
--   oeis = filter ((== 3) . (oeisIx @1222)) [1, 3 ..]

-- instance OEIS 46388 where
--   oeis = filter ((== 2) . (oeisIx @1221)) (oeis @56911)

-- instance OEIS 46447 where
--   oeis = 1 : filter f [1..] where
--      f x = length ps > 1 && ps' == reverse ps'
--            where ps' = concatMap show ps; ps = (rowT @27746) x

-- instance OEIS 46530 where
--   oeisIx n = genericLength $ nub $ map (`mod` n) $
--                              take (fromInteger n) $ tail (oeis @578)

-- instance OEIS 46642 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @9191)

-- instance OEIS 46665 where
--   oeisIx n = (oeisIx @6530) n - (oeisIx @20639) n

-- instance OEIS 46704 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @7953)) (oeis @40)

-- instance OEIS 46711 where
--   oeis = [x | x <- (oeis @42963), (oeisIx @161) x > 0]

-- instance OEIS 46712 where
--   oeis = filter ((`elem` [1,2]) . (`mod` 4)) (oeis @22544)

-- instance OEIS 46727 where
--   oeis = 0 : f (tail (oeis @1652)) (tail (oeis @46090)) where
--      f (x:_:xs) (_:y:ys) = x : y : f xs ys

-- instance OEIS 46741 where
--   oeis = tablList @46741
-- instance Table 46741 where
--   tabl = [[1], [1, 1], [1, 4, 2]] ++ f [1] [1, 1] [1, 4, 2] where
--      f us vs ws = ys : f vs ws ys where
--        ys = zipWith (+) (zipWith (+) (ws ++ [0]) ([0] ++ map (* 2) ws))
--                         (zipWith (-) ([0] ++ vs ++ [0]) ([0, 0, 0] ++ us))

-- instance OEIS 46758 where
--   oeis = filter (\n -> (oeisIx @50252) n == (oeisIx @55642) n) [1..]

-- instance OEIS 46759 where
--   oeis = filter (\n -> (oeisIx @50252) n < (oeisIx @55642) n) [1..]

-- instance OEIS 46760 where
--   oeis = filter (\n -> (oeisIx @50252) n > (oeisIx @55642) n) [1..]

-- instance OEIS 46810 where
--   oeisIx n = genericLength $ filter ((== 1) . (oeisIx @10051))
--                      $ map read (nub $ filter ((> '0') . head)
--                                               $ permutations $ show n)

-- instance OEIS 46816 where
--   oeis = concat $ concat $ iterate ([[1],[1,1]] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 46818 where
--   oeisIx = (oeisIx @120) . (oeisIx @16777)

-- instance OEIS 46831 where
--   oeis = filter ((> 0) . (`mod` 10)) (oeis @18834)

-- instance OEIS 46851 where
--   oeis = filter chi (oeis @8851) where
--      chi n = (x == y && xs `isSub` ys) where
--         x:xs = show $ div n 10
--         y:ys = show $ div (n^2) 10
--      isSub [] ys       = True
--      isSub _  []       = False
--      isSub us'@ (u:us) (v:vs)
--            | u == v    = isSub us vs
--            | otherwise = isSub us' vs

-- instance OEIS 46854 where
--   oeis = tablList @46854
-- instance Table 46854 where
--   tabl = [1] : f [1] [1,1] where
--      f us vs = vs : f vs  (zipWith (+) (us ++ [0,0]) ([0] ++ vs))

-- instance OEIS 46897 where
--   oeisIx 1 = 1
--   oeisIx n = product $ zipWith
--               (\p e -> if p == 2 then 3 else div (p ^ (e + 1) - 1) (p - 1))
--               (rowT @27748 n) (rowT @124010 n)

-- instance OEIS 46899 where
--   oeis = tablList @46899
-- instance Table 46899 where
--   tabl = zipWith take [1..] $ transpose (tabl @7318)

-- instance OEIS 46902 where
--   oeis = tablList @46902
-- instance Table 46902 where
--   tabl = [0] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [6])) [1,6]

-- instance OEIS 46922 where
--   oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (< n) (oeis @1105)

-- instance OEIS 46923 where
--   oeisIx = (oeisIx @46922) . (oeisIx @5408)

-- instance OEIS 46930 where
--   oeisIx 1 = 1
--   oeisIx n = subtract 2 $ (oeisIx @31131) n

-- instance OEIS 46951 where
--   oeisIx = sum . map (oeisIx @10052) . (rowT @27750)

-- instance OEIS 46953 where
--   oeis = map (`div` 6) $
--      filter ((== 0) . (oeisIx @10051)' . subtract 1) [6,12..]

-- instance OEIS 46954 where
--   oeis = map (`div` 6) $ filter ((== 0) . (oeisIx @10051)' . (+ 1)) [0,6..]

-- instance OEIS 46970 where
--   oeisIx = product . map ((1 -) . (^ 2)) . (rowT @27748)

-- instance OEIS 47160 where
--   oeisIx n = if null ms then -1 else head ms
--               where ms = [m | m <- [0 .. n - 1],
--                               (oeisIx @10051)' (n - m) == 1, (oeisIx @10051)' (n + m) == 1]

-- instance OEIS 47791 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @62028)) [1..]

-- instance OEIS 47813 where
--   oeisIx = last . (rowT @262188)

-- instance OEIS 47814 where
--   oeisIx n = if null ips then 0 else head ips
--      where ips = [p | p <- reverse $ takeWhile (<= n) (oeis @40),
--                       show p `isInfixOf` show n]

-- instance OEIS 47836 where
--   import Data.List.Ordered (union)
--   oeisIx n = (oeis @47836) !! (n - 1)
--   oeis = f [2] where
--      f (x:xs) = x : f (xs `union` map (x *) [2..x])

-- instance OEIS 47842 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = read $ concat $
--      zipWith ((++) `on` show) (map length xs) (map head xs)
--      where xs = group $ sort $ map (read . return) $ show n

-- instance OEIS 47846 where
--   oeis = 1 : zipWith (-) (tail (oeis @196277)) (oeis @196277)

-- instance OEIS 47916 where
--   oeis = tablList @47916
-- instance Table 47916 where
--   rowCol = rowCol_off @47916 @1 @1
--   rowT   = rowT_off   @47916 @1
--   tabl = zipWith4 (zipWith4 (\x u v w -> x * v ^ u * w))
--                  (tabl @54523) (tabl @2260) (tabl @10766) (tabl @166350)

-- instance OEIS 47917 where
--   oeis = tablList @47917
-- instance Table 47917 where
--   rowCol = rowCol_off @47917 @1 @1
--   rowT   = rowT_off   @47917 @1
--   tabl = zipWith (zipWith div) (tabl @47916) (tabl @2024)

-- instance OEIS 47918 where
--   oeis = tablList @47918
-- instance Table 47918 where
--   rowCol n k = sum [oeisIx (fi d) * (oeisIx @47916) n (k `div` d) |
--                      mod n k == 0, d <- [1..k], mod k d == 0]
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @47918) [1..]

-- instance OEIS 47919 where
--   oeis = tablList @47919
-- instance Table 47919 where
--   rowCol = rowCol_off @47919 @1 @1
--   rowT   = rowT_off   @47919 @1
--   tabl = zipWith (zipWith div) (tabl @47918) (tabl @2024)

-- instance OEIS 47920 where
--   oeis = tablList @47920
-- instance Table 47920 where
--   tabl = map fst $ iterate e ([1], 1) where
--      e (row, n) = (scanl (-) (n * head row) row, n + 1)

-- instance OEIS 47927 where
--   oeisIx n = if n == 2 then 0 else (oeisIx @245334) n 3

-- instance OEIS 47949 where
--   oeisIx n = if null qs then -1 else head qs  where
--      qs = [m | m <- [n, n - 1 .. 0], (oeisIx @10051)' (n+m) == 1, (oeisIx @10051)' (n-m) == 1]

-- instance OEIS 47983 where
--   oeisIx n = genericLength [x | x <- [1..n - 1], (oeisIx @5) x == (oeisIx @5) n]

-- instance OEIS 47993 where
--   oeisIx = flip (oeisIx @63995) 0

-- instance OEIS 47994 where
--   oeisIx n = f n 1 where
--      f 1 uph = uph
--      f x uph = f (x `div` sppf) (uph * (sppf - 1)) where sppf = (oeisIx @28233) x

-- instance OEIS 48004 where
--   tri n k | (k < 0) || (k > n) = 0
--           | (k == 0) || (k == n) = 1
--           | otherwise = 2*tri (n - 1) k + tri (n-1) (k-1) - 2*tri (n-2) (k-1)
--                               + tri (n-k-1) (k-1) - tri (n-k-2) k

-- instance OEIS 48050 where
--   oeisIx 1 = 0
--   oeisIx n = (subtract 1) $ sum $ (rowT @27751) n

-- instance OEIS 48055 where
--   oeis = [x | x <- (oeis @2808),
--                  let (us,vs) = partition ((== 1) . (oeisIx @10051)) $ (rowT @27751) x,
--                  sum us + x == sum vs]

-- instance OEIS 48098 where
--   oeis = [x | x <- [1..], (oeisIx @6530) x ^ 2 <= x]

-- instance OEIS 48102 where
--   import Data.Set (empty, fromList, deleteFindMin, union)
--   import qualified Data.Set as Set (null, map)
--   oeisIx n = (oeis @48102) !! (n - 1)
--   oeis = 1 : f empty [1] (oeis @51674) where
--     f s ys pps'@ (pp:pps)
--       | Set.null s = f (fromList (map (* pp) ys)) (pp:ys) pps
--       | pp < m     = f (s `union` Set.map (* pp) s `union`
--                         fromList (map (* pp) ys)) ys pps
--       | otherwise  = m : f s' (m:ys) pps'
--       where (m,s') = deleteFindMin s

-- instance OEIS 48103 where
--   oeis = filter (\x -> and $
--      zipWith (>) (oeisIx_row x) (map toInteger $ (rowT @124010) x)) [1..]

-- instance OEIS 48152 where
--   oeis = tablList @48152
-- instance Table 48152 where
--   rowCol = rowCol_off @48152 @1 @1
--   rowT   = rowT_off   @48152 @1
--   tabl = zipWith (map . flip mod) [1..] (tabl @133819)

-- instance OEIS 48153 where
--   oeisIx = sum . (rowT @48152)

-- instance OEIS 48161 where
--   oeis = [p | p <- (oeis @65091), (oeisIx @10051) ((p^2 + 1) `div` 2) == 1]

-- instance OEIS 48250 where
--   oeisIx = sum . (rowT @206778)

-- instance OEIS 48272 where
--   oeisIx n = (oeisIx @1227) n - (oeisIx @183063) n

-- instance OEIS 48298 where
--   oeisIx n = (oeisIx @209229) n * n

-- instance OEIS 48344 where
--   oeis = filter f (oeis @29742) where
--      f x = (oeisIx @136522) (x * (oeisIx @4086) x) == 1

-- instance OEIS 48377 where
--   oeisIx :: Integer -> Integer
--   oeisIx n =
--      read $ concat $ zipWith replicate (map ((+ 1) . digitToInt) ns) ns
--         where ns = show n

-- instance OEIS 48379 where
--   oeisIx n = if n == 0 then 1 else x n where
--      x m = if m == 0 then 0 else 10 * x m' + (d + 1) `mod` 10
--            where (m',d) = divMod m 10

-- instance OEIS 48385 where
--   oeisIx 0 = 0
--   oeisIx n = read (show (oeisIx n') ++ show (m ^ 2)) :: Integer
--               where (n', m) = divMod n 10

-- instance OEIS 48395 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @199771) (2 * n)

-- instance OEIS 48398 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @33075)

-- instance OEIS 48411 where
--   oeis = filter ((== 1) . (oeisIx @10052)) (oeis @33075)

-- instance OEIS 48519 where
--   oeis = map (oeisIx @40) $ filter ((== 1) . (oeisIx @10051)' . (oeisIx @65073)) [1..]

-- instance OEIS 48521 where
--   oeis = map (oeisIx @40) $ filter ((> 0) . (oeisIx @107740)) [1..]

-- instance OEIS 48594 where
--   oeis = tablList @48594
-- instance Table 48594 where
--   rowCol = rowCol_off @48594 @1 @1
--   rowT   = rowT_off   @48594 @1
--   tabl = map snd $ iterate f (1, [1]) where
--      f (i, xs) = (i + 1, zipWith (-) (zipWith (*) [1..] ([0] ++ xs))
--                                      (map (* i) (xs ++ [0])))

-- instance OEIS 48645 where
--   oeis = tablList @48645
--   rowCol = rowCol_off @48645 @1 @1
--   rowT   = rowT_off   @48645 @1
--   tabl = iterate (\xs -> insert (2 * head xs + 1) $ map ((* 2)) xs) [1]
--   oeis = concat (tabl @48645)

-- instance OEIS 48646 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @48653)

-- instance OEIS 48647 where
--   oeisIx 0 = 0
--   oeisIx n = 4 * (oeisIx @48647) n' + if m == 0 then 0 else 4 - m
--               where (n', m) = divMod n 4

-- instance OEIS 48653 where
--   oeis = filter (f . show . (^ 2)) [1..] where
--      f zs = g (init $ tail $ inits zs) (tail $ init $ tails zs)
--      g (xs:xss) (ys:yss)
--        | h xs      = h ys || f ys || g xss yss
--        | otherwise = g xss yss
--        where h ds = head ds /= '0' && (oeisIx @10052) (read ds) == 1
--      g _ _ = False

-- instance OEIS 48669 where
--   oeisIx n = maximum $ zipWith (-) (tail ts) ts where
--      ts = (rowT @38566) n ++ [n + 1]

-- instance OEIS 48678 where
--   oeisIx 0 = 0
--   oeisIx x = 2 * (b + 1) * (oeisIx @48678) x' + b
--               where (x', b) = divMod x 2

-- instance OEIS 48691 where
--   oeisIx = product . map (oeisIx @5408 . fi) . (rowT @124010)

-- instance OEIS 48700 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @48700) !! (n - 1)
--   oeis = f 1 $ singleton 1 where
--      f z s = m : f (z+1) (insert (c 0) (insert (c 1) s')) where
--        c d = foldl (\v d -> 2 * v + d) 0 $ (reverse b) ++ [d] ++ b
--        b = unfoldr
--            (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) z
--        (m,s') = deleteFindMin s

-- instance OEIS 48701 where
--   oeisIx n = foldr (\d v -> 2 * v + d) 0 (reverse bs ++ bs) where
--      bs = (rowT @30308) (n - 1)

-- instance OEIS 48728 where
--   oeisIx n = (oeisIx @8585) n - (oeisIx @48724) n

-- instance OEIS 48736 where
--   oeis = 1 : 1 : 1 : 1 :
--      zipWith div
--        (zipWith (+)
--          (zipWith (*) (drop 3 (oeis @48736))
--                       (drop 1 (oeis @48736)))
--          (drop 2 (oeis @48736)))
--        (oeis @48736)

-- instance OEIS 48761 where
--   oeisIx n = (oeisIx n + 1 - (oeisIx @10052) n) ^ 2
--   oeis = 0 : concat (f 1 1) where
--      f u v = (take v $ repeat u) : f (u + v + 2) (v + 2)

-- instance OEIS 48762 where
--   oeisIx n = last $ takeWhile (<= n) (oeis @578)

-- instance OEIS 48763 where
--   oeisIx 0 = 0
--   oeisIx n = head $ dropWhile (< n) (oeis @578)

-- instance OEIS 48765 where
--   oeisIx n = (oeis @48764) !! (n - 1)
--   oeis = f [1..] $ tail (oeis @142) where
--      f (u:us) vs'@ (v:vs) | u == v    = v : f us vs
--                          | otherwise = v : f us vs'

-- instance OEIS 48766 where
--   oeisIx = round . (** (1/3)) . fi
--   oeis = concatMap (\x -> take (oeisIx x) $ repeat x) [0..]

-- instance OEIS 48793 where
--   oeis = tablList @48793
-- instance Table 48793 where
--   tabf = [0] : [1] : f [[1]] where
--      f xss = yss ++ f (xss ++ yss) where
--        yss = [y] : map (++ [y]) xss
--        y = last (last xss) + 1

-- instance OEIS 48794 where
--   oeis = map (read . concatMap show) (tabf @48793) :: [Integer]

-- instance OEIS 48883 where
--   oeisIx = (oeisIx @244) . (oeisIx @120)

-- instance OEIS 48889 where
--   import Numeric (readInt)
--   oeisIx n = (oeis @48889) !! (n - 1)
--   oeis = filter f (oeis @2808) where
--      f n = n `mod` 10 > 0 &&
--            null ("23547" `intersect` show n)  &&
--            (oeisIx (fst $ head $ readInt 10 (const True) ud $ ns) == 1)
--          where ns = reverse $ show n
--                ud '6' = 9
--                ud '9' = 6
--                ud z = digitToInt z

-- instance OEIS 48890 where
--   oeis = filter f (oeis @40) where
--      f x = all (`elem` [0,1,6,8,9]) ds && x' /= x && (oeisIx @10051) x' == 1
--        where x' = foldl c 0 ds
--              c v 6 = 10*v + 9; c v 9 = 10*v + 6; c v d = 10*v + d
--              ds = unfoldr d x
--              d z = if z == 0 then Nothing else Just $ swap $ divMod z 10

-- instance OEIS 48951 where
--   oeis = 2 : 4 : ulam 2 4 (oeis @48951)

-- instance OEIS 48966 where
--   oeis = tablList @48966
-- instance Table 48966 where
--   rowCol = rowCol_off @48966 @1 @1
--   rowT   = rowT_off   @48966 @1
--   tabl = [1] : f 2 [1] where
--      f x xs = ys : f (x + 1) ys where
--        ys = map (flip div x) $ zipWith (+)
--             (map (* 3) $ zipWith (*) (map (3 * (x - 1) -) [1..]) (xs ++ [0]))
--             (zipWith (*) [1..] ([0] ++ xs))

-- instance OEIS 48973 where
--   import Data.List.Ordered (minus)
--   oeisIx n = (oeis @48973) !! (n - 1)
--   oeis = [1..] `minus` (oeis @5243)

-- instance OEIS 48985 where
--   oeisIx = foldr (\d v -> 2 * v + d) 0 . concatMap
--      (unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2))
--      . reverse . (rowT @27746)

-- instance OEIS 48994 where
--   oeis = tablList @48994
-- instance Table 48994 where
--   tabl = map fst $ iterate (\ (row, i) ->
--   (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 0)

-- instance OEIS 49002 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @49001)

-- instance OEIS 49039 where
--   oeis = tablList @49039
-- instance Table 49039 where
--   rowCol = rowCol_off @49039 @1 @1
--   rowT   = rowT_off   @49039 @1
--   tabl = f 1 1 [1..] where
--      f k p xs = ys : f (2 * k) (1 - p) (dropWhile (<= last ys) xs) where
--        ys  = take k $ filter ((== p) . (`mod` 2)) xs

-- instance OEIS 49061 where
--   oeis = tablList @49061
-- instance Table 49061 where
--   rowCol = rowCol_off @49061 @1 @1
--   rowT   = rowT_off   @49061 @1
--   tabl = map fst $ iterate t ([1], 1) where
--      t (row, k) = (if odd k then us else vs, k + 1) where
--        us = zipWith (-) (row ++ [0]) ([0] ++ row)
--        vs = zipWith (+) ((zipWith (*) ks row) ++ [0])
--                         ([0] ++ (zipWith (*) (reverse ks) row))
--             where ks = [1..k]

-- instance OEIS 49063 where
--   oeis = tablList @49063
-- instance Table 49063 where
--   tabf = [1] : iterate f [1, 1] where
--      f row = 1 : 2 : zipWith (+) ( map (* 2) row) ((tail row) ++ [0])

-- instance OEIS 49068 where
--   oeisIx = filter ((== 0) . (oeisIx @240025)) [0..]

-- instance OEIS 49076 where
--   oeisIx = (+ 1) . (oeisIx @78442)

-- instance OEIS 49098 where
--   oeis = filter ((== 0) . (oeisIx @8966) . (+ 1)) (oeis @40)

-- instance OEIS 49345 where
--   oeisIx n | n < 2100  = read $ concatMap show (oeisIx_row n) :: Int
--            | otherwise = error "ambiguous primorial representation"

-- instance OEIS 49354 where
--   oeis = filter f [1..] where
--      f n = t0 == (oeisIx @62756) n && t0 == (oeisIx @81603) n where t0 = (oeisIx @77267) n

-- instance OEIS 49388 where
--   oeisIx = (flip div 5040) . (oeisIx @142) . (+ 7)

-- instance OEIS 49389 where
--   oeisIx = (flip div 40320) . (oeisIx @142) . (+ 8)

-- instance OEIS 49398 where
--   oeisIx = (flip div 362880) . (oeisIx @142) . (+ 9)

-- instance OEIS 49417 where
--   oeisIx 1 = 1
--   oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
--      f p e = product $ zipWith div
--              (map (subtract 1 . (p ^)) $
--                   zipWith (*) (oeis @79) $ map (+ 1) $ (rowT @30308) e)
--              (map (subtract 1 . (p ^)) (oeis @79))

-- instance OEIS 49419 where
--   oeisIx = product . map (oeisIx . fi) . (rowT @124010)

-- instance OEIS 49444 where
--   oeis = tablList @49444
-- instance Table 49444 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 2)

-- instance OEIS 49445 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @199238)

-- instance OEIS 49455 where
--   oeis = tablList @49455
-- instance Table 49455 where
--   rowCol = rowCol_off @49455 @1 @1
--   rowT   = rowT_off @49455 @1
--   tabf = map (map numerator) $ iterate
--      (\row -> concat $ transpose [row, zipWith (+/+) row $ tail row]) [0, 1]
--      where u +/+ v = (numerator u + numerator v) %
--                      (denominator u + denominator v)

-- instance OEIS 49456 where
--   oeis = tablList @49456
-- instance Table 49456 where
--   rowCol = rowCol_off @49456 @1 @1
--   rowT   = rowT_off @49456 @1
--   tabf = iterate
--      (\row -> concat $ transpose [row, zipWith (+) row $ tail row]) [1, 1]

-- instance OEIS 49458 where
--   oeis = tablList @49458
-- instance Table 49458 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 3)

-- instance OEIS 49459 where
--   oeis = tablList @49459
-- instance Table 49459 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 4)

-- instance OEIS 49460 where
--   oeis = tablList @49460
-- instance Table 49460 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 5)

-- instance OEIS 49502 where
--   oeisIx = f 0 1 where
--      f m i x = if x <= 4
--                   then m else f (if mod x 4 == 1
--                                     then m + i else m) (i + 1) $ div x 2

-- instance OEIS 49514 where
--   oeis = filter ((== 0) . (oeisIx @95916)) [1..]

-- instance OEIS 49599 where
--   oeisIx = product . map ((+ 1) . (oeisIx @5) . fi) . (rowT @124010) . succ

-- instance OEIS 49613 where
--   oeisIx n = 2 * n - (oeisIx @7917) (2 * n - 2)

-- instance OEIS 49642 where
--   oeis = filter ((== 0) . (oeisIx @245656)) [1..]

-- instance OEIS 49773 where
--   oeis = tablList @49773
-- instance Table 49773 where
--   rowCol = rowCol_off @49773 @1 @1
--   rowT   = rowT_off @49773 @1
--   tabf = iterate f [1] where
--      f vs = (map (subtract 1) ws) ++ ws where ws = map (* 2) vs

-- instance OEIS 49782 where
--   oeisIx :: Int -> Integer
--   oeisIx n = (sum $ take n (oeis @142)) `mod` (fi n)

-- instance OEIS 49820 where
--   oeisIx n = n - (oeisIx @5) n

-- instance OEIS 49853 where
--   oeis = 1 : 2 : 2 : 3 :
--      zipWith (+) (oeis @49853)
--                  (zipWith (+) (drop 2 (oeis @49853)) (drop 3 (oeis @49853)))



-- instance OEIS 50000 where
--   oeis = 1 : f [1,0] where
--      f xs'@ (x:xs) | x `div` 2 `elem` xs = 3 * x : f (3 * x : xs')
--                   | otherwise = x `div` 2 : f (x `div` 2 : xs')

-- instance OEIS 50001 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @50000)) + 1

-- instance OEIS 50146 where
--   oeisIx n = if n == 0 then 1 else (oeisIx @35607) (2 * n - 2) (n - 1)
--   (oeisIx @50146) = lambda n : n*hypergeometric ([1-n, n], [2], -1) if n>0 else 1

-- instance OEIS 50150 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) [1, 3 ..]

-- instance OEIS 50252 where
--   oeisIx 1 = 1
--   oeisIx n = sum $ map (oeisIx @55642) $
--               (oeisIx_row n) ++ (filter (> 1) $ (rowT @124010) n)

-- instance OEIS 50264 where
--   oeis = filter chi [2..] where
--      chi n = f (oeis @40) where
--         f (p:ps) | p*p > n   = True
--                  | otherwise = 2 * abs (2 * (n `mod` p) - p) <= p && f ps

-- instance OEIS 50320 where
--   oeisIx n = h n $ tail $ (rowT @206778) n where
--      h 1 _          = 1
--      h _ []         = 0
--      h m fs'@ (f:fs) =
--        if f > m then 0 else if r > 0 then h m fs else h m' fs' + h m fs
--        where (m', r) = divMod m f

-- instance OEIS 50328 where
--   oeis = f 1 where
--      f x = (if x == 1 then 1 else
--            sum $ map (oeisIx . (div x)) $ tail $ (rowT @206778) x) : f (x + 1)

-- instance OEIS 50360 where
--   oeisIx = (oeisIx @688) . (oeisIx @25487)

-- instance OEIS 50361 where
--   oeisIx = product . map (oeisIx @9) . (rowT @124010)

-- instance OEIS 50382 where
--   oeisIx = (oeisIx @8480) . (oeisIx @25487)

-- instance OEIS 50430 where
--   import Numeric (showIntAtBase)
--   oeisIx n = (oeis @50430) !! (n - 1)
--   oeis = f 1 where
--      f n = g (showIntAtBase 2 intToDigit n "") : f (n+1)
--      g zs | zs == reverse zs = length zs
--           | otherwise        = max (h $ init zs) (h $ tail zs)
--      h zs@ ('0':_) = g zs
--      h zs@ ('1':_) = (oeisIx @50430) $ foldl (\v d -> digitToInt d + 2*v) 0 zs

-- instance OEIS 50435 where
--   oeisIx = (oeisIx @2808) . (oeisIx @2808)
--   oeis = map (oeisIx @2808) (oeis @2808)

-- instance OEIS 50480 where
--   oeis = filter chi [2..] where
--      chi x = xs `elem` (map concat $ choices divs) where
--         choices = concat . (map permutations) . subsequences
--         divs = filter (`isInfixOf` xs)
--                       $ map show $ filter ((== 0) . mod x) [1..oeisIx x]
--         xs = show x

-- instance OEIS 50488 where
--   oeisIx n = sum $ zipWith (*) (oeis @79) (reverse $ take n (oeis @5408))

-- instance OEIS 50600 where
--   oeis = tablList @50600
-- instance Table 50600 where
--   rowCol n k = adder 0 (n - k) k where
--      adder :: Int -> Int -> Int -> Int
--      adder c u 0 = c
--      adder c u v = adder (c + 1) (u `xor` v) (shiftL (u .&. v) 1)
--   rowT n = map (oeisIx n) $ reverse [0..n]
--   tabl = map (rowT @50600) [0..]

-- instance OEIS 50804 where
--   oeis = filter ((== 1) . (oeisIx @84888)) [0..]

-- instance OEIS 50805 where
--   oeis = filter ((all (== 0)) . f) (oeis @40) where
--      f p = map (i $ show p) "0123456789"
--      i ps d = (oeisIx @10051)' (read $ intersperse d ps :: Integer)

-- instance OEIS 50806 where
--   oeis = filter ((== 1) . sum . f) (oeis @40) where
--      f p = map (i $ show p) "0123456789"
--      i ps d = (oeisIx @10051)' (read $ intersperse d ps :: Integer)

-- instance OEIS 50925 where
--   oeis = 1 : -1 : (tail $ map (numerator . sum) $
--      zipWith (zipWith (%))
--      (zipWith (map . (*)) (drop 2 (oeis @142)) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 50931 where
--   oeis = filter (any (== 1) . map (flip mod 6) . (rowT @27748)) [1..]

-- instance OEIS 50932 where
--   oeis = 1 : map (denominator . sum) (zipWith (zipWith (%))
--      (zipWith (map . (*)) (drop 2 (oeis @142)) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 50936 where
--   import Data.Set (empty, findMin, deleteMin, insert)
--   import qualified Data.Set as Set (null)
--   oeisIx n = (oeis @50936) !! (n - 1)
--   oeis = f empty [2] 2 $ tail (oeis @40) where
--      f s bs c (p:ps)
--        | Set.null s || head bs <= m = f (foldl (flip insert) s bs') bs' p ps
--        | otherwise                  = m : f (deleteMin s) bs c (p:ps)
--        where m = findMin s
--              bs' = map (+ p) (c : bs)

-- instance OEIS 50941 where
--   import Data.List.Ordered (minus)
--   oeisIx n = (oeis @50941) !! (n - 1)
--   oeis = minus [0..] (oeis @34706)

-- instance OEIS 50997 where
--   oeisIx = (^ 5) . (oeisIx @40)
--   oeis = map (^ 5) (oeis @40)

-- instance OEIS 50999 where
--   oeisIx = sum . map (^ 2) . (rowT @182469)

-- instance OEIS 51000 where
--   oeisIx = sum . map (^ 3) . (rowT @182469)

-- instance OEIS 51004 where
--   oeis =  [x | x <- (oeis @5349),
--                        x == head (dropWhile (< x) (oeis @34838))]

-- instance OEIS 51010 where
--   oeis = tablList @51010
-- instance Table 51010 where
--   rowCol n k = snd $ until ((== 0) . snd . fst)
--                       (\ ((x, y), i) -> ((y, mod x y), i + 1)) ((n, k), 0)
--   rowT n = map (oeisIx n) [0..n - 1]
--   tabl = map (rowT @51010) [1..]

-- instance OEIS 51011 where
--   oeisIx n = numerator $ (sum $ (rowT @51010) n) % n

-- instance OEIS 51012 where
--   oeisIx n = denominator $ (sum $ (rowT @51010) n) % n

-- instance OEIS 51015 where
--   oeis = filter zeisel [3, 5 ..] where
--      zeisel x = 0 `notElem` ds && length ds > 2 &&
--            all (== 0) (zipWith mod (tail ds) ds) && all (== q) qs
--            where q:qs = (zipWith div (tail ds) ds)
--                  ds = zipWith (-) (tail ps) ps
--                  ps = 1 : (rowT @27746) x

-- instance OEIS 51022 where
--   oeisIx n = if n < 10 then n else (oeisIx @51022) n' * 100 + r
--               where (n', r) = divMod n 10

-- instance OEIS 51035 where
--   oeis = filter ((== 0) . (oeisIx @10051)) (oeis @14091)

-- instance OEIS 51037 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @51037) !! (n - 1)
--   oeis = f $ singleton 1 where
--      f s = y : f (insert (5 * y) $ insert (3 * y) $ insert (2 * y) s')
--                  where (y, s') = deleteFindMin s

-- instance OEIS 51129 where
--   oeis = tablList @51129
-- instance Table 51129 where
--   rowCol n k = k ^ (n - k)
--   rowT   = rowT_off   @51129 @1
--   tabl = zipWith (zipWith (^)) (tabl @2260) $ map reverse (tabl @2260)

-- instance OEIS 51135 where
--   oeis = map length $ group (oeis @4001)

-- instance OEIS 51139 where
--   oeisIx n = (oeisIx @994) (n + 2) - (oeisIx @995) (n + 2)

-- instance OEIS 51144 where
--   oeis = filter ((== 0) . (oeisIx @8966)) (oeis @37)

-- instance OEIS 51145 where
--   oeis = 0 : 1 : f 1 1 where
--      f x b = y : f y z where
--        (y, z) = head [ (y, z) | y <- [1..],
--                                let z = x .|. y :: Integer, z > b]

-- instance OEIS 51146 where
--   oeis = zipWith (.|.) (oeis @51145) $ tail (oeis @51145)

-- instance OEIS 51147 where
--   oeisIx = fromJust . (`elemIndex` (oeis @51145)) . (2 ^)

-- instance OEIS 51159 where
--   oeis = tablList @51159
-- instance Table 51159 where
--   tabl = [1] : f [1] [1,1] where
--      f us vs = vs : f vs (zipWith (+) ([0,0] ++ us) (us ++ [0,0]))

-- instance OEIS 51162 where
--   oeis = tablList @51162
-- instance Table 51162 where
--   tabl = iterate (\xs@ (x:_) -> (x + 1) : map (+ 2) xs) [0]

-- instance OEIS 51169 where
--   oeisIx n = head [m | m <- [2..],
--               all (== 0) $ map (oeisIx' . (2*m -)) $ take n (oeis @40)]

-- instance OEIS 51178 where
--   oeis = filter (\x -> (oeisIx @27423) x `mod` x == 0) [1..]

-- instance OEIS 51248 where
--   oeisIx n = 2 + length
--      (takeWhile (not . (show n `isPrefixOf`) . show) $ iterate (* n) (n^2))

-- instance OEIS 51250 where
--   oeis = filter (all ((== 1) . (oeisIx @10055)) . (rowT @38566)) [1..]

-- instance OEIS 51278 where
--   oeis = filter ((== 1) . (oeisIx @51521)) [1..]

-- instance OEIS 51279 where
--   oeis = filter ((== 2) . (oeisIx @51521)) [1..]

-- instance OEIS 51282 where
--   oeisIx = (oeisIx @7814) . (oeisIx @25487)

-- instance OEIS 51283 where
--   oeis = filter (\x -> (oeisIx @34699 x) ^ 2 < x) [1..]

-- instance OEIS 51338 where
--   oeis = tablList @51338
-- instance Table 51338 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 6)

-- instance OEIS 51339 where
--   oeis = tablList @51339
-- instance Table 51339 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 7)

-- instance OEIS 51362 where
--   oeis = filter p $ drop 4 (oeis @40) where
--      p x = all (== 1) $ map (oeisIx . read) $
--                zipWith (++) (inits $ show x) (tail $ tails $ show x)

-- instance OEIS 51377 where
--   oeisIx n = product $ zipWith sum_e (oeisIx_row n) (oeisIx_row n) where
--      sum_e p e = sum [p ^ d | d <- (rowT @27750) e]

-- instance OEIS 51378 where
--   oeisIx n = product $ zipWith sum_1e (oeisIx_row n) (oeisIx_row n)
--      where sum_1e p e = 1 + sum [p ^ d | d <- (rowT @27750) e]

-- instance OEIS 51379 where
--   oeis = tablList @51379
-- instance Table 51379 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 8)

-- instance OEIS 51380 where
--   oeis = tablList @51380
-- instance Table 51380 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 9)

-- instance OEIS 51402 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` ms) where
--      ms = map (abs . (oeisIx @2321)) [1..]

-- instance OEIS 51424 where
--   oeisIx = genericLength . filter f . partitions where
--      f [] = True
--      f (p:ps) = (all (== 1) $ map (gcd p) ps) && f ps
--      partitions n = ps 1 n where
--        ps x 0 = [[]]
--        ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

-- instance OEIS 51431 where
--   oeisIx = (flip div 3628800) . (oeisIx @142) . (+ 10)

-- instance OEIS 51436 where
--   oeisIx n = (3 ^ n + 3 ^ m - 2 ^ n + (1 - r) * 2 ^ m) `div` 2 + r
--               where (m,r) = divMod n 2

-- instance OEIS 51466 where
--   oeis = f [head (oeis @25487)] $ tail (oeis @25487) where
--      f us (v:vs) = fromJust (find (\x -> mod v x == 0) us) : f (v : us) vs

-- instance OEIS 51487 where
--   oeis = [x | x <- [2..], let t = (oeisIx @10) x, t == (oeisIx @10) (x - t)]

-- instance OEIS 51488 where
--   oeis = [x | x <- [2..], let t = (oeisIx @10) x, t < (oeisIx @10) (x - t)]

-- instance OEIS 51521 where
--   oeisIx n = genericLength [k | k <- [1..4*n^2],
--                           let d = (oeisIx @5) k, divMod k d == (n,0)]

-- instance OEIS 51523 where
--   oeis = tablList @51523
-- instance Table 51523 where
--   tabl = map fst $ iterate (\ (row, i) ->
--      (zipWith (-) ([0] ++ row) $ map (* i) (row ++ [0]), i + 1)) ([1], 10)

-- instance OEIS 51532 where
--   oeis = filter ((== 1) . (oeisIx @212793)) (oeis @56867)

-- instance OEIS 51597 where
--   oeis = tablList @51597
-- instance Table 51597 where
--   tabl = iterate (\row -> zipWith (+) ([1] ++ row) (row ++ [1])) [1]

-- instance OEIS 51599 where
--   oeis = tablList @51599
-- instance Table 51599 where
--   tabl = map fst $ iterate f ([2], (oeis @1223)) where
--      f (row, (d:ds)) =  (zipWith (+) ([d] ++ row) (row ++ [d]), ds)

-- instance OEIS 51601 where
--   oeis = tablList @51601
-- instance Table 51601 where
--   tabl = iterate
--                  (\row -> zipWith (+) ([1] ++ row) (row ++ [1])) [0]

-- instance OEIS 51611 where
--   oeis = filter ((== 0) . (oeisIx @53603)) [1..]

-- instance OEIS 51632 where
--   oeis = tablList @51632
--   oeis = concat (tabl @51632)
--   tabl = iterate (\rs -> zipWith (+) ([1] ++ rs) (rs ++ [1])) [-2]

-- instance OEIS 51634 where
--   oeis = f (oeis @40) where
--      f (p:qs@ (q:r:ps)) = if 2 * q > (p + r) then q : f qs else f qs

-- instance OEIS 51635 where
--   oeis = g (oeis @40) where
--      g (p:qs@ (q:r:ps)) = if 2 * q < (p + r) then q : g qs else g qs

-- instance OEIS 51656 where
--   oeisIx = sum . zipWith (*) (oeis @1906) . (rowT @47999)

-- instance OEIS 51666 where
--   oeis = tablList @51666
-- instance Table 51666 where
--   tabl = map fst $ iterate
--      (\ (vs, w:ws) -> (zipWith (+) ([w] ++ vs) (vs ++ [w]), ws))
--      ([0], [1, 3 ..])

-- instance OEIS 51683 where
--   oeis = tablList @51683
-- instance Table 51683 where
--   rowCol = rowCol_off @51683 @1 @1
--   rowT   = rowT_off   @51683 @1
--   tabl = map fst $ iterate f ([1], 2) where
--      f (row, n) = (row' ++ [head row' + last row'], n + 1) where
--        row' = map (* n) row

-- instance OEIS 51701 where
--   oeis = f 2 $ 1 : (oeis @40) where
--      f d (q:ps@ (p:p':_)) = (if d <= d' then q else p') : f d' ps
--        where d' = p' - p

-- instance OEIS 51777 where
--   oeis = tablList @51777
-- instance Table 51777 where
--   rowCol n k = (rowT @51777) n !! (k-1)
--   rowT n = map (mod n) [n, n - 1 .. 1]
--   tabl = map (rowT @51777) [1..]

-- instance OEIS 51778 where
--   oeis = tablList @51778
-- instance Table 51778 where
--   rowCol = rowCol_off @51778 @3 @1
--   rowT = rowT_off @51778 @3
--   tabl = map (\xs -> map (mod (head xs + 1)) xs) $
--                      iterate (\xs -> (head xs + 1) : xs) [2]

-- instance OEIS 51786 where
--   oeis = 1 : 1 : 1 : 1 :
--      zipWith div (tail $ zipWith3 (\u v w -> 1 + u * v * w)
--                  (drop 2 (oeis @51786)) (tail (oeis @51786)) (oeis @51786))
--                  (oeis @51786)

-- instance OEIS 51841 where
--   oeisIx n = (sum $ zipWith (\u v -> gcd 2 u * (oeisIx @8683) u * 2 ^ v)
--                ds $ reverse ds) `div` (2 * n) where ds = (rowT @27750) n

-- instance OEIS 51883 where
--   oeis = 1 : f 2 "1" where
--      f :: Integer -> String -> [Int]
--      f x zs = y : f (x + 1) (zs ++ show y) where
--        y = fromJust $ findIndex
--            ((== 0) . (`mod` x) . read . (zs ++)) $ map show [0..]

-- instance OEIS 51933 where
--   oeis = tablList @51933
-- instance Table 51933 where
--   rowCol n k = n `xor` k :: Int
--   rowT n = map (oeisIx n) [0..n]
--   tabl = map (rowT @51933) [0..]

-- instance OEIS 52008 where
--   oeisIx n = (oeisIx @4185) n + (oeisIx @4186) n

-- instance OEIS 52011 where
--   oeis = c 0 0 $ drop 2 (oeis @45) where
--     c x y fs'@ (f:fs) | x < f     = c (x+1) (y + (oeisIx @10051) x) fs'
--                       | otherwise = y : c (x+1) 0 fs

-- instance OEIS 52012 where
--   oeis = c 1 0 $ tail (oeis @204) where
--     c x y ls'@ (l:ls) | x < l     = c (x+1) (y + (oeisIx @10051) x) ls'
--                       | otherwise = y : c (x+1) 0 ls

-- instance OEIS 52018 where
--   oeis = filter f [0..] where
--      f x = show (oeisIx x) `isInfixOf` show x

-- instance OEIS 52021 where
--   oeis = tail $ filter (\x -> (oeisIx @7953) x == (oeisIx @6530) x) [1..]

-- instance OEIS 52109 where
--   oeis = 1 : f 2 [1] where
--      f n xs = z : f (n+1) (z:xs) where
--        z = sum $ map (oeisIx . fromInteger) $
--                      dropWhile (<= 0) $ map (n -) xs

-- instance OEIS 52191 where
--   oeisIx n = head $
--      filter ((> 1) . minimum . map length . group . show) $ [0,n..]

-- instance OEIS 52192 where
--   oeisIx n = fromJust $
--      findIndex ((> 1) . minimum . map length . group . show) $ [0,n..]

-- instance OEIS 52203 where
--   oeisIx n = (oeisIx @122366) (2 * n) n

-- instance OEIS 52216 where
--   oeis = 2 : f [2] 9 where
--      f xs@ (x:_) z = ys ++ f ys (10 * z) where
--                     ys = (x + z) : map (* 10) xs

-- instance OEIS 52227 where
--   oeisIx n = (oeisIx n) * (oeisIx n) `div` (oeisIx n)

-- instance OEIS 52248 where
--   oeis = f (oeis @65091) where
--      f (p:ps'@ (p':ps)) = (maximum $ map (oeisIx @6530) [p+1..p'-1]) : f ps'

-- instance OEIS 52287 where
--   import Data.List.Ordered (union)
--   oeisIx n = (oeis @52287) !! (n - 1)
--   oeis = f [3] where
--      f (x:xs) = x : f (xs `union` map (x *) [2..x])

-- instance OEIS 52343 where
--   oeisIx = (flip div 2) . (+ 1) . (oeisIx @8441)

-- instance OEIS 52383 where
--   oeisIx = f . subtract 1 where
--      f 0 = 0
--      f v = 10 * f w + if r > 0 then r + 1 else 0  where (w, r) = divMod v 9

-- instance OEIS 52404 where
--   oeisIx = f . subtract 1 where
--      f 0 = 0
--      f v = 10 * f w + if r > 1 then r + 1 else r  where (w, r) = divMod v 9

-- instance OEIS 52405 where
--   oeisIx = f . subtract 1 where
--      f 0 = 0
--      f v = 10 * f w + if r > 2 then r + 1 else r  where (w, r) = divMod v 9

-- instance OEIS 52406 where
--   oeisIx = f . subtract 1 where
--   f 0 = 0
--   f v = 10 * f w + if r > 3 then r + 1 else r where (w, r) = divMod v 9

-- instance OEIS 52410 where
--   oeisIx n = product $ zipWith (^)
--                         (oeisIx_row n) (map (`div` (foldl1 gcd es)) es)
--               where es = (rowT @124010) n

-- instance OEIS 52413 where
--   oeisIx = f . subtract 1 where
--   f 0 = 0
--   f v = 10 * f w + if r > 4 then r + 1 else r where (w, r) = divMod v 9

-- instance OEIS 52414 where
--   oeisIx = f . subtract 1 where
--   f 0 = 0
--   f v = 10 * f w + if r > 5 then r + 1 else r where (w, r) = divMod v 9

-- instance OEIS 52419 where
--   oeisIx = f . subtract 1 where
--   f 0 = 0
--   f v = 10 * f w + if r > 6 then r + 1 else r where (w, r) = divMod v 9

-- instance OEIS 52421 where
--   oeisIx = f . subtract 1 where
--   f 0 = 0
--   f v = 10 * f w + if r > 7 then r + 1 else r where (w, r) = divMod v 9

-- instance OEIS 52423 where
--   oeisIx n = f n n where
--      f x 1 = 1
--      f x y | x < 10    = gcd x y
--            | otherwise = if d == 1 then 1 else f x' (gcd d y)
--            where (x', d) = divMod x 10

-- instance OEIS 52474 where
--   oeisIx n = (tabl @56230) !! (n - 1) !! 0

-- instance OEIS 52485 where
--   oeis = filter ((== 0) . (oeisIx @112526)) [1..]

-- instance OEIS 52499 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @52499) !! n
--   oeis = f $ singleton 1 where
--      f s = m : f (insert (2*m) $ insert (4*m-1) s') where
--         (m, s') = deleteFindMin s

-- instance OEIS 52509 where
--   oeis = tablList @52509
-- instance Table 52509 where
--   tabl = [1] : [1,1] : f [1] [1,1] where
--      f row' row = rs : f row rs where
--        rs = zipWith (+) ([0] ++ row' ++ [1]) (row ++ [0])

-- instance OEIS 52548 where
--   oeisIx = (+ 2) . (oeisIx @79)
--   oeis = iterate ((subtract 2) . (* 2)) 3

-- instance OEIS 53127 where
--   oeisIx = (* 2) . (oeisIx @53132)

-- instance OEIS 53141 where
--   oeis = 0 : 2 : map (+ 2)
--      (zipWith (-) (map (* 6) (tail (oeis @53141))) (oeis @53141))

-- instance OEIS 53210 where
--   oeisIx = sum . (rowT @51599)

-- instance OEIS 53212 where
--   oeisIx = (oeisIx @5)' . (oeisIx @7416)

-- instance OEIS 53392 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = if ys == "" then 0 else read ys where
--      ys = foldl (++) "" $ map show $ zipWith (+) (tail ds) ds
--      ds = (map (read . return) . show) n

-- instance OEIS 53405 where
--   oeisIx n = head [a | a <- [1..], n `kara` a == Nothing] where
--      kara a b = if null ks then Nothing else Just $ head ks
--                 where ks = [c | c <- [1..a], a <= c * b, a > c * (b - 1)]

-- instance OEIS 53432 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @53432) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [8, 5, 4, 9, 1, 7, 6, 3, 2, 0]

-- instance OEIS 53433 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @53433) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [8, 5, 4, 9, 1, 7, 6, 3, 2, 0]

-- instance OEIS 53636 where
--   oeisIx 0 = 0
--   oeisIx n = sum . zipWith (*) (map (oeisIx @10) ods)
--                  $ map ((2 ^) . (div n)) ods
--     where ods = rowT @182469 n

-- instance OEIS 53989 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10051)' (k * n - 1) == 1]

instance OEIS 54008 where
  oeisIx (succ->n) = n `mod` (oeisIx @5 . pred) n

-- instance OEIS 54024 where
--   oeisIx n = mod (oeisIx n) n

-- instance OEIS 54123 where
--   oeis = tablList @54123
-- instance Table 54123 where
--   tabl = [1] : [1, 1] : f [1] [1, 1] where
--      f us vs = ws : f vs ws where
--                ws = zipWith (+) (0 : init us ++ [0, 0]) (vs ++ [1])

-- instance OEIS 54124 where
--   oeis = tablList @54124
-- instance Table 54124 where
--   tabl = map reverse (tabl @54123)

-- instance OEIS 54211 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @127423)) [1..]

-- instance OEIS 54240 where
--   oeisIx :: Integer -> Integer -> Integer
--   oeisIx x 0 = x
--   oeisIx x y = (oeisIx @54240) (x `xor` y) (shift (x .&. y) 2)
--   oeisIx_adiag n =  map (\k -> (oeisIx @54240) (n - k) k) [0..n]
--   oeisIx_square = map (oeisIx @54240)_adiag [0..]

-- instance OEIS 54440 where
--   oeisIx = sum . zipWith (*) (oeis @87960) . map (oeisIx @1255) . (rowT @260672)

-- instance OEIS 54496 where
--   oeisIx n = product $
--               zipWith (^) (rowT @54496 n) (map (oeisIx @290) $ (rowT @124010) n)

-- instance OEIS 54521 where
--   oeis = tablList @54521
--   rowCol = rowCol_off @54521 @1 @1
--   rowT   = rowT_off   @54521 @1
--   tabl = map (map (oeisIx @63524)) (tabl @50873)
--   oeis = concat (tabl @54521)

-- instance OEIS 54522 where
--   oeis = tablList @54522
-- instance Table 54522 where
--   rowCol = rowCol_off @54522 @1 @1
--   tabl = map (rowT @54522) [1..]
--   rowT n = map (\k -> if n `mod` k == 0 then (oeisIx @10) k else 0) [1..n]

-- instance OEIS 54523 where
--   oeis = tablList @54523
-- instance Table 54523 where
--   rowCol = rowCol_off @54523 @1 @1
--   rowT   = rowT_off   @54523 @1
--   tabl = map (map (\x -> if x == 0 then 0 else (oeisIx @10) x)) (tabl @126988)

-- instance OEIS 54531 where
--   oeis = tablList @54531
-- instance Table 54531 where
--   rowCol n k = div n $ gcd n k
--   rowT   = rowT_off   @54531 @1
--   tabl = zipWith (\u vs -> map (div u) vs) [1..] (tabl @50873)

-- instance OEIS 54584 where
--   oeisIx n = (oeisIx @5) n + 3 * (oeisIx @79978) n * (oeisIx @5) (oeisIx n) + (oeisIx @35191) n

-- instance OEIS 54635 where
--   oeis = tablList @54635
-- instance Table 54635 where
--   tabf = map reverse (tabf @30341)
--   oeis = concat (tabf @54635)

-- instance OEIS 54646 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @70167) $ (oeisIx @302) n

-- instance OEIS 54654 where
--   oeis = tablList @54654
-- instance Table 54654 where
--   tabl = map reverse (tabl @48994)

-- instance OEIS 54686 where
--   oeis = merge (oeis @290) (oeis @217) where
--      merge xs'@ (x:xs) ys'@ (y:ys)
--        | x <= y    = x : merge xs ys'
--        | otherwise = y : merge xs' ys

-- instance OEIS 54735 where
--   oeisIx = (+ 2) . (* 2) . (oeisIx @1359)

-- instance OEIS 54744 where
--   oeis = filter (\x -> and $
--      zipWith (<=) (oeisIx_row x) (map fi $ (rowT @124010) x)) [1..]

-- instance OEIS 54841 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ zipWith (*)
--                     (map ((10 ^) . subtract 1 . (oeisIx @49084)) $ (rowT @27748) n)
--                     (map fi $ (rowT @124010) n)

-- instance OEIS 55087 where
--   oeis = tablList @55087
-- instance Table 55087 where
--   tabf = concat $ transpose [oeisIx_tabl, (tabl @2262)]
--   oeis = concat (tabf @55087)

-- instance OEIS 55099 where
--   oeisIx n = (oeisIx @7481) (2 * n + 1) - (oeisIx @7481) (2 * n)

-- instance OEIS 55118 where
--   oeisIx 0 = 0
--   oeisIx n = if d == 0 then 8 * (oeisIx @55118) n' else 8 * (oeisIx @55118) n' + 8 - d
--               where (n', d) = divMod n 8

-- instance OEIS 55120 where
--   oeisIx = foldl f 0 . reverse . unfoldr g where
--      f v d = if d == 0 then 10 * v else 10 * v + 10 - d
--      g x = if x == 0 then Nothing else Just $ swap $ divMod x 10

-- instance OEIS 55122 where
--   oeisIx 0 = 0
--   oeisIx n = if d == 0 then 12 * (oeisIx @55122) n' else 12 * (oeisIx @55122) n' + 12 - d
--               where (n', d) = divMod n 12

-- instance OEIS 55124 where
--   oeisIx 0 = 0
--   oeisIx n = if d == 0 then 14 * (oeisIx @55124) n' else 14 * (oeisIx @55124) n' + 14 - d
--               where (n', d) = divMod n 14

-- instance OEIS 55126 where
--   oeisIx 0 = 0
--   oeisIx n = if d == 0 then 16 * (oeisIx @55126) n' else 16 * (oeisIx @55126) n' + 16 - d
--               where (n', d) = divMod n 16

-- instance OEIS 55205 where
--   oeisIx n = genericLength [d | d <- [1..n^2], n^2 `mod` d == 0, (oeisIx @10052) d == 0]

-- instance OEIS 55212 where
--   oeisIx = subtract 1 . (oeisIx @33273)

-- instance OEIS 55217 where
--   oeisIx n = sum $ take (n + 1) $ (rowT @27907) (n + 1)

-- instance OEIS 55229 where
--   oeisIx n = product $ zipWith (^) ps (map (flip mod 2) es) where
--      (ps, es) = unzip $
--                 filter ((> 1) . snd) $ zip (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 55248 where
--   oeis = tablList @55248
-- instance Table 55248 where
--   tabl = map reverse (tabl @8949)

-- instance OEIS 55265 where
--   oeis = 1 : f 1 [2..] where
--      f x vs = g vs where
--        g (w:ws) = if (oeisIx @10051) (x + w) == 1
--                      then w : f w (delete w vs) else g ws

-- instance OEIS 55266 where
--   oeis = 1 : f 1 [2..] where
--      f u vs = g vs where
--        g (w:ws) | (oeisIx @10051)' (u + w) == 0 = w : f w (delete w vs)
--                 | otherwise = g ws

-- instance OEIS 55396 where
--   oeisIx = (oeisIx @49084) . (oeisIx @20639)

-- instance OEIS 55742 where
--   oeis = [x | x <- [1..], (oeisIx @1221) x == (oeisIx @1221) (oeisIx x)]

-- instance OEIS 55744 where
--   oeis = 1 : filter f [2..] where
--      f x = all ((== 0) . mod x) (concatMap (oeisIx_row . subtract 1) ps) &&
--            all ((== 0) . mod (oeisIx x))
--                (map fst $ filter ((== 1) . snd) $ zip ps $ (rowT @124010) x)
--            where ps = (rowT @27748) x

-- instance OEIS 55768 where
--   oeisIx = (oeisIx @1221) . (oeisIx @5867)

-- instance OEIS 55769 where
--   oeisIx = (oeisIx @6530) . (oeisIx @5867)

-- instance OEIS 55938 where
--   oeis = concat $
--      zipWith (\u v -> [u+1..v-1]) (oeis @5187) $ tail (oeis @5187)

-- instance OEIS 55975 where
--   oeisIx n = (oeisIx @3188) n - (oeisIx @3188) (n - 1)
--   oeis = zipWith (-) (tail (oeis @3188)) (oeis @3188)

-- instance OEIS 55983 where
--   oeis = iterate (oeisIx @102487 . (+ 1)) 10

-- instance OEIS 56001 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 7) 7

-- instance OEIS 56003 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 8) 8

-- instance OEIS 56011 where
--   oeisIx n = (tabl @56011) !! (n - 1)
--   oeis = concat (tabl @56011)
--   oeisIx_tabl = ox False (tabl @27) where
--     ox turn (xs:xss) = (if turn then reverse xs else xs) : ox (not turn) xss
--   oeisIx_row n = (tabl @56011) !! (n - 1)

-- instance OEIS 56045 where
--   oeisIx n = sum $ map (oeisIx n) $ (rowT @27750) n

-- instance OEIS 56062 where
--   oeis = map length $ group (oeis @30190)

-- instance OEIS 56114 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 9) 9

-- instance OEIS 56230 where
--   oeis = tablList @56230
-- instance Table 56230 where
--   rowCol = rowCol_off @56230 @1 @1
--   tabl = [1] : f [1] [2..] [1] where
--      f adiag (a:as) us | null (adiag' `intersect` us) =
--                          adiag' : f adiag' (as \\ adiag') (us `union` adiag')
--                        | otherwise = f adiag as us
--                        where adiag' = scanl (+) a adiag

-- instance OEIS 56231 where
--   oeisIx n = ([1,2] ++ threeRows !! 0) !! (n - 1)
--   oeisIx n = ([3] ++ threeRows !! 1) !! (n - 1)
--   oeisIx n = threeRows !! 2 !! (n - 1)
--   threeRows = transpose $ f [4..] [1,2,3] [2,1] [3] [] where
--      f (u:free) used us vs ws
--          | u `notElem` used &&
--            v `notElem` used &&
--            w `notElem` used = [u, v, w] :
--                               f free (w:v:u:used) (u:us) (v:vs) (w:ws)
--          | otherwise        = f free used us vs ws
--          where v = u + head us; w = v + head vs

-- instance OEIS 56234 where
--   oeis = notUsed 1 (oeis @56231) (oeis @56232) (oeis @56233) where
--      notUsed x us'@ (u:us) vs'@ (v:vs) ws'@ (w:ws)
--       | x == u = notUsed (x + 1) us vs' ws'
--       | x == v = notUsed (x + 1) us' vs ws'
--       | x == w = notUsed (x + 1) us' vs' ws
--       | otherwise = x : notUsed (x + 1) us' vs' ws'

-- instance OEIS 56239 where
--   oeisIx n = sum $ zipWith (*) (map (oeisIx @49084) $ (rowT @27748) n) (oeisIx_row n)

-- instance OEIS 56240 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @1414))

-- instance OEIS 56524 where
--   oeis = [read (ns ++ reverse ns) :: Integer |
--                   n <- [0..], let ns = show n]

-- instance OEIS 56525 where
--   oeis = [1..9] ++ [read (ns ++ [z] ++ reverse ns) |
--                   n <- [1..], let ns = show n, z <- "0123456789"]

-- instance OEIS 56526 where
--   oeis = zipWith (-) (tail (oeis @960)) (oeis @960)

-- instance OEIS 56538 where
--   oeis = tablList @56538
-- instance Table 56538 where
--   rowCol = rowCol_off @56538 @1 @1
--   rowT   = rowT_off @56538 @1
--   tabf = map reverse (tabf @27750)

-- instance OEIS 56561 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @202018)) [0..]

-- instance OEIS 56576 where
--   oeisIx = subtract 1 . (oeisIx @20914)

-- instance OEIS 56577 where
--   oeisIx = head . (rowT @227048)

-- instance OEIS 56595 where
--   oeisIx n = genericLength [d | d <- [1..n], mod n d == 0, (oeisIx @10052) d == 0]

-- instance OEIS 56606 where
--   oeisIx = (oeisIx @7947) . (oeisIx @1142) . succ

-- instance OEIS 56608 where
--   oeisIx = (oeisIx @20639) . (oeisIx @2808)

-- instance OEIS 56768 where
--   oeisIx = (oeisIx @607) . (oeisIx @40)

-- instance OEIS 56789 where
--   oeisIx = sum . (rowT @51537)

-- instance OEIS 56792 where
--   c i = if i `mod` 2 == 0 then i `div` 2 else i - 1
--   b 0 foldCount = foldCount
--   b sheetCount foldCount = b (c sheetCount) (foldCount + 1)
--   oeisIx n = b n 0

-- instance OEIS 56815 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @45918)) (oeis @40)

-- instance OEIS 56867 where
--   oeis = filter (\x -> gcd x (oeisIx @173557 x) == 1) [1..]

-- instance OEIS 56868 where
--   oeis = filter (any (== 1) . pks) [1..] where
--      pks x = [p ^ k `mod` q | let fs = (rowT @27748) x, q <- fs,
--                               (p,e) <- zip fs $ (rowT @124010) x, k <- [1..e]]

-- instance OEIS 56911 where
--   oeis = filter ((== 1) . (oeisIx @8966)) [1,3..]

-- instance OEIS 56965 where
--   oeisIx n = n - (oeisIx @4086) n

-- instance OEIS 56970 where
--   oeisIx n = p (oeis @47261) n where
--      p _  0     = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 56973 where
--   oeisIx = f 0 where
--      f y x = if x == 0 then y else f (y + 0 ^ (mod x 4)) $ div x 2
--   a (n) = { my (x = bitor (n, n>>1));
--            if (x == 0, 0, 1 + logint (x, 2) - hammingweight (x)) }

-- instance OEIS 56978 where
--   oeisIx = sum . map (fromEnum . ([0,0,1] `isPrefixOf`)) .
--                       tails . (rowT @30308)

-- instance OEIS 56979 where
--   oeisIx = sum . map (fromEnum . ([1,0,1] `isPrefixOf`)) .
--                       tails . (rowT @30308)

-- instance OEIS 56980 where
--   oeisIx = sum . map (fromEnum . ([0,1,1] `isPrefixOf`)) .
--                       tails . (rowT @30308)

-- instance OEIS 56992 where
--   oeisIx = (oeisIx @10888) . (oeisIx @290)

-- instance OEIS 57020 where
--   oeisIx n = numerator $ (oeisIx @203) n % (oeisIx @5) n

-- instance OEIS 57021 where
--   oeisIx n = denominator $ (oeisIx @203) n % (oeisIx @5) n

-- instance OEIS 57022 where
--   oeisIx n = (oeisIx @203) n `div` (oeisIx @5) n

-- instance OEIS 57062 where
--   oeis = f 1 [1..] where
--      f j xs = (replicate (sum $ map (oeisIx @10051) dia) j) ++ f (j + 1) xs'
--        where (dia, xs') = splitAt j xs

-- instance OEIS 57142 where
--   oeisIx n = head $ reverse $ sort $ map length $ group $
--               sort [u * v | u <- [1..n], v <- [1..n]]

-- instance OEIS 57143 where
--   oeisIx n = head $ head $ reverse $ sortBy (compare `on` length) $
--               group $ sort [u * v | u <- [1..n], v <- [1..n]]

-- instance OEIS 57144 where
--   oeisIx n = head $ last $ head $ groupBy ((==) `on` length) $
--               reverse $ sortBy (compare `on` length) $
--               group $ sort [u * v | u <- [1..n], v <- [1..n]]

-- instance OEIS 57153 where
--   oeisIx n = (tabl @56230) !! (n - 1) !! (n-1)

-- instance OEIS 57165 where
--   import Data.Set (Set, singleton, notMember, insert)
--   oeisIx n = (oeis @57165) !! n
--   oeis = r (singleton 0) 1 0 where
--      r :: Set Integer -> Integer -> Integer -> [Integer]
--      r s n x = if x > n && (x - n) `notMember` s
--                   then r (insert (x-n) s) (n+1) (x-n)
--                   else n : r (insert (x+n) s) (n+1) (x+n)

-- instance OEIS 57211 where
--   oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @59841)

-- instance OEIS 57212 where
--   oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @35)

-- instance OEIS 57226 where
--   oeisIx = (oeisIx @43537) . (oeisIx @61493)

-- instance OEIS 57338 where
--   oeisIx n = head $ reverse $ sort $ map length $ group $
--               sort [u * v * w | u <- [1..n], v <- [1..n], w <- [1..n]]

-- instance OEIS 57449 where
--   oeisIx = product . (rowT @193829)

-- instance OEIS 57533 where
--   oeis = filter (\z -> p z [z]) [1..] where
--      p x ts = y > 0 && (y `elem` ts || p y (y:ts)) where y = (oeisIx @48050) x

-- instance OEIS 57562 where
--   oeisIx n = p (oeisIx_row n) n where
--      p _          0 = 1
--      p []         _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 57588 where
--   oeisIx = (subtract 1) . product . (flip take (oeis @40))

-- instance OEIS 57606 where
--   oeis = tablList @57606
-- instance Table 57606 where
--   rowCol = rowCol_off @57606 @3 @1
--   rowT   = rowT_off @57606 @3
--   tabf = map g $ drop 3 $
--     iterate (\xs -> (map (0 :) xs) ++ (map (1 :) xs)) [[]] where
--     g xss = map length $ fill0 $ group $ sort $ map (length . del2) xss
--       where fill0 uss = f0 uss [1 .. length xss `div` 4] where
--              f0 _  []        = []
--              f0 [] (j:js)    = [] : f0 [] js
--              f0 vss'@ (vs:vss) (j:js)
--               | j == head vs = vs : f0 vss js
--               | otherwise    = [] : f0 vss' js
--     del2 = nub . (concatMap del1) . del1
--     del1 xs = nub $
--               zipWith (++) (init $ inits xs) (map tail $ init $ tails xs)

-- instance OEIS 57607 where
--   oeis = tablList @57607
-- instance Table 57607 where
--   rowCol = rowCol_off @57607 @2 @0
--   rowT   = rowT_off @57607 @2
--   tabf =  [2] : map (0 :) (tabf @57606)

-- instance OEIS 57661 where
--   oeisIx n = (oeisIx @51193) n `div` n

-- instance OEIS 57683 where
--   oeis = filter (all (== 1) . p) [1..] where
--      p x = map (oeisIx . (+ (x + 1)) . (x ^)) [2..4]

-- instance OEIS 57705 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @57588)

-- instance OEIS 57716 where
--   oeis = filter ((== 0) . (oeisIx @209229)) [0..]

-- instance OEIS 57728 where
--   oeis = tablList @57728
-- instance Table 57728 where
--   rowCol = rowCol_off @57728 @1 @1
--   rowT   = rowT_off   @57728 @1
--   tabl = iterate
--      (\row -> zipWith (+) (row ++ [0]) ([0] ++ tail row ++ [1])) [1]

-- instance OEIS 57828 where
--   oeisIx x = genericLength $ filter ((== 1) . (gcd x)) $
--                        takeWhile (<= x) $ tail (oeis @290)

-- instance OEIS 57890 where
--   oeis = 0 : filter ((== 1) . (oeisIx @178225) . (oeisIx @265)) [1..]

-- instance OEIS 57891 where
--   oeis = filter ((== 0) . (oeisIx @178225) . (oeisIx @265)) [1..]

-- instance OEIS 57945 where
--   oeisIx n = g n $ reverse $ takeWhile (<= n) $ tail (oeis @217) where
--      g 0 _      = 0
--      g x (t:ts) = g r ts + a where (a,r) = divMod x t

-- instance OEIS 57979 where
--   oeisIx n = 1 - rest * (1 - n') where (n', rest) = divMod n 2
--   oeis = concat $ transpose [repeat 1, [0..]]

-- instance OEIS 58026 where
--   oeisIx n = product $ zipWith (\p e -> p ^ (e - 1) * (p - 2))
--                                 (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 58035 where
--   oeisIx n = product $
--      zipWith (^) (oeisIx_row n) (map (min 3) $ (rowT @124010) n)

-- instance OEIS 58042 where
--   oeisIx = (oeisIx @7088) . (oeisIx @61561)

-- instance OEIS 58071 where
--   oeis = tablList @58071
-- instance Table 58071 where
--   tabl = map (\fs -> zipWith (*) fs $ reverse fs) (tabl @104763)

-- instance OEIS 58197 where
--   oeisIx n = (+ 1) $ fromJust $ findIndex (n <=) $ tail (oeis @51950)

-- instance OEIS 58198 where
--   oeisIx = (+ 1) . (oeisIx @58197)

-- instance OEIS 58199 where
--   oeisIx n = fromJust $ findIndex (n <=) $ map negate (oeis @51950)

-- instance OEIS 58257 where
--   oeis = tablList @58257
-- instance Table 58257 where
--   tabl = [1] : ox 0 [1] where
--      ox turn xs = ys : ox (mod (turn + 1) 4) ys
--         where ys | turn <= 1 = scanl (+) 0 xs
--                  | otherwise = reverse $ scanl (+) 0 $ reverse xs

-- instance OEIS 58277 where
--   oeis = map length $ group (oeis @7614)

-- instance OEIS 58294 where
--   oeis = tablList @58294
-- instance Table 58294 where
--   rowCol = rowCol_off @58294 @1 @1
--   rowT   = rowT_off @58294 @1
--   tabf = [1] : zipWith (++) xss (map (tail . reverse) xss)
--                  where xss = tail (tabl @102473)

-- instance OEIS 58331 where
--   oeisIx = (+ 1) . (oeisIx @1105)

-- instance OEIS 58369 where
--   oeis =
--      elemIndices 0 $ zipWith ((-) `on` (oeisIx @7953)) [0..] (oeis @290)

-- instance OEIS 58529 where
--   oeis = filter (\x -> all (`elem` (takeWhile (<= x) (oeis @1132)))
--                                    $ (rowT @27748) x) [1..]

-- instance OEIS 58842 where
--   oeis = map numerator (renyi 1 []) where
--      renyi :: Rational -> [Rational] -> [Rational]
--      renyi x xs = r : renyi r (x:xs) where
--         r = q - fromInteger ((numerator q) `div` (denominator q))
--         q = 3%2 * x

-- instance OEIS 58971 where
--   oeisIx n = f [n % 1] where
--      f xs@ (x:_) | denominator y == 1 = numerator y
--                 | y `elem` xs        = 0
--                 | otherwise          = f (y : xs)
--                 where y = (oeisIx x') % (oeisIx x')
--                       x' = numerator x + denominator x

-- instance OEIS 58972 where
--   oeis = map numerator $ filter ((f [])) [1..] where
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
--   oeis = filter (odd . (oeisIx @23416)) [1..]

-- instance OEIS 59010 where
--   oeis = filter (even . (oeisIx @23416)) [1..]

-- instance OEIS 59011 where
--   oeis = filter (odd . (oeisIx @71295)) [0..]

-- instance OEIS 59175 where
--   oeisIx n = f [n % 1] where
--      f xs@ (x:_) | denominator y == 1 = numerator y
--                 | y `elem` xs        = 0
--                 | otherwise          = f (y : xs)
--                 where y = (numerator x * denominator x) %
--                           (oeisIx (numerator x) + (oeisIx @7953) (denominator x))



-- instance OEIS 59268 where
--   oeis = tablList @59268
-- instance Table 59268 where
--   tabl = iterate (scanl (+) 1) [1]

-- instance OEIS 59316 where
--   oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @35250)) [1..]

-- instance OEIS 59317 where
--   oeis = tablList @59317
-- instance Table 59317 where
--   tabf = [1] : [1,1,1] : f [1] [1,1,1] where
--      f ws vs = vs' : f vs vs' where
--        vs' = zipWith4 (\r s t x -> r + s + t + x)
--              (vs ++ [0,0]) ([0] ++ vs ++ [0]) ([0,0] ++ vs)
--              ([0,0] ++ ws ++ [0,0])

-- instance OEIS 59401 where
--   oeis = filter (\x -> (oeisIx @10055) x == 0 &&
--                  all (`isInfixOf` show x) (map show $ (rowT @141809) x)) [1..]

-- instance OEIS 59402 where
--   oeis = filter chi [1..] where
--     chi n = n `mod` 10 > 0 && f n 1 0 (oeis @40) where
--       f :: Integer -> Integer -> Int -> [Integer] -> Bool
--       f 1 1 o _    = o > 1
--       f m x o ps'@ (p:ps)
--        | r == 0    = f m' (p*x) o ps'
--        | x > 1     = show x `isInfixOf` show n && f m 1 (o+1) ps
--        | m < p * p = f 1 m o ps
--        | otherwise = f m 1 o ps
--        where (m',r) = divMod m p

-- instance OEIS 59405 where
--   oeis = filter f (oeis @238985) where
--      f x = all (== 0) (map (mod x) digs) && g x digs where
--            g z []         = z == 1
--            g z ds'@ (d:ds) = r == 0 && (h z' ds' || g z' ds)
--                             where (z', r) = divMod z d
--            h z []         = z == 1
--            h z ds'@ (d:ds) = r == 0 && h z' ds' || g z ds
--                             where (z', r) = divMod z d
--            digs = map (read . return) $ filter (/= '1') $ show x

-- instance OEIS 59436 where
--   oeisIx n = head [x | x <- [1..],
--      let dds = map length $ group $ sort $ concatMap show $ (rowT @27750) x,
--      minimum dds == n, length dds == 10]

-- instance OEIS 59448 where
--   oeisIx = (`mod` 2) . (oeisIx @23416)

-- instance OEIS 59481 where
--   oeis = tablList @59481
--   rowCol n k = (tabl @59481) !! n !! n
--   rowT n = (tabl @59481) !! n
--   tabl = map reverse (tabl @100100)

-- instance OEIS 59496 where
--   oeis = 2 : f [2] [2] where
--      f qs xs = g candidates where
--        g [] = []
--        g (ys:yss) | (oeisIx @10051) q == 0 || q `elem` qs = g yss
--                   | otherwise = q : f (q:qs) ys
--                   where q = foldr (\d r -> 10 * r + d) 0 ys
--        candidates = [us ++ [z] ++ vs | i <- [0 .. length xs - 1],
--                            let (us, (_:vs)) = splitAt i xs, z <- [1..9]] ++
--                     [xs ++ [z] | z <- [1..9]]

-- instance OEIS 59497 where
--   oeis = (oeis @40) \\  (oeis @59496)

-- instance OEIS 59514 where
--   oeisIx n = f [n % 1] where
--      f xs@ (x:_)
--        | denominator y == 1 = numerator y
--        | y `elem` xs        = 0
--        | otherwise          = f (y : xs)
--        where y = (numerator x * denominator x) %
--                  (oeisIx (numerator x) + (oeisIx @7953) (denominator x) - 1)

-- instance OEIS 59576 where
--   oeis = tablList @59576
-- instance Table 59576 where
--   tabl = [1] : map fst (iterate f ([1,1], [2,3,2])) where
--      f (us, vs) = (vs, map (* 2) ws) where
--        ws = zipWith (-) (zipWith (+) ([0] ++ vs) (vs ++ [0]))
--                         ([0] ++ us ++ [0])

-- instance OEIS 59590 where
--   oeis = elemIndices 1 $ map (oeisIx @115944) [0..]

-- instance OEIS 59632 where
--   oeisIx n = foldl (\v d -> 10 * v + d) 0 $
--                     map (flip mod 10) $ zipWith (+) ([0] ++ ds) (ds ++ [0])
--               where ds = map (read . return) $ show n

-- instance OEIS 59707 where
--   oeisIx n = if u == n || v == n then n else (oeisIx @59707) (u * v) where
--      (u,v) = foldl (\ (x,y) d -> if odd d then (10*x+d,y) else (x,10*y+d))
--           (0,0) $ reverse $ unfoldr
--           (\z -> if z == 0 then Nothing else Just $ swap $ divMod z 10) n

-- instance OEIS 59708 where
--   oeis = filter sameParity [0..] where
--      sameParity n = all (`elem` "02468") ns
--                  || all (`elem` "13579") ns where ns = show n

-- instance OEIS 59717 where
--   oeisIx n = if u == n || v == n then n else (oeisIx @59717) (u + v) where
--      (u,v) = foldl (\ (x,y) d -> if odd d then (10*x+d,y) else (x,10*y+d))
--           (0,0) $ reverse $ unfoldr
--           (\z -> if z == 0 then Nothing else Just $ swap $ divMod z 10) n

-- instance OEIS 59922 where
--   oeis = tablList @59922
-- instance Table 59922 where
--   oeisIx_flattened = concat (tabl @59922)
--   tabl = iterate (\rs ->
--      zipWith (+) (0 : reverse (0 : replicate (length rs - 1) 1))
--                  $ zipWith (*) ([1] ++ rs) (rs ++ [1])) [1]
--   oeisIx n = (tabl @59922) !! n !! (n - 3)
--   oeisIx n = sum (oeisIx_tabl !! n)
--   oeisIx n = (tabl @59922) !! (2*n) !! n
--   oeisIx n = (tabl @59922) !! n !! n `div` 2

-- instance OEIS 59941 where
--   oeis = map (foldr (\d v -> v * 10 + d) 0) $ f (tabf @30341) where
--      f (xs:xss)
--        | 0 `elem` xs = f xss
--        | otherwise = map fromEnum (zipWith (==)
--                      (tail $ inits xs) (reverse $ init $ tails xs)) : f xss

-- instance OEIS 59966 where
--   oeisIx n = sum (map (\x -> (oeisIx @8683) (n `div` x) * (oeisIx @225) x)
--                        [d | d <- [1..n], mod n d == 0]) `div` n

-- instance OEIS 59983 where
--   oeis = mapMaybe (`elemIndex` (oeis @7599)) [0..]

-- instance OEIS 60036 where
--   oeis = tablList @60036
-- instance Table 60036 where
--   rowCol = rowCol_off @60036 @2 @1
--   rowT = rowT_off @60036 @2
--   tabl = map init $ tail (tabl @48152)

-- instance OEIS 60054 where
--   oeis = -1 : map (numerator . sum) (tail $ zipWith (zipWith (%))
--      (zipWith (map . (*)) (oeis @142) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 60109 where
--   oeisIx n = if n == 0 then 22222 else read (conv n) :: Integer where
--      conv 0 = []
--      conv x = conv x' ++ mCode !! d where (x', d) = divMod x 10
--      mCode = map ('0' :) (mc ++ (reverse $ init $ tail $ map reverse mc))
--      mc = zipWith (++) (inits "111111") (tails "22222")

-- instance OEIS 60110 where
--   oeisIx = t . (oeisIx @60109) where
--      t 0 = 0
--      t n = if n == 0 then 0 else 3 * t n' + d  where (n', d) = divMod n 10

-- instance OEIS 60142 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @60142) !! n
--   oeis = 0 : f (singleton 1) where
--      f s = x : f (insert (4 * x) $ insert (2 * x + 1) s') where
--          (x, s') = deleteFindMin s

-- instance OEIS 60226 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @312) n - n * (oeisIx @312) (n - 1)

-- instance OEIS 60264 where
--   oeisIx = (oeisIx @151800) . (* 2)

-- instance OEIS 60265 where
--   oeisIx = (oeisIx @7917) . (* 2)

-- instance OEIS 60278 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ filter ((== 0) . (oeisIx @10051)) $ tail $ (rowT @27751) n

-- instance OEIS 60308 where
--   oeisIx = (oeisIx @7917) . (oeisIx @5843)

-- instance OEIS 60324 where
--   oeisIx n = head [q | q <- (oeis @40), (oeisIx @10051)' (n * (q + 1) - 1) == 1]

-- instance OEIS 60355 where
--   oeis = map (oeisIx @1694) $ filter ((== 1) . (oeisIx @76446)) [1..]

-- instance OEIS 60372 where
--   oeisIx n = (oeisIx n + n) `div` 2

-- instance OEIS 60373 where
--   oeisIx n = (oeisIx @60372) n - n

-- instance OEIS 60374 where
--   oeisIx n = f $ dropWhile (< n) (oeis @5836) where
--      f (p:ps) | (oeisIx @39966) (p-n) == 1 && (oeisIx @39966) (2*p-n) == 1 = 2*p - n
--               | otherwise                                  = f ps

-- instance OEIS 60381 where
--   oeisIx n = (oeisIx @98012) (2 * n - 1) n

-- instance OEIS 60417 where
--   oeisIx = genericLength . nub . show . (oeisIx @40)

-- instance OEIS 60418 where
--   oeisIx = (oeisIx @54055) . (oeisIx @40)

-- instance OEIS 60432 where
--   oeisIx n = sum $ zipWith (*) [n,n - 1..1] (oeis @10054)

-- instance OEIS 60441 where
--   oeis = tablList @60441
-- instance Table 60441 where
--   rowCol = rowCol_off @60441 @1 @1
--   rowT   = rowT_off @60441 @1
--   tabf = [0] : [1] : [1] : map (rowT @27746) (drop 3 (oeis @45))

-- instance OEIS 60442 where
--   oeis = tablList @60442
-- instance Table 60442 where
--   tabf = [0] : [1] : [1] : map (rowT @27748) (drop 3 (oeis @45))

-- instance OEIS 60448 where
--   oeisIx n = genericLength [us | let ds = (rowT @27750) n,
--                            us <- init $ tail $ subsequences ds,
--                            let vs = ds \\ us, head us < head vs,
--                            product us `mod` product vs == 0] + 1

-- instance OEIS 60476 where
--   oeis = filter ((== 0) . (oeisIx @10051)' . (+ 1) . (oeisIx @51903)) [1..]

-- instance OEIS 60547 where
--   oeisIx = (2 ^) . (oeisIx @8611) . (subtract 1)
--   oeis = f [2,1,2] where f xs = xs ++ f (map (* 2) xs)

-- instance OEIS 60640 where
--   oeisIx n = sum [d * (oeisIx @5) d | d <- (rowT @27750) n]

-- instance OEIS 60646 where
--   oeisIx n = (fromJust $ findIndex ((n+1) <) (oeis @14688)) + 1

-- instance OEIS 60652 where
--   oeis = filter h [1..] where
--      h x = any (> 2) (map snd pfs) || any (== 1) pks where
--        pks = [p ^ k `mod` q | (p,e) <- pfs, q <- map fst pfs, k <- [1..e]]
--        pfs = zip (oeisIx_row x) (oeisIx_row x)

-- instance OEIS 60680 where
--   oeisIx = minimum . (rowT @193829)

-- instance OEIS 60681 where
--   oeisIx n = div n p * (p - 1) where p = (oeisIx @20639) n

-- instance OEIS 60682 where
--   oeisIx = genericLength . nub . (rowT @193829)

-- instance OEIS 60683 where
--   oeis = 1 : filter (\x -> (oeisIx @60682) x == (oeisIx @5)' x - 1) [2..]

-- instance OEIS 60684 where
--   oeisIx = minimum . (rowT @193829) . (+ 1) . (* 2)

-- instance OEIS 60687 where
--   oeis = filter ((== 1) . (oeisIx @46660)) [1..]

-- instance OEIS 60715 where
--   oeisIx n = sum $ map (oeisIx @10051) [n+1..2*n - 1]

-- instance OEIS 60747 where
--   oeisIx = subtract 1 . (* 2)
--   oeis = [-1, 1 ..]

-- instance OEIS 60756 where
--   oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @60715)) [0..]

-- instance OEIS 60765 where
--   oeis = filter
--   (\x -> sort (nub $ (rowT @193829) x) `subset` (rowT @27750)' x) [1..]

-- instance OEIS 60819 where
--   oeisIx n = n `div` (oeisIx @109008) n

-- instance OEIS 60837 where
--   oeisIx n = (oeisIx n ^ 2) *
--      product (zipWith (^) (oeisIx_row m)
--                           (map ((subtract 1) . (* 2)) (oeisIx_row m)))
--      where m = (oeisIx @20653) n

-- instance OEIS 60857 where
--   oeis = 1 : f [1] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f (xs ++ ys) where
--             ys = concat $ transpose [map length zss, map head zss]
--             zss = group $ sort xs

-- instance OEIS 60901 where
--   oeisIx = (oeisIx @38500) . (oeisIx @45)

-- instance OEIS 60968 where
--   oeisIx 1 = 1
--   oeisIx n = (if p == 2 then (if e == 1 then 2 else 2^ (e+1)) else 1) *
--      (product $ zipWith (*) (map (\q -> q - 2 + mod q 4) ps'')
--                             (zipWith (^) ps'' (map (subtract 1) es'')))
--      where (ps'', es'') = if p == 2 then (ps, es) else (ps', es')
--            ps'@ (p:ps) = (rowT @27748) n; es'@ (e:es) = (rowT @124010) n

-- instance OEIS 60979 where
--   oeis = filter (\x -> let digs = map (read . return) $ show x in
--                                evens digs /= odds digs) [11, 22 ..]
--      where evens [] = 0; evens [x] = x; evens (x:_:xs) = x + evens xs
--            odds [] = 0; odds [x] = 0; odds (_:x:xs) = x + odds xs

-- instance OEIS 61020 where
--   oeisIx = sum . map (oeisIx @61019) . (rowT @27750)

-- instance OEIS 61205 where
--   oeisIx n = (oeisIx @4086) n * n

-- instance OEIS 61214 where
--   oeis = f (oeis @40) where
--      f (p:ps'@ (p':ps)) = (product [p+1..p'-1]) : f ps'

-- instance OEIS 61227 where
--   oeisIx n = p + (oeisIx @4086) p  where p = (oeisIx @40) n

-- instance OEIS 61228 where
--   oeisIx n = n + (oeisIx @20639) n

-- instance OEIS 61258 where
--   oeisIx n = sum $ zipWith (*) ds $ map (oeisIx @2322) ds
--               where ds = (rowT @27750) n

-- instance OEIS 61259 where
--   oeisIx n = sum $ zipWith (*) divs $ map (oeisIx @41) divs
--               where divs = (rowT @27750)' n

-- instance OEIS 61282 where
--   c i = if i `mod` 3 == 0 then i `div` 3 else i - 1
--   b 0 foldCount = foldCount
--   b sheetCount foldCount = b (c sheetCount) (foldCount + 1)
--   oeisIx n = b n 0

-- instance OEIS 61313 where
--   oeisIx n = fst $ until ((== 1) . snd) (\ (u, v) -> (u + 1, f v)) (0, n)
--      where f n = if r == 0 then n' else n + 1  where (n', r) = divMod n 2

-- instance OEIS 61338 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @6519) n + (oeisIx @120) n - 1

-- instance OEIS 61357 where
--   oeisIx n = sum $
--      zipWith (\u v -> (oeisIx @10051) u * (oeisIx @10051) v) [n+1..] $ reverse [1..n - 1]

-- instance OEIS 61373 where
--   oeisIx 1 = 1
--   oeisIx n = genericIndex (oeis @61373) (n - 1)
--   oeis = 1 : f 2 where
--      f x | x == spf  = 1 + (oeisIx @61373) (spf - 1) : f (x + 1)
--          | otherwise = (oeisIx @61373) spf + (oeisIx @61373) (x `div` spf) : f (x + 1)
--          where spf = (oeisIx @20639) x

-- instance OEIS 61383 where
--   oeis = filter (\x -> mod (oeisIx @7953 x) (oeisIx @55642 x) == 0) [0..]

-- instance OEIS 61389 where
--   oeisIx = product . map ((+ 1) . (oeisIx @10) . fi) . (rowT @124010) . succ

-- instance OEIS 61394 where
--   oeisIx = fromJust . (`elemIndex` (oeis @2110)) . (oeisIx @247451)

-- instance OEIS 61395 where
--   oeisIx = (oeisIx @49084) . (oeisIx @6530)

-- instance OEIS 61417 where
--   oeisIx = sum . (rowT @47917)

-- instance OEIS 61467 where
--   oeisIx 0 = 0
--   oeisIx n = mod (max n n') (min n n') where n' = (oeisIx @4086) n

-- instance OEIS 61470 where
--   oeis = filter ((== 1) . (oeisIx @225693)) [0..]

-- instance OEIS 61509 where
--   oeisIx n = product $ zipWith (^)
--     (oeis @40) (map digitToInt $ filter (/= '0') $ show n)

-- instance OEIS 61601 where
--   oeisIx n = if n <= 9 then 9 - n else 10 * ad n' + 9 - d
--               where (n',d) = divMod n 10
--                     ad = undefined

-- instance OEIS 61673 where
--   oeis = filter bothComp [4,6..] where
--      bothComp n = (1 - (oeisIx @10051) (n - 1)) * (1 - (oeisIx @10051) (n+1)) > 0

-- instance OEIS 61674 where
--   oeisIx n = until ((== 1) . (oeisIx @136522) . (oeisIx @4151) . (* n)) (+ 1) 1

-- instance OEIS 61681 where
--   oeis = iterate (oeisIx @182324) 1

-- instance OEIS 61775 where
--   oeis = 1 : g 2 where
--      g x = y : g (x + 1) where
--         y = if t > 0 then (oeisIx @61775) t + 1 else (oeisIx @61775) u + (oeisIx @61775) v - 1
--             where t = (oeisIx @49084) x; u = (oeisIx @20639) x; v = x `div` u

-- instance OEIS 61797 where
--   oeisIx 0 = 1
--   oeisIx n = head [k | k <- [1..], let x = k * n,
--                    all (`elem` "02468") $ show x, (oeisIx @136522) (oeisIx x) == 1]

-- instance OEIS 61827 where
--   oeisIx n =
--      p n (map digitToInt $ nub $ sort $ filter (/= '0') $ show n) where
--         p _ []        = 0
--         p 0 _         = 1
--         p m ds'@ (d:ds)
--           | m < d     = 0
--           | otherwise = p (m - d) ds' + p m ds

-- instance OEIS 61857 where
--   oeis = tablList @61857
-- instance Table 61857 where
--   rowCol n k = length [ ()| i <- [2..n], j <- [1..i-1], mod (i + j) k == 0]
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @61857) [1..]

-- instance OEIS 61909 where
--   oeis = filter (\x -> (oeisIx @4086) (x^2) == (oeisIx x)^2) [0..]

-- instance OEIS 61984 where
--   oeis = 0 : map (+ 1) (zipWith (+)
--      (map (oeisIx . (`div` 2)) [1..]) (map (oeisIx . (`div` 3)) [1..]))

-- instance OEIS 61985 where
--   oeis = f (-1) (oeis @61984) where
--      f u (v:vs) = if v == u then f u vs else v : f v vs

-- instance OEIS 61987 where
--   oeis = map length $ group (oeis @61984)

-- instance OEIS 62010 where
--   oeis = filter f [1..] where
--      f x = any (== 0) $ map (mod x) lower where
--          lower = map bas [1 + (oeisIx @54055) x .. 9]
--          bas b = foldl (\v d -> b*v + d) 0 bas10
--          bas10 = reverse $ unfoldr dig x where
--             dig n = if n== 0 then Nothing else Just $ swap $ divMod n 10

-- instance OEIS 62039 where
--   oeis = 1 : f 1 0 where
--      f x n | x > n     = (x-n) : f (x-n) (n+1)
--            | otherwise =    x' : f x' (n+1) where x' = x + (oeisIx @62039) x

-- instance OEIS 62050 where
--   oeisIx n = if n < 2 then n else 2 * (oeisIx @62050) n' + m - 1
--               where (n',m) = divMod n 2

-- instance OEIS 62052 where
--   oeis = map (+ 1) $ elemIndices 2 (oeis @78719)

-- instance OEIS 62053 where
--   oeis = map (+ 1) $ elemIndices 3 (oeis @78719)

-- instance OEIS 62054 where
--   oeis = map (+ 1) $ elemIndices 4 (oeis @78719)

-- instance OEIS 62055 where
--   oeis = map (+ 1) $ elemIndices 5 (oeis @78719)

-- instance OEIS 62056 where
--   oeis = map (+ 1) $ elemIndices 6 (oeis @78719)

-- instance OEIS 62057 where
--   oeis = map (+ 1) $ elemIndices 7 (oeis @78719)

-- instance OEIS 62058 where
--   oeis = map (+ 1) $ elemIndices 8 (oeis @78719)

-- instance OEIS 62059 where
--   oeis = map (+ 1) $ elemIndices 9 (oeis @78719)

-- instance OEIS 62060 where
--   oeis = map (+ 1) $ elemIndices 10 (oeis @78719)

-- instance OEIS 62115 where
--   oeis = filter ((== 0) . (oeisIx @39997)) (oeis @84984)

-- instance OEIS 62119 where
--   oeisIx n = (n - 1) * (oeisIx @142) n

-- instance OEIS 62161 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ cycle [0,1]

-- instance OEIS 62162 where
--   oeisIx = abs . sum . (rowT @247453)

-- instance OEIS 62173 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 (n - 1) n

-- instance OEIS 62251 where
--   oeisIx n = (oeisIx n + 1) * n - 1

-- instance OEIS 62272 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ cycle [1,0]

-- instance OEIS 62279 where
--   oeisIx 0 = 0
--   oeisIx n = until ((== 1) . (oeisIx @136522) . (oeisIx @4151)) (+ n) n

-- instance OEIS 62285 where
--   oeis = filter (even . (oeisIx @30)) (oeis @30141)

-- instance OEIS 62289 where
--   oeis = 2 : g 2 where
--      g n = nM n : g (n+1)
--      nM k = maximum $ map (\i -> i + min i (oeisIx $ k-i+1)) [2..k]
--

-- instance OEIS 62293 where
--   oeisIx 0 = 0
--   oeisIx n = head [x | x <- map (* n) [1..],
--                    all (`elem` "02468") $ show x, (oeisIx @136522) (oeisIx x) == 1]

-- instance OEIS 62320 where
--   oeisIx = (^ 2) . (oeisIx @13929)

-- instance OEIS 62323 where
--   oeis = tablList @62323
-- instance Table 62323 where
--   tabl = map fst $ iterate f ([1], [0,1]) where
--      f (us, vs) = (vs, ws) where
--        ws = (zipWith (+) (us ++ [0]) (map (* v) vs)) ++ [1]
--             where v = last (init vs) + 1

-- instance OEIS 62326 where
--   oeisIx = (oeisIx @40) . (oeisIx @137291)
--   oeis = map (oeisIx . (+ 1)) $
--                  elemIndices 1 $ map (oeisIx @10051)' (oeis @49001)

-- instance OEIS 62327 where
--   oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
--      f 2 e                  = 2 * e + 1
--      f p e | p `mod` 4 == 1 = (e + 1) ^ 2
--            | otherwise      = e + 1

-- instance OEIS 62332 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @208259)

-- instance OEIS 62373 where
--   oeis = filter ((== 2) . (oeisIx @34380)) [1..]

-- instance OEIS 62401 where
--   oeisIx = (oeisIx @10) . (oeisIx @203)

-- instance OEIS 62402 where
--   oeisIx = (oeisIx @203) . (oeisIx @10)

-- instance OEIS 62503 where
--   oeisIx = (oeisIx @290) . (oeisIx @5117)

-- instance OEIS 62509 where
--   oeisIx n = n ^ (oeisIx @1221) n

-- instance OEIS 62515 where
--   primes :: [Integer]
--   primes = 2 : 3 : filter (\a -> all (not . divides a) (takeWhile (\x -> x <= a `div` 2) primes)) [4..]
--       where
--     divides a b = a `mod` b == 0
--   primorials :: [Integer]
--   primorials = map product $ inits primes
--   partitions :: [[Integer]]
--   partitions = concat $ map (partitions_of_n) [0..]
--   partitions_of_n :: Integer -> [[Integer]]
--   partitions_of_n n = partitions_at_most n n
--   partitions_at_most :: Integer -> Integer -> [[Integer]]
--   partitions_at_most _ 0 = [[]]
--   partitions_at_most 0 _ = []
--   partitions_at_most m n = concat $ map (\k -> map ([k] ++) (partitions_at_most k (n-k))) ( reverse [1.. (min m n)])
--   oeisIx :: [Integer]
--   oeisIx = map primorial_signature partitions
--       where
--     primorial_signature p = product $ map ((drop 1 primorials) !!) (map fi p)

-- instance OEIS 62537 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ map (+ 1) $
--      zipWith ((+) `on` (oeisIx @62537)) (map (oeisIx @49084) $ (rowT @27748) n) (oeisIx_row n)

-- instance OEIS 62550 where
--   oeisIx 0 = 0
--   oeisIx n = sum $ (rowT @13942) n

-- instance OEIS 62584 where
--   oeisIx n = head [p | p <- (oeis @40), show n `isInfixOf` show p]

-- instance OEIS 62634 where
--   oeis = filter
--      (and . map ((elem '1') . show) . (rowT @27750)) (oeis @11531)

-- instance OEIS 62682 where
--   import Data.Set (singleton, deleteFindMin, insert, Set)
--   oeisIx n = (oeis @62682) !! (n - 1)
--   oeis = f (singleton (1 + 2^3, (1, 2))) 0 0 where
--      f s z z' = if y == z && z' /= z then y : f s'' y z else f s'' y z
--                 where s'' = (insert (y', (i, j')) $
--                              insert (y' - i ^ 3 , (i + 1, j')) s')
--                       y' = y + j' ^ 3; j' = j + 1
--                       ((y, (i, j)), s') = deleteFindMin s

-- instance OEIS 62715 where
--   oeis = tablList @62715
-- instance Table 62715 where
--   tabl = 1 : zipWith (:) (oeis @12) (tabl @38207)

-- instance OEIS 62730 where
--   oeis =  filter f $ [3..] where
--      f x = not $ all null $ zipWith
--            (\us (v:vs) -> map (v -) us `intersect` map (subtract v) vs)
--            (tail $ init $ inits bns) (tail $ init $ tails bns)
--            where bns = (rowT @34868) x

-- instance OEIS 62756 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @62756) n' + m `mod` 2 where (n',m) = divMod n 3

-- instance OEIS 62759 where
--   oeisIx n = (oeisIx @7947) n ^ (oeisIx @51904) n

-- instance OEIS 62789 where
--   oeisIx n = gcd n (phi * (phi + 1)) where phi = (oeisIx @10) n

-- instance OEIS 62799 where
--   oeisIx = sum . map (oeisIx @1221) . (rowT @27750)

-- instance OEIS 62825 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ (rowT @163870) (n - 1)

-- instance OEIS 62974 where
--   oeis = map (+ 1) $ findIndices (< 0) $
--      zipWith (-) (tail (oeis @1221)) $ map (* 2) (oeis @1221)

-- instance OEIS 62980 where
--   oeis = 1 : 5 : f 2 [5,1] where
--      f u vs'@ (v:vs) = w : f (u + 1) (w : vs') where
--        w = 6 * u * v + sum (zipWith (*) vs_ $ reverse vs_)
--        vs_ = init vs

-- instance OEIS 62992 where
--   oeisIx = sum . (rowT @234950)

-- instance OEIS 63007 where
--   oeis = tablList @63007
-- instance Table 63007 where
--   tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @46899)

-- instance OEIS 63051 where
--   oeisIx n = (oeis @33651) !! n
--   oeis = iterate (oeisIx @56964) 9

-- instance OEIS 63054 where
--   oeis = iterate (oeisIx @56964) 1997

-- instance OEIS 63057 where
--   oeis = iterate (oeisIx @56964) 7059

-- instance OEIS 63060 where
--   oeis = iterate (oeisIx @56964) 10553

-- instance OEIS 63063 where
--   oeis = iterate (oeisIx @56964) 10563

-- instance OEIS 63108 where
--   oeis = iterate (oeisIx @63114) 1

-- instance OEIS 63114 where
--   oeisIx n = n + (oeisIx @51801) n

-- instance OEIS 63171 where
--   import Data.Set (singleton, deleteFindMin, union, fromList)
--   newtype Word = Word String deriving (Eq, Show, Read)
--   instance Ord Word where
--      Word us <= Word vs | length us == length vs = us <= vs
--                         | otherwise              = length us <= length vs
--   oeisIx n = (oeis @63171) !! (n - 1)
--   oeis = dyck $ singleton (Word "S") where
--      dyck s | null ws   = (read w :: Integer) : dyck s'
--             | otherwise = dyck $ union s' (fromList $ concatMap gen ws)
--             where ws = filter ((== 'S') . head . snd) $
--                               map (`splitAt` w) [0..length w - 1]
--                   (Word w, s') = deleteFindMin s
--      gen (us,vs) = map (Word . (us ++) . (++ tail vs)) ["10", "1S0", "SS"]

-- instance OEIS 63232 where
--   oeis = 5 : 16 : 24 : 36 : zipWith3 (((-) .) . (+))
--      (drop 3 (oeis @63232)) (drop 2 (oeis @63232)) (tail (oeis @63232))

-- instance OEIS 63453 where
--   oeisIx = product . map ((1 -) . (^ 3)) . (rowT @27748)

-- instance OEIS 63510 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @63510) (oeisIx n) + 1

-- instance OEIS 63574 where
--   oeisIx n = fst $ until ((== 1) . flip mod 4 . snd)
--                           (\ (u, v) -> (u + 1, (oeisIx @7494) v)) (0, n)

-- instance OEIS 63637 where
--   oeis = filter ((== 1) . (oeisIx @64911) . (+ 2)) (oeis @40)

-- instance OEIS 63638 where
--   oeis = map (+ 2) $ filter ((== 1) . (oeisIx @64911)) (oeis @40976)

-- instance OEIS 63639 where
--   oeis = [p | p <- (oeis @40), (oeisIx @1222) (p+1) == 3]

-- instance OEIS 63733 where
--   oeis = 1 : f 0 [1] where
--      f x ys@ (y:_) | u > 0 && u `notElem` ys = u : f (x + 1) (u : ys)
--                   | otherwise               = v : f (x + 1) (v : ys)
--                   where u = y - x; v = x + y

-- instance OEIS 63759 where
--   oeis = concat $ transpose [oeis, (oeis @7283)]

-- instance OEIS 63776 where
--   oeisIx n = (oeisIx @53636) n `div` n

-- instance OEIS 63905 where
--   oeis =
--      concat $ zipWith ($) (map replicate (oeis @40)) (oeis @40)

-- instance OEIS 63919 where
--   oeisIx 1 = 1
--   oeisIx n = sum $ init $ (rowT @77610) n

-- instance OEIS 63936 where
--   oeis = map (+ 1) $
--                  findIndices (\x -> x > 1 && (oeisIx @10052) x == 1) (oeis @34460)

-- instance OEIS 63937 where
--   oeis = map (+ 2) $
--                  findIndices ((== 1) . (oeisIx @10052)) $ tail (oeis @34448)

-- instance OEIS 63947 where
--   oeis = filter ((== 1) . denominator . hm . (rowT @77609)) [1..]
--      where hm xs = genericLength xs / sum (map (recip . fi) xs)

-- instance OEIS 63962 where
--   oeisIx n = genericLength [p | p <- (rowT @27748) n, p ^ 2 <= n]

-- instance OEIS 63967 where
--   oeisIx_tabl = [1] : [1,1] : f [1] [1,1] where
--      f us vs = ws : f vs ws where
--        ws = zipWith (+) ([0] ++ us ++ [0]) $
--             zipWith (+) (us ++ [0,0]) $ zipWith (+) ([0] ++ vs) (vs ++ [0])

-- instance OEIS 63982 where
--   oeis = f [] $ tail (oeis @225) where
--      f us (v:vs) = (length ds) : f (v:us) vs where
--        ds = [d | d <- (rowT @27750) v, all ((== 1). (gcd d)) us]

-- instance OEIS 63993 where
--   oeisIx n = genericLength [ () | let ts = takeWhile (< n) $ tail (oeis @217),
--                       x <- ts, y <- takeWhile (<= x) ts,
--                       let z = n - x - y, 0 < z, z <= y, (oeisIx @10054) z == 1]

-- instance OEIS 63994 where
--   oeisIx n = product $ map (gcd (n - 1) . subtract 1) $ (rowT @27748) n

-- instance OEIS 63995 where
--   oeis = tablList @63995
-- instance Table 63995 where
--   rowCol n k = (tabf @63995) !! (n - 1) !! (n-1+k)
--   rowT   = rowT_off @63995 @1
--   tabf = [[1], [1, 0, 1]] ++ (map
--      (\rs -> [1, 0] ++ (init $ tail $ rs) ++ [0, 1]) $ drop 2 $ map
--      (map length . group . sort . map rank) $ tail pss) where
--         rank ps = maximum ps - length ps
--         pss = [] : map (\u -> [u] : [v : ps | v <- [1..u],
--                                ps <- pss !! (u - v), v <= head ps]) [1..]

-- instance OEIS 64097 where
--   oeis = 0 : f 2 where
--      f x | x == spf  = 1 + (oeisIx @64097) (spf - 1) : f (x + 1)
--          | otherwise = (oeisIx @64097) spf + (oeisIx @64097) (x `div` spf) : f (x + 1)
--          where spf = (oeisIx @20639) x

-- instance OEIS 64113 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @36263)

-- instance OEIS 64150 where
--   oeis = filter (\x -> x `mod` (oeisIx @53735) x == 0) [1..]

-- instance OEIS 64236 where
--   oeisIx = genericLength . show . (oeisIx @1042)

-- instance OEIS 64272 where
--   oeisIx n = sum $
--      map (oeisIx . (n -)) $ takeWhile (< n) $ tail (oeis @290)

-- instance OEIS 64275 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @32447))

-- instance OEIS 64283 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @64272))

-- instance OEIS 64364 where
--   oeis = tablList @64364
-- instance Table 64364 where
--   rowCol = rowCol_off @64364 @1 @1
--   rowT   = rowT_off @64364 @1
--   tabf = [1] : tail (f 1 [] 1 (map (oeisIx @792) [2..])) where
--      f k pqs v (w:ws) = (map snd pqs') :
--        f (k + 1) (union pqs'' (zip (map (oeisIx @1414) us) us )) w ws where
--          us = [v + 1 .. w]
--          (pqs', pqs'') = partition ((== k) . fst) pqs
--   oeis = concat (tabf @64364)

-- instance OEIS 64365 where
--   import Data.Set (singleton, notMember, insert)
--   oeisIx n = (oeis @64365) !! n
--   oeis = 0 : f 0 (oeis @40) (singleton 0) where
--      f x (p:ps) s | x' > 0 && x' `notMember` s = x' : f x' ps (insert x' s)
--                   | otherwise                  = xp : f xp ps (insert xp s)
--                   where x' = x - p; xp = x + p

-- instance OEIS 64372 where
--   oeisIx 0 = 1
--   oeisIx n = sum $ map (oeisIx @64372) $ (rowT @124010 . succ) n

-- instance OEIS 64413 where
--   oeis = 1 : f 2 [2..] where
--      ekg x zs = f zs where
--          f (y:ys) = if gcd x y > 1 then y : ekg y (delete y zs) else f ys

-- instance OEIS 64418 where
--   oeis = 1 : 2 : 3 : 4 : (f 4 [5..]) where
--      f :: Integer -> [Integer] -> [Integer]
--      f x xs = m : (f m $ delete m xs) where
--         m = head $ dropWhile ((< 4) . (gcd x)) xs

-- instance OEIS 64426 where
--   oeis = zipWith (-) (tail (oeis @64413)) (oeis @64413)

-- instance OEIS 64427 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @720) (n - 1) + n

-- instance OEIS 64549 where
--   oeisIx n = (oeisIx @7947) n * n

-- instance OEIS 64553 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map ((+ 1) . (oeisIx @49084)) $ (rowT @27746) n

-- instance OEIS 64554 where
--   oeisIx = head . (rowT @80688)

-- instance OEIS 64555 where
--   oeisIx = last . (rowT @80688)

-- instance OEIS 64614 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map f $ (rowT @27746) n where
--      f 2 = 3; f 3 = 2; f p = p

-- instance OEIS 64649 where
--   oeisIx = sum . (rowT @47916)

-- instance OEIS 64654 where
--   oeis = map length $ group (oeis @195376)

-- instance OEIS 64664 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @64413))

-- instance OEIS 64672 where
--   oeis = 0 : 1 : f (drop 2 (oeis @196)) 1 1 (tail (oeis @64672))
--      where f (r:rs) r' u (v:vs)
--              | r == r' = (u + v) : f rs r u vs
--              | r /= r' = u' : f rs r u' (tail (oeis @64672))
--              where u' = (oeisIx @64672) $ fromInteger r

-- instance OEIS 64689 where
--   oeisIx = fromJust . (`elemIndex` (oeis @64672))

-- instance OEIS 64700 where
--   oeis = filter f [1..] where
--      f x = mdr > 0 && x `mod` mdr == 0 where mdr = (oeisIx @31347) x

-- instance OEIS 64702 where
--   oeis = filter (\x -> (oeisIx @10888) x == (oeisIx @31347) x) [1..]

-- instance OEIS 64745 where
--   oeisIx n = fromJust (elemIndex n (oeis @64736)) + 1

-- instance OEIS 64770 where
--   oeisIx :: Integer -> Integer
--   oeisIx = read . map (("0111222223" !!) . digitToInt) . show

-- instance OEIS 64771 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @65205)

-- instance OEIS 64787 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @53212))

-- instance OEIS 64800 where
--   oeisIx n = (oeisIx @1222) n + n

-- instance OEIS 64807 where
--   oeis = filter (\x -> x `mod` (oeisIx @10888) x == 0) [1..]

-- instance OEIS 64823 where
--   oeis = f (oeis @796) $ replicate 10 0 where
--      f (d:ds) cs = (v + 1) : f ds (us ++ (v + 1) : vs) where
--        (us, v:vs) = splitAt d cs

-- instance OEIS 64834 where
--   oeisIx n = sum $ take (length nds `div` 2) $
--                     map abs $ zipWith (-) nds $ reverse nds
--      where nds = (rowT @31298) n

-- instance OEIS 64847 where
--   oeis = 1 : f [1,1] where
--      f xs'@ (x:xs) = y : f (y : xs') where y = x * sum xs

-- instance OEIS 64861 where
--   oeis = tablList @64861
-- instance Table 64861 where
--   tabl = map fst $ iterate f ([1], 2) where
--   f (xs, z) = (zipWith (+) ([0] ++ map (* z) xs) (xs ++ [0]), 3 - z)

-- instance OEIS 64944 where
--   oeisIx = sum . zipWith (*) [1..] . (rowT @27750)'

-- instance OEIS 64945 where
--   oeisIx = sum . zipWith (*) [1..] . reverse . (rowT @27750)'

-- instance OEIS 64953 where
--   oeis = map (+ 1) $ findIndices even (oeis @64413)

-- instance OEIS 64955 where
--   oeis =
--      map ((+ 1) . fromJust . (`elemIndex` (oeis @64413))) (oeis @40)

-- instance OEIS 64956 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @64417)) + 1

-- instance OEIS 64957 where
--   oeis = map (+ 1) $ findIndices odd (oeis @64413)

-- instance OEIS 64959 where
--   oeis = map ((+ 1) . fromJust . (`elemIndex` (oeis @64419))) [1..]

-- instance OEIS 64986 where
--   oeisIx = p (tail (oeis @142)) where
--      p _          0             = 1
--      p fs'@ (f:fs) m | m < f     = 0
--                     | otherwise = p fs' (m - f) + p fs m

-- instance OEIS 64987 where
--   oeisIx n = (oeisIx @203) n * n

-- instance OEIS 64989 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (oeisIx . (oeisIx @49084)) $ (rowT @27746) n

-- instance OEIS 65003 where
--   oeis = elemIndices 0 $ map (oeisIx @214772) [0..43]

-- instance OEIS 65031 where
--   oeisIx n = f n  where
--      f x | x < 10    = 2 - x `mod` 2
--          | otherwise = 10 * (f x') + 2 - m `mod` 2
--          where (x',m) = divMod x 10

-- instance OEIS 65037 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @36552))

-- instance OEIS 65090 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @151763)

-- instance OEIS 65109 where
--   oeis = tablList @65109
-- instance Table 65109 where
--   tabl = iterate
--      (\row -> zipWith (-) (map (* 2) row ++ [0]) ([0] ++ row)) [1]

-- instance OEIS 65205 where
--   oeisIx n = p (oeisIx_row n) n where
--      p _      0 = 1
--      p []     _ = 0
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 65206 where
--   oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @56964)) (oeis @29742)

-- instance OEIS 65253 where
--   oeis = zipWith (+) (map ((* 10) . (subtract 1)) (oeis @64823)) (oeis @796)

-- instance OEIS 65254 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @65253))

-- instance OEIS 65297 where
--   oeis = 1 : f 1 (drop 2 (oeis @290)) where
--      f x (q:qs) | null (xs \\ sq) && sort xs /= sort sq = y : f y qs
--                 | otherwise                             = f x qs
--                 where y = (oeisIx @196) q; sq = show q; xs = show (x * x)

-- instance OEIS 65306 where
--   oeis = map (subtract 2) $ f (concat (tabl @65305)) [] where
--      f (x:xs) ys = if x `elem` ys then f xs ys else x : f xs (x:ys)

-- instance OEIS 65307 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @65306)) + 1

-- instance OEIS 65330 where
--   oeisIx = (oeisIx @38502) . (oeisIx @265)

-- instance OEIS 65331 where
--   oeisIx = f 2 1 where
--      f p y x | r == 0    = f p (y * p) x'
--              | otherwise = if p == 2 then f 3 y x else y
--              where (x', r) = divMod x p

-- instance OEIS 65333 where
--   oeisIx = fromEnum . (== 1) . (oeisIx @38502) . (oeisIx @265)

-- instance OEIS 65338 where
--   oeisIx 1 = 1
--   oeisIx n = (spf `mod` 4) * (oeisIx @65338) (n `div` spf) where spf = (oeisIx @20639) n

-- instance OEIS 65339 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength [x | x <- (rowT @27746) n, mod x 4 == 3]

-- instance OEIS 65342 where
--   oeis = tablList @65342
-- instance Table 65342 where
--   rowCol = rowCol_off @65342 @1 @1
--   rowT   = rowT_off   @65342 @1
--   tabl = zipWith (map . (+)) (oeis @40) $ tail $ inits (oeis @40)

-- instance OEIS 65350 where
--   oeis = zipWith mod (tail (oeis @984)) (drop 2 (oeis @290))

-- instance OEIS 65359 where
--   oeisIx 0 = 0
--   oeisIx n = - (oeisIx @65359) n' + m where (n', m) = divMod n 2

-- instance OEIS 65371 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (oeisIx . (oeisIx @49084)) $ (rowT @27746) n

-- instance OEIS 65380 where
--   oeis = filter f (oeis @40) where
--      f p = any ((== 1) . (oeisIx @10051) . (p -)) $ takeWhile (<= p) (oeis @79)

-- instance OEIS 65381 where
--   oeis = filter f (oeis @40) where
--      f p = all ((== 0) . (oeisIx @10051) . (p -)) $ takeWhile (<= p) (oeis @79)

-- instance OEIS 65383 where
--   oeisIx n = head $ dropWhile (< (oeisIx @217) n) (oeis @40)

-- instance OEIS 65428 where
--   oeis = filter f [1..] where
--      f x = all (== 0) $
--            map (oeisIx' . (`mod` x) . (oeisIx @290)) [oeisIx x .. x-1]

-- instance OEIS 65435 where
--   oeis = 2 : 3 : zipWith (+) xs (tail xs) where
--                  xs = map (oeisIx . fromInteger) (oeis @65435)

-- instance OEIS 65500 where
--   oeisIx n = (oeisIx @3418) n + n - signum n

-- instance OEIS 65515 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @961)

-- instance OEIS 65516 where
--   oeis = zipWith (-) (tail (oeis @1358)) (oeis @1358)

-- instance OEIS 65602 where
--   oeis = tablList @65602
-- instance Table 65602 where
--   rowCol n k = sum
--      [ (k-1+2*j) * (oeisIx @7318)' (2*n-k-1-2*j) (n - 1) `div` (2*n-k-1-2*j) |
--       j <- [0 .. div (n-k) 2]]
--   rowT n = map (oeisIx n) [2..n]
--   tabl = map (rowT @65602) [2..]

-- instance OEIS 65621 where
--   oeisIx (succ->fi->n) = fi do n `xor` 1 * (n - n .&. negate n) :: Integer

-- instance OEIS 65641 where
--   oeis = map (fromJust . (`elemIndex` (oeis @193582))) [1..]

-- instance OEIS 65642 where
--   oeisIx 1 = 1
--   oeisIx n = head [x | let rad = (oeisIx @7947) n, x <- [n+1..], (oeisIx @7947) x == rad]

-- instance OEIS 65648 where
--   oeis = f (0 : (oeis @33307)) $ take 10 $ repeat 1 where
--      f (d:ds) counts = y : f ds (xs ++ (y + 1) : ys) where
--                              (xs, y:ys) = splitAt d counts

-- instance OEIS 65649 where
--   oeis = zipWith (+)
--                  (map ((* 10) . subtract 1) (oeis @65648)) (0 : (oeis @33307))

-- instance OEIS 65650 where
--   oeisIx = fromJust . (`elemIndex` (oeis @65649))

-- instance OEIS 65730 where
--   oeisIx = (oeisIx @48760) . (oeisIx @40)

-- instance OEIS 65896 where
--   oeisIx = (oeisIx @65855) . (* 2)

-- instance OEIS 65941 where
--   oeis = tablList @65941
-- instance Table 65941 where
--   tabl = iterate (\row ->
--      zipWith (+) ([0] ++ row) (zipWith (*) (row ++ [0]) (oeis @59841))) [1]

-- instance OEIS 66028 where
--   oeisIx = maximum . filter ((== 1) . (oeisIx @10051)') .
--                       map sum . tail . subsequences . flip take (oeis @40)

-- instance OEIS 66032 where
--   oeisIx 1 1 = 1
--   oeisIx n k = fromEnum (n <= k) +
--      (sum $ map (\d -> (oeisIx @66032) (n `div` d) d) $
--                 takeWhile (<= k) $ tail $ (rowT @27751) n)
--   oeisIx_row n = map (oeisIx n) [1..n]
--   oeisIx_tabl = map (rowT @66032) [1..]

-- instance OEIS 66054 where
--   oeis = iterate (oeisIx @56964) 10583

-- instance Table 66099 where
--   oeisIx_tabf = map (rowT @66099) [1..]
--   oeisIx_row n = reverse $ (rowT @228351) n
-- instance OEIS 66099 where
--   oeis = tablList @66099

-- instance OEIS 66186 where
--   oeisIx = sum . concat . ps 1 where
--      ps _ 0 = [[]]
--      ps i j = [t:ts | t <- [i..j], ts <- ps t (j - t)]

-- instance OEIS 66195 where
--   oeisIx n = fromJust $ find ((== n) . (oeisIx @23416)) (oeis @40)

-- instance OEIS 66197 where
--   oeisIx n = (oeisIx @7947) $ (oeisIx @33286) n * (oeisIx @14688) n

-- instance OEIS 66246 where
--   oeis = unfoldr x (1, 1, (oeis @2808)) where
--      x (i, z, cs'@ (c:cs)) | i == c = Just (z, (i + 1, z + 1, cs))
--                           | i /= c = Just (0, (i + 1, z, cs'))

-- instance OEIS 66360 where
--   oeisIx n = genericLength [ (x,y,z) | x <- [1 .. (oeisIx @196) n],
--                                 y <- [x .. div n x],
--                                 z <- [y .. n - x*y],
--                                 x*y+ (x+y)*z == n, gcd (gcd x y) z == 1]

-- instance OEIS 66376 where
--   oeisIx :: Int -> Int
--   oeisIx n = genericLength [d | d <- [1..n - 1], any ((== n) . (orm d)) [1..n]] where
--      orm 1 v = v
--      orm u v = orm (shiftR u 1) (shiftL v 1) .|. if odd u then v else 0

-- instance OEIS 66400 where
--   oeisIx = genericLength . (rowT @245499)

-- instance OEIS 66401 where
--   oeisIx = (oeisIx @196) . (oeisIx @245530)

-- instance OEIS 66411 where
--   oeisIx 0 = 1
--   oeisIx n = genericLength $ nub $ map
--      apex [perm | perm <- permutations [0..n], head perm < last perm] where
--      apex = head . until ((== 1) . length)
--                          (\xs -> (zipWith (+) xs $ tail xs))

-- instance OEIS 66446 where
--   oeisIx = (oeisIx @217) . subtract 1 . (oeisIx @5)'

-- instance OEIS 66522 where
--   oeis = filter f [1..] where
--      f x = genericLength ds == maximum ds where ds = (rowT @161906) x

-- instance OEIS 66527 where
--   oeis = filter ((== 1) . (oeisIx @10054)) (oeis @7504)

-- instance OEIS 66638 where
--   oeisIx n = (oeisIx @7947) n ^ (oeisIx @51903) n

-- instance OEIS 66657 where
--   oeis = map numerator
--      (1 : (concat $ tail $ zipWith (\u vs -> map (% u) vs)
--                                    (oeis @66720) (inits (oeis @66720))))

-- instance OEIS 66658 where
--   oeis = map denominator
--      (1 : (concat $ tail $ zipWith (\u vs -> map (% u) vs)
--                                    (oeis @66720) (inits (oeis @66720))))

-- instance OEIS 66664 where
--   oeis = filter ((== 0) . (oeisIx @10051)) $ tail (oeis @66522)

-- instance OEIS 66681 where
--   oeis = filter ((== 0) . (oeisIx @10051)) (oeis @66680)

-- instance OEIS 66710 where
--   oeis = iterate (oeisIx @36839) 3

-- instance OEIS 66711 where
--   oeis = iterate (oeisIx @36839) 9

-- instance OEIS 66713 where
--   oeisIx = (oeisIx @36839) . (2 ^)

-- instance OEIS 66720 where
--   import qualified Data.Set as Set (null)
--   import Data.Set as Set (empty, insert, member)
--   oeisIx n = (oeis @66720) !! (n - 1)
--   oeis = f [] 1 empty where
--      f ps z s | Set.null s' = f ps (z + 1) s
--               | otherwise   = z : f (z:ps) (z + 1) s'
--        where s' = g (z:ps) s
--              g []     s                      = s
--              g (x:qs) s | (z * x) `member` s = empty
--                         | otherwise          = g qs $ insert (z * x) s

-- instance OEIS 66721 where
--   oeis = filter ((== 0) . (oeisIx @10051)') (oeis @66720)

-- instance OEIS 66729 where
--   oeisIx n = if pds == [1] then n else product pds
--               where pds = (rowT @27751) n

-- instance OEIS 66822 where
--   oeisIx = flip (oeisIx @38622) 3 . (+ 3)

-- instance OEIS 66825 where
--   oeis = 1 : f 1 (drop 2 (oeis @290)) where
--      f x (q:qs) | all (`elem` show q) xs = y : f y qs
--                 | otherwise              = f x qs
--                 where y = (oeisIx @196) q; xs = show (x * x)

-- instance OEIS 66839 where
--   oeisIx = sum . (rowT @161906)

-- instance OEIS 66938 where
--   oeis = map (oeisIx @40) $ filter ((> 0) . (oeisIx @67432)) [1..]

-- instance OEIS 66955 where
--   oeisIx n = genericLength [ (x,y,z) | x <- [1 .. (oeisIx @196) (div n 3)],
--                                 y <- [x .. div n x],
--                                 z <- [y .. div (n - x*y) (x + y)],
--                                 x * y + (x + y) * z == n]



-- instance OEIS 66990 where
--   oeisIx n = product $ zipWith (^)
--              (oeisIx_row n) (map ((2 -) . (`mod` 2)) $ (rowT @124010) n)

-- instance OEIS 67018 where
--   oeis =  [1,4,3,2] ++ f [2,3,4,1] where
--     f xs = mexi : f (mexi : xs) where
--       mexi = head $ [0..] \\ zipWith xor xs (reverse xs) :: Integer

-- instance OEIS 67046 where
--   oeisIx = (`div` 6) . (oeisIx @33931)

-- instance OEIS 67078 where
--   oeis = scanl (+) 1 (oeis @142)

-- instance OEIS 67080 where
--   oeisIx n = if n <= 9 then n else n * (oeisIx @67080) (n `div` 10)

-- instance OEIS 67109 where
--   oeisIx n = sum $
--      map (fromEnum . (show n `isPrefixOf`)) (tails $ show $ (oeisIx @142) n)

-- instance OEIS 67139 where
--   oeis = 1 : map (+ 1) (elemIndices 1 (oeis @66376))

-- instance OEIS 67255 where
--   oeis = tablList @67255
-- instance Table 67255 where
--   rowCol = rowCol_off @67255 @1 @1
--   rowT 1 = [0]
--   rowT n = f n (oeis @40) where
--      f 1 _      = []
--      f u (p:ps) = g u 0 where
--        g v e = if m == 0 then g v' (e + 1) else e : f v ps
--                where (v',m) = divMod v p
--   tabf = map (rowT @67255) [1..]

-- instance OEIS 67266 where
--   oeis = filter (\x -> (oeisIx @1221) x == (oeisIx @2321) x) [1..]

-- instance OEIS 67391 where
--   oeisIx n | n <= 2    = 1
--             | otherwise = foldl lcm 1 $ (rowT @173540) n

-- instance OEIS 67398 where
--   oeisIx :: Integer -> Integer
--   oeisIx 0 = 0
--   oeisIx n = orm n n where
--      orm 1 v = v
--      orm u v = orm (shiftR u 1) (shiftL v 1) .|. if odd u then v else 0



-- instance OEIS 67434 where
--   oeisIx = (oeisIx @1221) . (oeisIx @984)

-- instance OEIS 67458 where
--   oeisIx (succ->n) = f 0 n where
--      f y 0 = y
--      f y x = if d == 0 then f y x' else f (y + mod n d) x'
--              where (x', d) = divMod x 10

-- instance OEIS 67513 where
--   oeisIx = sum . map (oeisIx . (+ 1)) . (rowT @27750)

-- instance OEIS 67581 where
--   oeis = 1 : f 1 [2..] where
--      f u vs = v : f v (delete v vs)
--        where v : _ = filter (null . (intersect `on` show) u) vs

-- instance OEIS 67599 where
--   oeisIx n = read $ foldl1 (++) $
--      zipWith ((++) `on` show) (oeisIx_row n) (oeisIx_row n) :: Integer

-- instance OEIS 67611 where
--   oeis = map (`div` 6) $
--      filter (\x -> (oeisIx @10051)' (x-1) == 0 || (oeisIx @10051)' (x+1) == 0) [6,12..]

-- instance OEIS 67722 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10052) (n * (n + k)) == 1]

-- instance OEIS 67747 where
--   oeis = concat $ transpose [oeis, (oeis @2808)]

-- instance OEIS 67815 where
--   oeisIx n = gcd n $ (oeisIx @196) n

-- instance OEIS 67824 where
--   oeisIx n = 1 + sum (map (oeisIx @67824) [d | d <- [1..n - 1], mod n d == 0])

-- instance OEIS 67872 where
--   oeisIx n = (until ((== 1) . (oeisIx @10052) . (+ 1)) (+ nn) nn) `div` nn
--               where nn = n ^ 2

-- instance OEIS 67953 where
--   oeisIx n = p [1..n] $ (oeisIx @40) n where
--      p _  0 = 1
--      p [] _ = 0
--      p (k:ks) m | m < k = 0 | otherwise = p ks (m - k) + p ks m

-- instance OEIS 67962 where
--   oeis = 1 : zipWith (*) (oeis @67962) (drop 2 (oeis @1654))

-- instance OEIS 67970 where
--   oeis = zipWith (-) (tail (oeis @14076)) (oeis @14076)

-- instance OEIS 68050 where
--   oeisIx n = genericLength [k | k <- [1..n], (oeisIx @10051) (n `div` k) == 1]

-- instance OEIS 68068 where
--   oeisIx = genericLength . filter odd . (rowT @77610)

-- instance OEIS 68074 where
--   oeisIx n | odd n     = - (oeisIx @48691) n
--             | otherwise = 2 * (oeisIx @48691) (n `div` 2) - (oeisIx @48691) n

-- instance OEIS 68101 where
--   oeisIx = sum . map (oeisIx @8683) . (rowT @161906)

-- instance OEIS 68106 where
--   oeis = tablList @68106
-- instance Table 68106 where
--   tabl = map reverse (tabl @47920)

-- instance OEIS 68119 where
--   oeisIx n = fst $ until ((== 1) . denominator . snd)
--                           (\ (i, x) -> (i + 1, f x)) (0, fromInteger n + 1%4)
--      where f x = x * fi (ceiling x)

-- instance OEIS 68148 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @32981)

-- instance OEIS 68164 where
--   oeisIx n = head (filter isPrime (digitExtensions n))
--   digitExtensions n = filter (includes n) [0..]
--   includes n k = listIncludes (show n) (show k)
--   listIncludes [] _ = True
--   listIncludes (h:_) [] = False
--   listIncludes l1@ (h1:t1) (h2:t2) = if (h1 == h2) then (listIncludes t1 t2) else (listIncludes l1 t2)
--   isPrime 1 = False
--   isPrime n = not (hasDivisorAtLeast 2 n)
--   hasDivisorAtLeast k n = (k*k <= n) && (((n `rem` k) == 0) || (hasDivisorAtLeast (k+1) n))

-- instance OEIS 68191 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @86299)

-- instance OEIS 68310 where
--   oeisIx n = f 1 $ (rowT @27746) (n^2 - 1) where
--      f y [] = y
--      f y [p] = y*p
--      f y (p:ps'@ (p':ps)) | p == p' = f y ps
--                          | otherwise = f (y*p) ps'

-- instance OEIS 68312 where
--   oeisIx = (oeisIx @3415) . (oeisIx @217)

-- instance OEIS 68319 where
--   oeisIx n = if n <= spf ^ 2 then spf else (oeisIx @68319) $ spf + div n spf
--               where spf = (oeisIx @20639) n

-- instance OEIS 68328 where
--   oeisIx = (oeisIx @3415) . (oeisIx @5117)

-- instance OEIS 68336 where
--   oeis = 1 : f 1 where
--      f x = (1 + sum (map (oeisIx @68336) $ (rowT @27750) x)) : f (x + 1)

-- instance OEIS 68341 where
--   oeis = f 1 [] where
--      f x ms = (sum $ zipWith (*) ms' $ reverse ms') : f (x + 1) ms' where
--        ms' = (oeisIx x) : ms

-- instance OEIS 68346 where
--   oeisIx = (oeisIx @3415) . (oeisIx @3415)

-- instance OEIS 68395 where
--   oeis = zipWith (-) (oeis @40) (oeis @7605)

-- instance OEIS 68396 where
--   oeisIx n = p - (oeisIx @4086) p  where p = (oeisIx @40) n

-- instance OEIS 68494 where
--   oeisIx n = mod n $ (oeisIx @10) n

-- instance OEIS 68500 where
--   oeis = h 0 (oeis @4090) (oeis @45) where
--      h r (q:qs) (f:fs) = if q <= r then h r qs fs else f : h q qs fs

-- instance OEIS 68505 where
--   oeisIx n = foldr (\d v -> v * b + d) 0 dds where
--   b = maximum dds + 1
--   dds = (rowT @31298) n

-- instance OEIS 68522 where
--   oeisIx 0 = 0
--   oeisIx n = 10 * (oeisIx @68522) n' + m ^ 2  where (n', m) = divMod n 10

-- instance OEIS 68636 where
--   oeisIx n = min n $ (oeisIx @4086) n

-- instance OEIS 68637 where
--   oeisIx n = max n $ (oeisIx @4086) n

-- instance OEIS 68690 where
--   oeis = filter (all (`elem` "02468") . init . show) (oeis @40)

-- instance OEIS 68700 where
--   import Data.List.Ordered (isect)
--   oeisIx n = (oeis @68700) !! (n - 1)
--   oeis = isect (oeis @30457) (oeis @54211)

-- instance OEIS 68720 where
--   oeisIx = (oeisIx @3415) . (oeisIx @290)

-- instance OEIS 68781 where
--   oeis = filter ((== 0) . (oeisIx @261869)) [1..]

-- instance OEIS 68861 where
--   oeis = f "x" (map show [1..]) where
--      f u us = g us where
--        g (v:vs)
--          | and $ zipWith (/=) u v = (read v :: Int) : f v (delete v us)
--          | otherwise = g vs

-- instance OEIS 68863 where
--   oeis = f "x" (map show (oeis @40)) where
--      f p ps = g ps where
--        g (q:qs)
--          | and $ zipWith (/=) p q = (read q :: Int) : f q (delete q ps)
--          | otherwise = g qs

-- instance OEIS 68872 where
--   oeis = filter
--      (all (== 1) . map (`mod` 10) . (rowT @27750)) (oeis @2808)

-- instance OEIS 68901 where
--   oeisIx n = head $
--      filter ((== 0) . (`mod` fi n) . (+ (oeisIx @40) n)) $ [0..]

-- instance OEIS 68919 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @56868)

-- instance OEIS 68936 where
--   oeis = [x | x <- [1..], (oeisIx @8472) x <= (oeisIx @1222) x]

-- instance OEIS 68997 where
--   oeis = filter (\x -> mod x (oeisIx @173557 x) == 0) [1..]

-- instance OEIS 69011 where
--   oeis = tablList @69011
-- instance Table 69011 where
--   tabl = map snd $ iterate f (1, [0]) where
--      f (i, xs@ (x:_)) = (i + 2, (x + i) : zipWith (+) xs [i + 1, i + 3 ..])

-- instance OEIS 69056 where
--   oeis = filter (\x -> x ^ 2 `mod` (oeisIx @46970) x == 0) [1..]

-- instance OEIS 69059 where
--   oeis = filter ((> 1) . (oeisIx @9194)) [1..]

-- instance OEIS 69090 where
--   oeis = filter
--      (all (== 0) . map (oeisIx . read) . init . tail . inits . show)
--      (oeis @40)

-- instance OEIS 69099 where
--   oeisIx n = length
--      [ (x,y) | x <- [-n+1..n - 1], y <- [-n+1..n-1], x + y <= n - 1]

-- instance OEIS 69104 where
--   oeis =
--      map (+ 1) $ elemIndices 0 $ zipWith mod (drop 2 (oeis @45)) [1..]

-- instance OEIS 69106 where
--   oeis = [x | x <- (oeis @2808), (oeisIx @45) (x-1) `mod` x == 0]

-- instance OEIS 69107 where
--   oeis = h 2 $ drop 3 (oeis @45) where
--      h n (fib:fibs) = if fib `mod` n > 0 || (oeisIx @10051) n == 1
--          then h (n+1) fibs else n : h (n+1) fibs

-- instance OEIS 69158 where
--   oeisIx = product . (rowT @225817)

-- instance OEIS 69213 where
--   oeisIx = last . (rowT @77581)

-- instance OEIS 69283 where
--   oeisIx 0 = 0
--   oeisIx n = genericLength $ tail $ (rowT @182469) n

-- instance OEIS 69288 where
--   oeisIx n = genericLength $ takeWhile (<= (oeisIx @196) n) $ (rowT @182469) n

-- instance OEIS 69289 where
--   oeisIx n = sum $ takeWhile (<= (oeisIx @196) n) $ (rowT @182469) n

-- instance OEIS 69352 where
--   oeisIx = (oeisIx @1222) . (oeisIx @3586)

-- instance OEIS 69360 where
--   oeisIx n = sum [oeisIx' (4*n-p) | p <- takeWhile (<= 2*n) (oeis @40)]

-- instance OEIS 69488 where
--   oeis = filter f $ dropWhile (<= 100) (oeis @38618) where
--      f x = x < 10 || (oeisIx @10051) (x `mod` 100) == 1 && f (x `div` 10)

-- instance OEIS 69489 where
--   oeis = filter g $ dropWhile (<= 1000) (oeis @40) where
--      g x = x < 100 || (oeisIx @10051) (x `mod` 1000) == 1 && g (x `div` 10)

-- instance OEIS 69490 where
--   import Data.Set (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @69490) !! (n - 1)
--   oeis = f $ fromList [1..9] where
--      f s | m < 1000               = f s''
--          | h m && (oeisIx @10051)' m == 1 = m : f s''
--          | otherwise              = f s''
--          where s'' = union s' $ fromList $ map (+ (m * 10)) [1, 3, 7, 9]
--                (m, s') = deleteFindMin s
--      h x = x < 100 && (oeisIx @10051)' x == 1 ||
--            (oeisIx @10051)' (x `mod` 1000) == 1 &&
--            (oeisIx @10051)' (x `mod` 100) == 1 && h (x `div` 10)

-- instance OEIS 69492 where
--   import Data.Set (singleton, deleteFindMin, fromList, union)
--   oeisIx n = (oeis @69492) !! (n - 1)
--   oeis = 1 : f (singleton z) [1, z] zs where
--      f s q5s p5s'@ (p5:p5s)
--        | m < p5 = m : f (union (fromList $ map (* m) ps) s') q5s p5s'
--        | otherwise = f (union (fromList $ map (* p5) q5s) s) (p5:q5s) p5s
--        where ps = (rowT @27748) m
--              (m, s') = deleteFindMin s
--      (z:zs) = (oeis @50997)

-- instance OEIS 69493 where
--   import Data.Set (singleton, deleteFindMin, fromList, union)
--   oeisIx n = (oeis @69493) !! (n - 1)
--   oeis = 1 : f (singleton z) [1, z] zs where
--      f s q6s p6s'@ (p6:p6s)
--        | m < p6 = m : f (union (fromList $ map (* m) ps) s') q6s p6s'
--        | otherwise = f (union (fromList $ map (* p6) q6s) s) (p6:q6s) p6s
--        where ps = (rowT @27748) m
--              (m, s') = deleteFindMin s
--      (z:zs) = (oeis @30516)

-- instance OEIS 69513 where
--   oeisIx 1 = 0
--   oeisIx n = (oeisIx @10055) n

-- instance OEIS 69536 where
--   oeis = map (* 8) (oeis @77495)

-- instance OEIS 69545 where
--   oeis = map length $ group (oeis @8836)

-- instance OEIS 69567 where
--   oeis = f (oeis @40) where
--      f (p:ps@ (p':_)) = if sort (show p) == sort (show p')
--                        then p : f ps else f ps

-- instance OEIS 69715 where
--   oeis = filter ((== 1) . (oeisIx @52423)) [1..]

-- instance OEIS 69720 where
--   oeisIx n = (oeisIx $ n - 1) * (oeisIx $ n - 1)

-- instance OEIS 69754 where
--   oeisIx 1 = 0
--   oeisIx 2 = 1
--   oeisIx n = 2 * (oeisIx @720) n - 2 - (toInteger $ (oeisIx @10051) $ toInteger n)

-- instance OEIS 69799 where
--   oeisIx n = product $ zipWith (^) (oeisIx_row n) (reverse $ (rowT @124010) n)

-- instance OEIS 69817 where
--   oeisIx 1 = 1
--   oeisIx n = if null ms then n else minimum $ map (`mod` n) ms
--      where ms = zipWith (+) ds $ map (div n') ds
--            ds = takeWhile (< n - 1) $ tail $ (rowT @27750) n'
--            n' = n ^ 2 - 1

-- instance OEIS 69835 where
--   oeisIx n = (oeisIx @81577) (2 * n) n

-- instance OEIS 69905 where
--   oeis = scanl (+) 0 (oeis @8615)

-- instance OEIS 69915 where
--   oeisIx n = product $ zipWith sum_1phi (oeisIx_row n) (oeisIx_row n)
--      where sum_1phi p e = 1 + sum [p ^ k | k <- (rowT @38566) e]

-- instance OEIS 69928 where
--   oeis = scanl1 (+) (oeis @245656)

-- instance OEIS 70005 where
--   oeis = filter ((== 0) . (oeisIx @10055)) (oeis @78174)

-- instance OEIS 70047 where
--   oeisIx n = p 1 n where
--      p k m | m == 0 = 1 | m < k = 0 | otherwise = q k (m-k) + p (k+1) m
--      q k m | m == 0 = 1 | m < k = 0 | otherwise = p (k+2) (m-k) + p (k+2) m

-- instance OEIS 70048 where
--   oeisIx = p (oeis @42968) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 70072 where
--   oeisIx n = genericLength [ () | x <- [1..n], y <- [1..x], (oeisIx @8966) (x*y) == 1]

-- instance OEIS 70073 where
--   oeisIx n = genericLength [ () | x <- [1..n], y <- [1..x], z <- [1..y],
--                            (oeisIx @212793) (x*y*z) == 1]

-- instance OEIS 70168 where
--   oeis = tablList @70168
-- instance Table 70168 where
--   rowCol = rowCol_off @70168 @1 @1
--   tabf = map (rowT @70168) [1..]
--   rowT n = (takeWhile (/= 1) $ iterate (oeisIx @14682) n) ++ [1]
--   oeis = concat (tabf @70168)



-- instance OEIS 70194 where
--   oeisIx n = maximum $ zipWith (-) (tail ts) ts where ts = (rowT @38566) n

-- instance OEIS 70215 where
--   oeisIx = (oeisIx @586) . (oeisIx @40)

-- instance OEIS 70216 where
--   oeis = tablList @70216
-- instance Table 70216 where
--   rowCol = rowCol_off @70216 @1 @1
--   rowT   = rowT_off   @70216 @1
--   tabl = zipWith (zipWith (\u v -> (u + v) `div` 2))
--                          (tabl @215630) (tabl @215631)

-- instance OEIS 70229 where
--   oeisIx n = n + (oeisIx @6530) n

-- instance OEIS 70550 where
--   oeis = 1 : 2 : 2 : 3 :
--      zipWith (+) (oeis @70550)
--                  (zipWith (+) (tail (oeis @70550)) (drop 3 (oeis @70550)))

-- instance OEIS 70647 where
--   oeisIx = (oeisIx @6530) . (oeisIx @6881)

-- instance OEIS 70750 where
--   oeisIx = (2 -) . (`mod` 4) . (oeisIx @40)

-- instance OEIS 70760 where
--   oeis = [x | x <- [0..], let y = (oeisIx @61205) x,
--                       y /= x ^ 2, (oeisIx @10052) y == 1]

-- instance OEIS 70861 where
--   oeis = concat (tabf @70861)
--   oeisIx_tabf = [1] : f 2 [1] where
--      f n ps = ps' : f (n+1) ps' where ps' = m ps $ map (n*) ps
--      m []         ys = ys
--      m xs'@ (x:xs) ys'@ (y:ys)
--          | x < y     = x : m xs ys'
--          | x == y    = x : m xs ys
--          | otherwise = y : m xs' ys
--   b070861 = bFile' "A070861" (concat $ take 20 (tabf @70861)) 1

-- instance OEIS 70867 where
--   oeis = 1 : 1 : zipWith (+)
--      (map (oeisIx @70867) $ zipWith (-) [2..] (oeis @70867))
--      (map (oeisIx @70867) $ zipWith (-) [2..] $ tail (oeis @70867))

-- instance OEIS 70883 where
--   oeis = zipWith xor [1..] (oeis @40)

-- instance OEIS 70887 where
--   oeis = tablList @70887
-- instance Table 70887 where
--   rowCol = rowCol_off @70887 @1 @1
--   rowT   = rowT_off   @70887 @1
--   tabl = zipWith take [1..] (tabf @75437)

-- instance OEIS 70897 where
--   oeisIx n = genericLength $ filter (all ((== 1) . (oeisIx @10051)))
--                        $ map (zipWith (+) [1..n]) (permutations [n+1..2*n])

-- instance OEIS 70940 where
--   oeisIx = maximum . (rowT @80080)

-- instance OEIS 70960 where
--   oeisIx n = if n == 1 then 1 else 3 * (oeisIx @142) n `div` 2
--   oeis = map (flip div 2) fs where fs = 3 : zipWith (*) [2..] fs

-- instance OEIS 70965 where
--   oeis = 1 : f 1 where
--      f x = y : f (x + 1) where
--        y = sum $ zipWith (*) (map (oeisIx @70965) $ (rowT @27750) x) (oeisIx_row x)

-- instance OEIS 71032 where
--   oeis = tablList @71032
-- instance Table 71032 where
--   tabf = map reverse (tabf @70950)

-- instance OEIS 71139 where
--   oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) [2..]

-- instance OEIS 71140 where
--   oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) (oeis @24619)

-- instance OEIS 71176 where
--   oeisIx n = fromJust $ findIndex (== 1) $
--               map (oeisIx . read . (show n ++) . show) [0..]

-- instance OEIS 71188 where
--   oeisIx = (oeisIx @6530) . (oeisIx @5)

-- instance OEIS 71249 where
--   oeis = filter ((> 1) . (oeisIx @55483)) [1..]

-- instance OEIS 71295 where
--   oeisIx n = (oeisIx @120) n * (oeisIx @23416) n

-- instance OEIS 71318 where
--   oeis = [x | x <- [1..],  (oeisIx @212793) x == 1, (oeisIx @8966) x == 0,
--                       let y = x+1, (oeisIx @212793) y == 1, (oeisIx @8966) y == 0]

-- instance OEIS 71321 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ zipWith (*) (oeis @33999) $ (rowT @27746) n

-- instance OEIS 71330 where
--   oeisIx n = sum $
--      map (oeisIx . (n -)) $ takeWhile (<= n `div` 2) (oeis @961)

-- instance OEIS 71331 where
--   oeis = filter ((== 0) . (oeisIx @71330)) [1..]

-- instance OEIS 71367 where
--   oeis = tail $ filter f [1..] where
--      f x = and $ map g [5, 4 .. 1] where
--        g k = sum (map h $ map (+ x) [0..4]) == 1 where
--          h z = if r == 0 then (oeisIx @10051)' z' else 0
--                where (z', r) = divMod z k

-- instance OEIS 71368 where
--   oeis = filter f [1..] where
--      f x = and $ map g [6, 5 .. 1] where
--        g k = sum (map h $ map (+ x) [0..5]) == 1 where
--          h z = if r == 0 then (oeisIx @10051)' z' else 0
--                where (z', r) = divMod z k

-- instance OEIS 71407 where
--   oeisIx n = head [k | k <- [2,4..], let x = k * (oeisIx @40) n,
--                         (oeisIx @10051)' (x - 1) == 1, (oeisIx @10051)' (x + 1) == 1]

-- instance OEIS 71413 where
--   oeisIx 0 = 0
--   oeisIx n | m == 0    = (oeisIx @71413) n' + n
--             | otherwise = (oeisIx @71413) n' - n  where (n',m) = divMod n 2

-- instance OEIS 71521 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @3586)

-- instance OEIS 71558 where
--   oeisIx n = head [k | k <- [1..], let x = k * n,
--                         (oeisIx @10051)' (x - 1) == 1, (oeisIx @10051)' (x + 1) == 1]

-- instance OEIS 71574 where
--   oeisIx 1 = 0
--   oeisIx n = 2 * (oeisIx @71574) (if j > 0 then j + 1 else (oeisIx @49084) n) + 1 - signum j
--               where j = (oeisIx @66246) n

-- instance OEIS 71681 where
--   oeisIx n = sum $ map (oeisIx @10051)' $
--      takeWhile (> 0) $ map (2 * (oeisIx @40) n -) $ drop n (oeis @40)

-- instance OEIS 71695 where
--   oeis = [p | p <- (oeis @2144), (oeisIx @10051)' (p + 2) == 1]

-- instance OEIS 71696 where
--   oeis = [p | p <- tail (oeis @2145), (oeisIx @10051)' (p - 2) == 1]

-- instance OEIS 71698 where
--   oeis = [x | x <- [3, 7 ..], (oeisIx @10051)' x == 1, (oeisIx @10051)' (x+2) == 1]

-- instance OEIS 71699 where
--   oeis = [x | x <- [5, 9 ..], (oeisIx @10051)' x == 1, (oeisIx @10051)' (x-2) == 1]

-- instance OEIS 71700 where
--   oeis = [x * y | x <- [3, 7 ..], (oeisIx @10051)' x == 1,
--                           let y = x + 2, (oeisIx @10051)' y == 1]

-- instance OEIS 71703 where
--   oeisIx = z (oeis @65091) 0 . (* 3) . (oeisIx @40) where
--      z _ 3 m = fromEnum (m == 0)
--      z ps'@ (p:ps) i m = if m < p then 0 else z ps' (i+1) (m - p) + z ps i m

-- instance OEIS 71704 where
--   oeisIx n = z (us ++ vs) 0 (3 * q)  where
--      z _ 3 m = fromEnum (m == 0)
--      z ps'@ (p:ps) i m = if m < p then 0 else z ps' (i+1) (m - p) + z ps i m
--      (us, _:vs) = span (< q) (oeis @65091); q = (oeisIx @40) n

-- instance OEIS 71786 where
--   oeisIx = product . map (oeisIx @4086) . (rowT @27746)

-- instance OEIS 71810 where
--   oeisIx = sum . map (oeisIx @10051)' . map sum .
--             tail . subsequences . flip take (oeis @40)

-- instance OEIS 71888 where
--   oeisIx 1 = 2
--   oeisIx n = head [m | m <- dropWhile (<= n) (oeis @5117), gcd m n > 1]

-- instance OEIS 71889 where
--   oeisIx n = gcd n $ (oeisIx @71888) n

-- instance OEIS 71890 where
--   oeisIx n = (oeisIx @71888) n - n

-- instance OEIS 71891 where
--   oeisIx n = (oeisIx @71890) n `div` (oeisIx @71889) n

-- instance OEIS 71892 where
--   oeisIx n = lcm n $ (oeisIx @71888) n

-- instance OEIS 71893 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @71891))

-- instance OEIS 71904 where
--   oeis = filter odd (oeis @2808)

-- instance OEIS 71931 where
--   oeis = filter f (oeis @2312) where
--      f x = 2 * gpf <= (oeisIx @6530) (gpf ^ 2 + 1) where gpf = (oeisIx @6530) x

-- instance OEIS 71954 where
--   oeis = 2 : 4 : zipWith (-)
--                  (map ((4 *) . pred) (tail (oeis @71954))) (oeis @71954)

-- instance OEIS 71974 where
--   oeisIx n = product $ zipWith (^) (oeisIx_row n) $
--      map (\e -> (1 - e `mod` 2) * e `div` 2) $ (rowT @124010) n

-- instance OEIS 71975 where
--   oeisIx n = product $ zipWith (^) (oeisIx_row n) $
--      map (\e -> (e `mod` 2) * (e + 1) `div` 2) $ (rowT @124010) n

-- instance OEIS 71977 where
--   oeis = tablList @71977
-- instance Table 71977 where
--   rowCol = rowCol_off @71977 @1 @1
--   rowT   = rowT_off   @71977 @1
--   tabl = f 1 [1..] where
--      f k xs = ys : f (k+1) (dropWhile (<= last ys) xs) where
--        ys  = take k $ filter ((== 1) . (gcd k)) xs

-- instance OEIS 72010 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map f $ (rowT @27746) n where
--      f 2 = 2
--      f p = p + 2 * (2 - p `mod` 4)

-- instance OEIS 72046 where
--   oeisIx n = gcd (oeisIx n) (oeisIx n)

-- instance OEIS 72047 where
--   oeis = map (oeisIx @1221) $ (oeis @5117)

-- instance OEIS 72048 where
--   oeisIx = (2 ^) . (oeisIx @72047)

-- instance OEIS 72055 where
--   oeisIx = (+ 1) . (* 2) . (oeisIx @40)

-- instance OEIS 72057 where
--   oeisIx = (oeisIx @203) . (oeisIx @72055)

-- instance OEIS 72084 where
--   oeisIx = product . map (oeisIx @120) . (rowT @27746)

-- instance OEIS 72085 where
--   oeisIx = (oeisIx @72084) . (oeisIx @72084)

-- instance OEIS 72086 where
--   oeisIx n = fst $
--      until ((== 1) . snd) (\ (i, x) -> (i + 1, (oeisIx @72084) x)) (0, n)

-- instance OEIS 72087 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (oeisIx @61712) $ (rowT @27746) n

-- instance OEIS 72103 where
--   import Data.Set (singleton, findMin, deleteMin, insert)
--   oeisIx n = (oeis @72103) !! (n - 1)
--   oeis = f 9 3 $ Set.singleton (4,2) where
--      f zz z s
--        | xx < zz   = xx : f zz z (Set.insert (x*xx, x) $ Set.deleteMin s)
--        | otherwise = zz : f (zz+2*z+1) (z+1) (Set.insert (z*zz, z) s)
--        where (xx, x) = Set.findMin s

-- instance OEIS 72137 where
--   oeisIx :: Int -> Int
--   oeisIx = genericLength . fst . spanCycle (abs . (oeisIx @56965)) where
--      spanCycle :: Eq a => (a -> a) -> a -> ([a],[a])
--      spanCycle f x = fromJust $ find (not . null . snd) $
--                                 zipWith (span . (/=)) xs $ inits xs
--                      where xs = iterate f x

-- instance OEIS 72182 where
--   (oeis, (oeis @72186)) = unzip wallisPairs
--     wallisPairs = [ (x, y) | (y, sy) <- tail ws,
--                             (x, sx) <- takeWhile ((< y) . fst) ws, sx == sy]
--                   where ws = zip [1..] $ map (oeisIx @203) $ tail (oeis @290)

-- instance OEIS 72186 where

-- instance OEIS 72202 where
--   oeis = [x | x <- [1..], (oeisIx @83025) x == (oeisIx @65339) x]

-- instance OEIS 72211 where
--   oeis = 1 : zipWith div (tail (oeis @217863)) (oeis @217863)

-- instance OEIS 72219 where
--   oeisIx = (+ 1) . (* 2) . (oeisIx @33264) . subtract 1

-- instance OEIS 72221 where
--   oeis = 1 : 4 : (map (+ 2) $
--      zipWith (-) (map (* 6) $ tail (oeis @72221)) (oeis @72221))

-- instance OEIS 72229 where
--   oeis = [0, 0, 0, 0, 1, 2, 3, 4] ++ zipWith (+)
--                  (zipWith (-) (tail (oeis @72229)) (oeis @72229))
--                  (drop 7 (oeis @72229))

-- instance OEIS 72403 where
--   oeis = map denominator $ scanl1 (-) $
--     map ((1 %) . (oeisIx @244)) $ (oeis @29837)

-- instance OEIS 72452 where
--   oeis = 0 : map (oeisIx @4086) (zipWith (+) (oeis @72452) [1..])

-- instance OEIS 72499 where
--   oeisIx = product . (rowT @161906)

-- instance OEIS 72504 where
--   oeisIx = foldl1 lcm . (rowT @161906)

-- instance OEIS 72511 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @61357))

-- instance OEIS 72513 where
--   oeisIx n = product $ map (n -) $ (rowT @27751) n

-- instance OEIS 72541 where
--   oeis = concat $ transpose
--                  [map (+ 1) (oeis @23200), map (+ 5) (oeis @23200)]

-- instance OEIS 72543 where
--   oeis = [x | x <- [0..], (oeisIx @54055) x == (oeisIx @30) x]

-- instance OEIS 72544 where
--   oeis = [x | x <- [0..], (oeisIx @54054) x == (oeisIx @30) x]

-- instance OEIS 72547 where
--   oeisIx n = (oeisIx @108561) (2 * (n - 1)) (n - 1)

-- instance OEIS 72591 where
--   oeisIx = foldl1 (.&.) . (rowT @27746)

-- instance OEIS 72593 where
--   oeisIx = foldl1 (.|.) . (rowT @27746)

-- instance OEIS 72594 where
--   oeisIx = foldl1 xor . (rowT @27746) :: Integer -> Integer

-- instance OEIS 72595 where
--   oeis = filter ((== 0) . (oeisIx @72594)) [1..]

-- instance OEIS 72596 where
--   oeis = filter ((== 0) . (oeisIx @10052)) (oeis @72595)

-- instance OEIS 72600 where
--   oeis = filter ((< 0) . (oeisIx @37861)) [0..]

-- instance OEIS 72601 where
--   oeis = filter ((<= 0) . (oeisIx @37861)) [0..]

-- instance OEIS 72602 where
--   oeis = filter ((>= 0) . (oeisIx @37861)) [1..]

-- instance OEIS 72603 where
--   oeis = filter ((> 0) . (oeisIx @37861)) [1..]

-- instance OEIS 72618 where
--   oeis = filter f [1 ..] where
--      f x = any (all ((== 1) . (oeisIx @10051)' . fi)) $
--            map cs [concat $ transpose [[2*x, 2*x-2 .. 2] , us] |
--                    us <- map (uncurry (++) . (uncurry $ flip (,))
--                               . flip splitAt [1, 3 .. 2 * x]) [1 .. x]]
--      cs zs = (head zs + last zs) : zipWith (+) zs (tail zs)

-- instance OEIS 72627 where
--   oeisIx = genericLength . filter ((== 1) . (oeisIx @10051) . (subtract 1)) . (rowT @27749)

-- instance OEIS 72649 where
--   oeis = f 1 where
--      f n = (replicate (fromInteger $ (oeisIx @45) n) n) ++ f (n+1)

-- instance OEIS 72701 where
--   oeisIx n = f (oeis @40) 1 n 0 where
--      f (p:ps) l nl x
--        | y > nl    = 0
--        | y < nl    = f ps (l + 1) (nl + n) y + f ps l nl x
--        | otherwise = if y `mod` l == 0 then 1 else 0
--        where y = x + p

-- instance OEIS 72762 where
--   oeisIx n = foldl (\v d -> 2*v + d) 0 $ map (oeisIx @10051) [1..n]

-- instance OEIS 72774 where
--   import Data.Map (empty, findMin, deleteMin, insert)
--   import qualified Data.Map.Lazy as Map (null)
--   oeisIx n = (oeis @72774) !! (n - 1)
--   (oeis, (oeis @72775), (oeis @72776)) = unzip3 $
--      (1, 1, 1) : f (tail (oeis @5117)) empty where
--      f vs'@ (v:vs) m
--       | Map.null m || xx > v = (v, v, 1) :
--                                f vs (insert (v^2) (v, 2) m)
--       | otherwise = (xx, bx, ex) :
--                     f vs' (insert (bx*xx) (bx, ex+1) $ deleteMin m)
--       where (xx, (bx, ex)) = findMin m

-- instance OEIS 72775 where

-- instance OEIS 72776 where

-- instance OEIS 72777 where
--   import Data.Map (singleton, findMin, deleteMin, insert)
--   oeisIx n = (oeis @72777) !! (n - 1)
--   oeis = f 9 (drop 2 (oeis @5117)) (singleton 4 (2, 2)) where
--      f vv vs'@ (v:ws@ (w:_)) m
--       | xx < vv = xx : f vv vs' (insert (bx*xx) (bx, ex+1) $ deleteMin m)
--       | xx > vv = vv : f (w*w) ws (insert (v^3) (v, 3) m)
--       where (xx, (bx, ex)) = findMin m

-- instance OEIS 72779 where
--   oeisIx n = (oeisIx @1157) n + (oeisIx n) * (oeisIx n)

-- instance OEIS 72823 where
--   oeis = tail $ elemIndices 0 (oeis @73267)

-- instance OEIS 72873 where
--   import Data.Set (empty, fromList, deleteFindMin, union)
--   import qualified Data.Set as Set (null)
--   oeisIx n = (oeis @72873) !! (n - 1)
--   oeis = 1 : h empty [1] (oeis @51674) where
--      h s mcs xs'@ (x:xs)
--       | Set.null s || x < m = h (s `union` fromList (map (* x) mcs)) mcs xs
--       | otherwise = m : h (s' `union` fromList (map (* m) $ init (m:mcs)))
--                           (m:mcs) xs'
--       where (m, s') = deleteFindMin s

-- instance OEIS 72905 where
--   oeisIx n = head [k | k <- [n + 1 ..], (oeisIx @10052) (k * n) == 1]

-- instance OEIS 72911 where
--   oeisIx = product . map (oeisIx . fi) . (rowT @124010)

-- instance OEIS 72941 where
--   oeisIx n = product $ zipWith (^) ps $ map (max 1) es where
--               (ps, es) = unzip $ dropWhile ((== 0) . snd) $
--                          zip (oeis @40) $ (rowT @67255) n

-- instance OEIS 72965 where
--   oeisIx n = f 1 (oeisIx_row n) where
--      f y []      = y
--      f y [p]     = p * y
--      f y (2:ps)  = f (2 * y) ps
--      f y (3:5:_) = (oeisIx @72965) (n `div` 15)
--      f y (p:qs@ (q:ps)) | q == p + 2 = f y ps
--                        | otherwise  = f (p * y) qs

-- instance OEIS 73034 where
--   oeis = filter (`elem` [2,3,5,7]) (oeis @33308)

-- instance OEIS 73046 where
--   oeisIx n = head $ dropWhile (== 0) $
--                      zipWith (*) prims $ map (oeisIx . (2*n -)) prims
--      where prims = takeWhile (<= n) (oeis @40)

-- instance OEIS 73101 where
--   oeisIx n = genericLength [ (x,y) |
--      x <- [n `div` 4 + 1 .. 3 * n `div` 4],   let y' = recip $ 4%n - 1%x,
--      y <- [floor y' + 1 .. floor (2*y') + 1], let z' = recip $ 4%n - 1%x - 1%y,
--      denominator z' == 1 && numerator z' > y && y > x]

-- instance OEIS 73121 where
--   oeisIx n = (oeisIx @53644) n * (fi n + 2 * (oeisIx @53645) n)

-- instance OEIS 73138 where
--   oeisIx n = (oeisIx @38573) n * (oeisIx @80100) n

-- instance OEIS 73180 where
--   oeisIx n = genericLength [x | x <- (rowT @27750) n, x <= (oeisIx @7947) n]

-- instance OEIS 73184 where
--   oeisIx = sum . map (oeisIx @212793) . (rowT @27750)

-- instance OEIS 73185 where
--   oeisIx = sum . filter ((== 1) . (oeisIx @212793)) . (rowT @27750)

-- instance OEIS 73267 where
--   oeisIx n = sum $ zipWith (*) (oeis @209229) $ reverse $ take n (oeis @36987)

-- instance OEIS 73311 where
--   oeisIx = sum . map (oeisIx @8966) . (rowT @38566)

-- instance OEIS 73334 where
--   oeisIx 0 = 3
--   oeisIx n = (oeisIx @45) $ (oeisIx @5811) n + 4

-- instance OEIS 73353 where
--   oeisIx n = n + (oeisIx @7947) n

-- instance OEIS 73364 where
--   oeisIx n = genericLength $ filter (all isprime)
--                        $ map (zipWith (+) [1..n]) (permutations [1..n])
--      where isprime n = (oeisIx @10051) n == 1

-- instance OEIS 73395 where
--   oeisIx n = (oeisIx @8472) n * (oeisIx @1414) n

-- instance OEIS 73445 where
--   oeis = zipWith (-) (tail (oeis @73783)) (oeis @73783)

-- instance OEIS 73481 where
--   oeisIx = (oeisIx @20639) . (oeisIx @5117)

-- instance OEIS 73482 where
--   oeisIx = (oeisIx @6530) . (oeisIx @5117)

-- instance OEIS 73483 where
--   oeisIx n = product $ filter ((> 0) . (mod m)) $
--      dropWhile (<= (oeisIx @20639) m) $ takeWhile (<= (oeisIx @6530) m) (oeis @40)
--      where m = (oeisIx @5117) n

-- instance OEIS 73485 where
--   oeis = filter ((== 1) . (oeisIx @192280)) [1..]

-- instance OEIS 73490 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength $ filter (> 1) $ zipWith (-) (tail ips) ips
--      where ips = map (oeisIx @49084) $ (rowT @27748) n

-- instance OEIS 73491 where
--   oeis = filter ((== 0) . (oeisIx @73490)) [1..]

-- instance OEIS 73492 where
--   oeis = filter ((> 0) . (oeisIx @73490)) [1..]

-- instance OEIS 73493 where
--   oeis = filter ((== 1) . (oeisIx @73490)) [1..]

-- instance OEIS 73494 where
--   oeis = filter ((== 2) . (oeisIx @73490)) [1..]

-- instance OEIS 73495 where
--   oeis = filter ((== 3) . (oeisIx @73490)) [1..]

-- instance OEIS 73576 where
--   oeisIx = p (oeis @5117) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 73601 where
--   oeisIx n = 2 + length
--      (takeWhile ((oeisIx n /=) . (oeisIx @30)) $ iterate (* n) (n^2))

-- instance OEIS 73642 where
--   oeisIx = sum . zipWith (*) [0..] . (rowT @30308)

-- instance OEIS 73646 where
--   oeisIx :: Integer -> Integer
--   oeisIx = read . concat . sort . map show . (rowT @27746)

-- instance OEIS 73703 where
--   oeisIx n = head [p | p <- (oeis @40), (oeisIx @10051) (p + 2 * (oeisIx @40) n) == 1]

-- instance OEIS 73708 where
--   oeis = conv (oeis @73707) [] where
--      conv (v:vs) ws = (sum $ zipWith (*) ws' $ reverse ws') : conv vs ws'
--                       where ws' = v : ws

-- instance OEIS 73709 where
--   oeis = 1 : zipWith (-) (tail (oeis @73708)) (oeis @73708)
--   --- _Reinhard Zumkeller_, Jun 13 2013

-- instance OEIS 73710 where
--   oeis = conv (oeis @73709) [] where
--      conv (v:vs) ws = (sum $ zipWith (*) ws' $ reverse ws') : conv vs ws'
--                       where ws' = v : ws

-- instance OEIS 73711 where
--   oeis = 1 :
--   (tail $ concat $ transpose [oeis, (oeis @73712)])

-- instance OEIS 73712 where
--   oeis = map (g (oeis @73711)) [1..] where
--   g xs k = sum $ zipWith (*) xs $ reverse $ take k xs

-- instance OEIS 73729 where
--   oeisIx n = 10 * (oeisIx @30) n + (oeisIx @10879) n

-- instance OEIS 73730 where
--   oeisIx n = 10 * (oeisIx @54055) n + (oeisIx @54054) n

-- instance OEIS 73734 where
--   oeis = zipWith gcd (oeis @64413) $ tail (oeis @64413)

-- instance OEIS 73738 where
--   oeis = tail zs where
--      zs = 1 : 1 : zipWith (+) (oeis @6005) zs

-- instance OEIS 73739 where
--   oeis = concat $ transpose [1 : 1 : repeat 0, tail (oeis @36467)]

-- instance OEIS 73740 where
--   oeis = tail $ f (oeis @73739) [] where
--      f (x:xs) ys = (sum $ zipWith (*) ys (oeis @73739)) : f xs (x : ys)

-- instance OEIS 73777 where
--   oeis = 1 : f [1] where
--      f xs = y : f (y : xs) where y = sum $ zipWith (*) xs ms'
--      ms' = map negate $ tail (oeis @68341)

-- instance OEIS 73783 where
--   oeis = zipWith (-) (tail (oeis @2808)) (oeis @2808)

-- instance OEIS 73785 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @73785) n' * 10 + m where
--      (n', m) = if r < 0 then (q + 1, r + 3) else (q, r)
--                where (q, r) = quotRem n (negate 3)

-- instance OEIS 73789 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @73789) n' * 10 + m where
--      (n', m) = if r < 0 then (q + 1, r + 8) else (q, r)
--                where (q, r) = quotRem n (negate 8)

-- instance OEIS 73846 where
--   oeis = concat $ transpose [oeis, (oeis @40)]

-- instance OEIS 73890 where
--   oeisIx n = numerator $ n % (oeisIx @196) n

-- instance OEIS 74206 where
--   oeisIx n | n <= 1 = n
--   | otherwise = 1 + (sum $ map (oeisIx . (div n)) $
--   tail $ (rowT @27751) n)

-- instance OEIS 74235 where
--   oeis = filter ((== 1) . (oeisIx @227481)) [1..]

-- instance OEIS 74394 where
--   oeis = 1 : 2 : 3 : zipWith (-)
--      (tail $ zipWith (*) (tail (oeis @74394)) (oeis @74394)) (oeis @74394)

-- instance OEIS 74480 where
--   import Data.Set (Set, singleton, delete, findMin, deleteFindMin, insert)
--   oeisIx n = (oeis @74480) !! (n - 1)
--   oeis = multClosure (oeis @37074) where
--     multClosure []     = [1]
--     multClosure (b:bs) = 1:h [b] (singleton b) bs where
--      h cs s []    = m:h (m:cs) (foldl (flip insert) s' $ map (*m) cs) []
--       where (m, s') = deleteFindMin s
--      h cs s xs'@ (x:xs)
--       | m < x     = m:h (m:cs) (foldl (flip insert) s' $ map (*m) cs) xs'
--       | otherwise = x:h (x:cs) (foldl (flip insert) s  $ map (*x) (x:cs)) xs
--       where (m, s') = deleteFindMin s

-- instance OEIS 74583 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @74583) !! (n - 1)
--   oeis = 1 : f (singleton 2) (oeis @40) where
--     f s ps'@ (p:p':ps)
--       | m == p      = p : f (insert (p*p) $ insert p' s') (p':ps)
--       | m < spf^spf = m : f (insert (m*spf) s') ps'
--       | otherwise   = m : f s' ps'
--         where spf = (oeisIx @20639) m
--               (m, s') = deleteFindMin s
--   -- Simpler version:
--   oeis = map (oeisIx @961) (oeis @192188)

-- instance OEIS 74677 where
--   oeis = 0 : 1 : 1 : 1 : zipWith (+) (oeis @74677)
--      (zipWith (+) (tail (oeis @74677)) (drop 3 (oeis @74677)))

-- instance OEIS 74695 where
--   oeisIx n = gcd n $ (oeisIx @48760) n

-- instance OEIS 74721 where
--   oeis = f 0 $ map toInteger (oeis @33308) where
--      f c ds'@ (d:ds) | (oeisIx @10051)'' c == 1 = c : f 0 ds'
--                     | otherwise = f (10 * c + d) ds

-- instance OEIS 74819 where
--   oeis = filter ((== 0) . (oeisIx @92410)) [1..]

-- instance OEIS 74829 where
--   oeis = tablList @74829
-- instance Table 74829 where
--   rowCol = rowCol_off @74829 @1 @1
--   rowT   = rowT_off   @74829 @1
--   tabl = map fst $ iterate
--      (\ (u:_, vs) -> (vs, zipWith (+) ([u] ++ vs) (vs ++ [u]))) ([1], [1,1])

-- instance OEIS 74909 where
--   oeis = tablList @74909
-- instance Table 74909 where
--   tabl = iterate
--      (\row -> zipWith (+) ([0] ++ row) (row ++ [1])) [1]

-- instance OEIS 74911 where
--   oeis = tablList @74911
-- instance Table 74911 where
--   rowCol = rowCol_off @74911 @1 @1
--   rowT   = rowT_off   @74911 @1
--   tabl = map fst $ iterate
--      (\ (vs, w:ws) -> (zipWith (+) ([w] ++ vs) (vs ++ [w]), ws))
--      ([1], tail (oeis @1563))

-- instance OEIS 74940 where
--   oeis = filter ((== 0) . (oeisIx @39966)) [0..]

-- instance OEIS 74963 where
--   oeisIx n = maximum [oeisIx (x*y) | x <- [1..n], y <- [x..n]]

-- instance OEIS 74964 where
--   oeis = filter (\x -> (oeisIx @74963) x == (oeisIx @65764) x) [1..]

-- instance OEIS 74976 where
--   oeis = map (round . recip) $ zipWith (-) (tail rs) rs
--                  where rs = map (sqrt . fi) (oeis @40)

-- instance OEIS 74985 where
--   oeisIx = (oeisIx @290) . (oeisIx @1358)

-- instance OEIS 74989 where
--   oeisIx 0 = 0
--   oeisIx n = min (n - last xs) (head ys - n) where
--      (xs,ys) = span (< n) (oeis @578)

-- instance OEIS 75090 where
--   oeis = filter even (oeis @1597)

-- instance OEIS 75093 where
--   oeis = f (oeis @40) where
--      f (p:ps@ (q:r:_)) =
--        if sort (show p) == sort (show q) && sort (show q) == sort (show r)
--           then p : f ps else f ps

-- instance OEIS 75104 where
--   oeisIx n = gcd n $ (oeisIx @523) n

-- instance OEIS 75105 where
--   oeisIx n = numerator $ n % (oeisIx @523) n

-- instance OEIS 75106 where
--   oeisIx n = denominator $ n % (oeisIx @523) n

-- instance OEIS 75109 where
--   oeis = filter odd (oeis @1597)

-- instance OEIS 75110 where
--   oeisIx (fi->n) = fi . read $ show (oeisIx n) ++ show n

-- instance OEIS 75119 where
--   oeisIx n = denominator $ n % (oeisIx @196) n

-- instance OEIS 75157 where
--   oeisIx 0 = 0
--   oeisIx n = product (zipWith (^) (oeis @40) rs') - 1 where
--      rs' = reverse $ r : map (subtract 1) rs
--      (r:rs) = reverse $ map length $ group $ (rowT @30308) n

-- instance OEIS 75158 where
--   oeisIx = fromJust . (`elemIndex` (oeis @75157))

-- instance OEIS 75177 where
--   oeis = map (fi . (+ 1)) $ elemIndices 1 $ map (oeisIx @10051 . oeisIx @7953) (oeis @40)

-- instance OEIS 75180 where
--   oeis = map (denominator . sum) $ zipWith (zipWith (%))
--      (zipWith (map . (*)) (oeis @142) (tabf @242179)) (tabf @106831)

-- instance OEIS 75188 where
--   oeis = f 1 [] where
--      f x hs = (length $ filter ((== 1) . (oeisIx @10051)') (map numerator hs')) :
--               f (x + 1) hs' where hs' = hs ++ map (+ recip x) (0 : hs)

-- instance OEIS 75189 where
--   import Data.Set (Set, empty, fromList, toList, union, size)
--   oeisIx n = (oeis @75189) !! (n - 1)
--   oeis = f 1 empty empty where
--      f x s s1 = size s1' : f (x + 1) (s `union` fromList hs) s1' where
--        s1' = s1 `union` fromList
--              (filter ((== 1) . (oeisIx @10051)') $ map numerator hs)
--        hs = map (+ 1 % x) $ 0 : toList s

-- instance OEIS 75226 where
--   oeis = f 2 [recip 1] where
--      f x hs = (maximum $ filter ((== 1) . (oeisIx @10051)') (map numerator hs')) :
--               f (x + 1) hs' where hs' = hs ++ map (+ recip x) hs

-- instance OEIS 75227 where
--   import Data.Set (Set, empty, fromList, toList, union)
--   oeisIx n = (oeis @75227) !! (n - 1)
--   oeis = f 1 empty (oeis @65091) where
--      f x s ps = head qs : f (x + 1) (s `union` fromList hs) qs where
--        qs = foldl (flip del)
--             ps $ filter ((== 1) . (oeisIx @10051)') $ map numerator hs
--        hs = map (+ 1 % x) $ 0 : toList s
--      del u vs'@ (v:vs) = case compare u v
--                         of LT -> vs'; EQ -> vs; GT -> v : del u vs

-- instance OEIS 75253 where
--   oeis = iterate (oeisIx @55944) 77

-- instance OEIS 75254 where
--   oeisIx n = n + (oeisIx @1414) n

-- instance OEIS 75268 where
--   oeis = iterate (oeisIx @55944) 442

-- instance OEIS 75321 where
--   oeisIx = (oeisIx @75323) . subtract 1 . (* 2)

-- instance OEIS 75322 where
--   oeisIx = (oeisIx @75323) . (* 2)

-- instance OEIS 75323 where
--   oeis = f 1 []  $ tail (oeis @40) where
--      f k ys qs = g qs where
--        g (p:ps) | (oeisIx @10051)' pk == 0 || pk `elem` ys = g ps
--                 | otherwise = p : pk : f (k + 1) (p:pk:ys) (qs \\ [p, pk])
--                 where pk = p + 2 * k

-- instance OEIS 75326 where
--   oeis = 0 : f [1..] where
--      f ws@ (u:v:_) = y : f (ws \\ [u, v, y]) where y = u + v

-- instance OEIS 75345 where
--   oeisIx = sum . (rowT @75348)

-- instance OEIS 75346 where
--   oeisIx = head . (rowT @75348)

-- instance OEIS 75347 where
--   oeisIx = last . (rowT @75348)

-- instance OEIS 75348 where
--   oeis = tablList @75348
--   rowCol = rowCol_off @75348 @1 @1
--   rowT   = rowT_off   @75348 @1
--   tabl = f 0 [1..] where
--      f x zs = (us ++ [y]) : f (x + 1) (zs \\ (y : us)) where
--        y = g vs
--        g (w:ws) = if (oeisIx @10051)' (sum us + w) == 1 then w else g ws
--        (us, vs) = splitAt x zs
--   oeis = concat (tabl @75348)

-- instance OEIS 75362 where
--   oeis = tablList @75362
-- instance Table 75362 where
--   rowCol = rowCol_off @75362 @1 @1
--   rowT   = rowT_off   @75362 @1
--   tabl = zipWith (zipWith (*)) (tabl @2260) (tabl @2024)

-- instance OEIS 75369 where
--   oeisIx = (^ 2) . (oeisIx @14574)

-- instance OEIS 75383 where
--   oeis = tablList @75383
--   rowCol = rowCol_off @75383 @1 @1
--   rowT   = rowT_off   @75383 @1
--   tabl = f 1 [1..] where
--      f x zs = ys : f (x + 1) (zs \\ ys) where
--               ys = take x $ filter ((== 0) . (`mod` x)) zs
--   oeis = concat (tabl @75383)

-- instance OEIS 75384 where
--   oeisIx = head . (rowT @75383)

-- instance OEIS 75386 where
--   oeisIx = sum . (rowT @75383)

-- instance OEIS 75387 where
--   oeisIx = product . (rowT @75383)

-- instance OEIS 75388 where
--   oeisIx n = (oeisIx @75384) n `div` n

-- instance OEIS 75390 where
--   oeisIx n = (oeisIx @75386) n `div` n

-- instance OEIS 75425 where
--   oeisIx n = snd $ until ((== 1) . fst)
--                           (\ (x, i) -> (oeisIx x, i + 1)) (n, 0)

-- instance OEIS 75426 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75425))

-- instance OEIS 75432 where
--   oeis = f [2, 4 ..] where
--      f (u:vs@ (v:ws)) | (oeisIx @8966) v == 1 = f ws
--                      | (oeisIx @8966) u == 1 = f vs
--                      | (oeisIx @10051)' (u + 1) == 0 = f vs
--                      | otherwise            = (u + 1) : f vs

-- instance OEIS 75437 where
--   oeis = tablList @75437
-- instance Table 75437 where
--   tabf = iterate rule110 [1] where
--      rule110 row = f ([0,0] ++ row ++ [0,0]) where
--          f [_,_]          = []
--          f (_:ws@ (0:0:_)) = 0 : f ws
--          f (1:ws@ (1:1:_)) = 0 : f ws
--          f (_:ws@ (_:_:_)) = 1 : f ws

-- instance OEIS 75518 where
--   oeisIx = (`div` 4) . (oeisIx @40)

-- instance OEIS 75520 where
--   oeis = zipWith (+) (oeis @1749) (oeis @39702)

-- instance OEIS 75521 where
--   oeis = map (oeisIx @40) $ filter ((== 1) . (oeisIx @10051)' . (oeisIx @75520)) [1..]

-- instance OEIS 75524 where
--   oeis = filter ((== 0) . (oeisIx @10051)') (oeis @75520)

-- instance OEIS 75540 where
--   oeis = map fst $ filter ((== 0) . snd) $
--      zipWith3 (\x y z -> divMod (x + y + z) 3)
--               (oeis @40) (tail (oeis @40)) (drop 2 (oeis @40))

-- instance OEIS 75677 where
--   oeisIx = (oeisIx @265) . subtract 2 . (* 6)

-- instance OEIS 75680 where
--   oeisIx n = snd $ until ((== 1) . fst)
--               (\ (x, i) -> (oeisIx (3 * x + 1), i + 1)) (2 * n - 1, 0)

-- instance OEIS 75877 where
--   oeisIx n = if n < 10 then n else (oeisIx @75877) (n `div` 10) ^ (n `mod` 10)

-- instance OEIS 76191 where
--   oeis = zipWith (-) (tail (oeis @1222)) (oeis @1222)

-- instance OEIS 76217 where
--   oeis = 1 : zipWith (+) (oeis @76217)
--      (zipWith (*) [2..] $ map (oeisIx @57427) $ zipWith (-) [2..] (oeis @76217))

-- instance OEIS 76259 where
--   oeis = zipWith (-) (tail (oeis @5117)) (oeis @5117)

-- instance OEIS 76271 where
--   oeis = iterate (oeisIx @70229) 1

-- instance OEIS 76339 where
--   oeis = filter ((== 1) . (oeisIx @10051)) [1,513..]

-- instance OEIS 76396 where
--   oeisIx = (oeisIx @20639) . (oeisIx @25478)

-- instance OEIS 76397 where
--   oeisIx = (oeisIx @6530) . (oeisIx @25478)

-- instance OEIS 76398 where
--   oeisIx = (oeisIx @1221) . (oeisIx @25478)

-- instance OEIS 76399 where
--   oeisIx n = (oeisIx @1222) (oeisIx n) * (oeisIx @25479) n

-- instance OEIS 76403 where
--   oeisIx = (oeisIx @7947) . (oeisIx @25478)

-- instance OEIS 76404 where
--   oeisIx = (`mod` 2) . (oeisIx @1597)

-- instance OEIS 76405 where
--   oeis = 1 : f (tail $ zip (oeis @1597) (oeis @25478)) where
--      f ((p, r) : us) = g us where
--        g ((q, r') : vs) = if r' == r then q : f us else g vs

-- instance OEIS 76446 where
--   oeis = zipWith (-) (tail (oeis @1694)) (oeis @1694)

-- instance OEIS 76468 where
--   import qualified Data.Set as Set (null)
--   import Data.Set (empty, insert, deleteFindMin)
--   oeisIx n = (oeis @76468) !! (n - 1)
--   oeis = 1 : f [2..] empty where
--      f xs'@ (x:xs) s | Set.null s || m > x ^ 4 = f xs $ insert (x ^ 4, x) s
--                     | m == x ^ 4  = f xs s
--                     | otherwise = m : f xs' (insert (m * b, b) s')
--                     where ((m, b), s') = deleteFindMin s

-- instance OEIS 76489 where
--   oeis = map (length . nub) $
--                  zipWith intersect (tail (tabf @31298)) (tabf @31298)

-- instance OEIS 76490 where
--   oeis = map (length . nub) $
--      zipWith (intersect `on` show) (tail (oeis @40)) (oeis @40)

-- instance OEIS 76566 where
--   oeisIx = (oeisIx @6530) . (* 3) . (+ 1)

-- instance OEIS 76627 where
--   oeisIx n = (oeisIx @5) n * (oeisIx @49820) n

-- instance OEIS 76641 where
--   oeisIx = (oeisIx @4086) . (oeisIx @67251)

-- instance OEIS 76654 where
--   oeis = f (oeis @67251) 1 where
--     f xs z = g xs where
--       g (y:ys) = if (oeisIx @30) y == mod z 10 then y : f (delete y xs) y else g ys

-- instance OEIS 76805 where
--   oeis = filter (not . ("13" `isInfixOf`) . show) (oeis @40)

-- instance OEIS 76845 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10051)'' (n ^ k + n - 1) == 1]

-- instance OEIS 76846 where
--   oeisIx n = n ^ (oeisIx n) + n - 1

-- instance OEIS 76941 where
--   oeisIx n = 2 ^ (oeisIx n) * 3 ^ (oeisIx n)

-- instance OEIS 76948 where
--   oeisIx 1 = 1
--   oeisIx n = if null qs then 0 else head qs
--               where qs = filter ((> 0) . (oeisIx @37213) . subtract 1 . (* n)) [1..n]

-- instance OEIS 77040 where
--   oeis = map (oeisIx . (+ 1)) $ findIndices (<= 0) $
--      zipWith (\s p -> abs s - p) (oeis @77039) (oeis @40)

-- instance OEIS 77041 where
--   oeis = map (oeisIx . (+ 1)) $ findIndices (> 0) $
--      zipWith (\s p -> abs s - p) (oeis @77039) (oeis @40)

-- instance OEIS 77043 where
--   oeis = scanl (+) 0 (oeis @1651)

-- instance OEIS 77063 where
--   oeisIx = (oeisIx @7947) . (oeisIx @6093)

-- instance OEIS 77066 where
--   oeisIx = (oeisIx @7947) . (oeisIx @8864)

-- instance OEIS 77076 where
--   oeis = iterate (oeisIx @55944) 537

-- instance OEIS 77077 where
--   oeis = iterate (oeisIx @55944) 775

-- instance OEIS 77221 where
--   oeis = scanl (+) 0 (oeis @47522)

-- instance OEIS 77223 where
--   oeis = 1 : g 1 [2..]
--     where
--       isOddSquarefree m = odd m && (oeisIx @8966) m == 1
--       g i xs = x : g x (delete x xs)
--         where
--           x = (fromJust $ find isOddSquarefree $ map (+ i))

-- instance OEIS 77267 where
--   oeisIx n = (oeisIx @79978) n + if n < 3 then 0 else (oeisIx @77267) (n `div` 3)

-- instance OEIS 77558 where
--   oeis = tablList @77558
-- instance Table 77558 where
--   rowCol n k = (rowT @77558) n !! (k-1)
--   rowT n = n : genericTake (n - 1)
--                       (filter ((== (oeisIx @46523) n) . (oeisIx @46523)) [n + 1 ..])
--   tabf = map (rowT @77558) [1..]

-- instance OEIS 77581 where
--   oeis = tablList @77581
-- instance Table 77581 where
--   rowCol = rowCol_off @77581 @1 @1
--   rowT   = rowT_off   @77581 @1
--   tabl = map (\x -> take x [z | z <- [1..], gcd x z == 1]) [1..]

-- instance OEIS 77582 where
--   oeisIx = sum . (rowT @77581)

-- instance OEIS 77609 where
--   oeis = tablList @77609
-- instance Table 77609 where
--   rowCol n k = (rowT @77609) n !! (k-1)
--   rowT n = filter
--      (\d -> d == 1 || null (oeisIx_row d \\ (rowT @213925) n)) $ (rowT @27750) n
--   tabf = map (rowT @77609) [1..]

-- instance OEIS 77623 where
--   oeis = 1 : 2 : 4 : zipWith3 (\u v w -> abs (w - v - u))
--                  (oeis @77623) (tail (oeis @77623)) (drop 2 (oeis @77623))

-- instance OEIS 77653 where
--   oeis = 1 : 2 : 2 : zipWith3 (\u v w -> abs (w - v - u))
--                  (oeis @77653) (tail (oeis @77653)) (drop 2 (oeis @77653))

-- instance OEIS 77664 where
--   oeis = tablList @77664
-- instance Table 77664 where
--   rowCol = rowCol_off @77664 @1 @1
--   rowT   = rowT_off   @77664 @1
--   tabl = map (\x -> take x $ filter ((== 1). gcd x) [x + 1 ..]) [1..]

-- instance OEIS 77665 where
--   oeisIx = last . (rowT @77664)

-- instance OEIS 77666 where
--   oeisIx = sum . (rowT @77664)

-- instance OEIS 77800 where
--   oeis = concat $ zipWith (\p q -> if p == q+2 then [q,p] else [])
--                                   (tail (oeis @40)) (oeis @40)

-- instance OEIS 77813 where
--   oeis = filter ((== 1) . (oeisIx @8966)) $ tail $
--      map ((foldr (\d v -> v * 10 + d) 0) . map fi) (tabf @30308)

-- instance OEIS 77854 where
--   oeis = scanl1 xor $ tail (oeis @975) :: [Integer]

-- instance OEIS 77957 where
--   oeisIx = sum . (rowT @204293)

-- instance OEIS 78012 where
--   oeis = 1 : 0 : 0 : 1 : zipWith (+) (oeis @78012)
--      (zipWith (+) (tail (oeis @78012)) (drop 2 (oeis @78012)))

-- instance OEIS 78057 where
--   oeisIx = sum . (rowT @35607)

-- instance OEIS 78065 where
--   oeis = 1 : zipWith (*) (cycle [-1, 1])
--      (zipWith (+) (map (* 2) (oeis @5251)) (map (* 3) $ drop 2 (oeis @5251)))

-- instance OEIS 78134 where
--   oeisIx = p $ drop 2 (oeis @290) where
--      p _          0 = 1
--      p ks'@ (k:ks) x = if x < k then 0 else p ks' (x - k) + p ks x

-- instance OEIS 78147 where
--   oeis = zipWith (-) (tail (oeis @13929)) (oeis @13929)

-- instance OEIS 78174 where
--   oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @1221) x == 0) [2..]

-- instance OEIS 78175 where
--   oeis = filter (\x -> (oeisIx @1414 x) `mod` (oeisIx @1222 x) == 0) [2..]

-- instance OEIS 78178 where
--   oeisIx n = head [k | k <- [2..], (oeisIx @10051)'' (n ^ k + n - 1) == 1]

-- instance OEIS 78179 where
--   oeisIx n = n ^ (oeisIx n) + n - 1

-- instance OEIS 78180 where
--   oeis = 1 : f 1 1 where
--      f x k = y : f y (k+1) where
--        y = head [z | z <- [x+1..], all (q z) $ take k (oeis @78180)]
--        q u v = m > 0 || (oeisIx @10051) u' == 0 where (u',m) = divMod (u-1) v

-- instance OEIS 78241 where
--   oeisIx n = head [x | x <- tail (oeis @169965), mod x n == 0]

-- instance OEIS 78242 where
--   oeisIx n = head [x | x <- tail (oeis @169966), mod x n == 0]

-- instance OEIS 78243 where
--   oeisIx n = head [x | x <- tail (oeis @169967), mod x n == 0]

-- instance OEIS 78244 where
--   oeisIx n = head [x | x <- tail (oeis @169964), mod x n == 0]

-- instance OEIS 78245 where
--   oeisIx n = head [x | x <- tail (oeis @204093), mod x n == 0]

-- instance OEIS 78246 where
--   oeisIx n = head [x | x <- tail (oeis @204094), mod x n == 0]

-- instance OEIS 78247 where
--   oeisIx n = head [x | x <- tail (oeis @204095), mod x n == 0]

-- instance OEIS 78248 where
--   oeisIx n = head [x | x <- tail (oeis @97256), mod x n == 0]

-- instance OEIS 78311 where
--   oeisIx = (oeisIx @20639) . (oeisIx @78310)

-- instance OEIS 78312 where
--   oeisIx = (oeisIx @6530) . (oeisIx @78310)

-- instance OEIS 78313 where
--   oeisIx = (oeisIx @1221) . (oeisIx @78310)

-- instance OEIS 78314 where
--   oeisIx = (oeisIx @1222) . (oeisIx @78310)

-- instance OEIS 78315 where
--   oeisIx = (oeisIx @51904) . (oeisIx @78310)

-- instance OEIS 78316 where
--   oeisIx = (oeisIx @51903) . (oeisIx @78310)

-- instance OEIS 78317 where
--   oeisIx = (oeisIx @5) . (oeisIx @78310)

-- instance OEIS 78318 where
--   oeisIx = (oeisIx @203) . (oeisIx @78310)

-- instance OEIS 78319 where
--   oeisIx = (oeisIx @8472) . (oeisIx @78310)

-- instance OEIS 78320 where
--   oeisIx = (oeisIx @1414) . (oeisIx @78310)

-- instance OEIS 78321 where
--   oeisIx = (oeisIx @10) . (oeisIx @78310)

-- instance OEIS 78322 where
--   oeisIx = (oeisIx @7947) . (oeisIx @78310)

-- instance OEIS 78323 where
--   oeisIx = (oeisIx @3415) . (oeisIx @78310)

-- instance OEIS 78324 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @224866)

-- instance OEIS 78325 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @224866)

-- instance OEIS 78343 where
--   oeis = -1 : 2 : zipWith (+)
--                           (map (* 2) $ tail (oeis @78343)) (oeis @78343)

-- instance OEIS 78358 where
--   oeis = filter ((== 0) . (oeisIx @5369)) [0..]

-- instance OEIS 78430 where
--   oeisIx = sum . (rowT @245717)

-- instance OEIS 78440 where
--   oeis = filter notbp (oeis @196871) where
--      notbp x = m > 0 && x > 1 || m == 0 && notbp x' where
--         (x',m) = divMod x 2

-- instance OEIS 78442 where
--   oeisIx (succ->n) = fst $ until ((== 0) . snd)
--                 (\ (i, p) -> (i + 1, (oeisIx @49084) p)) (-2, (oeisIx @40) n)

-- instance OEIS 78465 where
--   oeisIx n = (oeis @78465) `genericIndex` (n - 1)
--   oeis = 1 : 1 : f 3 where
--      f x = (sum $ map (oeisIx . (x -)) $
--            takeWhile (< x) (oeis @40)) : f (x + 1)

-- instance OEIS 78495 where
--   oeis = [1, 1, 1, 1, 1, 1, 1] ++
--     zipWith div (foldr1 (zipWith (+)) (map b [1,3])) (oeis @78495)
--     where b i = zipWith (*) (drop i (oeis @78495)) (drop (7-i) (oeis @78495))

-- instance OEIS 78613 where
--   oeis = filter ((== 0) . (oeisIx @5094)) [1..]

-- instance OEIS 78637 where
--   oeisIx n = (oeisIx @7947) $ product [n..n+2]

-- instance OEIS 78649 where
--   oeis = map (+ 1) $ filter ((== 0) . (oeisIx @54354)) [1..]

-- instance OEIS 78701 where
--   oeisIx n = if null odds then 1 else head odds
--               where odds = tail $ (rowT @182469) n

-- instance OEIS 78715 where
--   oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @61493)) [1..3999]



-- instance OEIS 78730 where
--   oeisIx n = sum $ zipWith (*) ps $ tail ps where ps = (rowT @27750) n

-- instance OEIS 78779 where
--   oeis = m (oeis @5117) $ map (* 2) (oeis @5117) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y     = x : m xs ys'
--                              | x == y    = x : m xs ys
--                              | otherwise = y : m xs' ys

-- instance OEIS 78812 where
--   oeis = tablList @78812
-- instance Table 78812 where
--   tabl = [1] : [2, 1] : f [1] [2, 1] where
--      f us vs = ws : f vs ws where
--        ws = zipWith (-) (zipWith (+) ([0] ++ vs) (map (* 2) vs ++ [0]))
--                         (us ++ [0, 0])

-- instance OEIS 78822 where
--   oeisIx = genericLength . (rowT @119709)
--   import Numeric (showIntAtBase)

-- instance OEIS 78823 where
--   oeisIx = sum . (rowT @119709)

-- instance OEIS 78826 where
--   oeisIx n | n <= 1 = 0
--             | otherwise = length $ (rowT @225243) n

-- instance OEIS 78829 where
--   oeis = filter ((== 1) . (oeisIx @78826)) [1..]

-- instance OEIS 78832 where
--   oeisIx = head . (rowT @225243)

-- instance OEIS 78833 where
--   oeisIx = last . (rowT @225243)

-- instance OEIS 78834 where
--   import Numeric (showIntAtBase)
--   oeisIx n = fromMaybe 1 $ find (\p -> showIntAtBase 2 ("01" !!) p ""
--                             `isInfixOf` showIntAtBase 2 ("01" !!) n "") $
--                    reverse $ (rowT @27748) n

-- instance OEIS 78894 where
--   oeis = sieve (oeis @40) where
--      sieve (p:ps) = p : sieve [q | (i,q) <- zip [2..] ps, mod i p > 0]

-- instance OEIS 78898 where
--   import Data.IntMap (empty, findWithDefault, insert)
--   oeisIx n = (oeis @78898) !! n
--   oeis = 0 : 1 : f empty 2 where
--      f m x = y : f (insert p y m) (x + 1) where
--              y = findWithDefault 0 p m + 1
--              p = (oeisIx @20639) x

-- instance OEIS 78972 where
--   oeis = filter brilliant (oeis @1358) where
--      brilliant x = (on (==) (oeisIx @55642)) p (x `div` p) where p = (oeisIx @20639) x

-- instance OEIS 79053 where
--   import Data.Set (Set, fromList, notMember, insert)
--   oeisIx n = (oeis @79053) !! (n - 1)
--   oeis = 1 : 2 : r (fromList [1,2]) 1 1 1 2 where
--     r :: Set Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
--     r s i j x y = if v > 0 && v `notMember` s
--                      then v : r (insert v s) j fib y v
--                      else w : r (insert w s) j fib y w where
--       fib = i + j
--       v = x + y - fib
--       w = x + y + fib
--   for_bFile = take 1000 (oeis @79053)

-- instance OEIS 79062 where
--   oeis = 2 : f 2 (tail (oeis @40)) where
--      f x ps = q : f q qs where
--        (q:qs) = dropWhile (\p -> (oeisIx @75802) (p - x) == 0 || p - x == 1) ps

-- instance OEIS 79070 where
--   oeisIx n = genericLength $ elemIndices (oeisIx n) $ map (oeisIx @23416) [1..n - 1]

-- instance OEIS 79080 where
--   oeisIx n = (oeisIx @79079) n `gcd` (oeisIx @23523) (n + 1)

-- instance OEIS 79081 where
--   oeis = zipWith div (oeis @79079) (oeis @79080)

-- instance OEIS 79082 where
--   oeis = zipWith div (tail (oeis @23523)) (oeis @79080)

-- instance OEIS 79083 where
--   oeisIx = (oeisIx @78701) . (oeisIx @79079)

-- instance OEIS 79084 where
--   oeisIx = (oeisIx @6530) . (oeisIx @79079)

-- instance OEIS 79085 where
--   oeisIx = (oeisIx @1221) . (oeisIx @79079)

-- instance OEIS 79086 where
--   oeisIx = (oeisIx @1222) . (oeisIx @79079)

-- instance OEIS 79087 where
--   oeisIx = (oeisIx @51903) . (oeisIx @79079)

-- instance OEIS 79088 where
--   oeisIx = (oeisIx @5) . (oeisIx @79079)

-- instance OEIS 79089 where
--   oeisIx = (oeisIx @203) . (oeisIx @79079)

-- instance OEIS 79090 where
--   oeisIx = (oeisIx @8472) . (oeisIx @79079)

-- instance OEIS 79091 where
--   oeisIx = (oeisIx @1414) . (oeisIx @79079)

-- instance OEIS 79092 where
--   oeisIx = (oeisIx @10) . (oeisIx @79079)

-- instance OEIS 79093 where
--   oeisIx = (oeisIx @7947) . (oeisIx @79079)

-- instance OEIS 79094 where
--   oeisIx = (oeisIx @3415) . (oeisIx @79079)

-- instance OEIS 79095 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @79079)

-- instance OEIS 79124 where
--   oeisIx n = p [1 .. (oeisIx @10) n] n where
--      p _      0 = 1
--      p []     _ = 0
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 79228 where
--   oeisIx n = head [k | k <- [n+1..], (oeisIx @7947) k > (oeisIx @7947) n]

-- instance OEIS 79229 where
--   oeis = f (oeis @7947) where
--      f (x:xs) = ((+ 1) . length $ takeWhile (<= x) xs) : f xs

-- instance OEIS 79364 where
--   oeis = filter
--      (\x -> (oeisIx @10051)' (x - 1) == 0 && (oeisIx @10051)' (x + 1) == 0) (oeis @2808)

-- instance OEIS 79559 where
--   oeisIx = p $ tail (oeis @225) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 79579 where
--   oeisIx 1 = 1
--   oeisIx n = product $ zipWith (*) pfs $ map (subtract 1) pfs
--      where pfs = (rowT @27746) n

-- instance OEIS 79623 where
--   oeis = 1 : 1 : 4 : zipWith3 (\u v w -> abs (w - v - u))
--                  (oeis @79623) (tail (oeis @79623)) (drop 2 (oeis @79623))

-- instance OEIS 79624 where
--   oeis = 1 : 1 : 6 : zipWith3 (\u v w -> abs (w - v - u))
--                  (oeis @79624) (tail (oeis @79624)) (drop 2 (oeis @79624))

-- instance OEIS 79635 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ map ((2 - ) . (`mod` 4)) $ (rowT @27746) n

-- instance OEIS 79648 where
--   oeisIx = sum . map (oeisIx @10051) . (rowT @214084)

-- instance OEIS 79695 where
--   import Data.List.Ordered (minus)
--   oeisIx n = (oeis @79695) !! (n - 1)
--   oeis = [1..] `minus` (oeis @2180)

-- instance OEIS 79890 where
--   oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1222) x == 1 + (oeisIx @1222) n]

-- instance OEIS 79892 where
--   oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1221) x == 1 + (oeisIx @1221) n]

-- instance OEIS 79901 where
--   oeis = tablList @79901
-- instance Table 79901 where
--   tabl = zipWith (map . (^)) [0..] (tabl @2262)

-- instance OEIS 79935 where
--   oeis =
--      1 : 3 : zipWith (-) (map (4 *) $ tail (oeis @79935)) (oeis @79935)

-- instance OEIS 80040 where
--   oeis =
--      2 : 2 : map (* 2) (zipWith (+) (oeis @80040) (tail (oeis @80040)))

-- instance OEIS 80046 where
--   oeis = tablList @80046
-- instance Table 80046 where
--   rowCol = rowCol_off @80046 @1 @1
--   rowT   = rowT_off   @80046 @1
--   tabl = iterate f [1] where
--      f (x:xs) = [x + 1] ++ (zipWith (*) xs $ reverse xs) ++ [x + 1]

-- instance OEIS 80079 where
--   oeisIx n = (length $ takeWhile (< (oeisIx @70940) n) (oeisIx_row n)) + 1

-- instance OEIS 80080 where
--   oeisIx :: Int -> Int -> Int
--   oeisIx n k = addc n k 0 where
--      addc x y z | y == 0    = z - 1
--                 | otherwise = addc (x `xor` y) (shiftL (x .&. y) 1) (z + 1)
--   oeisIx_row n = map (oeisIx n) [1..n]
--   oeisIx_tabl = map (rowT @80080) [1..]

-- instance OEIS 80096 where
--   oeis = 1 : 1 : 2 : zipWith3 (\u v w -> abs (w - v - u))
--                  (oeis @80096) (tail (oeis @80096)) (drop 2 (oeis @80096))

-- instance OEIS 80098 where
--   oeis = tablList @80098
-- instance Table 80098 where
--   rowCol n k = n .|. k :: Int
--   rowT n = map (oeisIx n) [0..n]
--   tabl = map (rowT @80098) [0..]

-- instance OEIS 80099 where
--   oeis = tablList @80099
-- instance Table 80099 where
--   rowCol n k = n .&. k :: Int
--   rowT n = map (oeisIx n) [0..n]
--   tabl = map (rowT @80099) [0..]

-- instance OEIS 80170 where
--   oeis = filter f [1..] where
--      f x = foldl1 gcd (map (flip (oeisIx @7318)' x) [2*x, 3*x .. x* (x+1)]) == 1

-- instance OEIS 80225 where
--   oeisIx n = genericLength [d | d <- takeWhile (<= n) (oeis @396), mod n d == 0]

-- instance OEIS 80237 where
--   oeis = tablList @80237
-- instance Table 80237 where
--   rowCol = rowCol_off @80237 @1 @1
--   rowT   = rowT_off @80237 @1
--   tabf = [1] : f (tabf @80237) where
--      f [[]] =[]
--      f (xs:xss) = concatMap (enumFromTo 1 . (+ 1)) xs : f xss
--   oeis = concat (tabf @80237)

-- instance OEIS 80239 where
--   oeis = 1 : 1 : zipWith (+)
--      (tail (oeis @11765)) (zipWith (+) (oeis @80239) $ tail (oeis @80239))

-- instance OEIS 80257 where
--   oeis = m (oeis @24619) (oeis @33942) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y  = x : m xs ys'
--                              | x == y = x : m xs ys
--                              | x > y  = y : m xs' ys

-- instance OEIS 80367 where
--   oeisIx n = if null us then 0 else fst $ last us
--     where us = filter ((== 1) . snd) $ zip (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 80368 where
--   oeisIx n = if null us then 0 else fst $ head us
--     where us = filter ((== 1) . snd) $ zip (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 80444 where
--   oeis = tablList @80444
-- instance Table 80444 where
--   rowCol = rowCol_off @80444 @1 @1
--   rowT   = rowT_off @80444 @1
--   tabf = zipWith replicate (oeis @1055) [1..]
--   oeis = concat (tabf @80444)

-- instance OEIS 80478 where
--   oeis = 1 : f 1 [2..] where
--      f x (y:ys) | (oeisIx @10051) (x*x + y*y) == 1 = y : (f y ys)
--                 | otherwise                = f x ys

-- instance OEIS 80578 where
--   oeis = 1 : f 2 [1] where
--      f x zs@ (z:_) = y : f (x + 1) (y : zs) where
--        y = if x `elem` zs then z + 1 else z + 3

-- instance OEIS 80579 where
--   oeis = 1 : f 2 [1] where
--      f x zs@ (z:_) = y : f (x + 1) (y : zs) where
--        y = if x `elem` zs then z + 1 else z + 4

-- instance OEIS 80590 where
--   oeis = 1 : f 2 [1] where
--      f x zs@ (z:_) = y : f (x + 1) (y : zs) where
--        y = z + (if x `elem` zs then 3 else 4)

-- instance OEIS 80655 where
--   oeis = 1 : 1 : f 3 where
--      f n = (oeisIx (n - 1) + (oeisIx @80655) (oeisIx (n-1))) : f (n+1)

-- instance OEIS 80670 where
--   oeisIx 1 = 1
--   oeisIx n = read $ foldl1 (++) $
--   zipWith (c `on` show) (oeisIx_row n) (oeisIx_row n) :: Integer
--   where c ps es = if es == "1" then ps else ps ++ es

-- instance OEIS 80672 where
--   oeis = filter ((<= 7) . (oeisIx @20639)) [2..]

-- instance OEIS 80688 where
--   oeis = tablList @80688
--   rowCol n k = (rowT @80688) n !! (k-1)
--   rowT n = map (+ 1) $ take (oeisIx n) $
--                   elemIndices n $ map fromInteger (oeis @64553)
--   tabl = map (rowT @80688) [1..]
--   oeis = concat (tabl @80688)

-- instance OEIS 80709 where
--   oeis = iterate (oeisIx @3132) 4

-- instance OEIS 80715 where
--   oeis = 1 : filter (\x -> all ((== 1) . (oeisIx @10051)) $
--      zipWith (+) (oeisIx_row x) (reverse $ (rowT @27750) x)) [2,4..]

-- instance OEIS 80719 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 .
--              concat . mapMaybe (flip lookup bin) . (rowT @31298)
--               where bin = zip [0..9] (tabf @30308)

-- instance OEIS 80736 where
--   oeisIx n = if n `mod` 4 == 2 then 0 else (oeisIx @10) n

-- instance OEIS 80764 where
--   oeis = tail $ zipWith (-) (tail (oeis @49472)) (oeis @49472)

-- instance OEIS 80786 where
--   oeis = tablList @80786
-- instance Table 80786 where
--   rowCol = rowCol_off @80786 @1 @1
--   rowT   = rowT_off   @80786 @1
--   tabl = map reverse $ iterate f [1] where
--      f xs@ (x:_) = (x + 1) :
--                   (zipWith (+) xs (map (fromEnum . (lpf <=)) [x, x-1 ..]))
--           where lpf = fromInteger $ (oeisIx @6530) $ fi (x + 1)

-- instance OEIS 80788 where
--   oeisIx n = (oeis @48890) !! (n - 1)
--   oeis = filter f (oeis @40) where
--      f x = all (`elem` [0,1,6,8,9]) ds && x' /= x && (oeisIx @10051) x' == 1
--        where x' = foldl c 0 ds
--              c v 6 = 10*v + 9; c v 9 = 10*v + 6; c v d = 10*v + d
--              ds = unfoldr d x
--              d z = if z == 0 then Nothing else Just $ swap $ divMod z 10

-- instance OEIS 80941 where
--   oeisIx n = if null ds then 0 else head ds  where
--               ds = filter ((flip isPrefixOf `on` (rowT @30308)) n) $
--                           reverse $ (rowT @27751) n

-- instance OEIS 80942 where
--   oeisIx n = genericLength $
--               filter ((flip isPrefixOf `on` (rowT @30308)) n) $ (rowT @27750) n

-- instance OEIS 80943 where
--   oeis = filter ((== 2) . (oeisIx @80942)) [1..]

-- instance OEIS 80944 where
--   oeis = filter ((<= 2) . (oeisIx @80942)) [1..]

-- instance OEIS 80945 where
--   oeis = filter ((> 2) . (oeisIx @80942)) [1..]

-- instance OEIS 80946 where
--   oeis = filter ((== 3) . (oeisIx @80942)) [1..]

-- instance OEIS 80947 where
--   oeis = filter ((> 3) . (oeisIx @80942)) [1..]

-- instance OEIS 80982 where
--   oeisIx n = (+ 1) $ fromJust $
--      findIndex ((== 0) . (`mod` (n ^ 2))) $ tail (oeis @217)

-- instance OEIS 80983 where
--   oeisIx = (oeisIx @217) . (oeisIx @80982)

-- instance OEIS 80995 where
--   oeisIx = (oeisIx @33683) . (+ 1) . (* 24)

-- instance OEIS 81091 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @14311)

-- instance OEIS 81092 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @52294)

-- instance OEIS 81118 where
--   oeis = tablList @81118
--   rowCol = rowCol_off @81118 @1 @1
--   rowT   = rowT_off   @81118 @1
--   tabl  = iterate
--      (\row -> (map ((+ 1) . (* 2)) row) ++ [4 * (head row) + 1]) [1]
--   oeis = concat (tabl @81118)

-- instance OEIS 81146 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @81145))

-- instance OEIS 81238 where
--   oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
--                            (oeisIx @8683) u * (oeisIx @8683) v == -1]

-- instance OEIS 81239 where
--   oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
--                            (oeisIx @8683) u * (oeisIx @8683) v == 0]

-- instance OEIS 81240 where
--   oeisIx n = genericLength [ () | u <- [1..n], v <- [1..n],
--                            (oeisIx @8683) u * (oeisIx @8683) v == 1]

-- instance OEIS 81308 where
--   oeisIx n = sum $ map (oeisIx' . (n -)) $ takeWhile (< n) (oeis @3586)

-- instance OEIS 81309 where
--   oeisIx n | null ps   = 0
--             | otherwise = head ps
--             where ps = [p | p <- takeWhile (< n) (oeis @40),
--                             (oeisIx @65333) (n - p) == 1]

-- instance OEIS 81310 where
--   oeis = filter ((== 0) . (oeisIx @81308)) [1..]

-- instance OEIS 81311 where
--   oeisIx n = (oeis @81310) !! (n - 1)
--   oeis = filter ((== 0) . (oeisIx @81308)) [1..]

-- instance OEIS 81312 where
--   oeis = filter ((== 1) . (oeisIx @81308)) [1..]

-- instance OEIS 81313 where
--   oeis = filter ((> 1) . (oeisIx @81308)) [1..]

-- instance OEIS 81324 where
--   oeis = 0 : elemIndices 1 (oeis @63725)

-- instance OEIS 81382 where
--   oeisIx 1 = 1
--   oeisIx n = head [x | let sopf = (oeisIx @8472) n, x <- [n+1..], (oeisIx @8472) x == sopf]

-- instance OEIS 81407 where
--   oeisIx n = (oeis @81408) !! n
--   oeis = 1 : 1 : 1 : 1 : zipWith (*) [5..] (oeis @81407)

-- instance OEIS 81408 where
--   oeis = 1 : 1 : 1 : 1 : zipWith (*) [5..] (oeis @81407)

-- instance OEIS 81577 where
--   oeis = tablList @81577
-- instance Table 81577 where
--   tabl = map fst $ iterate
--       (\ (us, vs) -> (vs, zipWith (+) (map (* 2) ([0] ++ us ++ [0])) $
--                          zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

-- instance OEIS 81578 where
--   oeis = tablList @81578
-- instance Table 81578 where
--   tabl = map fst $ iterate
--      (\ (us, vs) -> (vs, zipWith (+) (map (* 3) ([0] ++ us ++ [0])) $
--                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

-- instance OEIS 81603 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @81603) n' + m `div` 2 where (n',m) = divMod n 3

-- instance OEIS 81619 where
--   oeis = filter ((== 1) . (oeisIx @10054) . (oeisIx @5)) [1..]

-- instance OEIS 81729 where
--   oeisIx n = (oeisIx @209229) n + (oeisIx @33999) (n)

-- instance OEIS 81770 where
--   oeis = filter ((== 1) . (oeisIx @8966) . (`div` 4)) (oeis @17113)

-- instance OEIS 81827 where
--   oeis = zipWith (-) (tail (oeis @5185)) (oeis @5185)
--   b081827 = bFile "A081827" (oeis @81827) 1 10000

-- instance OEIS 81828 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @81827)

-- instance OEIS 81829 where
--   oeis = map (+ 1) $ findIndices (< 0) (oeis @81827)

-- instance OEIS 81830 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @81827)

-- instance OEIS 81834 where
--   oeis = 1 : f 2 [1] where
--      f x zs@ (z:_) = y : f (x + 1) (y : zs) where
--        y = z + (if x `elem` zs then 4 else 3)

-- instance OEIS 81848 where
--   oeis = 3 : tail (zipWith (-) (tail (oeis @70885)) (oeis @70885))

-- instance OEIS 82143 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx $ n - 1) * (oeisIx n)

-- instance OEIS 82416 where
--   oeis = map (`mod` 2) (oeis @73941)

-- instance OEIS 82495 where
--   oeisIx n = (oeisIx @15910) n + (oeisIx @48298) n - 1

-- instance OEIS 82498 where
--   oeis = 0 : 1 : concat
--      (zipWith (\u v -> [u, u + v]) (tail (oeis @82498)) (oeis @82498))

-- instance OEIS 82560 where
--   oeis = tablList @82560
-- instance Table 82560 where
--   rowCol = rowCol_off @82560 @1 @1
--   rowT   = rowT_off @82560 @1
--   tabf = iterate (concatMap (\x -> [x + 1, 2 * x + 2])) [1]
--   oeis = concat (tabf @82560)

-- instance OEIS 82582 where
--   oeis = 1 : 1 : f [1,1] where
--      f xs'@ (x:_:xs) = y : f (y : xs') where
--        y = x + sum (zipWith (*) xs' $ reverse xs)

-- instance OEIS 82587 where
--   oeis = concat $ transpose [tail (oeis @204), (oeis @204)]

-- instance OEIS 82601 where
--   oeis = tablList @82601
-- instance Table 82601 where
--   tabl = [1] : [1,0] : [1,1,0] : f [0,0,1] [0,1,0] [1,1,0]
--      where f us vs ws = ys : f (0:vs) (0:ws) ys where
--                         ys = zipWith3 (((+) .) . (+)) us vs ws ++ [0]

-- instance OEIS 82763 where
--   oeis = filter (containsL . (oeisIx @61493)) [1..3999] where
--      containsL x = d == 4 || x > 0 && containsL x' where
--                    (x',d) = divMod x 10

-- instance OEIS 82766 where
--   oeis = concat $ transpose [oeis, tail (oeis @1333)]

-- instance OEIS 82784 where
--   oeisIx = (oeisIx @7) . (`mod` 7)
--   oeis = cycle [1,0,0,0,0,0,0]

-- instance OEIS 82792 where
--   oeisIx n = until ((== 3) . (oeisIx @30)) (+ n) n

-- instance OEIS 82794 where
--   oeisIx n = until ((== 4) . (oeisIx @30)) (+ n) n

-- instance OEIS 82795 where
--   oeisIx n = until ((== 5) . (oeisIx @30)) (+ n) n

-- instance OEIS 82796 where
--   oeisIx n = until ((== 6) . (oeisIx @30)) (+ n) n

-- instance OEIS 82797 where
--   oeisIx n = until ((== 7) . (oeisIx @30)) (+ n) n

-- instance OEIS 82798 where
--   oeisIx n = until ((== 8) . (oeisIx @30)) (+ n) n

-- instance OEIS 82811 where
--   oeisIx n = until ((== 2) . (oeisIx @30)) (+ n) n

-- instance OEIS 82870 where
--   oeis = tablList @82870
-- instance Table 82870 where
--   tabf = map (takeWhile (> 0)) (tabl @82601)

-- instance OEIS 82949 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @82949) !! (n - 1)
--   oeis = f $ singleton (2 ^ 3 * 3 ^ 2, 2, 3) where
--      f s = y : f (if p' < q then insert (p' ^ q * q ^ p', p', q) s'' else s'')
--            where s'' = insert (p ^ q' * q' ^ p, p, q') s'
--                  p' = (oeisIx @151800) p; q' = (oeisIx @151800) q
--                  ((y, p, q), s') = deleteFindMin s

-- instance OEIS 83025 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength [x | x <- (rowT @27746) n, mod x 4 == 1]

-- instance OEIS 83207 where
--   oeis = filter (z 0 0 . (rowT @27750)) $ [1..] where
--      z u v []     = u == v
--      z u v (p:ps) = z (u + p) v ps || z u (v + p) ps

-- instance OEIS 83278 where
--   import Data.Set (empty, findMin, deleteMin, deleteMin, insert)
--   import qualified Data.Set as Set (null)
--   oeisIx n = (oeis @83278) !! (n - 1)
--   oeis = 1 : f empty (drop 2 (oeis @2275)) where
--      f rups rus'@ (ru:rus)
--        | Set.null rups || m > ru = f (insert (ru,ru) rups) rus
--        | otherwise = m : f (insert (m*m',m') (deleteMin rups)) rus'
--        where (m,m') = findMin rups

-- instance OEIS 83337 where
--   oeis =
--      0 : 3 : map (* 2) (zipWith (+) (oeis @83337) (tail (oeis @83337)))

-- instance OEIS 83347 where
--   oeis = filter ((< 0) . (oeisIx @168036)) [1..]

-- instance OEIS 83348 where
--   oeis = filter ((> 0) . (oeisIx @168036)) [1..]

-- instance OEIS 83368 where
--   oeis = concat $ h $ drop 2 (oeis @71) where
--      h (a:fs@ (a':_)) = (map (oeisIx . (a' -)) [a .. a' - 1]) : h fs

-- instance OEIS 83382 where
--   oeisIx n = f n n (oeis @10051) where
--      f m 0 _     = m
--      f m k chips = f (min m $ sum chin) (k - 1) chips' where
--        (chin,chips') = splitAt n chips

-- instance OEIS 83383 where
--   oeis = 1 : f 0 [2..] (tail (oeis @83382)) where
--      f m (x:xs) (y:ys) | y <= m    = f m xs ys
--                        | otherwise = x : f y xs ys

-- instance OEIS 83399 where
--   oeisIx = (+ 1) . (oeisIx @1221)

-- instance OEIS 83414 where
--   oeisIx n = minimum $ map c $ filter ((== 1) . (gcd n)) [1..n] where
--      c k = sum $ map (oeisIx @10051) $ enumFromThenTo k (k + n) (n ^ 2)

-- instance OEIS 83415 where
--   oeis = tablList @83415
-- instance Table 83415 where
--   rowCol n k = (rowT @83415) n !! (k-1)
--   rowT n = f n (oeis @10051) where
--      f 0 _     = []
--      f k chips = (sum chin) : f (k - 1) chips' where
--        (chin,chips') = splitAt n chips
--   tabl = map (rowT @83415) [1..]

-- instance OEIS 83479 where
--   oeis = m [0..] (oeis @33638) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x <= y    = x : m xs ys'
--                              | otherwise = y : m xs' ys

-- instance OEIS 83533 where
--   oeis = zipWith (-) (tail (oeis @2202)) (oeis @2202)

-- instance OEIS 83534 where
--   oeis = zipWith (-) (tail (oeis @7617)) (oeis @7617)

-- instance OEIS 83752 where
--   oeisIx n = head [k | k <- [n+1..], (oeisIx @10052) (12* (k+n)^2 + k*n) == 1]

-- instance OEIS 83866 where
--   oeis = filter ((== 0) . (oeisIx @4718)) [0..]

-- instance OEIS 83910 where
--   oeisIx = sum . map (oeisIx . (oeisIx @10879)) . (rowT @27750)

-- instance OEIS 84110 where
--   oeisIx = foldl (/*) 1 . (rowT @27750) where
--      x /* y = if m == 0 then x' else x*y where (x',m) = divMod x y

-- instance OEIS 84111 where
--   oeis = [x | x <- [1..], (oeisIx @84110) x == x]

-- instance OEIS 84112 where
--   oeis = filter ((== 0) . (oeisIx @10051)') (oeis @84111)

-- instance OEIS 84113 where
--   oeisIx = f 0 1 . (rowT @27750) where
--      f c _ []     = c
--      f c x (d:ds) = if r == 0 then f c x' ds else f (c + 1) (x * d) ds
--                     where (x', r) = divMod x d

-- instance OEIS 84114 where
--   oeisIx = g 0 1 . tail . (rowT @27750) where
--      g c _ []     = c
--      g c x (d:ds) = if r > 0 then g c (x * d) ds else g (c + 1) x' ds
--                     where (x', r) = divMod x d

-- instance OEIS 84115 where
--   oeisIx n = (oeisIx @84113) n - (oeisIx @84114) n

-- instance OEIS 84116 where
--   oeis = filter ((== 1) . (oeisIx @84115)) [1..]

-- instance OEIS 84126 where
--   oeisIx = (oeisIx @20639) . (oeisIx @1358)

-- instance OEIS 84127 where
--   oeisIx = (oeisIx @6530) . (oeisIx @1358)

-- instance OEIS 84190 where
--   oeisIx 1 = 1
--   oeisIx n = foldl1 lcm $ map (subtract 1) $ tail $ (rowT @27750)' n

-- instance OEIS 84196 where
--   oeis = f [] (oeis @40) where
--      f ps' (p:ps) = length [q | q <- ps', mod (p + 1) (q + 1) == 0] :
--                     f (p : ps') ps where

-- instance OEIS 84198 where
--   oeis = map (oeisIx @40) $ filter ((== 1) . (oeisIx @84196)) [1..]

-- instance OEIS 84345 where
--   oeis = filter ((== 0) . (oeisIx @10051)' . (oeisIx @120)) [0..]

-- instance OEIS 84349 where
--   oeis = 1 : filter (\x -> all (== 0) $ map (oeisIx . (x -)) $
--                              takeWhile (<= x) (oeis @290)) (oeis @5117)

-- instance OEIS 84471 where
--   oeisIx 1 = 1
--   oeisIx x = 2 * (2 - d) * (oeisIx @84471) x' + d  where (x',d) = divMod x 2

-- instance OEIS 84473 where
--   oeisIx 1 = 1
--   oeisIx x = 2 * (if b == 1 then 1 else 8) * (oeisIx @84473) x' + b
--               where (x', b) = divMod x 2

-- instance OEIS 84558 where
--   oeisIx n = (oeisIx @90529) (n + 1) - 1

-- instance OEIS 84600 where
--   oeis = concat $ iterate ([1,1,2] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 84608 where
--   oeis = concat $ iterate ([1,2,3] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 84888 where
--   oeisIx = (oeisIx @25426) . (oeisIx @578)

-- instance OEIS 84933 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @84937)) + 1

-- instance OEIS 85079 where
--   oeisIx n = product $ zipWith (^) (oeisIx_row n) (sort $ (rowT @124010) n)

-- instance OEIS 85238 where
--   oeisIx n = e (mod x 2 + 2) x where
--      x = (oeisIx @6899) n
--      e b p = if p == 1 then 0 else 1 + e b (p `div` b)

-- instance OEIS 85239 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @6899) n `mod` 2 + 2

-- instance OEIS 85392 where
--   oeisIx = (oeisIx @6530) . (oeisIx @32742)

-- instance OEIS 85423 where
--   oeisIx = (oeisIx @523) . (oeisIx @8585)

-- instance OEIS 85478 where
--   oeis = tablList @85478
-- instance Table 85478 where
--   tabl = zipWith (zipWith (oeisIx @7318)) (tabl @51162) (tabl @25581)

-- instance OEIS 85513 where
--   import Data.Text (Text); import qualified Data.Text as T (unpack))
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx = genericLength . filter (== 'e') . T.unpack . numeral where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 85604 where
--   oeis = tablList @85604
-- instance Table 85604 where
--   rowCol = rowCol_off @85604 @2 @1
--   rowT 1 = [0]
--   rowT n = (rowT @115627) n ++ (take $ (oeisIx @62298) $ fi n) [0,0..]
--   tabl = map (rowT @85604) [1..]

-- instance OEIS 85612 where
--   oeis = tablList @85612
-- instance Table 85612 where
--   rowCol n k = (rowT @85612) n !! (k-1)
--   rowT   = rowT_off @85612 @1
--   tabf = f 0 $ zip [1..] (oeis @46523) where
--      f x zs'@ (z:zs) = (map fst ys) : f (x + 1) (zs' \\ ys) where
--        ys = z : take x (filter ((== snd z) . snd) zs)
--   oeis = concat (tabf @85612)

-- instance OEIS 85713 where
--   import Data.List.Ordered (insertBag)
--   oeisIx n = (oeis @85713) !! (n - 1)
--   oeis = 1 : r yx3ss where
--      r (ps:pss) | (oeisIx @10051)' cd == 1 &&
--                   map (flip div cd) ps == [3, 4, 6] = cd : r pss
--                 | otherwise = r pss  where cd = foldl1 gcd ps
--      yx3ss = filter ((== 3) . length) $
--          map (map snd) $ groupBy ((==) `on` fst) $
--          f [1..] (oeis @2110) []
--          where f is'@ (i:is) ps'@ (p:ps) yxs
--                 | i < p = f is ps' $ insertBag (oeisIx' i, i) yxs
--                 | otherwise = yxs' ++ f is' ps yxs''
--                 where (yxs', yxs'') = span ((<= (oeisIx @10)' i) . fst) yxs

-- instance OEIS 85721 where
--   oeis = [p*q | (p,q) <- zip (oeis @84126) (oeis @84127),
--                         oeisIx p == (oeisIx @70939) q]

-- instance OEIS 85730 where
--   oeisIx 1 = 1
--   oeisIx n = (p - 1) * p ^ (e - 1)
--      where p =  (oeisIx @25473) n; e =  (oeisIx @25474) n

-- instance OEIS 85731 where
--   oeisIx n = gcd n $ (oeisIx @3415) n

-- instance OEIS 85732 where
--   oeisIx n = (oeis @85732) !! (n - 2)
--   oeis = map f $ drop 2 $ inits $ concatMap show (oeis @796)
--      where f xs = minimum $ init $ tail $
--                   zipWith (on (+) read) (inits xs) (tails xs)

-- instance OEIS 85787 where
--   oeis = scanl (+) 0 (oeis @80512)

-- instance OEIS 85809 where

-- instance OEIS 85974 where
--   oeisIx = count0 0 . (oeisIx @40) where
--      count0 c x | d == 0    = if x < 10 then c + 1 else count0 (c + 1) x'
--                 | otherwise = if x < 10 then c else count0 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85975 where
--   oeisIx = count1 0 . (oeisIx @40) where
--      count1 c x | d == 1    = if x < 10 then c + 1 else count1 (c + 1) x'
--                 | otherwise = if x < 10 then c else count1 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85976 where
--   oeisIx = count2 0 . (oeisIx @40) where
--      count2 c x | d == 2    = if x < 10 then c + 1 else count2 (c + 1) x'
--                 | otherwise = if x < 10 then c else count2 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85977 where
--   oeisIx = count3 0 . (oeisIx @40) where
--      count3 c x | d == 3    = if x < 10 then c + 1 else count3 (c + 1) x'
--                 | otherwise = if x < 10 then c else count3 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85978 where
--   oeisIx = count4 0 . (oeisIx @40) where
--      count4 c x | d == 4    = if x < 10 then c + 1 else count4 (c + 1) x'
--                 | otherwise = if x < 10 then c else count4 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85979 where
--   oeisIx = count5 0 . (oeisIx @40) where
--      count5 c x | d == 5    = if x < 10 then c + 1 else count5 (c + 1) x'
--                 | otherwise = if x < 10 then c else count5 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85980 where
--   oeisIx = count6 0 . (oeisIx @40) where
--      count6 c x | d == 6    = if x < 10 then c + 1 else count6 (c + 1) x'
--                 | otherwise = if x < 10 then c else count6 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85981 where
--   oeisIx = count7 0 . (oeisIx @40) where
--      count7 c x | d == 7    = if x < 10 then c + 1 else count7 (c + 1) x'
--                 | otherwise = if x < 10 then c else count7 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85982 where
--   oeisIx = count8 0 . (oeisIx @40) where
--      count8 c x | d == 8    = if x < 10 then c + 1 else count8 (c + 1) x'
--                 | otherwise = if x < 10 then c else count8 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 85983 where
--   oeisIx = count9 0 . (oeisIx @40) where
--      count9 c x | d == 9    = if x < 10 then c + 1 else count9 (c + 1) x'
--                 | otherwise = if x < 10 then c else count9 c x'
--                 where (x', d) = divMod x 10

-- instance OEIS 86005 where
--   oeis = filter
--      (\x -> (oeisIx @64911) (x - 1) == 1 && (oeisIx @64911) (x + 1) == 1) (oeis @100484)

-- instance OEIS 86006 where
--   oeisIx = flip div 2 . (oeisIx @86005)

-- instance OEIS 86283 where
--   oeisIx n = x086283_list !! (n - 1)
--   oeis = 1 : 1 : f 1 0 [1..] where
--      f x y (z:zs) = u : f u (abs $ x - u) zs where
--        u = minimum [if v < x then x - v else x + v |
--                     v <- if y < z then [y + z] else [y + z, y - z]]

-- instance OEIS 86299 where
--   oeisIx = fromEnum . (<= 7) . (oeisIx @6530)

-- instance OEIS 86417 where
--   oeisIx n = (2 ^ (oeisIx n + 1) - 1) * (3 ^ (oeisIx n + 1) - 1) `div` 2

-- instance OEIS 86457 where
--   oeis = filter (\x -> (oeisIx @30) x == (oeisIx @30) (x^2) &&
--                                oeisIx x == (oeisIx @10879) (x^2)) [0..]

-- instance OEIS 86517 where
--   oeis = 1 : f 1 [3, 5 ..] where
--      f x zs = g zs where
--        g (y:ys) = if (oeisIx @10051)' ((x + y) `div` 2) == 1
--                      then y : f y (delete y zs) else g ys

-- instance OEIS 86518 where
--   oeis = zipWith ((flip div 2 .) . (+))
--                          (oeis @86517) $ tail (oeis @86517)

-- instance OEIS 86754 where
--   oeis = concat $ concat $ iterate ([[1,1],[1,1]] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 86793 where
--   oeisIx = f 0 where
--      f y x = if x == 15 then y else f (y + 1) (oeisIx x)

-- instance OEIS 86799 where
--   oeisIx n | even n    = (oeisIx $ div n 2) * 2 + 1
--             | otherwise = n

-- instance OEIS 86862 where
--   oeis = zipWith (-) (tail (oeis @2113)) (oeis @2113)

-- instance OEIS 86892 where
--   oeis = tail $ zipWith gcd (oeis @225) (oeis @3462)

-- instance OEIS 86901 where
--   oeis = 1 : 1 : zipWith (+)
--                  (map (* 3) (oeis @86901)) (map (* 4) $ tail (oeis @86901))

-- instance OEIS 86971 where
--   oeisIx = sum . map (oeisIx @64911) . (rowT @27750)

-- instance OEIS 87039 where
--   oeisIx n | null ps   = 1
--             | otherwise = head ps
--             where ps = tail $ reverse $ (rowT @27746) n

-- instance OEIS 87112 where
--   oeis = tablList @87112
-- instance Table 87112 where
--   rowCol = rowCol_off @87112 @1 @1
--   rowT n = map (* last ps) ps where ps = take n (oeis @40)
--   tabl = map (rowT @87112) [1..]

-- instance OEIS 87188 where
--   oeisIx = p (oeis @5117) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 87207 where
--   oeisIx = sum . map ((2 ^) . (subtract 1) . (oeisIx @49084)) . (rowT @27748)

-- instance OEIS 87279 where
--   oeis = 0 : 2 : f (drop 2 (oeis @290))
--      where f (x:xs) = x-1 : x+1 : f xs

-- instance OEIS 87349 where
--   oeisIx n = (oeisIx @20639) (n + 1) + n

-- instance OEIS 87370 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . subtract 1 . (* 3)) [0..]

-- instance OEIS 87401 where
--   oeis = tablList @87401
-- instance Table 87401 where
--   tabl = iterate f [0] where
--      f row = row' ++ [last row'] where row' = zipWith (+) row [0..]

-- instance OEIS 87624 where
--   oeisIx n = if (oeisIx @10051) n == 1 then 0 else (oeisIx @1221) n

-- instance OEIS 87686 where
--   oeis = map succ $ findIndices (> 1) (oeis @51135)

-- instance OEIS 87695 where
--   oeis = filter
--      (\x -> (oeisIx @10051)' (x - 3) == 1 && (oeisIx @10051)' (x + 3) == 1) [2, 4 ..]

-- instance OEIS 87712 where
--   oeisIx 1 = 1
--   oeisIx n = read $ concatMap (show . (oeisIx @49084)) $ (rowT @27746) n :: Integer

-- instance OEIS 87713 where
--   oeisIx = (oeisIx @6530) . (oeisIx @84920)

-- instance OEIS 87808 where
--   oeis = 0 : concat
--      (transpose [map (+ 1) (oeis @87808), map (* 2) $ tail (oeis @87808)])

-- instance OEIS 87875 where
--   oeis = 1 : 1 : zipWith (+)
--      (map (oeisIx @87875) $ zipWith (-) [3..] (oeis @87875))
--      (map (oeisIx @720) $ zipWith (-) [3..] $ tail (oeis @720))

-- instance OEIS 87897 where
--   oeisIx = p [3,5..] where
--      p [] _ = 0
--      p _  0 = 1
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 87980 where
--   oeis = 1 : filter f [2..] where
--      f x = isPrefixOf ps (oeis @40) && all (< 0) (zipWith (-) (tail es) es)
--            where ps = (rowT @27748) x; es = (rowT @124010) x

-- instance OEIS 88208 where
--   oeis = tablList @88208
-- instance Table 88208 where
--   rowCol = rowCol_off @88208 @1 @1
--   rowT   = rowT_off @88208 @1
--   tabf = iterate f [1] where
--      f vs = (map (subtract 1) ws) ++ reverse ws where ws = map (* 2) vs

-- instance OEIS 88209 where
--   oeis = zipWith (+) (oeis @45) $ tail (oeis @45925)

-- instance OEIS 88226 where
--   oeis = 0 : 0 : 1 : zipWith3 (\u v w -> abs (w - v - u))
--                  (oeis @88226) (tail (oeis @88226)) (drop 2 (oeis @88226))

-- instance OEIS 88314 where
--   oeisIx = sum . concat . ps 1 where
--      ps _ 0 = [[]]
--      ps i j = [t:ts | t <- [i..j], ts <- ps t (j - t)]

-- instance OEIS 88359 where
--   oeis = map succ $ elemIndices 1 (oeis @51135)

-- instance OEIS 88380 where
--   oeisIx n = (oeis @88382) !! (n - 1)
--   oeis = [x | x <- [1..], x <= (oeisIx @20639) x ^ 3]

-- instance OEIS 88381 where
--   oeis = filter f [1..] where
--                         f x = p ^ 2 < div x p  where p = (oeisIx @20639) x

-- instance OEIS 88382 where
--   oeis = [x | x <- [1..], x <= (oeisIx @20639) x ^ 4]

-- instance OEIS 88383 where
--   oeis = [x | x <- [1..], x  (oeisIx @20639) x ^ 4]

-- instance OEIS 88442 where
--   oeisIx = (+ 1) . (oeisIx @4514)

-- instance OEIS 88517 where
--   oeisIx n = (oeisIx @1462) (n + 1) - (oeisIx @1462) n
--   oeis = zipWith (-) (tail (oeis @1462)) (oeis @1462)



-- instance OEIS 88580 where
--   oeisIx = (+ 1) . (oeisIx @203)

-- instance OEIS 88631 where
--   oeisIx n = (oeisIx @60265) n - n

-- instance OEIS 88643 where
--   oeisIx_tabl = map (rowT @88643) [1..]
--   oeisIx n k = (rowT @88643) n !! (k-1)
--   oeisIx_row n = n : f n [n - 1, n-2 .. 1] where
--      f u vs = g vs where
--        g []                            = []
--        g (x:xs) | (oeisIx @10051) (x + u) == 1 = x : f x (delete x vs)
--                 | otherwise            = g xs

-- instance OEIS 88670 where
--   oeisIx = p $ tail (oeis @10785) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 88707 where
--   oeisIx = (+ 1) . (oeisIx @1358)

-- instance OEIS 88723 where
--   oeis = filter f [2..] where
--      f x = 1 `elem` (zipWith (-) (tail divs) divs)
--            where divs = tail $ (rowT @27750) x

-- instance OEIS 88732 where
--   oeisIx n = head [q | q <- [2 * n + 1, 3 * n + 2 ..], (oeisIx @10051)' q == 1]

-- instance OEIS 88733 where
--   oeisIx n = last $ take n $
--               [q | q <- [2 * n + 1, 3 * n + 2 ..], (oeisIx @10051)' q == 1]

-- instance OEIS 88763 where
--   oeisIx = flip div 2 . (oeisIx @87695)

-- instance OEIS 88864 where
--   oeisIx 1 = 0
--   oeisIx n = maximum $ zipWith ((*) `on` foldr (\d v -> v * 2 + d) 0)
--               (init $ tail $ inits bs) (init $ tail $ tails bs)
--               where bs = (rowT @30308) n

-- instance OEIS 88956 where
--   oeis = tablList @88956
-- instance Table 88956 where
--   rowCol n k =  (oeisIx @95890) (n + 1) (k + 1) * (oeisIx @7318)' n k `div` (n - k + 1)
--   rowT n = map (oeisIx n) [0..n]
--   tabl = map (rowT @88956) [0..]

-- instance OEIS 88957 where
--   oeisIx = sum . (rowT @88956)

-- instance OEIS 89072 where
--   oeisIx = flip (^)
--   oeisIx_row n = map (oeisIx n) [1..n]
--   oeisIx_tabl = map (rowT @89072) [1..]

-- instance OEIS 89189 where
--   oeisIx n = (oeis @97375) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @212793) . (subtract 1)) (oeis @40)

-- instance OEIS 89194 where
--   oeis = filter ((== 1) . (oeisIx @212793) . (+ 1)) (oeis @97375)

-- instance OEIS 89224 where
--   oeisIx = (oeisIx @23416) . (oeisIx @23416)

-- instance OEIS 89233 where
--   oeisIx n = sum $ [oeisIx $ gcd u v | let ds = tail $ (rowT @27750) n,
--                                          u <- ds, v <- dropWhile (<= u) ds]

-- instance OEIS 89237 where
--   oeis = merge (oeis @40) (oeis @290) where
--      merge xs'@ (x:xs) ys'@ (y:ys) =
--            if x < y then x : merge xs ys' else y : merge xs' ys

-- instance OEIS 89247 where
--   oeisIx n = product $ zipWith (^)
--                         (oeisIx_row n) (reverse $ sort $ (rowT @124010) n)

-- instance OEIS 89341 where
--   oeis = filter (\x -> (oeisIx @6530) x < 2 * (oeisIx @20639) x) (oeis @24619)

-- instance OEIS 89589 where
--   import Data.Text (Text); import qualified Data.Text as T (all)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @89589) !! (n - 1)
--   oeis = filter (T.all (/= 'i') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 89590 where
--   import Data.Text (Text); import qualified Data.Text as T (all)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @89590) !! (n - 1)
--   oeis = filter (T.all (/= 'u') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 89610 where
--   oeisIx n = sum $ map (oeisIx @10051)' [n^2 .. n* (n+1)]

-- instance OEIS 89625 where
--   oeisIx n = f n 0 (oeis @40) where
--      f 0 y _      = y
--      f x y (p:ps) = f x' (y + p * r) ps where (x',r) = divMod x 2

-- instance OEIS 89648 where
--   oeis = filter ((<= 1) . abs . (oeisIx @37861)) [0..]

-- instance OEIS 89898 where
--   oeisIx n = if n < 10 then n + 1 else (d + 1) * (oeisIx @89898) n'
--               where (n', d) = divMod n 10

-- instance OEIS 89911 where
--   oeis = 0 : 1 : zipWith (\u v -> (u + v) `mod` 12)
--                          (tail (oeis @89911)) (oeis @89911)

-- instance OEIS 89951 where
--   oeis = [x | x <- [0..], (oeisIx @30) x == (oeisIx @30) (x ^ 2)]

-- instance OEIS 89999 where
--   oeisIx = (oeisIx @217) . (oeisIx @211201)

-- instance OEIS 90050 where
--   oeis = [x | x <- [1..], (oeisIx @87117) x == (oeisIx @38374) x]

-- instance OEIS 90076 where
--   oeis = zipWith (*) (oeis @40) $ drop 2 (oeis @40)

-- instance OEIS 90079 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 . map head . group . (rowT @30308)

-- instance OEIS 90127 where
--   oeis = f (oeis @10) [] where
--     f (x:xs) phis | x `elem` phis = f xs phis
--                   | otherwise     = x : f xs (x : phis)

-- instance OEIS 90390 where
--   oeis = 1 : 1 : 9 : zipWith (-) (map (* 5) $
--      tail $ zipWith (+) (tail (oeis @90390)) (oeis @90390)) (oeis @90390)

-- instance OEIS 90418 where
--   oeis = 0 : f 2 where
--      f x = (sum $ map g bpss) : f (x + 1) where
--        g ps | suffix == Nothing = 0
--             | suffix' == []     = 1
--             | last suffix' == 0 = 0
--             | otherwise         = (oeisIx @90418) $ fromBits suffix'
--             where suffix' = fromJust suffix
--                   suffix = stripPrefix ps $ toBits x
--        bpss = take (fromInteger $ (oeisIx @720) x) $
--                     map (toBits . fromInteger) (oeis @40)
--      toBits = unfoldr
--               (\u -> if u == 0 then Nothing else Just (mod u 2, div u 2))
--      fromBits = foldr (\b v -> 2 * v + b) 0

-- instance OEIS 90419 where
--   oeis = filter ((== 0) . (oeisIx @90418)) [1..]

-- instance OEIS 90420 where
--   oeis = filter ((== 1) . (oeisIx @90418)) [1..]

-- instance OEIS 90421 where
--   oeis = filter ((> 0) . (oeisIx @90418)) [1..]

-- instance OEIS 90422 where
--   oeis = filter ((== 1) . (oeisIx @90418) . fromInteger) (oeis @40)

-- instance OEIS 90423 where
--   oeis = filter ((> 1 ) . (oeisIx @90418) . fromInteger) (oeis @40)

-- instance OEIS 90424 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @90418))

-- instance OEIS 90425 where
--   oeisIx n = snd $ until ((== 1) . fst)
--                           (\ (u, v) -> (oeisIx u, v + 1)) (oeisIx n, 1)

-- instance OEIS 90431 where
--   oeisIx n = (oeisIx @7953) n - (oeisIx @7605) n

-- instance OEIS 90503 where
--   oeis = f [1..] where
--      f (x:xs) = g $ tail (oeis @961) where
--        g (q:pps) = h 0 $ map ((`div` (q - 1)) . subtract 1) $
--                              iterate (* q) (q ^ 3) where
--          h i (qy:ppys) | qy > x    = if i == 0 then f xs else g pps
--                        | qy < x    = h 1 ppys
--                        | otherwise = x : f xs

-- instance OEIS 90582 where
--   oeis = tablList @90582
-- instance Table 90582 where
--   rowCol = rowCol_off @90582 @1 @1
--   rowT   = rowT_off   @90582 @1
--   tabl = map reverse (tabl @19538)

-- instance OEIS 90597 where
--   oeis = [0,1,1,3,3,8,12] ++ zipWith (-)
--      (drop 4 $ zipWith (-) (map (* 5) zs) (drop 2 (oeis @90597)))
--      (zipWith (+) (drop 2 $ map (* 2) zs) (map (* 8) zs))
--      where zs = zipWith (+) (oeis @90597) $ tail (oeis @90597)

-- instance OEIS 90824 where
--   oeis = tablList @90824
-- instance Table 90824 where
--   tabl = zipWith (zipWith p)
--      (map (\x -> map (`enumFromTo` x) [1..x+1]) [0..]) (tabl @7318)
--      where p _          0 = 1
--            p []         _ = 0
--            p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 90826 where
--   oeis = map (sum . zipWith (*) (oeis @45) . reverse) $
--                      tail $ inits (oeis @108)

-- instance OEIS 91050 where
--   oeisIx = sum . map (oeisIx @75802) . (rowT @27750)

-- instance OEIS 91067 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @14707)

-- instance OEIS 91072 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @14707)

-- instance OEIS 91191 where
--   oeis = filter f [1..] where
--      f x = sum pdivs > x && all (<= 0) (map (\d -> (oeisIx @203) d - 2 * d) pdivs)
--            where pdivs = (rowT @27751) x

-- instance OEIS 91376 where
--   oeis = [x | x <- (oeis @2808), (oeisIx @1222) x == (oeisIx @20639) x]

-- instance OEIS 91441 where
--   oeis = tablList @91441
-- instance Table 91441 where
--   rowCol = rowCol_off @91441 @1 @1
--   rowT   = rowT_off   @91441 @1
--   tabl = iterate f [1] where
--      f xs = zipWith (+)
--        (zipWith (*) ([0] ++ xs) ks) (zipWith (*) (xs ++ [0]) (reverse ks))
--        where ks = [1 .. 1 + genericLength xs]

-- instance OEIS 91491 where
--   oeis = tablList @91491
-- instance Table 91491 where
--   tabl = iterate (\row -> 1 : scanr1 (+) row) [1]

-- instance OEIS 91633 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @136333)

-- instance OEIS 91871 where
--   oeis = f [1..] (oeis @40) where
--      f (i:is) (p:ps) = if (null $ show p `intersect` "024568")
--                           then i : f is ps else f is ps

-- instance OEIS 92206 where
--   oeis = filter ((== 0) . (oeisIx @214295)) [1..]

-- instance OEIS 92246 where
--   oeis = filter odd (oeis @69)

-- instance OEIS 92392 where
--   oeis = tablList @92392
-- instance Table 92392 where
--   rowCol = rowCol_off @92392 @1 @1
--   rowT   = rowT_off   @92392 @1
--   tabl = map reverse (tabl @46899)

-- instance OEIS 92495 where
--   oeisIx n = fromJust $ find ((== 0) . (`mod` n)) $ (oeis @142)

-- instance OEIS 92620 where
--   oeis = elemIndices 1 (oeis @193238)

-- instance OEIS 92624 where
--   oeis = elemIndices 2 (oeis @193238)

-- instance OEIS 92625 where
--   oeis = elemIndices 3 (oeis @193238)

-- instance OEIS 92693 where
--   oeisIx 1 = 0
--   oeisIx n = (+ 1) $ sum $ takeWhile (/= 1) $ iterate (oeisIx @10) $ (oeisIx @10) n

-- instance OEIS 92694 where
--   oeisIx n = snd $ until ((== 1) . fst) f (oeisIx n, 1) where
--      f (x, p) = (oeisIx x, p * x)

-- instance OEIS 92695 where
--   oeis = scanl (+) 0 $
--                  map (fromEnum . (> 7)) (8 : tail (oeis @20639))



-- instance OEIS 92892 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @6666))

-- instance OEIS 92953 where
--   oeisIx n = sum $
--      zipWith (\u v -> (oeisIx @10051)' u * (oeisIx @10051)' v) [1 .. n - 1] [n + 1 ..]

-- instance OEIS 92954 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @92953))

-- instance OEIS 93020 where
--   oeis = filter ((== 0) . (oeisIx @93019)) [0..]

-- instance OEIS 93021 where
--   oeis = filter ((== 1) . (oeisIx @93019)) [0..]

-- instance OEIS 93022 where
--   oeis = filter ((== 2) . (oeisIx @93019)) [0..]

-- instance OEIS 93023 where
--   oeis = filter ((== 3) . (oeisIx @93019)) [0..]

-- instance OEIS 93024 where
--   oeis = filter ((== 4) . (oeisIx @93019)) [0..]

-- instance OEIS 93025 where
--   oeis = filter ((== 5) . (oeisIx @93019)) [0..]

-- instance OEIS 93026 where
--   oeis = filter ((== 6) . (oeisIx @93019)) [0..]

-- instance OEIS 93027 where
--   oeis = filter ((== 7) . (oeisIx @93019)) [0..]

-- instance OEIS 93028 where
--   oeis = filter ((== 8) . (oeisIx @93019)) [0..]

-- instance OEIS 93029 where
--   oeis = filter ((== 9) . (oeisIx @93019)) [0..]

-- instance OEIS 93074 where
--   oeisIx 1 = 2
--   oeisIx n = maximum $ map (oeisIx @6530) [n - 1..n+1]

-- instance OEIS 93094 where
--   oeis = f [2,2] where
--      f (u : vs@ (v : _)) = u : f (vs ++
--        if w < 10 then [w] else uncurry ((. return) . (:)) $ divMod w 10)
--           where w = u * v

-- instance OEIS 93095 where
--   oeis = f [2,3] where
--      f (u : vs@ (v : _)) = u : f (vs ++
--        if w < 10 then [w] else uncurry ((. return) . (:)) $ divMod w 10)
--           where w = u * v

-- instance OEIS 93096 where
--   oeis = f [3,3] where
--      f (u : vs@ (v : _)) = u : f (vs ++
--        if w < 10 then [w] else uncurry ((. return) . (:)) $ divMod w 10)
--           where w = u * v

-- instance OEIS 93445 where
--   oeis = tablList @93445
-- instance Table 93445 where
--   rowCol n k = (rowT @93445) n !! (k-1)
--   rowT n = f [n, n - 1 .. 1] [1 ..] where
--      f [] _      = []
--      f (x:xs) ys = sum us : f xs vs where (us,vs) = splitAt x ys
--   tabl = map (rowT @93445) [1 ..]

-- instance OEIS 93446 where
--   oeisIx = maximum . (rowT @93445)

-- instance OEIS 93483 where
--   oeis = f ([2..7] ++ [8,14..]) [] where
--      f (x:xs) ys = if all (== 1) $ map (oeisIx . (+ x)) ys
--                       then x : f xs ((x+1):ys) else f xs ys

-- instance OEIS 93560 where
--   oeis = tablList @93560
-- instance Table 93560 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [3, 1]

-- instance OEIS 93561 where
--   oeis = tablList @93561
-- instance Table 93561 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [4, 1]

-- instance OEIS 93562 where
--   oeis = tablList @93562
-- instance Table 93562 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [5, 1]

-- instance OEIS 93563 where
--   oeis = tablList @93563
-- instance Table 93563 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [6, 1]

-- instance OEIS 93564 where
--   oeis = tablList @93564
-- instance Table 93564 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [7, 1]

-- instance OEIS 93565 where
--   oeis = tablList @93565
-- instance Table 93565 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [8, 1]

-- instance OEIS 93573 where
--   oeis = tablList @93573
-- instance Table 93573 where
--   rowCol n k = (rowT @93573) n !! (k-1)
--   rowT n = take n $ elemIndices n (oeis @20986)
--   tabl = map (rowT @93573) [1..]

-- instance OEIS 93640 where
--   oeisIx n  = genericLength [d | d <- [1..n], mod n d == 0,
--                            show (oeisIx d) `isInfixOf` show (oeisIx n)]

-- instance OEIS 93641 where
--   oeis = filter ((<= 2) . (oeisIx @1227)) [1..]

-- instance OEIS 93642 where
--   oeis = filter
--     (\x -> not $ all (`isInfixOf` b x) $ map b $ (rowT @27750) x) [1..] where
--     b = unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

-- instance OEIS 93644 where
--   oeis = tablList @93644
-- instance Table 93644 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [9, 1]

-- instance OEIS 93645 where
--   oeis = tablList @93645
-- instance Table 93645 where
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [10, 1]

-- instance OEIS 93703 where
--   oeis = filter
--      ((`elem` map (oeisIx @61493) [1..3999]) . (oeisIx @4086) . (oeisIx @61493)) [1..]

-- instance OEIS 93771 where
--   oeis = [oeisIx x | x <- [2..], (oeisIx @10051) (oeisIx x) == 1]

-- instance OEIS 93783 where
--   oeisIx n = q 0 $ (oeisIx @61493) n where
--        q s 0 = s
--        q s x = q (s + [0,1,5,10,50,100,500,1000] !! d') x'
--                where  (x',d) = divMod x 10; d' = fromInteger d

-- instance OEIS 93785 where
--   oeis = filter p [1..3999] where
--      p v = q $ (oeisIx @61493) v where
--        q w = w == 0 || v `mod` ([0,1,5,10,50,100,500,1000] !! d') == 0 && q w'
--             where  (w',d) = divMod w 10; d' = fromInteger d

-- instance OEIS 93796 where
--   oeis = concatMap (reverse . unfoldr r) $ map (oeisIx @61493) [1..3999]
--      where r 0 = Nothing
--            r x = Just ([0,1,5,10,50,100,500,1000] !! fromInteger d, x')
--                  where (x', d) = divMod x 10

-- instance OEIS 93873 where
--   {-# LANGUAGE ViewPatterns #-}
--   rat :: Rational -> (Integer,Integer)
--   rat r = (numerator r, denominator r)
--   data Harmony = Harmony Harmony Rational Harmony
--   rows :: Harmony -> [[Rational]]
--   rows (Harmony hL r hR) = [r] : zipWith (++) (rows hL) (rows hR)
--   kepler :: Rational -> Harmony
--   kepler r = Harmony (kepler (i% (i+j))) r (kepler (j% (i+j)))
--   .......... where (rat -> (i,j)) = r
--   k = rows $ kepler 1 :: [[Rational]] -- as list of lists
--   h = concat k :: [Rational] -- flattened
--   oeisIx n = numerator $ h !! (n - 1)
--   oeisIx n = denominator $ h !! (n - 1)
--   oeisIx n = numerator $ (map sum k) !! n -- denominator == 1
--   -- length (k !! n) == 2^n
--   -- numerator $ (map last k) !! n == fibonacci (n + 1)
--   -- denominator $ (map last k) !! n == fibonacci (n + 2)
--   -- numerator $ (map maximum k) !! n == n
--   -- denominator $ (map maximum k) !! n == n + 1
--   -- eop.

-- instance OEIS 93903 where
--   oeis = 1 : f [1] (oeis @40) where
--      f xs@ (x:_) ps = g ps where
--        g (q:qs) | x <= q         = h ps
--                 | y `notElem` xs = y : f (y:xs) (delete q ps)
--                 | otherwise      = g qs where
--          y = x - q
--          h (r:rs) | z `notElem` xs = z : f (z:xs) (delete r ps)
--                   | otherwise      = h rs where
--            z = x + r

-- instance OEIS 93995 where
--   oeis = tablList @93995
--   rowCol = rowCol_off @93995 @1 @1
--   rowT   = rowT_off   @93995 @1
--   tabl = zipWith replicate [1..] $ tail (oeis @290)
--   oeis = concat (tabl @93995)

-- instance OEIS 94015 where
--   oeisIx = sum . (rowT @152842)

-- instance OEIS 94048 where
--   oeisIx n = head [m | m <- map (oeisIx . subtract 1 . (* (oeisIx @2144) n))
--                                  (tail (oeis @290)), m > 0]

-- instance OEIS 94178 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @125203)

-- instance OEIS 94189 where
--   oeisIx n = sum $ map (oeisIx @10051)' [n* (n - 1) .. n^2]

-- instance OEIS 94305 where
--   oeis = tablList @94305
-- instance Table 94305 where
--   tabl = zipWith (map . (*)) (tail (oeis @217)) (tabl @7318)

-- instance OEIS 94328 where
--   oeis = iterate (oeisIx @6369) 4

-- instance OEIS 94329 where
--   oeis = iterate (oeisIx @6369) 16

-- instance OEIS 94379 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @66955))

-- instance OEIS 94407 where
--   oeis = filter ((== 1) . (oeisIx @10051)) [1,17..]

-- instance OEIS 94501 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @86793))

-- instance OEIS 94524 where
--   oeisIx = (+ 2) . (* 3) . (oeisIx @23208)

-- instance OEIS 94587 where
--   oeis = tablList @94587
-- instance Table 94587 where
--   tabl = map fst $ iterate f ([1], 1)
--      where f (row, i) = (map (* i) row ++ [1], i + 1)

-- instance OEIS 94588 where
--   oeis = 0 : zipWith (+) (tail (oeis @45))
--                                  (zipWith (*) [1..] (oeis @45))

-- instance OEIS 94638 where
--   oeis = tablList @94638
-- instance Table 94638 where
--   rowCol = rowCol_off @94638 @1 @1
--   rowT   = rowT_off   @94638 @1
--   tabl = map reverse (tabl @130534)

-- instance OEIS 94727 where
--   oeis = tablList @94727
-- instance Table 94727 where
--   rowCol n k = n + k
--   rowT   = rowT_off   @94727 @1
--   tabl = iterate (\row@ (h:_) -> (h + 1) : map (+ 2) row) [1]

-- instance OEIS 94784 where
--   oeis = [x | x <- [0..], (oeisIx @10052) x == 0, (oeisIx @10057) x == 0]

-- instance OEIS 95048 where
--   oeisIx = genericLength . group . sort . concatMap show . (rowT @27750)

-- instance OEIS 95050 where
--   oeis = map (+ 1) $ elemIndices 10 $ map (oeisIx @95048) [1..]

-- instance OEIS 95072 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . fi) (oeis @31444)

-- instance OEIS 95114 where
--   oeis = 1 : f [1] 1 where
--      f xs@ (x:_) k = y : f (y:xs) (k+1) where
--        y = x + length [z | z <- xs, z <= k]

-- instance OEIS 95116 where
--   oeisIx n = (oeisIx @40) n + toInteger n - 1

-- instance OEIS 95117 where
--   oeisIx n = (oeisIx @720) n + n

-- instance OEIS 95180 where
--   oeis =filter ((== 1) . (oeisIx @10051)) (oeis @4087)

-- instance OEIS 95195 where
--   oeis = tablList @95195
-- instance Table 95195 where
--   rowCol = rowCol_off @95195 @1 @1
--   rowT   = rowT_off   @95195 @1
--   tabl = f (oeis @40) [] where
--      f (p:ps) xs = ys : f ps ys where ys = scanl (-) p xs

-- instance OEIS 95381 where
--   oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @209229) (oeis @25586)

-- instance OEIS 95660 where
--   oeis = tablList @95660
-- instance Table 95660 where
--   tabl = [3] : iterate
--      (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,3]

-- instance OEIS 95666 where
--   oeis = tablList @95666
-- instance Table 95666 where
--   tabl = [4] : iterate
--      (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1,4]

-- instance OEIS 95774 where
--   oeisIx n = 2 * (oeisIx @3160) n - n

-- instance OEIS 95775 where
--   oeis = map (`div` 2) $ filter ((== 0) . (oeisIx @95774)) [1..]

-- instance OEIS 95840 where
--   oeisIx = (oeisIx @71330) . (oeisIx @961)

-- instance OEIS 95841 where
--   oeis = filter ((== 1) . (oeisIx @71330)) (oeis @961)

-- instance OEIS 95842 where
--   oeis = filter ((== 0) . (oeisIx @71330)) (oeis @961)

-- instance OEIS 95874 where
--   oeisIx n | y == n    = length xs + 1
--             | otherwise = 0
--             where (xs, y:ys) = span (< n) (oeis @961)

-- instance OEIS 95890 where
--   oeis = tablList @95890
-- instance Table 95890 where
--   rowCol n k = (n - k + 1) ^ (n - k)
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @95890) [1..]

-- instance OEIS 95916 where
--   oeis = zipWith (-) (tail (oeis @796)) (oeis @796)

-- instance OEIS 95958 where
--   oeis = f $ map show (oeis @77800) :: [Integer] where
--      f (t:t':ts) = read (t ++ t') : f ts

-- instance OEIS 95960 where
--   oeisIx n = genericLength [x | x <- (rowT @27750) n, x < (oeisIx @7947) n]

-- instance OEIS 96008 where
--   oeis = tablList @96008
-- instance Table 96008 where
--   rowCol = rowCol_off @96008 @1 @1
--   rowT   = rowT_off @96008 @1
--   tabf = [0] : map (0 :) (tabf @46071)

-- instance OEIS 96095 where
--   oeis = 1 : 1 : zipWith dadd (oeis @96095) (tail (oeis @96095)) where
--      dadd x y = foldl (\v d -> (if d < 10 then 10 else 100)*v + d)
--                       0 $ reverse $ unfoldr f (x,y) where
--           f (x,y) | x + y == 0 = Nothing
--                   | otherwise  = Just (xd + yd, (x',y'))
--                   where (x',xd) = divMod x 10; (y',yd) = divMod y 10

-- instance OEIS 96138 where
--   oeis = 1 : g 2 1 where
--      g x y = z : g (x + 1) z where z = (oeisIx @4086) (x * y)

-- instance OEIS 96139 where
--   oeisIx n = sum (map (oeisIx @10051) gs') + fromEnum (1 `elem` gs')
--      where gs' = map (2 * n -) $ takeWhile (< 2 * n) (oeis @8578)

-- instance OEIS 96145 where
--   oeis = tablList @96145
-- instance Table 96145 where
--   tabl = map (map (oeisIx @7953)) (tabl @7318)

-- instance OEIS 96165 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @1222)) $ tail (oeis @961)

-- instance OEIS 96268 where
--   oeisIx = (subtract 1) . (oeisIx @56832) . (+ 1)

-- instance OEIS 96274 where
--   oeis = elemIndices 0 (oeis @96535)

-- instance OEIS 96363 where
--   oeisIx = (oeisIx @1175) . (10 ^)

-- instance OEIS 96460 where
--   oeis = 1 : iterate (\x -> x + (oeisIx @8472) x) 2

-- instance OEIS 96461 where
--   oeis = 1 : iterate (oeisIx @75254) 2

-- instance OEIS 96465 where
--   oeis = tablList @96465
-- instance Table 96465 where
--   tabl = map reverse (tabl @91491)

-- instance OEIS 96494 where
--   oeisIx = (* 2) . (oeisIx @6)

-- instance OEIS 96780 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75383))

-- instance OEIS 96781 where
--   oeisIx = a . a where a = (oeis `genericIndex`) . subtract 1

-- instance OEIS 96796 where
--   oeis = 0 : 1 : zipWith (-)
--      (map (* 2) $ tail (oeis @96796)) (map (oeisIx @96796) $ tail (oeis @83920))

-- instance OEIS 96824 where
--   oeis = 0 : 1 : 2 : zipWith (-)
--      (map (* 2) $ drop 2 (oeis @96824)) (map (oeisIx @96824) $ tail (oeis @122797))

-- instance OEIS 96916 where
--   oeisIx = (oeisIx @20639) . (oeisIx @6881)

-- instance OEIS 96981 where
--   oeisIx = p $ tail (oeis @47273) where
--      p _  0         = 1
--      p ks'@ (k:ks) m = if k > m then 0 else p ks' (m - k) + p ks m

-- instance OEIS 97054 where
--   import Data.Map (singleton, findMin, deleteMin, insert)
--   oeisIx n = (oeis @97054) !! (n - 1)
--   oeis = f 9 (3, 2) (singleton 4 (2, 2)) where
--      f zz (bz, be) m
--       | xx < zz && even be =
--                   f zz (bz, be+1) (insert (bx*xx) (bx, be+1) $ deleteMin m)
--       | xx < zz = xx :
--                   f zz (bz, be+1) (insert (bx*xx) (bx, be+1) $ deleteMin m)
--       | xx > zz = f (zz+2*bz+1) (bz+1, 2) (insert (bz*zz) (bz, 3) m)
--       | otherwise = f (zz + 2 * bz + 1) (bz + 1, 2) m
--       where (xx, (bx, be)) = findMin m

-- instance OEIS 97133 where
--   oeis = 1 : 2 : 4 : zipWith (+)
--                  (map (* 2) $ tail (oeis @97133)) (oeis @97133)

-- instance OEIS 97140 where
--   oeis = concat $ transpose [oeis, map (1 -) (oeis @1477)]

-- instance OEIS 97207 where
--   oeis = tablList @97207
-- instance Table 97207 where
--   tabl = map init $ tail (tabl @29635)

-- instance OEIS 97256 where
--   oeis = map (* 9) (oeis @7088)

-- instance OEIS 97343 where
--   oeis = tablList @97343
-- instance Table 97343 where
--   rowCol = rowCol_off @97343 @2 @1
--   rowT   = rowT_off @97343 @2
--   tabf =
--      map (\p -> map (flip legendreSymbol p) [1..p]) $ tail (oeis @40)
--   legendreSymbol a p = if a' == 0 then 0 else twoSymbol * oddSymbol where
--      a' = a `mod` p
--      (s,q) = a' `splitWith` 2
--      twoSymbol = if (p `mod` 8) `elem` [1,7] || even s then 1 else -1
--      oddSymbol = if q == 1 then 1 else qrMultiplier * legendreSymbol p q
--      qrMultiplier = if p `mod` 4 == 3 && q `mod` 4 == 3 then -1 else 1
--      splitWith n p = spw 0 n where
--         spw s t = if m > 0 then (s, t) else spw (s + 1) t'
--                   where (t', m) = divMod t p

-- instance OEIS 97356 where
--   oeisIx n = p [1..oeisIx n] n where
--      p [] _ = 0
--      p _  0 = 1
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 97364 where
--   oeis = tablList @97364
-- instance Table 97364 where
--   rowCol n k = length [qs | qs <- pss !! n, last qs - head qs == k] where
--      pss = [] : map parts [1..] where
--            parts x = [x] : [i : ps | i <- [1..x],
--                                      ps <- pss !! (x - i), i <= head ps]
--   rowT n = map (oeisIx n) [0..n - 1]
--   tabl = map (rowT @97364) [1..]

-- instance OEIS 97451 where
--   oeisIx n = p (oeis @47228) n where
--      p _  0         = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 97557 where
--   oeis = 1 : f 1 0 where
--      f x z = y : f y z' where
--        y = x + z'; z' = z + 1 - fi (oeisIx' x)

-- instance OEIS 97602 where
--   oeis = 1 : f 1 1 where
--      f x c = y : f y (c + (oeisIx @10052) y) where y = x + c

-- instance OEIS 97613 where
--   oeisIx n = (oeisIx @209561) (2 * n - 1) n

-- instance OEIS 97796 where
--   oeisIx = p (oeis @396) where
--      p _ 0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 97807 where
--   oeis = tablList @97807
-- instance Table 97807 where
--   tabl = iterate (\xs@ (x:_) -> - x : xs) [1]

-- instance OEIS 97889 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @97889) !! (n - 1)
--   oeis = f $ singleton (6, 2, 3) where
--      f s = y : f (insert (w, p, q') $ insert (w `div` p, (oeisIx @151800) p, q') s')
--            where w = y * q'; q' = (oeisIx @151800) q
--                  ((y, p, q), s') = deleteFindMin s

-- instance OEIS 97944 where
--   oeisIx = (oeisIx @55642) . (oeisIx @40)

-- instance OEIS 97974 where
--   oeisIx n = sum [p | p <- (rowT @27748) n, p ^ 2 <= n]

-- instance OEIS 97977 where
--   oeisIx n = head [p | p <- dropWhile (<= n) (oeis @40),
--   oeisIx (p + n) == n]

-- instance OEIS 98006 where
--   oeisIx n = (oeisIx @5097) (n - 1) - (oeisIx @10) (oeisIx n)

-- instance OEIS 98096 where
--   oeisIx n = (oeisIx @1248) n * (oeisIx @34785) n

-- instance OEIS 98237 where
--   oeis = filter ((== 0) . (oeisIx @109925)) (oeis @71904)

-- instance OEIS 98282 where
--   oeisIx n = f [n] where
--      f xs = if y `elem` xs then length xs else f (y:xs) where
--        y = genericIndex (map (oeisIx @87712) [1..]) (head xs - 1)

-- instance OEIS 98294 where
--   oeisIx 0  = 0
--   oeisIx n  = fromJust (oeisIx n `elemIndex` (rowT @227048) n) + 1

-- instance OEIS 98312 where
--   oeisIx = (oeisIx @98311) . (oeisIx @98311)

-- instance OEIS 98313 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98311))

-- instance OEIS 98314 where
--   oeisIx = (oeisIx @98313) . (oeisIx @98313)

-- instance OEIS 98424 where
--   oeisIx n = genericLength [ (p,q,r) | p <- takeWhile (<= n) (oeis @40),
--               let r = p + 6, (oeisIx @10051) r == 1, q <- [p+1..r-1], (oeisIx @10051) q == 1]

-- instance OEIS 98430 where
--   oeisIx n = (oeisIx @302) n * (oeisIx @984) n

-- instance OEIS 98549 where
--   oeisIx = (oeisIx @98548) . (oeisIx @98548)

-- instance OEIS 98553 where
--   oeisIx = (oeisIx @98551) . (oeisIx @98551)

-- instance OEIS 98565 where
--   oeis = map (+ 2 ) $ elemIndices 3 (oeis @59233)

-- instance OEIS 98743 where
--   oeisIx n = p [nd | nd <- [1..n], mod n nd /= 0] n where
--      p _  0 = 1
--      p [] _ = 0
--      p ks'@ (k:ks) m | m < k = 0 | otherwise = p ks' (m - k) + p ks m
--   import Data.MemoCombinators (memo3, integral)
--   oeisIx n = (oeis @98743) !! n
--   oeis = map (\x -> pMemo x 1 x) [0..] where
--      pMemo = memo3 integral integral integral p
--      p _ _ 0 = 1
--      p x k m | m < k        = 0
--              | mod x k == 0 = pMemo x (k + 1) m
--              | otherwise    = pMemo x k (m - k) + pMemo x (k + 1) m

-- instance OEIS 98825 where
--   oeis = tablList @98825
-- instance Table 98825 where
--   tabl = map (zipWith (*) (oeis @166)) (tabl @7318)

-- instance OEIS 98842 where
--   oeis = map length $ group (oeis @60384)

-- instance OEIS 98884 where
--   oeisIx = p (oeis @7310) where
--      p _  0     = 1
--      p (k:ks) m = if k > m then 0 else p ks (m - k) + p ks m

-- instance OEIS 98962 where
--   oeis = 1 : f [2..] (tail (oeis @175944)) where
--      f xs'@ (x:xs) ps'@ (p:ps)
--        | (oeisIx @10051) x == 1    = x : f xs (delete x ps')
--        | u == q && v == q' = x : f xs' zs
--        | otherwise         = f xs ps'
--        where q = (oeisIx @20639) x; q' = div x q
--              (us, u:us') = span (< q) ps'
--              (vs, v:vs') = span (< q') us'
--              zs@ (z:_) = us ++ vs ++ vs'
--              xs' = if z == p then xs else filter ((> 0) . (`mod` p)) xs

-- instance OEIS 98983 where
--   oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (< n) (oeis @40)

-- instance OEIS 99009 where
--   oeis = [x | x <- [0..], (oeisIx @151949) x == x]

-- instance OEIS 99036 where
--   oeis = zipWith (-) (oeis @79) (oeis @45)

-- instance OEIS 99047 where
--   oeis = [m | m <- [1..],
--                       (oeisIx @10051)' (m - 1) == 0 && (oeisIx @10051)' (m + 1) == 0]

-- instance OEIS 99188 where
--   oeisIx = (* 2) . (oeisIx @49474)

-- instance OEIS 99245 where
--   oeisIx n = numerator $ (oeisIx n) % (oeisIx n)

-- instance OEIS 99246 where
--   oeisIx n = denominator $ (oeisIx n) % (oeisIx n)

-- instance OEIS 99302 where
--   oeisIx n = genericLength $ filter (== n) $ map (oeisIx @3415) [1 .. (oeisIx @2620) n]

-- instance OEIS 99304 where
--   oeisIx n = succ $ fromJust $ elemIndex 0 $
--      zipWith (-) (drop (fromInteger n + 1) (oeis @3415))
--                  (map (+ n') $ tail (oeis @3415))
--      where n' = (oeisIx @3415) n

-- instance OEIS 99305 where
--   oeis = f 1 $ h 1 empty where
--      f x ad = y : f (x + 1) (h (3 * x + 1) ad)  where
--               y = length [ () | k <- [1 .. 2 * x],
--                                let x' = ad ! x, ad ! (x + k) == x' + ad ! k]
--      h z = insert z (oeisIx z) .
--             insert (z+1) (oeisIx (z+1)) . insert (z+2) (oeisIx (z+2))

-- instance OEIS 99375 where
--   oeis = tablList @99375
--   rowCol n k = (rowT @99375) n !! k
--   rowT n = (tabl @99375) !! n
--   tabl = iterate (\xs -> (head xs + 2) : xs) [1]

-- instance OEIS 99425 where
--   oeisIx = sum . (rowT @102413)

-- instance OEIS 99542 where
--   oeis = filter (rhonda 10) [1..]
--   rhonda b x = (oeisIx @1414) x * b == product (unfoldr
--          (\z -> if z == 0 then Nothing else Just $ swap $ divMod z b) x)

-- instance OEIS 99543 where
--   oeisIx = (oeisIx @1414) . (oeisIx @99542)

-- instance OEIS 99619 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98962)) . (oeisIx @40)

-- instance OEIS 99620 where
--   oeisIx n = f (p - 1) $ drop (oeisIx n) (oeis @98962) where
--      f c (x:xs) | c == 1 = if m == 0 then x else f 1 xs
--                 | m /= 0 = f c xs
--                 | m == 0 = f (c - if x' == p then 2 else 1) xs
--                 where (x',m) = divMod x p
--      p = (oeisIx @40) n

-- instance OEIS 99627 where
--   oeis = tablList @99627
-- instance Table 99627 where
--   tabl = iterate (\xs@ (x:_) -> (2 * x) : map ((+ 1) . (* 2)) xs) [1]



-- instance OEIS 99751 where
--   oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n)
--      where f 2 e = e - 1; f 3 e = 1; f _ e = e + 1

-- instance OEIS 99848 where
--   oeis = concat $ zipWith replicate (oeis @8480) [1..]

-- instance OEIS 99909 where
--   oeis = map (flip div 2) $ tail $ zipWith (+)
--      (zipWith (*) (oeis @40) $ map (subtract 1) $ tail (oeis @40))
--      (zipWith (*) (map (subtract 1) (oeis @40)) $ tail (oeis @40))

-- instance OEIS 99965 where
--   oeisIx = flip (oeisIx @99964) 0
--   oeis = map head (tabf @99964)

-- instance OEIS 99966 where
--   oeis = map (last . init) $ tail (tabf @99964)

-- instance OEIS 99968 where
--   oeisIx = flip (oeisIx @99964) 1

-- instance OEIS 100100 where
--   oeis = tablList @100100
--   rowCol n k = (tabl @100100) !! n !! n
--   rowT n = (tabl @100100) !! n
--   tabl = [1] : f (tabl @92392) where
--      f (us : wss'@ (vs : wss)) = (vs !! 1 : us) : f wss'

-- instance OEIS 100104 where
--   oeisIx (succ->n) = n * (3 * n + 1)

-- instance OEIS 100208 where
--   import Data.Set (singleton, notMember, insert)
--   oeisIx n = (oeis @100208) !! (n - 1)
--   oeis = 1 : (f 1 [1..] $ singleton 1) where
--      f x (w:ws) s
--        | w `notMember` s &&
--          (oeisIx @10051) (x*x + w*w) == 1 = w : (f w [1..] $ insert w s)
--        | otherwise                = f x ws s where

-- instance OEIS 100320 where
--   oeisIx n = (oeisIx @124927) (2 * n) n

-- instance OEIS 100368 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @100368) !! (n - 1)
--   oeis = f (singleton 6) (tail (oeis @65091)) where
--   f s ps'@ (p:ps) | mod m 4 > 0 = m : f (insert (2*p) $ insert (2*m) s') ps
--   | otherwise = m : f (insert (2*m) s') ps'
--   where (m,s') = deleteFindMin s

-- instance OEIS 100428 where
--   oeis = f (oeis @2) where f (u:_:us) = u : f us

-- instance OEIS 100429 where
--   oeis = g (oeis @2) where g (_:v:vs) = v : g vs

-- instance OEIS 100440 where
--   oeisIx = genericLength . (rowT @200741)

-- instance OEIS 100678 where
--   oeisIx = genericLength . (rowT @247765)

-- instance OEIS 100695 where
--   oeisIx = last . (rowT @247765)

-- instance OEIS 100707 where
--   import qualified Data.Set as Set (insert)
--   import Data.Set (singleton, member)
--   oeisIx n = (oeis @100707) !! (n - 1)
--   oeis = 1 : f 1 (singleton 1) [1..] where
--      f y st ds = g ds where
--        g (k:ks) | v <= 0      = h ds
--                 | member v st = g ks
--                 | otherwise   = v : f v (Set.insert v st) (delete k ds)
--                 where v = y - k
--        h (k:ks) | member w st = h ks
--                 | otherwise   = w : f w (Set.insert w st) (delete k ds)
--                 where w = y + k

-- instance OEIS 100708 where
--   oeis = map abs $ zipWith (-) (tail (oeis @100707)) (oeis @100707)

-- instance OEIS 100716 where
--   oeis = filter (\x -> or $
--      zipWith (<=) (oeisIx_row x) (map toInteger $ (rowT @124010) x)) [1..]

-- instance OEIS 100717 where
--   oeis = filter ((== 0) . (oeisIx @203908)) [1..]

-- instance OEIS 100732 where
--   oeisIx = (oeisIx @142) . (oeisIx @8585)

-- instance OEIS 100795 where
--   oeis = f 0 (oeis @2024) where
--      f x ws = v : f v (us ++ vs) where (us, v:vs) = span (== x) ws

-- instance OEIS 100861 where
--   oeis = tablList @100861
-- instance Table 100861 where
--   tabf = zipWith take (oeis @8619) (tabl @144299)

-- instance OEIS 100892 where
--   oeisIx n = (2 * n - 1) `xor` (2 * n + 1)
--   oeis = zipWith xor (tail (oeis @5408)) (oeis @5408)

-- instance OEIS 100949 where
--   oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (< n) (oeis @1358)

-- instance OEIS 100962 where
--   oeis = filter ((== 0) . (oeisIx @64911)) (oeis @14092)

-- instance OEIS 100968 where
--   oeis = filter (rhonda 4) (oeis @23705)

-- instance OEIS 100969 where
--   oeis = filter (rhonda 6) (oeis @248910)

-- instance OEIS 100970 where
--   oeis = filter (rhonda 8) (oeis @255805)

-- instance OEIS 100971 where
--   oeis = filter (rhonda 12) $ iterate z 1 where
--      z x = 1 + if r < 11 then x else 12 * z x' where (x', r) = divMod x 12

-- instance OEIS 100972 where
--   oeis = filter (rhonda 14) $ iterate z 1 where
--      z x = 1 + if r < 13 then x else 14 * z x' where (x', r) = divMod x 14

-- instance OEIS 100973 where
--   oeis = filter (rhonda 9) (oeis @255808)

-- instance OEIS 100974 where
--   oeis = filter (rhonda 15) $ iterate z 1 where
--      z x = 1 + if r < 14 then x else 15 * z x' where (x', r) = divMod x 15

-- instance OEIS 100975 where
--   oeis = filter (rhonda 16) $ iterate z 1 where
--      z x = 1 + if r < 15 then x else 16 * z x' where (x', r) = divMod x 16

-- instance OEIS 101035 where
--   oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
--      f p 1 = 1 - 2 * p
--      f p e = (p - 1) ^ 2

-- instance OEIS 101048 where
--   oeisIx = p (oeis @1358) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 101082 where
--   oeis = filter ((> 0) . (oeisIx @49502)) [0..]

-- instance OEIS 101164 where
--   oeis = tablList @101164
-- instance Table 101164 where
--   tabl = zipWith (zipWith (-)) (tabl @8288) (tabl @7318)

-- instance OEIS 101203 where
--   oeis = scanl (+) 0 $ zipWith (*) [1..] $ map (1 -) (oeis @10051)

-- instance OEIS 101211 where
--   oeis = tablList @101211
-- instance Table 101211 where
--   rowCol = rowCol_off @101211 @1 @1
--   rowT   = rowT_off @101211 @1
--   tabf = map (reverse . map length . group) $ tail (tabf @30308)

-- instance OEIS 101265 where
--   oeis = 1 : 2 : 6 : zipWith (+) (oeis @101265)
--       (map (* 5) $ tail $ zipWith (-) (tail (oeis @101265)) (oeis @101265))

-- instance OEIS 101300 where
--   oeisIx = (oeisIx @151800) . (oeisIx @151800)

-- instance OEIS 101312 where
--   oeisIx n = f 1 {- January -} where
--      f 13                = 0
--      f m | h n m 13 == 6 = (f $ succ m) + 1
--          | otherwise     = f $ succ m
--      h year month day
--        | month <= 2 = h  (year - 1)  (month + 12)  day
--        | otherwise  = (day + 26 * (month + 1) `div` 10 + y + y `div` 4
--                       + century `div` 4 - 2 * century) `mod` 7
--          where (century, y) = divMod year 100

-- instance OEIS 101402 where
--   oeisIx = genericIndex (oeis @101402)
--   oeis = 0 : 1 : zipWith ((+) `on` (oeisIx @101402))
--                          (tail (oeis @53644)) (oeis @53645)

-- instance OEIS 101403 where
--   oeis = map length $ group (oeis @101402)

-- instance OEIS 101404 where
--   oeisIx n = (oeisIx @101403) n * n

-- instance OEIS 101438 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @101369))

-- instance OEIS 101461 where
--   oeisIx = maximum . (rowT @53121)

-- instance OEIS 101594 where
--   oeis = filter ((== 2) . (oeisIx @43537)) (oeis @52382)

-- instance OEIS 101624 where
--   oeisIx = sum . zipWith (*) (oeis @79) . map (flip mod 2) . (rowT @11973)

-- instance OEIS 102251 where
--   oeis = 1 : (map (* 2) $
--                  concatMap (map (read . return) . show) (oeis @102251))

-- instance OEIS 102364 where
--   oeisIx 0 = 0
--   oeisIx n = genericLength $ filter (== 0) $ (rowT @213676) n

-- instance OEIS 102370 where
--   oeis = 0 : map (oeisIx . toInteger) (oeis @62289)

-- instance OEIS 102371 where
--   oeis = map (oeisIx . toInteger) $ tail (oeis @225)

-- instance OEIS 102376 where
--   oeisIx = (4 ^) . (oeisIx @120)

-- instance OEIS 102413 where
--   oeis = tablList @102413
-- instance Table 102413 where
--   tabl = [1] : [1,1] : f [2] [1,1] where
--      f us vs = ws : f vs ws where
--                ws = zipWith3 (((+) .) . (+))
--                     ([0] ++ us ++ [0]) ([0] ++ vs) (vs ++ [0])

-- instance OEIS 102466 where
--   oeis = [x | x <- [1..], (oeisIx @5) x == (oeisIx @1221) x + (oeisIx @1222) x]

-- instance OEIS 102467 where
--   oeis = [x | x <- [1..], (oeisIx @5) x /= (oeisIx @1221) x + (oeisIx @1222) x]

-- instance OEIS 102472 where
--   oeis = tablList @102472
-- instance Table 102472 where
--   rowCol = rowCol_off @102472 @1 @1
--   rowT   = rowT_off   @102472 @1
--   tabl = map reverse (tabl @102473)

-- instance OEIS 102473 where
--   oeis = tablList @102473
-- instance Table 102473 where
--   rowCol = rowCol_off @102473 @1 @1
--   rowT   = rowT_off   @102473 @1
--   tabl = [1] : [1, 1] : f [1] [1, 1] 2 where
--      f us vs x = ws : f vs ws (x + 1) where
--                  ws = 1 : zipWith (+) ([0] ++ us) (map (* x) vs)

-- instance OEIS 102478 where
--   oeisIx = flip div 2 . (oeisIx @68700)

-- instance OEIS 102662 where
--   oeis = tablList @102662
-- instance Table 102662 where
--   tabl = [1] : [1,3] : f [1] [1,3] where
--      f xs ys = zs : f ys zs where
--        zs = zipWith (+) ([0] ++ xs ++ [0]) $
--                         zipWith (+) ([0] ++ ys) (ys ++ [0])

-- instance OEIS 102820 where
--   oeis =  map (sum . (map (oeisIx @10051))) $
--      zipWith enumFromTo (oeis @100484) (tail (oeis @100484))

-- instance OEIS 102900 where
--   oeis = 1 : 1 : zipWith (+)
--                  (map (* 4) (oeis @102900)) (map (* 3) $ tail (oeis @102900))

-- instance OEIS 103147 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @47160))

-- instance OEIS 103192 where
--   oeis = iterate (fromInteger . (oeisIx @102370)) 1

-- instance OEIS 103284 where
--   oeis = tablList @103284
-- instance Table 103284 where
--   tabl = iterate (\xs -> sort $ zipWith (+) (xs++[0]) ([0]++xs)) [1]

-- instance OEIS 103285 where
--   oeisIx = last . (rowT @103284)

-- instance OEIS 103339 where
--   oeisIx = numerator . uhm where uhm n = (n * (oeisIx @34444) n) % (oeisIx n)

-- instance OEIS 103340 where
--   oeisIx = denominator . uhm where uhm n = (n * (oeisIx @34444) n) % (oeisIx n)

-- instance OEIS 103371 where
--   oeis = tablList @103371
-- instance Table 103371 where
--   tabl = map reverse (tabl @132813)

-- instance OEIS 103631 where
--   oeis = tablList @103631
-- instance Table 103631 where
--   tabl = [1] : [0,1] : f [1] [0,1] where
--      f xs ys = zs : f ys zs where
--        zs = zipWith (+)  ([0,0] ++ xs)  (ys ++ [0])

-- instance OEIS 103689 where
--   oeisIx n = min (oeisIx n) (oeisIx n)

-- instance OEIS 103747 where
--   oeis = iterate (fromInteger . (oeisIx @102370)) 2

-- instance OEIS 103889 where
--   oeisIx n = n - 1 + 2 * mod n 2
--   oeis = concat $ transpose [tail (oeis @5843), (oeis @5408)]

-- instance OEIS 103960 where
--   oeisIx n = sum [oeisIx' $ p * q - 2 |
--                    let p = (oeisIx @40) n, q <- takeWhile (<= p) (oeis @40)]

-- instance OEIS 104035 where
--   oeis = tablList @104035
-- instance Table 104035 where
--   tabl = iterate f [1] where
--      f xs = zipWith (+)
--        (zipWith (*) [1..] (tail xs) ++ [0,0]) ([0] ++ zipWith (*) [1..] xs)

-- instance OEIS 104125 where
--   oeisIx = (^ 2) . (oeisIx @64413)

-- instance OEIS 104126 where
--   oeisIx n = p ^ (p + 1) where p = (oeisIx @40) n

-- instance OEIS 104235 where
--   oeis = [x | x <- [0..], (oeisIx @102370) x == toInteger x]

-- instance OEIS 104278 where
--   oeis = [m | m <- [1..],
--                       (oeisIx @10051)' (2 * m - 1) == 0 && (oeisIx @10051)' (2 * m + 1) == 0]

-- instance OEIS 104315 where
--   oeis = filter (\x -> (oeisIx @168046) x == 0 && (oeisIx @168046) (x ^ 2) == 1) [1..]

-- instance OEIS 104324 where
--   oeisIx = genericLength . map length . group . (rowT @213676)

-- instance OEIS 104499 where
--   oeis = findIndices ((== 1) . (oeisIx @10051)) (oeis @1945)

-- instance OEIS 104684 where
--   oeis = tablList @104684
-- instance Table 104684 where
--   tabl = map (map abs) $
--                  zipWith (zipWith (*)) (tabl @130595) (tabl @92392)

-- instance OEIS 104698 where
--   oeis = tablList @104698
-- instance Table 104698 where
--   rowCol = rowCol_off @104698 @1 @1
--   rowT   = rowT_off   @104698 @1
--   tabl = [1] : [2,1] : f [1] [2,1] where
--      f us vs = ws : f vs ws where
--        ws = zipWith (+) ([0] ++ us ++ [0]) $
--             zipWith (+) ([1] ++ vs) (vs ++ [0])

-- instance OEIS 104763 where
--   oeis = tablList @104763
-- instance Table 104763 where
--   rowCol = rowCol_off @104763 @1 @1
--   rowT   = rowT_off   @104763 @1
--   tabl = map (flip take $ tail (oeis @45)) [1..]

-- instance OEIS 104777 where
--   oeisIx = (^ 2) . (oeisIx @7310)

-- instance OEIS 105029 where
--   oeisIx n = foldl (.|.) 0 $ zipWith (.&.) (oeis @79) $
--      map (\x -> (len + 1 - (oeisIx @70939) x) * x)
--          (reverse $ enumFromTo n (n - 1 + len))  where len = (oeisIx @103586) n

-- instance OEIS 105047 where
--   oeisIx 1 = 1
--   oeisIx n = genericLength $ (rowT @260580) (n - 1)

-- instance OEIS 105082 where
--   oeis = scanl (+) 5 $ tail (oeis @48696)

-- instance OEIS 105153 where
--   oeisIx n = t [n] where
--      t xs@ (x:_) | y `elem` xs = length xs
--                 | otherwise   = t (y : xs) where y = (oeisIx @105025) x

-- instance OEIS 105154 where
--   oeisIx n = t [n] where
--      t xs@ (x:_) | y `elem` xs = length xs
--                 | otherwise   = t (y : xs) where y = (oeisIx @105027) x

-- instance OEIS 105179 where
--   oeis = 1 : filter (\x -> (oeisIx @10879) (oeisIx x) == (oeisIx @10879) x) [2..]

-- instance OEIS 105186 where
--   oeisIx 0 = 0
--   oeisIx n = 9 * (oeisIx @105186) n' + mod t 3
--               where (n', t) = divMod n 9

-- instance OEIS 105210 where
--   oeis = 393 : map
--         (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105210)

-- instance OEIS 105211 where
--   oeis = 412 : map
--         (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105211)

-- instance OEIS 105212 where
--   oeis = 668 : map
--         (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105212)

-- instance OEIS 105213 where
--   oeis = 932 : map
--         (\x -> x + 1 + sum (takeWhile (< x) $ (rowT @27748) x)) (oeis @105213)

-- instance OEIS 105221 where
--   oeisIx n = (oeisIx @8472) n - n * fi (oeisIx n)

-- instance OEIS 105271 where
--   oeis = [x | x <- [0..], (oeisIx @105025) x == x]

-- instance OEIS 105278 where
--   oeis = tablList @105278
-- instance Table 105278 where
--   rowCol = rowCol_off @105278 @1 @1
--   rowT   = rowT_off   @105278 @1
--   tabl = [1] : f [1] 2 where
--      f xs i = ys : f ys (i + 1) where
--        ys = zipWith (+) ([0] ++ xs) (zipWith (*) [i, i + 1 ..] (xs ++ [0]))

-- instance OEIS 105321 where
--   oeisIx n = if n == 0 then 1 else (oeisIx @1316) n + (oeisIx @1316) (n - 1)

-- instance OEIS 105417 where
--   oeis = filter ((== "1234567") . sort . nub . show . (oeisIx @61493)) [1..3999]

-- instance OEIS 105441 where
--   oeis = filter ((> 2) . (oeisIx @1227)) [1..]

-- instance OEIS 105471 where
--   oeis = 0 : 1 :
--      zipWith ((flip mod 100 .) . (+)) (oeis @105471) (tail (oeis @105471))

-- instance OEIS 105571 where
--   oeis = [x | x <- [3..], (oeisIx @64911) (x - 2) == 1, (oeisIx @64911) (x + 2) == 1]

-- instance OEIS 105612 where
--   oeisIx = (subtract 1) . (oeisIx @224)

-- instance OEIS 105728 where
--   oeis = tablList @105728
-- instance Table 105728 where
--   rowCol = rowCol_off @105728 @1 @1
--   rowT   = rowT_off   @105728 @1
--   tabl = iterate (\row -> zipWith (+) ([0] ++ tail row ++ [1]) $
--                                   zipWith (+) ([0] ++ row) (row ++ [0])) [1]

-- instance OEIS 105809 where
--   oeis = tablList @105809
-- instance Table 105809 where
--   tabl = map fst $ iterate
--      (\ (u:_, vs) -> (vs, zipWith (+) ([u] ++ vs) (vs ++ [0]))) ([1], [1,1])

-- instance OEIS 105870 where
--   oeis = 1 : 1 : zipWith (\u v -> (u + v) `mod` 7)
--                                  (tail (oeis @105870)) (oeis @105870)

-- instance OEIS 106001 where
--   oeisIx n = (oeis @250310) !! (n - 1)
--   oeis = [1..9] ++ [11] ++ f ([0..9] ++ [1,1]) 11 (10 : [12..])
--       where f ss i zs = g zs where
--           g (x:xs) = if ss !! i /= mod x 10
--               then g xs
--               else x : f (ss ++ map (read . return) (show x))
--                   (i + 1) (delete x zs)

-- instance OEIS 106039 where
--   oeis = filter belge0 [0..] where
--      belge0 n = n == (head $ dropWhile (< n) $
--                       scanl (+) 0 $ cycle ((map (read . return) . show) n))

-- instance OEIS 106108 where
--   oeis =
--      7 : zipWith (+) (oeis @106108) (zipWith gcd (oeis @106108) [2..])

-- instance OEIS 106146 where
--   oeisIx = flip mod 10 . (oeisIx @1358)

-- instance OEIS 106151 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 . concatMap
--      (\bs'@ (b:bs) -> if b == 0 then bs else bs') . group . (rowT @30308)

-- instance OEIS 106315 where
--   oeisIx n = n * (oeisIx @5) n `mod` (oeisIx @203) n

-- instance OEIS 106371 where
--   oeis = map fromJust $ takeWhile (/= Nothing) $ map f [1..] where
--      f n = g 2 n where
--        g b x = h x 0 where
--          h 0 y = if b <= 10 then Just (oeisIx y) else Nothing
--          h z y = if r == 0 then g (b + 1) n else h z' (10 * y + r)
--                  where (z', r) = divMod z b

-- instance OEIS 106372 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @106370))

-- instance OEIS 106404 where
--   oeisIx n = genericLength [d | d <- takeWhile (<= n) (oeis @100484), mod n d == 0]

-- instance OEIS 106432 where
--   oeis = zipWith (levenshtein `on` show)
--                          (oeis @79) $ tail (oeis @79) where
--      levenshtein us vs = last $ foldl transform [0..length us] vs where
--         transform xs@ (x:xs') c = scanl compute (x+1) (zip3 us xs xs') where
--            compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

-- instance OEIS 106435 where
--   oeis = 0 : 3 : map (* 3) (zipWith (+) (oeis @106435) (tail
--   oeis))

-- instance OEIS 106439 where
--   oeis = filter belge1 [1..] where
--      belge1 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 1 $ cycle (map (read . return) $ show x))

-- instance OEIS 106518 where
--   oeis = filter belge2 [2..] where
--      belge2 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 2 $ cycle (map (read . return) $ show x))

-- instance OEIS 106596 where
--   oeis = filter belge3 [3..] where
--      belge3 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 3 $ cycle (map (read . return) $ show x))

-- instance OEIS 106631 where
--   oeis = filter belge4 [4..] where
--      belge4 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 4 $ cycle (map (read . return) $ show x))

-- instance OEIS 106708 where
--   oeisIx 1           = 0
--   oeisIx n
--      | (oeisIx @10051) n == 1 = 0
--      | otherwise = read $ concat $ (map show) $ init $ tail $ (rowT @27750) n

-- instance OEIS 106747 where
--   oeisIx n = if n == 0 then 0 else 10 * (oeisIx n') + div d 2
--               where (n', d) = divMod n 10

-- instance OEIS 106792 where
--   oeis = filter belge5 [5..] where
--      belge5 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 5 $ cycle ((map (read . return) . show) x))

-- instance OEIS 106799 where
--   oeisIx = (oeisIx @1222) . (oeisIx @65330)

-- instance OEIS 106828 where
--   oeis = tablList @106828
-- instance Table 106828 where
--   tabf = map (fst . fst) $ iterate f (([1], [0]), 1) where
--      f ((us, vs), x) =
--        ((vs, map (* x) $ zipWith (+) ([0] ++ us) (vs ++ [0])), x + 1)

-- instance OEIS 106831 where
--   oeis = tablList @106831
-- instance Table 106831 where
--   rowCol n k = (tabf @106831) !! n !! n
--   tabf = map (map (\ (_, _, left, right) -> left * right)) $
--      iterate (concatMap (\ (x, f, left, right) -> let f' = f * x in
--      [ (x + 1, f', f', right), (3, 2, 2, left * right)])) [ (3, 2, 2, 1)]

-- instance OEIS 107014 where
--   oeis = filter belge6 [6..] where
--      belge6 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 6 $ cycle (map (read . return) $ show x))

-- instance OEIS 107018 where
--   oeis = filter belge7 [7..] where
--      belge7 x = x == (head $ dropWhile (< x) $
--                        scanl (+) 7 $ cycle (map (read . return) $ show x))

-- instance OEIS 107032 where
--   oeis = filter belge8 [8..] where
--      belge8 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 8 $ cycle (map (read . return) $ show x))

-- instance OEIS 107043 where
--   oeis = filter belge9 [9..] where
--      belge9 x = x == (head $ dropWhile (< x) $
--                       scanl (+) 9 $ cycle (map (read . return) $ show x))

-- instance OEIS 107128 where
--   oeisIx n = if n == 0 then 0 else 10 * (oeisIx @107128 n') + m * d + (1 - m) * d'
--               where (d', m) = divMod d 2
--                     (n', d) = divMod n 10

-- instance OEIS 107345 where
--   oeisIx n = (oeisIx @7318)' (oeisIx n) (oeisIx n)

-- instance OEIS 107430 where
--   oeis = tablList @107430
-- instance Table 107430 where
--   tabl = map sort (tabl @7318)

-- instance OEIS 107458 where
--   oeis = 1 : 0 : 0 : 0 : zipWith (+) (oeis @107458)
--      (zipWith (+) (tail (oeis @107458)) (drop 2 (oeis @107458)))

-- instance OEIS 107711 where
--   oeis = tablList @107711
-- instance Table 107711 where
--   tabl = [1] : zipWith (map . flip div) [1..]
--                  (tail $ zipWith (zipWith (*)) (tabl @7318) (tabl @109004))

-- instance OEIS 107715 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @7090)

-- instance OEIS 107740 where
--   oeisIx n = genericLength [ () | let p = (oeisIx @40) n,
--                            m <- [max 0 (p - 9 * (oeisIx @55642) p) .. p - 1],
--                            oeisIx m == p]

-- instance OEIS 107741 where
--   oeisIx n = if null ms then 0 else head ms  where
--      ms = [m | let p = (oeisIx @40) n,
--                m <- [max 0 (p - 9 * (oeisIx @55642) p) .. p - 1], (oeisIx @62028) m == p]

-- instance OEIS 107743 where
--   oeis = filter ((== 0) . (oeisIx @10051)' . (oeisIx @62028)) [1..]

-- instance OEIS 107750 where
--   oeis = 0 : f 0 where
--      f x = y : f y where
--        y = head [z | z <- [x + 1 ..], (oeisIx @23416) z /= (oeisIx @23416) x]

-- instance OEIS 107782 where
--   oeisIx n = (oeisIx @23416) n - (oeisIx @87116) n

-- instance OEIS 107788 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @107788) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (8 * y, i + 1, j) $ insert (11 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 107801 where
--   oeis = 2 : f 2 (tail (oeis @40)) where
--      f x ps = g ps where
--        g (q:qs) | null (show x `intersect` show q) = g qs
--                 | otherwise                        = q : f q (delete q ps)

-- instance OEIS 107988 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @107988) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (4 * y, i + 1, j) $ insert (11 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 108018 where
--   oeisIx = sum . map (oeisIx @10051)' . nub . map sum .
--             tail . subsequences . flip take (oeis @40)

-- instance OEIS 108035 where
--   oeis = tablList @108035
-- instance Table 108035 where
--   rowCol = rowCol_off @108035 @1 @1
--   rowT   = rowT_off   @108035 @1
--   tabl = zipWith replicate [1..] $ drop 2 (oeis @45)

-- instance OEIS 108037 where
--   oeis = tablList @108037
-- instance Table 108037 where
--   tabl = zipWith replicate [1..] (oeis @45)

-- instance OEIS 108040 where
--   oeis = tablList @108040
-- instance Table 108040 where
--   rowT n = (tabl @108040) !! k
--   tabl = ox False (tabl @8281) where
--     ox turn (xs:xss) = (if turn then reverse xs else xs) : ox (not turn) xss

-- instance OEIS 108044 where
--   oeis = tablList @108044
-- instance Table 108044 where
--   tabl = zipWith drop [0..] $ map (intersperse 0) (tabl @7318)

-- instance OEIS 108090 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @108090) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (11 * y, i + 1, j) $ insert (13 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 108218 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @108218) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (11 * y, i + 1, j) $ insert (12 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 108299 where
--   oeis = tablList @108299
-- instance Table 108299 where
--   tabl = [1] : iterate (\row ->
--      zipWith (+) (zipWith (*) ([0] ++ row) (oeis @33999))
--                  (zipWith (*) (row ++ [0]) (oeis @59841))) [1,-1]

-- instance OEIS 108309 where
--   oeisIx = sum . (map (oeisIx @10051)) . (rowT @176271)

-- instance OEIS 108348 where
--   oeis = 1 : f [2..] where
--      f (x:xs) = g (oeis @40) where
--        g (p:ps) = h 0 $ map ((`div` (p - 1)) . subtract 1) $
--                             iterate (* p) (p ^ 2) where
--          h i (pp:pps) | pp > x    = if i == 0 then f xs else g ps
--                       | pp < x    = h 1 pps
--                       | otherwise = x : f xs

-- instance OEIS 108396 where
--   oeis = tablList @108396
-- instance Table 108396 where
--   tabl = zipWith (\v ws -> map (flip div 2 . (* v) . (+ 1)) ws)
--                          [0..] (tabl @79901)

-- instance OEIS 108546 where
--   oeis =  2 : concat
--      (transpose [oeis, (oeis @2144)])

-- instance OEIS 108561 where
--   oeis = tablList @108561
-- instance Table 108561 where
--   tabl = map reverse (tabl @112465)

-- instance OEIS 108617 where
--   oeis = tablList @108617
-- instance Table 108617 where
--   tabl = [0] : iterate f [1,1] where
--      f row@ (u:v:_) = zipWith (+) ([v - u] ++ row) (row ++ [v - u])

-- instance OEIS 108647 where
--   oeisIx = flip (oeisIx @103371) 3 . (+ 3)

-- instance OEIS 108655 where
--   oeis = filter f (oeis @40) where
--      f p = any (> 0) $ map (oeisIx . (oeisIx @37213) . (p -)) $
--                            takeWhile (< p) (oeis @74985)

-- instance OEIS 108687 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @108687) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (9 * y, i + 1, j) $ insert (11 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 108698 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @108698) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (6 * y, i + 1, j) $ insert (11 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 108730 where
--   oeis = tablList @108730
-- instance Table 108730 where
--   rowCol = rowCol_off @108730 @1 @1
--   rowT = f . group . reverse . unfoldr
--      (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) where
--      f [] = []
--      f [os] = replicate (length os) 0
--      f (os:zs:dss) = replicate (length os - 1) 0 ++ [length zs] ++ f dss
--   tabf = map (rowT @108730) [1..]
--   oeis = concat (tabf @108730)

-- instance OEIS 108731 where
--   oeis = tablList @108731
-- instance Table 108731 where
--   rowCol n k = (rowT @108731) n !! k
--   rowT 0 = [0]
--   rowT n = t n $ reverse $ takeWhile (<= n) $ tail (oeis @142)
--      where t 0 []     = []
--            t x (b:bs) = x' : t m bs where (x',m) = divMod x b
--   tabf = map (rowT @108731) [0..]

-- instance OEIS 108761 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @108761) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (10 * y, i + 1, j) $ insert (13 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 108775 where
--   oeisIx n = div (oeisIx n) n

-- instance OEIS 108779 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @108779) !! (n - 1)
--   oeis = f $ singleton (1,0,0) where
--      f s = y : f (insert (10 * y, i + 1, j) $ insert (11 * y, i, j + 1) s')
--            where ((y, i, j), s') = deleteFindMin s

-- instance OEIS 108804 where
--   oeis = f [head (oeis @10060)] $ tail (oeis @10060) where
--      f xs (z:zs) = (sum $ zipWith (*) xs (reverse xs)) : f (z : xs) zs

-- instance OEIS 108839 where
--   oeis = 1 : f 2 [1] where
--      f x zs = z : f (x + 1) (z : zs) where
--        z = toInteger $ sum $ map (oeisIx . (+ x)) zs

-- instance OEIS 108872 where
--   oeis = tablList @108872
-- instance Table 108872 where
--   rowCol = rowCol_off @108872 @1 @1
--   rowT   = rowT_off   @108872 @1
--   tabl = map (\x -> [x + 1 .. 2 * x]) [1..]

-- instance OEIS 108898 where
--   oeis = -1 : 1 : 3 :
--      zipWith (-) (map (* 3) $ drop 2 (oeis @108898)) (map (* 2) (oeis @108898))

-- instance OEIS 108906 where
--   oeis = zipWith (-) (tail (oeis @6899)) (oeis @6899)

-- instance OEIS 109128 where
--   oeis = tablList @109128
-- instance Table 109128 where
--   tabl = iterate (\row -> zipWith (+)
--      ([0] ++ row) (1 : (map (+ 1) $ tail row) ++ [0])) [1]

-- instance OEIS 109129 where
--   oeis = 0 : 1 : g 3 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then (oeisIx @109129) t else (oeisIx @109129) r + (oeisIx @109129) s
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 109303 where
--   oeis = filter ((> 0) . (oeisIx @107846)) [0..]

-- instance OEIS 109373 where
--   oeis = filter ((== 1) . (oeisIx @64911)) (oeis @88707)

-- instance OEIS 109400 where
--   oeis = concat $ zipWith (++) (tabl @2260) (tabl @37126)

-- instance OEIS 109465 where
--   oeis = f 1 [1..] where
--      f o xs = g xs where
--        g (z:zs) = if o' == o then g zs else z : f o' (delete z xs)
--                   where o' = (oeisIx @1221) z

-- instance OEIS 109613 where
--   oeisIx = (+ 1) . (* 2) . (`div` 2)
--   oeis = 1 : 1 : map (+ 2) (oeis @109613)

-- instance OEIS 109671 where
--   oeis = concat (transpose [1 : f 1 (oeis @109671), (oeis @109671)])
--      where f u (v:vs) = y : f y vs where
--              y = if u > v then u - v else u + v

-- instance OEIS 109681 where
--   oeis = map (foldr (\d v -> 3 * v + d) 0) $ f (tabf @30341) where
--      f vss = (g 0 vss) : f (tail vss)
--      g k (ws:wss) = if k < length ws then ws !! k : g (k + 1) wss else []

-- instance OEIS 109682 where
--   oeis = compl (oeis @109681) [0..] where
--      compl us'@ (u:us) vs'@ (v:vs)
--          | u == v    = compl us vs
--          | u > 3 * v = v : compl us (delete u vs)
--          | otherwise = compl us (delete u vs')

-- instance OEIS 109683 where
--   oeisIx = (oeisIx @7089) . (oeisIx @109681)

-- instance OEIS 109735 where
--   oeis = scanl1 (+) (oeis @109890)

-- instance OEIS 109736 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @109890))

-- instance OEIS 109761 where
--   oeis = filter ((== 1) . (oeisIx @210455)) (oeis @91999)

-- instance OEIS 109812 where
--   oeis = f 0 [1..] :: [Int] where
--      f v ws = g ws where
--        g (x:xs) = if v .&. x == 0 then x : f x (delete x ws) else g xs

-- instance OEIS 109890 where
--   oeis = 1 : 2 : 3 : f (4, []) 6 where
--      f (m,ys) z = g $ dropWhile (< m) $ (rowT @27750) z where
--        g (d:ds) | elem d ys = g ds
--                 | otherwise = d : f (ins [m, m + 1 ..] (insert d ys)) (z + d)
--        ins (u:us) vs'@ (v:vs) = if u < v then (u, vs') else ins us vs

-- instance OEIS 109906 where
--   oeis = tablList @109906
-- instance Table 109906 where
--   tabl = zipWith (zipWith (*)) (tabl @58071) (tabl @7318)

-- instance OEIS 109925 where
--   oeisIx n = sum $ map (oeisIx' . (n -)) $ takeWhile (< n)  (oeis @79)

-- instance OEIS 109981 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @55642)) (oeis @46704)

-- instance OEIS 109983 where
--   oeis = tablList @109983
-- instance Table 109983 where
--   tabf = zipWith (++) (map (flip take (repeat 0)) [0..]) (tabl @63007)

-- instance OEIS 109984 where
--   oeisIx = sum . zipWith (*) [0..] . (rowT @109983)

-- instance OEIS 110080 where
--   import Data.Set (singleton, member, insert)
--   oeisIx n = (oeis @110080) !! (n - 1)
--   oeis = 1 : f 1 (oeis @40) (singleton 1) where
--      f x (p:ps) m = y : f y ps (insert y m) where
--        y = g x p
--        g 0 _ = h x p
--        g u 0 = u
--        g u v = g (u - 1) (if member (u - 1) m then v else v - 1)
--        h u 0 = u
--        h u v = h (u + 1) (if member (u + 1) m then v else v - 1)

-- instance OEIS 110085 where
--   oeis = filter (\x -> (oeisIx @51612) x < (oeisIx @110088) x) [1..]

-- instance OEIS 110086 where
--   oeis = filter (\x -> (oeisIx @51612) x <= (oeisIx @110088) x) [1..]

-- instance OEIS 110087 where
--   oeis = filter (\x -> (oeisIx @51612) x > (oeisIx @110088) x) [1..]

-- instance OEIS 110088 where
--   oeisIx n = (oeisIx @5) n ^ (oeisIx @1221) n

-- instance OEIS 110157 where
--   oeis = 0 : map ((+ 1) . (oeisIx @110157) . (+ 1)) (oeis @75423)

-- instance OEIS 110170 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @128966) (2 * n) n

-- instance OEIS 110240 where
--   oeisIx = foldl (\v d -> 2 * v + d) 0 . map toInteger . (rowT @70950)

-- instance OEIS 110353 where
--   oeisIx n = (+ 1) $ fromJust $
--      findIndex ((== 0) . (`mod` t)) $ dropWhile (<= t) (oeis @217)
--      where t = (oeisIx @217) n

-- instance OEIS 110475 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength us - 1 + 2 * length vs where
--               (us, vs) = span (== 1) $ (rowT @118914) n

-- instance OEIS 110745 where
--   oeisIx n = read (concat $ transpose [ns, reverse ns]) :: Integer
--               where ns = show n

-- instance OEIS 110765 where
--   oeisIx = product . zipWith (^) (oeis @40) .  reverse . (rowT @30308)

-- instance OEIS 110766 where
--   oeis = concat $ transpose [oeis, (oeis @110766)]

-- instance OEIS 110910 where
--   {- program for verification of periodic cases. The non-periodic cases listed here evolve into a periodic kernel plus gliders whose paths ahead do not intersect each other or the kernel (gliders marching in single file are not counted as intersecting). Replace leading dots with spaces before running! -}
--   import Data.Set
--   main = print [if n `elem` known then 0 else a n | n<-[0..105]]
--   known = [56,71,72,75,78,82,85,86,87,88,91,92,93,94,96,98,100,102,103,105]
--   a n = count empty (iterate evolve (fromList [ (x,0) | x<-[1..n]]))
--   neighbors (x,y) = fromList
--   ................. [ (x+u,y+v) | u<-[ -1,0,1], v<-[ -1,0,1], (u,v)/= (0,0)]
--   evolve life =
--   . let fil f = Data.Set.filter
--   ............. (\x-> f (size (life `intersection` neighbors x)))
--   . in (life `difference` fil (\k-> k<2 || k>3) life) `union` fil (== 3)
--   .... (unions (Prelude.map neighbors (elems life)) `difference` life)
--   count o (x:xs) | x `member` o = 0
--   .............. | otherwise = 1 + count (o `union` singleton x) xs

-- instance OEIS 111006 where
--   oeis = tablList @111006
-- instance Table 111006 where
--   tabl =  map fst $ iterate (\ (us, vs) ->
--      (vs, zipWith (+) (zipWith (+) ([0] ++ us ++ [0]) ([0,0] ++ us))
--                       ([0] ++ vs))) ([1], [0,1])

-- instance OEIS 111046 where
--   oeisIx = (* 2) . (oeisIx @54735)

-- instance OEIS 111133 where
--   oeisIx = subtract 1 . (oeisIx @9)

-- instance OEIS 111178 where
--   oeisIx = p $ tail (oeis @5563) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 111192 where
--   oeis = f (oeis @40) where
--      f (p:ps@ (q:r:_)) | q - p == 6 = (p*q) : f ps
--                       | r - p == 6 = (p*r) : f ps
--                       | otherwise  = f ps

-- instance OEIS 111208 where
--   oeisIx n = genericLength $ takeWhile (<= (oeisIx @217) n) (oeis @40)

-- instance OEIS 111227 where
--   oeis = filter (\x -> (oeisIx @19294) x > x) [1..]

-- instance OEIS 111418 where
--   oeis = tablList @111418
-- instance Table 111418 where
--   tabl = map reverse (tabl @122366)

-- instance OEIS 111546 where
--   oeis = 1 : f 2 [1] where
--      f v ws@ (w:_) = y : f (v + 1) (y : ws) where
--                     y = v * w + (sum $ zipWith (*) ws $ reverse ws)

-- instance OEIS 111650 where
--   oeis = tablList @111650
--   rowCol = rowCol_off @111650 @1 @1
--   rowT   = rowT_off   @111650 @1
--   tabl = iterate (\xs@ (x:_) -> map (+ 2) (x:xs)) [2]
--   oeis = concat (tabl @111650)

-- instance OEIS 111711 where
--   oeis = 1 : zipWith (+) (oeis @111711) (oeis @80512)

-- instance OEIS 111712 where
--   oeis = scanl (+) 1 (oeis @195013)

-- instance OEIS 112465 where
--   oeis = tablList @112465
-- instance Table 112465 where
--   tabl = iterate f [1] where
--      f xs'@ (x:xs) = zipWith (+) ([-x] ++ xs ++ [0]) ([0] ++ xs')

-- instance OEIS 112468 where
--   oeis = tablList @112468
-- instance Table 112468 where
--   tabl = iterate (\xs -> zipWith (-) ([2] ++ xs) (xs ++ [0])) [1]

-- instance OEIS 112798 where
--   oeis = tablList @112798
-- instance Table 112798 where
--   rowCol = rowCol_off @112798 @2 @1
--   rowT   = rowT_off @112798 @2
--   tabf = map (map (oeisIx @49084)) $ tail (tabf @27746)

-- instance OEIS 112988 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @89088)) . (oeisIx @40)

-- instance OEIS 112990 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @89088))

-- instance OEIS 113215 where
--   oeis = concat $ zipWith take
--                           [1, 3 ..] $ map (repeat . (oeisIx @6218)) [0 ..]

-- instance OEIS 113232 where
--   oeisIx = (oeisIx @109812) . (oeisIx @109812)

-- instance OEIS 113233 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @109812))

-- instance OEIS 113234 where
--   oeisIx = (oeisIx @113233) . (oeisIx @113233)

-- instance OEIS 113646 where
--   oeisIx n = if n < 3 then 4 else (oeisIx @14683) n

-- instance OEIS 114102 where
--   oeisIx n = genericLength $ filter (== 1) $
--               map (length . nub . (map (oeisIx @10888))) $ ps 1 n
--      where ps x 0 = [[]]
--            ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

-- instance OEIS 114180 where
--   oeis = filter ((== 0) . (oeisIx @261890)) [1..]

-- instance OEIS 114183 where
--   oeis = 1 : f [1] where
--      f xs@ (x:_) = y : f (y : xs) where
--        y = if z `notElem` xs then z else 2 * x where z = (oeisIx @196) x

-- instance OEIS 114227 where
--   oeisIx n = head [p | p <- tail (oeis @40),
--                         (oeisIx @10051)' (2 * p + (oeisIx @40) n) == 1]

-- instance OEIS 114228 where
--   oeisIx n = head [m | m <- [1..],
--                         (oeisIx @10051) (oeisIx n + 2 * (oeisIx @40) m) == 1]

-- instance OEIS 114229 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (map (oeisIx @114228) [2..]))

-- instance OEIS 114230 where
--   oeisIx n = head [p | let q = (oeisIx @40) n,
--                         p <- reverse $ takeWhile (< q) (oeis @40),
--                         (oeisIx @10051) (q + 2 * p) == 1]

-- instance OEIS 114231 where
--   oeisIx n = head [m | m <- [1..],
--                         (oeisIx @10051) (oeisIx n + 2 * (oeisIx @40) (n - m)) == 1]

-- instance OEIS 114233 where
--   oeisIx n = head [m | m <- [1 .. n],
--                         (oeisIx @10051)' (2 * (oeisIx @40) n + (oeisIx @40) m) == 1]

-- instance OEIS 114235 where
--   oeisIx n = head [p | let q = (oeisIx @40) n,
--                         p <- reverse $ takeWhile (< q) (oeis @40),
--                         (oeisIx @10051) (2 * q + p) == 1]

-- instance OEIS 114236 where
--   oeisIx n = head [m | m <- [1..],
--                         (oeisIx @10051) (2 * (oeisIx @40) n + (oeisIx @40) (n - m)) == 1]

-- instance OEIS 114262 where
--   oeisIx n = head [q | let (p:ps) = drop (n - 1) (oeis @40),
--                         q <- ps, (oeisIx @10051) (p + 2 * q) == 1]

-- instance OEIS 114263 where
--   oeisIx n = head [m | m <- [1..n],
--                         (oeisIx @10051) (oeisIx n + 2 * (oeisIx @40) (n + m)) == 1]

-- instance OEIS 114265 where
--   oeisIx n = head [p | let (q:qs) = drop (n - 1) (oeis @40), p <- qs,
--                         (oeisIx @10051) (2 * q + p) == 1]

-- instance OEIS 114266 where
--   oeisIx n = head [m | m <- [1..],
--                         (oeisIx @10051) (2 * (oeisIx @40) n + (oeisIx @40) (n + m)) == 1]

-- instance OEIS 114283 where
--   oeis = tablList @114283
-- instance Table 114283 where
--   tabl = iterate
--      (\row -> (sum $ zipWith (+) row $ reverse row) : row) [1]

-- instance OEIS 114327 where
--   oeis = tablList @114327
-- instance Table 114327 where
--   tabl = zipWith (zipWith (-)) (tabl @25581) (tabl @2262)

-- instance OEIS 114334 where
--   oeis = (rowT @27750) (6 ^ 6)

-- instance OEIS 114374 where
--   oeisIx = p (oeis @13929) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 114897 where
--   oeis = 1 : f 1 [1] where
--      f x zs = z : f (x + 1) (z : zs) where
--        z = toInteger $ sum $ map (oeisIx . (+ x)) zs

-- instance OEIS 115068 where
--   oeis = tablList @115068
-- instance Table 115068 where
--   rowCol = rowCol_off @115068 @1 @1
--   rowT   = rowT_off   @115068 @1
--   tabl = iterate (\row -> zipWith (+) (row ++ [1]) $
--                                   zipWith (+) (row ++ [0]) ([0] ++ row)) [1]

-- instance OEIS 115300 where
--   oeisIx n = (oeisIx @54054) n * (oeisIx @54055) n

-- instance OEIS 115339 where
--   oeis = [1, 1, 2, 3] ++
--                  zipWith (+) (oeis @115339) (drop 2 (oeis @115339))

-- instance OEIS 115390 where
--   oeis = 0 : 0 : 1 : map (* 2) (zipWith (-) (oeis @115390)
--      (tail $ map (* 2) $ zipWith (-) (oeis @115390) (tail (oeis @115390))))

-- instance OEIS 115408 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @115409))

-- instance OEIS 115409 where
--   oeis = tablList @115409
--   rowCol = rowCol_off @115409 @1 @1
--   rowT   = rowT_off   @115409 @1
--   tabl = map f $ drop 2 $ inits (oeis @24431) where
--      f xs = reverse $ map (z -) zs where (z:zs) = reverse xs
--   oeis = concat (tabl @115409)

-- instance OEIS 115627 where
--   oeis = tablList @115627
-- instance Table 115627 where
--   rowCol = rowCol_off @115627 @2 @1
--   rowT = map (oeisIx @100995) . (rowT @141809) . (oeisIx @142)
--   tabf = map (rowT @115627) [2..]

-- instance OEIS 115671 where
--   oeisIx = p [x | x <- [0..], (mod x 32) `notElem` [0,2,12,14,16,18,20,30]]
--      where p _          0 = 1
--            p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 115944 where
--   oeisIx = p (tail (oeis @142)) where
--      p _      0             = 1
--      p (f:fs) m | m < f     = 0
--                 | otherwise = p fs (m - f) + p fs m

-- instance OEIS 115945 where
--   oeis = elemIndices 0 $ map (oeisIx @115944) [0..]

-- instance OEIS 116371 where
--   oeisIx n = p (oeis @17173) n where
--      p _  0 = 1
--      p [] _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 116536 where
--   oeis = catMaybes $ zipWith div' (oeis @2110) (oeis @7504) where
--      div' x y | m == 0    = Just x'
--               | otherwise = Nothing where (x',m) = divMod x y

-- instance OEIS 116549 where
--   oeisIx = genericIndex (oeis @116549)
--   oeis = 1 : zipWith ((+) `on` (oeisIx @116549)) (oeis @523) (oeis @53645)

-- instance OEIS 116590 where
--   oeis = 1 : zipWith (+) (oeis @5185) (drop 2 (oeis @5185))

-- instance OEIS 116619 where
--   oeisIx = (+ 1) . (oeisIx @71681)

-- instance OEIS 116666 where
--   oeis = tablList @116666
-- instance Table 116666 where
--   rowCol = rowCol_off @116666 @1 @1
--   rowT   = rowT_off   @116666 @1
--   tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @158405)

-- instance OEIS 116697 where
--   oeis = [1,1,-2,2]
--                  ++ (zipWith (-) (oeis @116697)
--                                  $ zipWith (+) (tail (oeis @116697))
--                                                (drop 3 (oeis @116697)))
--   oeis = 0 : (map negate $ map (oeisIx @116697) [0,2..])
--   oeis = 1 : map (oeisIx @116697) [1,3..]
--   oeis = zipWith (-) (tail (oeis @116697)) (oeis @116697)
--   oeis = map (oeisIx @186679) [0,2..]
--   oeis = 0 : (map negate $ map (oeisIx @186679) [1,3..])
--   oeis = 1 : -3 : (zipWith (+) (oeis @186679) $ drop 2 (oeis @186679))

-- instance OEIS 116700 where
--   oeis = filter early [1 ..] where
--      early z = not (reverse (show (z - 1)) `isPrefixOf` fst bird) where
--         bird = fromJust $ find ((show z `isPrefixOf`) . snd) xys
--      xys = iterate (\ (us, v : vs) -> (v : us, vs))
--                    ([], concatMap show [0 ..])

-- instance OEIS 116853 where
--   oeis = tablList @116853
-- instance Table 116853 where
--   rowCol = rowCol_off @116853 @1 @1
--   rowT   = rowT_off   @116853 @1
--   tabl = map reverse $ f (tail (oeis @142)) [] where
--      f (u:us) vs = ws : f us ws where ws = scanl (-) u vs

-- instance OEIS 116854 where
--   oeis = tablList @116854
-- instance Table 116854 where
--   rowCol = rowCol_off @116854 @1 @1
--   rowT   = rowT_off   @116854 @1
--   tabl = [1] : zipWith (:) (tail $ map head tss) tss
--                  where tss = (tabl @116853)

-- instance OEIS 116933 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10051)' (n + k * (oeisIx @79578) n) == 1]

-- instance OEIS 116934 where
--   oeisIx n = head [q | k <- [1..], let q = n + k * (oeisIx @79578) n,
--                         (oeisIx @10051)' q == 1]

-- instance OEIS 116939 where
--   oeis = 0 : f [0] where
--      f xs@ (x : _) = ys ++ f ys where
--        ys = if odd x then (x + 1 : x : map (+ 1) xs) else map (+ 1) xs

-- instance OEIS 116940 where
--   oeisIx n = last $ elemIndices n $ takeWhile (<= n + 1) (oeis @116939)

-- instance OEIS 116941 where
--   oeis = f 0 1 (zip (oeis @116939) [0..]) [] where
--      f u v xis'@ ((x,i):xis) ws
--        | x == u    = i : f u v xis ws
--        | x == v    = f u v xis (i : ws)
--        | otherwise = reverse ws ++ f v x xis' []

-- instance OEIS 116942 where
--   oeisIx = fromJust . (`elemIndex` (oeis @116941))

-- instance OEIS 116966 where
--   oeis = zipWith (+) [0..] $ drop 2 (oeis @140081)

-- instance OEIS 117047 where
--   oeis = [x | k <- [0..], let x = 60 * k + 11, (oeisIx @10051)' x == 1]

-- instance OEIS 117128 where
--   import Data.Set (singleton, notMember, insert)
--   oeisIx n = (oeis @117128) !! n
--   oeis = 1 : f 1 (oeis @40) (singleton 1) where
--      f x (p:ps) s | x' > 0 && x' `notMember` s = x' : f x' ps (insert x' s)
--                   | otherwise                  = xp : f xp ps (insert xp s)
--                   where x' = x - p; xp = x + p

-- instance OEIS 117140 where
--   oeis = 5 : 7 : ulam 2 7 (oeis @117140)

-- instance OEIS 117214 where
--   oeisIx n = product $
--      filter ((> 0) . (mod m)) $ takeWhile (< (oeisIx @6530) m) (oeis @40)
--      where m = (oeisIx @5117) n

-- instance OEIS 117317 where
--   oeis = tablList @117317
-- instance Table 117317 where
--   tabl = map reverse (tabl @56242)

-- instance OEIS 117366 where
--   oeisIx = (oeisIx @151800) . (oeisIx @6530)

-- instance OEIS 117546 where
--   oeisIx = p $ drop 3 (oeis @73) where
--      p _  0     = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 117591 where
--   oeis = zipWith (+) (oeis @79) (oeis @45)

-- instance OEIS 117671 where
--   oeisIx n = (oeisIx @258993) (2 * n + 1) n

-- instance OEIS 117704 where
--   oeis = 1 : zipWith (-) (tail (oeis @5214)) (oeis @5214)

-- instance OEIS 117767 where
--   oeisIx = (+ 1) . (* 2) . (oeisIx @6)

-- instance OEIS 117818 where
--   oeisIx n = if (oeisIx @10051) n == 1 then n else (oeisIx @32742) n

-- instance OEIS 117872 where
--   oeisIx = flip mod 2 . (oeisIx @7501)

-- instance OEIS 117890 where
--   oeis = [x | x <- [1..], let z = (oeisIx @23416) x, z > 0, mod x z == 0]

-- instance OEIS 117922 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @55265))

-- instance OEIS 117930 where
--   oeisIx n = p (tail (oeis @142)) $ 2*n where
--      p _          0             = 1
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 117989 where
--   oeis = tail $ zipWith (-)
--                         (map (* 2) (oeis @41)) $ tail (oeis @41)

-- instance OEIS 118013 where
--   oeis = tablList @118013
-- instance Table 118013 where
--   rowCol = rowCol_off @118013 @1 @1
--   rowT n = map (div (n^2)) [1..n]
--   tabl = map (rowT @118013) [1..]

-- instance OEIS 118139 where
--   oeis = filter ((> 1) . (oeisIx @52343)) [0..]

-- instance OEIS 118416 where
--   oeis = tablList @118416
-- instance Table 118416 where
--   rowCol = rowCol_off @118416 @1 @1
--   rowT 1 = [1]
--   rowT n = (map (* 2) $ (rowT @118416) (n - 1)) ++ [oeisIx (n-1)]
--   tabl = map (rowT @118416) [1..]

-- instance OEIS 118478 where
--   oeisIx n = (+ 1) . fromJust $ elemIndex 0 $
--               map (flip mod (oeisIx n)) $ tail (oeis @2378)

-- instance OEIS 118532 where
--   oeis = iterate ((+ 15) . (oeisIx @4086)) 1

-- instance OEIS 118628 where
--   oeis = 3 : f [3] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f (ys) where
--             ys = concat $ transpose [map length zss, map head zss]
--             zss = group $ sort xs

-- instance OEIS 118668 where
--   oeisIx = (oeisIx @43537) . (oeisIx @217)
--   oeis = map (oeisIx @43537) (oeis @217)

-- instance OEIS 118882 where
--   oeis = findIndices (> 1) (oeis @161)

-- instance OEIS 118886 where
--   oeis = filter ((> 1) . (oeisIx @88534)) (oeis @3136)

-- instance OEIS 118914 where
--   oeis = tablList @118914
-- instance Table 118914 where
--   rowCol = rowCol_off @118914 @2 @1
--   rowT   = rowT_off @118914 @2
--   tabf = map sort $ tail (tabf @124010)

-- instance OEIS 118954 where
--   oeis = filter f [1..] where
--      f x = all (== 0) $ map (oeisIx . (x -)) $ takeWhile (< x) (oeis @79)

-- instance OEIS 118955 where
--   oeis = filter f [1..] where
--      f x = any (== 1) $ map (oeisIx . (x -)) $ takeWhile (< x) (oeis @79)

-- instance OEIS 118959 where
--   oeis = filter
--      (\x -> let x' = (oeisIx @4086) x in x' /= x && x `mod` x' == 0) [1..]

-- instance OEIS 118965 where
--   oeisIx = sum . map (0 ^) . (rowT @128924)

-- instance OEIS 119246 where
--   oeis =
--       filter (\x -> (oeisIx @10888) x `elem` (rowT @31298) (fromInteger x)) [0..]

-- instance OEIS 119258 where
--   oeis = tablList @119258
-- instance Table 119258 where
--   oeis = concat (tabl @119258)
--   tabl = iterate (\row -> zipWith (+)
--      ([0] ++ init row ++ [0]) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

-- instance OEIS 119259 where
--   oeisIx n = (oeisIx @119258) (2 * n) n

-- instance OEIS 119345 where
--   oeis = filter ((== 1) . (oeisIx @52343)) [0..]

-- instance OEIS 119347 where
--   oeisIx = genericLength . nub . map sum . tail . subsequences . (rowT @27750)'

-- instance OEIS 119354 where
--   oeisIx = fromJust . (`elemIndex` (oeis @119352))

-- instance OEIS 119387 where
--   oeisIx n = genericLength $ takeWhile (< (oeisIx @70940) n) [1..n]

-- instance OEIS 119393 where
--   oeis = filter
--      (\x -> not $ null $ show x `intersect` (show $ (oeisIx @40) x)) [1..]

-- instance OEIS 119416 where
--   oeisIx n = n * (oeisIx $ (oeisIx @6530) n)

-- instance OEIS 119467 where
--   oeis = tablList @119467
-- instance Table 119467 where
--   tabl = map (map (flip div 2)) $
--                  zipWith (zipWith (+)) (tabl @7318) (tabl @130595)

-- instance OEIS 119629 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @14631))

-- instance OEIS 119709 where
--   oeis = tablList @119709
-- instance Table 119709 where
--   rowT n = map (foldr (\d v -> v * 2 + toInteger d) 0) $
--      filter (`isInfixOf` (oeisIx_row n)) $ take (n + 1) (tabf @30308)
--   tabf = map (rowT @119709) [0..]

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

-- instance OEIS 119999 where
--   oeisIx n = p (filter ((`isInfixOf` show n) . show) [1..n]) n where
--      p _  0 = 1
--      p [] _ = 0
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 120005 where
--   oeisIx = fromJust . (`elemIndex` (oeis @120004))

-- instance OEIS 120007 where
--   oeisIx 1 = 0
--   oeisIx n | until ((> 0) . (`mod` spf)) (`div` spf) n == 1 = spf
--             | otherwise = 0
--             where spf = (oeisIx @20639) n

-- instance OEIS 120511 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (tail (oeis @6949)))

-- instance OEIS 120739 where
--   oeisIx n = if n < 2 then 0 else sum $ (rowT @166454) n

-- instance OEIS 120880 where
--   oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (<= n) (oeis @5836)

-- instance OEIS 120944 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @2808)

-- instance OEIS 120960 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @24362)

-- instance OEIS 121032 where
--   oeis = filter ((isInfixOf "12") . show) (oeis @8594)

-- instance OEIS 121041 where
--   oeisIx n = genericLength $ filter (\d -> n `mod` d == 0
--                                      && show d `isInfixOf` show n) [1..n]

-- instance OEIS 121065 where
--   oeisIx = fromJust . (`elemIndex` (oeis @85513))

-- instance OEIS 121281 where
--   oeis = tablList @121281
-- instance Table 121281 where
--   tabl = [1] : f [1] (oeis @40) where
--      f xs@ (x:_) (p:ps) = ys : f ys ps where ys = (map (* p) xs) ++ [x]

-- instance OEIS 121369 where
--   oeis = 1 : 1 : zipWith ((+) `on` (oeisIx @7947))
--                          (oeis @121369) (tail (oeis @121369))

-- instance OEIS 121924 where
--   oeisIx (fi->n) = fi $ (oeisIx @7318) b 3 + (n - (oeisIx @7318) b 2) * (b* (b+3) - 2* (n+1)) `div` 4
--               where b = round $ sqrt $ 2 * fi n + 1/4

-- instance OEIS 121993 where
--   oeis = filter (\x -> (oeisIx @45918) x < x) [0..]

-- instance OEIS 122132 where
--   oeis = filter ((== 1) . (oeisIx @8966) . (oeisIx @265)) [1..]

-- instance OEIS 122366 where
--   oeis = tablList @122366
-- instance Table 122366 where
--   tabl = f 1 (tabl @7318) where
--      f x (_:bs:pss) = (take x bs) : f (x + 1) pss

-- instance OEIS 122425 where
--   import import Data.List (maximumBy); Data.Ord (comparing)
--   oeisIx = maximumBy (comparing show) . (rowT @27750)

-- instance OEIS 122426 where
--   oeis = [x | x <- [1..], (oeisIx @122425) x < x]

-- instance OEIS 122427 where
--   oeisIx n = (oeis @122426) !! (n - 1)
--   oeis = [x | x <- [1..], (oeisIx @122425) x == x]

-- instance OEIS 122428 where
--   oeis = [x | x <- [1..], (oeisIx @122425) x == (oeisIx @6530) x]

-- instance OEIS 122494 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @122494) !! (n - 1)
--   oeis = f (singleton (4, 2)) 27 [3..] where
--      f s uu us@ (u:us'@ (u':_))
--        | vv > uu = uu : f (insert (uu * u, u) s) (u' ^ u') us'
--        | vv < uu = vv : f (insert (vv * v, v) s') uu us
--        | otherwise = vv : f (insert (vv * v, v) s') (u' ^ u') us'
--        where ((vv, v), s') = deleteFindMin s

-- instance OEIS 122516 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @46992)

-- instance OEIS 122535 where
--   oeisIx = (oeisIx @40) . (oeisIx @64113)

-- instance OEIS 122542 where
--   oeis = tablList @122542
-- instance Table 122542 where
--   tabl = map fst $ iterate
--      (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
--                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [0, 1])

-- instance OEIS 122631 where
--   oeis =
--      1 : 2 : map (oeisIx @6530) (zipWith (+) (map ((2 *) . (oeisIx @40)) (oeis @122631))
--                                       (map (oeisIx @40) (tail (oeis @122631))))

-- instance OEIS 122768 where
--   oeis = 0 : f (tail (oeis @41)) [1] where
--      f (p:ps) rs = (sum $ zipWith (*) rs $ tail (oeis @41)) : f ps (p : rs)

-- instance OEIS 122797 where
--   oeis  = 1 : zipWith (+) (oeis @122797) (map ((1 -) . (oeisIx @10054)) [1..])

-- instance OEIS 122840 where
--   oeisIx n = if n < 10 then 0 ^ n else 0 ^ d * (oeisIx n' + 1)
--               where (n', d) = divMod n 10

-- instance OEIS 122953 where
--   oeisIx = genericLength . (rowT @165416)

-- instance OEIS 122972 where
--   oeis = 1 : 2 : zipWith (+)
--      (zipWith (*) [2..] (oeis @122972)) (zipWith (*) [1..] $ tail (oeis @122972))

-- instance OEIS 123087 where
--   oeis = scanl (+) 0 (oeis @96268)

-- instance OEIS 123125 where
--   oeis = tablList @123125
-- instance Table 123125 where
--   tabl = [1] : zipWith (:) [0, 0 ..] (tabl @8292)

-- instance OEIS 123270 where
--   oeis = 1 : 1 : zipWith (-) (map (* 5) $
--      zipWith (+) (tail (oeis @123270)) (oeis @123270)) (oeis @123270)

-- instance OEIS 123345 where
--   oeis = filter
--     (\x -> all (`isInfixOf` b x) $ map b $ (rowT @27750) x) [1..] where
--     b = unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

-- instance OEIS 123346 where
--   oeis = tablList @123346
-- instance Table 123346 where
--   tabl = map reverse (tabl @11971)

-- instance OEIS 123581 where
--   oeis = iterate (oeisIx @70229) 3

-- instance OEIS 123921 where
--   oeis = filter ((== 1) . (oeisIx @10051)) $
--      map (flip (-) 2) $ zipWith (*) (oeis @40) (tail (oeis @40))

-- instance OEIS 123976 where
--   oeis = map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @45) [1..]

-- instance OEIS 124056 where
--   oeis = 1 : f [1] where
--      f xs@ (x:_) = y : f (y : xs) where
--        y = length $ filter (flip elem $ (rowT @27750) x) xs

-- instance OEIS 124108 where
--   oeisIx 0 = 0
--   oeisIx x = 2 * (b + 1) * (oeisIx @124108) x' + (b * 2)
--               where (x', b) = divMod x 2

-- instance OEIS 124134 where
--   oeis = filter ((> 0) . (oeisIx @161) . (oeisIx @45)) [1..]

-- instance OEIS 124240 where
--   oeis = filter
--      (\x -> all (== 0) $ map ((mod x) . pred) $ (rowT @27748) x) [1..]

-- instance OEIS 124665 where
--   oeis = filter
--      (\x -> all (== 0) $ map (oeisIx . (10*x +)) [1..9]) (oeis @65502)

-- instance OEIS 124837 where
--   oeisIx n = (oeisIx @213998) (n + 2) (n - 1)

-- instance OEIS 124838 where
--   oeisIx n = (oeisIx @213999) (n + 2) (n - 1)

-- instance OEIS 124844 where
--   oeis = tablList @124844
-- instance Table 124844 where
--   tabl = zipWith (zipWith (*))
--                          (tabl @7318) $ tail $ inits (oeis @61084)

-- instance OEIS 124927 where
--   oeis = tablList @124927
-- instance Table 124927 where
--   tabl = iterate
--      (\row -> zipWith (+) ([0] ++ reverse row) (row ++ [1])) [1]

-- instance OEIS 124934 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @125203)

-- instance OEIS 124978 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (tail (oeis @2635)))

-- instance OEIS 125002 where
--   oeisIx n = sum $ map (oeisIx' . read) $
--                     tail $ concatMap (f pds) [0 .. length pds - 1] where
--      pds = show $ (oeisIx @40) n
--      f ws k = [us ++ [y] ++ vs |
--               let (us, v:vs) = splitAt k ws, y <- delete v "0123456789"]

-- instance OEIS 125022 where
--   oeis = elemIndices 1 (oeis @161)

-- instance OEIS 125053 where
--   oeis = tablList @125053
-- instance Table 125053 where
--   tabf = iterate f [1] where
--   f zs = zs' ++ reverse (init zs') where
--   zs' = (sum zs) : g (map (* 2) zs) (sum zs)
--   g [x] y = [x + y]
--   g xs y = y' : g (tail $ init xs) y' where y' = sum xs + y

-- instance OEIS 125086 where
--   oeis = f [0, 2 ..] (oeis @36990) where
--      f (u:us) vs'@ (v:vs) = if u == v then f us vs else u : f us vs'

-- instance OEIS 125145 where
--   oeis =
--      1 : 4 : map (* 3) (zipWith (+) (oeis @125145) (tail (oeis @125145)))

-- instance OEIS 125217 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @125203)

-- instance OEIS 125218 where
--   oeis = map (+ 1) $ findIndices (> 1) (oeis @125203)

-- instance OEIS 125290 where
--   oeis = filter ((> 1) . (oeisIx @43537)) (oeis @52382)

-- instance OEIS 125308 where
--   oeis = 3 : h [1,3] where
--      h (u:us) | null (show v `intersect` "245679") &&
--                 (oeisIx @10051)' v == 1 = v : h (us ++ [v])
--               | otherwise       = h (us ++ [v])
--               where v = u + 10

-- instance OEIS 125605 where
--   oeis = tablList @125605
-- instance Table 125605 where
--   tabl = iterate f [1] where
--      f xs = zipWith (\v w -> (v + w) `div` gcd v w) ([0] ++ xs) (xs ++ [0])

-- instance OEIS 125639 where
--   oeis = filter f [1..] where
--      f x = sx > x && (oeisIx @1065) sx > sx where sx = (oeisIx @1065) x

-- instance OEIS 125640 where
--   oeis = f (oeis @125639) [] where
--      f (x:xs) ys = if null (oeisIx_row' x `intersect` ys)
--                       then x : f xs (x : ys) else f xs ys

-- instance OEIS 125717 where
--   import Data.IntMap (singleton, member, (!), insert)
--   oeisIx n = (oeis @125717) !! n
--   oeis =  0 : f [1..] 0 (singleton 0 0) where
--      f (v:vs) w m = g (reverse[w-v,w-2*v..1] ++ [w+v,w+2*v..]) where
--        g (x:xs) = if x `member` m then g xs else x : f vs x (insert x v m)

-- instance OEIS 125855 where
--   oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051)') $
--      iterate (zipWith (+) [1, 1, 1, 1]) [1, 3, 7, 9]

-- instance OEIS 125886 where
--   oeis = 1 : f 1 [10..] where
--      f u vs = g vs where
--        g (w:ws) = if (oeisIx @10879) w == iu then w : f w (delete w vs) else g ws
--        iu = (oeisIx @30) u

-- instance OEIS 125887 where
--   oeis = 1 : f '1' (filter ((> 0) . (`mod` 10)) [11..]) where
--      f d zs = y : f (last $ show y) (xs ++ ys) where
--          (xs, y:ys) = span ((/= d) . head . show) zs

-- instance OEIS 126024 where
--   oeisIx = genericLength . filter ((== 1) . (oeisIx @10052) . sum) .
--                             subsequences . enumFromTo 1

-- instance OEIS 126027 where
--   oeisIx = genericLength . (rowT @30717)

-- instance OEIS 126064 where
--   oeis = tablList @126064
-- instance Table 126064 where
--   tabl =  zipWith (zipWith (*)) (tabl @94587) (tabl @59268)

-- instance OEIS 126256 where
--   -- import Data.List.Ordered (insertSet)
--   oeisIx n = (oeis @126256) !! n
--   oeis = f (tabl @7318) [] where
--      f (xs:xss) zs = g xs zs where
--        g []     ys = length ys : f xss ys
--        g (x:xs) ys = g xs (insertSet x ys)

-- instance OEIS 126257 where
--   import Data.List.Ordered (minus, union)
--   oeisIx n = (oeis @126257) !! n
--   oeis = f [] (tabf @34868) where
--      f zs (xs:xss) = (length ys) : f (ys `union` zs) xss
--                      where ys = xs `minus` zs

-- instance OEIS 126596 where
--   oeisIx n = (oeisIx @5810) n * (oeisIx @5408) n `div` (oeisIx @16777) n

-- instance OEIS 126684 where
--   oeis = tail $ m (oeis @695) $ map (* 2) (oeis @695) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y     = x : m xs ys'
--                              | otherwise = y : m xs' ys

-- instance OEIS 126759 where
--   oeis = 1 : f 1 where
--      f n = (case mod n 6 of 1 -> 2 * div n 6 + 2
--                             5 -> 2 * div n 6 + 3
--                             3 -> (oeisIx @126759) $ div n 3
--                             _ -> (oeisIx @126759) $ div n 2) : f (n + 1)

-- instance OEIS 126768 where
--   oeis = map length $ group (oeis @117872)

-- instance OEIS 126869 where
--   oeisIx n = (rowT @204293) (2*n) !! n

-- instance OEIS 126890 where
--   oeis = tablList @126890
-- instance Table 126890 where
--   tabl = map fst $ iterate
--      (\ (xs@ (x:_), i) -> (zipWith (+) ((x-i):xs) [2*i+1 ..], i+1)) ([0], 0)

-- instance OEIS 126949 where
--   oeis = filter h [1..] where
--      h m = not $ null [ (x, e) | x <- [2 .. m - 2], gcd x m == 1,
--                                 e <- [2 .. (oeisIx @10) m `div` 2],
--                                 x ^ e `mod` m == m - 1]

-- instance OEIS 126988 where
--   oeis = tablList @126988
-- instance Table 126988 where
--   rowCol = rowCol_off @126988 @1 @1
--   rowT   = rowT_off   @126988 @1
--   tabl = zipWith (zipWith (*)) (tabl @10766) (tabl @51731)

-- instance OEIS 127013 where
--   oeis = tablList @127013
-- instance Table 127013 where
--   rowCol = rowCol_off @127013 @1 @1
--   rowT   = rowT_off   @127013 @1
--   tabl = map reverse (tabl @126988)

-- instance OEIS 127057 where
--   oeis = tablList @127057
-- instance Table 127057 where
--   rowCol = rowCol_off @127057 @1 @1
--   rowT   = rowT_off   @127057 @1
--   tabl = map (scanr1 (+)) (tabl @126988)

-- instance OEIS 127093 where
--   oeis = tablList @127093
-- instance Table 127093 where
--   rowCol n k = (rowT @127093) n !! (k-1)
--   rowT n = zipWith (*) [1..n] $ map ((0 ^) . (mod n)) [1..n]
--   tabl = map (rowT @127093) [1..]

-- instance OEIS 127330 where
--   oeis = tablList @127330
-- instance Table 127330 where
--   tabl = step 0 1 where
--      step s k = [s .. s + k - 1] : step (2 * s + 2) (k + 1)

-- instance OEIS 127354 where
--   oeisIx = (oeisIx @47842) . (oeisIx @40)

-- instance OEIS 127355 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @47842)) (oeis @40)

-- instance OEIS 127366 where
--   oeisIx n | even n'   = n'
--             | otherwise = 2*n - n'
--             where n' = n + (oeisIx @196) n

-- instance OEIS 127367 where
--   oeisIx n | even n    = n - m + 1
--             | otherwise = n + m
--             where m = length $ takeWhile (<= n) (oeis @2378)

-- instance OEIS 127423 where
--   oeis = 1 : map read (zipWith (++) (tail iss) iss) :: [Integer]
--                      where iss = map show [1..]

-- instance OEIS 127446 where
--   oeis = tablList @127446
-- instance Table 127446 where
--   rowCol = rowCol_off @127446 @1 @1
--   rowT   = rowT_off   @127446 @1
--   tabl = zipWith (\v ws -> map (* v) ws) [1..] (tabl @51731)

-- instance OEIS 127542 where
--   oeisIx = genericLength . filter ((== 1) . (oeisIx @10051) . sum) .
--                             subsequences . enumFromTo 1

-- instance OEIS 127626 where
--   oeis = tablList @127626
-- instance Table 127626 where
--   rowCol = rowCol_off @127626 @1 @1
--   rowT   = rowT_off   @127626 @1
--   tabl = map
--      (map (\x -> if x == 0 then 0 else (oeisIx @18804) x)) (tabl @127093)

-- instance OEIS 127648 where
--   oeis = tablList @127648
-- instance Table 127648 where
--   tabl = map reverse $ iterate (\ (x:xs) -> x + 1 : 0 : xs) [1]
--   oeis = concat (tabl @127648)

-- instance OEIS 127739 where
--   oeis = tablList @127739
-- instance Table 127739 where
--   rowCol = rowCol_off @127739 @1 @1
--   rowT   = rowT_off   @127739 @1
--   tabl = zipWith ($) (map replicate [1..]) $ tail (oeis @217)

-- instance OEIS 127812 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @72594))

-- instance OEIS 127824 where
--   oeis = tablList @127824
-- instance Table 127824 where
--   tabf = iterate f [1] where
--      f row = sort $ map (* 2) row `union`
--                     [x' | x <- row, let x' = (x - 1) `div` 3,
--                           x' * 3 == x - 1, odd x', x' > 1]

-- instance OEIS 127899 where
--   oeis = tablList @127899
-- instance Table 127899 where
--   rowCol = rowCol_off @127899 @1 @1
--   rowT   = rowT_off   @127899 @1
--   tabl = map reverse ([1] : xss) where
--      xss = iterate (\ (u : v : ws) -> u + 1 : v - 1 : ws ++ [0]) [2, -2]

-- instance OEIS 127936 where
--   oeis = findIndices ((== 1) . (oeisIx @10051)'') (oeis @7583)

-- instance OEIS 128014 where
--   oeisIx = (oeisIx @984) . flip div 2

-- instance OEIS 128059 where
--   oeisIx 0 = 1
--   oeisIx n = f n n where
--      f 1 _ = 1
--      f x q = if (oeisIx @10051)' q' == 1 then q' else f x' q'
--              where x' = x - 1; q' = q + x'

-- instance OEIS 128174 where
--   oeis = tablList @128174
-- instance Table 128174 where
--   rowCol = rowCol_off @128174 @1 @1
--   rowT   = rowT_off   @128174 @1
--   tabl = iterate (\xs@ (x:_) -> (1 - x) : xs) [1]

-- instance OEIS 128217 where
--   oeis = filter f [0..] where
--      f x = 4 * abs (root - fi (round root)) < 1
--            where root = sqrt $ fi x

-- instance OEIS 128218 where
--   oeis = zipWith (-) (tail (oeis @128217)) (oeis @128217)

-- instance OEIS 128543 where
--   oeisIx = sum . (rowT @134239) . subtract 1

-- instance OEIS 128630 where
--   oeis = map (minimum . map (sum . map (gpfs !!))) $ tail pss where
--      pss = [] : map parts [1..] :: [[[Int]]] where
--            parts u = [u] : [v : ps | v <- [1..u],
--                                      ps <- pss !! (u - v), v < head ps]
--      gpfs = map fromInteger (0 : map (oeisIx @6530) [1..])

-- instance OEIS 128783 where
--   oeis = filter (f . show . (^ 2)) [1..] where
--      f zs = g (init $ tail $ inits zs) (tail $ init $ tails zs)
--      g (xs:xss) (ys:yss)
--        | (oeisIx @10052) (read xs) == 1 = (oeisIx @10052) (read ys) == 1 || f ys || g xss yss
--        | otherwise              = g xss yss
--      g _ _ = False

-- instance OEIS 128924 where
--   oeis = tablList @128924
-- instance Table 128924 where
--   rowCol = rowCol_off @128924 @1 @1
--   tabl = map (rowT @128924) [1..]
--   rowT 1 = [1]
--   rowT n = f [0..n - 1] $ group $ sort $ g 1 ps where
--      f []     _                            = []
--      f (v:vs) wss'@ (ws:wss) | head ws == v = length ws : f vs wss
--                             | otherwise    = 0 : f vs wss'
--      g 0 (1 : xs) = []
--      g _ (x : xs) = x : g x xs
--      ps = 1 : 1 : zipWith (\u v -> (u + v) `mod` n) (tail ps) ps

-- instance OEIS 128966 where
--   oeis = tablList @128966
-- instance Table 128966 where
--   tabl = map fst $ iterate
--      (\ (us, vs) -> (vs, zipWith (+) ([0] ++ us ++ [0]) $
--                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([0], [1, 1])

-- instance OEIS 129117 where
--   oeis = elemIndices 5 (oeisIx @101403)M_list

-- instance OEIS 129150 where
--   oeis = iterate (oeisIx @3415) 8

-- instance OEIS 129151 where
--   oeis = iterate (oeisIx @3415) 81

-- instance OEIS 129152 where
--   oeis = iterate (oeisIx @3415) 15625

-- instance OEIS 129283 where
--   oeisIx n = (oeisIx @3415) n + n

-- instance OEIS 129284 where
--   oeisIx 0 = 2
--   oeisIx n = (oeisIx @129151) n `div` 27

-- instance OEIS 129286 where
--   oeis = iterate (oeisIx @129283) 5

-- instance OEIS 129299 where
--   oeis = 1 : f [1] 2 where
--      f xs@ (x:_) k = y : f (y:xs) (k+1) where
--        y = x + sum [z | z <- xs, z <= k]

-- instance OEIS 129300 where
--   oeis = 1 : f [1] 1 where
--      f xs@ (x:_) k = y : f (y:xs) (k+1) where
--        y = x + sum [z | z <- xs, z <= k]

-- instance OEIS 129363 where
--   oeisIx n = sum $ map (oeisIx . (2*n -)) $ takeWhile (<= n) (oeis @1097)

-- instance OEIS 129505 where
--   oeisIx n = abs $ (oeisIx @8275) (2 * n - 1) n

-- instance OEIS 129511 where
--   import Data.List.Ordered (isect, union)
--   oeisIx n = (oeis @129511) !! (n - 1)
--   oeis = filter (f [] . (rowT @27750)') [1..] where
--      f _ [_] = True
--      f zs (d:ds) = null (dds `isect` zs) && f (dds `union` zs) ds
--                    where dds = map (subtract d) ds

-- instance OEIS 129512 where
--   import Data.List.Ordered (minus)
--   oeisIx n = (oeis @129512) !! (n - 1)
--   oeis = minus [1..] (oeis @129511)

-- instance OEIS 129521 where
--   oeisIx n = p * (2 * p - 1) where p = (oeisIx @5382) n

-- instance OEIS 129713 where
--   oeis = tablList @129713
-- instance Table 129713 where
--   tabl = [1] : [1, 1] : f [1] [1, 1] where
--      f us vs = ws : f vs ws where
--                ws = zipWith (+) (init us ++ [0, 0, 0]) (vs ++ [1])

-- instance OEIS 129800 where
--   oeis = filter ((== 1) . length . f) (oeis @40) where
--     f x = filter (\ (us, vs) ->
--                  (oeisIx @10051)' (read us :: Integer) == 1 &&
--                  (oeisIx @10051)' (read vs :: Integer) == 1) $
--                  map (flip splitAt $ show x) [1 .. length (show x) - 1]

-- instance OEIS 129805 where
--   oeis = [x | x <- (oeis @56020), (oeisIx @10051) x == 1]

-- instance OEIS 129845 where
--   oeis =
--      filter (\x -> not $ null (show (2*x) `intersect` show x)) [1..]

-- instance OEIS 129871 where
--   oeis = 1 : (oeis @58)

-- instance OEIS 129893 where
--   oeis = 1 : zipWith div (tail fs) fs where
--      fs = map (oeisIx @142) (oeis @124)

-- instance OEIS 130321 where
--   oeis = tablList @130321
-- instance Table 130321 where
--   tabl = iterate (\row -> (2 * head row) : row) [1]

-- instance OEIS 130330 where
--   oeis = tablList @130330
-- instance Table 130330 where
--   rowCol n k = (rowT @130330) n !! (k-1)
--   rowT   = rowT_off   @130330 @1
--   tabl = iterate (\xs -> (2 * head xs + 1) : xs) [1]

-- instance OEIS 130517 where
--   oeis = tablList @130517
-- instance Table 130517 where
--   rowCol = rowCol_off @130517 @1 @1
--   rowT   = rowT_off   @130517 @1
--   tabl = iterate (\row -> (head row + 1) : reverse row) [1]

-- instance OEIS 130534 where
--   oeis = tablList @130534
-- instance Table 130534 where
--   tabl = map (map abs) (tabl @8275)

-- instance OEIS 130578 where
--   oeis = 0 : 0 : 1 : 3 : zipWith (+)
--      (map (* 2) $ drop 3 (oeis @130578))
--      (zipWith (-) (map (+ 1) (oeis @130578)) (drop 2 (oeis @130578)))

-- instance OEIS 130595 where
--   oeis = concat $ iterate ([-1,1] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 130667 where
--   oeis = 1 : (concat $ transpose
--      [zipWith (+) vs (oeis @130667), zipWith (+) vs $ tail (oeis @130667)])
--      where vs = map (* 5) (oeis @130667)

-- instance OEIS 130883 where
--   oeisIx = (oeisIx @128918) . (* 2)

-- instance OEIS 130887 where
--   oeisIx = sum . map (oeisIx @225) . (rowT @27750)

-- instance OEIS 130897 where
--   oeis = filter
--      (any (== 0) . map (oeisIx . fi) . (rowT @124010)) [1..]

-- instance OEIS 131073 where
--   oeis = 2 : f 2 1 where
--      f x c = y : f y (c + (oeisIx @10051) y) where y = x + c

-- instance OEIS 131094 where
--   oeis = tablList @131094
-- instance Table 131094 where
--   rowCol = rowCol_off @131094 @1 @1
--   rowT   = rowT_off   @131094 @1
--   tabl = [2] : f 2 [2] where
--      f v ws = ys : f (v + 1) ys where
--               ys = take v $ nub $ sort $ concatMap h ws
--      h z = [2 * z, 4 * z + 1, 4 * z' + b] where (z', b) = divMod z 2

-- instance OEIS 131095 where
--   oeis = tablList @131095
-- instance Table 131095 where
--   rowCol = rowCol_off @131095 @1 @1
--   rowT   = rowT_off   @131095 @1
--   tabl = [2] : [4, 9] : [17, 18, 20] : f 4 [17, 18, 20] where
--      f v ws = ys : f (v + 1) ys where
--        ys = take v $ dropWhile (<= last ws) $ nub $ sort $ concatMap h ws
--      h z = [2 * z, 4 * z + 1, 4 * z' + b] where (z', b) = divMod z 2

-- instance OEIS 131293 where
--   oeis = 0 : 1 : map read
--                  (zipWith ((++) `on` show) (oeis @131293) $ tail (oeis @131293))

-- instance OEIS 131361 where
--   oeisIx n = p [r | r <- tail (oeis @10785), head (show r) `elem` show n] n
--      where p _          0 = 1
--            p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 131364 where
--   oeisIx n = p [r | r <- tail (oeis @10785), head (show r) `elem` show n] n
--      where p _      0 = 1
--            p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 131365 where
--   oeis = elemIndices 0 (oeis @131364)

-- instance OEIS 131366 where
--   oeis = tail $ findIndices (> 0) (oeis @131364)

-- instance OEIS 131410 where
--   oeis = tablList @131410
-- instance Table 131410 where
--   rowCol = rowCol_off @131410 @1 @1
--   rowT   = rowT_off   @131410 @1
--   tabl = zipWith replicate [1..] $ tail (oeis @45)

-- instance OEIS 131507 where
--   oeis = tablList @131507
-- instance Table 131507 where
--   tabl = zipWith ($) (map replicate [1..]) [1, 3 ..]
--   oeis = concat (tabl @131507)

-- instance OEIS 131577 where
--   oeisIx = (`div` 2) . (oeisIx @79)
--   oeis = 0 : (oeis @79)

-- instance OEIS 131644 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = (oeis @131644) !! (n - 1)
--   oeis = map fst $ iterate f (0, 2) where
--      f (v, w) = (powerMod 2 v w, w + 1)

-- instance OEIS 131816 where
--   oeis = tablList @131816
-- instance Table 131816 where
--   tabl = map (map (subtract 1)) $
--      zipWith (zipWith (+)) (tabl @130321) (tabl @59268)

-- instance OEIS 132080 where
--   oeis = [x | x <- [2..], all null $
--                       map (show x `intersect`) $ map show $ (rowT @27751) x]

-- instance OEIS 132090 where
--   oeisIx = (oeisIx @720) . (oeisIx @720)

-- instance OEIS 132157 where
--   oeis = (map length) (group (oeis @63882))

-- instance OEIS 132159 where
--   oeis = tablList @132159
-- instance Table 132159 where
--   tabl = map reverse (tabl @121757)

-- instance OEIS 132163 where
--   oeisIx_tabl = map (rowT @132163) [1..]
--   oeisIx n k = (rowT @132163) n !! (k-1)
--   oeisIx_row n = 1 : f 1 [n, n - 1 .. 2] where
--      f u vs = g vs where
--        g []                            = []
--        g (x:xs) | (oeisIx @10051) (x + u) == 1 = x : f x (delete x vs)
--                 | otherwise            = g xs

-- instance OEIS 132188 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @132345) n + (oeisIx $ fromInteger n)

-- instance OEIS 132199 where
--   oeis = zipWith (-) (tail (oeis @106108)) (oeis @106108)

-- instance OEIS 132213 where
--   oeisIx n = sum $ map (oeisIx @10051)' $
--               nub $ genericTake n $ map (`mod` n) $ tail (oeis @290)

-- instance OEIS 132231 where
--   oeis = [x | k <- [0..], let x = 30 * k + 7, (oeisIx @10051)' x == 1]

-- instance OEIS 132240 where
--   oeis = [x | x <- (oeis @175887), (oeisIx @10051) x == 1]

-- instance OEIS 132345 where
--   oeisIx n = sum $ zipWith (*)
--      (tail (oeis @10)) (map ((div n) . (^ 2)) [2..oeisIx n])

-- instance OEIS 132350 where
--   oeisIx 1 = 1
--   oeisIx n = 1 - (oeisIx @75802) n

-- instance OEIS 132393 where
--   oeis = tablList @132393
-- instance Table 132393 where
--   tabl = map (map abs) (tabl @48994)

-- instance OEIS 132431 where
--   oeisIx n = (oeisIx @60226) n - (oeisIx @62119) n + (oeisIx @2378) (n - 1)

-- instance OEIS 132678 where
--   oeis = elemIndices 1 (oeis @96535)

-- instance OEIS 132679 where
--   import Data.Set (fromList, insert, deleteFindMin)
--   oeisIx n = (oeis @132679) !! (n - 1)
--   oeis = f $ fromList [1,2] where
--      f s = m : f (insert (4*m) $ insert (4*m+3) s') where
--          (m,s') = deleteFindMin s

-- instance OEIS 132739 where
--   oeisIx n | r > 0     = n
--             | otherwise = (oeisIx @132739) n' where (n',r) = divMod n 5

-- instance OEIS 132813 where
--   oeis = tablList @132813
-- instance Table 132813 where
--   tabl = zipWith (zipWith (*)) (tabl @7318) $ tail (tabl @7318)

-- instance OEIS 132995 where
--   oeis = tail $ f (oeis @40) 0 1 where
--      f (p:ps) u v = (gcd u v) : f ps (p + u) (p * v)

-- instance OEIS 133008 where
--   oeisIx n = genericLength [x | x <- takeWhile (< n) (oeis @28),
--                       n `mod` x == 0, let y = n `div` x, x < y,
--                       y `elem` takeWhile (<= n) (oeis @28)]

-- instance OEIS 133042 where
--   oeisIx = (^ 3) . (oeisIx @41)

-- instance OEIS 133280 where
--   oeis = tablList @133280
-- instance Table 133280 where
--   tabl = f 0 1 [0..] where
--      f m j xs = (filter ((== m) . (`mod` 2)) ys) : f (1 - m) (j + 2) xs'
--        where (ys,xs') = splitAt j xs

-- instance OEIS 133457 where
--   oeis = tablList @133457
-- instance Table 133457 where
--   rowCol n k = (tabf @133457) !! (n - 1) !! n
--   rowT   = rowT_off @133457 @1
--   tabf = map (fst . unzip . filter ((> 0) . snd) . zip [0..]) $
--                      tail (tabf @30308)

-- instance OEIS 133466 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @57918)

-- instance OEIS 133610 where
--   oeis = scanl1 (+) (oeis @53616)

-- instance OEIS 133808 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @133808) !! (n - 1)
--   oeis = 1 : f (singleton (2, 2, 1)) where
--      f s = y : f (insert (y * p, p, e + 1) $ insert (y * q^e, q, e) s')
--                where q = (oeisIx @151800) p
--                      ((y, p, e), s') = deleteFindMin s

-- instance OEIS 133809 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @133809) !! (n - 1)
--   oeis = 1 : f (singleton (2, 2, 1)) where
--      f s = y : f (insert (y*p, p, e+1) $ insert (y*q^ (e+1), q, e+1) s')
--                where q = (oeisIx @151800) p
--                      ((y, p, e), s') = deleteFindMin s

-- instance OEIS 133810 where
--   oeis = 1 : filter f [2..] where
--      f x = (and $ zipWith (<=) eps $ tail eps) &&
--            (all (== 1) $ zipWith (-) (tail ips) ips)
--        where ips = map (oeisIx @49084) $ (rowT @27748) x
--              eps = (rowT @124010) x

-- instance OEIS 133811 where
--   oeis = 1 : filter f [2..] where
--      f x = (and $ zipWith (<) eps $ tail eps) &&
--            (all (== 1) $ zipWith (-) (tail ips) ips)
--        where ips = map (oeisIx @49084) $ (rowT @27748) x
--              eps = (rowT @124010) x

-- instance OEIS 133813 where
--   oeis = 1 : filter f [2..] where
--      f x = isPrefixOf ps (dropWhile (< (oeisIx @20639) x) (oeis @40)) &&
--              all (< 0) (zipWith (-) (tail es) es)
--            where ps = (rowT @27748) x; es = (rowT @124010) x

-- instance OEIS 133820 where
--   oeis = tablList @133820
-- instance Table 133820 where
--   rowCol = rowCol_off @133820 @1 @1
--   rowT   = rowT_off   @133820 @1
--   tabl = map (`take` (tail (oeis @578))) [1..]

-- instance OEIS 133821 where
--   oeis = tablList @133821
-- instance Table 133821 where
--   rowCol = rowCol_off @133821 @1 @1
--   rowT   = rowT_off   @133821 @1
--   tabl = map (`take` (tail (oeis @583))) [1..]

-- instance OEIS 133870 where
--   oeis = filter ((== 1) . (oeisIx @10051)) [1,33..]

-- instance OEIS 134204 where
--   oeis = 2 : f 1 2 (tail (oeis @40)) where
--      f x q ps = p' : f (x + 1) p' (delete p' ps) where
--        p' = head [p | p <- ps, mod (p + q) x == 0]

-- instance OEIS 134239 where
--   oeis = tablList @134239
-- instance Table 134239 where
--   tabl = [1] : zipWith (map . (*))
--                  [2..] (map reverse $ tail (tabl @29635))

-- instance OEIS 134287 where
--   oeisIx = flip (oeisIx @103371) 4 . (+ 4)

-- instance OEIS 134451 where
--   oeisIx = until (< 3) (oeisIx @53735)

-- instance OEIS 134636 where
--   oeis = tablList @134636
-- instance Table 134636 where
--   tabl = iterate (\row -> zipWith (+) ([2] ++ row) (row ++ [2])) [1]

-- instance OEIS 134640 where
--   oeis = tablList @134640
-- instance Table 134640 where
--   rowCol = rowCol_off @134640 @1 @1
--   rowT n = sort $
--      map (foldr (\dig val -> val * n + dig) 0) $ permutations [0 .. n - 1]
--   tabf = map (rowT @134640) [1..]
--   oeis = concat (tabf @134640)

-- instance OEIS 134941 where
--   oeis = elemIndices 1 (oeis @178333)

-- instance OEIS 134948 where
--   oeis = filter h [0..] where
--      h x = all (`isInfixOf` xs)
--                (map (fss !!) $ map (read . return) $ sort $ nub xs)
--            where xs = show x
--      fss = map show $ take 10 (oeis @142)

-- instance OEIS 135093 where
--   oeisIx 0 = 4
--   oeisIx n = (+ 1) $ fromJust $ (`elemIndex` (oeis @46665)) $ (oeisIx @30173) n

-- instance OEIS 135141 where
--   oeis = 1 : map f [2..] where
--      f x | iprime == 0 = 2 * (oeisIx $ (oeisIx @66246) x) + 1
--          | otherwise   = 2 * (oeisIx iprime)
--          where iprime = (oeisIx @49084) x

-- instance OEIS 135282 where
--   oeisIx = (oeisIx @7814) . head . filter ((== 1) . (oeisIx @209229)) . (rowT @70165)

-- instance OEIS 135414 where
--   oeis = 1 : 1 : zipWith (-) [3..] (map (oeisIx @135414) (oeis @135414))

-- instance OEIS 135440 where
--   oeis = zipWith (-) (tail (oeis @14551)) (oeis @14551)

-- instance OEIS 135499 where
--   oeis = filter ((== 0) . (oeisIx @225693)) [1..]

-- instance OEIS 135504 where
--   oeis = 1 : zipWith (+)
--                      (oeis @135504) (zipWith lcm (oeis @135504) [2..])

-- instance OEIS 135507 where
--   oeis = 1 : zipWith (+)
--      (map (* 2) $ (oeis @135507)) (zipWith lcm (oeis @135507) [2..])

-- instance OEIS 135528 where
--   oeis = concat $ iterate ([1,0] *) [1]
--   instance Num a => Num [a] where
--   fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (0:ps) * qs         = 0 : ps * qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 135581 where
--   oeisIx n = [d | d <- [1..], (oeisIx @137488) n `mod` d == 0] !! 4

-- instance OEIS 135643 where
--   oeis = filter f [100..] where
--      f x = all (== 0) ws where
--            ws = zipWith (-) (tail vs) vs
--            vs = zipWith (-) (tail us) us
--            us = map (read . return) $ show x

-- instance OEIS 135837 where
--   oeis = tablList @135837
-- instance Table 135837 where
--   rowCol = rowCol_off @135837 @1 @1
--   rowT   = rowT_off   @135837 @1
--   tabl = [1] : [1, 2] : f [1] [1, 2] where
--      f xs ys = ys' : f ys ys' where
--        ys' = zipWith3 (\u v w -> 2 * u - v + 2 * w)
--                       (ys ++ [0]) (xs ++ [0, 0]) ([0, 0] ++ xs)

-- instance OEIS 136119 where
--   oeis = f [1..] where
--      f zs@ (y:xs) = y : f (delete (zs !! y) xs)

-- instance OEIS 136183 where
--   oeisIx n = sum $ zipWith lcm ps $ tail ps where ps = (rowT @27750) n

-- instance OEIS 136400 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @136400) n' * 10 + min 1 d where (n',d) = divMod n 10

-- instance OEIS 136414 where
--   oeis = zipWith (+) (tail (oeis @7376)) $ map (10 *) (oeis @7376)

-- instance OEIS 136431 where
--   oeis = tablList @136431
-- instance Table 136431 where
--   tabl = map fst $ iterate h ([0], 1) where
--      h (row, fib) = (zipWith (+) ([0] ++ row) (row ++ [fib]), last row)

-- instance OEIS 136446 where
--   oeis = map (+ 1) $ findIndices (> 1) (oeis @211111)

-- instance OEIS 136447 where
--   oeis = filter ((== 0) . (oeisIx @210455)) [1..]

-- instance OEIS 136480 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @7814) $ n + mod n 2

-- instance OEIS 136495 where
--   oeisIx n = (fromJust $ n `elemIndex` tail (oeis @5374)) + 1

-- instance OEIS 136572 where
--   oeis = tablList @136572
-- instance Table 136572 where
--   tabl = map fst $ iterate f ([1], 1) where
--      f (row, i) = (0 : map (* i) row, i + 1)

-- instance OEIS 136655 where
--   oeisIx = product . (rowT @182469)

-- instance OEIS 136798 where
--   oeis = tail $ map (+ 1) $ elemIndices 1 $
--      zipWith (*) (0 : (oeis @10051)) $ map (1 -) $ tail (oeis @10051)

-- instance OEIS 137291 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @49001)) [1..]

-- instance OEIS 137409 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @24362)

-- instance OEIS 137488 where
--   oeis = m (map (^ 24) (oeis @40)) (map (^ 4) (oeis @6881)) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y = x : m xs ys'
--                              | otherwise = y : m xs' ys

-- instance OEIS 137580 where
--   oeisIx = genericLength . nub . show . (oeisIx @142)

-- instance OEIS 137613 where
--   oeis =  filter (> 1) (oeis @132199)

-- instance OEIS 137948 where
--   oeis = tablList @137948
-- instance Table 137948 where
--   tabl = zipWith (zipWith div) (tabl @245334) (tabl @7318)

-- instance OEIS 138109 where
--   oeis = filter f [1..] where
--      f x = p ^ 2 < x && x < p ^ 3 where p = (oeisIx @20639) x

-- instance OEIS 138290 where
--   oeis = map (+ 1) $ tail $ elemIndices 0 (oeis @208083)

-- instance OEIS 138353 where
--   oeis = filter ((== 1) . (oeisIx @10051)') $ map (+ 9) (oeis @290)

-- instance OEIS 138510 where
--   oeis = mapMaybe f [1..] where
--     f x | (oeisIx @10051)' q == 0 = Nothing
--         | q == p          = Just 1
--         | otherwise       = Just $
--           head [b | b <- [2..], length (d b p) == length (d b q)]
--         where q = div x p; p = (oeisIx @20639) x
--     d b = unfoldr (\z -> if z == 0 then Nothing else Just $ swap $ divMod z b)

-- instance OEIS 138511 where
--   oeis = filter f [1..] where
--                         f x = p ^ 2 < q && (oeisIx @10051)' q == 1
--                               where q = div x p; p = (oeisIx @20639) x

-- instance OEIS 138530 where
--   oeis = tablList @138530
-- instance Table 138530 where
--   rowCol = rowCol_off @138530 @1 @1
--   rowT   = rowT_off   @138530 @1
--   tabl = zipWith (map . flip q) [1..] (tabl @2260) where
--      q 1 n = n
--      q b n = if n < b then n else q b n' + d where (n', d) = divMod n b

-- instance OEIS 138666 where
--   oeis = map (head . tail) $
--      filter (all (== 0) . map (oeisIx @10051) . tail) $ drop 2 (tabl @87401)

-- instance OEIS 139127 where
--   oeisIx 1 = 1
--   oeisIx n = head [y | let z = (oeisIx @5244) n + 1,
--               y <- reverse $ takeWhile (<= z `div` (oeisIx @20639) z) (oeis @5244),
--               z `mod` y == 0]

-- instance OEIS 139337 where
--   oeisIx n = read $ concatMap show $ mapMaybe (flip lookup ls) ds :: Int
--      where ls = zip (map head zss) (map length zss)
--            zss = group $ sort ds
--            ds = map (read . return) $ show n :: [Int]

-- instance OEIS 139366 where
--   oeisIx 1 1               = 0
--   oeisIx n k | gcd n k > 1 = 0
--               | otherwise   = head [r | r <- [1..], k ^ r `mod` n == 1]
--   oeisIx_row n = map (oeisIx n) [1..n]
--   oeisIx_tabl = map (rowT @139366) [1..]

-- instance OEIS 139399 where
--   oeisIx = f 0 where
--      f k x = if x `elem` [1,2,4] then k else f (k + 1) (oeisIx x)

-- instance OEIS 139532 where
--   oeis = [x | x <- [0..], (oeisIx @10051)' (24 * x + 19) == 1]

-- instance OEIS 139544 where
--   oeis = 1 : 2 : 4 : tail (oeis @16825)

-- instance OEIS 139555 where
--   oeisIx = sum . map (oeisIx @10055) . (rowT @38566)

-- instance OEIS 139764 where
--   oeisIx = head . (rowT @35517)

-- instance OEIS 140119 where
--   oeisIx = sum . (rowT @95195)

-- instance OEIS 140253 where
--   oeis = -1 : concat
--                       (transpose [oeis, map (* 2) (oeis @83420)])

-- instance OEIS 140434 where
--   oeis = 1 : zipWith (-) (tail (oeis @18805)) (oeis @18805)

-- instance OEIS 140470 where
--   oeis = filter
--      (\x -> all (== 0) $ map ((mod x) . (+ 1)) $ (rowT @27748) x) [1..]

-- instance OEIS 140472 where
--   oeis = 0 : 1 : h 2 1 where
--     h x y = z : h (x + 1) z where z = (oeisIx @140472) (x - y) + (oeisIx @140472) (x `div` 2)

-- instance OEIS 140480 where
--   oeis = filter
--       ((== 1) . (oeisIx @10052) . (\x -> (oeisIx @1157) x `div` (oeisIx @5) x)) (oeis @20486)

-- instance OEIS 140513 where
--   oeis = tablList @140513
--   rowCol = rowCol_off @140513 @1 @1
--   rowT   = rowT_off   @140513 @1
--   tabl = iterate (\xs@ (x:_) -> map (* 2) (x:xs)) [2]
--   oeis = concat (tabl @140513)

-- instance OEIS 140690 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @140690) !! (n - 1)
--   oeis = f $ singleton (1, 1, 2) where
--      f s | k == 1 = m : f (insert (2*b-1, 1, 2*b) $ insert (b*m, k+1, b) s')
--          | even k    = m : f (insert (b*m+b-1, k+1, b) s')
--          | otherwise = m : f (insert (b*m, k+1, b) s')
--          where ((m, k, b), s') = deleteFindMin s

-- instance OEIS 140750 where
--   oeis = tablList @140750
-- instance Table 140750 where
--   rowCol = rowCol_off @140750 @1 @1
--   rowT   = rowT_off @140750 @1
--   tabf = [1] : [1,1,1] : f [1] [1,1,1] where
--      f ws vs = vs' : f vs vs' where
--        vs' = zipWith3 (\r s x -> r + s + x)
--              (vs ++ [0,0]) ([0,0] ++ ws ++ [0,0]) ([0,0] ++ vs)

-- instance OEIS 141036 where
--   oeis = 2 : 1 : 1 : zipWith3 (((+) .) . (+))
--      (oeis @141036) (tail (oeis @141036)) (drop 2 (oeis @141036))

-- instance OEIS 141092 where
--   oeis = catMaybes $ zipWith div' (oeis @36691) (oeis @53767) where
--      div' x y | m == 0    = Just x'
--               | otherwise = Nothing where (x',m) = divMod x y

-- instance OEIS 141164 where
--   oeis = map succ $ elemIndices 1 $ map (oeisIx @188172) [1..]

-- instance OEIS 141169 where
--   oeis = tablList @141169
-- instance Table 141169 where
--   tabl = tail $ inits (oeis @45)
--   oeis = concat $ (tabl @141169)

-- instance OEIS 141197 where
--   oeisIx = sum . map (oeisIx . (+ 1)) . (rowT @27750)

-- instance OEIS 141258 where
--   oeisIx = sum . map (oeisIx @2322) . (rowT @27750)

-- instance OEIS 141686 where
--   oeis = tablList @141686
-- instance Table 141686 where
--   rowCol = rowCol_off @141686 @1 @1
--   rowT   = rowT_off   @141686 @1
--   tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @8292)

-- instance OEIS 141707 where
--   oeisIx n = head [k | k <- [1, 3 ..], (oeisIx @178225) (k * (2 * n - 1)) == 1]

-- instance OEIS 141708 where
--   oeisIx n = (oeisIx @141707) n * (2 * n - 1)

-- instance OEIS 141709 where
--   oeisIx n = until ((== 1) . (oeisIx @178225) . (oeisIx @265)) (+ n) n

-- instance OEIS 141755 where
--   oeis = filter f (oeis @1358) where
--      f x = (oeisIx @10052)' (spf + x `div` spf) == 1 where spf = (oeisIx @20639) x

-- instance OEIS 141766 where
--   oeis = filter f [1..] where
--      f x = all (== 0) $ map (mod x) $ (map pred ps) ++ (map succ ps)
--            where ps = (rowT @27748) x

-- instance OEIS 141767 where
--   oeis = filter f [1..] where
--      f x = all (== 0) $
--            map (mod x) $ zipWith (*) (map pred ps) (map succ ps)
--            where ps = (rowT @27748) x

-- instance OEIS 142149 where
--   oeisIx :: Integer -> Integer
--   oeisIx = foldl xor 0 . zipWith (.|.) [0..] . reverse . enumFromTo 1

-- instance OEIS 142151 where
--   oeisIx :: Integer -> Integer
--   oeisIx = foldl (.|.) 0 . zipWith xor [0..] . reverse . enumFromTo 1

-- instance OEIS 142925 where
--   oeis = filter ((== 1) . (oeisIx @10051)) [1,65..]

-- instance OEIS 142978 where
--   oeis = tablList @142978
-- instance Table 142978 where
--   rowCol = rowCol_off @142978 @1 @1
--   rowT   = rowT_off   @142978 @1
--   tabl = map reverse (tabl @104698)

-- instance OEIS 142983 where
--   oeis = 1 : 2 : zipWith (+)
--                          (map (* 2) $ tail (oeis @142983))
--                          (zipWith (*) (drop 2 (oeis @2378)) (oeis @142983))

-- instance OEIS 142984 where
--   oeis = 1 : 4 : zipWith (+)
--                          (map (* 4) $ tail (oeis @142984))
--                          (zipWith (*) (drop 2 (oeis @2378)) (oeis @142984))

-- instance OEIS 142985 where
--   oeis = 1 : 6 : zipWith (+)
--                          (map (* 6) $ tail (oeis @142985))
--                          (zipWith (*) (drop 2 (oeis @2378)) (oeis @142985))

-- instance OEIS 142986 where
--   oeis = 1 : 8 : zipWith (+)
--                          (map (* 8) $ tail (oeis @142986))
--                          (zipWith (*) (drop 2 (oeis @2378)) (oeis @142986))

-- instance OEIS 142987 where
--   oeis = 1 : 10 : zipWith (+)
--                           (map (* 10) $ tail (oeis @142987))
--                           (zipWith (*) (drop 2 (oeis @2378)) (oeis @142987))

-- instance OEIS 143070 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @23416)) [1..]

-- instance OEIS 143071 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @120)) [1..]

-- instance OEIS 143072 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @23416)) (oeis @143071)

-- instance OEIS 143201 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (+ 1) $ zipWith (-) (tail pfs) pfs
--      where pfs = (rowT @27748) n

-- instance OEIS 143202 where
--   oeis = filter (\x -> (oeisIx @6530) x - (oeisIx @20639) x == 2) [1,3..]

-- instance OEIS 143203 where
--   oeis = filter f [1,3..] where
--      f x = length pfs == 2 && last pfs - head pfs == 4 where
--          pfs = (rowT @27748) x

-- instance OEIS 143205 where
--   oeis = filter f [1,3..] where
--      f x = length pfs == 2 && last pfs - head pfs == 6 where
--          pfs = (rowT @27748) x

-- instance OEIS 143206 where
--   oeis = (3*7) : f (oeis @40) where
--      f (p:ps@ (p':_)) | p'-p == 4 = (p*p') : f ps
--                      | otherwise = f ps

-- instance OEIS 143207 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @143207) !! (n - 1)
--   oeis = f (singleton (2*3*5)) where
--      f s = m : f (insert (2*m) $ insert (3*m) $ insert (5*m) s') where
--        (m,s') = deleteFindMin s

-- instance OEIS 143215 where
--   oeisIx n = (oeisIx @40) n * (oeisIx @7504) n

-- instance OEIS 143333 where
--   oeis = tablList @143333
-- instance Table 143333 where
--   rowCol = rowCol_off @143333 @1 @1
--   rowT   = rowT_off   @143333 @1
--   tabl = zipWith (zipWith (*)) (tabl @7318) (tabl @47999)

-- instance OEIS 143344 where
--   oeis = zipWith (-) (tail (oeis @22941)) (oeis @22941)

-- instance OEIS 143473 where
--   oeisIx n = foldl (\v d -> 10 * v + d) 0 $ (10 - z) : zs where
--      (z:zs) = map (read . return) $ show n

-- instance OEIS 143520 where
--   oeisIx n = product $ zipWith (\p e -> (e + 2 * mod p 2 - 1) * p ^ e)
--                                 (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 143536 where
--   oeis = tablList @143536
-- instance Table 143536 where
--   rowCol = rowCol_off @143536 @1 @1
--   rowT   = rowT_off   @143536 @1
--   tabl = zipWith take [1..] $ map repeat (oeis @10051)

-- instance OEIS 143667 where
--   oeis = f (oeis @3849) where
--      f (0:0:ws) = 0 : f ws; f (0:1:ws) = 1 : f ws; f (1:0:ws) = 2 : f ws

-- instance OEIS 143683 where
--   oeis = tablList @143683
-- instance Table 143683 where
--   tabl = map fst $ iterate
--      (\ (us, vs) -> (vs, zipWith (+) (map (* 8) ([0] ++ us ++ [0])) $
--                         zipWith (+) ([0] ++ vs) (vs ++ [0]))) ([1], [1, 1])

-- instance OEIS 143691 where
--   oeis = f 1 [1..] where
--      f m xs = g xs where
--        g (z:zs) = if m + m' /= 1 then g zs else z : f m' (delete z xs)
--                   where m' = (oeisIx @1222) z `mod` 2

-- instance OEIS 143692 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @143691))

-- instance OEIS 143792 where
--   oeisIx n = genericLength $ (rowT @225243) n `intersect` (rowT @27748) (fi n)

-- instance OEIS 143967 where
--   oeisIx = f 0 . (+ 1) where
--      f y 1 = (oeisIx @4086) y
--      f y x = f (10 * y + 3 + 4 * r) x' where (x', r) = divMod x 2

-- instance OEIS 144100 where
--   oeis = filter (\x -> (oeisIx @144907) x < x) [1..]

-- instance OEIS 144299 where
--   oeis = tablList @144299
-- instance Table 144299 where
--   tabl = [1] : [1, 0] : f 1 [1] [1, 0] where
--      f i us vs = ws : f (i + 1) vs ws where
--                  ws = (zipWith (+) (0 : map (i *) us) vs) ++ [0]

-- instance OEIS 144328 where
--   oeis = tablList @144328
-- instance Table 144328 where
--   rowCol = rowCol_off @144328 @1 @1
--   rowT   = rowT_off   @144328 @1
--   tabl = [1] : map (\xs@ (x:_) -> x : xs) (tabl @2260)

-- instance OEIS 144331 where
--   oeis = tablList @144331
-- instance Table 144331 where
--   tabf = iterate (\xs ->
--     zipWith (+) ([0] ++ xs ++ [0]) $ zipWith (*) (0:[0..]) ([0,0] ++ xs)) [1]

-- instance OEIS 144394 where
--   oeis = tablList @144394
-- instance Table 144394 where
--   rowCol = rowCol_off @144394 @4 @0
--   rowT = rowT_off @144394 @4
--   tabl = map (drop 2 . reverse . drop 2) $ drop 4 (tabl @7318)

-- instance OEIS 144582 where
--   oeis = [x | x <- [0..], (oeisIx @30) x == (oeisIx @30) (x ^ 3)]

-- instance OEIS 144623 where
--   oeisIx = (subtract 1) . (oeisIx @78822)

-- instance OEIS 144624 where
--   oeisIx n = (oeisIx @78823) n - fi n

-- instance OEIS 144907 where
--   oeisIx x | (oeisIx @10051) x == 1 = 1
--             | x `mod` 4 == 0 = 2 * rad
--             | otherwise      = rad  where rad = (oeisIx @7947) x

-- instance OEIS 144925 where
--   oeisIx = genericLength . (rowT @163870)

-- instance OEIS 145037 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @145037) n' + 2*m - 1 where (n', m) = divMod n 2

-- instance OEIS 145108 where
--   oeis = filter ((== 0) . (`mod` 4)) (oeis @133809)

-- instance OEIS 145292 where
--   oeis = filter ((== 0) . (oeisIx @10051)) (oeis @202018)

-- instance OEIS 145445 where
--   oeis = f (oeis @40) $ drop 2 (oeis @290) where
--      f ps'@ (p:ps) xs'@ (x:xs) = if p < x then x : f ps xs' else f ps' xs

-- instance OEIS 145784 where
--   oeis = filter ((== 0) . (oeisIx @10872) . (oeisIx @1222)) [1..]

-- instance OEIS 145799 where
--   oeisIx = maximum . map (foldr (\b v -> 2 * v + b) 0) .
--                       filter (\bs -> bs == reverse bs && head bs == 1) .
--                       substr . bin where
--      substr [] = []
--      substr us'@ (_:us) = sub us' ++ substr us where
--         sub [] = []; sub (v:vs) = [v] : [v : ws | ws <- sub vs ]
--      bin 0 = []; bin n = b : bin n' where (n', b) = divMod n 2

-- instance OEIS 145829 where
--   oeis = map (oeisIx @196) $ filter ((== 1) . (oeisIx @10052)) $ tail (oeis @145768)

-- instance OEIS 146288 where
--   oeisIx = (oeisIx @5) . (oeisIx @25487)

-- instance OEIS 146562 where
--   oeis = map (+ 1) $
--      findIndices (/= 0) $ zipWith (-) (oeis @64353) $ cycle [1, 3]

-- instance OEIS 147812 where
--   oeis = map (oeisIx . (+ 1)) $ findIndices (< 0) (oeis @36263)

-- instance OEIS 147813 where
--   oeis = map (oeisIx . (+ 1)) $ findIndices (>= 0) (oeis @36263)

-- instance OEIS 147991 where
--   import Data.Set (singleton, insert, deleteFindMin)
--   oeisIx n = (oeis @147991) !! (n - 1)
--   oeis = f $ singleton 1 where
--      f s = m : (f $ insert (3*m - 1) $ insert (3*m + 1) s')
--            where (m, s') = deleteFindMin s

-- instance OEIS 151666 where
--   oeisIx n = fromEnum (n < 2 || m < 2 && (oeisIx @151666) n' == 1)
--      where (n', m) = divMod n 4

-- instance OEIS 151764 where
--   oeisIx = (oeisIx @71786) . (oeisIx @71786)

-- instance OEIS 151765 where
--   oeisIx = (oeisIx @71786) . (oeisIx @4086)

-- instance OEIS 151799 where
--   oeisIx = (oeisIx @7917) . (subtract 1)

-- instance OEIS 151821 where
--   oeis = x : xs where (x : _ : xs) = (oeis @79)

-- instance OEIS 151910 where
--   oeis = zipWith (-) (tail (oeis @1682)) (oeis @1682)

-- instance OEIS 151945 where
--   oeis = 1 : 1 : f [2..] where
--      f (x:xs) = p (take x (oeis @151945)) x : f xs
--      p _ 0 = 1; p [] _ = 0
--      p ds'@ (d:ds) m = if m < d then 0 else p ds' (m - d) + p ds m

-- instance OEIS 151949 where
--   oeisIx n = (oeisIx @4186) n - (oeisIx @4185) n

-- instance OEIS 152223 where
--   oeis = 1 : -6 : zipWith (-)
--      (map (* 6) $ (oeis @152223)) (map (* 4) $ tail (oeis @152223))

-- instance OEIS 152271 where
--   oeisIx = (oeisIx @57979) . (+ 2)
--   oeis = concat $ transpose [repeat 1, [1..]]

-- instance OEIS 152458 where
--   oeis = [x | x <- [1..], (oeisIx @64413) x == x]

-- instance OEIS 152723 where
--   oeisIx n = min (oeisIx n) (oeisIx n)

-- instance OEIS 152724 where
--   oeisIx n = max (oeisIx n) (oeisIx n)

-- instance OEIS 152842 where
--   oeis = tablList @152842
-- instance Table 152842 where
--   tabl = map fst $ iterate f ([1], 3) where
--      f (xs, z) = (zipWith (+) ([0] ++ map (* z) xs) (xs ++ [0]), 4 - z)

-- instance OEIS 153733 where
--   oeisIx n = if b == 0 then n else (oeisIx @153733) n'  where (n', b) = divMod n 2

-- instance OEIS 153860 where
--   oeis = tablList @153860
-- instance Table 153860 where
--   rowCol = rowCol_off @153860 @1 @1
--   rowT   = rowT_off   @153860 @1
--   tabl = [1] : [0, 1] : iterate (\ (x:xs) -> -x : 0 : xs) [1, 1, 1]

-- instance OEIS 154314 where
--   oeis = findIndices (/= 3) (oeis @212193)

-- instance OEIS 154530 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @248378)

-- instance OEIS 154691 where
--   oeis = 1 : zipWith (+)
--                      (oeis @154691) (drop 2 $ map (* 2) (oeis @45))

-- instance OEIS 154771 where
--   oeisIx = sum . (rowT @218978) :: Integer -> Integer

-- instance OEIS 154809 where
--   oeis = filter ((== 0) . (oeisIx @178225)) [0..]

-- instance OEIS 155038 where
--   oeis = tablList @155038
-- instance Table 155038 where
--   rowCol = rowCol_off @155038 @1 @1
--   rowT   = rowT_off   @155038 @1
--   tabl = iterate
--      (\row -> zipWith (+) (row ++ [0]) (init row ++ [0,1])) [1]

-- instance OEIS 155043 where
--   oeis = 0 : map ((+ 1) . (oeisIx @155043)) (oeis @49820)

-- instance OEIS 155046 where
--   oeis = concat $ transpose [tail (oeis @1333), tail (oeis @129)]

-- instance OEIS 155161 where
--   oeis = tablList @155161
-- instance Table 155161 where
--   tabl = [1] : [0,1] : f [0] [0,1] where
--      f us vs = ws : f vs ws where
--        ws = zipWith (+) (us ++ [0,0]) $ zipWith (+) ([0] ++ vs) (vs ++ [0])

-- instance OEIS 156031 where
--   oeis = tail $ concat (transpose [oeis, (oeis @143344)])

-- instance OEIS 156144 where
--   oeisIx n = p [x | x <- [1..n], (oeisIx @10888) x == (oeisIx @10888) n] n where
--      p _  0 = 1
--      p [] _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 156596 where
--   oeisIx n = (oeis @143667) !! (n - 1)
--   oeis = f (oeis @3849) where
--      f (0:0:ws) = 0 : f ws; f (0:1:ws) = 1 : f ws; f (1:0:ws) = 2 : f ws

-- instance OEIS 156659 where
--   oeisIx n = fromEnum $ (oeisIx @10051) n == 1 && (oeisIx @10051) (n `div` 2) == 1

-- instance OEIS 156660 where
--   oeisIx n = fromEnum $ (oeisIx @10051) n == 1 && (oeisIx @10051) (2 * n + 1) == 1

-- instance OEIS 156678 where
--   oeis = f 1 1 where
--      f u v | v > uu `div` 2        = f (u + 1) (u + 2)
--            | gcd u v > 1 || w == 0 = f u (v + 2)
--            | otherwise             = v : f u (v + 2)
--            where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

-- instance OEIS 156679 where
--   oeis = f 1 1 where
--      f u v | v > uu `div` 2        = f (u + 1) (u + 2)
--            | gcd u v > 1 || w == 0 = f u (v + 2)
--            | otherwise             = w : f u (v + 2)
--            where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

-- instance OEIS 156689 where
--   oeis = f 1 1 where
--      f u v | v > uu `div` 2        = f (u + 1) (u + 2)
--            | gcd u v > 1 || w == 0 = f u (v + 2)
--            | otherwise             = (u + v - w) `div` 2 : f u (v + 2)
--            where uu = u ^ 2; w = (oeisIx @37213) (uu + v ^ 2)

-- instance OEIS 157037 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @3415)) [1..]

-- instance OEIS 157104 where
--   oeisIx = (oeisIx @3415) . (oeisIx @4709)

-- instance OEIS 157454 where
--   oeis = tablList @157454
-- instance Table 157454 where
--   rowCol = rowCol_off @157454 @1 @1
--   rowT   = rowT_off   @157454 @1
--   tabl = concatMap h $ tail $ inits [1, 3 ..] where
--      h xs = [xs ++ tail xs', xs ++ xs'] where xs' = reverse xs

-- instance OEIS 157725 where
--   oeisIx = (+ 2) . (oeisIx @45)
--   oeis = 2 : 3 : map (subtract 2)
--                          (zipWith (+) (oeis @157725) $ tail (oeis @157725))

-- instance OEIS 157726 where
--   oeisIx = (+ 3) . (oeisIx @45)
--   oeis = 3 : 4 : map (subtract 3)
--                          (zipWith (+) (oeis @157726) $ tail (oeis @157726))

-- instance OEIS 157727 where
--   oeisIx = (+ 4) . (oeisIx @45)
--   oeis = 4 : 5 : map (subtract 4)
--                          (zipWith (+) (oeis @157727) $ tail (oeis @157727))

-- instance OEIS 157728 where
--   oeisIx = subtract 4 . (oeisIx @45)

-- instance OEIS 157729 where
--   oeisIx = (+ 5) . (oeisIx @45)
--   oeis = 5 : 6 : map (subtract 5)
--                          (zipWith (+) (oeis @157729) $ tail (oeis @157729))

-- instance OEIS 157793 where
--   oeis = f [head (oeis @23416)] $ tail (oeis @23416) where
--      f zs (x:xs) = (sum $ zipWith (*) zs (oeis @120)) : f (x:zs) xs

-- instance OEIS 157931 where
--   oeis = filter ((== 1) . (oeisIx @64911)) (oeis @14091)

-- instance OEIS 157996 where
--   oeis = map (+ 1) $ filter f (oeis @6093) where
--      f x = g $ takeWhile (< x) (oeis @65091) where
--        g []  = False
--        g [_] = False
--        g (p:ps@ (_:qs)) = (x - p) `elem` qs || g ps

-- instance OEIS 158036 where
--   oeisIx = (\x -> (4^x - 2^x + 8*x^2 - 2) `div` (2*x* (2*x + 1))) . (oeisIx @158034)

-- instance OEIS 158294 where
--   oeisIx n = (oeisIx $ (oeisIx @20486) n) `div` (oeisIx $ (oeisIx @20486) n)

-- instance OEIS 158405 where
--   oeis = tablList @158405
-- instance Table 158405 where
--   rowCol n k = (rowT @158405) n !! (k-1)
--   rowT   = rowT_off   @158405 @1
--   tabl = map reverse (tabl @99375)

-- instance OEIS 159051 where
--   oeis = map (+ 2) $ elemIndices 0 $ zipWith mod (oeis @45) [2..]

-- instance OEIS 159611 where
--   oeis = map (+ 2) $ elemIndices 0 (oeis @98006)

-- instance OEIS 159700 where
--   oeisIx n = genericLength $ filter (\ (p, q) -> p < q - 2 && (oeisIx @164292) q == 1) $
--                               zip ps (map (2 * n -) ps)
--                        where ps = filter ((== 1) . (oeisIx @164292)) [1..n]

-- instance OEIS 159765 where
--   oeis = (rowT @27750) 1000000

-- instance OEIS 159780 where
--   oeisIx n = sum $ zipWith (*) bs $ reverse bs
--      where bs = (rowT @30308) n

-- instance OEIS 160113 where
--   oeisIx = (oeisIx @60431) . (2 ^)

-- instance OEIS 160180 where
--   oeisIx = (oeisIx @32742) . (oeisIx @2808)

-- instance OEIS 160516 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75075))

-- instance OEIS 160588 where
--   oeis = concat $ transpose [oeis @53645, oeis @27]

-- instance OEIS 160638 where
--   import Data.Word (Word8)
--   oeisIx :: Word8 -> Word8
--   oeisIx n = rev 0 0 where
--      rev 8 y = y
--      rev i y = rev (i + 1) (if testBit n i then setBit y (7 - i) else y)

-- instance OEIS 160676 where
--   oeis = filter (\x -> (oeisIx @6968) x == (oeisIx @6968) (2 * x)) [1..]

-- instance OEIS 160700 where
--   oeis = [0..15] ++ map f [16..] where
--      f x = (oeisIx @160700) x' `xor` m :: Int where (x', m) = divMod x 16

-- instance OEIS 160855 where
--   oeis = 1 : f 2 1 [2..] where
--      f x sum zs = g zs where
--        g (y:ys) = if binSub x (sum + y)
--                      then y : f (x + 1) (sum + y) (delete y zs) else g ys
--      binSub u = sub where
--         sub w = mod w m == u || w > u && sub (div w 2)
--         m = (oeisIx @62383) u

-- instance OEIS 160967 where
--   oeis = m (oeis @79) (oeis @2450) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y     = x : m xs ys'
--                              | x == y    = x : m xs ys
--                              | otherwise = y : m xs' ys

-- instance OEIS 161187 where
--   oeis = tail $ findIndices ((== 1) . (oeisIx @10052)) (oeis @89237)

-- instance OEIS 161188 where
--   oeis = map (+ 1) $ findIndices ((== 1) . (oeisIx @10051)) $ tail (oeis @89237)

-- instance OEIS 161385 where
--   oeisIx = (+ 1) . (oeisIx @161382)

-- instance OEIS 161390 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @161390) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [0, 5, 4, 2, 9, 8, 6, 7, 3, 1]

-- instance OEIS 161466 where
--   oeis = (rowT @27750) $ (oeisIx @142) 10

-- instance OEIS 161597 where
--   oeis = filter (\x -> (oeisIx @161594) x == x) [1..]

-- instance OEIS 161598 where
--   oeis = filter (\x -> (oeisIx @161594) x /= x) [1..]

-- instance OEIS 161600 where
--   oeis = filter ((== 0) . (oeisIx @10051)) (oeis @161597)

-- instance OEIS 161764 where
--   oeisIx n = n - (oeisIx @199238) n

-- instance OEIS 161906 where
--   oeis = tablList @161906
-- instance Table 161906 where
--   rowCol = rowCol_off @161906 @1 @1
--   rowT   = rowT_off @161906 @1
--   tabf = zipWith (\m ds -> takeWhile ((<= m) . (^ 2)) ds)
--                          [1..] (oeisIx @27750)_tabf'

-- instance OEIS 161908 where
--   oeis = tablList @161908
-- instance Table 161908 where
--   rowCol = rowCol_off @161908 @1 @1
--   rowT   = rowT_off @161908 @1
--   tabf = zipWith
--                  (\x ds -> reverse $ map (div x) ds) [1..] (tabf @161906)

-- instance OEIS 162247 where
--   import Data.Ord (comparing)
--   oeisIx n k = (tabl @162247) !! (n - 1) !! (k-1)
--   oeisIx_row n = (tabl @162247) !! (n - 1)
--   oeisIx_tabl = map (concat . sortBy (comparing length)) $ tail fss where
--      fss = [] : map fact [1..] where
--            fact x = [x] : [d : fs | d <- [2..x], let (x',r) = divMod x d,
--                                     r == 0, fs <- fss !! x', d <= head fs]

-- instance OEIS 162551 where
--   oeisIx n = (oeisIx @51601) (2 * n) n

-- instance OEIS 162608 where
--   oeis = tablList @162608
-- instance Table 162608 where
--   tabl = map fst $ iterate f ([1], 1) where
--      f (row, n) = (row' ++ [head row' + last row'], n + 1) where
--        row' = map (* n) row

-- instance OEIS 162610 where
--   oeis = tablList @162610
-- instance Table 162610 where
--   rowCol n k = k * n - k + n
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @162610) [1..]

-- instance OEIS 162643 where
--   oeis = filter ((== 0) . (oeisIx @209229) . (oeisIx @5)) [1..]

-- instance OEIS 162711 where
--   oeis = tablList @162711
-- instance Table 162711 where
--   rowCol = rowCol_off @162711 @1 @1
--   rowT   = rowT_off   @162711 @1
--   tabl = map (map (read . concatMap show) . tail . inits) $
--                  zipWith take [1..] $ tails (oeis @7376) :: [[Integer]]

-- instance OEIS 162741 where
--   oeis = tablList @162741
-- instance Table 162741 where
--   rowCol = rowCol_off @162741 @1 @1
--   rowT   = rowT_off @162741 @1
--   tabf = iterate
--      (\row -> zipWith (+) ([0] ++ row ++ [0]) (row ++ [0,1])) [1]

-- instance OEIS 162909 where
--   import Ratio
--   bird :: [Rational]
--   bird = branch (recip . succ) (succ . recip) 1
--   branch f g a = a : branch f g (f a) \/ branch f g (g a)
--   (a : as) \/ bs = a : (bs \/ as)
--   oeisIx = map numerator bird
--   oeisIx = map denominator bird

-- instance OEIS 162910 where
--   import Ratio; bird :: [Rational]; bird = branch (recip . succ) (succ . recip) 1; branch f g a = a : branch f g (f a) \/ branch f g (g a); (a : as) \/ bs = a : (bs \/ as); (oeisIx @162909) = map numerator bird; (oeisIx @162910) = map denominator bird

-- instance OEIS 162995 where
--   oeis = tablList @162995
-- instance Table 162995 where
--   rowCol = rowCol_off @162995 @1 @1
--   rowT   = rowT_off   @162995 @1
--   tabl = map fst $ iterate f ([1], 3)
--      where f (row, i) = (map (* i) row ++ [1], i + 1)

-- instance OEIS 163271 where
--   oeisIx = sum . (rowT @128966) . (subtract 1)

-- instance OEIS 163753 where
--   oeis = filter ((> 0) . (oeisIx @39997)) [0..]

-- instance OEIS 163870 where
--   oeis = tablList @163870
-- instance Table 163870 where
--   rowCol = rowCol_off @163870 @1 @1
--   rowT   = rowT_off @163870 @1
--   tabf = filter (not . null) $ map tail (tabf @27751)

-- instance OEIS 163925 where
--   oeis = tablList @163925
-- instance Table 163925 where
--   rowCol = rowCol_off @163925 @1 @1
--   tabf = map (rowT @163925) [1..]
--   rowT n = [k | k <- takeWhile (<= n ^ 2) (oeis @18252),
--                        let k' = k * n, let divs = (rowT @27750) k',
--                        last (takeWhile ((<= k') . (^ 2)) divs) == n]

-- instance OEIS 163926 where
--   oeisIx = genericLength . (rowT @163925)

-- instance OEIS 163974 where
--   oeisIx n = f (oeis @40) 1 nn 0 where
--      f (p:ps) l nl xx
--        | yy > nl   = 0
--        | yy < nl   = f ps (l + 1) (nl + nn) yy + f ps l nl xx
--        | otherwise = if w == n then 1 else 0
--        where w = if r == 0 then (oeisIx @196) m else 0
--              (m, r) = divMod yy l
--              yy = xx + p * p
--      nn = n ^ 2

-- instance OEIS 164055 where
--   oeis = 1 : 10 : 325 : zipWith (+) (oeis @164055)
--      (map (* 35) $ tail $ zipWith (-) (tail (oeis @164055)) (oeis @164055))

-- instance OEIS 164283 where
--   oeisIx n = f [1..] 1 nn 0 where
--      f (k:ks) l nl xx
--        | yy > nl  = 0
--        | yy < nl  = f ks (l + 1) (nl + nn) yy + f ks l nl xx
--        | otherwise = if w == n then 1 else 0
--        where w = if r == 0 then (oeisIx @196) m else 0
--              (m, r) = divMod yy l
--              yy = xx + k * k
--      nn = n ^ 2

-- instance OEIS 164292 where
--   oeisIx 1 = 0
--   oeisIx 2 = 0
--   oeisIx n = signum (oeisIx' n * (oeisIx' (n - 2) + (oeisIx @10051)' (n + 2)))

-- instance OEIS 164296 where
--   oeisIx n = genericLength [m | let ts = (rowT @38566) n, m <- ts,
--                           all ((== 1) . gcd m) (ts \\ [m])]

-- instance OEIS 164297 where
--   oeisIx n = genericLength [m | let ts = (rowT @38566) n, m <- ts,
--                           any ((> 1) . gcd m) (ts \\ [m])]

-- instance OEIS 164338 where
--   oeis = iterate (oeisIx @36839) 12334444

-- instance OEIS 164349 where
--   oeisIx n = if n == 0 then 0 else until (<= 1) (oeisIx . subtract 1) n

-- instance OEIS 164555 where
--   oeis = 1 : map (numerator . sum) (zipWith (zipWith (%))
--      (zipWith (map . (*)) (tail (oeis @142)) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 164652 where
--   oeis = tablList @164652
-- instance Table 164652 where
--   tabl = [0] : tail (zipWith (zipWith (*)) (tabl @128174) $
--      zipWith (map . flip div) (tail (oeis @217)) (map init $ tail (tabl @130534)))

-- instance OEIS 164861 where
--   oeis = filter ((== 0) . (oeisIx @178225)) (oeis @5408)

-- instance OEIS 164874 where
--   oeis = tablList @164874
-- instance Table 164874 where
--   rowCol = rowCol_off @164874 @1 @1
--   rowT   = rowT_off   @164874 @1
--   tabl = map reverse $ iterate f [2] where
--      f xs@ (x:_) = (2 * x + 2) : map ((+ 1) . (* 2)) xs

-- instance OEIS 165153 where
--   oeisIx = product . (rowT @165416)

-- instance OEIS 165157 where
--   oeis = scanl (+) 0 (oeis @133622)

-- instance OEIS 165413 where
--   oeisIx = genericLength . nub . map length . group . (rowT @30308)

-- instance OEIS 165416 where
--   oeis = tablList @165416
-- instance Table 165416 where
--   rowCol = rowCol_off @165416 @1 @1
--   rowT   = rowT_off @165416 @1
--   tabf = map (dropWhile (== 0)) $ tail (tabf @119709)

-- instance OEIS 165430 where
--   oeis = tablList @165430
-- instance Table 165430 where
--   rowCol n k = last (oeisIx_row n `intersect` (rowT @77610) k)
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @165430) [1..]

-- instance OEIS 165476 where
--   oeisIx = flip legendreSymbol 131071

-- instance OEIS 165634 where
--   oeis = concatMap (\x ->
--      if (oeisIx @10051) x == 1 then [oeisIx x, x] else [x]) [1..]

-- instance OEIS 165712 where
--   oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1222) x == (oeisIx @1222) n]

-- instance OEIS 165713 where
--   oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1221) x == (oeisIx @1221) n]

-- instance OEIS 165909 where
--   oeisIx n = sum $ nub $ map (`mod` n) $
--                           take (fromInteger n) $ tail (oeis @290)

-- instance OEIS 165930 where
--   oeis = 1 : zipWith (-) (tail (oeis @64491)) (oeis @64491)

-- instance OEIS 166133 where
--   oeis = 1 : 2 : 4 : f (3:[5..]) 4 where
--      f zs x = y : f (delete y zs) y where
--               y = head $ O.isect (rowT @27750 (x ^ 2 - 1)) zs

-- instance OEIS 166238 where
--   oeisIx = a' 0 where a' n = n : takeWhile (< (n - 2)) (oeisIx @166238) ++ a' (n + 1)

-- instance OEIS 166251 where
--   oeis = concat $ (filter ((== 1) . length)) $
--      map (filter ((== 1) . (oeisIx @10051))) $
--      zipWith enumFromTo (oeis @100484) (tail (oeis @100484))

-- instance OEIS 166350 where
--   oeis = tablList @166350
-- instance Table 166350 where
--   rowCol = rowCol_off @166350 @1 @1
--   rowT   = rowT_off   @166350 @1
--   tabl = tail $ inits $ tail (oeis @142)

-- instance OEIS 166360 where
--   oeis = tablList @166360
-- instance Table 166360 where
--   rowCol = rowCol_off @166360 @1 @1
--   rowT   = rowT_off   @166360 @1
--   tabl = map (map (flip mod 2)) (tabl @1263)

-- instance OEIS 166474 where
--   oeis = 1 : 2 : zipWith (+)
--      (tail (oeis @166474)) (zipWith (*) (oeis @166474) $ drop 2 (oeis @217))

-- instance OEIS 166573 where
--   oeis = filter (("13" `isInfixOf`) . show) (oeis @40)

-- instance OEIS 166920 where
--   oeis = scanl (+) 0 (oeis @14551)

-- instance OEIS 167008 where
--   oeisIx = sum . (rowT @219206)

-- instance OEIS 167151 where
--   oeis = 0 : concat (transpose [oeis, (oeis @30124)])

-- instance OEIS 167376 where
--   import Data.List.Ordered (minus)
--   oeisIx n = (oeis @167376) !! n
--   oeis = minus [0..] (oeis @41)

-- instance OEIS 167377 where
--   import Data.List.Ordered (minus)
--   oeisIx n = (oeis @167377) !! n
--   oeis = minus [0..] (oeis @9)

-- instance OEIS 167392 where
--   import Data.List.Ordered (member)
--   oeisIx = fromEnum . flip member (oeis @41)

-- instance OEIS 167393 where
--   import Data.List.Ordered (member)
--   oeisIx = fromEnum . flip member (oeis @9)

-- instance OEIS 167489 where
--   oeisIx = product . map length . group . (rowT @30308)

-- instance OEIS 167535 where
--   oeis = filter ((> 0) . (oeisIx @193095)) (oeis @40)

-- instance OEIS 167700 where
--   oeisIx = p (oeis @16754) where
--      p _  0 = 1
--      p (q:qs) m = if m < q then 0 else p qs (m - q) + p qs m

-- instance OEIS 167772 where
--   oeis = tablList @167772
-- instance Table 167772 where
--   rowCol n k = genericIndex (oeisIx_row n) k
--   rowT n = genericIndex (tabl @167772) n
--   tabl = [1] : [0, 1] :
--                  map (\xs@ (_:x:_) -> x : xs) (tail (tabl @65602))

-- instance OEIS 167831 where
--   oeisIx n = head [x | let ds = (rowT @31298) n, x <- [n, n - 1 ..],
--                         all (< 10) $ zipWith (+) ds (oeisIx_row x)]

-- instance OEIS 167832 where
--   b167832 n = (oeisIx @167831) n + n

-- instance OEIS 167877 where
--   oeisIx n = head [x | let ts = (rowT @30341) n, x <- [n, n - 1 ..],
--                         all (< 3) $ zipWith (+) ts (oeisIx_row x)]

-- instance OEIS 167878 where
--   oeisIx n = (oeisIx @167877) n + n

-- instance OEIS 167939 where
--   a :: [Integer]
--   a = scanl1 (+) . (!! 1) . transpose . fix $ map ((1:) . zipWith (*) (scanl1 (*) l) . zipWith poly (scanl1 (+) l)) . scanl (flip (:)) [] . zipWith (zipWith (*)) pascal where l = iterate (2*) 1
--   pascal :: [[Integer]]
--   pascal = iterate (\l -> zipWith (+) (0: l) l) (1: repeat 0)
--   -- evaluate a polynomial at a given value
--   poly :: (Num a) => a -> [a] -> a
--   poly t = foldr (\e i -> e + t*i) 0

-- instance OEIS 168036 where
--   oeisIx n = (oeisIx @3415) n - n

-- instance OEIS 168223 where
--   oeisIx n = (oeisIx @6369) n - (oeisIx @6368) n

-- instance OEIS 168396 where
--   oeis = tablList @168396
-- instance Table 168396 where
--   rowCol = rowCol_off @168396 @1 @1
--   rowT   = rowT_off   @168396 @1
--   tabl = [1] : f [[1]] where
--      f xss = ys : f (ys : xss) where
--        ys = (map sum $ zipWith take [2..] xss) ++ [1]

-- instance OEIS 168559 where
--   oeis = scanl (+) 0 $ drop 2 (oeis @290)

-- instance OEIS 169611 where
--   oeisIx = (oeisIx @1222) . (oeisIx @65331)

-- instance OEIS 169630 where
--   oeisIx n = (oeisIx @7598) n * n

-- instance OEIS 169669 where
--   oeisIx n = (oeisIx @30) n * mod n 10

-- instance OEIS 169718 where
--   oeisIx = p [1,5,10,25,50,100] where
--      p _          0 = 1
--      p []         _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 169834 where
--   oeis = f (oeis @51950) [0..] where
--      f (0:0:ws) (x:y:zs) = y : f (0:ws) (y:zs)
--      f (_:v:ws) (_:y:zs) = f (v:ws) (y:zs)

-- instance OEIS 169835 where
--   oeis = f [] (tail (oeis @217)) (tail (oeis @290)) where
--      f ts us'@ (u:us) vs'@ (v:vs)
--        | u <= v = f (u : ts) us vs'
--        | any p $ map (divMod v) ts = v : f ts us' vs
--        | otherwise = f ts us' vs
--        where p (q, r) = r == 0 && (oeisIx @10054) q == 1

-- instance OEIS 169936 where
--   import Data.Map (empty, insertWith, elems)
--   import Data.Text (unpack); import Data.Maybe (fromJust)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @169936) !! (n - 1)
--   oeis = sort $ concat $ filter ((> 1) . length) $
--      elems $ fill [1..999] empty where
--         fill [] m = m
--         fill (z:zs) m = fill zs $ insertWith (++) (sort $ engl z) [z] m
--         engl :: Integer -> String
--         engl = unpack . fromJust . EN.us_cardinal defaultInflection

-- instance OEIS 170942 where
--   oeis = tablList @170942
-- instance Table 170942 where
--   rowCol n k = (tabf @170942) !! (n - 1) (k-1)
--   rowT n = map fps $ sort $ permutations [1..n] where
--      fps perm = sum $ map fromEnum $ zipWith (==) perm [1..n]
--   tabf = map (rowT @170942) [1..]

-- instance OEIS 170949 where
--   oeis = tablList @170949
-- instance Table 170949 where
--   rowCol = rowCol_off @170949 @1 @1
--   rowT   = rowT_off @170949 @1
--   tabf = [1] : (map fst $ iterate f ([3,2,4], 3)) where
--     f (xs@ (x:_), i) = ([x + i + 2] ++ (map (+ i) xs) ++ [x + i + 3], i + 2)
--   oeis = concat (tabf @170949)

-- instance OEIS 171135 where
--   oeisIx n = head [p | p <- (oeis @40), let x = p + 2 * n,
--                         oeisIx x == 1 || (oeisIx @10051)' x == 1]

-- instance OEIS 171137 where
--   oeisIx n = head [m | m <- [1..], (oeisIx @171135) m == (oeisIx @40) n]

-- instance OEIS 171462 where
--   oeisIx n = div n p * (p - 1) where p = (oeisIx @6530) n

-- instance OEIS 171492 where
--   oeis = filter f [1..] where
--      f x = any ((> 0) . mod x) ds where
--        ds = map digitToInt (if c == '0' then cs else cs')
--        cs'@ (c:cs) = nub $ sort $ show x

-- instance OEIS 171637 where
--   oeis = tablList @171637
-- instance Table 171637 where
--   rowCol = rowCol_off @171637 @2 @1
--   tabf = map (rowT @171637) [2..]
--   rowT n = reverse $ filter ((== 1) . (oeisIx @10051)) $
--      map (2 * n -) $ takeWhile (<= 2 * n) (oeis @40)

-- instance OEIS 171746 where
--   oeisIx = (+ 1) . length . takeWhile (== 0) .
--                              map (oeisIx @10052) . tail . iterate (oeisIx @28392)

-- instance OEIS 171797 where
--   oeisIx n = read $ concatMap (show . ($ n))
--                      [oeisIx, (oeisIx @196563), (oeisIx @196564)] :: Integer

-- instance OEIS 171798 where
--   oeisIx n = read $ concatMap (show . ($ n))
--                      [oeisIx, (oeisIx @23416), (oeisIx @120)] :: Integer

-- instance OEIS 171862 where
--   oeisIx n = 1 + fromJust (elemIndex n (oeis @181391))

-- instance OEIS 171865 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @181391)

-- instance OEIS 171874 where
--   oeis = [0, 0, 0, 1, 1] ++ zipWith5 (\z y x w v -> z + x*y + w^v)
--      (drop 4 (oeis @171874)) (drop 3 (oeis @171874))
--      (drop 2 (oeis @171874)) (tail (oeis @171874)) (oeis @171874)

-- instance OEIS 171886 where
--   oeis = elemIndices 1 $ map (oeisIx @209229) $ concat (tabl @8949)

-- instance OEIS 171901 where
--   oeis = elemIndices 0 (oeis @196368)

-- instance OEIS 171903 where
--   oeis = elemIndices 0 $
--                  zipWith (+) (oeis @196368) $ tail (oeis @196368)

-- instance OEIS 171904 where
--   oeisIx n = head [m | m <- (oeis @171901), (oeisIx @196368) (m + n) == 0]

-- instance OEIS 171942 where
--   oeisIx 1 = 0
--   oeisIx n = head [m | m <- [1..], (oeisIx @120) (m + n - 1) == (oeisIx @120) (n - 1)]

-- instance OEIS 171971 where
--   oeisIx = floor . (/ 4) . (* sqrt 3) . fromInteger . (oeisIx @290)

-- instance OEIS 171972 where
--   oeisIx = floor . (* sqrt 3) . fromInteger . (oeisIx @290)

-- instance OEIS 171973 where
--   oeisIx = floor . (/ 12) . (* sqrt 2) . fromInteger . (oeisIx @578)

-- instance OEIS 171978 where
--   oeisIx n = q (fromInteger n) $ zipWith (%) [1..n] [2..] where
--      q 0 _         = 1
--      q _ []        = 0
--      q x ks'@ (k:ks)
--        | x < k     = fromEnum (x == 0)
--        | otherwise = q (x - k) ks' + q x ks

-- instance OEIS 172287 where
--   oeis = filter
--      (\p -> (oeisIx @10051)' (2 * p - 3) + (oeisIx @10051)' (3 * p - 2) == 1) (oeis @40)

-- instance OEIS 172471 where
--   oeisIx = (oeisIx @196) . (* 2)

-- instance OEIS 173018 where
--   oeis = tablList @173018
-- instance Table 173018 where
--   tabl = map reverse (tabl @123125)

-- instance OEIS 173333 where
--   oeis = tablList @173333
-- instance Table 173333 where
--   rowCol = rowCol_off @173333 @1 @1
--   rowT   = rowT_off   @173333 @1
--   tabl = map fst $ iterate f ([1], 2)
--      where f (row, i) = (map (* i) row ++ [1], i + 1)

-- instance OEIS 173517 where
--   oeisIx n = (1 - (oeisIx @10052) n) * (oeisIx @28391) n

-- instance OEIS 173525 where
--   oeisIx = (+ 1) . (oeisIx @53824) . (subtract 1)

-- instance OEIS 173540 where
--   oeis = tablList @173540
-- instance Table 173540 where
--   rowCol n k = (rowT @173540) n !! (k-1)
--   rowT   = rowT_off @173540 @1
--   tabf = [0] : [0] : map
--                  (\v -> [w | w <- [2 .. v - 1], mod v w > 0]) [3..]

-- instance OEIS 173541 where
--   oeis = tablList @173541
-- instance Table 173541 where
--   rowCol = rowCol_off @173541 @1 @1
--   rowT   = rowT_off   @173541 @1
--   tabl = zipWith (zipWith (*))
--                          (tabl @2260) $ map (map (1 -)) (tabl @51731)

-- instance OEIS 173557 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (subtract 1) $ (rowT @27748) n

-- instance OEIS 173694 where
--   oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @2322)) [1..]

-- instance OEIS 173732 where
--   oeis = f $ tail (oeis @25480) where f (x : _ : _ : xs) = x : f xs

-- instance OEIS 173927 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` map (+ 1) (oeis @185816))

-- instance OEIS 173990 where
--   :m Math.Operad
--   let a = corolla 1 [1,2]
--   let t1_23 = shuffleCompose 2 [1,2,3] a a
--   let t12_34 = shuffleCompose 1 [1,2,3,4] t1_23 a
--   let t13_24 = shuffleCompose 1 [1,3,2,4] t1_23 a
--   let t14_23 = shuffleCompose 1 [1,4,2,3] t1_23 a
--   let r1 = (oet t12_34) - (oet t13_24) :: OperadElement Integer Rational PathPerm
--   let r2 = (oet t14_23) - (oet t13_24) :: OperadElement Integer Rational PathPerm
--   let gens = [r1,r2]
--   let cors = [a]
--   let gb = operadicBuchberger gens
--   let lms = map leadingMonomial gb
--   let computeStep d = basisElements cors lms d
--   map (length . computeStep) [1..]

-- instance OEIS 174168 where
--   oeis = [1,2,5,17] ++ zipWith div (zipWith (+)
--      (zipWith (*) (tail (oeis @174168)) (drop 3 (oeis @174168)))
--                   (map ((* 3) . (^ 2)) (drop 2 (oeis @174168)))) (oeis @174168)

-- instance OEIS 174332 where
--   oeisIx = (oeisIx @208238) . (oeisIx @40)

-- instance OEIS 174375 where
--   oeisIx n = n ^ 2 - (oeisIx @169810) n

-- instance OEIS 174382 where
--   oeis = tablList @174382
-- instance Table 174382 where
--   rowCol = rowCol_off @174382 @1 @0
--   rowT   = rowT_off @174382 @1
--   tabf = iterate f [0] where
--      f xs = g (xs ++ [0, 0 ..]) [0..] (map head zs) (map length zs)
--        where g _ _ _ [] = []
--              g (u:us) (k:ks) hs'@ (h:hs) vs'@ (v:vs)
--                | k == h = u + v : g us ks hs vs
--                | k /= h = u : g us ks hs' vs'
--              zs = group $ sort xs

-- instance OEIS 174429 where
--   oeisIx = (oeisIx @45) . (oeisIx @8908)

-- instance OEIS 174903 where
--   oeisIx n = genericLength [d | let ds = (rowT @27750) n, d <- ds,
--                       not $ null [e | e <- [d+1 .. 2*d-1] `intersect` ds]]

-- instance OEIS 174904 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` map (oeisIx @174903) [1..])

-- instance OEIS 174905 where
--   oeis = filter ((== 0) . (oeisIx @174903)) [1..]

-- instance OEIS 174956 where
--   oeis = unfoldr x (1, 1, (oeis @1358)) where
--      x (i, z, ps'@ (p:ps)) | i == p = Just (z, (i + 1, z + 1, ps))
--                           | i /= p = Just (0, (i + 1, z, ps'))

-- instance OEIS 174973 where
--   oeis = filter f [1..] where
--      f n = all (<= 0) $ zipWith (-) (tail divs) (map (* 2) divs)
--            where divs = (rowT @27750)' n

-- instance OEIS 175046 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 .
--             concatMap (\bs@ (b:_) -> b : bs) . group . (rowT @30308)

-- instance OEIS 175047 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 . concatMap
--      (\bs@ (b:_) -> if b == 0 then 0 : bs else bs) . group . (rowT @30308)

-- instance OEIS 175048 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 . concatMap
--      (\bs@ (b:_) -> if b == 1 then 1 : bs else bs) . group . (rowT @30308)

-- instance OEIS 175118 where
--   oeis = 2 : f 2 (oeis @40) where
--      f x ps = g $ dropWhile (<= x) ps where
--        g (q:qs) | (oeisIx @10051)' (q - x + 1) == 1 = g qs
--                 | otherwise                 = q : f q qs

-- instance OEIS 175119 where
--   oeis = map (+ 1) $ zipWith (-) (tail (oeis @175118)) (oeis @175118)

-- instance OEIS 175130 where
--   oeis = map (+ 1) $ findIndices ((== 0) . (oeisIx @212793)) $ tail (oeis @45)

-- instance OEIS 175332 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @175332) !! (n - 1)
--   oeis = f $ singleton 3 where
--     f s = x : f (if even x then insert z s' else insert z $ insert (z+1) s')
--           where z = 2*x; (x, s') = deleteFindMin s

-- instance OEIS 175499 where
--   oeis = zipWith (-) (tail (oeis @175498)) (oeis @175498)

-- instance OEIS 175522 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @192895)

-- instance OEIS 175524 where
--   oeis = map (+ 1) $ findIndices (< 0) (oeis @192895)

-- instance OEIS 175526 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @192895)

-- instance OEIS 175592 where
--   oeis = filter (z 0 0 . (rowT @27746)) $ [1..] where
--      z u v []     = u == v
--      z u v (p:ps) = z (u + p) v ps || z u (v + p) ps

-- instance OEIS 175688 where
--   oeis = filter f [0..] where
--      f x = m == 0 && ("0123456789" !! avg) `elem` show x
--            where (avg, m) = divMod (oeisIx x) (oeisIx x)

-- instance OEIS 175840 where
--   oeis = tablList @175840
-- instance Table 175840 where
--   tabf = iterate (\xs@ (x:_) -> x * 3 : map (* 2) xs) [1]

-- instance OEIS 175872 where
--   oeisIx = f . (rowT @30308) where
--      f xs | all (== 1) xs = length xs
--           | otherwise     = f $ map genericLength $ group xs

-- instance OEIS 175899 where
--   oeis = 0 : 2 : 3 : 10 : zipWith (+) (map (* 2) (oeis @175899))
--      (zipWith (+) (tail (oeis @175899)) (drop 2 (oeis @175899)))

-- instance OEIS 175911 where
--   oeisIx n = foldl1 (\v d -> b * v + d) rls where
--      b = maximum rls + 1
--      rls = (rowT @101211) n

-- instance OEIS 175944 where
--   oeis = concat $ zipWith ($) (map replicate (oeis @18252)) (oeis @18252)

-- instance OEIS 176271 where
--   oeis = tablList @176271
-- instance Table 176271 where
--   rowCol = rowCol_off @176271 @1 @1
--   rowT   = rowT_off   @176271 @1
--   tabl = f 1 (oeis @5408) where
--      f x ws = us : f (x + 1) vs where (us, vs) = splitAt x ws

-- instance OEIS 176892 where
--   oeisIx 0 = 2oeisIx n = foldl (\v d -> 10 * v + d + 2) 0 $
--      unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2) n

-- instance OEIS 176995 where
--   oeis = filter ((> 0) . (oeisIx @230093)) [1..]

-- instance OEIS 177000 where
--   oeis = filter (all (\x -> even x || (oeisIx @10051)' x == 1) .
--                          (init . (rowT @70165))) (oeis @40)

-- instance OEIS 177047 where
--   toBinary 0 = []
--   toBinary n = toBinary (n `div` 2) ++ [odd n]
--   a = [2 + fromJust (findIndex (isPrefixOf (toBinary n)) [toBinary (n ^ k) | k <- [2..]]) | n <- [1..]]

-- instance OEIS 177048 where
--   toBinary 0 = []
--   toBinary n = toBinary (n `div` 2) ++ [odd n]
--   a = [1 + fromJust (findIndex (isPrefixOf (toBinary n)) [toBinary (k ^ n) | k <- [1..]]) | n <- [1..]]

-- instance OEIS 177062 where
--   toBinary 0 = []
--   toBinary n = toBinary (n `div` 2) ++ [odd n]
--   lcstr xs ys = maximum . concat $ [f xs' ys | xs' <- tails xs] ++ [f xs ys' | ys' <- drop 1 $ tails ys] where f xs ys = scanl g 0 $ zip xs ys; g z (x, y) = if x == y then z + 1 else 0
--   a = [lcstr (toBinary $ n) (toBinary $ n^2) | n <- [1..]]

-- instance OEIS 177729 where
--   oeisIx = head . (rowT @192719)

-- instance OEIS 177869 where
--   base_weight b g n | n == 0 = 0 | otherwise = (base_weight b g (n `div` b)) + (g $ n `mod` b)
--   interesting b g = filter f [1..] where f n = n `mod` (base_weight b g n) == 0
--   bin_interesting g = interesting 2 g
--   weights l n | (n >=0) && ((length l) > fromInteger n) = l !! fromInteger n | otherwise = 0
--   cnst = weights [1, 1]
--   let sequence = bin_interesting cnst

-- instance OEIS 177904 where
--   oeis = 1 : 1 : 1 : (map (oeisIx @6530) $ zipWith (+)
--      (oeis @177904) (tail $ zipWith (+) (oeis @177904) $ tail (oeis @177904)))

-- instance OEIS 177994 where
--   oeis = tablList @177994
-- instance Table 177994 where
--   tabl = [1] : [1,1] : map f (tabl @177994)
--                  where f xs@ (x:_) = (x + 1) : 1 : xs

-- instance OEIS 178158 where
--   oeis = filter (\suff -> all ((== 0) . (mod suff))
--      (map read $ tail $ init $ tails $ show suff :: [Integer])) (oeis @67251)

-- instance OEIS 178212 where
--   oeis = filter f [1..] where
--      f x = length (oeisIx_row x) == 3 && any (> 1) (oeisIx_row x)

-- instance OEIS 178318 where
--   oeis = 2 : 5 : filter f (oeis @62332) where
--      f p = null (show p `intersect` "34679") && (oeisIx @10051)' (r 0 p) == 1
--      r y 0 = y
--      r y x = r (10 * y + genericIndex [0,1,5,0,0,2,0,0,8,0] d) x'
--                where (x', d) = divMod x 10

-- instance OEIS 178333 where
--   oeisIx n = fromEnum $
--      n `mod` 10 == 1 && (oeisIx @30) n == 1 && (oeisIx @196368) n == 1 && and down where
--         down = dropWhile (== False) $ zipWith (<) (tail $ show n) (show n)

-- instance OEIS 178358 where
--   oeisIx (fi->n) = fi do read $ show (oeisIx n) ++ show n :: Integer

-- instance OEIS 178361 where
--   oeis = [x | x <- [1..], (oeisIx @7953) x <= (oeisIx @55642) x]

-- instance OEIS 178384 where
--   oeis = [1, 1, 1, 3] ++
--      zipWith div (foldr1 (zipWith subtract) (map b [1..2])) (oeis @178384)
--      where b i = zipWith (*) (drop i (oeis @178384)) (drop (4-i) (oeis @178384))

-- instance OEIS 178609 where
--   oeisIx n = head [k | k <- [n - 1, n - 2 .. 0], let p2 = 2 * (oeisIx @40) n,
--                         (oeisIx @40) (n - k) + (oeisIx @40) (n + k) == p2]

-- instance OEIS 178649 where
--   oeisIx n = div (oeisIx n) (oeisIx n)

-- instance OEIS 178799 where
--   oeis = zipWith (-) (tail (oeis @25487)) (oeis @25487)

-- instance OEIS 178910 where
--   oeisIx = foldl1 xor . (rowT @27750) :: Integer -> Integer

-- instance OEIS 178943 where
--   oeis = 2 : h (oeis @40) where
--      h (p:qs@ (q:r:ps)) = if 2 * q /= (p + r) then q : h qs else h qs

-- instance OEIS 178953 where
--   oeis = filter ((== 0) . (oeisIx @178609)) [1..]

-- instance OEIS 179242 where
--   oeis = concatMap h $ drop 3 $ inits $ drop 2 (oeis @45) where
--      h is = reverse $ map (+ f) fs where
--             (f:_:fs) = reverse is

-- instance OEIS 179248 where
--   oeis = filter ((== 9) . (oeisIx @7895)) [1..]

-- instance OEIS 179249 where
--   oeis = filter ((== 9) . (oeisIx @7895)) [1..]

-- instance OEIS 179250 where
--   oeis = filter ((== 10) . (oeisIx @7895)) [1..]

-- instance OEIS 179251 where
--   oeis = filter ((== 11) . (oeisIx @7895)) [1..]

-- instance OEIS 179253 where
--   oeis = filter ((== 13) . (oeisIx @7895)) [1..]

-- instance OEIS 179336 where
--   oeis = filter (any (`elem` "2357") . show ) (oeis @40)

-- instance OEIS 179627 where
--   oeisIx = (+ 1) . (oeisIx @6666) . (oeisIx @40)

-- instance OEIS 179909 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 1 (oeis @79066)

-- instance OEIS 179910 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 2 (oeis @79066)

-- instance OEIS 179911 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 3 (oeis @79066)

-- instance OEIS 179912 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 4 (oeis @79066)

-- instance OEIS 179913 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 5 (oeis @79066)

-- instance OEIS 179914 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 6 (oeis @79066)

-- instance OEIS 179915 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 7 (oeis @79066)

-- instance OEIS 179916 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 8 (oeis @79066)

-- instance OEIS 179917 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 9 (oeis @79066)

-- instance OEIS 179918 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 10 (oeis @79066)

-- instance OEIS 179919 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 11 (oeis @79066)

-- instance OEIS 179922 where
--   oeis = map (oeisIx . (+ 1)) $ elemIndices 12 (oeis @79066)

-- instance OEIS 179924 where
--   oeis = map (oeisIx . (+ 1)) $ findIndices (> 0) (oeis @79066)

-- instance OEIS 180046 where
--   oeis = [1..4] ++ zipWith4 (((((+) .) . (+)) .) . (-))
--                             (drop 3 (oeis @180046)) (drop 2 (oeis @180046))
--                               (tail (oeis @180046)) (oeis @180046)

-- instance OEIS 180058 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @59233))

-- instance OEIS 180076 where
--   oeis :: [Integer]
--   oeis = 0 : f 0 [1..] where
--      f x zs = y : f y (delete y zs) where
--        y = if null ys then 3 * x + 1 else head ys
--        ys = [y | y <- takeWhile (< x) zs, binInfix y x]
--      binInfix u v = ib v where
--        ib w = w `mod` m == u || w > u && ib (w `div` 2)
--        m = (oeisIx @62383) u

-- instance OEIS 180077 where
--   oeisIx = fromJust . (`elemIndex` (oeis @180076))

-- instance OEIS 180094 where
--   oeisIx n = snd $ until ((< 2) . fst) (\ (x, c) -> (oeisIx x, c+1)) (n,0)

-- instance OEIS 180110 where
--   oeis = map (+ 2) $ elemIndices True $ zipWith (&&) zs (tail zs)
--      where zs = zipWith (<) (oeis @180076) (tail (oeis @180076))

-- instance OEIS 180149 where
--   oeis = filter ((== 2) . (oeisIx @2635)) [0..]

-- instance OEIS 180191 where
--   oeisIx n = if n == 1 then 0 else sum $ (rowT @116853) (n - 1)

-- instance OEIS 180197 where
--   oeis = f 1 where
--      f x = if length ps == 3 && nub ps == ps
--            then (2 ^ (ps!!0 * ps!!1) `mod` ps!!2) : f (x+2) else f (x+2)
--            where ps = (rowT @27746) x

-- instance OEIS 180304 where
--   oeisIx n = fi (oeisIx n) * n
--   oeis = map sum $ group (oeis @6)

-- instance OEIS 180477 where
--   oeis = filter (\x -> mod (x * (oeisIx @55642) x) (oeisIx x) == 0) [1..]

-- instance OEIS 180662 where
--   oeis = tablList @180662
-- instance Table 180662 where
--   tabl = tail $ inits (oeis @1654)

-- instance OEIS 180663 where
--   oeis = tablList @180663
-- instance Table 180663 where
--   tabl = map reverse (tabl @180662)

-- instance OEIS 180853 where
--   oeis = iterate (oeisIx @6368) 4

-- instance OEIS 180864 where
--   oeis = iterate (oeisIx @6368) 13

-- instance OEIS 181363 where
--   oeis = concat $ transpose [oeis, (oeis @181363)]

-- instance OEIS 181424 where
--   oeisIx = (oeisIx @40) . (+ 2) . (oeisIx @64113)

-- instance OEIS 181511 where
--   oeis = tablList @181511
-- instance Table 181511 where
--   rowCol = rowCol_off @181511 @1 @0
--   rowT   = rowT_off   @181511 @1
--   tabl = tail $ map init (tabl @8279)

-- instance OEIS 181522 where
--   oeisIx = genericLength . filter ((== 1) . (oeisIx @64911) . sum) .
--                             subsequences . enumFromTo 1

-- instance OEIS 181741 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @81118)

-- instance OEIS 181819 where
--   oeisIx = product . map (oeisIx @40) . (rowT @124010) . succ

-- instance OEIS 181894 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ (rowT @213925) n

-- instance OEIS 181921 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @78350))

-- instance OEIS 181971 where
--   oeis = tablList @181971
-- instance Table 181971 where
--   tabl = map snd $ iterate f (1, [1]) where
--      f (i, row) = (1 - i, zipWith (+) ([0] ++ row) (row ++ [i])

-- instance OEIS 181988 where
--   interleave (hdx : tlx) y = hdx : interleave y tlx
--   oeis003602 = interleave [1..] oeis003602
--   oeis181988 = interleave [1..] (zipWith (+) oeis003602 oeis181988)

-- instance OEIS 182049 where
--   oeis = filter ((< 10) . (oeisIx @137580)) [0..]

-- instance OEIS 182061 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @61373))

-- instance OEIS 182126 where
--   oeis = zipWith3 (\p p' p'' -> mod (p * p') p'')
--                     (oeis @40) (tail (oeis @40)) (drop 2 (oeis @40))

-- instance OEIS 182134 where
--   oeisIx = genericLength . (rowT @244365)

-- instance OEIS 182140 where
--   oeis = [x | x <- [1..], (oeisIx @60968) x == (oeisIx @201629) x]

-- instance OEIS 182145 where
--   oeis = zipWith xor (oeis @1223) $ tail (oeis @1223) :: [Integer]

-- instance OEIS 182147 where
--   oeis = [w | w <- [1..] , sum (dropWhile (<= (oeisIx @196) w) $
--                                         (rowT @27751) $ fromInteger w) == w]

-- instance OEIS 182182 where
--   oeis = 0 : 1 : zipWith xor [2..]
--                  (zipWith xor (oeis @182182) $ tail (oeis @182182)) :: [Integer]
--   _Reinhard Zumkeller_, Apr 23 2012

-- instance OEIS 182183 where
--   oeis = f (oeis @209933) [1] where
--      f (x:xs) ys =
--        if null (oeisIx_row x \\ ys) then x : f xs (x : ys) else f xs ys

-- instance OEIS 182205 where
--   oeis = iterate (oeisIx @6368) 40

-- instance OEIS 182210 where
--   oeis = tablList @182210
-- instance Table 182210 where
--   rowCol = rowCol_off @182210 @1 @1
--   tabl = [[k* (n+1) `div` (k+1) | k <- [1..n]] | n <- [1..]]

-- instance OEIS 182211 where
--   oddDigits 0 = True
--   oddDigits n = let (q,r) = quotRem n 10
--   ..............in (odd r) && oddDigits q
--   oddSet 0 = []
--   oddSet 1 = [1,3..9]
--   oddSet k = [n | i <- [1,3..9], x <- oddSet (k-1), let n = i*10^ (k-1) + x,
--   ...............oddDigits ((n^3) `mod` 10^k)]
--   main = putStrLn $ map (length . oddSet) [1..]

-- instance OEIS 182229 where
--   oeis = 2 : 3 : zipWith (+)
--                          (map (flip div 3) (oeis @182229)) (tail (oeis @182229))

-- instance OEIS 182230 where
--   oeis = 3 : 4 : zipWith (+)
--                          (map (flip div 4) (oeis @182230)) (tail (oeis @182230))

-- instance OEIS 182237 where
--   oeis = map (+ 2 ) $ elemIndices 2 (oeis @59233)

-- instance OEIS 182242 where
--   oeis = map fst $ iterate f (0,1) where
--      f (y,x) = ((x + y) .&. x, x + 1) :: (Integer,Integer)

-- instance OEIS 182243 where
--   oeis = map fst $ iterate f (0,1) where
--      f (y,x) = ((x .&. y) + x, x + 1) :: (Integer,Integer)

-- instance OEIS 182248 where
--   oeis = map fst $ iterate f (0,1) where
--      f (y,x) = ((x .|. y) + x, x + 1) :: (Integer,Integer)

-- instance OEIS 182280 where
--   oeis = 3 : 4 : zipWith (+)
--                          (oeis @182280) (map (flip div 4) $ tail (oeis @182280))

-- instance OEIS 182281 where
--   oeis = 2 : 3 : zipWith (+)
--                          (oeis @182281) (map (flip div 3) $ tail (oeis @182281))

-- instance OEIS 182310 where
--   oeis = 0 : map (+ 1)
--      (zipWith xor (oeis @182310) $ map (`div` 2) (oeis @182310)) :: [Integer]

-- instance OEIS 182324 where
--   oeisIx n = n + (oeisIx @30) n [0]

-- instance OEIS 182388 where
--   oeis = f 0 1 where
--      f x y = y' : f (x + 1) y' :: [Integer] where y' = (x `xor` y) + x

-- instance OEIS 182402 where
--   oeis = map (sum . map (oeisIx @55642)) $ t 1 [1..] where
--      t i xs = ys : t (i + 1) zs where
--        (ys, zs) = splitAt i xs

-- instance OEIS 182426 where
--   oeis = concatMap f $ group $ zipWith (-) (tail ips) ips where
--      f xs | head xs == 1 = reverse $ enumFromTo 2 $ length xs + 1
--           | otherwise    = take (length xs) $ repeat 1
--      ips = map (oeisIx @49084) (oeis @166251)

-- instance OEIS 182458 where
--   oeis = 1 : 2 : zipWith mod
--      (map (+ 1) $ zipWith (*) (oeis @182458) (tail (oeis @182458))) [2..]

-- instance OEIS 182472 where
--   oeisIx = fromJust . (`elemIndex` (oeis @182458))

-- instance OEIS 182560 where
--   oeis = 0 : 1 : 2 : zipWith xor [3..]
--      (tail $ zipWith (.&.) (oeis @182560) $ tail (oeis @182560)) :: [Integer]

-- instance OEIS 182579 where
--   oeis = tablList @182579
-- instance Table 182579 where
--   tabl = [1] : iterate (\row ->
--     zipWith (+) ([0] ++ row) (zipWith (*) (row ++ [0]) (oeis @59841))) [1,2]

-- instance OEIS 182584 where
--   oeisIx n = (oeisIx @182579) (2*n) n

-- instance OEIS 182834 where
--   oeisIx n = (oeisIx @196) (2 * n - 2) + n

-- instance OEIS 182850 where
--   oeisIx n = genericLength $ takeWhile (`notElem` [1,2]) $ iterate (oeisIx @181819) n

-- instance OEIS 182938 where
--   oeisIx n = product $ zipWith (oeisIx @7318)'
--      (oeisIx_row n) (map toInteger $ (rowT @124010) n)

-- instance OEIS 182991 where
--   oeis = filter f [1..] where
--      f x = all (== 1) $ zipWith (+) dps $ tail dps where
--            dps = map (flip mod 2) $ (rowT @27750)' x

-- instance OEIS 183063 where
--   oeisIx = sum . map (1 -) . (rowT @247795)

-- instance OEIS 183079 where
--   oeis = tablList @183079
-- instance Table 183079 where
--   rowCol = rowCol_off @183079 @1 @1
--   tabf = [1] : iterate (\row -> concatMap f row) [2]
--      where f x = [oeisIx x, (oeisIx @14132) x]
--   oeis = concat (tabf @183079)

-- instance OEIS 183091 where
--   oeisIx = product . (rowT @210208)

-- instance OEIS 183168 where
--   oeisIx n = z (drop (fromInteger (mod n 2)) (oeis @40)) (n ^ 2) 3 where
--      z _      m 1 = if m <= 0 then 0 else (oeisIx @10051) m
--      z (p:ps) m c = if m <= 2*p then 0 else z ps (m - p) (c - 1) + z ps m c

-- instance OEIS 184162 where
--   oeis = 1 : g 2 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then 2 * (oeisIx @184162) t + 1 else (oeisIx @184162) r + (oeisIx @184162) s - 1
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 184165 where
--   oeisIx n = (oeisIx @228731) n + (oeisIx @228732) n
--   oeisIx n = genericIndex (oeis @228731) (n - 1)
--   oeisIx n = genericIndex (oeis @228732) (n - 1)

-- instance OEIS 184389 where
--   oeisIx = (oeisIx @217) . (oeisIx @5)'

-- instance OEIS 184989 where
--   oeisIx n = read $ interleave (show n) (show (n - 1)) :: Integer where
--      interleave []     ys = ys
--      interleave (x:xs) ys = x : interleave ys xs

-- instance OEIS 184992 where
--   oeis = 1 : f 1 [2..] where
--      f u vs = v : f v (delete v vs)
--        where v : _ = filter (not . null . (intersect `on` show) u) vs

-- instance OEIS 185024 where
--   oeis = map (+ 2 ) $ elemIndices 1 (oeis @59233)

-- instance OEIS 185038 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @90895)

-- instance OEIS 185080 where
--   oeisIx (fi->n) = fi $ 6 * (oeisIx @7318) (2 * n) (n - 1) + (oeisIx @7318) (2 * n - 1) n

-- instance OEIS 185086 where
--   oeis = filter (\p -> any ((== 1) . (oeisIx @10052)) $
--                  map (p -) $ takeWhile (<= p) (oeis @1248)) (oeis @40)

-- instance OEIS 185137 where
--   oeis = 1 : 1 : 1 : 1 : 1 : 1 : f 7 1 1 where
--      f x u v = w : f (x + 1) v w where
--                w = (oeisIx . (oeisIx @185137) . (oeisIx @185137)) (x - 1) +
--                    (oeisIx @185137) (x - (oeisIx . (oeisIx @185137)) (x - 3))

-- instance OEIS 185154 where
--   oeis = catMaybes $ map f (oeis @6093) where
--      f x = g $ takeWhile (< x) (oeis @65091) where
--        g []  = Nothing
--        g [_] = Nothing
--        g (p:ps@ (_:qs)) | (x - p) `elem` qs = Just p
--                        | otherwise         = g ps

-- instance OEIS 185208 where
--   oeis =  filter ((== 1) . (oeisIx @141197)) [1..]

-- instance OEIS 185212 where
--   oeisIx = (+ 1) . (* 4) . (oeisIx @567)

-- instance OEIS 185242 where
--   oeis = iterate (oeisIx @203907) 3

-- instance OEIS 185359 where
--   oeis = [x | x <- [1..], or $ zipWith (<)
--                       (oeisIx_row x) (map toInteger $ (rowT @124010) x)]

-- instance OEIS 185550 where
--   import Data.List.Ordered (minus)
--   oeisIx n = (oeis @185550) !! (n - 1)
--   oeis = [0..] `minus` (oeis @185549)

-- instance OEIS 185589 where
--   oeis = iterate (oeisIx @6369) 144

-- instance OEIS 185590 where
--   oeis = iterate (oeisIx @6369) 44

-- instance OEIS 185635 where
--   oeis = filter (> 0) $
--      zipWith (\x y -> if x == y then y else 0) [1..] (oeis @75075)

-- instance OEIS 185694 where
--   oeis = 1 : f [1] where
--      f xs = y : f (y : xs) where
--             y = sum $ zipWith (*) xs $ map negate (oeis @8683)

-- instance OEIS 185816 where
--   oeisIx n = if n == 1 then 0 else (oeisIx @185816) (oeisIx n) + 1

-- instance OEIS 185817 where
--   oeisIx n = pref101pow 0 1 where
--      pref101pow e pow101 = if isPrefixOf (show n) (show pow101)
--                               then e
--                               else pref101pow (e + 1) (101 * pow101)

-- instance OEIS 185934 where
--   oeis = map (oeisIx . (+ 1)) $
--      elemIndices 1 $ zipWith (*) (oeis @39701) $ tail (oeis @39701)

-- instance OEIS 186102 where
--   oeisIx n = f (oeis @40) where
--      f (q:qs) = if (q - n) `mod` (oeisIx n) == 0 then q else f qs

-- instance OEIS 186253 where
--   oeis = filter ((== 0) . (oeisIx @261301)) [1..]

-- instance OEIS 186336 where
--   oeisIx n = f $ takeWhile (<= n) (oeis @1358) where
--      f []       = 0
--      f (sp:sps) = g sp sps + f sps
--      g spSum []                    = fromEnum (spSum == n)
--      g spSum (sp:sps) | spSum < n  = g (sp + spSum) sps
--                       | spSum == n = 1
--                       | otherwise  = 0

-- instance OEIS 186712 where
--   oeisIx n = (+ 1) $ fromJust $ findIndex (== (oeisIx @3586) n) (oeis @186711)

-- instance OEIS 186771 where
--   oeis = map (+ 1) $ findIndices (== 1) (oeis @186711)

-- instance OEIS 186826 where
--   oeis = tablList @186826
-- instance Table 186826 where
--   tabl = map reverse (tabl @144944)

-- instance OEIS 187059 where
--   oeisIx = (oeisIx @7814) . (oeisIx @1142)

-- instance OEIS 187072 where
--   import Data.Set (Set, empty, member, insert)
--   oeisIx n = (oeis @187072) !! (n - 1)
--   oeis = goldbach 0 (oeis @65091) empty where
--     goldbach :: Integer -> [Integer] -> Set Integer -> [Integer]
--     goldbach q (p:ps) gbEven
--         | qp `member` gbEven = goldbach q ps gbEven
--         | otherwise          = p : goldbach p (oeis @65091) (insert qp gbEven)
--         where qp = q + p

-- instance OEIS 187085 where
--   oeisIx  n = (oeis @187085) !! (n - 1)
--   oeis = zipWith (+) (oeis @187072) $ tail (oeis @187072)

-- instance OEIS 187090 where
--   oeisIx n = until ((== 9) . (oeisIx @30)) (+ n) n

-- instance OEIS 187098 where
--   oeis = 1 : 2 : map (`div` 2) (oeis @187085)

-- instance OEIS 187099 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` oeis) . fi

-- instance OEIS 187204 where
--   oeis = map (+ 1) $ elemIndices 0 $ map (oeisIx @187202) [1..]

-- instance OEIS 187205 where
--   oeis = map (+ 1) $ elemIndices 0 $ map (oeisIx @187203) [1..]

-- instance OEIS 187208 where
--   oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @187203) [1..]

-- instance OEIS 187285 where
--   oeisIx n = until ((== 1) . (oeisIx @30)) (+ n) n

-- instance OEIS 187744 where
--   oeis = filter ((== 1) . (oeisIx @10054) . (oeisIx @7953)) [0..]

-- instance OEIS 187769 where
--   import List (elemIndices)
--   oeisIx n k = (tabf @187769) !! n !! k
--   oeisIx_row n = (tabf @187769) !! n
--   oeisIx_tabf = [0] : [elemIndices (b, len - b) $
--      takeWhile ((<= len) . uncurry (+)) $ zip (oeis @120) (oeis @23416) |
--      len <- [1 ..], b <- [1 .. len]]
--   oeis = concat (tabf @187769)

-- instance OEIS 187786 where
--   import List (find)
--   import Maybe (fromJust)
--   oeisIx n k = (tabf @187786) !! n !! k
--   oeisIx_row n = fromJust $ find (elem n) (tabf @187769)
--   oeisIx_tabf = map (rowT @187786) [0..]

-- instance OEIS 187921 where
--   import Data.Set (Set, singleton, member, insert)
--   oeisIx n = (oeis @187921) !! (n - 1)
--   oeis = r (singleton 0) 1 0 where
--      r :: Set Integer -> Integer -> Integer -> [Integer]
--      r s n x | x <= n           = n : r (insert (x+n) s) (n+1) (x+n)
--              | (x-n) `member` s = r (insert (x+n) s) (n+1) (x+n)
--              | otherwise        = r (insert (x-n) s) (n+1) (x-n)

-- instance OEIS 187922 where
--   import Data.Set (Set, singleton, member, insert)
--   oeisIx n = (oeis @187922) !! (n - 1)
--   oeis = r (singleton 0) 1 0 where
--      r :: Set Integer -> Integer -> Integer -> [Integer]
--      r s n x | x <= n           = r (insert (x+n) s) (n+1) (x+n)
--              | (x-n) `member` s = n : r (insert (x+n) s) (n+1) (x+n)
--              | otherwise        = r (insert (x-n) s) (n+1) (x-n)

-- instance OEIS 188069 where
--   oeis = filter ((== 2) . (oeisIx @7538)) [1..]
--
-- instance OEIS 188070 where
--   oeis = filter ((== 3) . (oeisIx @7538)) [1..]

-- instance OEIS 188145 where
--   oeis = elemIndices 0 $ zipWith3 (\x y z -> x - y - z)
--      (map (oeisIx @3415) (oeis @3415)) (oeis @3415) [0..]

-- instance OEIS 188163 where
--   oeisIx n = succ $ fromJust $ elemIndex n (oeis @4001)

-- instance OEIS 188226 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (map (oeisIx @188172) [1..]))) [0..]

-- instance OEIS 188264 where
--   oeis =
--      map (+ 1) $ elemIndices 0 $ zipWith mod [1..] $ map (oeisIx @66459) [1..]

-- instance OEIS 188528 where
--   oeisIx n = succ $ fromJust $
--     findIndex (\m -> h n m 13 == 6) [1..12] where
--       h year month day
--         | month <= 2 = h  (year - 1)  (month + 12)  day
--         | otherwise  = (day + 26 * (month + 1) `div` 10 + y + y `div` 4
--                        + century `div` 4 - 2 * century) `mod` 7
--           where (century, y) = divMod year 100
--   b188528 = bFileFun "A188528" (oeisIx @188528) 1991 3000
--   -- For statistics (see example) ...
--   ff13_perMonth ys m = length $ filter (== m) (map (oeisIx @188528) ys)
--   century20 = map (ff13_perMonth [1901..2000]) [1..12]
--   century21 = map (ff13_perMonth [2001..2100]) [1..12]

-- instance OEIS 188654 where
--   oeis = map (+ 1) $ findIndices (/= 0) (oeis @225230)

-- instance OEIS 188666 where
--   oeis = g 1 (oeis @961) where
--      g n pps'@ (pp:pp':pps) | n < 2*pp  = pp  : g (n+1) pps'
--                            | otherwise = pp' : g (n+1) (pp':pps)
--   oeisIx' n = last $ elemIndices (f 1) $ map f [0..n] where
--      f from = foldl lcm 1 [from..n]

-- instance OEIS 188715 where
--   let ext (c,l) = [ (tails.filter (\b->a* (a-1)`mod` (b-a)==0)$r,a:l) | (a:r)<-c] in map (last.snd.head) . tail . iterate (>>= ext) $ [ (map reverse (inits[2..]),[])]
--   -- [m:[m-d|d<-divisors (m* (m-1)),d<m-1]|m<-[2..]], defining divisors appropriately.

-- instance OEIS 188915 where
--   import Data.List.Ordered (union)
--   oeisIx n = (oeis @188915) !! n
--   oeis = union (oeis @290) (oeis @79)

-- instance OEIS 188916 where
--   oeis = filter ((== 1) . (oeisIx @10052). (oeisIx @188915)) [0..]

-- instance OEIS 188917 where
--   oeis = filter ((== 1) . (oeisIx @209229). (oeisIx @188915)) [0..]

-- instance OEIS 188968 where
--   oeis = filter ((== 0) . (oeisIx @188967)) [1..]

-- instance OEIS 188969 where
--   oeis = filter ((== 1) . (oeisIx @188967)) [1..]

-- instance OEIS 188999 where
--   oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
--      f p e = (p ^ (e + 1) - 1) `div` (p - 1) - (1 - m) * p ^ e' where
--              (e', m) = divMod e 2

-- instance OEIS 189056 where
--   oeis = 0 : filter (\x -> (oeisIx @258682) x /= x ^ 2) [1..]

-- instance OEIS 189398 where
--   oeisIx n = product $ zipWith (^) (oeis @40) (map digitToInt $ show n)

-- instance OEIS 189419 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @114183))

-- instance OEIS 189639 where
--   oeisIx n = (oeis @189710) !! (n - 1)
--   oeis = elemIndices 0 $
--      zipWith (-) (map (oeisIx @3415) (oeis @3415)) (map pred (oeis @3415))

-- instance OEIS 189710 where
--   oeis = elemIndices 0 $
--      zipWith (-) (map (oeisIx @3415) (oeis @3415)) (map pred (oeis @3415))

-- instance OEIS 189711 where
--   oeis = tablList @189711
-- instance Table 189711 where
--   rowCol n k = (n - k) ^ k - 2 * (oeisIx @7318) (n - 1) k + n - k
--   rowT n = map (oeisIx n) [3..n - 2]
--   tabl = map (rowT @189711) [5..]

-- instance OEIS 189835 where
--   oeisIx n = (oeisIx @1157) n - (oeisIx @38040) n

-- instance OEIS 189920 where
--   oeis = tablList @189920
-- instance Table 189920 where
--   rowCol n k = (rowT @189920) n !! k
--   rowT n = z n $ reverse $ takeWhile (<= n) $ tail (oeis @45) where
--      z x (f:fs'@ (_:fs)) | f == 1 = if x == 1 then [1] else []
--                         | f == x = 1 : replicate (length fs) 0
--                         | f < x  = 1 : 0 : z (x - f) fs
--                         | f > x  = 0 : z x fs'
--   tabf = map (rowT @189920) [1..]

-- instance OEIS 190016 where
--   import Data.Ord (comparing)
--   oeisIx n = (oeis @190016) !! (n - 1)
--   oeis = sortBy (comparing show) [1..10000]

-- instance OEIS 190017 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190016))) [1..10000]

-- instance OEIS 190018 where
--   oeis = 0 : drop 2 (merge (merge fibs $
--       map (^ 2) fibs) $ zipWith (*) fibs (drop 2 fibs))
--       where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
--             merge xs'@ (x:xs) ys'@ (y:ys)
--                | x < y     = x : merge xs ys'
--                | x == y    = x : merge xs ys
--                | otherwise = y : merge xs' ys

-- instance OEIS 190126 where
--   import Data.Ord (comparing)
--   oeisIx n = (oeis @190126) !! (n - 1)
--   oeis = sortBy (comparing (show . (oeisIx @7088))) [1..10000]

-- instance OEIS 190127 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190126))) [1..10000]

-- instance OEIS 190128 where
--   import Data.Ord (comparing)
--   oeisIx n = (oeis @190128) !! (n - 1)
--   oeis = sortBy (comparing (show . (oeisIx @7089))) [1..10000]

-- instance OEIS 190129 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190128))) [1..10000]

-- instance OEIS 190130 where
--   import Data.Ord (comparing)
--   oeisIx n = (oeis @190130) !! (n - 1)
--   oeis = sortBy (comparing (show . (oeisIx @7094))) [1..10000]

-- instance OEIS 190131 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190130))) [1..10000]

-- instance OEIS 190132 where
--   import Data.Ord (comparing)
--   import Numeric (showIntAtBase)
--   oeisIx n = (oeis @190132) !! (n - 1)
--   oeis =
--      sortBy (comparing (flip (showIntAtBase 12 intToDigit) "")) [1..10000]

-- instance OEIS 190133 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190132))) [1..10000]

-- instance OEIS 190134 where
--   import Data.Ord (comparing)
--   import Numeric (showHex)
--   oeisIx n = (oeis @190134) !! (n - 1)
--   oeis = sortBy (comparing (flip showHex "")) [1..10000]

-- instance OEIS 190135 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190134))) [1..10000]

-- instance OEIS 190136 where
--   oeisIx n = maximum $ map (oeisIx @6530) [n..n+3]

-- instance OEIS 190137 where
--   oeisIx n = head [k | k <- [1..9],
--                         all (<= "0123456789" !! k) $ show (k * n)]

-- instance OEIS 190311 where
--   oeisIx n = g n $ reverse $ takeWhile (<= n) $ tail (oeis @578) where
--     g _ []                 = 0
--     g m (x:xs) | x > m     = g m xs
--                | otherwise = signum m' + g r xs where (m',r) = divMod m x

-- instance OEIS 190321 where
--   oeisIx n = g n $ reverse $ takeWhile (<= n) $ tail (oeis @290) where
--     g _ []                 = 0
--     g m (x:xs) | x > m     = g m xs
--                | otherwise = signum m' + g r xs where (m',r) = divMod m x

-- instance OEIS 190619 where
--   oeis = map read $ f 2 1 :: [Integer] where
--     f m k
--       | k < m - 1 = ((take k ones) ++ "0" ++ (take (m-k) ones)) : f m (k+1)
--       | otherwise = ((take k ones) ++ "01") : f (m + 1) 1
--     ones = repeat '1'

-- instance OEIS 190620 where
--   oeis = filter odd $ elemIndices 1 (oeis @23416)
--   oeis' = g 8 2 where
--      g m 2 = (m - 3) : g (2*m) (m `div` 2)
--      g m k = (m - k - 1) : g m (k `div` 2)

-- instance OEIS 190641 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @56170)

-- instance OEIS 190651 where
--   oeis = filter ((== 1) . (oeisIx @101312)) [1901..]

-- instance OEIS 190652 where
--   oeis = filter ((== 2) . (oeisIx @101312)) [1901..]

-- instance OEIS 190653 where
--   oeis = filter ((== 3) . (oeisIx @101312)) [1901..]

-- instance OEIS 190803 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190803) !! (n - 1)
--   oeis = 1 : f (singleton 2)
--      where f s = m : (f $ insert (2*m-1) $ insert (3*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190805 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190805) !! (n - 1)
--   oeis = 1 : f (singleton 4)
--      where f s = m : (f $ insert (2*m-1) $ insert (3*m+1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190806 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190806) !! (n - 1)
--   oeis = 1 : f (singleton 5)
--      where f s = m : (f $ insert (2*m-1) $ insert (3*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190807 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190807) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (2*m) $ insert (3*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190808 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190808) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (2*m) $ insert (3*m+1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190809 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190809) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (2*m) $ insert (3*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190810 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190810) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (2*m+1) $ insert (3*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190811 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190811) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (2*m+1) $ insert (3*m) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190812 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @190812) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (2*m+1) $ insert (3*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 190944 where
--   oeisIx = (oeisIx @7088) . (* 3)

-- instance OEIS 191113 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191113) !! (n - 1)
--   oeis = 1 : f (singleton 2)
--      where f s = m : (f $ insert (3*m-2) $ insert (4*m-2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191114 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191114) !! (n - 1)
--   oeis = 1 : f (singleton 3)
--      where f s = m : (f $ insert (3*m-2) $ insert (4*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191115 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191115) !! (n - 1)
--   oeis = 1 : f (singleton 4)
--      where f s = m : (f $ insert (3*m-2) $ insert (4*m) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191116 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191116) !! (n - 1)
--   oeis = 1 : f (singleton 5)
--      where f s = m : (f $ insert (3*m-2) $ insert (4*m+1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191117 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191117) !! (n - 1)
--   oeis = 1 : f (singleton 6)
--      where f s = m : (f $ insert (3*m-2) $ insert (4*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191118 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191118) !! (n - 1)
--   oeis = 1 : f (singleton 7)
--      where f s = m : (f $ insert (3*m-2) $ insert (4*m+3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191119 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191119) !! (n - 1)
--   oeis = 1 : f (singleton 2)
--      where f s = m : (f $ insert (3*m-1) $ insert (4*m-3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191120 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191120) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m-1) $ insert (4*m-2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191121 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191121) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m-1) $ insert (4*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191122 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191122) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m-1) $ insert (4*m) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191123 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191123) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m-1) $ insert (4*m+1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191124 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191124) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m-1) $ insert (4*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191125 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191125) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m-1) $ insert (4*m+3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191126 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191126) !! (n - 1)
--   oeis = 1 : f (singleton 3)
--      where f s = m : (f $ insert (3*m) $ insert (4*m-3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191127 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191127) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m) $ insert (4*m-2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191128 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191128) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m) $ insert (4*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191129 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191129) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m) $ insert (4*m+1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191130 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191130) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m) $ insert (4*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191131 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191131) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m) $ insert (4*m+3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191132 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191132) !! (n - 1)
--   oeis = 1 : f (singleton 4)
--      where f s = m : (f $ insert (3*m+1) $ insert (4*m-3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191133 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191133) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+1) $ insert (4*m-2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191134 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191134) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+1) $ insert (4*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191135 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191135) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+1) $ insert (4*m) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191136 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191136) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+1) $ insert (4*m+1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191137 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191137) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+1) $ insert (4*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191138 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191138) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+1) $ insert (4*m+3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191139 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191139) !! (n - 1)
--   oeis = 1 : f (singleton 5)
--      where f s = m : (f $ insert (3*m+2) $ insert (4*m-3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191140 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191140) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+2) $ insert (4*m-2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191141 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191141) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+2) $ insert (4*m-1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191142 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191142) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+2) $ insert (4*m) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191143 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191143) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+2) $ insert (4*m+1) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191144 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191144) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+2) $ insert (4*m+2) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191145 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191145) !! (n - 1)
--   oeis = f $ singleton 1
--      where f s = m : (f $ insert (3*m+2) $ insert (4*m+3) s')
--                where (m, s') = deleteFindMin s

-- instance OEIS 191203 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191203) !! (n - 1)
--   oeis = f $ singleton 1 where
--      f s = m : f (insert (2 * m) $ insert (m ^ 2 + 1) s')
--            where (m, s') = deleteFindMin s

-- instance OEIS 191211 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @191211) !! (n - 1)
--   oeis = f $ singleton 1 where
--      f s = m : f (insert (2 * m + 1) $ insert (m ^ 2 + 1) s')
--            where (m, s') = deleteFindMin s

-- instance OEIS 191292 where
--   oeis = f (oeis @31443) where
--      f (x:x':xs) | x' == x+2 = (x+1) : f xs
--                  | otherwise = f (x':xs)

-- instance OEIS 191587 where
--   oeisIx n = head [p | p <- dropWhile (<= n) (oeis @40),
--                         (oeisIx @1221) (p - n) == n]

-- instance OEIS 191610 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ takeWhile (> 0) $ map ((n - 1) `div`) (oeis @351)

-- instance OEIS 191854 where
--   oeisIx = (oeisIx @7966) . (oeisIx @7969)

-- instance OEIS 191855 where
--   oeisIx = (oeisIx @7967) . (oeisIx @7969)

-- instance OEIS 191856 where
--   oeisIx = (oeisIx @7966) . (oeisIx @7970)

-- instance OEIS 191857 where
--   oeisIx = (oeisIx @7967) . (oeisIx @7970)

-- instance OEIS 191860 where
--   oeisIx = fst . sqrtPair . (oeisIx @7969)

-- instance OEIS 191861 where
--   oeisIx = snd . sqrtPair . (oeisIx @7969)

-- instance OEIS 191862 where
--   oeisIx = fst . sqrtPair . (oeisIx @7970)

-- instance OEIS 191863 where
--   oeisIx = snd . sqrtPair . (oeisIx @7970)

-- instance OEIS 191933 where
--   oeis = findIndices (> 0) $ map (oeisIx @193095) [0..]

-- instance OEIS 191967 where
--   oeisIx n = n * (oeisIx @1651) n

-- instance OEIS 192010 where
--   oeisIx n = succ $ fromJust $ elemIndex n $ map (oeisIx @50252) [1..]

-- instance OEIS 192015 where
--   oeisIx = (oeisIx @3415) . (oeisIx @961)

-- instance OEIS 192016 where
--   oeisIx = (oeisIx @68346) . (oeisIx @961)

-- instance OEIS 192066 where
--   oeisIx = sum . filter odd . (rowT @77610)

-- instance OEIS 192109 where
--   oeis = map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @918) [1..]

-- instance OEIS 192134 where
--   oeisIx n = (oeisIx @961) n - (oeisIx @192015) n



-- instance OEIS 192362 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @131644))

-- instance OEIS 192476 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @192476) !! (n - 1)
--   oeis = f [1] (singleton 1) where
--      f xs s =
--        m : f xs' (foldl (flip insert) s' (map (+ m^2) (map (^ 2) xs')))
--        where xs' = m : xs
--              (m,s') = deleteFindMin s

-- instance OEIS 192484 where
--   oeis = 1 : 2 : f [2,1] where
--      f xs = y : f (y : xs) where
--        y = sum $ zipWith xor xs $ reverse xs :: Integer

-- instance OEIS 192503 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @3309)

-- instance OEIS 192504 where
--   oeis = filter ((== 0) . (oeisIx @10051)) (oeis @3309)

-- instance OEIS 192505 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @192607)

-- instance OEIS 192506 where
--   oeis = filter ((== 0) . (oeisIx @10051)) (oeis @192607)

-- instance OEIS 192512 where
--   oeis = scanl1 (+) $ map (oeisIx @192490) [1..]

-- instance OEIS 192607 where
--   oeis = filter ((== 0) . (oeisIx @192490)) [1..]

-- instance OEIS 192719 where
--   oeis = tablList @192719
-- instance Table 192719 where
--   rowCol = rowCol_off @192719 @1 @1
--   rowT   = rowT_off @192719 @1
--   tabf = f [1..] where
--      f (x:xs) = (oeisIx_row x) : f (del xs $ (rowT @220237) x)
--      del us [] = us
--      del us'@ (u:us) vs'@ (v:vs) | u > v     = del us' vs
--                                | u < v     = u : del us vs'
--                                | otherwise = del us vs

-- instance OEIS 192734 where
--   oeisIx n = head [x | x <- [2^u + 2^v + 1 | u <- [2..], v <- [1..u-1]],
--                         oeisIx x == n]

-- instance OEIS 192817 where
--   oeis = [x | x <- [1..], gcd x (oeisIx @61601 x) == 1]

-- instance OEIS 192825 where
--   oeis = filter (\x ->
--      '0' `elem` show x && null (show (2*x) `intersect` show x)) [1..]

-- instance OEIS 192849 where
--   oeisIx n = if n < 3 then 0 else (oeisIx @245334) (n + 1) 4



-- instance OEIS 192977 where
--   oeis = f 0 $ group (oeis @68395) where
--      f n xss'@ (xs:xss)
--        | head xs `div` 9 == n = length xs : f (n+1) xss
--        | otherwise            = 0 : f (n+1) xss'

-- instance OEIS 192993 where
--   oeis = findIndices (> 1) $ map (oeisIx @193095) [0..]

-- instance OEIS 193095 where
--   oeisIx n = sum $ map c [1.. (length $ show n) - 1] where
--      c k | head ys == '0' = 0
--          | otherwise      = (oeisIx @10052) (read xs) * (oeisIx @10052) (read ys) where
--          (xs,ys) = splitAt k $ show n

-- instance OEIS 193096 where
--   oeis = elemIndices 0 $ map (oeisIx @193095) [0..]

-- instance OEIS 193097 where
--   oeis = elemIndices 1 $ map (oeisIx @193095) [0..]

-- instance OEIS 193159 where
--   oeis = map (+ 1) $ findIndices (<= 3) (oeis @50430)

-- instance OEIS 193166 where
--   oeis = filter ((== 0) . (oeisIx @192280)) [1..]

-- instance OEIS 193169 where
--   oeisIx = genericLength . filter odd . (rowT @27750) . (oeisIx @2322)

-- instance OEIS 193213 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @99267)

-- instance OEIS 193286 where
--   -- See Theorem 5 in John Derbyshire link.
--   oeisIx n = p n [] where
--      p 0 ks       = product ks
--      p n []       = p (n - 1) [1]
--      p n (k:ks)
--       | n < 0     = 0
--       | otherwise = max (p (n - 1) ((k+1):ks)) (p (n-3) (1:k:ks))

-- instance OEIS 193314 where
--   oeisIx n = head [k | k <- [1..], let kk' = (oeisIx @2378) k,
--                         mod kk' (oeisIx n) == 0, (oeisIx @6530) kk' == (oeisIx @40) n]

-- instance OEIS 193315 where
--   oeisIx 1 = 1
--   oeisIx n = maximum $ zipWith (*) prims $ map (oeisIx . (2*n -)) prims
--      where prims = takeWhile (<= n) (oeis @8578)

-- instance OEIS 193331 where
--   oeis = tablList @193331
-- instance Table 193331 where
--   rowCol = rowCol_off @193331 @1 @1
--   tabl = map (rowT @193331) [1..]
--   rowT n = zipWith div (map (* n^2) [0..n - 1]) (map (2 *) [1..n])

-- instance OEIS 193358 where
--   oeis =
--      1 : 2 : (map ((+ 2) . (oeisIx @193358)) $ zipWith (-) [2..] (oeis @193358))

-- instance OEIS 193422 where
--   oeis = map (fromJust . (`elemIndex` (oeis @193358))) [1..]

-- instance OEIS 193428 where
--   oeisIx n = sum $ map ($ n) [oeisIx, (oeisIx @31288), (oeisIx @31289), (oeisIx @31290), (oeisIx @31291), (oeisIx @31292), (oeisIx @31293), (oeisIx @31294), (oeisIx @31295), (oeisIx @31296)]



-- instance OEIS 193460 where
--   oeis = elemIndices 0 $ 1 : zipWith (-) (oeis @193459) (oeis @5)

-- instance OEIS 193496 where
--   oeisIx = fromEnum . (>= 0) . (oeisIx @95916)

-- instance OEIS 193513 where
--   oeisIx n = p "0123456789" n 1 where
--      p "" _ _      = 0
--      p _  0 _      = 1
--      p cds m k
--        | m < k     = 0
--        | otherwise = p (cds `intersect` show k) (m - k) k + p cds m (k + 1)

-- instance OEIS 193574 where
--   oeisIx n = head [d | d <- [1..sigma] \\ nDivisors, mod sigma d == 0]
--      where nDivisors = (rowT @27750) n
--            sigma = sum nDivisors



-- instance OEIS 193596 where
--   oeis = tablList @193596
-- instance Table 193596 where
--   tabl = map (map ((flip div 2) . (+ 1))) (tabl @7318)

-- instance OEIS 193671 where
--   oeis = map (+ 1) $ findIndices (> 0) $ map (oeisIx @187202) [1..]

-- instance OEIS 193672 where
--   oeis = map (+ 1) $ findIndices (< 0) $ map (oeisIx @187202) [1..]

-- instance OEIS 193714 where
--   oeis =
--      map ((+ 1) . fromJust . (`elemIndex` (oeis @5214))) $ tail (oeis @290)

-- instance OEIS 193715 where
--   oeis =
--      map ((+ 1) . fromJust . (`elemIndex` (oeis @5214))) $ tail (oeis @217)

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

-- instance OEIS 193748 where
--   oeisIx = p (oeis @5214) where
--      p _          0 = 1
--      p ks'@ (k:ks) m
--        | m < k      = 0
--        | otherwise  = p ks' (m - k) + p ks m

-- instance OEIS 193749 where
--   oeisIx = p (oeis @5214) where
--      p _      0    = 1
--      p (k:ks) m
--        | m < k     = 0
--        | otherwise = p ks (m - k) + p ks m

-- instance OEIS 193829 where
--   oeis = tablList @193829
-- instance Table 193829 where
--   rowCol n k = genericIndex (tabf @193829) (n - 1) !! (k - 1)
--   rowT n = genericIndex (tabf @193829) (n - 1)
--   tabf = zipWith (zipWith (-))
--                          (map tail (oeisIx @27750)_tabf') (oeisIx @27750)_tabf'

-- instance OEIS 193832 where
--   oeis = tablList @193832
-- instance Table 193832 where
--   rowCol = rowCol_off @193832 @1 @1
--   rowT   = rowT_off @193832 @1
--   tabf = zipWith (++) (tabf @1650) (tabl @111650)
--   oeisIx' n = (oeis @193832) !! (n - 1)
--   oeis = concat (tabf @193832)

-- instance OEIS 193854 where
--   oeis = elemIndices 1 (oeis @62039)

-- instance OEIS 193890 where
--   oeis = filter f (oeis @107715) where
--      f n = (all ((== 1) . (oeisIx @10051)) $
--                  zipWith (\ins (t:tns) -> read $ (ins ++ x3 t ++ tns))
--                          (init $ inits $ show n) (init $ tails $ show n))
--          where x3 '0' = "0"
--                x3 '1' = "3"
--                x3 '2' = "6"
--                x3 '3' = "9"

-- instance OEIS 193891 where
--   oeis = tablList @193891
-- instance Table 193891 where
--   tabl = [1] : map fst (iterate
--      (\ (xs, i) -> (zipWith (+) (0:xs) [i, 2 * i ..], i + 1)) ([1,2], 2))

-- instance OEIS 193926 where
--   oeis = zipWith (-) (tail (oeis @62039)) (oeis @62039)

-- instance OEIS 193927 where
--   oeis = findIndices (< 0) (oeis @193926)

-- instance OEIS 193928 where
--   oeis = findIndices (0 <=) (oeis @193926)

-- instance OEIS 194005 where
--   oeis = tablList @194005
-- instance Table 194005 where
--   tabl = [1] : [1,1] : f [1] [1,1] where
--      f row' row = rs : f row rs where
--        rs = zipWith (+) ([0,1] ++ row') (row ++ [0])

-- instance OEIS 194020 where
--   oeisIx n = p (oeisIx n) n where
--      p _  0 = 1
--      p k m | m < k     = 0
--            | otherwise = p k (m - k) + p (k+1) m

-- instance OEIS 194081 where
--   oeis = map (fromJust . (`elemIndex` (oeis @5375))) [0..]

-- instance OEIS 194187 where
--   oeis = zipWith (-) (oeis @40) (oeis @70883)

-- instance OEIS 194189 where
--   oeisIx n = sum $ map (oeisIx @10051) [n* (n+1) `div` 2 + 1 .. n^2 - 1]

-- instance OEIS 194218 where
--   oeis = map fst kaprekarPairs
--   oeisIx n = (oeis @194219) !! (n - 1)
--   oeis = map snd kaprekarPairs
--   oeisIx n = (oeis @6886) !! (n - 1)
--   oeis = map (uncurry (+)) kaprekarPairs
--   kaprekarPairs = (1,0) : (mapMaybe (\n -> kSplit n $ splits (n^2)) [1..])
--      where kSplit x = find (\ (left, right) -> left + right == x)
--            splits q = no0 . map (divMod q) $ iterate (10 *) 10
--            no0 = takeWhile ((> 0) . fst) . filter ((> 0) . snd)

-- instance OEIS 194233 where
--   oeisIx n =
--      fromMaybe (10*n) $ find (== (oeisIx @4186) n) $ map (oeisIx @4186) [n+1..10*n]

-- instance OEIS 194597 where
--   oeisIx n = [1,6,6,1,9,3,1,3,9] !! (oeisIx @10878) (n - 1)

-- instance OEIS 194626 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @81827)

-- instance OEIS 194733 where
--   oeisIx n = genericLength $ filter (nTau <) $
--               map (snd . properFraction . (* tau) . fromInteger) [1..n]
--      where (_, nTau) = properFraction (tau * fromInteger n)
--            tau = (1 + sqrt 5) / 2

-- instance OEIS 195041 where
--   oeis = scanl (+) 0 (oeis @47336)

-- instance OEIS 195042 where
--   oeis = scanl (+) 0 (oeis @56020)

-- instance OEIS 195043 where
--   oeis = scanl (+) 0 (oeis @175885)

-- instance OEIS 195045 where
--   oeis = scanl (+) 0 (oeis @175886)

-- instance OEIS 195063 where
--   oeis = filter (\x -> (oeisIx @36044) x < x) [0,2..]

-- instance OEIS 195064 where
--   oeis = filter (\x -> (oeisIx @36044) x <= x) [0,2..]

-- instance OEIS 195065 where
--   oeis = filter (\x -> (oeisIx @36044) x > x) [0,2..]

-- instance OEIS 195066 where
--   oeis = filter (\x -> (oeisIx @36044) x >= x) [0,2..]

-- instance OEIS 195069 where
--   oeis = filter ((== 10) . (oeisIx @46660)) [1..]

-- instance OEIS 195085 where
--   oeis = map (+ 1) $ elemIndices 2 (oeis @57918)

-- instance OEIS 195086 where
--   oeis = filter ((== 2) . (oeisIx @46660)) [1..]

-- instance OEIS 195087 where
--   oeis = filter ((== 3) . (oeisIx @46660)) [1..]

-- instance OEIS 195088 where
--   oeis = filter ((== 4) . (oeisIx @46660)) [1..]

-- instance OEIS 195089 where
--   oeis = filter ((== 5) . (oeisIx @46660)) [1..]

-- instance OEIS 195090 where
--   oeis = filter ((== 6) . (oeisIx @46660)) [1..]

-- instance OEIS 195091 where
--   oeis = filter ((== 7) . (oeisIx @46660)) [1..]

-- instance OEIS 195092 where
--   oeis = filter ((== 8) . (oeisIx @46660)) [1..]

-- instance OEIS 195093 where
--   oeis = filter ((== 9) . (oeisIx @46660)) [1..]

-- instance OEIS 195106 where
--   oeis = filter (\x -> (oeisIx @6530) x - (oeisIx @20639) x == 4) [1,3..]

-- instance OEIS 195118 where
--   oeis = filter f [3,5..] where
--      f x = last pfs - head pfs == 6 where pfs = (rowT @27748) x

-- instance OEIS 195142 where
--   oeis = scanl (+) 0 (oeis @90771)

-- instance OEIS 195143 where
--   oeis = scanl (+) 0 (oeis @91998)

-- instance OEIS 195145 where
--   oeis = scanl (+) 0 (oeis @113801)

-- instance OEIS 195238 where
--   oeis = filter (\x -> (oeisIx @1221) x `elem` [2,3] &&
--                                (oeisIx @6530) x `elem` [5,7] &&
--                                (mod x 7 == 0 || mod x 15 == 0)) [1..]

-- instance OEIS 195324 where
--   oeis = filter p [2,4..] where
--      p n = all ((== 0) . (oeisIx @10051)) $ takeWhile (> 1) $ map (n -) (oeis @5385)

-- instance OEIS 195376 where
--   oeis = map (`mod` 2) (oeis @64413)

-- instance OEIS 195437 where
--   oeis = tablList @195437
-- instance Table 195437 where
--   tabl = tail $ g 1 1 [0..] where
--      g m j xs = (filter ((== m) . (`mod` 2)) ys) : g (1 - m) (j + 2) xs'
--        where (ys,xs') = splitAt j xs
--   b195437 = bFile' "A195437" (concat $ take 101 (tabl @195437)) 0

-- instance OEIS 195470 where
--   oeisIx n = genericLength $ filter ((== 0) . (`mod` n)) $
--                          take (fromInteger n) (oeis @51)

-- instance OEIS 195610 where
--   oeis = catMaybes $ map k [1..] where
--      k x = elemIndex 0 $ map (`mod` x) $ take (fromInteger x) (oeis @51)

-- instance OEIS 195758 where
--   oeisIx = (oeisIx @20639) . (oeisIx @16105)

-- instance OEIS 195759 where
--   oeisIx = (oeisIx @6530) . (oeisIx @16105)

-- instance OEIS 195942 where
--   oeis = filter (\x -> (oeisIx @10051) x == 0 && (oeisIx @10055) x == 1) (oeis @52382)

-- instance OEIS 195943 where
--   oeis = filter ((== 1) . (oeisIx @10055)) (oeis @52382)

-- instance OEIS 196046 where
--   oeis = 0 : g 2 where
--     g x = y : g (x + 1) where
--       y | t > 0     = max (oeisIx t) (oeisIx t + 1)
--         | otherwise = maximum [oeisIx r, (oeisIx @196046) s, (oeisIx @1222) r + (oeisIx @1222) s]
--         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

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

-- instance OEIS 196063 where
--   oeis = 0 : 1 : g 3 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196063) t * (oeisIx t + 1) `div` (oeisIx @1222) t
--          | otherwise = (oeisIx @196063) r * (oeisIx @196063) s * (oeisIx @1222) x `div`
--                        (oeisIx r * (oeisIx @1222) s)
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196068 where
--   oeis = 1 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @196068) t + (oeisIx @61775) t + 1
--          | otherwise = (oeisIx @196068) r + (oeisIx @196068) s - 1
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 196149 where
--   oeis = filter f [1..] where
--      f n = all (<= 0) $ zipWith (-) (tail divs) (map (* 3) divs)
--                         where divs = (rowT @27750)' n

-- instance OEIS 196175 where
--   oeis = map (+ 2) $ elemIndices True $
--      zipWith (\x y -> x < 0 && y > 0) (oeis @36263) $ tail (oeis @36263)

-- instance OEIS 196199 where
--   oeis = tablList @196199
-- instance Table 196199 where
--   rowCol n k = (rowT @196199) n !! k
--   tabf = map (rowT @196199) [0..]
--   rowT n = [-n..n]
--   b196199 = bFile' "A196199" (concat $ take 101 (tabf @196199)) 0

-- instance OEIS 196202 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 (p - 1) (p ^ 2) where p = (oeisIx @40) n

-- instance OEIS 196276 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @196274)

-- instance OEIS 196277 where
--   oeis = map (+ 1) $ findIndices (> 1) (oeis @196274)



-- instance OEIS 196415 where
--   oeis =
--      map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @36691) (oeis @53767)

-- instance OEIS 196486 where
--   oeis = tablList @196486
-- instance Table 196486 where
--   rowCol = rowCol_off @196486 @1 @1
--   rowT   = rowT_off @196486 @1
--   tabf = map (tail . reverse) $ tail (tabf @227048)

-- instance OEIS 196526 where
--   oeisIx n = genericLength [c | let p = (oeisIx @40) n,
--                           c <- [-1,1..p-1], let b = p - c,
--                           gcd b c == 1,
--                           (oeisIx @6530) b ^ 2 < p || p == 3, (oeisIx @6530) c ^ 2 < p]

-- instance OEIS 196871 where
--   oeis = filter
--      (all (== 0) . map (oeisIx @10051) . takeWhile (> 2) . iterate (oeisIx @6370)) [1..]

-- instance OEIS 197182 where
--   oeisIx = (oeisIx @64986) . (oeisIx @290)

-- instance OEIS 197183 where
--   oeisIx = (oeisIx @115944) . (oeisIx @290)

-- instance OEIS 197704 where
--   base_weight b g n | n == 0 = 0 | otherwise = (base_weight b g (n `div` b)) + (g $ n `mod` b)
--   interesting b g = filter f [1..] where f n = n `mod` (base_weight b g n) == 0
--   bin_interesting g = interesting 2 g
--   weights l n | (n >=0) && ((length l) > fromInteger n) = l !! fromInteger n | otherwise = 0
--   original = weights [4,3]
--   let a = bin_interesting original

-- instance OEIS 197863 where
--   oeisIx n = product $
--      zipWith (^) (oeisIx_row n) (map (max 2) $ (rowT @124010) n)

-- instance OEIS 197877 where
--   oeis = map (fromJust . (`elemIndex` (oeis @96535))) [0..]

-- instance OEIS 197911 where
--   oeis = scanl (+) 0 (oeis @56832)

-- instance OEIS 197945 where
--   oeisIx n = genericLength $ takeWhile (`isPrefixOf` (show $ (oeisIx @96095) $ n+1)) $
--                                  tail $ inits $ show $ (oeisIx @96095) n

-- instance OEIS 198069 where
--   oeis = tablList @198069
-- instance Table 198069 where
--   tabf = [0] : iterate f [1, 1] where
--      f (x:xs) = ys ++ tail (reverse ys) where ys = (2 * x) : xs

-- instance OEIS 198259 where
--   oeisIx n = sum $ map (oeisIx . (mod n)) [1..n]

-- instance OEIS 198273 where
--   oeis = map (oeisIx @40) $ filter ((== 0) . (oeisIx @67432)) [1..]

-- instance OEIS 198277 where
--   oeisIx n = (oeisIx @40) . (+ 1) . fromJust $ elemIndex n (oeis @67432)

-- instance OEIS 198328 where
--   oeis = 1 : 1 : g 3 where
--      g x = y : g (x + 1) where
--        y = if t > 0 then (oeisIx @40) (oeisIx t) else (oeisIx @198328) r * (oeisIx @198328) s
--            where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 198332 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @198332) t + 2 * (oeisIx @1222) t
--          | otherwise = (oeisIx @198332) r + (oeisIx @198332) s + 2 * (oeisIx @1222) r * (oeisIx @1222) s
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 198384 where
--   oeis = map (^ 2) (oeis @198388)

-- instance OEIS 198385 where
--   oeis = map (^ 2) (oeis @198389)

-- instance OEIS 198386 where
--   oeis = map (^ 2) (oeis @198390)

-- instance OEIS 198387 where
--   oeis = zipWith (-) (oeis @198385) (oeis @198384)

-- instance OEIS 198409 where
--   oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @8966) $
--      zipWith gcd (oeis @198384) $ zipWith gcd (oeis @198385) (oeis @198386)

-- instance OEIS 198435 where
--   oeis = map (oeisIx @198384) (oeis @198409)

-- instance OEIS 198436 where
--   oeis = map (oeisIx @198385) (oeis @198409)

-- instance OEIS 198437 where
--   oeis = map (oeisIx @198386) (oeis @198409)

-- instance OEIS 198438 where
--   oeis = map (oeisIx @198387) (oeis @198409)

-- instance OEIS 198439 where
--   oeis = map (oeisIx @198388) (oeis @198409)

-- instance OEIS 198440 where
--   oeis = map (oeisIx @198389) (oeis @198409)

-- instance OEIS 198441 where
--   oeis = map (oeisIx @198390) (oeis @198409)

-- instance OEIS 198772 where
--   oeis = filter ((== 1) . (oeisIx @88534)) (oeis @3136)

-- instance OEIS 198773 where
--   oeis = filter ((== 2) . (oeisIx @88534)) (oeis @3136)

-- instance OEIS 198774 where
--   oeis = filter ((== 3) . (oeisIx @88534)) (oeis @3136)

-- instance OEIS 198775 where
--   oeis = filter ((== 4) . (oeisIx @88534)) (oeis @3136)

-- instance OEIS 198799 where
--   oeisIx n = fromJust $ elemIndex n (oeis @88534)

-- instance OEIS 199016 where
--   199016 = p (oeis @2858) where
--      p _ 0 = 1
--      p us'@ (u:us) m | m < u     = 0
--                     | otherwise = p us' (m - u) + p us m

-- instance OEIS 199017 where
--   oeisIx = p (oeis @2858) where
--      p _  0 = 1
--      p (u:us) m | m < u = 0
--                 | otherwise = p us (m - u) + p us m

-- instance OEIS 199045 where
--   oeisIx n = head $
--      filter ((<= 2) . (oeisIx @54055)) $ map (* 2^n) [oeisIx (n - 1)..]

-- instance OEIS 199118 where
--   oeisIx = p (oeis @2859) where
--      p _ 0 = 1
--      p us'@ (u:us) m | m < u     = 0
--                     | otherwise = p us' (m - u) + p us m

-- instance OEIS 199119 where
--   oeisIx = p (oeis @2859) where
--      p _  0 = 1
--      p (u:us) m | m < u = 0
--                 | otherwise = p us (m - u) + p us m

-- instance OEIS 199120 where
--   oeisIx = p (oeis @3666) where
--      p _ 0 = 1
--      p us'@ (u:us) m | m < u     = 0
--                     | otherwise = p us' (m - u) + p us m

-- instance OEIS 199121 where
--   oeisIx = p (oeis @3666) where
--      p _  0 = 1
--      p (u:us) m | m < u =
--                 | otherwise = p us (m - u) + p us m

-- instance OEIS 199122 where
--   oeisIx = p (oeis @1857) where
--      p _ 0 = 1
--      p us'@ (u:us) m | m < u     = 0
--                     | otherwise = p us' (m - u) + p us m

-- instance OEIS 199123 where
--   oeisIx = p (oeis @1857) where
--      p _  0 = 1
--      p (u:us) m | m < u = 0
--                 | otherwise = p us (m - u) + p us m

-- instance OEIS 199162 where
--   oeis = 1 : 6 : ulam 2 6 (oeis @199162)

-- instance OEIS 199238 where
--   oeis = zipWith mod [1..] $ tail (oeis @120)

-- instance OEIS 199262 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @199238)) + 1

-- instance OEIS 199332 where
--   oeis = tablList @199332
--   rowCol = rowCol_off @199332 @1 @1
--   rowT   = rowT_off   @199332 @1
--   oeis = concat (tabl @199332)
--   tabl = f [1..] [1..] where
--      f (x:xs) ys'@ (y:ys) | odd x  = (replicate x y) : f xs ys
--                          | even x = us : f xs vs
--                          where (us,vs) = splitAt x ys'

-- instance OEIS 199333 where
--   oeis = tablList @199333
-- instance Table 199333 where
--   oeis = concat (tabl @199333)
--   tabl = iterate
--      (\row -> map (oeisIx @159477) $ zipWith (+) ([0] ++ row) (row ++ [0])) [1]

-- instance OEIS 199424 where
--   oeisIx n = fromJust $ findIndex (elem $ (oeisIx @40) n) (tabl @199333)

-- instance OEIS 199425 where
--   oeis = f [] (tabl @199333) where
--      f ps (ts:tts) =  (length ps') : f ps' tts where
--        ps' = ps `union` (take ((length ts - 1) `div` 2) $ tail ts)

-- instance OEIS 199581 where
--   oeisIx n = (rowT @199333) (2*n) !! n

-- instance OEIS 199582 where
--   oeisIx n = (rowT @199333) n !! (n `div` 2)

-- instance OEIS 199694 where
--   oeisIx = sum . (rowT @199333)

-- instance OEIS 199695 where
--   oeisIx = product . (rowT @199333)

-- instance OEIS 199696 where
--   oeisIx n = product . (take (n `div` 2 + 1)) $ (rowT @199333) n

-- instance OEIS 199713 where
--   oeisIx n = f ps where
--      f (q:qs) = if sort (show q) `contains` sort (show p) then q else f qs
--      contains _  []                         = True
--      contains [] _                          = False
--      contains (u:us) vs'@ (v:vs) | u == v    = contains us vs
--                                 | otherwise = contains us vs'
--      p : ps = drop (n - 1) (oeis @40)

-- instance OEIS 199745 where
--   oeis = filter (\x -> 2 * (oeisIx x) == (oeisIx @8472) x) [1..]

-- instance OEIS 199770 where
--   oeis = 1 : f [1] where
--      f xs = y : f (y : xs) where
--        y = sum $ zipWith xor xs $ reverse xs :: Integer

-- instance OEIS 199771 where
--   oeisIx  = sum . (rowT @199332)

-- instance OEIS 199921 where
--   oeis = map length $ group $ sort $ map (oeisIx . (oeisIx @61493)) [1..3999]

-- instance OEIS 199968 where
--   oeisIx = head . (rowT @173540)

-- instance OEIS 200069 where
--   oeis = 1 : 4 : zipWith (+)
--      (map (* 4) $ tail (oeis @200069)) (map (* 13) (oeis @200069))

-- instance OEIS 200087 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @79878)) + 1

-- instance OEIS 200379 where
--   oeisIx n = (tabl @56230) !! n !! (n - 1)

-- instance OEIS 200612 where
--   oeis = filter f [2..] where
--      f x = r == 0 && x' == 3 where (x',r) = divMod (oeisIx x) (oeisIx x)

-- instance OEIS 200723 where
--   oeisIx = sum . zipWith (*) [1..] . map (oeisIx @63524) . (rowT @165430)

-- instance OEIS 200737 where
--   oeis = tablList @200737
-- instance Table 200737 where
--   rowCol = rowCol_off @200737 @1 @1
--   rowT n = sort
--      [v*w + w*u + u*v | w <- [1..n], v <- [1..w], u <- [1..v]]
--   tabl = map (rowT @200737) [1..]

-- instance OEIS 200738 where
--   oeis = f (tabl @200737) where
--      f (rs:rss'@ (rs':rss)) =
--        (length $ takeWhile (== EQ) $ zipWith compare rs rs') : f rss'

-- instance OEIS 200741 where
--   oeis = tablList @200741
-- instance Table 200741 where
--   rowCol = rowCol_off @200741 @1 @1
--   rowT = nub . (rowT @200737)
--   tabl = map (rowT @200741) [1..]

-- instance OEIS 200742 where
--   oeis = f (tabl @200741) where
--      f (rs:rss'@ (rs':rss)) =
--        (length $ takeWhile (== EQ) $ zipWith compare rs rs') : f rss'

-- instance OEIS 200996 where
--   oeisIx n = max (oeisIx n) (oeisIx n)

-- instance OEIS 201009 where
--   oeisIx = (oeis @201009)
--   oeis = 1 : filter
--      (\x -> (rowT @27748) x == (rowT @27748) (oeisIx x)) [2..]

-- instance OEIS 201053 where
--   oeis = 0 : concatMap (\x -> genericReplicate (oeisIx @56107 x) (x ^ 3)) [1..]

-- instance OEIS 201217 where
--   oeis = elemIndices 0 (oeis @61023)

-- instance OEIS 201462 where
--   oeis = [x | x <- [1..], gcd x (oeisIx @61601 x) > 1]

-- instance OEIS 201651 where
--   oeisIx :: Integer -> Integer -> Integer
--   oeisIx n 0 = n
--   oeisIx n k = (oeisIx @54240) (n `xor` k) (shift (n .&. k) 2)
--   oeisIx_row n = map (oeisIx n) [0..n]
--   oeisIx_tabl = map (rowT @201651) [0..]

-- instance OEIS 202014 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @63882)) + 1

-- instance OEIS 202016 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @132157)

-- instance OEIS 202018 where
--   oeisIx = (+ 41) . (oeisIx @2378)

-- instance OEIS 202022 where
--   oeisIx = fromEnum . (== 1) . (oeisIx @43537)

-- instance OEIS 202089 where
--   oeis = elemIndices 0 (oeis @240752)

-- instance OEIS 202262 where
--   oeis = [4,6,8,9] ++ [x | u <- (oeis @202262), v <- [4,6,8,9],
--                          let x = 10 * u + v, v /= 9 || (oeisIx @10051) x == 0]

-- instance OEIS 202337 where
--   oeis = f (oeis @62723) where
--      f (x:xs'@ (x':xs)) = if x == x' then f xs' else x : f xs'

-- instance OEIS 202340 where
--   oeis = map length $ group (oeis @5374)

-- instance OEIS 202341 where
--   oeis = elemIndices 1 (oeis @202340)

-- instance OEIS 202342 where
--   oeis = elemIndices 2 (oeis @202340)

-- instance OEIS 202387 where
--   oeis = [x | x <- (oeis @120944),
--                       oeisIx x == sum (map (oeisIx @7953) (oeisIx_row x))]

-- instance OEIS 202822 where
--   oeis = filter ((== 1) . flip mod 3) (oeis @3136)

-- instance OEIS 203069 where
--   oeis = 1 : f 1 [2..] where
--      f u vs = g vs where
--        g (w:ws) | odd z && (oeisIx @10051)' z == 0 = w : f w (delete w vs)
--                 | otherwise = g ws
--                 where z = u + w

-- instance OEIS 203814 where
--   oeisIx n = genericLength [x | x <- [0..n], (oeisIx @43537) x == (oeisIx @43537) n]

-- instance OEIS 203908 where
--   oeisIx n = product $ map abs $
--               zipWith (-) (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 203967 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @9087)

-- instance OEIS 203976 where
--   oeis = 0 : 1 : 5 : 4 : zipWith (-)
--      (map (* 3) $ drop 2 (oeis @203976)) (oeis @203976)

-- instance OEIS 204093 where
--   oeis = map (* 6) (oeis @7088)

-- instance OEIS 204094 where
--   oeis = map (* 7) (oeis @7088)

-- instance OEIS 204095 where
--   oeis = map (* 8) (oeis @7088)

-- instance OEIS 204138 where
--   oeis = filter ((== 1) . (oeisIx @10051)) $ drop 6 (oeis @1945)

-- instance OEIS 204200 where
--   oeis = 1 : 1 : 2 : zipWith (+) (oeis @204200) (tail $ zipWith (-)
--      (map (* 4) (tail (oeis @204200))) (map (* 3) (oeis @204200)))

-- instance OEIS 204293 where
--   oeis = tablList @204293
-- instance Table 204293 where
--   tabl = [1] : [0,0] : f [1] [0,0] where
--      f xs ys = xs' : f ys xs' where
--        xs' = zipWith (+) ([0,0] ++ xs) (xs ++ [0,0])

-- instance OEIS 204389 where
--   oeisIx = p (oeis @2808) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 204515 where
--   oeisIx n = (oeisIx @247500) (2 * n) n

-- instance OEIS 204556 where
--   oeisIx = head . (rowT @45975)

-- instance OEIS 204557 where
--   oeisIx = last . (rowT @45975)

-- instance OEIS 204558 where
--   oeisIx = sum . (rowT @45975)

-- instance OEIS 204878 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @97796)

-- instance OEIS 204879 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @97796)

-- instance OEIS 205216 where
--   oeisIx = p $ tail (oeis @5836) where
--      p _ 0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 205217 where
--   oeisIx = p $ tail (oeis @5836) where
--      p _ 0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 205565 where
--   oeisIx n = sum $ map (oeisIx . (n -)) $
--                     takeWhile (<= n `div` 2) (oeis @5836)

-- instance OEIS 205666 where
--   oeis = [x | x <- [1..], (oeisIx @65306) x == fi x]

-- instance OEIS 205667 where
--   oeis = filter ((== 1) . (oeisIx @39997)) [0..]

-- instance OEIS 205745 where
--   oeisIx n = sum $ map ((`mod` 2) . (n `div`))
--      [p | p <- takeWhile (<= n) (oeis @40), n `mod` p == 0]

-- instance OEIS 205956 where
--   oeis = sort $ filter ((== 1) . (oeisIx @10051)) $
--                         nub $ map read (tail $ subsequences "39467139")

-- instance OEIS 205959 where
--   oeisIx n = product $ map (div n) $ (rowT @27748) n

-- instance OEIS 206159 where
--   oeis = filter ((<= 2) . (oeisIx @95048)) [1..]

-- instance OEIS 206244 where
--   oeisIx = p $ tail (oeis @2275) where
--      p _             0 = 1
--      p rus'@ (ru:rus) n = if n < ru then 0 else p rus' (n - ru) + p rus n

-- instance OEIS 206245 where
--   oeisIx = p (oeis @83278) where
--      p _      0 = 1
--      p rps'@ (rp:rps) n = if n < rp then 0 else p rps' (n - rp) + p rps n

-- instance OEIS 206282 where
--   oeis = 1 : 1 : -1 : -4 :
--      zipWith div
--        (zipWith (+)
--          (zipWith (*) (drop 3 (oeis @206282))
--                       (drop 1 (oeis @206282)))
--          (drop 2 (oeis @206282)))
--        (oeis @206282)

-- instance OEIS 206332 where
--   oeis = compl [1..] (oeis @92754) where
--      compl (u:us) vs'@ (v:vs) | u == v = compl us vs
--                              | u /= v = u : compl us vs'

-- instance OEIS 206351 where
--   oeis = 1 : 3 : map (subtract 4)
--                  (zipWith (-) (map (* 7) (tail (oeis @206351))) (oeis @206351))

-- instance OEIS 206368 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @206475)

-- instance OEIS 206369 where
--   oeisIx n = product $
--      zipWith h (oeisIx_row n) (map toInteger $ (rowT @124010) n) where
--              h p e = sum $ take (fromInteger e + 1) $
--                            iterate ((* p) . negate) (1 - 2 * (e `mod` 2))

-- instance OEIS 206462 where
--   oeis = map (oeisIx . (+ 1)) $
--                      elemIndices 1 $ map (oeisIx @8966) (oeis @1043)

-- instance OEIS 206475 where
--   oeis = zipWith (-) (tail (oeis @206369)) (oeis @206369)

-- instance OEIS 206497 where
--   oeis = 1 : g 2 where
--     g x = y : g (x + 1) where
--       y | t > 0     = (oeisIx @206497) t
--         | otherwise = product $ zipWith (\p e -> (oeisIx @142) e * (oeisIx @206497) p ^ e)
--                                         (oeisIx_row x) (oeisIx_row x)
--         where t = (oeisIx @49084) x

-- instance OEIS 206498 where
--   oeisIx 1 = 0
--   oeisIx 2 = 2
--   oeisIx x = if t > 0 then (oeisIx @196062) t + t `mod` 2 else (oeisIx @196062) x
--               where t = (oeisIx @49084) x

-- instance OEIS 206553 where
--   oeisIx n = head [p | p <- drop 2 (oeis @40),
--                         (oeisIx @10051) (2^n + p*2^ (div (n+1) 2) - 1) == 1]

-- instance OEIS 206702 where
--   import Control.Monad
--   --this creates the powerset of a set
--   ps n = filterM (\x->[True,False]) n
--   --given a set z, this creates the set X of (a+b) for all a, b, in Z
--   addset z = do x<-z
--                 y<-z
--                 [x+y]
--   --this check if two sets are disjoint
--   disjoint a [] = True
--   disjoint a (c:d) = (disjoint a d) && ((filter (\x->x==c) a) ==[])
--   --this checks if a set z is disjoint from its "adsset" in a certain Zn, n being the second argument.
--   good z n = disjoint z (map (\x->rem x n) (addset z))
--   --this generates all off Zn's subsets with the required property.
--   sets n = filter (\x ->good x n) (ps [0.. (n - 1)])
--   --this generates the first n terms of the sequence
--   sequence n = map (\x->length (sets x) ) [1..n]

-- instance OEIS 206787 where
--   oeisIx = sum . filter odd . (rowT @206778)

-- instance OEIS 206925 where
--   import Data.Map (fromList, (!), insert)
--   oeisIx n = (oeis @206925) !! (n - 1)
--   oeis = 1 : f [0, 1] (fromList [ (Bin [0], 1), (Bin [1], 1)]) where
--      f bs'@ (b:bs) m = y : f (succ bs') (insert (Bin bs') y m) where
--        y = m ! (Bin bs) +
--            length (filter (\ds -> ds == reverse ds) $ tail $ inits bs')
--        succ [] = [1]; succ (0:ds) = 1 : ds; succ (1:ds) = 0 : succ ds

-- instance OEIS 206941 where
--   oeisIx = (oeisIx @10) . (oeisIx @2322)

-- instance OEIS 207193 where
--   oeisIx 1 = 1
--   oeisIx n | p == 2 && e > 2 = 2 ^ (e - 2)
--             | otherwise       = (p - 1) * p ^ (e - 1)
--             where p = (oeisIx @25473) n; e = (oeisIx @25474) n

-- instance OEIS 207337 where
--   oeis = f (oeis @2522) where
--      f (x:xs) | m == 0 && (oeisIx @10051) y == 1 = y : f xs
--               | otherwise                = f xs
--               where (y,m) = divMod x 10

-- instance OEIS 207432 where
--   oeisIx n = (fromJust $ elemIndex (oeisIx n) (oeis @66680)) + 1

-- instance OEIS 207481 where
--   oeis = [x | x <- [1..], and $ zipWith (<=)
--                       (map toInteger $ (rowT @124010) x) (oeisIx_row x) ]

-- instance OEIS 207674 where
--   oeis = filter
--      (\x -> (rowT @27750) x `intersect` (rowT @70165) x == (rowT @27750) x) [1..]

-- instance OEIS 207675 where
--   oeis = filter
--      (\x -> (rowT @27750) x `intersect` (rowT @70165) x /= (rowT @27750) x) [1..]

-- instance OEIS 207852 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @178830)) + 1

-- instance OEIS 207954 where
--   oeis = filter ((== 1) . (oeisIx @136522)) (oeis @33075)

-- instance OEIS 208083 where
--   oeisIx = sum . map (oeisIx @10051) . (rowT @81118)

-- instance OEIS 208091 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @208083))

-- instance OEIS 208101 where
--   oeis = tablList @208101
-- instance Table 208101 where
--   tabl =  iterate
--      (\row -> zipWith (+) ([0,1] ++ init row) (row ++ [0])) [1]

-- instance OEIS 208131 where
--   oeis = scanl (*) 1 $ (oeis @52901)

-- instance OEIS 208134 where
--   oeisIx = genericLength . filter (== 0) . (rowT @8975)

-- instance OEIS 208177 where
--   oeis = filter ((== 1) . (oeisIx @10051)) [1,129..]

-- instance OEIS 208178 where
--   oeis = filter ((== 1) . (oeisIx @10051)) [1,257..]

-- instance OEIS 208238 where
--   oeisIx = genericIndex (oeis @208238)
--   oeis = f nns $ filter ((== 1) . (oeisIx @10051)' . fst) nns where
--      f mms'@ ((m,ms):mms) pps'@ ((p,ps):pps) =
--        if m == p then f mms' pps else q : f mms pps'
--        where q = fst $ fromJust $ find ((ms `isInfixOf`) . snd) pps'
--      nns = zip [0..] (tabf @30308)

-- instance OEIS 208239 where
--   oeis = tablList @208239
-- instance Table 208239 where
--   rowCol n k = (rowT @208239) n !! k
--   rowT n = map (+ n) $ zipWith (-) divs $ reverse divs
--                   where divs = (rowT @27750) n
--   tabl = map (rowT @208239) [1..]

-- instance OEIS 208245 where
--   oeis = tablList @208245
-- instance Table 208245 where
--   rowCol = rowCol_off @208245 @1 @1
--   rowT   = rowT_off   @208245 @1
--   tabl = map fst $ iterate f ([1], [1, 1]) where
--      f (us, vs) = (vs, zipWith (+) ([0] ++ us ++ [0]) (us ++ [0, 1]))

-- instance OEIS 208247 where
--   oeisIx n = (oeis @95841) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @71330)) (oeis @961)

-- instance OEIS 208259 where
--   oeis = 1 : map ((+ 1) . (* 10)) (oeis @131835)

-- instance OEIS 208260 where
--   oeis = filter ((== 0) . (oeisIx @10051)') (oeis @208259)

-- instance OEIS 208278 where
--   oeisIx = sum . (rowT @8975)

-- instance OEIS 208279 where
--   oeisIx n = (oeisIx @8975) (2*n) n

-- instance OEIS 208280 where
--   oeisIx = genericLength . nub . (rowT @8975)

-- instance OEIS 208355 where
--   oeisIx n = (rowCol @208101) n n
--   oeis = map last (tabl @208101)

-- instance OEIS 208448 where
--   oeis = zipWith gcd (oeis @10786) $ tail (oeis @10786)

-- instance OEIS 208449 where
--   oeis = map numerator $
--      zipWith (%) (tail (oeis @10786)) (oeis @10786)

-- instance OEIS 208450 where
--   oeis = map denominator $
--      zipWith (%) (tail (oeis @10786)) (oeis @10786)

-- instance OEIS 208570 where
--   oeisIx n = lcm n $ (oeisIx @7978) n

-- instance OEIS 208662 where
--   oeisIx n = head [m | m <- [1..], let p = (oeisIx @65091) n,
--      let q = 2 * m - p, (oeisIx @10051)' q == 1,
--      all ((== 0) . (oeisIx @10051)') $ map (2 * m -) $ take (n - 1) (oeis @65091)]

-- instance OEIS 208768 where
--   oeis = nub (oeis @70198)

-- instance OEIS 208852 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @90895))

-- instance OEIS 208976 where
--   oeis = map (subtract 1) $ tail (oeis @50168)

-- instance OEIS 208981 where
--   oeisIx = genericLength . takeWhile ((== 0) . (oeisIx @209229)) . (rowT @70165)

-- instance OEIS 208983 where
--   oeisIx n = (oeisIx @208101) (2 * n) n

-- instance OEIS 209061 where
--   oeis = filter
--      (all (== 1) . map (oeisIx . fi) . (rowT @124010)) [1..]

-- instance OEIS 209211 where
--   oeis = filter (\x -> (x - 1) `gcd` (oeisIx @10) x == 1) [1..]

-- instance OEIS 209297 where
--   oeis = tablList @209297
-- instance Table 209297 where
--   rowCol n k = k * n + k - n
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @209297) [1..]

-- instance OEIS 209561 where
--   oeis = tablList @209561
-- instance Table 209561 where
--   rowCol = rowCol_off @209561 @1 @1
--   rowT   = rowT_off   @209561 @1
--   tabl = [1] : iterate
--                  (\row -> zipWith (+) ([1] ++ row) (row ++ [0])) [1,1]

-- instance OEIS 210108 where
--   oeis = tablList @210108
-- instance Table 210108 where
--   tabl = zipWith take [1..] (tabf @8301)

-- instance OEIS 210111 where
--   oeis = tablList @210111
-- instance Table 210111 where
--   tabl = zipWith take [1..] (tabf @125053)

-- instance OEIS 210442 where
--   oeisIx n = p (oeisIx_row n) n where
--      p _          0 = 1
--      p []         _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 210454 where
--   oeisIx = (`div` 3) . (subtract 1) . (4 ^) . (oeisIx @40) . (+ 2)

-- instance OEIS 210455 where
--   oeisIx n = fromEnum $ p (oeisIx_row n) n where
--      p ks m = m == 0 || not (null ks) && head ks <= m &&
--               (p (tail ks) (m - head ks) || p (tail ks) m)

-- instance OEIS 210481 where
--   oeisIx n = sum [oeisIx' $ p * q - 2 |
--                    let p = (oeisIx @40) n, q <- takeWhile (< p) (oeis @40)]

-- instance OEIS 210490 where
--   oeis = filter chi [1..] where
--      chi x = all (== 1) es || all even es where es = (rowT @124010) x

-- instance OEIS 210494 where
--   oeis = filter
--      (\x -> (oeisIx x + (oeisIx @38040) x) `mod` (oeisIx @74400) x == 0) [1..]

-- instance OEIS 210582 where
--   oeis = filter (\x -> mod x (oeisIx x) == (oeisIx @30) x) (oeis @67251)

-- instance OEIS 210719 where
--   oeis = f (zip [1..] (oeis @10)) [] where
--      f ((i,x):ixs) phis | x `elem` phis = f ixs phis
--                         | otherwise     = i : f ixs (x : phis)

-- instance OEIS 210757 where
--   oeis = sortBy (compare `on` show) $
--                         takeWhile (<= 10^4) (oeis @40)

-- instance OEIS 210758 where
--   oeis = sortBy (compare `on` show) $
--                         takeWhile (<= 1000) (oeis @40)

-- instance OEIS 210759 where
--   oeis = sortBy (compare `on` show) $
--                         takeWhile (<= 10^4) (oeis @40)

-- instance OEIS 210760 where
--   oeis = sortBy (compare `on` show) $
--                         takeWhile (<= 10^5) (oeis @40)

-- instance OEIS 210761 where
--   Data.List (sortBy)
--   oeisIx n = (oeis @210761) !! (n - 1)
--   oeis = sortBy (compare `on` show) $
--                         takeWhile (<= 10^6) (oeis @40)

-- instance OEIS 210771 where
--   oeisIx n = fromJust (elemIndex n (oeis @210770)) + 1

-- instance OEIS 211005 where
--   oeis = map length $ group (oeis @69754)

-- instance OEIS 211110 where
--   oeisIx n = p (tail $ (rowT @27750) n) n where
--      p _      0 = 1
--      p []     _ = 0
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 211111 where
--   oeisIx n = p (tail $ (rowT @27750) n) n where
--      p _  0 = 1
--      p [] _ = 0
--      p (k:ks) m | m < k     = 0
--                  | otherwise = p ks (m - k) + p ks m

-- instance OEIS 211201 where
--   oeisIx = fromJust . (`elemIndex` (oeis @50493))

-- instance OEIS 211223 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @211225)

-- instance OEIS 211225 where
--   oeisIx n = genericLength $ filter (== (oeisIx @203) n) $ zipWith (+) us' vs where
--      (us,vs@ (v:_)) = splitAt (fromInteger $ (n - 1) `div` 2) (oeis @203)
--      us' = if even n then v : reverse us else reverse us

-- instance OEIS 211316 where
--   oeisIx n | not $ null ps = n * (head ps + 1) `div` (3 * head ps)
--             | m == 0        = n'
--             | otherwise     = (n - 1) `div` 3
--             where ps = [p | p <- (rowT @27748) n, mod p 3 == 2]
--                   (n',m) = divMod n 3

-- instance OEIS 211396 where
--   oeisIx n = if null ips then 0 else head ips
--      where ips = [p | p <- takeWhile (<= n) (oeis @40),
--                       show p `isInfixOf` show n]

-- instance OEIS 211858 where
--   oeisIx n = p 0 [] [1..3] n where
--      p m ms _      0 = if m `elem` ms then 0 else 1
--      p _ _  []     _ = 0
--      p m ms ks'@ (k:ks) x
--        | x < k       = 0
--        | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
--        | m `elem` ms = p (m + 1) ms ks' (x - k)
--        | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

-- instance OEIS 211859 where
--   oeisIx n = p 0 [] [1..4] n where
--      p m ms _      0 = if m `elem` ms then 0 else 1
--      p _ _  []     _ = 0
--      p m ms ks'@ (k:ks) x
--        | x < k       = 0
--        | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
--        | m `elem` ms = p (m + 1) ms ks' (x - k)
--        | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

-- instance OEIS 211860 where
--   oeisIx n = p 0 [] [1..5] n where
--      p m ms _      0 = if m `elem` ms then 0 else 1
--      p _ _  []     _ = 0
--      p m ms ks'@ (k:ks) x
--        | x < k       = 0
--        | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
--        | m `elem` ms = p (m + 1) ms ks' (x - k)
--        | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

-- instance OEIS 211861 where
--   oeisIx n = p 0 [] [1..6] n where
--      p m ms _      0 = if m `elem` ms then 0 else 1
--      p _ _  []     _ = 0
--      p m ms ks'@ (k:ks) x
--        | x < k       = 0
--        | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
--        | m `elem` ms = p (m + 1) ms ks' (x - k)
--        | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

-- instance OEIS 211862 where
--   oeisIx n = p 0 [] [1..7] n where
--      p m ms _      0 = if m `elem` ms then 0 else 1
--      p _ _  []     _ = 0
--      p m ms ks'@ (k:ks) x
--        | x < k       = 0
--        | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
--        | m `elem` ms = p (m + 1) ms ks' (x - k)
--        | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

-- instance OEIS 211863 where
--   oeisIx n = p 0 [] [1..8] n where
--      p m ms _      0 = if m `elem` ms then 0 else 1
--      p _ _  []     _ = 0
--      p m ms ks'@ (k:ks) x
--        | x < k       = 0
--        | m == 0      = p 1 ms ks' (x - k) + p 0 ms ks x
--        | m `elem` ms = p (m + 1) ms ks' (x - k)
--        | otherwise   = p (m + 1) ms ks' (x - k) + p 0 (m : ms) ks x

-- instance OEIS 211868 where
--   oeisIx n = f (oeis @5408) 1 nn 0 where
--      f (o:os) l nl xx
--        | yy > nl   = 0
--        | yy < nl   = f os (l + 1) (nl + nn) yy + f os l nl xx
--        | otherwise = if w == n then 1 else 0
--        where w = if r == 0 then (oeisIx @196) m else 0
--              (m, r) = divMod yy l
--              yy = xx + o * o
--      nn = n ^ 2

-- instance OEIS 211889 where
--   oeisIx n = head [k | let p = (oeisIx @40) n, k <- [1..],
--               all ((== 1) . (oeisIx @10051)') $ map ((+ p) . (* k)) (oeisIx_row n)]

-- instance OEIS 211890 where
--   oeis = tablList @211890
-- instance Table 211890 where
--   tabl = zipWith3 (\p k row -> map ((+ p) . (* k)) row)
--                           (oeis @8578) (0 : (oeis @211889)) (tabl @2262)

-- instance OEIS 211996 where
--   oeisIx n = genericLength [x | x <- [1..n], let (y, m) = divMod n x,
--                           m == 0, (oeisIx @10052) (x + y) == 1]

-- instance OEIS 212133 where
--   oeisIx n = if n == 0 then 0 else (oeisIx n + 1) `div` 2

-- instance OEIS 212164 where
--   oeis = map (+ 1) $ findIndices (< 0) (oeis @225230)

-- instance OEIS 212165 where
--   oeis = map (+ 1) $ findIndices (<= 0) (oeis @225230)

-- instance OEIS 212166 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @225230)

-- instance OEIS 212167 where
--   oeis = map (+ 1) $ findIndices (>= 0) (oeis @225230)

-- instance OEIS 212168 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @225230)

-- instance OEIS 212177 where
--   oeis = filter (> 0) (oeis @56170)

-- instance OEIS 212190 where
--   oeis = filter ((== 1) . (oeisIx @10052)) (oeis @14311)

-- instance OEIS 212191 where
--   oeis = map (oeisIx @196) (oeis @212190)

-- instance OEIS 212192 where
--   oeis = filter ((== 1) . (oeisIx @10054)) (oeis @14311)

-- instance OEIS 212210 where
--   oeis = tablList @212210
-- instance Table 212210 where
--   rowCol = rowCol_off @212210 @1 @1
--   rowT   = rowT_off   @212210 @1
--   tabl = f $ tail $ zip (inits pis) (tails pis) where
--      f ((xs,ys) : zss) = (zipWith (-) (map (+ last xs) (xs)) ys) : f zss
--      pis = (oeis @720)

-- instance OEIS 212211 where
--   oeis = tablList @212211
-- instance Table 212211 where
--   rowCol = rowCol_off @212211 @2 @2
--   tabl = map (rowT @212211) [2..]
--   rowT n = zipWith (-)
--      (map (+ (oeisIx @720) n) $ take (n - 1) $ tail (oeis @720))
--      (drop (n + 1) (oeis @720))

-- instance OEIS 212300 where
--   oeis = f 1 (2 : (oeis @40)) [1] $ tail (oeis @6530) where
--      f x ps'@ (_ : ps@ (_ : p : _)) gpfs (q : qs) =
--        y : f (x + 1) (if y == p then ps else ps') (q : gpfs) qs where
--        y = head [z | z <- ps', length (filter (> z) gpfs) <= div x 2]

-- instance OEIS 212412 where
--   oeisIx = (`mod` 2) . (oeisIx @181935)

-- instance OEIS 212439 where
--   oeisIx n = 2 * n + (oeisIx @212412) n

-- instance OEIS 212440 where
--   oeis = filter (even . (oeisIx @181935)) [0..]

-- instance OEIS 212441 where
--   oeis = filter (odd . (oeisIx @181935)) [0..]

-- instance OEIS 212444 where
--   oeis = iterate (oeisIx @212439) 0

-- instance OEIS 212529 where
--   oeisIx = (oeisIx @39724) . negate

-- instance OEIS 212721 where
--   oeis = tablList @212721
-- instance Table 212721 where
--   rowCol n k = (rowT @212721) n !! (k-1)
--   rowT = nub . sort . (map product) . ps 1 where
--      ps x 0 = [[]]
--      ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]
--   tabf = map (rowT @212721) [0..]

-- instance OEIS 212760 where
--   oeisIx = (oeisIx @260706) . fromInteger . (oeisIx @1318) . (+ 1)

-- instance OEIS 212813 where
--   oeisIx n | n < 7     = -1
--             | otherwise = fst $ (until ((== 8) . snd))
--                                 (\ (s, x) -> (s + 1, (oeisIx @36288) x)) (0, n)

-- instance OEIS 212908 where
--   oeis = map (+ 1) $ elemIndices 3 (oeis @212813)

-- instance OEIS 212909 where
--   oeis = map (+ 1) $ elemIndices 4 (oeis @212813)

-- instance OEIS 212911 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @212813))

-- instance OEIS 213025 where
--   oeis = f (oeis @1358) where
--      f (x:sps'@ (y:z:sps)) | 2 * y == (x + z) = y : f sps'
--                           | otherwise        = f sps'

-- instance OEIS 213087 where
--   oeis = f (oeis @30190) where
--      f xs = foldl1 (\v d -> 10 * v + d) (ys ++ [0]) : f zs where
--             (ys, _:zs) = span (/= 0) xs

-- instance OEIS 213190 where
--   oeis = 1 : 1 : zipWith (+)
--      (zipWith (*) [2..] $ tail (oeis @213190)) (map (* 3) (oeis @213190))

-- instance OEIS 213517 where
--   oeis = filter ((<= 2) . (oeisIx @118668)) [0..]

-- instance OEIS 213518 where
--   oeis = filter ((== 2) . (oeisIx @118668)) [0..]

-- instance OEIS 213629 where
--   oeis = tablList @213629
-- instance Table 213629 where
--   rowCol = rowCol_off @213629 @1 @1
--   rowT   = rowT_off   @213629 @1
--   tabl = map f $ tail $ inits $ tail $ map reverse (tabf @30308) where
--      f xss = map (\xs ->
--              sum $ map (fromEnum . (xs `isPrefixOf`)) $ tails $ last xss) xss

-- instance OEIS 213676 where
--   oeis = tablList @213676
-- instance Table 213676 where
--   tabf = [0] : map reverse (tabf @189920)

-- instance OEIS 213684 where
--   oeisIx n = (oeisIx @155161) (2*n) n

-- instance OEIS 213714 where
--   oeis = f [0..] (oeis @5187) 0 where
--      f (x:xs) ys'@ (y:ys) i | x == y    = i : f xs ys (i+1)
--                            | otherwise = 0 : f xs ys' i

-- instance OEIS 213723 where
--   oeisIx = (* 2) . (oeisIx @213714)

-- instance OEIS 213724 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @213723) n + signum (oeisIx n)

-- instance OEIS 213911 where
--   oeisIx = genericLength . filter ((== 0) . head) . group . (rowT @213676)

-- instance OEIS 213912 where
--   oeis = 1 : f [1] where
--      f xs@ (x:_) = y : f (y : xs) where
--        y = if z `notElem` xs then z else 3 * x where z = (oeisIx @196) x

-- instance OEIS 213913 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @213912))

-- instance OEIS 213998 where
--   oeis = tablList @213998
-- instance Table 213998 where
--   tabl = map (map numerator) $ iterate pf [1] where
--      pf row = zipWith (+) ([0] ++ row) (row ++ [-1 % (x * (x + 1))])
--               where x = denominator $ last row

-- instance OEIS 213999 where
--   oeis = tablList @213999
-- instance Table 213999 where
--   tabl = map (map denominator) $ iterate pf [1] where
--      pf row = zipWith (+) ([0] ++ row) (row ++ [-1 % (x * (x + 1))])
--               where x = denominator $ last row

-- instance OEIS 214075 where
--   oeis = tablList @214075
-- instance Table 214075 where
--   tabl = zipWith (zipWith div) (tabl @213998) (tabl @213999)

-- instance OEIS 214084 where
--   oeis = tablList @214084
-- instance Table 214084 where
--   tabf = zipWith enumFromTo (oeis @290) (oeis @578)

-- instance OEIS 214178 where
--   oeis = tablList @214178
-- instance Table 214178 where
--   tabl = [0] : map f (tabl @37027) where
--      f row = (zipWith (*) (oeis @142) row) ++ [0]

-- instance OEIS 214282 where
--   oeisIx n = (oeisIx @7318) (n - 1) (oeisIx (n - 1))

-- instance OEIS 214283 where
--   oeisIx 1 = 0
--   oeisIx n = - (oeisIx @7318) (n - 1) (oeisIx n)

-- instance OEIS 214292 where
--   oeis = tablList @214292
-- instance Table 214292 where
--   tabl = map diff $ tail (tabl @7318)
--      where diff row = zipWith (-) (tail row) row

-- instance OEIS 214295 where
--   oeisIx n = (oeisIx @10052) n - (oeisIx @10052) (3*n)

-- instance OEIS 214320 where
--   oeis = 1 : 1 : 1 : (map (oeisIx @6530) $
--      zipWith (+) (oeis @214320) (drop 2 $ (oeis @214320)))

-- instance OEIS 214321 where
--   import Data.Set (fromList, toList, Set)
--   oeis_conjectured  = toList $ fromList $ take 100000 (oeis @214551)
--   b214321 = bFile' "A214321" (take 10000 (oeis @214321)_conjectured) 1

-- instance OEIS 214322 where
--   oeis = 1 : 1 : 1 : zipWith (+) (oeis @214551) (drop 2 (oeis @214551))

-- instance OEIS 214323 where
--   oeis = 1 : 1 : 1 : zipWith gcd (oeis @214551) (drop 2 (oeis @214551))

-- instance OEIS 214360 where
--   oeis = [x | k <- [0..], let x = 3120613860*k+23, (oeisIx @10051)' x == 1]

-- instance OEIS 214433 where
--   oeis = [x | x <- [0..], (oeisIx @105025) x == (oeisIx @105027) x]

-- instance OEIS 214489 where
--   oeis = [x | x <- [0..], (oeisIx @70939) x == (oeisIx @103586) x]

-- instance OEIS 214511 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @45698))

-- instance OEIS 214546 where
--   oeis = zipWith (-) (tail (oeis @140472)) (oeis @140472)

-- instance OEIS 214551 where
--   oeis = 1 : 1 : 1 : zipWith f (oeis @214551) (drop 2 (oeis @214551))
--      where f u v = (u + v) `div` gcd u v

-- instance OEIS 214560 where
--   oeisIx = (oeisIx @23416) . (oeisIx @290)

-- instance OEIS 214567 where
--   oeis = 1 : g 2 where
--     g x = y : g (x + 1) where
--       y | t > 0     = (oeisIx @214567) t + 1
--         | otherwise = 1 + sum (map ((subtract 1) . (oeisIx @214567)) $ (rowT @27748) x)
--          where t = (oeisIx @49084) x

-- instance OEIS 214583 where
--   oeis = filter (p 3 1) [2..] where
--      p i k2 x = x <= k2 || (gcd k2 x > 1 || (oeisIx @10051)' (x - k2) == 1) &&
--                            p (i + 2) (k2 + i) x

-- instance OEIS 214604 where
--   oeis = tablList @214604
-- instance Table 214604 where
--   rowCol = rowCol_off @214604 @1 @1
--   rowT   = rowT_off   @214604 @1
--   tabl = zipWith take [1..] $ transpose (tabl @176271)

-- instance OEIS 214614 where
--   oeis = tablList @214614
-- instance Table 214614 where
--   rowCol n k = (tabf @214614) !! (n - 1) (k-1)
--   rowT   = rowT_off @214614 @1
--   tabf = zipWith f [1..] (tabf @70165) where
--                          f v ws = sort $ filter (<= v) ws

-- instance OEIS 214626 where
--   oeis = 1 : 1 : 3 : zipWith f (oeis @214626) (drop 2 (oeis @214626))
--      where f u v = (u + v) `div` gcd u v

-- instance OEIS 214653 where
--   oeis = elemIndices 1 (oeis @214323)

-- instance OEIS 214661 where
--   oeis = tablList @214661
-- instance Table 214661 where
--   rowCol = rowCol_off @214661 @1 @1
--   rowT   = rowT_off   @214661 @1
--   tabl = zipWith take [1..] $ transpose $ map reverse (tabl @176271)

-- instance OEIS 214723 where
--   oeis = elemIndices 1 (oeis @45698)

-- instance OEIS 214727 where
--   oeis = 1 : 2 : 2 : zipWith3 (\x y z -> x + y + z)
--      (oeis @214727) (tail (oeis @214727)) (drop 2 (oeis @214727))

-- instance OEIS 214772 where
--   oeisIx = p [6, 9, 20] where
--      p _      0 = 1
--      p []     _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 214777 where
--   oeis = findIndices (> 0) (oeis @214772)

-- instance OEIS 214848 where
--   oeis = zipWith (-) (tail (oeis @22846)) (oeis @22846)

-- instance OEIS 214855 where
--   oeisIx = (oeisIx @45) . (oeisIx @8597) . subtract 1

-- instance OEIS 214866 where
--   oeis = elemIndices 0 (oeis @59175)

-- instance OEIS 214879 where
--   oeis = elemIndices 0 (oeis @45698)

-- instance OEIS 214957 where
--   oeis = [x | x <- [0..], (oeisIx @214950) x == 1]

-- instance OEIS 214958 where
--   oeis = [x | x <- [0..], (oeisIx @214949) x == 1]

-- instance OEIS 215217 where
--   twinLow [] = []
--   twinLow [_] = []
--   twinLow (n : (m : ns))
--       | m == n + 1 = n : twinLow (m : ns)
--       | otherwise = twinLow (m : ns)
--   oeisIx n = (twinLow (oeis @7304)) !! (n - 1)

-- instance OEIS 215231 where
--   (oeis, (oeis @85809)) = unzip $ (2, 1) : f 1 2 (oeis @65516) where
--      f i v (q:qs) | q > v = (q, i) : f (i + 1) q qs
--                   | otherwise = f (i + 1) v qs

-- instance OEIS 215244 where
--   import Data.Map (Map, singleton, (!), insert)
--   newtype Bin = Bin [Int] deriving (Eq, Show, Read)
--   instance Ord Bin where
--      Bin us <= Bin vs | length us == length vs = us <= vs
--                       | otherwise              = length us <= length vs
--   oeisIx n = (oeis @215244) !! n
--   oeis = 1 : f [1] (singleton (Bin [0]) 1) where
--      f bs m | last bs == 1 = y : f (succ bs) (insert (Bin bs) y m)
--             | otherwise    = f (succ bs) (insert (Bin bs) y m) where
--        y = fromEnum (pal bs) +
--            sum (zipWith (\us vs -> if pal us then m ! Bin vs else 0)
--                         (init $ drop 1 $ inits bs) (drop 1 $ tails bs))
--        pal ds = reverse ds == ds
--        succ [] = [0]; succ (0:ds) = 1 : ds; succ (1:ds) = 0 : succ ds

-- instance OEIS 215403 where
--   oeis = map (foldr (\d v -> 10 * v + d) 0) $
--                      concatMap (\x -> map (x :) [plut', nept']) [4..] where
--     plut' = [1,1,2,2,2,3,2,1,1,2,1,1,2,2,2,1,2,2,2,3,1,1,2,2,1,3]
--     nept' = [1,1,2,2,3,3,1,2,1,1,2,2,1,1,2,2,3,1,1,2,3,3,1,1,2,2,2,1,1,3,1]
-- instance Table 215403 where
--   rowCol n k = (oeis @215403) !! (n - 1)

-- instance OEIS 215630 where
--   oeis = tablList @215630
-- instance Table 215630 where
--   rowCol = rowCol_off @215630 @1 @1
--   rowT   = rowT_off   @215630 @1
--   tabl = zipWith3 (zipWith3 (\u v w -> u - v + w))
--                           (tabl @93995) (tabl @75362) (tabl @133819)

-- instance OEIS 215631 where
--   oeis = tablList @215631
-- instance Table 215631 where
--   rowCol = rowCol_off @215631 @1 @1
--   rowT   = rowT_off   @215631 @1
--   tabl = zipWith3 (zipWith3 (\u v w -> u + v + w))
--                           (tabl @93995) (tabl @75362) (tabl @133819)

-- instance OEIS 215879 where
--   oeisIx n = if t == 0 then 0 else (oeisIx @215879) n' + 1
--               where (n',t) = divMod n 3

-- instance OEIS 216155 where
--   oeis = filter
--      (\x -> (oeisIx @196) (oeisIx x) == (oeisIx @196) (oeisIx x) + 1) [1..]

-- instance OEIS 216176 where
--   oeisIx n = sum $ zipWith (*) zs $ reverse zs
--      where zs = (rowT @189920) n

-- instance OEIS 216183 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @59514)

-- instance OEIS 216237 where
--   oeis = filter ((== 1) . (oeisIx @136522)) (oeis @7770)

-- instance OEIS 216261 where
--   oeisIx = fromJust . (`elemIndex` (oeis @6942))

-- instance OEIS 216345 where
--   oeis = 1 : (filter (\x -> (oeisIx @2) x /= (oeisIx @2) (x - 1)) [2..])

-- instance OEIS 216407 where
--   oeisIx = (45 -) . (oeisIx @217928)

-- instance OEIS 216599 where
--   oeisIx n | n <= 2    = 0
--             | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 3 - 1)

-- instance OEIS 216600 where
--   oeisIx n | n <= 3    = 0
--             | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 4 - 1)

-- instance OEIS 216601 where
--   oeisIx n | n <= 4    = 0
--             | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 5 - 1)

-- instance OEIS 216602 where
--   oeisIx n | n <= 5    = 0
--             | otherwise = (tabl @66032) !! (n - 1) !! (n `div` 6 - 1)

-- instance OEIS 216965 where
--   oeis = [p | p <- (oeis @40), odd $ (oeisIx @141468) $ fromInteger p]

-- instance OEIS 217122 where
--   oeis = f 1 [0..] where
--      f x zs = y : f (x + 1) (delete y zs) where
--        y = zs !! (oeisIx @120) x

-- instance OEIS 217218 where
--   oeis = iterate (oeisIx @6368) 44

-- instance OEIS 217261 where
--   import Data.Set (singleton, insert, deleteFindMin)
--   oeisIx n = (oeis @217261) !! (n - 1)
--   oeis = f [3..] $ singleton (16, (2, 2)) where
--      f xs'@ (x:xs) s
--        | m > x ^ 4  = f xs $ insert (x ^ 4, (x, 2)) s
--        | m == x ^ 4 = f xs s
--        | otherwise  = m : f xs' (insert (i ^ (j + 1) ^ 2, (i, j + 1)) s')
--        where ((m, (i,j)), s') = deleteFindMin s

-- instance OEIS 217398 where
--   oeis = filter ((== 5) . (oeisIx @30)) [1..]

-- instance OEIS 217575 where
--   oeisIx = subtract 1 . (oeisIx @63657)

-- instance OEIS 217589 where
--   import Data.Word (Word16)
--   oeisIx :: Word16 -> Word16
--   oeisIx n = rev 0 0 where
--      rev 16 y = y
--      rev i y = rev (i + 1) (if testBit n i then setBit y (15 - i) else y)

-- instance OEIS 217657 where
--   oeisIx n | n <= 9    = 0
--             | otherwise = 10 * (oeisIx @217657) n' + m where (n', m) = divMod n 10

-- instance OEIS 217659 where
--   oeisIx = (oeisIx @151800) . fromInteger . (oeisIx @185934)

-- instance OEIS 217712 where
--   import Data.Set (Set, empty, fromList, toList, union, size)
--   import Data.Set (member, delete, insert)
--   oeisIx n = (oeis @217712) !! (n - 1)
--   oeis = f 1 empty empty where
--      f x s s1 = size s1' : f (x + 1) (s `union` fromList hs) s1' where
--        s1' = g s1 $ filter ((== 1) . (oeisIx @10051)') $ map numerator hs
--        g v []                    = v
--        g v (w:ws) | w `member` v = g (delete w v) ws
--                   | otherwise    = g (insert w v) ws
--        hs = map (+ 1 % x) $ 0 : toList s

-- instance OEIS 217793 where
--   oeis = tablList @217793
-- instance Table 217793 where
--   rowCol = rowCol_off @217793 @1 @0
--   rowT   = rowT_off @217793 @1
--   tabf =
--      map (\p -> [2*p*k + k^2 `mod` p | k <- [0..p-1]]) (oeis @65091)

-- instance OEIS 217843 where
--   import Data.Set (singleton, deleteFindMin, insert, Set)
--   oeisIx n = (oeis @217843) !! (n - 1)
--   oeis = f (singleton (0, (0,0))) (-1) where
--      f s z = if y /= z then y : f s'' y else f s'' y
--                 where s'' = (insert (y', (i, j')) $
--                              insert (y' - i ^ 3 , (i + 1, j')) s')
--                       y' = y + j' ^ 3; j' = j + 1
--                       ((y, (i, j)), s') = deleteFindMin s

-- instance OEIS 217863 where
--   oeisIx = (oeisIx @10) . (oeisIx @3418)

-- instance OEIS 217921 where
--   oeisIx n = fst $ until (all (== 1) . snd) f (0, (rowT @30308) n) where
--      f (i, xs)  = (i + 1, map genericLength $ group xs)

-- instance OEIS 218388 where
--   oeisIx = foldl1 (.|.) . (rowT @27750) :: Integer -> Integer

-- instance OEIS 218403 where
--   oeisIx = foldl (.|.)  0 . (rowT @27751) :: Integer -> Integer

-- instance OEIS 218454 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @176352))

-- instance OEIS 218494 where
--   oeisIx = p (tail (oeis @290)) . (^ 3) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 218533 where
--   oeisIx n = (oeisIx @176352) n `div` (oeisIx @218535) n
--   oeis = map numerator $ zipWith (%) (oeis @176352) $ tail (oeis @176352)

-- instance OEIS 218534 where
--   oeisIx n = (oeisIx @176352) (n + 1) `div` (oeisIx @218535) n
--   oeis = map denominator $ zipWith (%) (oeis @176352) $ tail (oeis @176352)

-- instance OEIS 219069 where
--   oeis = tablList @219069
--   rowCol = rowCol_off @219069 @1 @1
--   rowT n = (tabl @219069) !! n
--   tabl = zipWith (zipWith (*)) (tabl @215630) (tabl @215631)

-- instance OEIS 219117 where
--   oeis = filter (all (== 1) . p) [1..] where
--      p x = map (oeisIx . (+ (x + 1)) . (x ^)) [1..4]

-- instance OEIS 219206 where
--   oeis = tablList @219206
-- instance Table 219206 where
--   tabl = zipWith (zipWith (^)) (tabl @7318) (tabl @2262)

-- instance OEIS 219244 where
--   oeis = map (`div`  6) $ zipWith (-) (oeis @217659) (oeis @185934)

-- instance OEIS 219462 where
--   oeisIx = sum . zipWith (*) (oeis @1906) . (rowT @34870)

-- instance OEIS 219463 where
--   oeis = tablList @219463
--   rowCol n k = (tabl @219463) !! n !! k :: Int
--   rowT n = (tabl @219463) !! n
--   tabl = map (map (1 -)) (tabl @47999)

-- instance OEIS 219529 where
--   -- Very slow, could certainly be accelerated.  SST stands for Snub Square Tiling.
--   setUnion [] l2 = l2
--   setUnion (a:rst) l2 = if (elem a l2) then doRest else (a:doRest)
--     where doRest = setUnion rst l2
--   setDifference [] l2 = []
--   setDifference (a:rst) l2 = if (elem a l2) then doRest else (a:doRest)
--     where doRest = setDifference rst l2
--   adjust k = (if (even k) then 1 else -1)
--   weirdAdjacent (x,y) = (x+ (adjust y),y+ (adjust x))
--   sstAdjacents (x,y) = [ (x+1,y), (x-1,y), (x,y+1), (x,y-1), (weirdAdjacent (x,y))]
--   sstNeighbors core = foldl setUnion core (map sstAdjacents core)
--   sstGlob n core = if (n == 0) then core else (sstGlob (n - 1) (sstNeighbors core))
--   sstHalo core = setDifference (sstNeighbors core) core
--   origin = [ (0,0)]
--   oeisIx n = genericLength (sstHalo (sstGlob (n - 1) origin))

-- instance OEIS 219603 where
--   oeisIx n = (oeisIx @40) n * (oeisIx @31368) n

-- instance OEIS 219607 where
--   oeisIx = p (oeis @47221) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 219608 where
--   oeis = filter odd (oeis @60142)

-- instance OEIS 219609 where
--   oeis = map (`div` 2) $ zipWith (-) (tail (oeis @219608)) (oeis @219608)

-- instance OEIS 219762 where
--   oeisIx = subtract 1 . (oeisIx @99054) . subtract 1

-- instance OEIS 219843 where
--   oeisIx = foldr (\u v-> 2*v + u) 0 . map toInteger . (rowT @219463)

-- instance OEIS 219907 where
--   import Data.Set (deleteFindMin, empty, fromList, union)
--   import qualified Data.Set as Set (null)
--   oeisIx n = (oeis @219907) !! (n - 1)
--   oeis = f 0 empty where
--      f z s | Set.null s || z' <= m = f (z + 1) (s `union` (fromList ws))
--            | otherwise             = m : f z s'
--            where (m,s') = deleteFindMin s
--                  ws = map (h z) [0..z] ++ map (flip h z) [0..z-1]
--                  h i j = 4 * i ^ 2 + 2 * i * j + 7 * j ^ 2
--                  z' = h z 0

-- instance OEIS 219908 where
--   oeisIx n = (oeis @219907) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @219907)

-- instance OEIS 219922 where
--   oeisIx n = (fromJust $ findIndex (n `elem`) (tabl @26835)) + 1

-- instance OEIS 220053 where
--   oeis = tablList @220053
-- instance Table 220053 where
--   rowCol = rowCol_off @220053 @1 @1
--   rowT   = rowT_off   @220053 @1
--   tabl = map (scanl1 (+)) (tabl @130517)

-- instance OEIS 220073 where
--   oeis = tablList @220073
-- instance Table 220073 where
--   rowCol = rowCol_off @220073 @1 @1
--   rowT   = rowT_off   @220073 @1
--   tabl = map reverse (tabl @130517)

-- instance OEIS 220075 where
--   oeis = tablList @220075
-- instance Table 220075 where
--   rowCol = rowCol_off @220075 @1 @1
--   rowT   = rowT_off   @220075 @1
--   tabl = map (scanl1 (+)) (tabl @220073)

-- instance OEIS 220096 where
--   oeisIx n = if z == 1 then n - 1 else z  where z = (oeisIx @32742) n

-- instance OEIS 220101 where
--   oeisIx n = (oeisIx @51666 (2 * (n - 1)) (n - 1)) `div` 2

-- instance OEIS 220218 where
--   oeis = 1 : filter (all (== 1) . map (oeisIx' . (+ 1)) . (rowT @124010)) [1..]

-- instance OEIS 220263 where
--   oeisIx = genericLength . (rowT @192719)

-- instance OEIS 220264 where
--   oeisIx n = fromJust $ find ((== n) . (oeisIx @86971)) (oeis @220423)

-- instance OEIS 220347 where
--   oeisIx =  (+ 1) . fromJust . (`elemIndex` (oeis @183079))

-- instance OEIS 220348 where
--   oeisIx n = fromJust (findIndex (elem n) (tabf @183079)) + 1

-- instance OEIS 220423 where
--   import Data.Set (deleteFindMin, empty, fromList, union)
--   import qualified Data.Set as Set (null)
--   oeisIx n = (oeis @220423) !! (n - 1)
--   oeis = f (splitAt 1 (oeis @2110)) empty where
--      f (us'@ (u:_), vs'@ (v:vs)) s
--        | Set.null s || m > u
--                    = f (v:us', vs) (s `union` (fromList $ map (* u) us'))
--        | otherwise = m : f (us', vs') s'
--        where (m,s') = deleteFindMin s

-- instance OEIS 220424 where
--   oeis = tablList @220424
-- instance Table 220424 where
--   rowCol = rowCol_off @220424 @1 @1
--   rowT   = rowT_off @220424 @1
--   tabf = iterate
--                  (concatMap (\xs -> [head xs, length xs]) . group) [1]

-- instance OEIS 220432 where
--   oeis = filter (\x -> null $
--      intersect (oeisIx_row x) (takeWhile (<= x) (oeis @219908))) (oeis @7310)

-- instance OEIS 220654 where
--   oeisIx = fromJust . (`elemIndex` (oeis @215244))

-- instance OEIS 220812 where
--   oeisIx = (oeisIx @11557) . (oeisIx @79)

-- instance OEIS 220894 where
--   t :: Int -> Int -> Integer
--   t 0 m = (fi m)
--   t n m = t (n - 1) (m+1) + foldl (+) 0 (let tt = [t i m | i<- [0.. (n-1)]] in
--                                         (map (uncurry (*)) (zip tt (reverse tt))))

-- instance OEIS 221264 where
--   oeis = filter ((< 0) . (oeisIx @5094)) [1..] is

-- instance OEIS 221869 where
--   import Data.Set (singleton, member, insert)
--   oeisIx n = (oeis @221869) !! (n - 1)
--   oeis = f 2 7 (singleton 1) where
--      f u v s | d `member` s = f (u + 1) (v + d) s
--              | otherwise    = d : f (u + 1) (v + d) (d `insert` s)
--              where d = gcd u v

-- instance OEIS 222208 where
--   oeis = 1 : 3 : f 3 (2 : [4 ..]) where
--      f u vs = g vs where
--        g (w:ws) = if all (== 0) $ map ((mod w) . (oeisIx @222208)) $ (rowT @27751) u
--                      then w : f (u + 1) (delete w vs) else g ws

-- instance OEIS 222209 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @222208))

-- instance OEIS 222222 where
--   oeisIx = foldl f 0 . map (read . return) . show :: Integer -> Integer
--             where f v d = 10 * v + if d == 1 || d == 5 then 6 - d else d

-- instance OEIS 222493 where
--   oeisIx = (oeisIx @133500) . (oeisIx @221221)

-- instance OEIS 222581 where
--   oeis = map length $ group (oeis @93796)

-- instance OEIS 222622 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @113966))

-- instance OEIS 222623 where
--   oeis = filter (\x -> (oeisIx @113966) x == x) [1..]

-- instance OEIS 222946 where
--   oeis = tablList @222946
-- instance Table 222946 where
--   rowCol = rowCol_off @222946 @2 @1
--   rowT = rowT_off @222946 @2
--   tabl = zipWith p [2..] (tabl @55096) where
--      p x row = zipWith (*) row $
--                map (\k -> ((x + k) `mod` 2) * (oeisIx @63524) (gcd x k)) [1..]

-- instance OEIS 223456 where
--   oeis = filter ((== 1 ) . (oeisIx @10051) . (oeisIx @32741) . (oeisIx @32741)) (oeis @2808)

-- instance OEIS 223490 where
--   oeisIx = head . (rowT @213925)

-- instance OEIS 223491 where
--   oeisIx = last . (rowT @213925)

-- instance OEIS 224075 where
--   oeis = tablList @224075
-- instance Table 224075 where
--   rowCol = rowCol_off @224075 @1 @1
--   rowT   = rowT_off @224075 @1
--   tabf = f 3 where
--      f x = g [] 3 1 where
--        g ps i k2 | x <= k2        = ps : f (x + 1)
--                  | gcd k2 x > 1   = g ps (i + 2) (k2 + i)
--                  | (oeisIx @10051) q == 1 = g (q:ps) (i + 2) (k2 + i)
--                  | otherwise      = f (x + 1)
--                  where q = x - k2

-- instance OEIS 224076 where
--   oeisIx = genericLength . (rowT @224075)

-- instance OEIS 224345 where
--   gtab :: [[Integer]]
--   gtab = [0..] : [[s n m |  m <- [0..]] | n <- [1..]]
--     where s n m  = let fi =  [ftab !! i !! m | i <- [0.. (n - 1)]]
--                        gi =  [gtab !! i !! m | i <- [0.. (n - 1)]]
--                    in foldl (+) 0 (map (uncurry (*)) (zip fi (reverse gi)))
--   ftab :: [[Integer]]
--   ftab = [0..] : [[ftab !! (n - 1) !! (m+1) + gtab !! n !! m | m<-[0..]] | n<-[1..]]
--   f (n,m) = ftab !! n !! m

-- instance OEIS 224363 where
--   oeisIx = (oeisIx @40) . (oeisIx @221056)

-- instance OEIS 224401 where
--   oeisIx = (+ 1) . fromJust . (`findIndex` (tabf @85612)) . elem

-- instance OEIS 224458 where
--   oeis = 0 : g 2 where
--      g x = y : g (x + 1) where
--        y | t > 0     = (oeisIx @224458) t + (oeisIx @1222) t
--          | otherwise = (oeisIx @224458) r + (oeisIx @224458) s + (oeisIx @1222) r * (oeisIx @1222) s
--          where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

-- instance OEIS 224729 where
--   oeisIx n = (oeisIx @59283) (2*n) n

-- instance OEIS 224782 where
--   oeis = map (foldl h 0 . group . show) (oeis @79) where
--      h x zs@ (z:_) = if z == '0' then max x $ length zs else x

-- instance OEIS 224823 where
--   oeisIx n = genericLength [ () | let ts = takeWhile (<= n) (oeis @217),
--               x <- ts, y <- ts, z <- takeWhile (<= div (n - x - y) 3) ts,
--               x + y + 3 * z == n]

-- instance OEIS 224829 where
--   oeis = filter ((== 0) . (oeisIx @224823)) [0..]

-- instance OEIS 224839 where
--   oeis = f [1..] [] where
--      f (x:xs) ys = if all ((== 0) . (oeisIx @10052)) $ map (x -) ys
--                       then x : f xs (x:ys) else f xs ys

-- instance OEIS 224841 where
--   oeis = tablList @224841
-- instance Table 224841 where
--   rowCol = rowCol_off @224841 @1 @1
--   rowT   = rowT_off   @224841 @1
--   tabl = map
--      (reverse . map (read . concatMap show) . init . tails) $
--      tail $ inits (oeis @7376) :: [[Integer]]

-- instance OEIS 224866 where
--   oeis = [x | x <- [2..] , let x' = x - 1, let k = (oeisIx @7947) x',
--                       let (y,m) = divMod x' k, m == 0, (oeisIx @7947) y == k]

-- instance OEIS 224909 where
--   oeis = 1 : 1 : zipWith mod
--      (zipWith (+) (oeis @224909) $ tail (oeis @224909))
--      (zipWith (-) [3..] $ tail (oeis @224909))

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

-- instance OEIS 225043 where
--   oeis = tablList @225043
-- instance Table 225043 where
--   tabl = zipWith (map . flip mod) [1..] (tabl @7318)

-- instance OEIS 225044 where
--   oeisIx = p (oeis @14132) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 225045 where
--   oeisIx = p (oeis @14132) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 225047 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121216))

-- instance OEIS 225078 where
--   oeis = elemIndices 1 $
--      zipWith ((*) `on` (oeisIx @10051)') (oeis @2522) (oeis @8865)

-- instance OEIS 225105 where
--   oeis = filter
--      ((== 1) . (oeisIx @10051)' . maximum . filter odd . (rowT @70165)) (oeis @5408)

-- instance OEIS 225124 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @25586)) . (oeisIx @79)

-- instance OEIS 225126 where
--   oeisIx n = (oeisIx @48152) (2 * n - 1)  n

-- instance OEIS 225183 where
--   zipl :: [[x]] -> [x]
--   zipl (s:ss) = head s : zipl (ss ++ [ (tail s)])
--   oeisIx = s where
--     s = 0 : x
--     x = 1 : zipl [x,y]
--     y = 0 : 1 : zipl [z,x,y]
--     z = 0 : zipl [y,x]

-- instance OEIS 225228 where
--   oeis = filter f [1..] where
--      f x = length es == 3 && sum es `elem` [3,5,7] &&
--                              maximum es - minimum es <= 1
--            where es = (rowT @124010) x

-- instance OEIS 225230 where
--   oeisIx n = (oeisIx @1221) n - (oeisIx @51903) n

-- instance OEIS 225243 where
--   oeis = tablList @225243
-- instance Table 225243 where
--   rowCol = rowCol_off @225243 @1 @1
--   rowT   = rowT_off @225243 @1
--   tabf = [1] : map (filter ((== 1) . (oeisIx @10051)')) (tail (tabf @165416))

-- instance OEIS 225244 where
--   oeisIx n = p (oeisIx_row n) n where
--      p _          0 = 1
--      p []         _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 225245 where
--   oeisIx n = p (oeisIx_row n) n where
--      p _      0 = 1
--      p []     _ = 0
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 225353 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @225245)

-- instance OEIS 225354 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @225245)

-- instance OEIS 225395 where
--   oeisIx n = product $ zipWith (^)
--       (map (oeisIx @49084) $ (rowT @27748) n) (map (oeisIx @225395) $ (rowT @124010) n)

-- instance OEIS 225413 where
--   oeis = tablList @225413
-- instance Table 225413 where
--   tabl = map (map (`div` 2)) $
--                  zipWith (zipWith (-)) (tabl @101164) (tabl @14473)

-- instance OEIS 225481 where
--   oeisIx n = product [p | p <- takeWhile (<= n + 1) (oeis @40),
--                            mod n (p - 1) == 0 || mod (n + 1) p == 0]

-- instance OEIS 225493 where
--   import Data.Set (singleton, fromList, union, deleteFindMin)
--   oeisIx n = (oeis @225493) !! (n - 1)
--   oeis = 1 : h (singleton p) ps [p] where
--      (p:ps) = (oeis @51634)
--      h s xs'@ (x:xs) ys
--        | m > x     = h (s `union` (fromList $ map (* x) (1 : ys))) xs ys
--        | otherwise = m : h (s' `union` (fromList $ map (* m) ys')) xs' ys'
--        where ys' = m : ys; (m, s') = deleteFindMin s

-- instance OEIS 225494 where
--   import Data.Set (singleton, fromList, union, deleteFindMin)
--   oeisIx n = (oeis @225494) !! (n - 1)
--   oeis = 1 : h (singleton p) ps [p] where
--      (p:ps) = (oeis @6562)
--      h s xs'@ (x:xs) ys
--        | m > x     = h (s `union` (fromList $ map (* x) (1 : ys))) xs ys
--        | otherwise = m : h (s' `union` (fromList $ map (* m) ys')) xs' ys'
--        where ys' = m : ys; (m, s') = deleteFindMin s

-- instance OEIS 225495 where
--   import Data.Set (singleton, fromList, union, deleteFindMin)
--   oeisIx n = (oeis @225495) !! (n - 1)
--   oeis = 1 : h (singleton p) ps [p] where
--      (p:ps) = (oeis @51635)
--      h s xs'@ (x:xs) ys
--        | m > x     = h (s `union` (fromList $ map (* x) (1 : ys))) xs ys
--        | otherwise = m : h (s' `union` (fromList $ map (* m) ys')) xs' ys'
--        where ys' = m : ys; (m, s') = deleteFindMin s

-- instance OEIS 225496 where
--   import Data.Set (singleton, fromList, union, deleteFindMin)
--   oeisIx n = (oeis @225496) !! (n - 1)
--   oeis = 1 : h (singleton p) ps [p] where
--      (p:ps) = (oeis @178943)
--      h s xs'@ (x:xs) ys
--        | m > x     = h (s `union` (fromList $ map (* x) (1 : ys))) xs ys
--        | otherwise = m : h (s' `union` (fromList $ map (* m) ys')) xs' ys'
--        where ys' = m : ys; (m, s') = deleteFindMin s

-- instance OEIS 225589 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @217122))

-- instance OEIS 225621 where
--   oeisIx n = (oeisIx @74911) (2 * n - 1) n

-- instance OEIS 225793 where
--   oeis = filter ((== 1) . (oeisIx @230093)) [1..]

-- instance OEIS 225817 where
--   oeis = tablList @225817
-- instance Table 225817 where
--   rowCol = rowCol_off @225817 @1 @1
--   rowT   = rowT_off @225817 @1
--   tabf = map (map (oeisIx @8683)) (tabf @27750)

-- instance OEIS 225840 where
--   oeisIx (succ->n) = maximum $ filter (< n) $ (rowT @70165) n

-- instance OEIS 225850 where
--   oeisIx = fromJust . (`elemIndex` (oeis @167151))

-- instance OEIS 225860 where
--   oeisIx n = p (oeisIx_row n) (2 ^ n) where
--      p _          0 = 1
--      p []         _ = 0
--      p bs'@ (b:bs) m = if m < b then 0 else p bs' (m - b) + p bs m

-- instance OEIS 226025 where
--   oeis = filter ((/= 2) . (oeisIx @100995)) (oeis @71904)

-- instance OEIS 226029 where
--   oeis = zipWith (-) (tail (oeis @182402)) (oeis @182402)

-- instance OEIS 226030 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226029))

-- instance OEIS 226047 where
--   oeisIx = maximum . (rowT @226078)

-- instance OEIS 226078 where
--   oeis = tablList @226078
-- instance Table 226078 where
--   tabf = map (rowT @141809) (oeis @984)

-- instance OEIS 226219 where
--   oeisIx n = head [k | k <- [2..],
--                         isInfixOf (show n) (show (k*n)), not $ p10 k]
--      where p10 = flip isPrefixOf ('1' : repeat '0') . show  :: Int -> Bool

-- instance OEIS 226222 where
--   oeis = 1 : 1 : 1 : zipWith (+)
--      (map (oeisIx @226222) $ zipWith (-) [3..] (oeis @226222))
--      (map (oeisIx @226222) $ zipWith (-) [2..] $ tail (oeis @226222))

-- instance OEIS 226227 where
--   oeis = filter (all (== 1) .
--                  map (oeisIx . genericLength) .
--                      other . tail . reverse . group . (rowT @30308)) [1..]
--      where other [] = []; other [x] = [x]; other (x:_:xs) = x : other xs

-- instance OEIS 226228 where
--   oeis = filter (all (== 1) .
--                  map (oeisIx . genericLength) .
--                      other . reverse . group . (rowT @30308)) [1..]
--      where other [] = []; other [x] = [x]; other (x:_:xs) = x : other xs

-- instance OEIS 226229 where
--   oeis = filter
--      (all (== 1) . map (oeisIx . genericLength) . group . (rowT @30308)) [1..]

-- instance OEIS 226244 where
--   (oeis, (oeis @226245)) = unzip $ (1,1) : f 1 1 (oeis @5185) where
--      f i v (q:qs) | q > v = (q,i) : f (i + 1) q qs
--                   | otherwise = f (i + 1) v qs

-- instance OEIS 226245 where

-- instance OEIS 226272 where
--   oeis = tablList @226272
-- instance Table 226272 where
--   rowT n = sort $ nub [u ^ v | u <- digs, v <- digs]
--                   where digs = nub $ map (read . return) $ show n
--   tabf = map (rowT @226272) [0..]

-- instance OEIS 226273 where
--   oeisIx = genericLength . (rowT @226272) :: Integer -> Int

-- instance OEIS 226277 where
--   oeis = sort [w | u <- [0..9], v <- [0..9], let w = u ^ v,
--      "0123456789" !! u `elem` show w, "0123456789" !! v `elem` show w]

-- instance OEIS 226314 where
--   oeis = tablList @226314
-- instance Table 226314 where
--   rowCol n k = n - (n - k) `div` gcd n k
--   rowT   = rowT_off   @226314 @1
--   tabl = map f $ tail (tabl @2262) where
--      f us'@ (_:us) = map (v -) $ zipWith div vs (map (gcd v) us)
--        where (v:vs) = reverse us'

-- instance OEIS 226387 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @85612))

-- instance OEIS 226463 where
--   oeis = tablList @226463
-- instance Table 226463 where
--   tabf = map (map (1 -)) (tabf @70950)

-- instance OEIS 226464 where
--   oeis = tablList @226464
-- instance Table 226464 where
--   tabf = map reverse (tabf @226463)

-- instance OEIS 226481 where
--   oeis = tablList @226481
-- instance Table 226481 where
--   tabf = map (map length . group) (tabf @70950)

-- instance OEIS 226482 where
--   oeisIx = genericLength . (rowT @226481)

-- instance OEIS 226483 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @224909))

-- instance OEIS 226518 where
--   oeis = tablList @226518
-- instance Table 226518 where
--   rowCol = rowCol_off @226518 @1 @0
--   rowT   = rowT_off @226518 @1
--   tabf = map (scanl1 (+)) (tabf @226520)

-- instance OEIS 226520 where
--   oeis = tablList @226520
-- instance Table 226520 where
--   rowCol = rowCol_off @226520 @1 @0
--   rowT   = rowT_off @226520 @1
--   tabf =
--      map (\p -> map (flip legendreSymbol p) [0..p-1]) (oeis @40)

-- instance OEIS 226532 where
--   oeisIx n = product $ zipWith (^)
--               (oeis @40) (scanr1 xor $ (rowT @67255) n :: [Integer])

-- instance OEIS 226555 where
--   oeisIx n = numerator $ sum $
--               zipWith ((%) `on` toInteger) (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 226569 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226532))

-- instance OEIS 226637 where
--   oeis = filter ((== 0) . (oeisIx @76489)) [0..]

-- instance OEIS 226649 where
--   oeis = concat $ transpose [oeis, drop 2 (oeis @1611)]

-- instance OEIS 226748 where
--   oeisIx = p (oeis @53012) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 226749 where
--   oeisIx = p (oeis @53012) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 226777 where
--   import qualified Data.Set as Set (null, split, filter)
--   import Data.Set (Set, empty, insert, member)
--   oeisIx n = (oeis @226777) !! (n - 1)
--   oeis = f (oeis @76467) empty where
--      f (x:xs) s | Set.null $ Set.filter ((`member` s) . (x -)) s'
--                             = f xs (x `insert` s)
--                 | otherwise = x : f xs (x `insert` s)
--                 where (s', _) = Set.split (x `div` 2) s

-- instance OEIS 226778 where
--   oeis = filter ((== 1) . (oeisIx @55483)) [1..]

-- instance OEIS 226898 where
--   oeisIx = maximum . map length .
--      map (\ds@ (d:_) -> takeWhile (<= e' d) ds) . init . tails . (rowT @27750)
--      where e' = floor . (* e) . fi; e = exp 1

-- instance OEIS 226946 where
--   oeis = filter ((== 0) . (oeisIx @86)) [1..]

-- instance OEIS 226950 where
--   import qualified Data.Set as Set (split, filter)
--   import Data.Set (Set, empty, size, insert, member)
--   oeisIx n = (oeis @226950) !! (n - 1)
--   oeis = f (oeis @76467) empty where
--      f (x:xs) s | size s'' <= 1 = f xs (x `insert` s)
--                 | otherwise     = x : f xs (x `insert` s)
--                 where s'' = Set.filter ((`member` s) . (x -)) s'
--                       (s', _) = Set.split (x `div` 2) s

-- instance OEIS 227048 where
--   oeis = tablList @227048
-- instance Table 227048 where
--   rowCol = rowCol_off @227048 @0 @1
--   tabf = map f (oeis @244)  where
--      f x = reverse $ map (x -) $ takeWhile (<= x) (oeis @79)

-- instance OEIS 227068 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @56924))

-- instance OEIS 227114 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @227113))

-- instance OEIS 227118 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @184992))

-- instance OEIS 227190 where
--   oeisIx n = n - (oeisIx @167489) n

-- instance OEIS 227288 where
--   oeis = zipWith gcd (tail (oeis @227113)) (oeis @227113)

-- instance OEIS 227289 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @227288))

-- instance OEIS 227291 where
--   oeisIx n = fromEnum $ (sum $ zipWith (*) mds (reverse mds)) == 1
--      where mds = (rowT @225817) n

-- instance OEIS 227296 where
--   oeisIx n = p [1 .. (oeisIx @10) n] n where
--      p _          0 = 1
--      p []         _ = 0
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 227325 where
--   oeisIx n = (oeisIx @272) (n + 1) * (oeisIx @984) n

-- instance OEIS 227326 where
--   oeis = 0 : xs where
--      xs = concat $ transpose
--           [oeis, zipWith (+) (tail (oeis @302)) (map (* 2) xs)]

-- instance OEIS 227378 where
--   oeisIx = fromJust . (`elemIndex` (oeis @217928))

-- instance OEIS 227388 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226390))

-- instance OEIS 227389 where
--   oeis = map length $ group (oeis @226390)

-- instance OEIS 227413 where
--   oeis = 1 : concat (transpose [map (oeisIx @40) (oeis @227413),
--                                         map (oeisIx @2808) (oeis @227413)])

-- instance OEIS 227431 where
--   oeis = tablList @227431
-- instance Table 227431 where
--   rowCol = rowCol_off @227431 @1 @1
--   rowT   = rowT_off   @227431 @1
--   tabl = h [] 0 1 where
--      h row u v = row' : h row' v (u + v) where row' = scanl (-) v row

-- instance OEIS 227455 where
--   oeis = 1 : f [2..] [1] where
--      f (v:vs) ws = if any (`notElem` ws) $ map (subtract 1) $ (rowT @27748) v
--                       then v : f vs (v : ws) else f vs ws

-- instance OEIS 227481 where
--   oeisIx = sum . map (oeisIx @10052) . (rowT @69011)

-- instance OEIS 227550 where
--   oeis = tablList @227550
-- instance Table 227550 where
--   tabl = map fst $ iterate
--      (\ (vs, w:ws) -> (zipWith (+) ([w] ++ vs) (vs ++ [w]), ws))
--      ([1], (oeis @1563))

-- instance OEIS 227617 where
--   import qualified Data.Map as Map (null, insert)
--   import Data.Map (empty, deleteFindMin)
--   oeisIx n = (oeis @227617) !! (n - 1)
--   oeis = f 1 empty $ zip (oeis @100707) [1..] where
--      f i mp (uv:uvs)
--        | Map.null mp = f i (uncurry Map.insert uv mp) uvs
--        | y == i      = x : f (i + 1) (uncurry Map.insert uv mp') uvs
--        | otherwise   = f i (uncurry Map.insert uv mp) uvs
--        where ((y,x), mp') = deleteFindMin mp

-- instance OEIS 227632 where
--   (oeis, (oeis @227633)) = unzip $ (1,1) : f 1 1 (oeis @227617) where
--      f i v (q:qs) | q > v = (q,i) : f (i + 1) q qs
--                   | otherwise = f (i + 1) v qs

-- instance OEIS 227633 where

-- instance OEIS 227736 where
--   oeis = tablList @227736
-- instance Table 227736 where
--   rowCol = rowCol_off @227736 @1 @1
--   rowT   = rowT_off @227736 @1
--   tabf = map (map length . group) $ tail (tabf @30308)

-- instance OEIS 227791 where
--   oeisIx n = (oeisIx @227550) (2 * n) n

-- instance OEIS 227836 where
--   oeisIx = (oeisIx @7814) . (oeisIx @214551)

-- instance OEIS 227837 where
--   oeisIx = (oeisIx @7949) . (oeisIx @214551)

-- instance OEIS 227838 where
--   oeisIx = (oeisIx @7814) . (oeisIx @5132)

-- instance OEIS 227839 where
--   oeisIx = (oeisIx @7949) . (oeisIx @5132)

-- instance OEIS 227862 where
--   oeis = tablList @227862
-- instance Table 227862 where
--   tabl = map snd $ iterate ox (False, [1]) where
--      ox (turn, xs) = (not turn, if turn then reverse ys else ys)
--         where ys = scanl (+) 1 (if turn then reverse xs else xs)

-- instance OEIS 227876 where
--   oeisIx n = fst $ until (null . snd) h (0, (rowT @31298) n) where
--               h (s, ds) = (s + sum ds, map abs $ zipWith (-) ds $ tail ds)

-- instance OEIS 227878 where
--   oeis = f (oeis @51701) where
--      f (p:ps@ (_:p':_)) = if p == p' then p : f ps else f ps

-- instance OEIS 227915 where
--   oeis = filter ((== 4) . (oeisIx @228085)) [1..]

-- instance OEIS 227928 where
--   oeis = 1 : f 0 0 (tail (oeis @79)) (tail (oeis @244)) where
--      f x y us'@ (u:us) vs'@ (v:vs)
--        | x > 0     = u : f 0 (u - x + y) us vs'
--        | y > v - u = v : f (v + x - y) 0 us' vs
--        | otherwise = u : f 0 (u + y) us vs'

-- instance OEIS 227944 where
--   oeisIx n = fst $
--               until ((== 1) . snd) (\ (i, x) -> (i + 1, (oeisIx @53575) x)) (0, n)

-- instance OEIS 227946 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @227944))

-- instance OEIS 227953 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @70965))

-- instance OEIS 227954 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @70965)) . negate

-- instance OEIS 228053 where
--   oeis = tablList @228053
-- instance Table 228053 where
--   tabl = iterate (\row@ (i:_) -> zipWith (+)
--      ([- i] ++ tail row ++ [0]) ([0] ++ init row ++ [- i])) [- 1]
--

-- instance OEIS 228057 where
--   oeis = filter odd (oeis @228056)

-- instance OEIS 228058 where
--   oeis = filter f [1, 3 ..] where
--      f x = length us == 1 && not (null vs) &&
--            fst (head us) `mod` 4 == 1 && snd (head us) `mod` 4 == 1
--            where (us,vs) = partition (odd . snd) $
--                            zip (oeisIx_row x) (oeisIx_row x)

-- instance OEIS 228074 where
--   oeis = tablList @228074
-- instance Table 228074 where
--   tabl = map fst $ iterate
--      (\ (u:_, vs) -> (vs, zipWith (+) ([u] ++ vs) (vs ++ [1]))) ([0], [1,1])

-- instance OEIS 228078 where
--   oeisIx = subtract 1 . (oeisIx @99036)

-- instance OEIS 228082 where
--   oeis = 0 : filter ((> 0) . (oeisIx @228085)) [1..]

-- instance OEIS 228085 where
--   oeisIx n = genericLength $ filter ((== n) . (oeisIx @92391)) [n - (oeisIx @70939) n .. n]

-- instance OEIS 228088 where
--   oeis = 0 : filter ((== 1) . (oeisIx @228085)) [1..]

-- instance OEIS 228276 where
--   oeis = 1 : f 1 [2..] where
--      f x zs = g zs where
--        g (y:ys) = if null $ show (x + y) \\ (show x ++ show y)
--                      then y : f y (delete y zs) else g ys

-- instance OEIS 228340 where
--   oeis = tablList @228340
-- instance Table 228340 where
--   rowCol = rowCol_off @228340 @1 @0
--   rowT   = rowT_off   @228340 @1
--   tabl = map (reverse . fst) $ iterate f ([1], [1,0]) where
--      f (us, vs'@ (_ : vs@ (v : _))) = (vs', ws) where
--        ws = 1 : (v + 1) : zipWith (+) us (map (* (v + 2)) vs)

-- instance Table 228351 where
--   rowT 0 = []
--   rowT n = (oeisIx @1511) n : (rowT @228351) (n `div` 2^ (oeisIx @1511 n))
-- instance OEIS 228351 where
--   oeis = concatMap (rowT @228351) [1..]

-- instance OEIS 228369 where
--   oeis = concatMap (rowT @228369) [1..]
--   oeisIx_row 0 = []
--   oeisIx_row n
--     | 2^k == 2 * n + 2 = [k - 1]
--     | otherwise        = (rowT @228369) (n `div` 2^k) ++ [k] where
--       k = (oeisIx @7814) (n + 1) + 1

-- instance OEIS 228446 where
--   oeisIx n = head
--      [q | let m = 2 * n + 1,
--           q <- map (m -) $ reverse $ takeWhile (< m) $ tail (oeis @2378),
--           oeisIx q == 1]

-- instance OEIS 228643 where
--   oeis = tablList @228643
-- instance Table 228643 where
--   rowCol = rowCol_off @228643 @1 @1
--   rowT   = rowT_off   @228643 @1
--   tabl = map fst $ iterate
--      (\ (row, x) -> (scanl (+) (x * (x - 1) + 1) row, x + 1)) ([1], 2)

-- instance OEIS 229037 where
--   import Data.IntMap (empty, (!), insert)
--   oeisIx n = (oeis @229037) !! (n - 1)
--   oeis = f 0 empty  where
--      f i m = y : f (i + 1) (insert (i + 1) y m) where
--        y = head [z | z <- [1..],
--                      all (\k -> z + m ! (i - k) /= 2 * m ! (i - k `div` 2))
--                          [1, 3 .. i - 1]]

-- instance OEIS 229109 where
--   oeisIx n  = (oeisIx @1221) n + n

-- instance OEIS 229139 where
--   oeisIx 1 = 0
--   oeisIx n = head $
--      dropWhile (== 0) $ map (oeisIx . (t -) . (^ 2)) [s, s - 1 ..]
--      where t = (oeisIx @45) (2 * n - 1); s = (oeisIx @196) t

-- instance OEIS 229363 where
--   oeis = f "" [0, 2 ..] where
--      f xs (e:es) = if null $ intersect xs ys then e : f ys es else f xs es
--                    where ys = show e

-- instance OEIS 229364 where
--   oeis = f "" [1, 3 ..] where
--      f xs (o:os) = if null $ intersect xs ys then o : f ys os else f xs os
--                    where ys = show o

-- instance OEIS 230091 where
--   oeis = filter ((== 2) . (oeisIx @228085)) [1..]

-- instance OEIS 230092 where
--   oeis = filter ((== 3) . (oeisIx @228085)) [1..]

-- instance OEIS 230094 where
--   oeis = filter ((== 2) . (oeisIx @230093)) [0..]

-- instance OEIS 230097 where
--   oeis = 0 : f 0 0 where
--      f i m = if v > m then i : f (i + 1) v else f (i + 1) m
--              where v = (oeisIx @159918) i

-- instance OEIS 230107 where
--   oeisIx = fromMaybe (-1) . f (10^5) 1 1 1 where
--      f k i u j v | k <= 0    = Nothing
--                  | u < v     = f (k - 1) (i + 1) (oeisIx u) j v
--                  | u > v     = f (k - 1) i u (j + 1) (oeisIx v)
--                  | otherwise = Just j

-- instance OEIS 230116 where
--   oeisIx = foldr (\u v-> 2*v + u) 0 . map toInteger . (rowT @166360)

-- instance OEIS 230286 where
--   oeisIx = (flip div 3) . (oeisIx @16052)

-- instance OEIS 230287 where
--   oeis = zipWith (-) (tail (oeis @230286)) (oeis @230286)

-- instance OEIS 230504 where
--   oeisIx n = head $ filter ((== 1) . (oeisIx @10051)') rs where
--                      rs = n : zipWith (+) rs (zipWith gcd rs [2..])

-- instance OEIS 230584 where
--   oeis = 2 : 3 : concat
--                  (transpose [drop 2 (oeis @59100), drop 2 (oeis @8865)])

-- instance OEIS 230585 where
--   oeisIx 1 = 3
--   oeisIx n = (oeisIx @7318) (2*n) n - (oeisIx @7318) (2*n) (n+2)

-- instance OEIS 230641 where
--   oeisIx n = (oeisIx @53735) n + n

-- instance OEIS 230709 where
--   oeis = filter (\x -> (oeisIx @10060) x * x `mod` 2 == 0) [0..]

-- instance OEIS 230720 where
--   oeisIx = (oeisIx @3071) . (oeisIx @92246)
--   oeis = filter even (oeis @3071)

-- instance OEIS 230721 where
--   oeisIx = (oeisIx @3071) . (oeisIx @230709) . (+ 1)
--   oeis = filter odd (oeis @3071)

-- instance OEIS 230780 where
--   oeis = filter (all (/= 1) . map (flip mod 6) . (rowT @27748)) [1..]

-- instance OEIS 230871 where
--   data Dtree = Dtree Dtree (Integer, Integer) Dtree
--   oeisIx n k = (tabf @230871) !! n !! k
--   oeisIx_row n = (tabf @230871) !! n
--   oeisIx_tabf = [0] : map (map snd) (rows $ deleham (0, 1)) where
--      rows (Dtree left (x, y) right) =
--           [ (x, y)] : zipWith (++) (rows left) (rows right)
--      deleham (x, y) = Dtree
--              (deleham (y, y + x)) (x, y) (deleham (y, 3 * y - x))

-- instance OEIS 230872 where
--   oeis = f [] (tabf @231330) where
--      f ws (xs:xss) = us ++ f (merge vs xs) xss where
--        (us,vs) = span (< head xs) ws
--      merge us [] = us
--      merge [] vs = vs
--      merge us'@ (u:us) vs'@ (v:vs)
--           | u < v = u : merge us vs'
--           | u > v = v : merge us' vs
--           | otherwise = u : merge us vs

-- instance OEIS 230873 where
--   oeis = f [0..] (oeis @230872) where
--      f (u:us) vs'@ (v:vs) = if u == v then f us vs else u : f us vs'

-- instance OEIS 230950 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @10060)

-- instance OEIS 230951 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @10059)

-- instance OEIS 230952 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @120)

-- instance OEIS 230953 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ tail (oeis @40)

-- instance OEIS 230954 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @2808)

-- instance OEIS 230955 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @18252)

-- instance OEIS 230957 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @9)

-- instance OEIS 230958 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @1285)

-- instance OEIS 230960 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @142)

-- instance OEIS 230961 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ tail (oeis @142)

-- instance OEIS 231179 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) [0..]

-- instance OEIS 231200 where
--   oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ [0, 2 ..]

-- instance OEIS 231330 where
--   oeis = tablList @231330
-- instance Table 231330 where
--   tabf = map (sort . nub) (tabf @230871)

-- instance OEIS 231331 where
--   oeisIx = genericLength . (rowT @231330)

-- instance OEIS 231335 where
--   oeisIx = genericLength . filter ((== 1) . (oeisIx @10056)) . (rowT @231330)

-- instance OEIS 232359 where
--   (oeis, (oeis @232361)) = unzip $
--      f 2 (tail (oeis @232221)) $ zipWith (-) (tail (oeis @232221)) (oeis @232221)
--      where f x (y:ys) (u:ws@ (v:_))
--              | u > 0 && v < 0 = (x, y) : f (x + 1) ys ws
--              | otherwise      = f (x + 1) ys ws

-- instance OEIS 232361 where

-- instance OEIS 232501 where
--   oeis = filter f [1..] where
--      f x = all ((== 1) . (oeisIx @10054)) $ init $ sort $
--            map (abs . (x -) . (^ 2) . (+ (oeisIx x))) [-1..2]

-- instance OEIS 232608 where
--   oeis = filter f $ tail (oeis @217) where
--      f x = all ((== 1) . (oeisIx @10054)) $ init $ sort $
--            map (abs . (x -) . (^ 2) . (+ (oeisIx x))) [-1..2]

-- instance OEIS 232642 where
--   import Data.List.Ordered (member); import Data.List (sort)
--   oeisIx n k = (tabf @232642) !! (n - 1) !! (k-1)
--   oeisIx_row n = (tabf @232642) !! (n - 1)
--   oeisIx_tabf = f (tabf @82560) [] where
--      f (xs:xss) zs = ys : f xss (sort (ys ++ zs)) where
--        ys = [v | v <- xs, not $ member v zs]
--   oeis = concat (tabf @232642)

-- instance OEIS 232643 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @232642))

-- instance OEIS 232991 where
--   oeisIx = (0 ^) . subtract 1 . gcd 6 . (+ 1)
--   oeis = cycle [1,0,0,0,1,0]

-- instance OEIS 233281 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @1177)) [1..]

-- instance OEIS 233734 where
--   oeisIx n = (oeisIx @19538) (2 * n - 1) n

-- instance OEIS 233836 where
--   oeis = map length $ group (oeis @4539)

-- instance OEIS 234098 where
--   oeis = filter ((== 1) . (oeisIx @10051)') $
--                         map ((flip div 2) . (+ 1)) (oeis @46388)

-- instance OEIS 234324 where
--   oeisIx n = (oeisIx @8955) (2 * n) n

-- instance OEIS 234575 where
--   oeis = tablList @234575
-- instance Table 234575 where
--   rowCol = rowCol_off @234575 @1 @1
--   rowT   = rowT_off   @234575 @1
--   tabl = zipWith (zipWith (+)) (tabl @48158) (tabl @10766)

-- instance OEIS 234586 where
--   oeis = concat (transpose [oeis, [2, 4 ..]])
--   oeis = 1 : 1 : (drop 2 $
--                  map abs $ zipWith (-) (oeis @234586) $ tail (oeis @234586))

-- instance OEIS 234587 where

-- instance OEIS 234814 where
--   oeis = filter (\x -> x `mod` (oeisIx @7953) x == 0 &&
--                                x `mod` (oeisIx @10888) x /= 0) [1..]

-- instance OEIS 234932 where
--   oeis = 1 : f 1 [2..] where
--      f x zs = g zs where
--        g (y:ys) = if null $ show (x * y) \\ (show x ++ show y)
--                      then y : f y (delete y zs) else g ys

-- instance OEIS 234950 where
--   oeis = tablList @234950
-- instance Table 234950 where
--   rowCol n k = sum [oeisIx s k * (oeisIx @9766) n s | s <- [k..n]]
--   rowT n = map (oeisIx n) [0..n]
--   tabl = map (rowT @234950) [0..]

-- instance OEIS 235049 where
--   oeisIx x = if x == 0 then 0 else 10 * (oeisIx @235049) x' + max 0 (d - 1)
--               where (x', d) = divMod x 10

-- instance OEIS 235168 where
--   oeis = tablList @235168
-- instance Table 235168 where
--   rowCol n k = (rowT @235168) n !! k
--   rowT 0 = [0]
--   rowT n = t n $ reverse $ takeWhile (<= n) (oeis @2110)
--      where t 0 []     = []
--            t x (b:bs) = x' : t m bs where (x', m) = divMod x b
--   tabf = map (rowT @235168) [0..]

-- instance OEIS 235224 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @2110)

-- instance OEIS 235249 where
--   oeisIx n = if y == n then n else (oeisIx @235249) y  where y = (oeisIx @1175) n

-- instance OEIS 235353 where
--   oeis = filter (\x -> mod x (oeisIx x) == 0) (oeis @7694)

-- instance OEIS 235540 where
--   oeis = filter ((== 0) . (oeisIx @10051)') (oeis @158034)

-- instance OEIS 235708 where
--   oeisIx n = f n where
--      f 1 = 1
--      f b = if isPandigital b n then b else f (b - 1) where
--            isPandigital b = (== b) . length . nub . unfoldr
--              (\x -> if x == 0 then Nothing else Just $ swap $ divMod x b)

-- instance OEIS 235726 where
--   a 1 = 1
--   a 4 = 2
--   a n = head $ filter (`notElem` disallowedValues) [1..] where
--     disallowedValues = map a $ (n - 1) : filter (<n) sums where
--       sums = map divisorSum divisors where
--         divisors = filter (\d -> n `mod` d == 0) [1..n]
--         divisorSum d = d + n `div` d

-- instance OEIS 235775 where
--   oeisIx = (oeisIx @47842) . (oeisIx @47842)

-- instance OEIS 235991 where
--   oeis = filter (odd . (oeisIx @3415)) [0..]

-- instance OEIS 235992 where
--   oeis = filter (even . (oeisIx @3415)) [0..]

-- instance OEIS 236076 where
--   oeis = tablList @236076
-- instance Table 236076 where
--   tabl = [1] : [0, 2] : f [1] [0, 2] where
--      f us vs = ws : f vs ws where
--        ws = [0] ++ zipWith (+) (zipWith (+) ([0] ++ us) (us ++ [0])) vs

-- instance OEIS 236246 where
--   oeis = filter ((== 1) . (oeisIx @229037)) [1..]

-- instance OEIS 236341 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @160855))

-- instance OEIS 236473 where
--   oeisIx = p (oeis @7422) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 236563 where
--   oeisIx n = foldl lcm 1 $ zipWith (\p e -> p ^ (e + 1) * (p - 1))
--                                     (oeisIx_row n) (oeisIx_row n)

-- instance OEIS 237056 where
--   oeis = concat $ transpose [oeis, (oeis @192607)]

-- instance OEIS 237058 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @237056))

-- instance OEIS 237126 where
--   oeis = 0 : es where
--      es = 1 : concat (transpose [map (oeisIx @192607) es, map (oeisIx . (+ 1)) es])

-- instance OEIS 237347 where
--   oeis = zipWith (-) (tail (oeis @78633)) (oeis @78633)

-- instance OEIS 237424 where
--   oeisIx = flip div 3 . (+ 1) . (oeisIx @52216)

-- instance OEIS 237427 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @237126))

-- instance OEIS 237709 where
--   oeis = map length $ group (oeis @188666)

-- instance OEIS 237739 where
--   oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @71574))

-- instance OEIS 237851 where
--   oeis = 1 : f 1 [2..] where
--      f x zs = g zs where
--        g (u:us) | all ((== 0) . (mod u)) ds = u : f u (delete u zs)
--                 | otherwise = g us
--                 where ds = dropWhile (<= 1) $
--                            sort $ nub $ map (read . return) $ show x

-- instance OEIS 237860 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @237851))

-- instance OEIS 238056 where
--   oeis = filter ((== 1) . length . f) (oeis @40) where
--     f x = filter (\ (us, vs) ->
--                  head vs /= '0' &&
--                  (oeisIx @10051)' (read us :: Integer) == 1 &&
--                  (oeisIx @10051)' (read vs :: Integer) == 1) $
--                  map (flip splitAt $ show x) [1 .. length (show x) - 1]

-- instance OEIS 238057 where
--   oeis = filter ((== 2) . length . f) (oeis @40) where
--     f x = filter (\ (us, vs) ->
--                  head vs /= '0' &&
--                  (oeisIx @10051)' (read us :: Integer) == 1 &&
--                  (oeisIx @10051)' (read vs :: Integer) == 1) $
--                  map (flip splitAt $ show x) [1 .. length (show x) - 1]

-- instance OEIS 238246 where
--   oeis = filter ((== 3) . (oeisIx @72219)) [1..]

-- instance OEIS 238247 where
--   oeis = filter ((== 5) . (oeisIx @72219)) [1..]

-- instance OEIS 238248 where
--   oeis = filter ((== 7) . (oeisIx @72219)) [1..]

-- instance OEIS 238327 where
--   oeis = iterate ((+ 2) . (oeisIx @151800)) 1

-- instance OEIS 238332 where
--   oeis = f [] $ drop 4 (oeis @40) where
--      f xs (p:ps) | (oeisIx @10051) t == 1 || t `elem` xs = f xs ps
--                  | otherwise = p : f (t:xs) ps
--                  where t = read $ tail $ show p

-- instance OEIS 238333 where
--   oeis = f [] $ drop 4 (oeis @40) where
--      f xs (p:ps) | (oeisIx @10051) t == 1 || t `elem` xs = f xs ps
--                  | otherwise = t : f (t:xs) ps
--                  where t = read $ tail $ show p

-- instance OEIS 238453 where
--   oeis = tablList @238453
-- instance Table 238453 where
--   tabl = [1] : f [1] (oeis @10) where
--      f xs (z:zs) = (map (div y) $ zipWith (*) ys $ reverse ys) : f ys zs
--        where ys = y : xs; y = head xs * z

-- instance OEIS 238497 where
--   oeis = filter ((== 1) . (oeisIx @212793)) $ tail (oeis @45)

-- instance OEIS 238498 where
--   oeis = tablList @238498
-- instance Table 238498 where
--   tabl = [1] : f [1] (oeis @1615) where
--      f xs (z:zs) = (map (div y) $ zipWith (*) ys $ reverse ys) : f ys zs
--        where ys = y : xs; y = head xs * z

-- instance OEIS 238525 where
--   oeisIx n = mod n $ (oeisIx @1414) n

-- instance OEIS 238593 where
--   oeisIx n = (+ 1) $ fromJust $ findIndex
--      (isInfixOf $ show $ (oeisIx @40) n) (scanl1 (++) $ map show (oeis @40))

-- instance OEIS 238685 where
--   oeisIx n = (oeisIx @142) n * (oeisIx @8275) (2 * n - 1) n

-- instance OEIS 238689 where
--   oeisIx_row 1 = [1]
--   oeisIx_row n = a n [] (oeis @40) where
--     a m factors ps@ (p:ps')
--       | m == 1         = factors
--       | m `mod` p == 0 = a (m `div` p) (p : factors) ps
--       | otherwise      = a m           factors       ps'
--   a _ _ [] = []

-- instance OEIS 238704 where
--   oeis = 1 : f 1 [2..] where
--      f x zs = g zs where
--        g (y:ys) =
--          if y `mod` 2 /= m then g ys else y : f y (delete y zs)
--        m = (oeisIx @30) x `mod` 2

-- instance OEIS 238718 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @238704))

-- instance OEIS 238778 where
--   oeisIx n = sum $ filter ((== 1) . (oeisIx @10051)') $
--      map (2 * n -) $ takeWhile (<= 2 * n) (oeis @40)

-- instance OEIS 238845 where
--   oeisIx n = genericLength $ takeWhile (== 0) $ zipWith (-) (bin n) (bin (n+1))
--   where bin = reverse . unfoldr
--   (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

-- instance OEIS 238862 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @239965))

-- instance OEIS 238880 where
--   oeis = f [0..] where
--      f (u:us) = u : g us where
--        g vs = h vs where
--          h (w:ws) = if reverse ys == ys then w : f (delete w vs) else h ws
--                     where ys = xs ++ show w
--        xs = show u

-- instance OEIS 238985 where
--   import Data.Set (singleton, deleteFindMin, fromList, union)
--   oeisIx n = (oeis @238985) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @168046)) $ f $ singleton 1 where
--      f s = x : f (s' `union` fromList
--                  (filter ((> 0) . (`mod` 10)) $ map (* x) [2,3,5,7]))
--                  where (x, s') = deleteFindMin s

-- instance OEIS 239070 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @132995)) . (oeisIx @5117)

-- instance OEIS 239122 where
--   oeis = drop 2 $ scanl1 (+) (oeis @61019)

-- instance OEIS 239293 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = head [c | c <- (oeis @2808), powerMod n c c == n]

-- instance OEIS 239324 where
--   oeis = scanl (+) 0 (oeis @90431)

-- instance OEIS 239433 where
--   oeis = filter
--      (\z -> any (== z) $ map (oeisIx @3415) $ takeWhile (<= (oeisIx @2620) z) (oeis @13929)) [2..]

-- instance OEIS 239452 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = head [m | m <- [2..], powerMod m n n == mod m n]

-- instance OEIS 239508 where
--   oeisIx = p (oeis @469) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 239509 where
--   oeisIx = p (oeis @469) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 239549 where
--   oeis = 0 : 1 : zipWith (+)
--                  (map (* 8) $ tail (oeis @239549)) (map (* 12) (oeis @239549))

-- instance OEIS 239585 where
--   oeisIx = (oeisIx @20639) . (oeisIx @78972)

-- instance OEIS 239586 where
--   oeisIx n = (oeisIx @78972) n `div` (oeisIx @239585) n

-- instance OEIS 239634 where
--   oeisIx = (oeisIx @30) . (oeisIx @1358)

-- instance OEIS 239636 where
--   oeisIx = subtract 1 . (* 2) . (oeisIx @14689)

-- instance OEIS 239639 where
--   oeis = map length $ group (oeis @239634)

-- instance OEIS 239656 where
--   oeis = zipWith (-) (tail (oeis @7304)) (oeis @7304)

-- instance OEIS 239664 where
--   oeisIx n = (oeis @239664) `genericIndex` (n - 1)
--   oeis = 1 : f 1 [2..] where
--      f v ws = g ws where
--        g (x:xs) = if gcd v x == 1 && ((intersect `on` show) v x == "")
--                      then x : f x (delete x ws) else g xs

-- instance OEIS 239673 where
--   (oeis, (oeis @239674)) = unzip $ (12, 1) : f 1 12 (oeis @239656) where
--      f i v (q:qs) | q > v = (q, i) : f (i + 1) q qs
--                   | otherwise = f (i + 1) v qs

-- instance OEIS 239674 where

-- instance OEIS 239728 where
--   import Data.Map (singleton, findMin, deleteMin, insert)
--   oeisIx n = (oeis @239728) !! (n - 1)
--   oeis = f 9 (3, 2) (singleton 4 (2, 2)) where
--      f zz (bz, be) m
--       | xx < zz && gcd 6 be > 1 =
--                   f zz (bz, be+1) (insert (bx*xx) (bx, be+1) $ deleteMin m)
--       | xx < zz = xx :
--                   f zz (bz, be+1) (insert (bx*xx) (bx, be+1) $ deleteMin m)
--       | xx > zz = f (zz+2*bz+1) (bz+1, 2) (insert (bz*zz) (bz, 3) m)
--       | otherwise = f (zz + 2 * bz + 1) (bz + 1, 2) m
--       where (xx, (bx, be)) = findMin m

-- instance OEIS 239740 where
--   oeisIx n = gcd (sum fs) (product fs)
--               where fs = take n $ tail (oeis @45)

-- instance OEIS 239826 where
--   oeisIx n = sum $
--               filter ((flip isPrefixOf `on` (rowT @30308)) n) $ (rowT @27750) n

-- instance OEIS 239870 where
--   import Data.Map (singleton, findMin, deleteMin, insert)
--   oeisIx n = (oeis @239870) !! (n - 1)
--   oeis = f 9 (3, 2) (singleton 4 (2, 2)) where
--      f zz (bz, ez) m
--       | xx < zz = if ex `mod` 3 > 0
--         then xx : f zz (bz, ez+1) (insert (bx*xx) (bx, ex+1) $ deleteMin m)
--         else      f zz (bz, ez+1) (insert (bx*xx) (bx, ex+1) $ deleteMin m)
--       | xx > zz = if ez `mod` 3 > 0
--         then zz : f (zz+2*bz+1) (bz+1, 2) (insert (bz*zz) (bz, 3) m)
--         else      f (zz+2*bz+1) (bz+1, 2) (insert (bz*zz) (bz, 3) m)
--       | otherwise = f (zz+2*bz+1) (bz+1, 2) m
--       where (xx, (bx, ex)) = findMin m

-- instance OEIS 239878 where
--   oeis = elemIndices 1 (oeis @240752)

-- instance OEIS 239930 where
--   oeisIx = sum . map (oeisIx @240025) . (rowT @27750)

-- instance OEIS 239943 where
--   oeis = [x | x <- [1..], (oeisIx @239965) x == x]

-- instance OEIS 239968 where
--   oeis = unfoldr c (1, 1, (oeis @18252)) where
--      c (i, z, xs'@ (x:xs)) | i == x = Just (z, (i + 1, z + 1, xs))
--                           | i /= x = Just (0, (i + 1, z, xs'))

-- instance OEIS 239969 where
--   oeisIx n = head [k | k <- [1..],
--                         (oeisIx @10054) (oeisIx n + (oeisIx @217) (n + k)) == 1]

-- instance OEIS 239970 where
--   oeisIx n = head [k | k <- [1..],
--                         (oeisIx @10054) (oeisIx k + (oeisIx @217) (n + k)) == 1]

-- instance OEIS 240052 where
--   oeisIx = (oeisIx @68346) . (oeisIx @6094)

-- instance OEIS 240162 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 3 (oeisIx $ (oeisIx @10) n) n

-- instance OEIS 240236 where
--   oeis = tablList @240236
-- instance Table 240236 where
--   rowCol = rowCol_off @240236 @1 @1
--   rowT   = rowT_off   @240236 @1
--   tabl = zipWith (map . flip q)
--                          [2..] (map tail $ tail (tabl @2260)) where
--      q b n = if n < b then n else q b n' + d where (n', d) = divMod n b

-- instance OEIS 240277 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @7456))

-- instance OEIS 240400 where
--   oeis = filter ((> 0) . (oeisIx @241759)) [0..]

-- instance OEIS 240508 where
--   oeisIx = genericLength . (rowT @174382)

-- instance OEIS 240595 where
--   oeis = tablList @240595
-- instance Table 240595 where
--   rowCol = rowCol_off @240595 @1 @1
--   rowT   = rowT_off @240595 @1
--   tabf = iterate f [1] where
--      f xs = concat [map length zss, map head zss]
--             where zss = group $ sort xs

-- instance OEIS 240752 where
--   oeis = zipWith (-) (tail (oeis @4159)) (oeis @4159)

-- instance OEIS 240754 where
--   oeis = elemIndices (-1) (oeis @240752)

-- instance OEIS 240769 where
--   oeis = tablList @240769
-- instance Table 240769 where
--   rowCol = rowCol_off @240769 @1 @1
--   rowT   = rowT_off   @240769 @1
--   tabl = iterate (\ (x:xs) -> xs ++ [2*x, 2*x-1]) [1]

-- instance OEIS 240807 where
--   oeis = -1 : -1 : 2 : zipWith (+) xs (tail xs)
--      where xs = map (oeisIx @240807) $ zipWith (-) [1..] $ tail (oeis @240807)

-- instance OEIS 240808 where
--   oeis = 2 : 1 : 0 : zipWith (+) xs (tail xs)
--      where xs = map (oeisIx @240808) $ zipWith (-) [1..] $ tail (oeis @240808)

-- instance OEIS 240844 where
--   oeisIx = p $ drop 3 (oeis @73) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 240857 where
--   oeis = tablList @240857
-- instance Table 240857 where
--   tabl = iterate (\ (x:xs) -> xs ++ [x, x + 1]) [0]

-- instance OEIS 240883 where
--   oeisIx n = (oeisIx @240857) (2 * n) n

-- instance OEIS 240913 where
--   oeis = filter (not . elem '1' . show) (oeis @69715)

-- instance OEIS 240923 where
--   oeisIx (succ->n) = numerator sq - (oeisIx @203) (denominator sq)
--      where sq = (oeisIx @203) n % n

-- instance OEIS 240952 where
--   oeisIx = fromJust . (`elemIndex` (oeis @245575))

-- instance OEIS 240960 where
--   oeis = filter (\x -> (oeisIx @51612) x == (oeisIx @110088) x) [1..]

-- instance OEIS 240993 where
--   oeisIx n = (oeisIx @142) (n + 1) * (oeisIx @2109) n

-- instance OEIS 241012 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @109465))

-- instance OEIS 241023 where
--   oeisIx n = (oeisIx @102413) (2 * n) n

-- instance OEIS 241218 where
--   oeisIx = fromJust . (`elemIndex` (oeis @240808))

-- instance OEIS 241235 where
--   oeis = map length $ group (oeis @6949)

-- instance OEIS 241241 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @241241) !! (n - 1)
--   oeis = 0 : 1 : f (singleton 2) where
--      f s = m : f (insert (oeisIx m) $ insert (oeisIx m) s')
--            where (m, s') = deleteFindMin s

-- instance OEIS 241418 where
--   oeis = zipWith (-) (tail (oeis @99054)) (oeis @99054)

-- instance OEIS 241426 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 . concat . inits .
--             unfoldr (\x -> if x == 0 then Nothing
--                                      else Just $ swap $ divMod x 2)

-- instance OEIS 241582 where
--   (oeis, (oeis @241583)) =  unzip $ f [1..] (oeis @131644) (-1) where
--      f (x:xs) (y:ys) r = if y > r then (y, x) : f xs ys y else f xs ys r

-- instance OEIS 241583 where

-- instance OEIS 241664 where
--   oeisIx n = fst $ until ((<= 1) . snd)
--                           (\ (u, v) -> (u + 1, (oeisIx @58026) v)) (0, n)

-- instance OEIS 241671 where
--   oeis = filter ((== 0) . (oeisIx @65806)) [1..]

-- instance OEIS 241673 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @229037)) [1..]

-- instance OEIS 241752 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @229037))

-- instance OEIS 241759 where
--   oeisIx = p $ tail (oeis @1047) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 241766 where
--   oeisIx = p $ tail (oeis @1047) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 241772 where
--   oeis = zipWith (-) (tail (oeis @65094)) (oeis @65094)

-- instance OEIS 241783 where
--   oeis = filter ((== 0) . (oeisIx @241759)) [0..]

-- instance OEIS 241816 where
--   oeisIx n = f (oeisIx_row n) [] where
--      f [] _ = n
--      f (0 : 1 : us) vs = foldr (\b y -> 2 * y + b) 0 $
--                                reverse vs ++ 1 : 0 : us
--      f (u : us) vs     = f us (u : vs)

-- instance OEIS 241887 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @65806))

-- instance OEIS 241909 where
--   oeisIx 1 = 1
--   oeisIx n = product $ zipWith (^) (oeis @40) $ zipWith (-) is (1 : is)
--               where is = reverse ((j + 1) : js)
--                     (j:js) = reverse $ map (oeisIx @49084) $ (rowT @27746) n

-- instance OEIS 241917 where
--   oeisIx n = i - j where
--               (i:j:_) = map (oeisIx @49084) $ reverse (1 : (rowT @27746) n)

-- instance OEIS 241919 where
--   oeisIx 1 = 0
--   oeisIx n = i - j where
--               (i:j:_) = map (oeisIx @49084) $ reverse (1 : (rowT @27748) n)

-- instance OEIS 241944 where
--   oeisIx = sum . (rowT @27420)

-- instance OEIS 242094 where
--   oeis = c [1..] (oeis @3249) where
--      c (v:vs) ws'@ (w:ws) = if v == w then c vs ws else v : c vs ws'

-- instance OEIS 242114 where
--   oeis = tablList @242114
-- instance Table 242114 where
--   rowCol = rowCol_off @242114 @1 @1
--   rowT   = rowT_off   @242114 @1
--   tabl = map (map (oeisIx @18805)) (tabl @10766)

-- instance OEIS 242179 where
--   oeisIx  n k = (tabf @242179) !! n !! n
--   oeisIx_row n = (tabf @242179) !! n
--   oeisIx_tabf = iterate (concatMap (\x -> [-x, x])) [1] :: (Num t => [[t]])
--   oeis = concat (tabf @242179)

-- instance OEIS 242183 where
--   oeis = concatMap (\ (r,x) -> take r [x,x..]) $
--                            zip (oeis @242192) [1..]

-- instance OEIS 242186 where
--   oeis = filter ((> 1) . (oeisIx @242192)) [1..]

-- instance OEIS 242192 where
--   oeisIx n = sum $ map (oeisIx . (n ^ 4 -)) $
--                         takeWhile (< n ^ 4) $ map (^ 3) [1..]

-- instance OEIS 242311 where
--   oeisIx = maximum . (rowT @96145)

-- instance OEIS 242312 where
--   oeis = tablList @242312
-- instance Table 242312 where
--   tabl = map (map (oeisIx @10888)) (tabl @7318)

-- instance OEIS 242314 where
--   oeisIx = maximum . (rowT @242312)

-- instance OEIS 242342 where
--   oeisIx n = if n <= 2 then 0 else (oeisIx @7318)' n (oeisIx n)

-- instance OEIS 242399 where
--   oeisIx n = foldr (\t v -> 3 * v + t) 0 $
--                     map (flip mod 3) $ zipWith (+) ([0] ++ ts) (ts ++ [0])
--               where ts = (rowT @30341) n

-- instance OEIS 242400 where
--   oeisIx n = (oeisIx @8586) n - (oeisIx @242399) n

-- instance OEIS 242401 where
--   oeis = filter ((== 0) . (oeisIx @10054)) (oeis @37)

-- instance OEIS 242407 where
--   oeis = filter ((== 0) . (oeisIx @242400)) [0..]

-- instance OEIS 242408 where
--   oeis = filter ((> 0) . (oeisIx @242400)) [0..]

-- instance OEIS 242411 where
--   oeisIx 1 = 0
--   oeisIx n = i - j where
--               (i:j:_) = map (oeisIx @49084) $ ps ++ [p]
--               ps@ (p:_) = reverse $ (rowT @27748) n

-- instance OEIS 242535 where
--   oeis = f [1..] where
--      f xs'@ (x:xs) = x : f (xs \\ [z, 2 * z]) where z = xs' !! x

-- instance OEIS 242614 where
--   oeis = tablList @242614
-- instance Table 242614 where
--   rowCol n k = (rowT @242614) n !! (k-1)
--   rowT n = filter ((== n) . (oeisIx @7953)) [n .. (oeisIx @2275) n]
--   tabf = map (rowT @242614) [0..]

-- instance OEIS 242622 where
--   oeisIx = genericLength . (rowT @242614)

-- instance OEIS 242885 where
--   oeisIx n = head [k | let nn = n ^ n,
--                         k <- [1..], mod (k ^ k + nn) (k + n) == 0]

-- instance OEIS 242901 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @242885))

-- instance OEIS 243355 where
--   oeis = filter
--      (\x -> null $ show x `intersect` (show $ (oeisIx @40) x)) [1..]

-- instance OEIS 243451 where
--   oeis = [x | x <- (oeis @241751), (oeisIx @10051)' x == 1]

-- instance OEIS 243757 where
--   oeis = scanl (*) 1 (oeis @60904)

-- instance OEIS 244080 where
--   oeisIx = (oeisIx @6530) . (oeisIx @166133)

-- instance OEIS 244112 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = read $ concat $
--      zipWith ((++) `on` show) (map length xs) (map head xs)
--      where xs = group $ reverse $ sort $ map (read . return) $ show n

-- instance OEIS 244365 where
--   oeis = tablList @244365
-- instance Table 244365 where
--   rowCol = rowCol_off @244365 @1 @1
--   rowT   = rowT_off @244365 @1
--   tabf = zipWith farideh (map (+ 1) (oeis @40)) (oeis @249669)
--                  where farideh u v = filter ((== 1) .  (oeisIx @10051)') [u..v]

-- instance OEIS 244408 where
--   oeis = map (* 2) $ filter f [2..] where
--      f x = sqrt (fi $ 2 * x) <= fi (oeisIx x)

-- instance OEIS 244477 where
--   oeis = 3 : 2 : 1 : zipWith (+)
--      (map (oeisIx @244477) $ zipWith (-) [4..] $ tail (oeis @244477))
--      (map (oeisIx @244477) $ zipWith (-) [4..] $ drop 2 (oeis @244477))

-- instance OEIS 244478 where
--   oeis = 2 : 0 : 2 : zipWith (+) xs (tail xs)
--      where xs = map (oeisIx @244478) $ zipWith (-) [1..] $ tail (oeis @244478)

-- instance OEIS 244479 where
--   oeisIx = (`div` 2) . (oeisIx @244478)

-- instance OEIS 244483 where
--   oeis = 3 : 1 : 0 : zipWith (+) xs (tail xs)
--      where xs = map (oeisIx @244483) $ zipWith (-) [1..] $ tail (oeis @244483)

-- instance OEIS 244724 where
--   oeis = 1 : f 1 [2..] where
--      f x xs = f' xs where
--        f' (u:us) | (oeisIx @10051)' (x + u) == 1 = g u (delete u xs)
--                  | otherwise             = f' us where
--           g y ys = g' ys where
--             g' (v:vs) | (oeisIx @10051)' (y + v) == 0 = u : v : f v (delete v ys)
--                       | otherwise        = g' vs

-- instance OEIS 244731 where
--   oeis = [x | x <- [1..], (oeisIx @244724) x == x]

-- instance OEIS 244732 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @244724))

-- instance OEIS 244747 where
--   oeis = findIndices ((== 1) . (oeisIx @209229)) (oeis @51145)

-- instance OEIS 245022 where
--   oeis = filter ((== 3) . (oeisIx @2635)) [0..]

-- instance OEIS 245057 where
--   oeisIx = fromJust . (`elemIndex` (oeis @249129))

-- instance OEIS 245066 where
--   oeisIx n = (oeisIx @1497) (2 * n) n

-- instance OEIS 245097 where
--   oeisIx n = sum $ map (oeisIx @10051)' [n + 1 .. (oeisIx @7535) n]

-- instance OEIS 245193 where
--   oeisIx n = head [p | p <- (oeis @40),
--                         (isSuffixOf `on` show) (oeisIx n) p]

-- instance OEIS 245234 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @238880))

-- instance OEIS 245300 where
--   oeis = tablList @245300
--   rowCol n k = (n + k) * (n + k + 1) `div` 2 + k
--   rowT n = map (oeisIx n) [0..n]
--   tabl = map (rowT @245300) [0..]
--   oeis = concat (tabl @245300)

-- instance OEIS 245304 where
--   oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051)') $
--      iterate (zipWith (+) [1, 1, 1, 1, 1]) [1, 3, 7, 9, 13]

-- instance OEIS 245305 where
--   oeis = map ((`div` 4) . (subtract 1) . head) $
--      filter (all (== 1) . map (oeisIx @10051)') $
--             iterate (zipWith (+) [4, 4, 6]) [1, 3, 5]

-- instance OEIS 245334 where
--   oeis = tablList @245334
-- instance Table 245334 where
--   tabl = iterate (\row@ (h:_) -> (h + 1) : map (* h) row) [1]

-- instance OEIS 245340 where
--   import Data.IntMap (singleton, member, (!), insert)
--   oeisIx n = (oeis @245340) !! n
--   oeis = 0 : f [1..] [1..] 0 (singleton 0 0) where
--      f us'@ (u:us) vs'@ (v:vs) w m
--        | u `member` m = (m ! u) : f us vs' w m
--        | otherwise    = g (reverse[w-v,w-2*v..1] ++ [w+v,w+2*v..]) where
--        g (x:xs) = if x `member` m then g xs else f us' vs x $ insert x v m

-- instance OEIS 245394 where
--   (oeis, (oeis @245395)) =  unzip $ f [0..] (oeis @125717) (-1) where
--      f (x:xs) (y:ys) r = if y > r then (y,x) : f xs ys y else f xs ys r

-- instance OEIS 245395 where

-- instance OEIS 245396 where
--   oeisIx n = (oeisIx @244365) n (oeisIx n)

-- instance OEIS 245471 where
--   oeis = concat $ transpose [odds (oeis @65621), [1..]]
--      where odds [] = []; odds [x] = []; odds (_:x:xs) = x : odds xs

-- instance OEIS 245492 where
--   oeis = [0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0] ++
--                  zipWith3 (((+) .) . (+))
--                  (drop 8 (oeis @245492)) (drop 10 (oeis @245492))
--                  (cycle [1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 2, 0, 0, 1, 0])

-- instance OEIS 245508 where
--   oeis = f (oeis @40) (oeis @1105) where
--      f ps'@ (p:ps) xs'@ (x:xs) = if p <= x then x : f ps xs' else f ps' xs

-- instance OEIS 245530 where
--   oeisIx = product . (rowT @245499)

-- instance OEIS 245543 where
--   oeis = scanl1 (+) (oeis @160239)

-- instance OEIS 245585 where
--   oeis = filter ((== 0) . (oeisIx @245575)) [0..]

-- instance OEIS 245586 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @228276))

-- instance OEIS 245644 where
--   oeis = filter ((== 1) . (oeisIx @245656) . (^ 3)) [1..]

-- instance OEIS 245656 where
--   oeisIx = (0 ^) . (oeisIx @54025) :: (Integral a, Integral t) => a -> t

-- instance OEIS 245717 where
--   oeis = tablList @245717
-- instance Table 245717 where
--   rowCol = rowCol_off @245717 @1 @1
--   rowT   = rowT_off   @245717 @1
--   tabl = zipWith (zipWith gcd) (tabl @2024) (tabl @133819)

-- instance OEIS 245718 where
--   oeisIx n = (oeisIx @245677) n `div` (oeisIx @245678) n

-- instance OEIS 245722 where
--   oeisIx = product . (rowT @244365)

-- instance OEIS 245729 where
--   oeis = filter f [1..] where
--                         f x = p ^ 2 < q && (oeisIx' q == 1 || f q)
--                               where q = div x p; p = (oeisIx @20639) x

-- instance OEIS 245826 where
--   oeis = tablList @245826
-- instance Table 245826 where
--   rowCol n k = n * k * (2 * n^2 * k^2 - n^2 - k^2) `div` 6
--   rowT n = map (oeisIx n) [1..n]
--   tabl = map (rowT @245826) [1..]

-- instance OEIS 245836 where
--   oeisIx = sum . (rowT @53398)

-- instance OEIS 245970 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 (phi + (oeisIx @245970) phi) n
--               where phi = (oeisIx @10) n

-- instance OEIS 245971 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 4 (phi + (oeisIx @245971) phi) n
--               where phi = (oeisIx @10) n

-- instance OEIS 246398 where
--   oeis = f 0 $ map show (oeis @40) where
--      f x pss = (length ps - length xs) :
--                f (x + 1) (dropWhile (== xs) pss)
--        where ps = head [qs | qs <- pss, isin xs qs]; xs = show x
--      isin [] _  = True
--      isin _  [] = False
--      isin (u:us) vs = not (null ws) && isin us ws
--                       where ws = dropWhile (/= u) vs

-- instance OEIS 246430 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @212300)) . (oeisIx @40)

-- instance OEIS 246431 where
--   oeisIx = fromJust . (`elemIndex` (oeis @101403))

-- instance OEIS 246433 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @113963))

-- instance OEIS 246435 where
--   oeisIx n = if n < 3 then 1 else (oeisIx @246435) (2 * div n 3) + 1

-- instance OEIS 246436 where
--   oeisIx n = genericLength $ [1..n] \\ genericIndex (tabf @220237) (n - 1)

-- instance OEIS 246438 where
--   oeis = filter ((== 0) . (oeisIx @164349)) [0..]

-- instance OEIS 246439 where
--   oeis = filter ((== 1) . (oeisIx @164349)) [0..]

-- instance OEIS 246517 where
--   oeis = filter ((== 1) . (oeisIx @10051)'' . (oeisIx @141036)) [0..]

-- instance OEIS 246518 where
--   oeis = filter ((== 1) . (oeisIx @10051)'') $ (oeis @141036)

-- instance OEIS 246520 where
--   oeisIx = maximum . (rowT @246830)

-- instance OEIS 246558 where
--   oeisIx = (oeisIx @7954) . (oeisIx @45)

-- instance OEIS 246588 where
--   oeisIx = product . map (oeisIx . length) .
--             filter ((== 1) . head) . group . (rowT @30308)

-- instance OEIS 246606 where
--   oeisIx n = (oeisIx @116853) (2 * n - 1) n

-- instance OEIS 246694 where
--   oeis = tablList @246694
-- instance Table 246694 where
--   tabl = [1] : [1,2] : f 1 2 [1,2] where
--      f i z xs = ys : f j (z + 1) ys where
--        ys = take (z + 1) $ map (+ 1) (xs !! (z - i) : xs !! (z - j) : ys)
--        j = 3 - i

-- instance OEIS 246700 where
--   oeis = tablList @246700
-- instance Table 246700 where
--   rowCol n k = genericIndex (tabf @246700) (n - 1) !! (k-1)
--   rowT n = genericIndex (tabf @246700) (n - 1)
--   tabf = [1] : f 2  where
--      f x = (x : (rowT @246700) (oeisIx x)) : f (x + 1)

-- instance OEIS 246701 where
--   oeis = zipWith (-) (tail (oeis @246520)) (oeis @246520)

-- instance OEIS 246704 where
--   oeis = filter (\x -> (oeisIx @113963) x == x) [1..]

-- instance OEIS 246776 where
--   oeisIx n = (oeisIx @249669) n - (oeisIx @40) (n + 1)

-- instance OEIS 246781 where
--   oeis = filter ((== 3) . (oeisIx @182134)) [1..]

-- instance OEIS 246782 where
--   oeis = filter ((== 2) . (oeisIx @182134)) [1..]

-- instance OEIS 246785 where
--   oeisIx n = if null ms then 0 else head ms
--               where ms = [m | m <- [1 .. n - 1], (oeisIx @182134) (n - m) == m]

-- instance OEIS 246830 where
--   oeis = tablList @246830
-- instance Table 246830 where
--   tabl = zipWith (zipWith f) (tabl @51162) (tabl @25581) where
--      f x y = foldr (\b v -> 2 * v + b) 0 $ x |+| y
--      (|+|) = (++) `on` (rowT @30308)
--   (oeisIx @246830) = []

-- instance OEIS 246878 where
--   oeis = 1 : f [1] (oeis @523) where
--      f xs (k:ks) = y : f (xs ++ [y]) ks where y = sum $ genericDrop k xs

-- instance OEIS 246999 where
--   oeisIx n = read $ s ++ "21" ++ s ++ "211" ++ s ++ "2" :: Integer
--               where s = replicate n '1'

-- instance OEIS 247023 where
--   oeis = tablList @247023
-- instance Table 247023 where
--   tabl = map reverse (tabl @201634)

-- instance OEIS 247061 where
--   oeis = [1,8,16,17] ++ zipWith (+)
--      (drop 3 (oeis @247061)) (zipWith (-) (tail (oeis @247061)) (oeis @247061))

-- instance OEIS 247062 where
--   oeis = [1,2,5,6,8,11,12,16,17] ++ zipWith (+)
--      (drop 8 (oeis @247062)) (zipWith (-) (tail (oeis @247062)) (oeis @247062))

-- instance OEIS 247065 where
--   oeis = [1,16,24,32,40,49,64,65] ++ zipWith (+)
--      (drop 7 (oeis @247065)) (zipWith (-) (tail (oeis @247065)) (oeis @247065))

-- instance OEIS 247066 where
--   oeis = [1,2,6,8,12,16,17,21,24,27,32,33] ++ zipWith (+)
--      (drop 11 (oeis @247066)) (zipWith (-) (tail (oeis @247066)) (oeis @247066))

-- instance OEIS 247073 where
--   oeis = tablList @247073
-- instance Table 247073 where
--   rowCol = rowCol_off @247073 @1 @1
--   tabl = map (rowT @247073) [1..]
--   rowT n = map length $ groupBy ((==) `on` fst) $ sort $
--      takeWhile ((<= 2^n). snd) $ tail $ zip (oeis @25474) (oeis @961)

-- instance OEIS 247095 where
--   oeisIx = (+ 5) . fromJust . (`elemIndex` (oeis @250030))

-- instance OEIS 247104 where
--   oeis = filter ((== 1) . (oeisIx @8966)) $ tail (oeis @3052)

-- instance OEIS 247108 where
--   oeis = tablList @247108
-- instance Table 247108 where
--   tabl = iterate (\row -> scanl (+) (- last row) row) [1]

-- instance OEIS 247143 where
--   oeis = [0..10] ++ f 11 (map show [11..]) where
--      f x zss = (read ys :: Int) : f (x + 1) (delete ys zss) where
--                ys = fromJust $ find (elem $ ds !! x) zss
--      ds = concatMap show (oeis @247143)

-- instance OEIS 247144 where
--   oeisIx = fromJust . (`elemIndex` (oeis @247143))

-- instance OEIS 247149 where
--   oeis = map digitToInt $ concatMap show (oeis @247143)

-- instance OEIS 247160 where
--   oeis = [1..14] ++ [16,17] ++ zipWith (+)
--      (drop 15 (oeis @247160)) (zipWith (-) (tail (oeis @247160)) (oeis @247160))

-- instance OEIS 247161 where
--   oeis = [1,2,4,5,6,8,9,11,12,13,16,17] ++ zipWith (+)
--      (drop 11 (oeis @247161)) (zipWith (-) (tail (oeis @247161)) (oeis @247161))

-- instance OEIS 247167 where
--   oeis = filter ((zipWith (==) [0..] (oeis @247143)) !!) [0..]

-- instance OEIS 247180 where
--   oeis = filter ((== 1) . (oeisIx @67029)) [1..]

-- instance OEIS 247199 where
--   oeis = filter f [1..] where
--      f x = 1 == denominator
--            (sum [v % w | (v:ws) <- tails $ reverse $ (rowT @27750) x, w <- ws])

-- instance OEIS 247204 where
--   oeis = filter ((zipWith (==) [1..] (oeis @250552)) !!) [1..]

-- instance OEIS 247233 where
--   oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @75323)) . (oeisIx @65091)

-- instance OEIS 247253 where
--   oeis = zipWith (-) (tail (oeis @251239)) (oeis @251239)

-- instance OEIS 247303 where
--   oeis = f [head (oeis @10059)] $ tail (oeis @10059) where
--      f xs (z:zs) = (sum $ zipWith (*) xs (reverse xs)) : f (z : xs) zs

-- instance OEIS 247358 where
--   oeis = tablList @247358
-- instance Table 247358 where
--   rowCol = rowCol_off @247358 @1 @1
--   rowT   = rowT_off   @247358 @1
--   tabl = map sort (tabl @51129)

-- instance OEIS 247363 where
--   oeisIx n = (oeisIx @247358) (2 * n - 1) n

-- instance OEIS 247364 where
--   oeis = tablList @247364
-- instance Table 247364 where
--   tabl = [1] : (map reverse (tabf @34928))

-- instance OEIS 247365 where
--   oeisIx n = (oeisIx @102473) (2 * n - 1) n

-- instance OEIS 247366 where
--   import Data.Set (Set, singleton, insert, deleteFindMin)
--   oeisIx n = (oeis @247366) !! (n - 1)
--   oeis = h $ singleton (1, 0, 0) where
--      h :: Set (Double, Int, Int) -> [Integer]
--      h s = (floor x) : h (insert (f i (j + 1)) $ insert (f (i + 1) j) s')
--            where ((x, i, j), s') = deleteFindMin s
--      f :: Int -> Int -> (Double, Int, Int)
--      f u v = (2 ^^ uh * 3 ^^ vh * g ur vr, u, v) where
--        g 0 0 = 1; g 0 1 = sqrt 3; g 1 0 = sqrt 2; g 1 1 = sqrt 6
--        (uh, ur) = divMod u 2; (vh, vr) = divMod v 2

-- instance OEIS 247367 where
--   oeisIx n = sum $ map ((1 -) . (oeisIx @10052) . (n -)) $
--                     takeWhile (<= n) (oeis @290)

-- instance OEIS 247379 where
--   oeisIx n = gcd n $ (oeisIx @64413) n

-- instance OEIS 247382 where
--   oeis = [-3, 7, 1, 46] ++ zipWith (flip div) (oeis @247382)
--      (zipWith (+)
--           (zipWith (*) (tail (oeis @247382)) (drop 3 (oeis @247382)))
--           (zipWith (*) (cycle [-1, 1]) (map (^ 2) $ drop 2 (oeis @247382))))

-- instance OEIS 247383 where
--   import Data.IntMap (empty, member, (!), insert)
--   oeisIx n = (oeis @247383) !! (n - 1)
--   oeis = f 1 1 empty where
--      f x z m | member x m = m ! x : f (x + 1) (x + 1) m
--              | member y m = f x (z + 1) m
--              | otherwise  = f x (z + 1) (insert y z m)
--              where y = (oeisIx @247379) z

-- instance OEIS 247414 where
--   oeis = zipWith (-) (tail (oeis @24431)) (oeis @24431)

-- instance OEIS 247419 where
--   oeis = concat $
--                  transpose [map (subtract 1) (oeis @3256), (oeis @3256)]

-- instance OEIS 247451 where
--   oeis = map (oeisIx @7947) (oeis @25487)

-- instance OEIS 247453 where
--   oeis = tablList @247453
-- instance Table 247453 where
--   tabl = zipWith (zipWith (*)) (tabl @109449) (tabl @97807)

-- instance OEIS 247462 where
--   oeisIx 1 = 1
--   oeisIx n = fst $ until ((== 1) . denominator . snd)
--                           (\ (i, x) -> (i + 1, f x)) (0, 1 % n) where
--      f x = (oeisIx @8472) x' % (oeisIx @1221) x' where x' = numerator x + denominator x

-- instance OEIS 247468 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247462))

-- instance OEIS 247485 where
--   oeisIx = (+ 1) . floor . (* 2) . sqrt . fi . (oeisIx @40)

-- instance OEIS 247499 where
--   oeisIx = sum . (rowT @247500)

-- instance OEIS 247500 where
--   oeis = tablList @247500
-- instance Table 247500 where
--   tabl = zipWith (zipWith div) (tabl @105278) (tabl @4736)

-- instance OEIS 247503 where
--   oeisIx = product . filter (odd . (oeisIx @49084)) . (rowT @27746)

-- instance OEIS 247514 where
--   oeis = filter (\x -> (oeisIx @117767) x == (oeisIx @247485) x) [1..]

-- instance OEIS 247515 where
--   oeis = filter (\x -> (oeisIx @117767) x < (oeisIx @247485) x) [1..]

-- instance OEIS 247540 where
--   oeis = 1 : 1 : zipWith (-)
--      (map (* 2) xs) (zipWith div (map ((* 3) . (^ 2)) xs) (oeis @247540))
--      where xs = tail (oeis @247540)

-- instance OEIS 247560 where
--   oeis = 1 : 1 : zipWith (-) (map (* 3) $ tail (oeis @247560))
--                                      (map (* 4) (oeis @247560))

-- instance OEIS 247563 where
--   oeis = 2 : 3 : zipWith (-) (map (* 3) $ tail (oeis @247563))
--                                      (map (* 4) (oeis @247563))

-- instance OEIS 247564 where
--   oeis = [2,1,3,1] ++ zipWith (-) (map (* 3) $ drop 2 (oeis @247564))
--                                           (map (* 4) $ (oeis @247564))

-- instance OEIS 247594 where
--   oeis = 1 : 2 : 5 : zipWith (+)
--      (tail $ zipWith (+) (oeis @247594) $ tail (oeis @247594))
--      (map (* 3) (oeis @247594))

-- instance OEIS 247595 where
--   oeis = 1 : 3 : 10 : map (* 4) (zipWith3 (((+) .) . (-))
--      (drop 2 (oeis @247595)) (tail (oeis @247595)) (oeis @247595))

-- instance OEIS 247628 where
--   oeis = filter f (oeis @216155) where
--      f x = any ((== 1) . (oeisIx @10057)) [oeisIx x .. (oeisIx @2378) x - 1]

-- instance OEIS 247647 where
--   oeisIx = (oeisIx @7088) . (oeisIx @247648)

-- instance OEIS 247648 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @247648) !! (n - 1)
--   oeis = f $ singleton 1 where
--      f s = x : f (insert (4 * x + 1) $ insert (2 * x + 1) s')
--            where (x, s') = deleteFindMin s

-- instance OEIS 247657 where
--   oeis = f 0 $ drop 2 (oeis @40) where
--      f z (p:ps) | (oeisIx @10051)' z' == 1 = z' : f z' (delete z' ps)
--                 | otherwise        = f z' ps
--                 where z' = z + p

-- instance OEIS 247714 where
--   oeisIx = (+ 1) . fromJust .
--                     (`elemIndex` (oeis @3586)) . (oeis !!)

-- instance OEIS 247750 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247750) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [4, 9, 2, 1, 0, 8, 5, 7, 6, 3]

-- instance OEIS 247751 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247751) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [1, 5, 4, 9, 0, 8, 6, 7, 2, 3]

-- instance OEIS 247752 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247752) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [8, 1, 3, 9, 0, 2, 4, 5, 6, 7]

-- instance OEIS 247753 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247753) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [8, 2, 3, 6, 4, 0, 7, 5, 9, 1]

-- instance OEIS 247754 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247754) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [5, 2, 8, 9, 4, 7, 6, 3, 1, 0]

-- instance OEIS 247755 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247755) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [8, 3, 1, 5, 9, 0, 6, 7, 4, 2]

-- instance OEIS 247756 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247756) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [1, 3, 6, 7, 2, 9, 4, 0, 8, 5]

-- instance OEIS 247757 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247757) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [5, 2, 9, 8, 4, 6, 7, 3, 1, 0]

-- instance OEIS 247758 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247758) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [2, 9, 8, 4, 5, 6, 7, 3, 1, 0]

-- instance OEIS 247759 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247759) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [8, 1, 5, 4, 0, 6, 7, 9, 2, 3]

-- instance OEIS 247760 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247760) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [4, 2, 9, 1, 8, 5, 6, 7, 3, 0]

-- instance OEIS 247761 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247761) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [8, 2, 9, 0, 1, 5, 7, 3, 4, 6]

-- instance OEIS 247762 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247762) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--                   fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--            where (x, s') = deleteFindMin s
--      digs = [9, 2, 1, 0, 8, 5, 7, 6, 4, 3]

-- instance OEIS 247764 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   oeisIx n = (oeis @247764) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s = x : f (s' `union`
--            fromList (map (+ 10 * x) $ dropWhile (/= mod x 10) digs))
--        where (x, s') = deleteFindMin s
--      digs = [6, 5, 1, 9, 4, 2, 8, 0, 3, 7]

-- instance OEIS 247765 where
--   oeis = tablList @247765
-- instance Table 247765 where
--   rowCol = rowCol_off @247765 @1 @1
--   tabf = map (rowT @247765) [1..]
--   rowT n = f (map recip [2..]) (n % (n + 1)) where
--      f es x | numerator x == 1 = [denominator x]
--             | otherwise        = g es
--             where g (u:us) | u <= x    = (denominator u) : f us (x - u)
--                            | otherwise =  g us

-- instance OEIS 247793 where
--   oeis = 2 : f (zip [2..] $ tail (oeis @40)) where
--      f ((x, p) : xps) = m : f xps where
--        m = head [y | y <- [1..], (p + (oeisIx @40) y) `mod` (oeisIx @720) (x * y) == 0]

-- instance OEIS 247795 where
--   oeis = tablList @247795
-- instance Table 247795 where
--   rowCol = rowCol_off @247795 @1 @1
--   rowT   = rowT_off @247795 @1
--   tabf = map (map (flip mod 2)) (tabf @27750)

-- instance OEIS 247797 where
--   oeis = f 1 $ zip (oeis @40) (oeis @7605) where
--      f q' vws = g vws where
--        g  ((p,q):pqs) = if gcd q q' == 1
--                            then p : f q (delete (p,q) vws) else g pqs

-- instance OEIS 247798 where
--   oeisIx n = (oeisIx @77581) (2 * n - 1) n

-- instance OEIS 247799 where
--   oeis = 0 : f 1 [0] where
--      f x zs@ (z:_) = y : f (x + 1) (y : zs) where
--                     y = z + maybe x id (elemIndex x $ reverse zs)

-- instance OEIS 247800 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247800) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [4, 9, 2, 1, 0, 8, 5, 7, 6, 3]

-- instance OEIS 247801 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247801) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [1, 5, 4, 9, 0, 8, 6, 7, 2, 3]

-- instance OEIS 247802 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247802) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [8, 1, 3, 9, 0, 2, 4, 5, 6, 7]

-- instance OEIS 247803 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247803) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [8, 2, 3, 6, 4, 0, 7, 5, 9, 1]

-- instance OEIS 247804 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247804) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [5, 2, 8, 9, 4, 7, 6, 3, 1, 0]

-- instance OEIS 247805 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247805) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [8, 3, 1, 5, 9, 0, 6, 7, 4, 2]

-- instance OEIS 247806 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247806) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [1, 3, 6, 7, 2, 9, 4, 0, 8, 5]

-- instance OEIS 247807 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247807) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [5, 2, 9, 8, 4, 6, 7, 3, 1, 0]

-- instance OEIS 247808 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247808) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [2, 9, 8, 4, 5, 6, 7, 3, 1, 0]

-- instance OEIS 247809 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247809) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [8, 1, 5, 4, 0, 6, 7, 9, 2, 3]

-- instance OEIS 247810 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247810) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [4, 2, 9, 1, 8, 5, 6, 7, 3, 0]

-- instance OEIS 247811 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247811) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [8, 2, 9, 0, 1, 5, 7, 3, 4, 6]

-- instance OEIS 247812 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247812) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [9, 2, 1, 0, 8, 5, 7, 6, 4, 3]

-- instance OEIS 247813 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247813) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [0, 5, 4, 2, 9, 8, 6, 7, 3, 1]

-- instance OEIS 247814 where
--   import Data.IntSet (fromList, deleteFindMin, union)
--   import qualified Data.IntSet as Set (null)
--   oeisIx n = (oeis @247814) !! (n - 1)
--   oeis = 0 : f (fromList [1..9]) where
--      f s | Set.null s = []
--          | otherwise  = x : f (s' `union`
--            fromList (map (+ 10 * x) $ tail $ dropWhile (/= mod x 10) digs))
--          where (x, s') = deleteFindMin s
--      digs = [6, 5, 1, 9, 4, 2, 8, 0, 3, 7]

-- instance OEIS 247815 where
--   oeisIx = sum . map (oeisIx @10051)' . (rowT @77581)

-- instance OEIS 247824 where
--   oeis = f ips where
--      f ((x, p) : xps) = head
--        [y | (y, q) <- ips, (p + q) `mod` (x + y) == 0] : f xps
--      ips = zip [1..] (oeis @40)

-- instance OEIS 247857 where
--   oeis = concat $ zipWith replicate (oeis @256852) (oeis @40)

-- instance OEIS 247869 where
--   oeisIx n = (oeisIx (oeisIx n) + (oeisIx @40) n) `div` (oeisIx n + n)

-- instance OEIS 247879 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @248025))

-- instance OEIS 247892 where
--   oeisIx n = n - (oeisIx @247815) n

-- instance OEIS 247894 where
--   oeisIx = (oeisIx @196) . (oeisIx @10807)

-- instance OEIS 247896 where
--   oeis = filter f (oeis @40) where
--      f p = any ((== 1) . (oeisIx @10051)') $
--                map (+ p) $ filter (> 0) $ map (read . return) $ show p

-- instance OEIS 248012 where
--   oeisIx = foldr1 (^) . (rowT @27748)

-- instance OEIS 248013 where
--   oeis = filter (\x -> (oeisIx @247796) x == x) [0..]

-- instance OEIS 248014 where
--   oeis = filter (\x -> (oeisIx @247796) x < x) [0..]

-- instance OEIS 248024 where
--   oeis = 1 : f 1 [2..] where
--     f x zs = g zs where
--       g (y:ys) = if x `mod` (oeisIx y) == 0
--                  then y : f y (delete y zs) else g ys

-- instance OEIS 248025 where
--   oeis = 1 : f 1 [2..] where
--     f x zs = g zs where
--       g (y:ys) = if (oeisIx @30) y == (oeisIx @10888) x
--                  then y : f y (delete y zs) else g ys

-- instance OEIS 248043 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @248024))

-- instance OEIS 248045 where
--   oeisIx n = (oeisIx @891) (n - 1) * (oeisIx @142) n

-- instance OEIS 248098 where
--   oeis = 1 : 1 : 1 : map (+ 1) (zipWith3 (((+) .) . (+))
--                  (oeis @248098) (tail (oeis @248098)) (drop 2 (oeis @248098)))

-- instance OEIS 248101 where
--   oeisIx = product . filter (even . (oeisIx @49084)) . (rowT @27746)

-- instance OEIS 248110 where
--   oeis = tablList @248110
-- instance Table 248110 where
--   rowCol = rowCol_off @248110 @1 @1
--   rowT   = rowT_off @248110 @1
--   tabf = map (\x -> [x + 1 .. x + (oeisIx @7953) x]) [1 ..]

-- instance OEIS 248122 where
--   a 0 = 0; a 1 = 0;
--   a n = 3 * a (n - 1) + 3^ceiling (n % 2) - a (ceiling (n % 2))

-- instance OEIS 248131 where
--   oeis = 1 : (map (* 3) $
--                  concatMap (map (read . return) . show) (oeis @248131))

-- instance OEIS 248141 where
--   oeis = tablList @248141
-- instance Table 248141 where
--   rowCol = rowCol_off @248141 @1 @1
--   rowT   = rowT_off @248141 @1
--   tabf = map concat usss where
--      usss = iterate f [[1]] where
--        f vss = group [1 .. last (last vss) + 1] ++
--                map (\ws -> ws ++ [last ws + 1]) vss

-- instance OEIS 248147 where
--   oeis = tablList @248147
-- instance Table 248147 where
--   rowCol = rowCol_off @248147 @1 @1
--   rowT   = rowT_off @248147 @1
--   tabf = map concat psss where
--      psss = iterate f [[2]] where
--         f pss = group (h $ last pss) ++ map h pss
--         h ws = ws ++ [oeisIx $ last ws]

-- instance OEIS 248164 where
--   oeis = tablList @248164
-- instance Table 248164 where
--   rowCol = rowCol_off @248164 @1 @1
--   rowT   = rowT_off @248164 @1
--   tabf = map (map product) psss where
--      psss = iterate f [[2]] where
--         f pss = group (h $ last pss) ++ map h pss
--         h ws = ws ++ [oeisIx $ last ws]

-- instance OEIS 248327 where
--   oeisIx 0 = 0
--   oeisIx n = levenshtein (show n) (dropWhile (== '0') $ reverse $ show n)
--   levenshtein :: (Eq t) => [t] -> [t] -> Int
--   levenshtein us vs = last $ foldl transform [0..length us] vs where
--      transform xs@ (x:xs') c = scanl compute (x+1) (zip3 us xs xs') where
--         compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

-- instance OEIS 248336 where
--   oeisIx = fromJust . (`elemIndex` map (oeisIx @248327) [0..])

-- instance OEIS 248353 where
--   oeis = filter k [1..] where
--      k x = elem x $ map (uncurry (+)) $
--            takeWhile ((> 0) . fst) $ map (divMod (x ^ 2)) (oeis @11557)

-- instance OEIS 248378 where
--   oeis = concat $ transpose [oeis, tail (oeis @127423)]

-- instance OEIS 248387 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247665)) . (oeisIx @40)

-- instance OEIS 248479 where
--   oeis = 1 : 3 : zipWith ($) (map uncurry $ cycle [ (-), (*)])
--                                      (zip (tail (oeis @248479)) (oeis @248479))

-- instance OEIS 248518 where
--   oeisIx = p $ tail (oeis @52383) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 248519 where
--   oeisIx = p $ tail (oeis @52383) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 248574 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @27306) (n - 1) + (oeisIx @27306) n

-- instance OEIS 248663 where
--   oeisIx = foldr (xor) 0 . map (\i -> 2^ (i - 1)) . (rowT @112798)

-- instance OEIS 248906 where
--   oeisIx = sum . map ((2 ^) . subtract 2 . (oeisIx @95874)) . tail . (rowT @210208)

-- instance OEIS 248907 where
--   oeisIx = (oeisIx @32810) . (oeisIx @185969)

-- instance OEIS 248918 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247665)) . (oeisIx @961) . (+ 1)

-- instance OEIS 248940 where
--   oeis = (rowT @248939) 7

-- instance OEIS 248941 where
--   oeis = (rowT @248939) 17

-- instance OEIS 248942 where
--   oeis = (rowT @248939) 20

-- instance OEIS 248952 where
--   import Data.IntSet (singleton, member, insert, findMin, findMax)
--   oeisIx n = (oeis @248952) !! n
--   (oeis, (oeis @248953)) = unzip $
--      map (\x -> minmax 1 x $ singleton x) [0..] where
--      minmax _ 0 s = (findMin s, findMax s)
--      minmax k x s = minmax (k + 1) y (insert y s) where
--                            y = x + (if (x - j) `member` s then j else -j)
--                            j = k * signum x

-- instance OEIS 248953 where

-- instance OEIS 248973 where
--   oeis = tablList @248973
-- instance Table 248973 where
--   tabf = map (scanl1 (+)) (tabf @248939)

-- instance OEIS 249032 where
--   oeis = zipWith (-) (tail (oeis @75326)) (oeis @75326)

-- instance OEIS 249034 where
--   oeis = filter odd (oeis @171946)

-- instance OEIS 249039 where
--   oeis = 1 : 2 : f 2 2 1 1 where
--      f x u v w = y : f (x + 1) y (v + 1 - mod y 2) (w + mod y 2)
--                  where y = u + (oeisIx @249039) (x - v) + (oeisIx @249039) (x - w)

-- instance OEIS 249040 where
--   oeis = tail $ scanl (\i j -> i + 1 - mod j 2) 0 (oeis @249039)

-- instance OEIS 249041 where
--   oeis = tail $ scanl (\i j -> i + mod j 2) 0 (oeis @249039)

-- instance OEIS 249043 where
--   oeis = iterate (oeisIx @62028) 42

-- instance OEIS 249044 where
--   oeis = filter ((== 0) . (oeisIx @10051)') $ tail (oeis @3052)

-- instance OEIS 249045 where
--   oeis = filter ((== 0) . flip mod 3) (oeis @3052)

-- instance OEIS 249046 where
--   oeis = filter ((> 0) . flip mod 9) (oeis @249045)

-- instance OEIS 249047 where
--   oeis = filter ((> 0) . flip mod 3) (oeis @3052)

-- instance OEIS 249048 where
--   oeis = filter ((== 0) . flip mod 9) (oeis @3052)

-- instance OEIS 249053 where
--   import Data.Map (singleton, findMin, delete, insert)
--   oeisIx n = (oeis @249053) !! (n - 1)
--   oeis = 1 : f 1 1 (oeis @2808) (singleton 1 1) where
--      f x z cs m
--        | k == x    = p : f (x + 1) p cs (insert (x + p) 0 $ delete x m)
--        | otherwise = c : f (x + 1) c cs' (insert (x + c) 0 m)
--        where p = (oeisIx @7918) z
--              (c:cs') = dropWhile (<= z) cs
--              (k,_) = findMin m

-- instance OEIS 249054 where
--   import Data.Map (singleton, findMin, delete, insert)
--   oeisIx n = (oeis @249054) !! (n - 1)
--   oeis = 1 : f 1 (oeis @40) (oeis @2808) (singleton 1 1) where
--      f x ps'@ (p:ps) cs'@ (c:cs) m
--        | k == x    = p : f (x + 1) ps cs' (insert (x + p) 0 $ delete x m)
--        | otherwise = c : f (x + 1) ps' cs (insert (x + c) 0 m)
--        where (k,_) = findMin m

-- instance OEIS 249133 where
--   oeis = tablList @249133
-- instance Table 249133 where
--   tabf = map (map (flip mod 2)) (tabf @249095)

-- instance OEIS 249167 where
--   oeis = 1 : 2 : 3 : f 2 3 [4..] where
--      f u v ws = g ws where
--        g (x:xs) | null (intersect fdx $ (rowT @213925) u) ||
--                   not (null $ intersect fdx $ (rowT @213925) v) = g xs
--                 | otherwise =  x : f v x (delete x ws)
--                 where fdx = (rowT @213925) x

-- instance OEIS 249183 where
--   oeisIx = foldr (\b v -> 10 * v + b) 0 . (rowT @249133)

-- instance OEIS 249184 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 . (rowT @249133)

-- instance OEIS 249278 where
--   oeis = 0 : f 0 [1..] where
--      f u vs = g vs where
--        g (x:xs) = if (oeisIx x) `mod` 2 == u `mod` 2
--                      then x : f x (delete x vs) else g xs

-- instance OEIS 249279 where
--   oeisIx = fromJust . (`elemIndex` (oeis @249278))

-- instance OEIS 249304 where
--   oeisIx n = if n == 0 then 0 else (oeisIx @48967) n + (oeisIx @48967) (n - 1)

-- instance OEIS 249307 where
--   oeis = tablList @249307
-- instance Table 249307 where
--   tabf = map (zipWith (*) (oeis @79)) (tabf @249095)

-- instance OEIS 249308 where
--   oeisIx n = (oeisIx @249307) n n

-- instance OEIS 249343 where
--   oeisIx = (oeisIx @7949) . (oeisIx @1142)

-- instance OEIS 249346 where
--   oeisIx = (oeisIx @122841) . (oeisIx @1142)

-- instance OEIS 249406 where
--   oeis = 1 : f [2..] where
--      f ws@ (u:v:_) = y : f (ws \\ [u, v, y]) where y = u * v

-- instance OEIS 249407 where
--   oeis = f [2..] where
--      f ws@ (u:v:_) = u : v : f (ws \\ [u, v, u * v])

-- instance OEIS 249408 where
--   oeis = filter ((== 0) . (oeisIx @5369)) (oeis @249406)

-- instance OEIS 249411 where
--   oeis = filter ((== 1) . (oeisIx @5369)) (oeis @249407)

-- instance OEIS 249566 where
--   oeis = filter ((== 4) . (oeisIx @182134)) [1..]

-- instance OEIS 249569 where
--   oeis = 1 : 1 : zipWith (+)
--      (map (oeisIx @249569) $ zipWith (-) [3..] $ tail (oeis @249569))
--      (map (oeisIx @249569) $ zipWith (-) [3..] $ map (oeisIx @249569) (oeis @249569))

-- instance OEIS 249571 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054))

-- instance OEIS 249575 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @84937)) [1..]

-- instance OEIS 249591 where
--   oeis = 1 : f 1 [2..] where
--      f x zs = g zs where
--        g (y:ys) = if null $ show y `intersect` show (x - 1)
--                      then g ys else y : f y (delete y zs)

-- instance OEIS 249594 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . (oeisIx @40)

-- instance OEIS 249595 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249054)) . (oeisIx @18252)

-- instance OEIS 249602 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @84937)) [1..]

-- instance OEIS 249603 where
--   oeisIx = flip mod 3 . (oeisIx @84937)

-- instance OEIS 249626 where
--   oeis = f (zip [0,0..] [0..9]) (tabf @31298) where
--      f acds@ ((_,dig):_) zss = g zss where
--        g (ys:yss) = if dig `elem` ys
--                        then y : f acds' (delete ys zss) else g yss
--          where y = foldr (\d v -> 10 * v + d) 0 ys
--                acds' = sortBy (compare `on` fst) $
--                       addd (sortBy (compare `on` snd) acds)
--                            (sortBy (compare `on` snd) $
--                                    zip (map length gss) (map head gss))
--                addd cds [] = cds
--                addd []   _ = []
--                addd ((c, d) : cds) yys'@ ((cy, dy) : yys)
--                     | d == dy  = (c + cy, d) : addd cds yys
--                     | otherwise = (c, d) : addd cds yys'
--                gss = sortBy compare $ group ys

-- instance OEIS 249629 where
--   a 0 = 0; a 1 = 0;
--   a n = 4 * a (n - 1) + 4^ceiling (n % 2) - a (ceiling (n % 2))

-- instance OEIS 249638 where
--   a 0 = 0; a 1 = 0;
--   a n = 5 * a (n - 1) + 5^ceiling (n % 2) - a (ceiling (n % 2))

-- instance OEIS 249648 where
--   oeisIx = fromJust . (`elemIndex` (oeis @249626)) . (oeisIx @11540)

-- instance OEIS 249669 where
--   oeisIx n = floor $ fi (oeisIx n) ** (1 + recip (fi n))

-- instance OEIS 249680 where
--   oeisIx = (oeisIx @84937) . (+ 1) . (* 3)

-- instance OEIS 249681 where
--   oeisIx = (oeisIx @84937) . (+ 2) . (* 3)

-- instance OEIS 249682 where
--   oeisIx = (oeisIx @84937) . (* 3)

-- instance OEIS 249683 where
--   oeisIx = flip div 2 . (oeisIx @249681)

-- instance OEIS 249684 where
--   oeis = filter ((== 0) . (oeisIx @249777)) [1..]

-- instance OEIS 249694 where
--   oeis = zipWith gcd (drop 3 (oeis @84937)) (oeis @84937)

-- instance OEIS 249830 where
--   oeis = filter ((== 0) . (oeisIx @249832)) [0..]

-- instance OEIS 249854 where
--   oeis = sortBy (compare `on` show) $
--                         takeWhile (<= 10^6) (oeis @93018)

-- instance OEIS 249855 where
--   oeis = sortBy (compare `on` f) $ takeWhile (<= 10^6) (oeis @93018)
--                  where f x = (head $ reverse ds, ds) where ds = show x

-- instance OEIS 249858 where
--   oeisIx n = (oeisIx @249857) n - (oeisIx @249856) n

-- instance OEIS 249873 where
--   oeisIx n = if n == 0 then 0 else 100*oeisIx n' + 10*oeisIx (2*t) + d
--               where (n', td) = divMod n 100; (t, d) = divMod td 10

-- instance OEIS 249900 where
--   oeis = [1..4] ++ concatMap (uncurry (++))
--             (f [2] [3,4] (drop 2 (oeis @40)) (tail (oeis @2808))) where
--      f us@ (u:_) vs ps'@ (p:p':ps) cs'@ (c:c':cs)
--        | (oeisIx @10051) u == 1 = g ([c] ++ us ++ [c']) vs ps' cs
--        | otherwise      = g ([p] ++ us ++ [p']) vs ps cs'
--      g us vs@ (v:_) (p:ps) (c:cs) = (us, ws) : f us ws ps cs where
--        ws = if (oeisIx @10051) v == 1 then [c] ++ vs ++ [p] else [p] ++ vs ++ [c]

-- instance OEIS 249918 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @203069))

-- instance OEIS 249920 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @55266))

-- instance OEIS 249951 where
--   oeis = filter ((== 1) . (oeisIx @10051)'' . (oeisIx @113630)) [1..]

-- instance OEIS 250007 where
--   oeis = map length $ group $ map (oeisIx @100618) [1..]

-- instance OEIS 250030 where
--   oeisIx n = snd $ until ((== 5) . fst)
--                     (\ (x, s) -> (oeisIx x, s + 1)) (oeisIx n, 1)

-- instance OEIS 250402 where
--   oeis = filter ((== 0) . (oeisIx @247149)) [0..]

-- instance OEIS 250403 where
--   oeis = filter ((== 1) . (oeisIx @247149)) [0..]

-- instance OEIS 250552 where
--   oeisIx = (oeisIx @49084) . (oeisIx @247797)

-- instance OEIS 250553 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @250552))

-- instance OEIS 251045 where
--   oeisIx = (oeisIx @7913) . (oeisIx @98550)

-- instance OEIS 251046 where
--   oeisIx = (oeisIx @7913) . (oeisIx @98548)

-- instance OEIS 251089 where
--   oeisIx = (oeisIx @7947) . (oeisIx @98550)

-- instance OEIS 251090 where
--   oeisIx = (oeisIx @7947) . (oeisIx @98548)

-- instance OEIS 251101 where
--   oeisIx = (oeisIx @20639) . (oeisIx @98550)

-- instance OEIS 251102 where
--   oeis = zipWith gcd (drop 2 (oeis @98550)) (oeis @98550)

-- instance OEIS 251103 where
--   oeisIx = (oeisIx @6530) . (oeisIx @98550)

-- instance OEIS 251104 where
--   oeisIx = (oeisIx @6530) . (oeisIx @98548)

-- instance OEIS 251138 where
--   oeisIx = (oeisIx @1221) . (oeisIx @98550)

-- instance OEIS 251139 where
--   oeisIx = (oeisIx @1221) . (oeisIx @98548)

-- instance OEIS 251140 where
--   oeisIx = (oeisIx @1222) . (oeisIx @98550)

-- instance OEIS 251141 where
--   oeisIx = (oeisIx @1222) . (oeisIx @98548)

-- instance OEIS 251239 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @98550)) [1..]

-- instance OEIS 251240 where
--   oeis = filter ((== 2) . (oeisIx @62799) . (oeisIx @98550)) [1..]

-- instance OEIS 251241 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @98550)) [1..]

-- instance OEIS 251391 where
--   oeis = filter ((== 1) . (oeisIx @8966) . (oeisIx @98550)) [1..]

-- instance OEIS 251392 where
--   oeis = filter ((== 0) . (oeisIx @10051)' . (oeisIx @98550)) [1..]

-- instance OEIS 251393 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @98550)) [1..]

-- instance OEIS 251394 where
--   oeis = filter ((== 1) . gcd 6 . (oeisIx @98550)) [1..]

-- instance OEIS 251412 where
--   oeis = iterate (oeisIx @98550) 11

-- instance OEIS 251535 where
--   oeis = fst (oeisIx @98548)_bisect
--   oeisIx_bisect = cleave (oeis @98548) where
--      cleave xs = (evens xs, odds xs) where
--         evens [] = []; evens [x] = [x]; evens (x:_:xs) = x : evens xs
--         odds [] = []; odds [x] = []; odds (_:x:xs) = x : odds xs

-- instance OEIS 251536 where
--   oeis = map (flip div 2) $ snd (oeisIx @98548)_bisect

-- instance OEIS 251537 where
--   oeis = filter (\x -> (oeisIx @98548) (x + 2) > (oeisIx @98548) x + 6) [1, 3 ..]

-- instance OEIS 251538 where
--   oeis = filter (\x -> (oeisIx @98548) (2*x+3) > (oeisIx @98548) (2*x+1) + 6) [1..]

-- instance OEIS 251539 where
--   oeis = zipWith (-) (tail (oeis @251538)) (oeis @251538)

-- instance OEIS 251540 where
--   oeis = filter ((> 0) . flip mod 3 . (oeisIx @98548)) [1, 3 ..]

-- instance OEIS 251542 where
--   oeis = [div u v | (u, v) <- zip (drop 2 (oeis @98550)) (oeis @98550),
--                             (oeisIx @10051)' v == 1]

-- instance OEIS 251546 where
--   oeisIx n = head $ [2, 4 ..] \\ filter even (take n (oeis @98550))

-- instance OEIS 251547 where
--   oeisIx = flip div 2 . (oeisIx @251546)

-- instance OEIS 251548 where
--   oeis = map length $ group $ map (oeisIx @251546) [1..]

-- instance OEIS 251549 where
--   oeisIx n = head $ [1, 3 ..] \\ filter odd (take n (oeis @98550))

-- instance OEIS 251550 where
--   oeisIx = flip div 2 . subtract 1 . (oeisIx @251549)

-- instance OEIS 251551 where
--   oeisIx n = (oeisIx @251546) n - (oeisIx @251549) n

-- instance OEIS 251552 where
--   oeisIx = flip div 2 . subtract 1 . (oeisIx @251551)

-- instance OEIS 251553 where
--   oeis = filter ((== 0) . flip mod 3 . (oeisIx @98550)) [1..]

-- instance OEIS 251557 where
--   oeis = map (+ 2) $ tail $ scanl maxEven 0 (oeis @98550)
--                  where maxEven u v = if even v then max u v else u

-- instance OEIS 251558 where
--   oeis = 9 : 9 : 9 : f 2 3 [4..] (tail (oeis @14076)) where
--      f u v ws zs = g ws where
--        g (x:xs) = if gcd x u > 1 && gcd x v == 1
--                      then y : f v x (delete x ws) ys else g xs
--                   where ys@ (y:_) = zs `minus` [x]

-- instance OEIS 251561 where
--   oeisIx 1 = 1
--   oeisIx n | q == 1                    = 2 * p
--             | p == 2 && (oeisIx @10051)' q == 1 = q
--             | otherwise                 = n
--             where q = div n p; p = (oeisIx @20639) n

-- instance OEIS 251595 where
--   oeis = map head $ group (oeis @251416)

-- instance OEIS 251608 where
--   oeis = 2 : 3 : f 2 3 (drop 5 (oeis @45)) where
--      f u v (w:ws) = if gcd u w > 1 && gcd v w == 1
--                        then w : f v w ws else f u v ws

-- instance OEIS 251618 where
--   oeisIx n = fromJust $
--               find (\x -> mod x (fi $ (oeisIx @40) n) == 0) (oeis @98550)

-- instance OEIS 251619 where
--   oeisIx = (oeisIx @20639) . (oeisIx @251618)

-- instance OEIS 251620 where
--   oeis = map head $ group (oeis @249943)

-- instance OEIS 251621 where
--   oeis = map length $ group (oeis @249943)

-- instance OEIS 251635 where
--   oeis = tablList @251635
-- instance Table 251635 where
--   tabl = [1] : iterate (0 :) [-2, 1]

-- instance OEIS 251725 where
--   oeisIx 1 = 1
--   oeisIx n = if length ps == 1 then 1 else head $ filter f [2..]  where
--     f b = all (== len) lbs where len:lbs = map (length . d b) ps
--     ps = (rowT @27748) n
--     d b = unfoldr (\z -> if z == 0 then Nothing else Just $ swap $ divMod z b)

-- instance OEIS 251728 where
--   oeis = filter f [1..] where
--                         f x = q < p ^ 2 && (oeisIx @10051)' q == 1
--                               where q = div x p; p = (oeisIx @20639) x

-- instance OEIS 251756 where
--   oeis = 0 : f 0 (oeis @2808) where
--      f x zs = g zs where
--        g (y:ys) | d == 1 || (oeisIx @10051)' d == 1 = g ys
--                 | otherwise = y : f y (delete y zs)
--                 where d = gcd x y

-- instance OEIS 251767 where
--   oeis = map head $ group (oeis @251539)

-- instance OEIS 251768 where
--   oeis = map length $ group (oeis @251539)

-- instance OEIS 251984 where
--   oeisIx n = if d > 0 then 10 - d else 10 * (oeisIx @251984) n'
--               where (n',d) = divMod n 10

-- instance OEIS 252001 where
--   oeis = 1 : f [1] (drop 2 (tabf @31298)) where
--      f xs zss = g zss where
--        g (ds:dss) = if any (> 9) $ zipWith (+) xs ds
--          then (foldr (\d v -> 10 * v + d) 0 ds) : f ds (delete ds zss)
--          else g dss

-- instance OEIS 252002 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @252001))

-- instance OEIS 252022 where
--   oeis = 1 : f [1] (drop 2 (tabf @31298)) where
--      f xs zss = g zss where
--        g (ds:dss) = if all (<= 9) $ zipWith (+) xs ds
--          then (foldr (\d v -> 10 * v + d) 0 ds) : f ds (delete ds zss)
--          else g dss

-- instance OEIS 252023 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @252022))

-- instance OEIS 252078 where
--   oeis = [x | x <- [1..], (oeisIx @252001) x == x]

-- instance OEIS 252079 where
--   oeis = [x | x <- [1..], (oeisIx @252022) x == x]

-- instance OEIS 252094 where
--   (oeis, (oeis @252095)) = unzip $ f 3 where
--      f x = if z then (q, p - q) : f (x + 2) else f (x + 2)  where
--            z = 0 `notElem` ds && length ds > 2 &&
--                all (== 0) (zipWith mod (tail ds) ds) && all (== q) qs
--            q:qs = (zipWith div (tail ds) ds)
--            ds = zipWith (-) (tail ps) ps
--            ps = 1 : ps'; ps'@ (p:_) = (rowT @27746) x

-- instance OEIS 252095 where

-- instance OEIS 252448 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249990))

-- instance OEIS 252458 where
--   oeis = [x | x <- [1..], (oeisIx @249990) x == x]

-- instance OEIS 252477 where
--   oeis = map (floor . recip) $ zipWith (-) (tail rs) rs
--                  where rs = map (sqrt . fi) (oeis @40)

-- instance OEIS 252596 where
--   oeis = iterate (\x -> (oeisIx @40) (oeisIx x + mod x 3)) 5

-- instance OEIS 252837 where
--   oeis = f (oeis @98550) where
--      f us = (h 0 vs) : f vs where
--        (_:vs) = dropWhile ((== 0) . (oeisIx @10051)') us
--        h e (w:_:ws) = if even w then h (e + 1) ws else e

-- instance OEIS 252849 where
--   oeis = filter (even . (oeisIx @46951)) [1..]

-- instance OEIS 252865 where
--   oeis = 1 : 2 : 3 : f 2 3 (drop 3 (oeis @5117)) where
--      f u v ws = g ws where
--        g (x:xs) = if gcd x u > 1 && gcd x v == 1
--                      then x : f v x (delete x ws) else g xs

-- instance OEIS 252867 where
--   oeis = 0 : 1 : 2 : f 1 2 [3..] where
--      f :: Int -> Int -> [Int] -> [Int]
--      f u v ws = g ws where
--        g (x:xs) = if x .&. u > 0 && x .&. v == 0
--                      then x : f v x (delete x ws) else g xs

-- instance OEIS 252895 where
--   oeis = filter (odd . (oeisIx @46951)) [1..]

-- instance OEIS 252912 where
--   oeis = filter (\x -> (oeisIx @98550) x == (oeisIx @251555) x) [1..]

-- instance OEIS 252939 where
--   oeis = zipWith (-) (tail (oeis @252912)) (oeis @252912)

-- instance OEIS 252940 where
--   oeis = map length $ group (oeis @252939)

-- instance OEIS 252942 where
--   oeisIx n = head [y | m <- [1..],
--      let y = read (show m ++ show n ++ show m) :: Integer, (oeisIx @10051)' y == 1]

-- instance OEIS 253046 where
--   oeisIx n | i == 0 || p > 3 = n
--             | p == 2          = 3 * (oeisIx @40) (i + 1)
--             | otherwise       = 2 * (oeisIx @40) (i - 1)
--               where i = (oeisIx @49084) (div n p);  p = (oeisIx @20639) n

-- instance OEIS 253048 where
--   oeis = filter ((== 0) . (oeisIx @10051)') $ map (oeisIx @253049) [1..]

-- instance OEIS 253073 where
--   oeis = 0 : f 0 (oeis @18252) where
--      f u vs = g vs where
--        g (w:ws) | (oeisIx @10051)' (u + w) == 1 = g ws
--                 | otherwise = w : f w (delete w vs)

-- instance OEIS 253074 where
--   oeis = 0 : f 0 [1..] where
--      f u vs = g vs where
--        g (w:ws) | (oeisIx @10051)' (u + w) == 1 = g ws
--                 | otherwise = w : f w (delete w vs)

-- instance OEIS 253106 where
--   oeis = filter f [1..] where
--      f x = p <= 3 && (oeisIx @10051)' (div x p) == 1  where p = (oeisIx @20639) x

-- instance OEIS 253138 where
--   oeisIx n = sum $ map (oeisIx @64911) $
--      takeWhile (> 0) $ map (2 * p -) $ dropWhile (< p) (oeis @1358)
--      where p = (oeisIx @40) n

-- instance OEIS 253146 where
--   oeis = tablList @253146
-- instance Table 253146 where
--   rowCol = rowCol_off @253146 @1 @1
--   rowT   = rowT_off   @253146 @1
--   tabl = [1] : [2,3] : f [1] [2,3] where
--      f us vs@ (v:_) = ws : f vs ws where
--                      ws = [v + 2] ++ us ++ [v + 3]

-- instance OEIS 253169 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @256188))

-- instance OEIS 253172 where
--   oeis = filter f [2..] where
--      f x = g divs $ reverse divs where
--            g (d:ds) (q:qs) = d <= q &&
--              (sort (nub $ xs ++ show d ++ show q) == decs || g ds qs)
--            xs = show x
--            divs = (rowT @27750) x
--      decs = "0123456789"

-- instance OEIS 253253 where
--   oeis = f (oeis @1704) [] where
--      f (x:xs) ds = y : f xs (insert y ds) where
--                    y = head (oeisIx_row' x `minus` ds)

-- instance OEIS 253297 where
--   oeis = f (oeis @98550) where
--      f (u:vs@ (_:v:_)) = if (oeisIx @10051)' v == 1 && div u v > 2
--                            then v : f vs else f vs

-- instance OEIS 253315 where
--   oeisIx :: Integer -> Integer
--   oeisIx = f 0 0 where
--      f _ y 0 = y
--      f z y x = f (z + 1) (y `xor` b * z) x' where (x', b) = divMod x 2

-- instance OEIS 253415 where
--   oeisIx n = (oeis @253415) !! (n - 2)
--   oeis = f [2..] 1 where
--      f xs z = g xs where
--        g (y:ys) = if mod z' y > 0 then g ys else x : f xs' (z + y)
--                   where xs'@ (x:_) = delete y xs
--        z' = z + 2

-- instance OEIS 253425 where
--   oeis = map length $ group (oeis @253415)

-- instance OEIS 253443 where
--   oeisIx n = (oeis @253443) !! (n - 4)
--   oeis = f (4, []) 6 where
--      f (m,ys) z = g $ dropWhile (< m) $ (rowT @27750)' z where
--        g (d:ds) | elem d ys = g ds
--                 | otherwise = m : f (ins [m, m+1 ..] (insert d ys)) (z + d)
--        ins (u:us) vs'@ (v:vs) = if u < v then (u, vs') else ins us vs

-- instance OEIS 253444 where
--   oeis = map length $ group (oeis @253443)

-- instance OEIS 253569 where
--   oeis = filter f [1..] where
--                       f x = (p ^ 2 < (oeisIx @20639) q) && (oeisIx' q == 1 || f q)
--                             where q = div x p; p = (oeisIx @20639) x
--   oeisIx n = (oeis @253569) !! (n - 1)
--   oeis = filter (not . f''') (oeis @2808) where
--      f''' x = p ^ 2 > (oeisIx @20639) q || (oeisIx q == 0 && f''' q)
--               where q = div x p; p = (oeisIx @20639) x

-- instance OEIS 253580 where
--   oeis = tablList @253580
-- instance Table 253580 where
--   tabf = [0] : [1,0,2] : f [1,0,2] where
--      f xs@ (x:_) = ys : f ys where ys = [x + 2] ++ xs ++ [x + 3]
--   oeis = concat (tabf @253580)

-- instance OEIS 253581 where
--   oeis = zipWith (.&.) (oeis @252867) $ drop 2 (oeis @252867) :: [Int]

-- instance OEIS 253582 where
--   oeis = zipWith (.|.) (oeis @252867) $ tail (oeis @252867) :: [Int]
--   oeis' = zipWith (+) (oeis @252867) $ tail (oeis @252867)

-- instance OEIS 253584 where
--   oeis = map head $ group (oeis @253443)

-- instance OEIS 253589 where
--   oeisIx = (oeisIx @120) . (oeisIx @252867)

-- instance OEIS 253603 where
--   oeisIx = fromJust . (`elemIndex` (oeis @253581))

-- instance OEIS 253607 where
--   oeis = zipWith (-) (tail (oeis @253580)) (oeis @253580)

-- instance OEIS 253672 where
--   oeis = tablList @253672
-- instance Table 253672 where
--   tabf = [0,1,2] : f [] [0,1,2] [] (iterate (map (+ 3)) [3..5]) where
--      f as bs cs (uvws:uvwss) = (as' ++ uvws ++ cs') : f as' uvws cs' uvwss
--        where as' = as ++ [u,v]; cs' = [w] ++ cs
--              [u,v,w] = bs
--   oeis = concat (tabf @253672)

-- instance OEIS 253717 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @106039)

-- instance OEIS 253721 where
--   oeisIx = flip mod 10 . (oeisIx @14612)

-- instance OEIS 253853 where
--   oeis = 1 : 1 : 1 : map (+ 1)
--                              (zipWith (*) (oeis @253853) $ tail (oeis @253853))

-- instance OEIS 253910 where
--   oeisIx n = (oeis @253911) !! (n - 1)
--   oeis = map read $
--      zipWith ((++) `on` show) (oeis @18252) (oeis @40) :: [Integer]

-- instance OEIS 253911 where
--   oeis = map read $
--      zipWith ((++) `on` show) (oeis @18252) (oeis @40) :: [Integer]

-- instance OEIS 254143 where
--   import Data.Set (empty, fromList, deleteFindMin, union)
--   import qualified Data.Set as Set (null)
--   oeisIx n = (oeis @254143) !! (n - 1)
--   oeis = f (oeis @237424) [] empty where
--      f xs'@ (x:xs) zs s
--        | Set.null s || x < y = f xs zs' (union s $ fromList $ map (* x) zs')
--        | otherwise           = y : f xs' zs s'
--        where zs' = x : zs
--              (y, s') = deleteFindMin s

-- instance OEIS 254308 where
--   oeis = 0 : 1 : 1 : zipWith3 (\u v w -> u + if odd u then v else w)
--                  (drop 2 (oeis @254308)) (tail (oeis @254308)) (oeis @254308)

-- instance OEIS 254323 where
--   oeisIx = (oeisIx @137564) . (oeisIx @254143)

-- instance OEIS 254338 where
--   oeisIx = (oeisIx @30) . (oeisIx @254143)

-- instance OEIS 254339 where
--   oeisIx = flip mod 10 . (oeisIx @254143)

-- instance OEIS 254397 where
--   oeisIx = (oeisIx @30) . (oeisIx @237424)

-- instance OEIS 254398 where
--   oeisIx = flip mod 10 . (oeisIx @237424)

-- instance OEIS 254524 where
--   import Data.IntMap (empty, findWithDefault, insert)
--   oeisIx n = (oeis @254524) !! (n - 1)
--   oeis = f 1 empty where
--      f x m = y : f (x + 1) (insert q (y + 1) m) where
--              y = findWithDefault 1 q m; q = (oeisIx @7953) x

-- instance OEIS 254531 where
--   oeisIx = (+ 49) . round . (* 12) . logBase 2 . (/ 440) . fi

-- instance OEIS 254609 where
--   oeis = tablList @254609
-- instance Table 254609 where
--   tabl = zipWith (map . div)
--      (oeis @243757) $ zipWith (zipWith (*)) xss $ map reverse xss
--      where xss = tail $ inits (oeis @243757)

-- instance OEIS 254650 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @254649))

-- instance OEIS 254656 where
--   oeis = [x | x <- [1..], (oeisIx @254649) x == x]

-- instance OEIS 254679 where
--   oeis = tablList @254679
-- instance Table 254679 where
--   rowCol = rowCol_off @254679 @1 @1
--   rowT   = rowT_off @254679 @1
--   tabf = map (sortBy (comparing show)) (tabf @27750)

-- instance OEIS 254730 where
--   oeis = tablList @254730
-- instance Table 254730 where
--   tabl = zipWith (map . div)
--      (oeis @243758) $ zipWith (zipWith (*)) xss $ map reverse xss
--      where xss = tail $ inits (oeis @243758)

-- instance OEIS 255313 where
--   oeis = tablList @255313
-- instance Table 255313 where
--   rowCol = rowCol_off @255313 @1 @1
--   rowT   = rowT_off   @255313 @1
--   tabl = zipWith (zipWith (+)) tss $ map tail tss
--                  where tss = tail (tabl @88643)

-- instance OEIS 255316 where
--   oeis = tablList @255316
-- instance Table 255316 where
--   rowCol = rowCol_off @255316 @1 @1
--   rowT   = rowT_off @255316 @1
--   tabf = map (sort . nub) (tabl @255313)

-- instance OEIS 255395 where
--   oeisIx = genericLength . nub . (rowT @255313)

-- instance OEIS 255427 where
--   oeisIx = product . nub . (rowT @255313)

-- instance OEIS 255479 where
--   oeisIx = (+ 1) . fromJust. (`elemIndex` (oeis @255582))

-- instance OEIS 255480 where
--   oeis = zipWith gcd (oeis @255582) $ drop 2 (oeis @255582)

-- instance OEIS 255481 where
--   oeis = zipWith gcd (oeis @255582) $ tail (oeis @255582)

-- instance OEIS 255482 where
--   oeisIx n = (oeisIx @64664) n - (oeisIx @255479) n

-- instance OEIS 255507 where
--   oeis = zipWith (-) (tail (oeis @255437)) (oeis @255437)
--
-- instance OEIS 255508 where
--   oeis = scanl1 (+) (oeis @255437)

-- instance OEIS 255527 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @255437)) . (oeisIx @164514)

-- instance OEIS 255646 where
--   oeisIx = flip mod 10 . (oeisIx @46316)

-- instance OEIS 255678 where
--   oeisIx n = head $ filter ((== n) . (oeisIx @118668)) [0..]

-- instance OEIS 255723 where
--   oeis = 0 : concat (transpose [map (subtract 2) (oeis @255723),
--                                         map (-1 -) (oeis @255723),
--                                         map (+ 2) (oeis @255723),
--                                         tail (oeis @255723)])

-- instance OEIS 255731 where
--   oeis = filter (rhonda 60) $ iterate z 1 where
--      z x = 1 + if r < 59 then x else 60 * z x' where (x', r) = divMod x 60

-- instance OEIS 255732 where
--   oeis = filter (rhonda 20) $ iterate z 1 where
--      z x = 1 + if r < 29 then x else 30 * z x' where (x', r) = divMod x 30

-- instance OEIS 255735 where
--   oeis = filter (rhonda 18) $ iterate z 1 where
--      z x = 1 + if r < 17 then x else 18 * z x' where (x', r) = divMod x 18

-- instance OEIS 255736 where
--   oeis = filter (rhonda 30) $ iterate z 1 where
--      z x = 1 + if r < 29 then x else 30 * z x' where (x', r) = divMod x 30

-- instance OEIS 255833 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @166133))

-- instance OEIS 255872 where
--   oeisIx n = head $ filter (rhonda b) $ iterate zeroless 1 where
--
--               zeroless x = 1 + if r < b - 1 then x else b * zeroless x'
--                            where (x', r) = divMod x b
--               b = (oeisIx @2808) n

-- instance OEIS 255878 where
--   oeis = zipWith (-) (tail (oeis @256188)) (oeis @256188)

-- instance OEIS 255880 where
--   oeisIx n = (filter (rhonda b) $ iterate zeroless 1) !! (n - 1) where
--
--               zeroless x = 1 + if r < b - 1 then x else b * zeroless x'
--                            where (x', r) = divMod x b
--               b = (oeisIx @2808) n

-- instance OEIS 255940 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249167))

-- instance OEIS 255972 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @251604))

-- instance OEIS 256012 where
--   oeisIx = p (oeis @13929) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 256015 where
--   oeis = tablList @256015
-- instance Table 256015 where
--   rowCol = rowCol_off @256015 @1 @1
--   rowT   = rowT_off @256015 @1
--   tabf = map (sort . filter ((== 1) . (oeisIx @10051)') . nub .
--                   map sum . tail . subsequences) (tail $ inits (oeis @40))

-- instance OEIS 256113 where
--   oeis = tablList @256113
-- instance Table 256113 where
--   rowCol = rowCol_off @256113 @1 @1
--   rowT   = rowT_off @256113 @1
--   tabf = map (rowT @27748) $ tail (oeis @1142)

-- instance OEIS 256152 where
--   256152_list = filter f (oeis @6881) where
--      f x = (oeisIx @10052)' ((spf + 1) * (x `div` spf + 1)) == 1
--            where spf = (oeisIx @20639) x

-- instance OEIS 256184 where
--   oeis = 0 : concat (transpose [map (subtract 2) (oeis @256184),
--                                         map (subtract 1) (oeis @256184),
--                                         map negate $ tail (oeis @256184)])

-- instance OEIS 256185 where
--   oeis = 0 : concat (transpose [map (subtract 3) (oeis @256185),
--                                         map (-2 -) (oeis @256185),
--                                         map negate $ tail (oeis @256185)])

-- instance OEIS 256187 where
--   oeis = zipWith (-) (tail (oeis @4718)) (oeis @4718)

-- instance OEIS 256213 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @254077)) [1..]

-- instance OEIS 256232 where
--   oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n)
--      where f 2 e = e - 1; f 3 e = 1; f _ e = e + 1

-- instance OEIS 256248 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @55744)) [1..]

-- instance OEIS 256283 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257905))

-- instance OEIS 256285 where
--   oeis = f (tail (oeis @127423)) [] where
--      f (x:xs) ds = y : f xs (insert y ds) where
--                    y = head (oeisIx_row x `minus` ds)

-- instance OEIS 256371 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @256210))

-- instance OEIS 256372 where
--   oeis = [x | x <- [1..], (oeisIx @256210) x == x]

-- instance OEIS 256393 where
--   oeis = 2 : zipWith ($) (cycle [oeisIx, (oeisIx @61228)]) (oeis @256393)

-- instance OEIS 256405 where
--   oeis = 2 : 3 : f (3:[5..]) 4 where
--      f zs@ (z:_) x = z : f (delete y zs) y where
--                     y = head $ isect (oeisIx_row' (x ^ 2 - 1)) zs

-- instance OEIS 256406 where
--   oeis = f (oeis @166133) where
--      f (u:vs'@ (v:ws)) | u > v || v /= u ^ 2 - 1 = f vs'
--                       | otherwise               = u : f ws

-- instance OEIS 256414 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121217)) . (oeisIx @40)

-- instance OEIS 256415 where
--   oeisIx n | (oeisIx @10051) n == 1 = 2 * n
--             | r == 0 && (oeisIx @10051) n' == 1 = 2 * n'
--             | otherwise = n
--             where (n', r) = divMod n 3

-- instance OEIS 256417 where
--   oeisIx = (oeisIx @256415) . (oeisIx @64413)

-- instance OEIS 256419 where
--   oeisIx = (oeisIx @256415) . (oeisIx @121217)

-- instance OEIS 256489 where
--   oeis = zipWith (-) (tail (oeis @257509)) (oeis @257509)

-- instance OEIS 256507 where
--   oeis = tablList @256507
-- instance Table 256507 where
--   rowCol = rowCol_off @256507 @1 @1
--   rowT   = rowT_off @256507 @1
--   tabf = zipWith (\us vs ->
--                          map ((+ 1) . fromJust . (`elemIndex` vs)) us)
--                          (tabf @256946) $ tail (tabf @256946)

-- instance OEIS 256541 where
--   oeis = zipWith (-) (tail (oeis @166133)) (oeis @166133)

-- instance OEIS 256542 where
--   oeisIx = (oeisIx @5) . (oeisIx @166133)

-- instance OEIS 256543 where
--   oeis = [x | x <- [1..], abs (oeisIx @256541 x) == 1]

-- instance OEIS 256561 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @166133)) . (oeisIx @40)

-- instance OEIS 256563 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @166133)) . (oeisIx @1358)

-- instance OEIS 256564 where
--   oeisIx = (oeisIx @20639) . (oeisIx @166133)

-- instance OEIS 256578 where
--   oeisIx = (oeisIx @32742) . (oeisIx @166133)

-- instance OEIS 256607 where
--   oeisIx = (oeisIx @7733) . (oeisIx @7733)

-- instance OEIS 256617 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @256617) !! (n - 1)
--   oeis = f (singleton (6, 2, 3)) $ tail (oeis @40) where
--      f s ps@ (p : ps'@ (p':_))
--        | m < p * p' = m : f (insert (m * q, q, q')
--                             (insert (m * q', q, q') s')) ps
--        | otherwise  = f (insert (p * p', p, p') s) ps'
--        where ((m, q, q'), s') = deleteFindMin s

-- instance OEIS 256618 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121217))

-- instance OEIS 256628 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @251622))

-- instance OEIS 256673 where
--   oeis = filter odd (oeis @157037)

-- instance OEIS 256703 where
--   oeis = map (+ 1) $ findIndices (\ (u, v) -> v == u^2-1) $
--                              zip (oeis @166133) (tail (oeis @166133))

-- instance OEIS 256751 where
--   oeisIx n = (+ 1) $ fromJust $
--               (oeisIx @166133) n `elemIndex` (rowT @27750)' (oeisIx (n - 1) ^ 2 - 1)

-- instance OEIS 256757 where
--   oeisIx n = fst $ until ((== 1) . snd)
--               (\ (i, x) -> (i + 1, fi $ (oeisIx @7733) x)) (0, n)

-- instance OEIS 256758 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex`  (oeis @256757))

-- instance OEIS 256775 where
--   oeis = [x | x <- map (+ 81) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256776 where
--   oeis = [x | x <- map (+ 256) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256777 where
--   oeis = [x | x <- map (+ 625) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256786 where
--   oeis = filter f (oeis @52382) where
--      f x = g x where
--        g z = z == 0 || x `mod` (oeisIx @40) d == 0 && g z'
--              where (z', d) = divMod z 10

-- instance OEIS 256834 where
--   oeis = [x | x <- map (+ 1296) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256835 where
--   oeis = [x | x <- map (+ 2401) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256836 where
--   oeis = [x | x <- map (+ 4096) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256837 where
--   oeis = [x | x <- map (+ 6561) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256838 where
--   oeis = [x | x <- map (+ 10000) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256839 where
--   oeis = [x | x <- map (+ 14641) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256840 where
--   oeis = [x | x <- map (+ 20736) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256841 where
--   oeis = [x | x <- map (+ 28561) (oeis @290), (oeisIx @10051)' x == 1]

-- instance OEIS 256852 where
--   oeis = f (oeis @40) [] $ tail (oeis @583) where
--      f ps'@ (p:ps) us vs'@ (v:vs)
--        | p > v     = f ps' (v:us) vs
--        | otherwise = (sum $ map (oeisIx . (p -)) us) : f ps us vs'

-- instance OEIS 256863 where
--   oeis = map (oeisIx @40) $ filter ((== 0) . (oeisIx @256852)) [1..]

-- instance OEIS 256885 where
--   oeisIx n = (oeisIx @217) n - (oeisIx @720) n

-- instance OEIS 256913 where

-- instance OEIS 256914 where
--   oeisIx = last . (rowT @256913)

-- instance OEIS 256915 where
--   oeisIx = genericLength . (rowT @256913)

-- instance OEIS 256918 where
--   oeisIx n = (oeis @257218) !! (n - 1)
--   oeis = zipWith gcd (oeis @257218) $ tail (oeis @257218)

-- instance OEIS 256946 where
--   oeis = tablList @256946
-- instance Table 256946 where
--   rowCol = rowCol_off @256946 @1 @1
--   rowT   = rowT_off @256946 @1
--   tabf = f 0 [] [] where
--      f k us vs = (xs ++ ys) : f (k+1) xs ys where
--        xs = us ++ qs
--        ys = sortBy (compare `on`
--                     snd . properFraction . sqrt . fi) (vs ++ rs)
--        (qs, rs) = span ((== 1) . (oeisIx @10052)') [k* (k+2)+1 .. (k+1)* (k+3)]

-- instance OEIS 257046 where
--   oeis = filter ((== 1) . (oeisIx @256914)) [0..]

-- instance OEIS 257047 where
--   oeis = filter ((/= 1) . (oeisIx @256914)) [0..]

-- instance OEIS 257053 where
--   oeis = tablList @257053
-- instance Table 257053 where
--   rowCol = rowCol_off @257053 @1 @0
--   rowT   = rowT_off @257053 @1
--   tabf = map (oeisIx_row . fi) (oeis @40)

-- instance OEIS 257070 where
--   oeisIx = last . (rowT @257053)

-- instance OEIS 257071 where
--   oeisIx = genericLength . (rowT @257053)

-- instance OEIS 257111 where
--   oeis = zipWith (-) (tail (oeis @257339)) (oeis @257339)

-- instance OEIS 257120 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @256918))

-- instance OEIS 257122 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257218))

-- instance OEIS 257143 where
--   oeis = concat $ transpose [oeis, (oeis @5408)]

-- instance OEIS 257145 where
--   oeisIx 0 = 1
--   oeisIx n = div (n + 2) 5 * 5 - n

-- instance OEIS 257173 where
--   oeisIx = fromJust . (`elemIndex` (oeis @248737))

-- instance OEIS 257218 where
--   oeis = 1 : f 1 [2..] (oeis @4526) where
--      f x zs cds = g zs where
--        g (y:ys) | cd `member` cds = y : f y (delete y zs) (delete cd cds)
--                 | otherwise       = g ys
--                 where cd = gcd x y

-- instance OEIS 257232 where
--   oeis = tablList @257232
-- instance Table 257232 where
--   rowCol = rowCol_off @257232 @1 @1
--   rowT   = rowT_off   @257232 @1
--   tabl = iterate
--                  (\xs@ (x:_) -> map (+ 1) xs ++ [1 - (oeisIx @10051) (x + 1)]) [1]

-- instance OEIS 257241 where
--   oeis = tablList @257241
-- instance Table 257241 where
--   rowCol = rowCol_off @257241 @1 @1
--   rowT   = rowT_off @257241 @1
--   tabf = iterate stifel [1] where
--      stifel xs@ (x:_) = if odd x then xs' else xs' ++ [last xs']
--                        where xs' = zipWith (+) xs (1 : xs)

-- instance OEIS 257244 where
--   oeis = zipWith gcd (oeis @256393) $ tail (oeis @256393)

-- instance OEIS 257265 where
--   oeisIx = genericLength . us where
--      us n = if (oeisIx @79559) n == 0
--                then [] else () : zipWith (const $ const ())
--                                  (us $ (oeisIx @213723) n) (us $ (oeisIx @213724) n)

-- instance OEIS 257278 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @257278) !! (n - 1)
--   oeis = f (singleton (4, 2)) 27 (tail (oeis @40)) where
--      f s pp ps@ (p:ps'@ (p':_))
--        | qq > pp   = pp : f (insert (pp * p, p) s) (p' ^ p') ps'
--        | otherwise = qq : f (insert (qq * q, q) s') pp ps
--        where ((qq, q), s') = deleteFindMin s

-- instance OEIS 257279 where
--   oeis = filter ((== 1) . (oeisIx @168046)) (oeis @257278)

-- instance OEIS 257455 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257339))

-- instance OEIS 257456 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257340))

-- instance OEIS 257457 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (oeisIx @257339)) [1..]

-- instance OEIS 257458 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @257339)) [1..]

-- instance OEIS 257465 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @175498))

-- instance OEIS 257478 where
--   oeisIx n = (oeisIx @257475) n - (oeisIx @257120) n

-- instance OEIS 257508 where
--   oeis = filter ((== 1) . (oeisIx @257265)) [0..]

-- instance OEIS 257509 where
--   oeis = filter ((== 2) . (oeisIx @257265)) [0..]

-- instance OEIS 257572 where
--   oeisIx = (oeisIx @20639) . (oeisIx @257278)

-- instance OEIS 257573 where
--   oeisIx = (oeisIx @1222) . (oeisIx @257278)

-- instance OEIS 257585 where
--   oeisIx = flip mod 2 . (oeisIx @254077)

-- instance OEIS 257641 where
--   oeisIx n = (oeisIx @103284) (2 * n) n

-- instance OEIS 257644 where
--   oeis = scanl (+) 1 (oeis @7503)

-- instance OEIS 257646 where
--   oeisIx n = fromJust $ findIndex (elem n) (tabl @103284)

-- instance OEIS 257719 where
--   oeis = filter f [1..] where
--      f x = sx >= x && (oeisIx @1065) sx < sx where sx = (oeisIx @1065) x

-- instance OEIS 257720 where
--   oeis = filter f [1..] where
--      f x = sx > 0 && sx < x && (oeisIx @1065) sx >= sx where sx = (oeisIx @1065) x

-- instance OEIS 257762 where
--   oeis = map (oeisIx @258432) $ filter ((== 2) . (oeisIx @258383)) [1..]

-- instance OEIS 257770 where
--   oeis = tablList @257770
-- instance Table 257770 where
--   rowT n = filter belge [0..9] where
--      belge k = n == (head $ dropWhile (< n) $
--                     scanl (+) k $ cycle $ (map (read . return) . show) n)
--   tabf = map (rowT @257770) [0..]

-- instance OEIS 257773 where
--   oeisIx = genericLength . (rowT @257770)

-- instance OEIS 257778 where
--   oeisIx = head . (rowT @257770)

-- instance OEIS 257779 where
--   oeisIx = last . (rowT @257770)

-- instance OEIS 257782 where
--   oeis = filter ((> 0) . (oeisIx @257778)) [0..]

-- instance OEIS 257785 where
--   oeis = filter ((== 1) . (oeisIx @257773)) [0..]

-- instance OEIS 257815 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @64364))

-- instance OEIS 257831 where
--   oeisIx = foldr (\b v -> 10 * v + b) 0 .
--              concat . mapMaybe (flip lookup bin) . (rowT @31298)
--               where bin = zip [0..9] (tabf @30308)

-- instance OEIS 257836 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @257836) !! (n - 1)
--   oeis = f $ singleton (15, 3, 5) where
--      f s = y : f (insert (w, u, v') $ insert (w `div` u, u + 2, v') s')
--            where w = y * v'; v' = v + 2
--                  ((y, u, v), s') = deleteFindMin s

-- instance OEIS 257851 where
--   oeis = tablList @257851
-- instance Table 257851 where
--   tabl = map
--      (\x -> take (x + 1) $ filter ((== x) . (oeisIx @46660)) [1..]) [0..]

-- instance OEIS 257860 where
--   oeis = 1 : filter f [1..] where
--      f x = any (\d -> member (x - q + d) $ ps d) $ filter (> 1) $ nub ds
--            where q = sum ds; ds = (map (read . return) . show . fi) x
--      ps x = iterate (* x) (x ^ 2)

-- instance OEIS 257891 where
--   import Data.Set (singleton, deleteFindMin, insert)
--   oeisIx n = (oeis @257891) !! (n - 1)
--   oeis = f $ singleton (30, 2, 5) where
--      f s = y : f (insert (w, p, q') $ insert (w `div` p, (oeisIx @151800) p, q') s')
--            where w = y * q'; q' = (oeisIx @151800) q
--                  ((y, p, q), s') = deleteFindMin s

-- instance OEIS 257892 where
--   oeis = map (oeisIx @258432) $ filter ((== 4) . (oeisIx @258383)) [1..]

-- instance OEIS 257905 where
--   oeis = 0 : f [0] [0] where
--      f xs@ (x:_) ds = g [2 - x .. -1] where
--        g [] = y : f (y:xs) (h:ds) where
--                     y = x + h
--                     (h:_) = [z | z <- [1..] \\ ds, x - z `notElem` xs]
--        g (h:hs) | h `notElem` ds && y `notElem` xs = y : f (y:xs) (h:ds)
--                 | otherwise = g hs
--                 where y = x + h

-- instance OEIS 257906 where
--   oeis = 0 : f [0] [1] where
--      f xs@ (x:_) ds = g [2 - x .. -1] where
--        g [] = y : f (y:xs) (h:ds) where
--                     y = x + h
--                     (h:_) = [z | z <- [1..] \\ ds, x - z `notElem` xs]
--        g (h:hs) | h `notElem` ds && y `notElem` xs = y : f (y:xs) (h:ds)
--                 | otherwise = g hs
--                 where y = x + h

-- instance OEIS 257907 where
--   oeis = 1 : f [0] [1] where
--      f xs@ (x:_) ds = g [2 - x .. -1] where
--        g [] = h : f ((x + h) : xs) (h : ds) where
--                     (h:_) = [z | z <- [1..] \\ ds, x - z `notElem` xs]
--        g (h:hs) | h `notElem` ds && y `notElem` xs = h : f (y:xs) (h:ds)
--                 | otherwise = g hs
--                 where y = x + h

-- instance OEIS 257951 where
--   oeis = map (oeisIx @258432) $ filter ((== 5) . (oeisIx @258383)) [1..]

-- instance OEIS 257956 where
--   oeisIx = sum . (rowT @232642)

-- instance OEIS 257971 where
--   oeis = zipWith (-) (tail (oeis @6921)) (oeis @6921)

-- instance OEIS 257997 where
--   import Data.List.Ordered (unionAll)
--   oeisIx n = (oeis @257997) !! (n - 1)
--   oeis = unionAll [oeis, (oeis @3592), (oeis @3593)]

-- instance OEIS 257999 where
--   oeis = filter (odd . flip mod 2 . (oeisIx @1222)) (oeis @3586)

-- instance OEIS 258023 where
--   import Data.List.Ordered (union)
--   oeisIx n = (oeis @258023) !! (n - 1)
--   oeis = union (oeis @3586) (oeis @3593)

-- instance OEIS 258032 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . flip div 10. (^ 3)) (oeis @40)

-- instance OEIS 258033 where
--   oeis = 0 : f (tail (oeis @22328)) where
--      f xs = (0 : (delete (maximum ys) ys)) ++ f zs
--             where (ys, (_ : zs)) = span (> 0) xs

-- instance OEIS 258051 where
--   oeis = f (tail (oeis @258033)) where
--      f xs = (0 : (delete (maximum ys) ys)) ++ f zs
--             where (ys, (_ : zs)) = span (> 0) xs

-- instance OEIS 258057 where
--   oeis = zipWith (-) (tail (oeis @3415)) (oeis @3415)

-- instance OEIS 258059 where
--   oeisIx = f 0 . (rowT @30386) where
--      f i [] = i
--      f i (t:ts) = if t == 1 then f (i + 1) ts else i

-- instance OEIS 258062 where
--   oeis = map length $ group (oeis @188967)

-- instance OEIS 258063 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258062))

-- instance OEIS 258083 where
--   oeis = f 1 $ tail $ zip
--      (oeis @8585) $ map (reverse . show) (oeis @8585) where
--      f x ws = g ws where
--        g ((u, vs) : uvs) = if isPrefixOf xs vs
--                            then u : f (x + 1) (delete (u, vs) ws) else g uvs
--        xs = reverse $ show x

-- instance OEIS 258087 where
--   oeis = f 0 [0] $
--      map (\i -> take (i + 1) (repeat 0) ++ replicate (i + 2) i) [0..] where
--      f i ys@ (y:_) (xs:xss) = (ys !! i) :
--                              f (i + 1) (zipWith (+) (ys ++ repeat 0) xs) xss

-- instance OEIS 258091 where
--   oeisIx = (oeisIx @20639) . (oeisIx @258073)

-- instance OEIS 258095 where
--   oeis = filter
--                  (\x -> (oeisIx @258091) x `notElem` [3, 5, 7, 13, 19, 37, 73]) [1..]

-- instance OEIS 258115 where
--   oeisIx n = (oeisIx @208570) n `div` n

-- instance OEIS 258125 where
--   oeis = 2 : 2 : zipWith (+)
--                  (map (oeisIx @6530) (oeis @258125)) (tail (oeis @258125))

-- instance OEIS 258138 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @99305))

-- instance OEIS 258144 where
--   oeisIx = sum . zipWith (*) (cycle [1, -1]) . (rowT @257241)

-- instance OEIS 258143 where
--   oeisIx = sum . (rowT @257241)

-- instance OEIS 258188 where
--   oeis = f 1 $ tail $ zip
--      (oeis @8589) $ map (reverse . show) (oeis @8589) where
--      f x ws = g ws where
--        g ((u, vs) : uvs) = if isPrefixOf xs vs
--                            then u : f (x + 1) (delete (u, vs) ws) else g uvs
--        xs = reverse $ show x

-- instance OEIS 258193 where
--   oeisIx n = head $ (filter ((== 1) . (oeisIx @10051)'') $
--                       scanl1 (<+>) (oeisIx_row n)) ++ [0]
--               where (<+>) = (oeisIx .) . on (+) (oeisIx @265)

-- instance OEIS 258197 where
--   oeis = tablList @258197
-- instance Table 258197 where
--   tabl = map (map (oeisIx @3415)) (tabl @7318)

-- instance OEIS 258217 where
--   oeis = f 1 $ tail $ zip (oeis @8589) $ map show (oeis @8589) where
--      f x ws = g ws where
--        g ((u, vs) : uvs) = if isPrefixOf (show x) vs
--                            then u : f (x + 1) (delete (u, vs) ws) else g uvs

-- instance OEIS 258225 where
--   oeisIx = flip div 3 . (oeisIx @258083)

-- instance OEIS 258226 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258225))

-- instance OEIS 258227 where
--   oeis = f 12 1 (map fi $ tail (oeis @7376)) where
--      f x y (d:ds) | gcd x y > 1 = y : f y d ds
--                   | otherwise   = f x (10 * y + d) ds

-- instance OEIS 258262 where
--   oeis = filter ((== 1) . (oeisIx @10057)) (oeis @258865)

-- instance OEIS 258290 where
--   oeisIx = (oeisIx @3415) . (oeisIx @984)

-- instance OEIS 258317 where
--   oeisIx = sum . (rowT @258197)

-- instance OEIS 258318 where
--   import Data.Set (singleton, insert, size)
--   oeisIx n = (oeis @258318) !! n
--   oeis = f 2 (tabl @258197) $ singleton 0 where
--      f k (xs:xss) zs = g (take (div k 2) xs) zs where
--        g []     ys = size ys : f (k + 1) xss ys
--        g (x:xs) ys = g xs (insert x ys)

-- instance OEIS 258324 where
--   oeisIx n = foldl lcm 1 $ map (n -) $ (rowT @27751) n

-- instance OEIS 258329 where
--   oeisIx = flip div 7 . (oeisIx @258188)

-- instance OEIS 258330 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258329))

-- instance OEIS 258334 where
--   oeisIx = flip div 7 . (oeisIx @258217)

-- instance OEIS 258335 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258334))

-- instance OEIS 258337 where
--   oeis = f 1 $ map show (oeis @40) where
--      f x pss = g pss where
--        g (qs:qss) = if show x `isPrefixOf` qs
--                        then (read qs :: Int) : f (x + 1) (delete qs pss)
--                        else g qss

-- instance OEIS 258353 where
--   oeis = filter ((== 1) . (oeisIx @212306)) [1..]

-- instance OEIS 258354 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @212306))

-- instance OEIS 258383 where
--   oeis = map length $ group (oeis @62234)

-- instance OEIS 258409 where
--   oeisIx n = foldl1 gcd $ map (subtract 1) $ tail $ (rowT @27750)' n

-- instance OEIS 258432 where
--   oeis = map (snd . head) $
--                       groupBy ((==) `on` fst) $ zip (oeis @62234) [1..]

-- instance OEIS 258437 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258383))

-- instance OEIS 258449 where
--   oeis = map (oeisIx @258432) $ filter ((== 3) . (oeisIx @258383)) [1..]

-- instance OEIS 258469 where
--   oeis = map (oeisIx @258432) $ filter ((== 1) . (oeisIx @258383)) [1..]

-- instance OEIS 258565 where
--   oeisIx = (oeisIx @3415) . (oeisIx @1694)

-- instance OEIS 258567 where
--   oeisIx = (oeisIx @20639) . (oeisIx @1694)

-- instance OEIS 258568 where
--   oeisIx = (oeisIx @20639) . (oeisIx @36966)

-- instance OEIS 258569 where
--   oeisIx = (oeisIx @20639) . (oeisIx @36967)

-- instance OEIS 258570 where
--   oeisIx = (oeisIx @20639) . (oeisIx @69492)

-- instance OEIS 258571 where
--   oeisIx = (oeisIx @20639) . (oeisIx @69493)

-- instance OEIS 258599 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258567)) . (oeisIx @40)

-- instance OEIS 258600 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258568)) . (oeisIx @40)

-- instance OEIS 258601 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258569)) . (oeisIx @40)

-- instance OEIS 258602 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258570)) . (oeisIx @40)

-- instance OEIS 258603 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258571)) . (oeisIx @40)

-- instance OEIS 258613 where
--   oeis = filter ((== 1) . (oeisIx @74695)) [1..]

-- instance OEIS 258614 where
--   oeis = filter ((> 1) . (oeisIx @74695)) [1..]

-- instance OEIS 258682 where
--   oeisIx n = read ('0' : minus (show (n ^ 2)) (show n)) :: Int  where
--      minus [] _  = []
--      minus us [] = us
--      minus (u:us) vs | elem u vs = minus us $ delete u vs
--                      | otherwise = u : minus us vs

-- instance OEIS 258706 where
--   oeis = f (oeis @40) where
--      f ps'@ (p:ps) | any (== 0) (map (oeisIx @10051)' dps) = f ps
--                   | otherwise = p : f (ps' \\ dps)
--                   where dps = map read $ permutations $ show p

-- instance OEIS 258708 where
--   oeis = tablList @258708
-- instance Table 258708 where
--   rowCol = rowCol_off @258708 @1 @0
--   rowT   = rowT_off   @258708 @1
--   tabl = zipWith (zipWith ((round .) . ((/) `on` fi)))
--                          (tabl @258993) (tabl @158405)

-- instance OEIS 258738 where
--   oeis = f [1..] where
--      f (x:xs) = if show x `elem` zipWith (++) kss (map show $ (rowT @27750)' x)
--                    then x : f xs else f xs
--      kss = map show [1..]

-- instance OEIS 258765 where
--   oeisIx = fromJust . (`elemIndex` (map abs (oeis @258057)))

-- instance OEIS 258767 where
--   oeis = 1 : f 1 [2..] where
--      f x zs = g zs where
--        g (y:ys) | (oeisIx @8966) (x^2 + y^2) == 1 = g ys
--                 | otherwise = y : f y (delete y zs)

-- instance OEIS 258768 where
--   oeis = [x | x <- [1..], (oeisIx @258767) x == x]

-- instance OEIS 258827 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @258767))

-- instance OEIS 258865 where
--   import Data.Set (singleton, deleteFindMin, fromList)
--   import qualified Data.Set as Set (union)
--   import qualified Data.List.Ordered as List (union)
--   oeisIx n = (oeis @258865) !! (n - 1)
--   oeis = tail $ f (singleton 1) 1 [] [] (oeis @30078) where
--      f s z vs qcs pcs'@ (pc:pcs)
--        | m < z = m : f s' z vs qcs pcs'
--        | otherwise = f (Set.union s $ fromList $ map (+ pc) ws)
--                        pc ws (pc:qcs) pcs
--        where ws = List.union vs $ map (+ pc) (pc : qcs)
--              (m, s') = deleteFindMin s

-- instance OEIS 258993 where
--   oeis = tablList @258993
-- instance Table 258993 where
--   rowCol = rowCol_off @258993 @1 @0
--   rowT   = rowT_off   @258993 @1
--   tabl = zipWith (zipWith (oeisIx @7318)) (tabl @94727) (tabl @4736)

-- instance OEIS 259043 where
--   oeisIx x = if x < 10 then x else (oeisIx @259043) (x' + d) + d
--               where (x', d) = divMod x 10

-- instance OEIS 259046 where
--   oeisIx = fromJust . (`elemIndex` (map (oeisIx @259043) [0..]))

-- instance OEIS 259143 where
--   oeis = [length $ nub $ show m ++ show d |
--                   m <- [1 .. 12], d <- [1 .. (oeisIx @8685) m]]

-- instance OEIS 259315 where
--   oeis = filter ((== 0) . (oeisIx @10051)') (oeis @52382)

-- instance OEIS 259361 where
--   oeisIx = floor . subtract (1 / 2) . sqrt . (+ 1 / 4) . fi
--   oeis = concat xss where
--      xss = iterate (\ (xs@ (x:_)) -> map (+ 1) (x : x : xs)) [0, 0]

-- instance OEIS 259366 where
--   oeis = filter (\x -> (oeisIx @60682) x < (oeisIx @5)' x - 1) [2..]

-- instance OEIS 259645 where
--   import Data.List.Ordered (isect)
--   oeisIx n = (oeis @259645) !! (n - 1)
--   oeis = (oeis @5574) `isect` (oeis @87370) `isect` (oeis @56561)

-- instance OEIS 259966 where
--   oeis = 0 : 0 : 2 : 7 : zipWith (+)
--      (zipWith3 (((+) .) . (+))
--                (oeis @259966) (drop 2 (oeis @259966)) (drop 3 (oeis @259966)))
--      (drop 2 $ zipWith (+)
--                (map (* 2) $ drop 2 (oeis @5251)) (map (* 3) (oeis @5251)))

-- instance OEIS 259967 where
--   oeis = 3 : 2 : 2 : 5 : zipWith3 (((+) .) . (+))
--      (oeis @259967) (drop 2 (oeis @259967)) (drop 3 (oeis @259967))

-- instance OEIS 259968 where
--   oeis = 1 : 1 : 3 : 6 : zipWith3 (((+) .) . (+))
--      (oeis @259968) (drop 2 (oeis @259968)) (drop 3 (oeis @259968))

-- instance OEIS 259969 where
--   oeisIx n = (oeis @259967) !! n
--   oeis = 3 : 2 : 2 : 5 : zipWith3 (((+) .) . (+))
--      (oeis @259967) (drop 2 (oeis @259967)) (drop 3 (oeis @259967))

-- instance OEIS 260020 where
--   oeis = filter
--                  (\x -> 2 * (oeisIx @10)' x == (oeisIx @10)' (2 * (oeisIx @203)' x)) [1..]

-- instance OEIS 260022 where
--   oeisIx = (oeisIx @6921) . (* 2)

-- instance OEIS 260031 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = if x > 0 then x else f $ div (n ^ n) 12
--             where x = powerMod n n 12
--                   f z = if m == 0 then f z' else m
--                         where (z', m) = divMod z 12

-- instance OEIS 260254 where
--   oeisIx n = sum $ map (oeisIx . (n -)) $
--                  takeWhile (<= n `div` 2) (oeis @2113)

-- instance OEIS 260255 where
--   oeis = filter ((> 0) . (oeisIx @260254)) [0..]

-- instance OEIS 260273 where
--   oeis = iterate (\x -> x + (oeisIx @261461) x) 1

-- instance OEIS 260485 where
--   oeisIx = head . (rowT @260580)

-- instance OEIS 260580 where
--   import Data.List.Ordered (union); import Data.List ((\\))
--   oeisIx n k = (tabf @260580) !! (n - 1) !! (k-1)
--   oeisIx_row n = (tabf @260580) !! (n - 1)
--   oeisIx_tabf = zipWith (\\) (tail zss) zss where
--                               zss = scanl union [] (tabl @65305)

-- instance OEIS 260633 where

-- instance OEIS 260664 where
--   oeisIx = sum . zipWith (*) (oeis @87960) . map (oeisIx @133042) . (rowT @260672)

-- instance OEIS 260669 where
--   oeisIx = flip div 2 . (oeisIx @54440)

-- instance OEIS 260672 where
--   oeis = tablList @260672
-- instance Table 260672 where
--   tabf = map (takeWhile (>= 0) . flip map (oeis @1318) . (-)) [0..]

-- instance OEIS 260682 where
--   oeis = filter ((== 1) . flip mod 6) (oeis @3136)

-- instance OEIS 260689 where
--   oeis = tablList @260689
-- instance Table 260689 where
--   rowCol = rowCol_off @260689 @2 @1
--   rowT n = [m | m <- [1, 3 .. 2 * n - 3],
--                        (oeisIx @10051)' (2*n + m) == 1, (oeisIx @10051)' (2*n - m) == 1]
--   tabf = map (rowT @260689) [2..]

-- instance OEIS 260706 where
--   oeisIx = sum . (rowT @260672)

-- instance OEIS 260797 where
--   oeisIx = (oeisIx @98743) . (oeisIx @142)

-- instance OEIS 260822 where
--   oeis = f 1 [1..] where
--      f x zs = g zs where
--        g (y:ys) = if y /= x && (oeisIx @10051)' (x + y) == 0
--                      then y : f (x + 1) (delete y zs) else g ys

-- instance OEIS 260895 where
--   oeisIx = sum . map (oeisIx @10051)' . (rowT @77664)

-- instance OEIS 260910 where
--   oeis = tablList @260910
-- instance Table 260910 where
--   rowCol = rowCol_off @260910 @1 @1
--   rowT   = rowT_off   @260910 @1
--   tabl = zipWith (map . sylvester) [1..] (tabl @77664) where
--      sylvester u v = u * v - u - v

-- instance OEIS 260933 where
--   oeis = f 1 [1..] where
--      f x zs = g zs where
--        g (y:ys) = if (oeisIx @10051)' (x + y) == 0 && (oeisIx @10051)' (x + y + 1) == 0
--                      then y : f (x + 1) (delete y zs) else g ys

-- instance OEIS 260936 where
--   oeis = [x | x <- [1..], (oeisIx @260933) x == x]

-- instance OEIS 260987 where
--   (oeis, (oeis @260633)) = unzip $ f 1 0 where
--      f x r = if y > r then (y, x) : f (x + 1) y else f (x + 1) r
--              where y = (oeisIx @8480) x

-- instance OEIS 261009 where
--   oeisIx = (oeisIx @53735) . (oeisIx @79)

-- instance OEIS 261012 where
--   oeisIx = signum
--   oeis = -1 : 0 : [1, 1 ..]

-- instance OEIS 261016 where
--   oeisIx = sum . zipWith (*) [0..] . (rowT @261019)'

-- instance OEIS 261017 where
--   oeisIx = subtract 1 . length . (rowT @261019)

-- instance OEIS 261018 where
--   oeis = zipWith (-) (tail (oeis @260273)) (oeis @260273)

-- instance OEIS 261019 where
--   oeis = tablList @261019
-- instance Table 261019 where
--   rowCol = rowCol_off @261019 @1 @0
--   rowT   = rowT_off @261019 @1
--   tabf = map (i 0 . group . sort . map f) (tabf @76478)
--      where f bs = g (tabf @30308) where
--              g (cs:css) | isInfixOf cs bs = g css
--                         | otherwise = foldr (\d v -> 2 * v + d) 0 cs
--            i _ [] = []
--            i r gss'@ (gs:gss) | head gs == r = (length gs) : i (r + 1) gss
--                              | otherwise    = 0 : i (r + 1) gss'

-- instance OEIS 261089 where
--   oeisIx = fromJust . (`elemIndex` (oeis @155043))

-- instance OEIS 261121 where
--   oeisIx = (oeisIx @98743) . (oeisIx @2110)

-- instance OEIS 261189 where
--   import Data.List.Ordered (isect)
--   oeisIx n = (oeis @261189) !! (n - 1)
--   oeis = (oeis @52382) `isect` (oeis @52413)

-- instance OEIS 261255 where
--   oeisIx n = fromJust (findIndex (== (oeisIx @7335) n) (oeis @3586)) + 1

-- instance OEIS 261256 where
--   oeisIx n = (oeisIx @257851) n (n - 1)

-- instance OEIS 261279 where
--   oeis = [x | x <- [0..], (oeisIx @65649) x == x]

-- instance OEIS 261293 where
--   oeisIx = (oeisIx @65649) . (oeisIx @65649)

-- instance OEIS 261294 where
--   oeisIx = (oeisIx @65650) . (oeisIx @65650)

-- instance OEIS 261301 where
--   oeis = 1 : map abs
--      (zipWith (-) (oeis @261301) (zipWith gcd [1..] (oeis @261301)))

-- instance OEIS 261333 where
--   oeis = zipWith (+) (map ((* 10) . subtract 1) (oeis @256100)) (oeis @7376)

-- instance OEIS 261334 where
--   oeisIx = fromJust . (`elemIndex` (oeis @261333))

-- instance OEIS 261335 where
--   oeis = [x | x <- [0..], (oeisIx @261333) x == x]

-- instance OEIS 261366 where
--   oeisIx = sum . map ((1 -) . flip mod 2) . (rowT @261363)

-- instance OEIS 261392 where
--   oeisIx = maximum . (rowT @261019)

-- instance OEIS 261396 where
--   oeis = f 1 1 (oeis @260273) where
--      f z k (x:xs) | x >= z    = k : f (2 * z) (k + 1) xs
--                   | otherwise = f z (k + 1) xs

-- instance OEIS 261441 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @261019)' (n + 3) 5

-- instance OEIS 261442 where
--   oeisIx n = (oeisIx @261019)' (n + 6) 6

-- instance OEIS 261443 where
--   oeisIx n = (oeisIx @261019)' (n + 5) 7

-- instance OEIS 261461 where
--   oeisIx x = f $ tail (tabf @30308) where
--      f (cs:css) = if isInfixOf cs (oeisIx_row x)
--                      then f css else foldr (\d v -> 2 * v + d) 0 cs

-- instance OEIS 261466 where
--   (oeis, (oeis @261467)) = unzip $ (0, 1) : f 0 1 where
--      f i x | y > x     = (y, i) : f (i + 1) y
--            | otherwise = f (i + 1) x
--            where y = (oeisIx @261461) i

-- instance OEIS 261467 where

-- instance OEIS 261518 where
--   oeis = 1 : zipWith (-)
--                  (map (oeisIx @40) (zipWith (+) (oeis @261518) [1..])) (oeis @40) [1]

-- instance OEIS 261525 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @31131)) . (* 2)

-- instance OEIS 261575 where
--   oeis = tablList @261575
-- instance Table 261575 where
--   tabf = [0] : [1] :
--      zipWith (add 0) (tail (tabf @261575)) (tabf @261575) where
--      add c (a:as) (b:bs) = y : add c' as bs where (c', y) = divMod (a+b+c) 60
--      add c (a:as) [] = y : add c' as [] where (c', y) = divMod (a+c) 60
--      add 1 _ _ = [1]
--      add _ _ _ = []

-- instance OEIS 261585 where
--   oeisIx = genericLength . (rowT @261575)

-- instance OEIS 261587 where
--   oeisIx = sum . (rowT @261575)

-- instance OEIS 261598 where
--   oeisIx = product . (rowT @261575)

-- instance OEIS 261606 where
--   oeis = 0 : 1 : map (flip mod 60)
--                              (zipWith (+) (oeis @261606) $ tail (oeis @261606))

-- instance OEIS 261607 where
--   oeisIx = last . (rowT @261575)

-- instance OEIS 261644 where
--   oeis = zipWith (-)
--                  (map (oeisIx @62383) (oeis @260273)) $ map fi (oeis @260273)
--   oeisIx_tabf = [1] : f (tail $ zip (oeis @261645) (oeis @261644)) where
--      f dxs = (map snd (dxs'' ++ [dx])) : f dxs' where
--        (dxs'', dx:dxs') = span ((<= 0) . fst) dxs
--   oeisIx_row n = (tabf @261644) !! (n - 1)

-- instance OEIS 261645 where
--   oeis = zipWith (-) (tail (oeis @261644)) (oeis @261644)

-- instance OEIS 261646 where
--   oeisIx = genericLength . (rowT @261644)

-- instance OEIS 261712 where
--   oeis = tablList @261712
-- instance Table 261712 where
--   rowCol = rowCol_off @261712 @1 @1
--   rowT   = rowT_off @261712 @1
--   tabf = map reverse (tabf @261644)

-- instance OEIS 261723 where
--   oeis = concat $ transpose [tail (oeis @52548), tail (oeis @51)]

-- instance OEIS 261727 where
--   oeis = map (length . takeWhile (== 0)) $
--                      zipWith (zipWith (-)) (tabf @261712) $ tail (tabf @261712)

-- instance OEIS 261786 where
--   oeis = iterate (\x -> x + (oeisIx @261787) x) 1

-- instance OEIS 261787 where
--   oeisIx x = f $ tail (tabf @30341) where
--      f (cs:css) = if isInfixOf cs (oeisIx_row x)
--                      then f css else foldr (\d v -> 3 * v + d) 0 cs

-- instance OEIS 261788 where
--   oeis = f 1 1 (oeis @261786)' where
--      f z k (x:xs) | x >= z    = k : f (3 * z) (k + 1) xs
--                   | otherwise = f z (k + 1) xs

-- instance OEIS 261789 where
--   oeis = zipWith (-) (tail (oeis @261786)') (oeis @261786)'

-- instance OEIS 261793 where
--   oeis = iterate (\x -> x + (oeisIx @261794) x) 1

-- instance OEIS 261794 where
--   oeisIx x = f $ tail (tabf @31298) where
--      f (cs:css) = if isInfixOf cs (oeisIx_row x)
--                      then f css else foldr (\d v -> 10 * v + d) 0 cs

-- instance OEIS 261795 where
--   oeis = zipWith (-) (tail (oeis @261793)') (oeis @261793)'

-- instance OEIS 261869 where
--   oeis = zipWith (-) (tail (oeis @55615)) (oeis @55615)

-- instance OEIS 261890 where
--   oeis = zipWith (-) (tail (oeis @261869)) (oeis @261869)

-- instance OEIS 261893 where
--   oeisIx n = n * (n * (n + 2) + 3) + 1
--   oeis = zipWith (-) (tail (oeis @578)) (oeis @290)

-- instance OEIS 261897 where
--   oeis = tablList @261897
-- instance Table 261897 where
--   rowCol = rowCol_off @261897 @0 @1
--   tabl = [1] : f 1 0 [1] where
--      f t h xs | t <= (h + 1) ^ 2  = ys : f (t + 1) h ys
--               | otherwise         = ys' : f (t + 1) (h + 1) ys'
--               where ys = zipWith (+) ([0] ++ xs) (xs ++ [0])
--                     ys' = zipWith (+) ([0] ++ xs) (us ++ (0:vs) ++ [0])
--                     (us, _:vs) = splitAt h xs

-- instance OEIS 261922 where
--   oeisIx x = f (tabf @30308) where
--      f (cs:css) = if isInfixOf cs (oeisIx_row x)
--                      then f css else foldr (\d v -> 2 * v + d) 0 cs

-- instance OEIS 261923 where
--   oeisIx n = fst $ until ((== 0) . snd)
--                           (\ (step, x) -> (step + 1, (oeisIx @261922) x)) (0, n)

-- instance OEIS 261930 where
--   oeisIx = sum . (rowT @261897)

-- instance OEIS 261969 where
--   oeisIx n = product $ map fst $ filter ((== emax) . snd) $ zip ps es
--       where emax = maximum es
--             ps = (rowT @27748) n; es = (rowT @124010) n

-- instance OEIS 262038 where
--   oeis = f 0 (oeis @2113) where
--      f n ps'@ (p:ps) = p : f (n + 1) (if p > n then ps' else ps)

-- instance OEIS 262065 where
--   import Data.List.Ordered (union)
--   oeisIx n = (oeis @262065) !! (n - 1)
--   oeis = union us vs where
--      us = [val60 $ bs ++ reverse bs | bs <- bss]
--      vs = [0..59] ++ [val60 $ bs ++ cs ++ reverse bs |
--             bs <- tail bss, cs <- take 60 bss]
--      bss = iterate s [0] where
--            s [] = [1]; s (59:ds) = 0 : s ds; s (d:ds) = (d + 1) : ds
--      val60 = foldr (\b v -> 60 * v + b) 0

-- instance OEIS 262069 where
--   -- import Data.List.Ordered (isect)
--   oeisIx n = (oeis @262069) !! (n - 1)
--   oeis = isect (oeis @2113) (oeis @262065)

-- instance OEIS 262079 where
--   oeis = zipWith (-) (tail (oeis @262065)) (oeis @262065)

-- instance OEIS 262095 where
--   oeisIx = sum . map ((1 -) . (oeisIx @64911)) . (rowT @27750)

-- instance OEIS 262138 where
--   oeis = concat $ transpose [oeis, (oeis @36263)]

-- instance OEIS 262188 where
--   oeis = tablList @262188
-- instance Table 262188 where
--   tabf = map (sort . nub . map (foldr (\d v -> 10 * v + d) 0) .
--      filter (\xs -> length xs == 1 || last xs > 0 && reverse xs == xs) .
--             concatMap (tail . inits) . tails) (tabf @31298)

-- instance OEIS 262198 where
--   oeis = [x | x <- [0..], (oeisIx @55642) x /= (oeisIx @262190) x]

-- instance OEIS 262223 where
--   oeisIx n = n + (oeisIx @47813) n

-- instance OEIS 262224 where
--   oeisIx' n = (oeis @262224)' !! n
--   oeis' = iterate (oeisIx @262223) 1

-- instance OEIS 262243 where
--   oeis = zipWith (-) (tail (oeis @262224)) (oeis @262224)

-- instance OEIS 262255 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @262323))

-- instance OEIS 262257 where
--   oeis = zipWith (levenshtein `on` show) [0..] (oeis @261423) where
--      levenshtein us vs = last $ foldl transform [0..length us] vs where
--        transform xs@ (x:xs') c = scanl compute (x+1) (zip3 us xs xs') where
--          compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

-- instance OEIS 262279 where
--   oeisIx = fromJust . (`elemIndex` (oeis @261923))

-- instance OEIS 262282 where
--   oeis = 11 : f "1" (map show (delete 11 (oeis @40))) where
--      f xs pss = (read ys :: Integer) :
--                 f (dropWhile (== '0') ys') (delete ys pss)
--                 where ys@ (_:ys') = head $ filter (isPrefixOf xs) pss

-- instance OEIS 262283 where
--   oeis = 2 : f "" (map show $ tail (oeis @40)) where
--      f xs pss = (read ys :: Integer) :
--                 f (dropWhile (== '0') ys') (delete ys pss)
--                 where ys@ (_:ys') = head $ filter (isPrefixOf xs) pss

-- instance OEIS 262323 where
--   oeis = 1 : f "1" (map show [2..]) where
--      f xs zss = g zss where
--        g (ys:yss) | null (intersect its $ tail $ inits ys) &&
--                     null (intersect tis $ init $ tails ys) = g yss
--                   | otherwise = (read ys :: Int) : f ys (delete ys zss)
--        its = init $ tails xs; tis = tail $ inits xs

-- instance OEIS 262356 where
--   import Data.Set (singleton, notMember, insert)
--   oeisIx n = (oeis @262356) !! (n - 1)
--   oeis = 1 : f "" (singleton "1") where
--      f xs s = (read ys :: Int) : f (dropWhile (== '0') ys') (insert ys s)
--        where ys@ (_:ys') = head
--                [vs | vs <- zss, isPrefixOf xs vs, notMember vs s]
--      zss = map show [2..]

-- instance OEIS 262358 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @262356))

-- instance OEIS 262360 where
--   oeis = [x | x <- [1..], (oeisIx @262356) x == x]

-- instance OEIS 262363 where
--   oeis = filter ((== 1) . (oeisIx @10051)') (oeis @262356)

-- instance OEIS 262367 where
--   oeis = [x | x <- [1..], (oeisIx @262323) x == x]

-- instance OEIS 262371 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @262356)) [1..]

-- instance OEIS 262377 where
--   oeis = filter ((== 1) . (oeisIx @10051)') $ map (oeisIx @262358) [1..]

-- instance OEIS 262378 where
--   oeis = filter ((== 1) . (oeisIx @10051)' . (oeisIx @262358)) [1..]

-- instance OEIS 262390 where
--   oeis = filter ((== 1) . (oeisIx @30)) (oeis @262356)

-- instance OEIS 262393 where
--   oeis = filter ((== 1) . (oeisIx @30) . (oeisIx @262356)) [1..]

-- instance OEIS 262401 where
--   oeisIx = product . map (oeisIx @54055) . (rowT @27746)'

-- instance OEIS 262411 where
--   oeis = 1 : f [1] (drop 2 (tabf @30341)) where
--      f xs tss = g tss where
--        g (ys:yss) | null (intersect its $ tail $ inits ys) &&
--                     null (intersect tis $ init $ tails ys) = g yss
--                   | otherwise = (foldr (\t v -> 3 * v + t) 0 ys) :
--                                 f ys (delete ys tss)
--        its = init $ tails xs; tis = tail $ inits xs

-- instance OEIS 262412 where
--   oeis = 1 : f [1] (drop 2 (tabf @30341)) where
--      f xs tss = g tss where
--        g (ys:yss) | null (intersect its $ tail $ inits ys) &&
--                     null (intersect tis $ init $ tails ys) = g yss
--                   | otherwise = (foldr (\t v -> 10 * v + t) 0 ys) :
--                                 f ys (delete ys tss)
--        its = init $ tails xs; tis = tail $ inits xs

-- instance OEIS 262429 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @262411))

-- instance OEIS 262435 where
--   oeis = [x | x <- [1..], (oeisIx @262411) x == x]

-- instance OEIS 262437 where
--   oeis = tablList @262437
-- instance Table 262437 where
--   tabf = iterate succ [0] where
--      succ []      = [1]
--      succ (15:hs) = 0 : succ hs
--      succ (h:hs)  = (h + 1) : hs

-- instance OEIS 262438 where
--   oeisIx = genericLength . (rowT @262437)

-- instance OEIS 262460 where
--   oeis = 1 : f [1] (drop 2 (tabf @262437)) where
--      f xs tss = g tss where
--        g (ys:yss) | null (intersect its $ tail $ inits ys) &&
--                     null (intersect tis $ init $ tails ys) = g yss
--                   | otherwise = (foldr (\t v -> 16 * v + t) 0 ys) :
--                                 f ys (delete ys tss)
--        its = init $ tails xs; tis = tail $ inits xs

-- instance OEIS 262461 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @262460))

-- instance OEIS 262481 where
--   oeis = filter (\x -> (oeisIx @120) x == (oeisIx @20639) x) [1..]

-- instance OEIS 262530 where
--   oeis = filter f [1..] where
--      f = g d10' . show where
--        g _ [] = True
--        g ts (d:ds) = elem d ts && g (delete d ts) ds
--      d10' = d10 ++ d10; d10 = "0123456789"

-- instance OEIS 262557 where
--   oeis = 0 : f [[0]] where
--      f xss = if x < 9 then (map (read . concatMap show) zss) ++ f zss else []
--              where zss = (map (z :) $ map tail xss) ++ (map (z :) xss)
--                    z = x + 1; x = head $ head xss

-- instance OEIS 262604 where
--   oeis = zipWith (-) (tail (oeis @252022)) (oeis @252022)

-- instance OEIS 262663 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75348))

-- instance OEIS 262665 where
--   oeis = [x | x <- [1..], (oeisIx @75348)' x == x]

-- instance OEIS 262675 where
--   oeis = filter
--      (all (== 1) . map (oeisIx . fi) . (rowT @124010)) [1..]

-- instance OEIS 262703 where
--   oeis = zipWith (-) (tail (oeis @252001)) (oeis @252001)

-- instance OEIS 263017 where
--   import Data.IntMap (empty, findWithDefault, insert)
--   oeisIx n = (oeis @263017) !! (n - 1)
--   oeis = f 1 empty where
--      f x m = y : f (x + 1) (insert h (y + 1) m) where
--              y = findWithDefault 1 h m
--              h = (oeisIx @120) x

-- instance OEIS 263109 where
--   import Data.IntMap (empty, findWithDefault, insert)
--   oeisIx n = (oeis @263109) !! (n - 1)
--   oeis = f 1 empty where
--      f x m = y : f (x + 1) (insert q (y + 1) m) where
--              y = findWithDefault 1 q m; q = (oeisIx @53832) x

-- instance OEIS 263110 where
--   import Data.IntMap (empty, findWithDefault, insert)
--   oeisIx n = (oeis @263110) !! (n - 1)
--   oeis = f 1 empty where
--      f x m = y : f (x + 1) (insert q (y + 1) m) where
--              y = findWithDefault 1 q m; q = (oeisIx @53836) x

-- instance OEIS 263231 where
--   oeisIx n = n * (25 * n - 39) `div` 2
--   oeis = 0 : -7 : 11 : zipWith (+) (oeis @263231)
--      (map (* 3) $ tail $ zipWith (-) (tail (oeis @263231)) (oeis @263231))

-- instance OEIS 263327 where
--   oeisIx 0 = 0
--   oeisIx n = head [x | x <- [1..1023], (oeisIx @262557) x == (oeisIx @9995)' n]

-- instance OEIS 263328 where
--   oeisIx 0 = 0
--   oeisIx n = head [x | x <- [1..1023], (oeisIx @9995)' x == (oeisIx @262557) n]

-- instance OEIS 263329 where
--   oeis = [x | x <- [0..1023], (oeisIx @263327) x == x]

-- instance OEIS 263355 where
--   oeis = tablList @263355
-- instance Table 263355 where
--   rowCol = rowCol_off @263355 @1 @1
--   rowT   = rowT_off @263355 @1
--   tabf = sort $ cc (oeis @263327) where
--      cc [] = []
--      cc (x:xs) = (reverse $ sort ys) : cc (xs \\ ys)
--         where ys = x : c x
--               c z = if y /= x then y : c y else []
--                     where y = (oeisIx @263327) z

-- instance OEIS 263383 where
--   oeisIx = genericLength . (rowT @263355)

-- instance OEIS 263766 where
--   oeis = scanl (*) 1 (oeis @8865)

-- instance OEIS 263774 where
--   oeisIx 1 = 1
--   oeisIx n = foldl (-) (oeisIx n) $ zipWith (^) (map a' $ reverse ds) ds
--               where a' x = if x == n then 0 else (oeisIx @263774) x
--                     ds = (rowT @27750) n

-- instance OEIS 263808 where
--   oeis = filter essicran [1..] where
--      essicran x = last (takeWhile (>= -x) es) == -x where
--        es = scanl (-) x (cycle $ map (read . return) $ show x)

-- instance OEIS 263837 where
--   oeis = filter (\x -> (oeisIx @1065) x <= x) [1..]

-- instance OEIS 263838 where
--   import Data.List.Ordered (union)
--   oeisIx n = (oeis @263838) !! (n - 1)
--   oeis = union (oeis @257719) (oeis @257720)

-- instance OEIS 263845 where
--   oeis = filter (> 0) (oeis @258059)

-- instance OEIS 263847 where
--   oeis = 0 : zipWith (-)
--      (zipWith (-) (tail qs) qs) (drop 2 (oeis @41))
--      where qs = es $ tail (oeis @41)
--            es [] = []; es [x] = []; es (_:x:xs) = x : es xs

-- instance OEIS 263856 where
--   oeis = f [] (oeis @4676) where
--      f bps (x:xs) = y : f bps' xs where
--        y = fromJust (elemIndex x bps') + 1
--        bps' = insertBy (compare `on` (reverse . show)) x bps

-- instance OEIS 263896 where
--   oeisIx n = (oeisIx @75383) (2 * n - 1) n

-- instance OEIS 263922 where
--   oeisIx = (oeisIx @51903) . (oeisIx @984)

-- instance OEIS 263924 where
--   import Math.NumberTheory.Primes.Factorisation (factorise)
--   oeisIx n = (oeis @263924) !! (n - 1)
--   oeis = filter f [2..] where
--      f x = not (null pe23s) && any ((> e23) . snd) pes' where
--            e23 = maximum (map snd pe23s)
--            (pe23s, pes') = span ((<= 3) . fst) $ factorise $ (oeisIx @984) x

-- instance OEIS 264164 where
--   oeis = filter ((== 0) . (oeisIx @65333) . (oeisIx @5)') (oeis @3586)

-- instance OEIS 264165 where
--   oeis = filter ((== 1) . (oeisIx @65333) . (oeisIx @5)') (oeis @3586)

-- instance OEIS 264526 where
--   oeisIx = head . (rowT @260689)

-- instance OEIS 264527 where
--   oeisIx = last . (rowT @260689)

-- instance OEIS 264618 where
--   oeisIx n = foldr (\b v -> 2 * v + b) 0 $ (reverse bs ++ (0 : bs))
--               where bs = map fi $ (rowT @30308) n

-- instance OEIS 264619 where
--   oeisIx 0 = 1
--   oeisIx n = foldr (\b v -> 2 * v + b) 0 $ (reverse bs ++ (1 : bs))
--               where bs = map fi $ (rowT @30308) n

-- instance OEIS 264646 where
--   oeis = 11 : f 2 [0, 1, 1] where
--      f x digs = (foldl (\v d -> 10 * v + d) 0 ys) : f (x + 1) (digs ++ ys)
--        where ys = map (read . return) (show x) ++ [genericIndex digs x]

-- instance OEIS 264647 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @263856))

-- instance OEIS 264740 where
--   oeisIx = sum . map (oeisIx @265) . (rowT @27750)'

-- instance OEIS 264782 where
--   oeisIx n = sum $ zipWith (^) (map (oeisIx @8683) divs) (reverse divs)
--               where divs = (rowT @27750) n

-- instance OEIS 264856 where
--   oeisIx n = fromJust $ findIndex (elem n) (tabl @125605)

-- instance OEIS 264893 where
--   oeis = zipWith (-) (tail (oeis @155043)) (oeis @155043)

-- instance OEIS 264898 where
--   oeis = filter ((== 0) . (oeisIx @264893)) [0..]

-- instance OEIS 264959 where
--   oeisIx n = (oeisIx @257851) n n

-- instance OEIS 265008 where
--   oeisIx n = genericLength [ () | let cs = (rowT @165416) n, c <- cs,
--               let as = takeWhile (<= c) cs, a <- as, b <- as, a * b == c]

-- instance OEIS 265012 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 10 (p - 1) (p ^ 2) where p = (oeisIx @40) n

-- instance OEIS 265183 where
--   oeisIx n = genericLength [ () | let cs = (rowT @218978) n, a <- cs, b <- cs, c <- cs,
--                            a * b == c || c == 0 && a * b == 0]

-- instance OEIS 265236 where
--   oeisIx n = genericLength [ () | let cs = (rowT @119709) n, a <- cs, b <- cs, c <- cs,
--                            a * b == c || c == 0 && a * b == 0]

-- instance OEIS 265327 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @238324))

-- instance OEIS 265377 where
--   import Data.Set (singleton, deleteFindMin, insert, Set)
--   oeisIx n = (oeis @265377) !! (n - 1)
--   oeis = f (singleton (1 + 2^3, (1, 2))) (-1) where
--      f s z = if y /= z then y : f s'' y else f s'' y
--                 where s'' = (insert (y', (i, j')) $
--                              insert (y' - i ^ 3 , (i + 1, j')) s')
--                       y' = y + j' ^ 3; j' = j + 1
--                       ((y, (i, j)), s') = deleteFindMin s

-- instance OEIS 265509 where
--   oeis = f (tail (tabf @30308)) [[]] where
--      f (bs:_:bss) pss = y : f bss pss' where
--        y = foldr (\d v -> 2 * v + d) 0 ys
--        (ys:_) = dropWhile (\ps -> not $ and $ zipWith (<=) ps bs) pss'
--        pss' = if bs /= reverse bs then pss else bs : pss

-- instance OEIS 265510 where
--   oeisIx = (oeisIx @7088) . (oeisIx @265509)

-- instance OEIS 265525 where
--   oeis = f (tabf @31298) [[]] where
--      f (ds:dss) pss = y : f dss pss' where
--        y = foldr (\d v -> 10 * v + d) 0 ys
--        (ys:_) = dropWhile (\ps -> not $ and $ zipWith (<=) ps ds) pss'
--        pss' = if ds /= reverse ds then pss else ds : pss

-- instance OEIS 265675 where
--   oeis = map (\ (x:xs) -> length $ filter ((== 1) . gcd x) xs) $
--                      map reverse $ tail $ inits (oeis @5117)

-- instance OEIS 265845 where
--   import Data.Set (singleton, deleteFindMin, insert, Set)
--   oeisIx n = (oeis @265845) !! (n - 1)
--   oeis = f (singleton (1, (1, 1))) 0 0 where
--      f s z z' = if y == z && z' /= z then y : f s'' y z else f s'' y z
--                 where s'' = (insert (y', (i, j')) $
--                              insert (y' - i ^ 3 , (i + 1, j')) s')
--                       y' = y + j' ^ 3; j' = j + 1
--                       ((y, (i, j)), s') = deleteFindMin s

-- instance OEIS 265848 where
--   oeis = tablList @265848
-- instance Table 265848 where
--   tabl = zipWith (++) ([] : (tabf @14413)) (tabf @34868)

-- instance OEIS 265885 where
--   oeisIx n = n `bimpl` (oeisIx @40) n where
--      bimpl 0 0 = 0
--      bimpl p q = 2 * bimpl p' q' + if u <= v then 1 else 0
--                  where (p', u) = divMod p 2; (q', v) = divMod q 2

-- instance OEIS 265912 where
--   oeisIx = fromJust . (flip findIndex (tabl @7318)) . elem . (oeisIx @14631)

-- instance OEIS 269526 where
--   oeisIx n = head $ [1..] \\ map (oeisIx @269526) (oeisIx_row n)

-- instance OEIS 273191 where
--   oeis = (map length . group) $ map (oeisIx @273190) [0..]

-- instance OEIS 273620 where
--   oeisIxT :: Integral a => a -> a -> a
--   oeisIxT n k = floor $ sqrt k' * c where
--     (n', k') = (fi n, fi k)
--     c = fi $ floor $ n' / sqrt k' + 1

-- instance OEIS 273823 where
--   oeis = concatMap (rowT @273823) [1..]
--   oeisIx_tabf = map (rowT @273823) [1..]
--   oeisIx_row n
--     | a_i == 0  = []
--     | otherwise = a_i : (rowT @273823) a_i where
--       a_i = (oeisIx @271439) n

-- instance OEIS 273824 where
--   oeis = concatMap (rowT @273824) [1..]
--   oeisIx_tabf = map (rowT @273824) [1..]
--   oeisIx_row n
--     | a_i == 0  = []
--     | otherwise = a_i : (rowT @273824) a_i where
--       a_i = (oeisIx @271439) (n - 1)

-- instance OEIS 273825 where
--   oeis = concatMap (rowT @273825) [1..]
--   oeisIx_tabf = map (rowT @273825) [1..]
--   oeisIx_row n
--     | a_i == 0  = []
--     | otherwise = a_i : (rowT @273825) a_i where
--       a_i = (oeisIx @271439) $ (oeisIx @271439) (n - 1)

-- instance OEIS 274079 where
--   oeis = concatMap (rowT @274079) [1..]
--   oeisIx_tabf = map (rowT @274079) [1..]
--   oeisIx_row n = [n - 1, n-2..n - (oeisIx @2262) (n - 1)]

-- instance OEIS 274080 where
--   oeis = concatMap (rowT @274080) [1..]
--   oeisIx_tabf = map (rowT @274080) [1..]
--   oeisIx_row n = nub $ sort $ concatMap (\f -> f n) [oeisIx_row, (rowT @273825), (rowT @273824), (rowT @273823)]

-- instance OEIS 276127 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @1414) $ (oeisIx @64413) n

-- instance OEIS 276165 where
--   oeisIx = minimax . (rowT @66099)

-- instance OEIS 276166 where
--   minimaxDifference [] = 0
--   minimaxDifference as = max (head as - minimaxDifference (tail as)) (last as - minimaxDifference (init as))
--   minimaxScore as = (sum as + minimaxDifference as) `div` 2
--   oeisIx = minimaxScore . (rowT @66099)

-- instance OEIS 276167 where
--   minimaxDifference [] = 0
--   minimaxDifference as = max (head as - minimaxDifference (tail as)) (last as - minimaxDifference (init as))
--   minimaxScore2 as = (sum as - minimaxDifference as) `div` 2
--   oeisIx = minimaxScore2 . (rowT @66099)

-- instance OEIS 277278 where
--   oeisIx n
--     | isSquare n = n
--     | otherwise = last $ fromJust $ find (isSquare . sum) s
--     where
--       s = map ((n:) . map (n+)) (tabf @48793)
--       isSquare m = m == (integerRoot * integerRoot) where integerRoot = isqrtA m

-- instance OEIS 277516 where
--   oeisIx n = (oeisIx @277278) n - n

-- instance OEIS 279968 where
--   oeis = map count [1..] where
--     count n = genericLength $ filter (odd . (n+)) adjacentLabels where
--       adjacentLabels = map (oeisIx @279968) (oeisIx_row n)

-- instance OEIS 288208 where
--   pairs l = zip l (drop 1 l)
--   d n = filter (all (uncurry (/=)) . zip [1..]) $ Data.List.permutations [1..n]
--   a n = length $ filter (all ((1<) . abs . uncurry (-)) . pairs) $ d n

-- instance OEIS 290151 where
  -- oeisIx 1 = []
  -- oeisIx n | l<-f$n - 1 = l++[head [i |i<-[2..],gcd i n<2,all (/=i)l,abs (n-i)>1]]

-- instance OEIS 301851 where
--   oeis = tablList @301851
-- instance Table 301851 where
--   rowCol n k = length $ nub [i^2 + j^2 | i <- [0..n - 1], j <- [0..k-1]]

-- instance OEIS 306998 where
--   -- Very poor Haskell code, but let it stand until someone contributes the
--   -- used to produce the data given.
--   isPrime :: Int -> Bool
--   isPrime = isPrime1 2
--   isPrime1 :: Int -> Int -> Bool
--   isPrime1 d n = n /= 1 && (d^2 > n || mod n d /= 0 && isPrime1 (d+1) n)
--   count :: (a -> Bool) -> [a] -> Int
--   count f [] = 0
--   count f (x:xs) = (if f x then 1 else 0) + count f xs
--   pdf :: Int -> Double
--   pdf n = fi (count isPrime [1..n]) / fi n
--   isRecord :: Int -> Bool
--   isRecord n = (n == 2) || (pdf n) < (minimum (map pdf [2.. (n - 1)]))
--   records :: [Int]
--   records = filter isRecord [2..100]

-- instance OEIS 308267 where
--   import Data.Numbers.Primes
--   bintodec :: [Int] -> Int
--   bintodec = sum . zipWith (*) (iterate (*2) 1) . reverse
--   decomp :: (Integer, [Integer]) -> (Integer, [Integer])
--   decomp (x, ys) = if even x then (x `div` 2, 0:ys) else (x - 1, 1:ys)
--   zeck :: Integer -> String
--   zeck n = bintodec (1 : snd (last $ takeWhile (\ (x, ys) -> x > 0) $ iterate decomp (n, [])))
--   output :: [Integer]
--   output = filter (\x -> 0 == zeck x `mod` x) [1..100]

-- instance OEIS 308339 where
--   twinLowX [] = []
--   twinLowX [_] = []
--   twinLowX (n : (m : ns))
--       | m == n + 1 = 1 : (map succ (twinLowX (m : ns)))
--       | otherwise = (map succ (twinLowX (m : ns)))
--   oeisIx n = (twinLowX (oeis @7304)) !! (n - 1)

-- instance OEIS 308495 where
--   -- expected to be part of (oeisIx @27748)
--   oeis = concat (map (rowT @27748) [1..])
--   minIdx [] _ = []
--   minIdx _ [] = []
--   minIdx (a:as) (b:bs)
--       | a == b = 1 : (map succ (minIdx as bs))
--       | otherwise = map succ (minIdx as (b:bs))
--   oeis = minIdx (oeis @27748) (oeis @40)
--   oeisIx n = (oeis @308495) !! (n - 1)

-- instance OEIS 309096 where
--   wheelSeeds = [2, 3, 5, 7, 11, 13]
--   wheelOffsets = filter (\c -> all (\s -> mod c s /= 0) wheelSeeds) [1..product wheelSeeds]
--   restOfWheel = (concat (map (replicate (length wheelOffsets)) (map (* (product wheelSeeds)) [1..])))
--   wheel = wheelSeeds ++ (tail wheelOffsets) ++ (zipWith (+) (cycle wheelOffsets) restOfWheel)
--   isPrime n = and [n > 1, all (\c -> mod n c /= 0) (takeWhile (\c -> c * c <= n) wheel)]
--   primes = filter isPrime wheel
--   exponents bases acc n =
--       if (n == 1)
--           then (dropWhile (== 0) acc)
--           else if (mod n (head bases) == 0)
--               then (exponents bases (((head acc) + 1) : (tail acc)) (div n (head bases)))
--               else (exponents (tail bases) (0 : acc) n)
--   a = filter (\n -> all (\e -> elem e (takeWhile (<= e) a)) (exponents primes [0] n)) [1..]

-- instance OEIS 309415 where
--   oeisIx n = genericLength $ nub $ permutations $ show $ fact n
--     map (oeisIx @309415) [0..]

-- instance OEIS 309979 where
--   hash :: Double -> Inthash = read . sort . take 6 . filter (/='0') . drop 1 . dropWhile (/='.') . show . (** 0.03125)
--   main :: IO ()main = print $ map (floor . fst) . filter ((==234477) . snd) $ map (\x -> (x, hash x)) [2..1000000]

-- instance OEIS 316343 where
--   oeisIx_count :: Integer -> Int
--   oeisIx_count n = fi ((v + 1) ^ 2) where
--       v
--           | n `mod` 2 == 1 = 0
--           | otherwise      = 1 + (n `div` 2)
--   oeisIx_S :: Integer -> [Integer]
--   oeisIx_S n = 0 : (replicate (oeisIx_count n) 1)
--   oeisIx :: [Integer]
--   oeisIx = concatMap (oeisIx @316343)_S [1..]

-- instance OEIS 316532 where
--   divides :: Integer -> Integer -> Bool
--   divides a b = a `mod` b == 0
--   partitions :: [[Integer]]
--   partitions = concat $ map (partitions_of_n) [0..]
--   partitions_of_n :: Integer -> [[Integer]]
--   partitions_of_n n = partitions_at_most n n
--   partitions_at_most :: Integer -> Integer -> [[Integer]]
--   partitions_at_most _ 0 = [[]]
--   partitions_at_most 0 _ = []
--   partitions_at_most m n = concat $ map (\k -> map ([k] ++) (partitions_at_most k (n-k))) ( reverse [1.. (min m n)])
--   prime_signature :: [Integer] -> Integer
--   prime_signature p = product $ zipWith (^) primes p
--   seq :: [Integer]
--   seq = map prime_signature $ filter compare_first_second partitions
--       where
--     compare_first_second p
--           | length p == 0 = True
--           | length p == 1 = False
--           | otherwise = p!!0 == p!!1


