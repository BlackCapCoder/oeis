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


-- instance OEIS 16 where
--   oeisIx 0 = 1
--   oeisIx n =  (`div` (2 * n)) $ sum $
--    zipWith (*) (map (oeisIx @10) oddDivs) (map ((2 ^) . (div n)) $ oddDivs)
--     where oddDivs = rowT @182469 n

-- instance OEIS 86 where
--   oeisIx n = if n `mod` 9 == 0 then 0
--   else product $ map ((* 2) . (oeisIx @79978) . (+ 2)) $ rowT @27748 $ oeisIx @38502 n

-- instance OEIS 658 where
--   oeisIx (succ->n) = sum $ map c3 [0..n]
--     where
--       c3 k = (rowCol @7318    n     k)^2
--            * (rowCol @7318 (2*k)    k)^2
--            * (rowCol @7318 (2*k) (n-k))

-- instance OEIS 790 where
--   oeisIx n = head [c | c <- oeis @2808, powMod n c c == mod n c]

-- instance OEIS 914 where
--   oeis = scanl1 (+) $ oeis @6002

-- instance OEIS 989 where
--   oeisIx = (oeisIx @7949) . (oeisIx @984)

-- instance OEIS 1031 where
--   oeisIx n = sum (map (oeisIx @10051) gs) + fi (fromEnum (1 `elem` gs))
--      where gs = map (2 * n -) $ takeWhile (<= n) (oeis @8578)

-- instance OEIS 1037 where
--   oeisIx 0 = 1
--   oeisIx n = (sum $ map (\d -> ((oeisIx @79) d) * (oeisIx @8683) (n `div` d)) $
--                          (rowT @27750) n) `div` n

-- instance OEIS 1097 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @164292

-- instance OEIS 1102 where
--   oeis = filter (\x -> (oeisIx @10052) (x `div` (oeisIx @7953 x)) == 1) $ oeis @5349

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

-- instance OEIS 1479 where
--   oeisIx n = (oeisIx @196) $ head $
--      filter ((== 1) . (oeisIx @10052)) $ map ((oeisIx @7645) n -) $ tail (oeis @33428)

-- instance OEIS 1480 where
--   oeisIx n = (oeisIx @196) $ (`div` 3) $ ((oeisIx @7645) n) - ((oeisIx @1479) n) ^ 2

-- instance OEIS 1614 where
--   oeis = f 0 0 (oeis @57211) where
--      f c z (x:xs) = z' : f x z' xs where z' = z + 1 + 0 ^ abs (x - c)

-- instance OEIS 1650 where
--   (oeisIx @1650) n k = (tabf @1650) !! (n - 1) !! (k-1)
--  (rowT @1650) n = (tabf @1650) !! (n - 1)
--  (tabf @1650) = iterate (\xs@ (x:_) -> map (+ 2) (x:x:xs)) [1]
--  oeis = concat (tabf @1650)

-- instance OEIS 1692 where
--   oeisIx n = flip div n $ sum $
--               zipWith (*) (map (oeisIx @8683) divs) (map (oeisIx @351) $ reverse divs)
--               where divs = (rowT @27750) n

-- instance OEIS 1694 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @112526

-- instance OEIS 1768 where
--   oeisIx n = n * (z - 1) - (2 ^ (z + 2) - 3 * z) `div` 6
--      where z = (oeisIx @85423) $ n + 1

-- instance OEIS 1935 where
--   (oeisIx @1935) = p (oeis @42968) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 1970 where
--  oeis = 1 : f 1 [1] where
--      f x ys = y : f (x + 1) (y : ys) where
--               y = sum (zipWith (*) ys (oeis @61259)) `div` x

-- instance OEIS 2034 where
--   oeisIx 1 = 1
--   oeisIx n = fromJust ((oeisIx @92495) n `elemIndex` (oeis @142))

-- instance OEIS 2100 where
--   (oeisIx @2100) = p (oeis @6881) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m


-- instance OEIS 2123 where
--  oeis = 0 : 0 : f 3 where
--      f x = y : f (x + 1) where
--        y = (oeisIx @61397) x -
--            sum (map ((oeisIx @2123) . (x -)) $ takeWhile (< x) (oeis @65091))

-- instance OEIS 2294 where
--   oeis = [oeisIx @258708 (3 * n) (2 * n) | n <- [1..]]

-- instance OEIS 2322 where
--   oeisIx n = foldl lcm 1 $ map ((oeisIx @207193) . (oeisIx @95874)) $
--                             zipWith (^) ((rowT @27748) n) ((rowT @124010) n)

-- instance OEIS 2348 where
--   oeisIx n = product (zipWith d ps es) * 4 ^ e0 `div` 8 where
--      d p e = (p ^ 2 - 1) * p ^ e
--      e0 = if even n then head $ (rowT @124010) n else 0
--      es = map ((* 2) . subtract 1) $
--               if even n then tail $ (rowT @124010) n else (rowT @124010) n
--      ps = if even n then tail $ (rowT @27748) n else (rowT @27748) n

-- instance OEIS 2373 where
--   oeisIx n = head $ dropWhile ((== 0) . (oeisIx @10051) . (2*n -)) (oeis @65091)

-- instance OEIS 2457 where
--   oeisIx n = (oeisIx @116666) (2 * n + 1) (n + 1)

-- instance OEIS 2503 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @65350)

-- instance OEIS 2516 where
--  oeis = 0 : concat (transpose
--   [a004767_list, f (oeis @2516), (oeis @17089), g $ drop 2 (oeis @2516)])
--   where f [z] = []; f (_:z:zs) = 2 * z : f zs

-- instance OEIS 2616 where
--   oeisIx = flip div 2 . oeisIx @2322

-- instance OEIS 2618 where
--   oeisIx n = oeisIx @10 n * n

-- instance OEIS 2645 where
--   oeis = 2 : (map (oeisIx @40) $ filter ((> 1) . (oeisIx @256852)) [1..])

-- instance OEIS 2654 where
--   oeisIx n = product $ zipWith f ((rowT @27748) m) ((rowT @124010) m) where
--      f p e | p `mod` 4 == 1 = e + 1
--            | otherwise      = (e + 1) `mod` 2
--      m = (oeisIx @265) n

-- instance OEIS 2690 where
--   oeisIx n = (oeisIx @245334) (2 * n) n

-- instance OEIS 2694 where
--   oeisIx (fi->n) = (oeisIx @7318)' (2 * n) (n - 2)

-- instance OEIS 2733 where
--   oeisIx = oeisIx @196 . (subtract 1) . (* 10) . (oeisIx @207337)

-- instance OEIS 2964 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (map (oeisIx @2963 . pred) [1..3888]))

-- instance OEIS 2996 where
--   oeisIx n = sum $ zipWith (*) (map (oeisIx @8683) divs) (map (oeisIx @108) $ reverse divs)
--      where divs = (rowT @27750) n

-- instance OEIS 2997 where
--   oeis = [x | x <- (oeis @24556),
--       all (== 0) $ map ((mod (x - 1)) . (subtract 1)) $ (rowT @27748) x]

-- instance OEIS 3072 where
--   oeis = filter c3 [1..] where
--      c3 x = any (== 1) $ map (oeisIx @10057) $
--                          takeWhile (> 0) $ map (x -) $ (oeis @3325)

-- instance OEIS 3116 where
--   oeisIx n = (oeisIx @168396) (2 * n + 1) n

-- instance OEIS 3169 where
--   oeisIx = flip (oeisIx @100326) 0

-- instance OEIS 3242 where
--   oeis = 1 : f [1] where
--      f xs = y : f (y : xs) where
--             y = sum $ zipWith (*) xs (oeis @48272)

-- instance OEIS 3256 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @242094)) . (oeisIx @1950)

-- instance OEIS 3271 where
  -- oeis = map ((+ 1) . fromJust . (`elemIndex` (oeis @49865))) [0..]

-- instance OEIS 3277 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @9195)

-- instance OEIS 3601 where
  -- oeis = map succ $ elemIndices 1 $ tail $ oeis @245656

-- instance OEIS 3624 where
--   oeis = filter ((== 1) . (oeisIx @9194)) (oeis @2808)

-- instance OEIS 3990 where
--   oeisIx x y = (oeisIx @3990)_adiag x !! (y-1)
--   oeisIx_adiag n = (tabl @3990) !! (n - 1)
--   oeisIx_tabl = zipWith (zipWith lcm) (tabl @2260) $ map reverse (tabl @2260)

-- instance OEIS 4302 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @103371) (n + 1) 2

-- instance OEIS 4431 where
--   oeis = findIndices (> 1) (oeis @63725)

-- instance OEIS 4613 where
--   oeis = filter (all (== 1) . map (oeisIx @79260) . (rowT @27748)) [1..]

-- instance OEIS 4614 where
--   oeis = filter (all (== 1) . map (oeisIx @79261) . (rowT @27748)) [1..]

-- instance OEIS 4615 where
--   oeis = filter (all (== 1) . (map (`mod` 5) . (rowT @27748))) [1..]

-- instance OEIS 4780 where
--   oeis = filter ((> 1) . (oeisIx @48728)) [1..]

-- instance OEIS 4957 where
--   oeis = findIndices even (oeis @60142)

-- instance OEIS 5276 where
--   oeis = filter p [1..] where
--      p z = p' z [0, z] where
--        p' x ts = if y `notElem` ts then p' y (y:ts) else y == z
--                  where y = (oeisIx @48050) x

-- instance OEIS 5279 where
--   oeis = filter ((> 0) . (oeisIx @174903)) [1..]
-- instance OEIS 5341 where
--   oeisIx = genericLength . (rowT @34002)

-- instance OEIS 5349 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @70635

-- instance OEIS 5378 where
--   oeis = 1 : zipWith (-) [1..] (map (oeisIx @5379) (oeis @5378))

-- instance OEIS 5413 where
--   oeis = 1 : zipWith (*) [1 ..] (zipWith (+) (tail (oeis @5412)) (zipWith (*) [4, 6 ..] (oeis @5413)))

-- instance OEIS 5773 where
--   oeis = 1 : f (oeis @1006) [] where
--      f (x:xs) ys = y : f xs (y : ys) where
--        y = x + sum (zipWith (*) (oeis @1006) ys)

-- instance OEIS 5775 where
--   oeisIx = flip (rowCol @38622) 2 . (subtract 1)

-- instance OEIS 5835 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @210455

-- instance OEIS 5846 where
--   oeis = filter ((== 1) . (oeisIx @10051)) (oeis @202018)

-- instance OEIS 6036 where
--   oeis = filter (all (== 0) . map (oeisIx @210455) . (rowT @27751)) (oeis @5835)

-- instance OEIS 6037 where
--   oeis = filter ((== 0) . (oeisIx @210455)) (oeis @5101)

-- instance OEIS 6086 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @103340

-- instance OEIS 6285 where
--   oeis = filter ((== 0) . (oeisIx @109925)) [1, 3 ..]

-- instance OEIS 6338 where
--   oeis = tail (oeis @214848)

-- instance OEIS 6356 where
--   oeis = 1 : 3 : 6 : zipWith (+) (map (2 *) $ drop 2 (oeis @6056))
--      (zipWith (-) (tail (oeis @6056)) (oeis @6056))

-- instance OEIS 6378 where
--   oeis = map (oeisIx @40) $ (map succ $ elemIndices 0 $ tail $ oeis @107740)

-- instance OEIS 6509 where
--   oeis = 1 : f [1] (oeis @40) where
--      f xs'@(x:_) (p:ps)
--        | x' > 0 && x' `notElem` xs = x' : f (x':xs) ps
--        | x'' `notElem` xs          = x'' : f (x'':xs) ps
--        | otherwise                 = 0 : f (0:xs) ps
--       where x' = x - p; x'' = x + p

-- instance OEIS 6532 where
--   oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @203)) [1..]

-- instance OEIS 6549 where
--   oeis = [1,2,3,4,7,8] ++ f (drop 4 (oeis @40)) where
--      f (p:ps) | (oeisIx @10055) (p - 1) == 1 = (p - 1) : f ps
--               | (oeisIx @10055) (p + 1) == 1 = p : f ps
--               | otherwise            = f ps

-- instance OEIS 6577 where
--   oeisIx n = fromJust $ findIndex (n `elem`) (tabf @127824)

-- instance OEIS 6580 where
--   oeis = map sum (tabl @3990)

-- instance OEIS 6601 where
--   oeis = map (+ 1) $ elemIndices 0 $
--      zipWith3 (((+) .) . (+)) ds (tail ds) (drop 2 ds) where
--      ds = map abs $ zipWith (-) (tail (oeis @5)) (oeis @5)

-- instance OEIS 6696 where
--   oeis = 0 : f 1 [0] where
--      f u vs = w : f (u + 1) (w : vs) where
--        w = minimum $ zipWith (+)
--            (reverse vs) (zipWith (*) (tail (oeis @79)) (map (+ u) vs))

-- instance OEIS 6751 where
--   oeisIx = foldl1 (\v d -> 10 * v + d) . (rowT @88203)

-- instance OEIS 6872 where
--   oeis = filter (\x -> (oeisIx @10)' x == (oeisIx @10)' (oeisIx' x)) [1..]

-- instance OEIS 6992 where
--   oeis = iterate (oeisIx @7917 . (* 2)) 2

-- instance OEIS 7015 where
--   oeisIx n = 1 + (fromJust $
--               elemIndex 0 $ zipWith (-) (oeis @10) $ drop n (oeis @10))

-- instance OEIS 7340 where
--   oeis = filter ((== 0) . (oeisIx @54025)) (oeis @1599)

-- instance OEIS 7367 where
--   oeis = map fst $ filter ((== 3) . snd) $ zip (oeis @2202) (oeis @58277)

-- instance OEIS 7401 where
--   oeis = [x | x <- [0..], (oeisIx @23531) x == 0]

-- instance OEIS 7412 where
--   oeisIx n = n + (oeisIx @48766) (n + (oeisIx @48766) n)

-- instance OEIS 7428 where
--   oeisIx n = product
--      [oeisIx' 3 e * cycle [1,-1] !! fi e | e <- (rowT @124010) n]

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

-- instance OEIS 7534 where
--   oeis = f [2,4..] S.empty 1 (oeis @1097) where
--      f xs'@ (x:xs) s m ps'@ (p:ps)
--        | x > m = f xs' (S.insert p s) p ps
--        | S.null (s `S.intersection` S.map (x -) s) = x : f xs s m ps'
--        | otherwise = f xs s m ps'
-- instance OEIS 7542 where
--   oeis = iterate (oeisIx @203907) 2

-- instance OEIS 7547 where
--   oeis = tail $ elemIndices 2 $ map (oeisIx @6530) (oeis @7542)

-- instance OEIS 7554 where
--   oeis = 1 : f 1 where
--      f x = (sum $ zipWith (*) (map (oeisIx @8683) divs)
--                               (map (oeisIx @7554) $ reverse divs)) : f (x + 1)
--             where divs = (rowT @27750) x

-- instance OEIS 7620 where
--   oeis = 1 : filter (\x -> all (p $ (rowT @27751) x) [1..x]) [2..]
--      where p _  0 = True
--            p [] _ = False
--            p ds'@ (d:ds) m = d <= m && (p ds (m - d) || p ds m)

-- instance OEIS 7691 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @17666

-- instance OEIS 7755 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @3434)) . (subtract 1)

-- instance OEIS 7811 where
--   oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051 . pred)) $
--      iterate (zipWith (+) [10, 10, 10, 10]) [1, 3, 7, 9]

-- instance OEIS 7957 where
--   oeis = findIndices (> 0) (oeis @196564)

-- instance OEIS 8287 where
--   oeis = concat $ iterate ([1,1,1,1] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 8365 where
--   oeis = 1 : filter ((> 11) . (oeisIx @20639)) [1..]

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

-- instance OEIS 9023 where
--   oeis = filter ((> 1) . (oeisIx @227481)) [1..]

-- instance OEIS 9087 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

-- instance OEIS 10096 where
--   oeisIx = genericLength . takeWhile (/= 0) . iterate (oeisIx @523)

-- instance OEIS 10120 where
--   oeisIx = (oeisIx @70167) . (oeisIx @79)

-- instance OEIS 10554 where
--   oeisIx = (oeisIx @10) . (oeisIx @10)

-- instance OEIS 10683 where
--   oeisIx = sum . (rowT @144944)

-- instance OEIS 11379 where
--   oeisIx n = (oeisIx @290) n + (oeisIx @578) n

-- instance OEIS 11539 where
--   oeis = filter ((> 0) . (oeisIx @102683)) [1..]

-- instance OEIS 11754 where
--   oeisIx = (oeisIx @120) . (oeisIx @244)

-- instance OEIS 11756 where
--   oeis = map (oeisIx @40) $ tail (oeis @217)

-- instance OEIS 11784 where
--   oeisIx = last . (rowT @12257)

-- instance OEIS 13609 where
--   oeis = concat $ iterate ([1,2] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 13638 where
--   oeisIx n = (oeisIx @151799) n * (oeisIx @151800) n

-- instance OEIS 14313 where
--   oeisIx = f . (oeisIx @38447) where
--      f x = if x == 0 then 0 else 2 * f x' + b  where (x', b) = divMod x 10

-- instance OEIS 14320 where
--   oeis = nub $ (oeis @1223)

-- instance OEIS 14342 where
--   oeis= f (tail (oeis @40)) [head (oeis @40)] 1 where
--      f (p:ps) qs k = sum (zipWith (*) qs $ reverse qs) :
--                      f ps (p : qs) (k + 1)

-- instance OEIS 14417 where
--   oeisIx 0 = 0
--   oeisIx n = foldl (\v z -> v * 10 + z) 0 $ (rowT @189920) n

-- instance OEIS 14454 where
--   oeisIx n = sum $ zipWith gcd kfs $ map (div nf) kfs
--      where (nf:kfs) = reverse $ (rowT @166350) n

-- instance OEIS 14481 where
--   oeisIx n = (oeisIx @9445) n `div` (oeisIx @1147) n

-- instance OEIS 14567 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @9194

-- instance OEIS 14597 where
--   oeis = tail $ elemIndices 1 $ map (oeisIx @197183) [0..]

-- instance OEIS 14657 where
--   oeis = map (+ 1) $ findIndices (> 0) $ map (oeisIx @195470) [1..]

-- instance OEIS 14661 where
--   oeis = 2 : map (+ 1) (elemIndices 0 $ map (oeisIx @195470) [1..])

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

-- instance OEIS 15976 where
--   oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @56964)) [1..]

-- instance OEIS 16035 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ map (oeisIx @10) $ init $ tail $ (rowT @27750) n

-- instance OEIS 16726 where
--   oeis = [1,2,6,9] ++ (f 5 $ drop 4 (oeis @1751)) where
--      f n qs'@ (q:qs) | q < 2*n   = f n qs
--                     | otherwise = q : f (n+1) qs'

-- instance OEIS 18194 where
--   oeisIx n = 1 + length (takeWhile (/= 0) $ zipWith (-) ks $ tail ks)
--      where ks = iterate (oeisIx @2034) n

-- instance OEIS 18819 where
--   oeis = 1 : f (tail (oeis @8619)) where
--      f (x:xs) = (sum $ take x (oeis @18819)) : f xs

-- instance OEIS 18825 where
--   oeis = tail $ elemIndices 0 (oeis @25426)

-- instance OEIS 19268 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @19269))

-- instance OEIS 19302 where
--   oeisIx = sum . zipWith (*) (oeis @10060) . rowT @7318

-- instance OEIS 20330 where
--   oeisIx n = foldr (\d v -> 2 * v + d) 0 (bs ++ bs) where
--      bs = (rowT @30308) n

-- instance OEIS 20475 where
--   oeis = 0 : map (sum . map (0 ^)) (tail (tabl @53200))

-- instance OEIS 20482 where
--   oeisIx = last . (rowT @171637)

-- instance OEIS 20486 where
--   oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @5) x == 0) [1..]

-- instance OEIS 20487 where
--   oeis = filter (\x -> (oeisIx @1157) x `mod` (oeisIx @203) x == 0) [1..]

-- instance OEIS 20653 where
--   oeis = concat $ map reverse $ tail (tabf @38566)

-- instance OEIS 22482 where
--   oeis = iterate (oeisIx @45918 . oeisIx @4086) 2

-- instance OEIS 22506 where
--   oeis = 0 : 10 : iterate (oeisIx @45918 . (oeisIx @4086)) 1011

-- instance OEIS 22507 where
--   oeis = iterate (oeisIx @45918 . oeisIx @4086) 3

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

-- instance OEIS 23888 where
--   oeisIx = sum . (rowT @210208)

-- instance OEIS 23896 where
--   oeisIx = sum . (rowT @38566)

-- instance OEIS 23900 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (1 -) $ (rowT @27748) n

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
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) $ tail (oeis @56911)

-- instance OEIS 24619 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @10055

-- instance OEIS 24620 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @25474

-- instance OEIS 24816 where
--   oeisIx = sum . (rowT @173541)

-- instance OEIS 24894 where
--   oeisIx = flip div 5 . subtract 1 . (oeisIx @30430)

-- instance OEIS 24939 where
--   oeisIx = p (oeis @65091) where
--      p _  0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 25565 where
--   oeis = 1 : f (oeis @1006) [1] where
--      f (x:xs) ys = y : f xs (y : ys) where
--        y = x + sum (zipWith (*) (oeis @1006) ys)

-- instance OEIS 26233 where
--   oeisIx n = (oeisIx @49084) n + (oeisIx @239968) n

-- instance OEIS 26238 where
--   oeisIx n = (oeisIx @49084) n + (oeisIx @66246) n

-- instance OEIS 26239 where
--   oeisIx 1 = 1
--   oeisIx n | (oeisIx @10051) n == 1 = (oeisIx @2808) $ (oeisIx @49084) n
--             | otherwise      = (oeisIx @40) $ (oeisIx @66246) n

-- instance OEIS 26274 where
--   oeis = map (subtract 1) $ tail $ (map succ $ elemIndices 1 $ tail $ oeis @35612)

-- instance OEIS 26351 where
--   oeis = findIndices odd (oeis @60142)

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

-- instance OEIS 27306 where
--   oeisIx n = (oeisIx @8949) n (n `div` 2)

-- instance OEIS 27383 where
--   oeis = concat $ transpose [oeis, drop 2 (oeis @918)]

-- instance OEIS 27642 where
--   oeis = 1 : map (denominator . sum) (zipWith (zipWith (%))
--      (zipWith (map . (*)) (tail (oeis @142)) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 27810 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 5) 5

-- instance OEIS 27818 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 6) 6

-- instance OEIS 27837 where
--   oeis = f 1 [] where
--      f x ys = y : f (x + 1) (y : ys) where
--               y = (oeisIx @1044) x * x - sum (zipWith (*) ys $ tail (oeis @1044))

-- instance OEIS 27868 where
--   oeisIx n = sum $ takeWhile (> 0) $ map (n `div`) $ tail (oeis @351)

-- instance OEIS 27914 where
--   oeisIx n = sum $ take (n + 1) $ (rowT @27907) n

-- instance OEIS 27988 where
--   oeisIx = maximum . (rowT @27926)

-- instance OEIS 28242 where
--   oeisIx n = n' + 1 - m where (n',m) = divMod n 2
--   oeis = concat $ transpose [oeis, (oeis @1477)]

-- instance OEIS 28916 where
--   oeis = map (oeisIx @40) $ filter ((> 0) . (oeisIx @256852)) [1..]

-- instance OEIS 29730 where
--   oeis = map (foldr (\h v -> 16 * v + h) 0) $
--                      filter (\xs -> xs == reverse xs) (tabf @262437)

-- instance OEIS 29783 where
--   oeis = filter (\x -> (oeisIx @258682) x == x ^ 2) [1..]

-- instance OEIS 29907 where
--   oeis = 0 : 1 : zipWith (+) (tail (oeis @45))
--                         (zipWith (+) (tail (oeis @29907)) (oeis @29907))

-- instance OEIS 30147 where
--   oeis = filter ((== 1) . (oeisIx @228710)) (oeis @2113)

-- instance OEIS 30152 where
--   oeis = filter ((== 1) . (oeisIx @228710)) (oeis @290)

-- instance OEIS 30293 where
--   oeis = filter ((<= 2) . (oeisIx @43537)) (oeis @578)

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

-- instance OEIS 30461 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @45533)

-- instance OEIS 30658 where
--   oeisIx = (fi.fromEnum) . (<= 0) . (oeisIx @95916)

-- instance OEIS 30664 where
--   oeisIx n = (oeisIx @7917) n * (oeisIx @7918) n

-- instance OEIS 31076 where
--   oeis = concat $ map reverse $ tail (tabf @31087)

-- instance OEIS 31359 where
--   oeisIx = (oeisIx @1615) . (subtract 1) . (* 2)

-- instance OEIS 31444 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @37861

-- instance OEIS 31448 where
--   oeis = filter ((== -1) . (oeisIx @37861)) [1..]

-- instance OEIS 31877 where
--   oeis = [x | x <- [1..], x `mod` 10 > 0,
--                       let x' = (oeisIx @4086) x, x' /= x && x `mod` x' == 0]

-- instance OEIS 31971 where
--   oeisIx = sum . (rowT @89072)

-- instance OEIS 32358 where
--   oeisIx = genericLength . takeWhile (/= 2) . (iterate (oeisIx @10))

-- instance OEIS 32447 where
--   oeis = f [1..] (oeis @2110) [] where
--      f xs'@ (x:xs) ps'@ (p:ps) us
--        | x < p = f xs ps' $ O.insertBag (oeisIx @10 x, x) us
--        | otherwise = map snd vs ++ f xs' ps ws
--        where (vs, ws) = span ((<= (oeisIx @10) x) . fst) us

-- instance OEIS 32448 where
--   oeisIx n = head [q | q <- (oeis @40), let p = (oeisIx @40) n,
--                         q `mod` p == p - 1]

-- instance OEIS 32741 where
--   oeisIx n = if n == 0 then 0 else (oeisIx @5) n - 1

-- instance OEIS 32810 where
--   oeisIx = f 0 . (+ 1) where
--      f y 1 = (oeisIx @4086) y
--      f y x = f (10 * y + m + 2) x' where (x', m) = divMod x 2

-- instance OEIS 33180 where
--   oeis = filter ((> 0) . (oeisIx @67109)) [1..]

-- instance OEIS 33294 where
--   oeis = filter chi (oeis @290) where
--     chi m = m `mod` 10 > 0 && head ds `elem` [1,4,5,6,9] &&
--             (oeisIx @10052) (foldl (\v d -> 10 * v + d) 0 ds) == 1 where
--       ds = unfoldr
--            (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 10) m

-- instance OEIS 33491 where
--   oeisIx = head . (rowT @127824)

-- instance OEIS 33548 where
--   oeis = filter ((== 0) . (oeisIx @90431) . (oeisIx @49084)) (oeis @40)

-- instance OEIS 33549 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @90431

-- instance OEIS 33556 where
--   oeis = iterate (\x -> 2*x - (oeisIx @151799 x)) 3


-- instance OEIS 33620 where
--   oeis = filter chi [1..] where
--      chi n = (oeisIx @136522) spf == 1 && (n' == 1 || chi n') where
--         n' = n `div` spf
--         spf = (oeisIx @20639) n

-- instance OEIS 33632 where
--   oeis = filter (\x -> (oeisIx @62401) x == (oeisIx @62402) x) [1..]

-- instance OEIS 33651 where
  -- oeisIx n = (oeis @63051) !! n
  -- oeis = iterate (oeisIx @56964) 879

-- instance OEIS 33683 where
--   oeisIx n = fromEnum $ odd n && mod n 3 > 0 && (oeisIx @10052) n == 1

-- instance OEIS 33815 where
--   oeisIx n = (oeisIx @116854) (2 * n + 1) (n + 1)

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

-- instance OEIS 34262 where
--   oeisIx n = (oeisIx @578) n + n

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

-- instance OEIS 34699 where
--   oeisIx = last . (rowT @210208)

-- instance OEIS 34708 where
--   oeis = filter ((== 1) . (oeisIx @168046)) (oeis @214957)

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

-- instance OEIS 34883 where
--   oeisIx = maximum . (rowT @51010)

-- instance OEIS 34886 where
--   oeisIx = (oeisIx @55642) . (oeisIx @142)

-- instance OEIS 34891 where
--   oeisIx = genericLength . (rowT @212721)

-- instance OEIS 35103 where
--   oeisIx = (oeisIx @23416) . (oeisIx @40)

-- instance OEIS 35116 where
--   oeisIx = (^ 2) . (oeisIx @5)'

-- instance OEIS 35191 where
--   oeisIx n = (oeisIx @1817) n + (oeisIx @1822) n

-- instance OEIS 35526 where
--   oeisIx = (oeisIx @7088) . (oeisIx @35522)

-- instance OEIS 35612 where
--   oeisIx = (oeisIx @7814) . (oeisIx @22340)

-- instance OEIS 35614 where
--   oeisIx = (oeisIx @122840) . (oeisIx @14417) . (+ 1)

-- instance OEIS 35928 where
--   oeis = filter (\x -> (oeisIx @36044) x == x) [0,2..]

-- instance OEIS 35959 where
--   oeisIx = p (oeis @47201) where
--      p _      0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 36262 where
--   oeis = tablList @36262
-- instance Table 36262 where
--   rowCol n k = delta !! (n - k) !! (k - 1)
--     where
--       delta = iterate
--         (\pds -> zipWith (\x y -> abs (x - y)) (tail pds) pds) (oeis @40)

-- instance OEIS 36391 where
--   oeisIx = sum . (rowT @139366)

-- instance OEIS 36449 where
--   oeis = map fst listsOfValsAndDiffs
--   oeisIx n = (oeis @189475) !! (n - 1)
--   oeis = tail $ map snd listsOfValsAndDiffs
--   listsOfValsAndDiffs = (0,1) : f (0,1) where
--      f (x,y) = (u,v) : f (u,v) where
--        u = x + v
--        v = head $ dropWhile ((== 0) . (oeisIx @10052) . (+ x)) $ tail (oeis @217)

-- instance OEIS 36454 where
--   oeis = filter ((== 1) . (oeisIx @10051) . (+ 1) . (oeisIx @100995)) (oeis @961)

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

-- instance OEIS 36667 where
--   oeis = filter (even . flip mod 2 . (oeisIx @1222)) (oeis @3586)

-- instance OEIS 36692 where
--   oeisIx n = (oeisIx @36355) (2 * n) n

-- instance OEIS 36763 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @51521

-- instance OEIS 36786 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x < (oeisIx @55642) x]

-- instance OEIS 36787 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x == (oeisIx @55642) x]

-- instance OEIS 36788 where
--   oeis = [x | x <- [1..], (oeisIx @6968) x <= (oeisIx @55642) x]

-- instance OEIS 36844 where
--   oeis = filter ((== 0). (oeisIx @238525)) [2..]

-- instance OEIS 36998 where
--   oeisIx n = p (rowT @38566 n) n where
--      p _      0 = 1
--      p []     _ = 0
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 37019 where
--   oeisIx = product .
--      zipWith (^) (oeis @40) . reverse . map (subtract 1) . (rowT @27746)

-- instance OEIS 37264 where
--   oeis = filter ((== 1) . (oeisIx @168046)) $
--                         takeWhile (<= 999999999) (oeis @214958)

-- instance OEIS 37268 where
--   oeis = filter ((== 1) . (oeisIx @168046)) $
--                         takeWhile (<= 999999999) (oeis @214959)

-- instance OEIS 37271 where
--   oeisIx = genericLength . takeWhile ((== 0) . (oeisIx @10051)'') .
--                                iterate (oeisIx @37276) . (oeisIx @2808)

-- instance OEIS 37444 where
--   oeisIx n = p (map (^ 2) [1..]) (n^2) where
--      p _      0 = 1
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

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

-- instance OEIS 38163 where
--   oeis = map
--       (sum . zipWith (*) (intersperse 0 $ tail (oeis @217)) . reverse) $
--       tail $ inits $ tail (oeis @217) where

-- instance OEIS 38186 where
--   oeis = map succ $ elemIndices 1
--                  $ zipWith (*) (map (oeisIx @188641) [1..]) (map (oeisIx @188642) [1..])

-- instance OEIS 38207 where
--   oeis = concat $ iterate ([2,1] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 38221 where
--   oeis = concat $ iterate ([3,3] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 38444 where
--   oeis = 11 : f [11] 90 where
--      f xs@ (x:_) z = ys ++ f ys (10 * z) where
--                     ys = (x + z) : map (* 10) xs

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

-- instance OEIS 38549 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @38548))

-- instance OEIS 38550 where
--   oeis = map succ $ elemIndices 2 $ tail $ oeis @1227

-- instance OEIS 38555 where
--   oeisIx n = foldr (\d v -> v * 3 + d) 0 $
--      zipWith (\x y -> (x + y) `mod` 3) ts $ tail ts
--      where ts = (rowT @30341) n

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

-- instance OEIS 38575 where
--   oeisIx n = if n == 0 then 0 else (oeisIx @1222) $ (oeisIx @45) n

-- instance OEIS 38610 where
--   oeisIx = foldl lcm 1 . (rowT @38566)

-- instance OEIS 38618 where
--   oeis = filter ((== 1) . (oeisIx @168046)) (oeis @40)

-- instance OEIS 38670 where
--   oeis = elemIndices 2 $ map (oeisIx @193095) [0..]

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

-- instance OEIS 39686 where
--   oeis = filter ((== 1) . (oeisIx @10052)) (oeis @191933)

-- instance OEIS 39701 where
--   oeisIx = (`mod` 3) . (oeisIx @40)
--   oeis = map (`mod` 3) (oeis @40)

-- instance OEIS 39723 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @39723) n' * 10 + m where
--      (n',m) = if r < 0 then (q + 1, r + 10) else qr where
--               qr@ (q, r) = quotRem n (negate 10)

-- instance OEIS 39833 where
--   oeis = f (oeis @6881) where
--      f (u : vs@ (v : w : xs))
--        | v == u+1 && w == v+1 = u : f vs
--        | otherwise            = f vs

-- instance OEIS 40027 where
--   oeisIx n = head $ (rowT @46936) (n + 1)

-- instance OEIS 40040 where
--   oeisIx = flip div 2 . (oeisIx @14574)

-- instance OEIS 41013 where
--   oeis = 1 : f 1 where
--      f x | rev <= x  = (2*x) : f (2*x)
--          | otherwise = rev : f rev where rev = (oeisIx @4086) x

-- instance OEIS 42939 where
--   oeisIx = (oeisIx @40997) . (oeisIx @40)

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

-- instance OEIS 45572 where
--   oeis = filter ((/= 0) . (`mod` 5)) (oeis @5408)

-- instance OEIS 45797 where
--   oeis = filter (even . (`mod` 10) . (`div` 10)) (oeis @45572)

-- instance OEIS 45798 where
--   oeis = filter (odd . (`mod` 10) . (`div` 10)) (oeis @45572)

-- instance OEIS 45910 where
--   oeis =  [x | x <- takeWhile (<= 999999999) $ (oeis @9994),
--                        oeisIx x == 1]

-- instance OEIS 45920 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @76191)

-- instance OEIS 45967 where
--   oeisIx 1 = 4
--   oeisIx n = product $ zipWith (^)
--               (map (oeisIx @151800) $ (rowT @27748) n) (map (+ 1) $ (rowT @124010) n)

-- instance OEIS 45985 where
--   oeisIx n = head [k | (k, x) <- zip [1..] (oeis @7504),
--                         let (y, r) = divMod x n, r == 0, (oeisIx @10051)' y == 1]

-- instance OEIS 46092 where
--   oeisIx = (* 2) . (oeisIx @2378)

-- instance OEIS 46099 where
--   oeis = filter ((== 1) . (oeisIx @212793.pred)) [1..]

-- instance OEIS 46100 where
--   oeis = filter ((< 4) . (oeisIx @51903)) [1..]

-- instance OEIS 46101 where
--   oeis = filter ((> 3) . (oeisIx @51903)) [1..]

-- instance OEIS 46315 where
--   oeis = filter odd (oeis @1358)

-- instance OEIS 46316 where
--   oeis = filter ((== 3) . (oeisIx @1222)) [1, 3 ..]

-- instance OEIS 46388 where
--   oeis = filter ((== 2) . (oeisIx @1221 . pred)) (oeis @56911)

-- instance OEIS 46530 where
--   oeisIx n = genericLength $ nub $ map (`mod` n) $
--                              take (fromInteger n) $ tail (oeis @578)

-- instance OEIS 46642 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @9191)

-- instance OEIS 46665 where
--   oeisIx n = (oeisIx @6530) n - (oeisIx @20639) n

-- instance OEIS 46711 where
--   oeis = [x | x <- (oeis @42963), (oeisIx @161) x > 0]

-- instance OEIS 46712 where
--   oeis = filter ((`elem` [1,2]) . (`mod` 4)) (oeis @22544)

-- instance OEIS 46727 where
--   oeis = 0 : f (tail (oeis @1652)) (tail (oeis @46090)) where
--      f (x:_:xs) (_:y:ys) = x : y : f xs ys

-- instance OEIS 46758 where
--   oeis = filter (\n -> (oeisIx @50252) n == (oeisIx @55642) n) [1..]

-- instance OEIS 46759 where
--   oeis = filter (\n -> (oeisIx @50252) n < (oeisIx @55642) n) [1..]

-- instance OEIS 46760 where
--   oeis = filter (\n -> (oeisIx @50252) n > (oeisIx @55642) n) [1..]

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

-- instance OEIS 46923 where
--   oeisIx = (oeisIx @46922) . (oeisIx @5408)

-- instance OEIS 46930 where
--   oeisIx 1 = 1
--   oeisIx n = subtract 2 $ (oeisIx @31131) n

-- instance OEIS 46970 where
--   oeisIx = product . map ((1 -) . (^ 2)) . (rowT @27748)

-- instance OEIS 47160 where
--   oeisIx n = if null ms then -1 else head ms
--               where ms = [m | m <- [0 .. n - 1],
--                               (oeisIx @10051)' (n - m) == 1, (oeisIx @10051)' (n + m) == 1]

-- instance OEIS 47813 where
--   oeisIx = last . (rowT @262188)

-- instance OEIS 47846 where
--   oeis = 1 : zipWith (-) (tail (oeis @196277)) (oeis @196277)

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

-- instance OEIS 48050 where
--   oeisIx 1 = 0
--   oeisIx n = (subtract 1) $ sum $ (rowT @27751) n

-- instance OEIS 48055 where
--   oeis = [x | x <- (oeis @2808)
--          , let (us,vs) = Data.List.partition
--                           ((== 1) . (oeisIx @10051.pred)) $ (rowT @27751) x
--          , sum us + x == sum vs]

-- instance OEIS 48098 where
--   oeis = [x | x <- [1..], (oeisIx @6530) x ^ 2 <= x]

-- instance OEIS 48153 where
--   oeisIx = sum . (rowT @48152)

-- instance OEIS 48250 where
--   oeisIx = sum . (rowT @206778)

-- instance OEIS 48272 where
--   oeisIx n = (oeisIx @1227) n - (oeisIx @183063) n

-- instance OEIS 48298 where
--   oeisIx n = (oeisIx @209229) n * n

-- instance OEIS 48344 where
--   oeis = filter f (oeis @29742) where
--      f x = (oeisIx @136522) (x * (oeisIx @4086) x) == 1

-- instance OEIS 48395 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @199771) (2 * n)

-- instance OEIS 48398 where
--   oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @33075)

-- instance OEIS 48411 where
--   oeis = filter ((== 1) . (oeisIx @10052)) (oeis @33075)

-- instance OEIS 48519 where
--   oeis = map (oeisIx @40) $ filter ((== 1) . (oeisIx @10051.pred) . (oeisIx @65073)) [1..]

-- instance OEIS 48521 where
--   oeis = map (oeisIx @40) $ filter ((> 0) . (oeisIx @107740)) [1..]

-- instance OEIS 48645 where
--   oeis = tablList @48645
--   rowCol = rowCol_off @48645 @1 @1
--   rowT   = rowT_off   @48645 @1
--   tabl = iterate (\xs -> insert (2 * head xs + 1) $ map ((* 2)) xs) [1]
--   oeis = concat (tabl @48645)

-- instance OEIS 48646 where
--   oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @48653)

-- instance OEIS 48669 where
--   oeisIx n = maximum $ zipWith (-) (tail ts) ts where
--      ts = (rowT @38566) n ++ [n + 1]

-- instance OEIS 48691 where
--   oeisIx = product . map (oeisIx @5408 . fi) . (rowT @124010)

-- instance OEIS 48701 where
--   oeisIx n = foldr (\d v -> 2 * v + d) 0 (reverse bs ++ bs) where
--      bs = (rowT @30308) (n - 1)

-- instance OEIS 48728 where
--   oeisIx n = (oeisIx @8585) n - (oeisIx @48724) n

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

-- instance OEIS 48883 where
--   oeisIx = (oeisIx @244) . (oeisIx @120)

-- instance OEIS 48890 where
--   oeis = filter f (oeis @40) where
--      f x = all (`elem` [0,1,6,8,9]) ds && x' /= x && (oeisIx @10051) x' == 1
--        where x' = foldl c 0 ds
--              c v 6 = 10*v + 9; c v 9 = 10*v + 6; c v d = 10*v + d
--              ds = unfoldr d x
--              d z = if z == 0 then Nothing else Just $ swap $ divMod z 10

-- instance OEIS 48985 where
--   oeisIx = foldr (\d v -> 2 * v + d) 0 . concatMap
--      (unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2))
--      . reverse . (rowT @27746)

-- instance OEIS 49068 where
--   oeisIx = elemIndices 0 (oeis @240025)

-- instance OEIS 49076 where
--   oeisIx = (+ 1) . (oeisIx @78442)

-- instance OEIS 49098 where
--   oeis = filter ((== 0) . (oeisIx @8966) . (+ 1)) (oeis @40)

-- instance OEIS 49354 where
--   oeis = filter f [1..] where
--      f n = t0 == (oeisIx @62756) n && t0 == (oeisIx @81603) n where t0 = (oeisIx @77267) n

-- instance OEIS 49388 where
--   oeisIx = (flip div 5040) . (oeisIx @142) . (+ 7)

-- instance OEIS 49389 where
--   oeisIx = (flip div 40320) . (oeisIx @142) . (+ 8)

-- instance OEIS 49398 where
--   oeisIx = (flip div 362880) . (oeisIx @142) . (+ 9)

-- instance OEIS 49445 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @199238)

-- instance OEIS 49514 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @95916

-- instance OEIS 49599 where
--   oeisIx = product . map ((+ 1) . (oeisIx @5) . fi) . (rowT @124010) . succ

-- instance OEIS 49613 where
--   oeisIx n = 2 * n - (oeisIx @7917) (2 * n - 2)

-- instance OEIS 49642 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @245656

-- instance OEIS 50000 where
--   oeis = 1 : f [1,0] where
--      f xs'@ (x:xs) | x `div` 2 `elem` xs = 3 * x : f (3 * x : xs')
--                   | otherwise = x `div` 2 : f (x `div` 2 : xs')

-- instance OEIS 50001 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @50000)) + 1

-- instance OEIS 50146 where
--   oeisIx n = if n == 0 then 1 else (oeisIx @35607) (2 * n - 2) (n - 1)
--   (oeisIx @50146) = lambda n : n*hypergeometric ([1-n, n], [2], -1) if n>0 else 1

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

-- instance OEIS 50360 where
--   oeisIx = (oeisIx @688) . (oeisIx @25487)

-- instance OEIS 50361 where
--   oeisIx = product . map (oeisIx @9) . (rowT @124010)

-- instance OEIS 50382 where
--   oeisIx = (oeisIx @8480) . (oeisIx @25487)

-- instance OEIS 50435 where
--   oeisIx = (oeisIx @2808) . (oeisIx @2808)
--   oeis = map (oeisIx @2808) (oeis @2808)

-- instance OEIS 50488 where
--   oeisIx n = sum $ zipWith (*) (oeis @79) (reverse $ take n (oeis @5408))

-- instance OEIS 50925 where
--   oeis = 1 : -1 : (tail $ map (numerator . sum) $
--      zipWith (zipWith (%))
--      (zipWith (map . (*)) (drop 2 (oeis @142)) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 50931 where
--   oeis = filter (any (== 1) . map (flip mod 6) . (rowT @27748)) [1..]

-- instance OEIS 50932 where
--   oeis = 1 : map (denominator . sum) (zipWith (zipWith (%))
--      (zipWith (map . (*)) (drop 2 (oeis @142)) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 50999 where
--   oeisIx = sum . map (^ 2) . (rowT @182469)

-- instance OEIS 51000 where
--   oeisIx = sum . map (^ 3) . (rowT @182469)

-- instance OEIS 51004 where
--   oeis =  [x | x <- (oeis @5349),
--                        x == head (dropWhile (< x) (oeis @34838))]

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

-- instance OEIS 51035 where
--   oeis = filter ((== 0) . (oeisIx @10051.pred)) (oeis @14091)

-- instance OEIS 51135 where
--   oeis = map length $ group (oeis @4001)

-- instance OEIS 51139 where
--   oeisIx n = (oeisIx @994) (n + 2) - (oeisIx @995) (n + 2)

-- instance OEIS 51144 where
--   oeis = filter ((== 0) . (oeisIx @8966)) (oeis @37)

-- instance OEIS 51147 where
--   oeisIx = fromJust . (`elemIndex` (oeis @51145)) . (2 ^)

-- instance OEIS 51169 where
--   oeisIx n = head [m | m <- [2..],
--               all (== 0) $ map (oeisIx' . (2*m -)) $ take n (oeis @40)]

-- instance OEIS 51178 where
--   oeis = filter (\x -> (oeisIx @27423) x `mod` x == 0) [1..]

-- instance OEIS 51250 where
--   oeis = filter (all ((== 1) . (oeisIx @10055)) . (rowT @38566)) [1..]

-- instance OEIS 51278 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @51521

-- instance OEIS 51279 where
--   oeis = map succ $ elemIndices 2 $ tail $ oeis @51521

-- instance OEIS 51282 where
--   oeisIx = (oeisIx @7814) . (oeisIx @25487)

-- instance OEIS 51283 where
--   oeis = filter (\x -> (oeisIx @34699 x) ^ 2 < x) [1..]

-- instance OEIS 51402 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` ms) where
--      ms = map (abs . (oeisIx @2321)) [1..]

-- instance OEIS 51431 where
--   oeisIx = (flip div 3628800) . (oeisIx @142) . (+ 10)

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

-- instance OEIS 51532 where
--   oeis = filter ((== 1) . (oeisIx @212793.pred)) (oeis @56867)

-- instance OEIS 51611 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @53603

-- instance OEIS 51656 where
--   oeisIx = sum . zipWith (*) (oeis @1906) . (rowT @47999)

-- instance OEIS 51701 where
--   oeis = f 2 $ 1 : (oeis @40) where
--      f d (q:ps@ (p:p':_)) = (if d <= d' then q else p') : f d' ps
--        where d' = p' - p

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

-- instance OEIS 52021 where
--   oeis = tail $ filter (\x -> (oeisIx @7953) x == (oeisIx @6530) x) [1..]

-- instance OEIS 52203 where
--   oeisIx n = (oeisIx @122366) (2 * n) n

-- instance OEIS 52248 where
--   oeis = f (oeis @65091) where
--      f (p:ps'@ (p':ps)) = (maximum $ map (oeisIx @6530) [p+1..p'-1]) : f ps'

-- instance OEIS 52474 where
--   oeisIx n = (tabl @56230) !! (n - 1) !! 0

-- instance OEIS 52485 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @112526

-- instance OEIS 52548 where
--   oeisIx = (+ 2) . (oeisIx @79)
--   oeis = iterate ((subtract 2) . (* 2)) 3

-- instance OEIS 53127 where
--   oeisIx = (* 2) . (oeisIx @53132)

-- instance OEIS 53210 where
--   oeisIx = sum . (rowT @51599)

-- instance OEIS 53212 where
--   oeisIx = (oeisIx @5)' . (oeisIx @7416)

-- instance OEIS 53636 where
--   oeisIx 0 = 0
--   oeisIx n = sum . zipWith (*) (map (oeisIx @10) ods)
--                  $ map ((2 ^) . (div n)) ods
--     where ods = rowT @182469 n

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

-- instance OEIS 54646 where
--   oeisIx 1 = 1
--   oeisIx n = (oeisIx @70167) $ (oeisIx @302) n

-- instance OEIS 54686 where
--   oeis = merge (oeis @290) (oeis @217) where
--      merge xs'@ (x:xs) ys'@ (y:ys)
--        | x <= y    = x : merge xs ys'
--        | otherwise = y : merge xs' ys

-- instance OEIS 54735 where
--   oeisIx = (+ 2) . (* 2) . (oeisIx @1359)

-- instance OEIS 54841 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ zipWith (*)
--                     (map ((10 ^) . subtract 1 . (oeisIx @49084)) $ (rowT @27748) n)
--                     (map fi $ (rowT @124010) n)

-- instance OEIS 55099 where
--   oeisIx n = (oeisIx @7481) (2 * n + 1) - (oeisIx @7481) (2 * n)

-- instance OEIS 55205 where
--   oeisIx n = genericLength [d | d <- [1..n^2], n^2 `mod` d == 0, (oeisIx @10052) d == 0]

-- instance OEIS 55212 where
--   oeisIx = subtract 1 . (oeisIx @33273)

-- instance OEIS 55217 where
--   oeisIx n = sum $ take (n + 1) $ (rowT @27907) (n + 1)

-- instance OEIS 55396 where
--   oeisIx = (oeisIx @49084) . (oeisIx @20639)

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

-- instance OEIS 56062 where
--   oeis = map length $ group (oeis @30190)

-- instance OEIS 56114 where
--   oeisIx n = (n + 1) * (oeisIx @7318)' (n + 9) 9

-- instance OEIS 56234 where
--   oeis = notUsed 1 (oeis @56231) (oeis @56232) (oeis @56233) where
--      notUsed x us'@ (u:us) vs'@ (v:vs) ws'@ (w:ws)
--       | x == u = notUsed (x + 1) us vs' ws'
--       | x == v = notUsed (x + 1) us' vs ws'
--       | x == w = notUsed (x + 1) us' vs' ws
--       | otherwise = x : notUsed (x + 1) us' vs' ws'

-- instance OEIS 56240 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @1414))

-- instance OEIS 56526 where
--   oeis = zipWith (-) (tail (oeis @960)) (oeis @960)

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

-- instance OEIS 56978 where
--   oeisIx = sum . map (fromEnum . ([0,0,1] `isPrefixOf`)) .
--                       tails . (rowT @30308)

-- instance OEIS 56979 where
--   oeisIx = sum . map (fromEnum . ([1,0,1] `isPrefixOf`)) .
--                       tails . (rowT @30308)

-- instance OEIS 57153 where
--   oeisIx n = (tabl @56230) !! (n - 1) !! (n-1)

-- instance OEIS 57211 where
--   oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @59841)

-- instance OEIS 57212 where
--   oeis = concat $ zipWith ($) (map replicate [1..]) (oeis @35)

-- instance OEIS 57226 where
--   oeisIx = (oeisIx @43537) . (oeisIx @61493)

-- instance OEIS 57449 where
--   oeisIx = product . (rowT @193829)

-- instance OEIS 57533 where
--   oeis = filter (\z -> p z [z]) [1..] where
--      p x ts = y > 0 && (y `elem` ts || p y (y:ts)) where y = (oeisIx @48050) x

-- instance OEIS 57588 where
--   oeisIx = (subtract 1) . product . (flip take (oeis @40))

-- instance OEIS 57661 where
--   oeisIx n = (oeisIx @51193) n `div` n

-- instance OEIS 57705 where
--   oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @57588)

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

-- instance OEIS 58277 where
--   oeis = map length $ group (oeis @7614)

-- instance OEIS 58331 where
--   oeisIx = (+ 1) . (oeisIx @1105)

-- instance OEIS 58369 where
--   oeis =
--      elemIndices 0 $ zipWith ((-) `on` (oeisIx @7953)) [0..] (oeis @290)

-- instance OEIS 58529 where
--   oeis = filter (\x -> all (`elem` (takeWhile (<= x) (oeis @1132)))
--                                    $ (rowT @27748) x) [1..]

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

-- instance OEIS 59316 where
--   oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @35250)) [1..]

-- instance OEIS 59448 where
--   oeisIx = (`mod` 2) . (oeisIx @23416)

-- instance OEIS 59481 where
--   oeis = tablList @59481
--   rowCol n k = (tabl @59481) !! n !! n
--   rowT n = (tabl @59481) !! n
--   tabl = map reverse (tabl @100100)

-- instance OEIS 59497 where
--   oeis = (oeis @40) \\  (oeis @59496)

-- instance OEIS 59590 where
--   oeis = elemIndices 1 $ map (oeisIx @115944) [0..]

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

-- instance OEIS 60054 where
--   oeis = -1 : map (numerator . sum) (tail $ zipWith (zipWith (%))
--      (zipWith (map . (*)) (oeis @142) (tabf @242179)) (oeisIx @106831)_tabf)

-- instance OEIS 60110 where
--   oeisIx = t . (oeisIx @60109) where
--      t 0 = 0
--      t n = if n == 0 then 0 else 3 * t n' + d  where (n', d) = divMod n 10

-- instance OEIS 60226 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @312) n - n * (oeisIx @312) (n - 1)

-- instance OEIS 60308 where
--   oeisIx = (oeisIx @7917) . (oeisIx @5843)

-- instance OEIS 60355 where
--   oeis = map (oeisIx @1694) $ (map succ $ elemIndices 1 $ tail $ oeis @76446)

-- instance OEIS 60372 where
--   oeisIx n = (oeisIx @60374 n + n) `div` 2

-- instance OEIS 60373 where
--   oeisIx n = (oeisIx @60372) n - n

-- instance OEIS 60374 where
--   oeisIx n = f $ dropWhile (< n) (oeis @5836) where
--      f (p:ps) | (oeisIx @39966) (p-n) == 1 && (oeisIx @39966) (2*p-n) == 1 = 2*p - n
--               | otherwise                                  = f ps

-- instance OEIS 60381 where
--   oeisIx n = (oeisIx @98012) (2 * n - 1) n

-- instance OEIS 60418 where
--   oeisIx = (oeisIx @54055) . (oeisIx @40)

-- instance OEIS 60432 where
--   oeisIx n = sum $ zipWith (*) [n,n - 1..1] (oeis @10054)

-- instance OEIS 60646 where
--   oeisIx n = (fromJust $ findIndex ((n+1) <) (oeis @14688)) + 1

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
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @46660

-- instance OEIS 60756 where
--   oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @60715)) [0..]

-- instance OEIS 60765 where
--   oeis = filter
--     (\x -> sort (nub $ (rowT @193829) x) `O.subset` (rowT @27750) x) [1..]

-- instance OEIS 60901 where
--   oeisIx = (oeisIx @38500) . (oeisIx @45)

-- instance OEIS 60968 where
--   oeisIx 1 = 1
--   oeisIx n = (if p == 2 then (if e == 1 then 2 else 2^ (e+1)) else 1) *
--      (product $ zipWith (*) (map (\q -> q - 2 + mod q 4) ps'')
--                             (zipWith (^) ps'' (map (subtract 1) es'')))
--      where (ps'', es'') = if p == 2 then (ps, es) else (ps', es')
--            ps'@ (p:ps) = (rowT @27748) n; es'@ (e:es) = (rowT @124010) n

-- instance OEIS 61205 where
--   oeisIx n = (oeisIx @4086) n * n

-- instance OEIS 61214 where
--   oeis = f (oeis @40) where
--      f (p:ps'@ (p':ps)) = (product [p+1..p'-1]) : f ps'

-- instance OEIS 61227 where
--   oeisIx n = p + (oeisIx @4086) p  where p = (oeisIx @40) n

-- instance OEIS 61258 where
--   oeisIx (succ->n) = sum $ zipWith (*) ds $ map (oeisIx @2322) ds
--               where ds = (rowT @27750) n

-- instance OEIS 61338 where
--   oeisIx 0 = 0
--   oeisIx n = (oeisIx @6519) n + (oeisIx @120) n - 1

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

-- instance OEIS 61674 where
--   oeisIx n = until ((== 1) . (oeisIx @136522) . (oeisIx @4151) . (* n)) (+ 1) 1

-- instance OEIS 61681 where
--   oeis = iterate (oeisIx @182324) 1

-- instance OEIS 61775 where
--   oeis = 1 : g 2 where
--      g x = y : g (x + 1) where
--         y = if t > 0 then (oeisIx @61775) t + 1 else (oeisIx @61775) u + (oeisIx @61775) v - 1
--             where t = (oeisIx @49084) x; u = (oeisIx @20639) x; v = x `div` u

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

-- instance OEIS 62162 where
--   oeisIx = abs . sum . (rowT @247453)

-- instance OEIS 62279 where
--   oeisIx 0 = 0
--   oeisIx n = until ((== 1) . (oeisIx @136522) . (oeisIx @4151)) (+ n) n

-- instance OEIS 62285 where
--   oeis = filter (even . (oeisIx @30)) (oeis @30141)

-- instance OEIS 62320 where
--   oeisIx = (^ 2) . (oeisIx @13929)

-- instance OEIS 62332 where
--   oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @208259)

-- instance OEIS 62373 where
--   oeis = map succ $ elemIndices 2 $ tail $ oeis @34380

-- instance OEIS 62401 where
--   oeisIx = (oeisIx @10) . (oeisIx @203)

-- instance OEIS 62402 where
--   oeisIx = (oeisIx @203) . (oeisIx @10)

-- instance OEIS 62503 where
--   oeisIx = (oeisIx @290) . (oeisIx @5117)

-- instance OEIS 62550 where
--   oeisIx 0 = 0
--   oeisIx n = sum $ (rowT @13942) n

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

-- instance OEIS 62759 where
--   oeisIx n = (oeisIx @7947) n ^ (oeisIx @51904) n

-- instance OEIS 62980 where
--   oeis = 1 : 5 : f 2 [5,1] where
--      f u vs'@ (v:vs) = w : f (u + 1) (w : vs') where
--        w = 6 * u * v + sum (zipWith (*) vs_ $ reverse vs_)
--        vs_ = init vs

-- instance OEIS 62992 where
--   oeisIx = sum . (rowT @234950)

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

-- instance OEIS 63453 where
--   oeisIx = product . map ((1 -) . (^ 3)) . (rowT @27748)

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

-- instance OEIS 63993 where
--   oeisIx n = genericLength [ () | let ts = takeWhile (< n) $ tail (oeis @217),
--                       x <- ts, y <- takeWhile (<= x) ts,
--                       let z = n - x - y, 0 < z, z <= y, (oeisIx @10054) z == 1]

-- instance OEIS 63994 where
--   oeisIx n = product $ map (gcd (n - 1) . subtract 1) $ (rowT @27748) n

-- instance OEIS 64097 where
--   oeis = 0 : f 2 where
--      f x | x == spf  = 1 + (oeisIx @64097) (spf - 1) : f (x + 1)
--          | otherwise = (oeisIx @64097) spf + (oeisIx @64097) (x `div` spf) : f (x + 1)
--          where spf = (oeisIx @20639) x

-- instance OEIS 64113 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @36263)

-- instance OEIS 64150 where
--   oeis = filter (\x -> x `mod` (oeisIx @53735) x == 0) [1..]

-- instance OEIS 64275 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @32447))

-- instance OEIS 64283 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @64272))

-- instance OEIS 64372 where
--   oeisIx 0 = 1
--   oeisIx n = sum $ map (oeisIx @64372) $ (rowT @124010 . succ) n

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

-- instance OEIS 64956 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @64417)) + 1

-- instance OEIS 64959 where
--   oeis = map ((+ 1) . fromJust . (`elemIndex` (oeis @64419))) [1..]

-- instance OEIS 64986 where
--   oeisIx = p (tail (oeis @142)) where
--      p _          0             = 1
--      p fs'@ (f:fs) m | m < f     = 0
--                     | otherwise = p fs' (m - f) + p fs m

-- instance OEIS 64987 where
--   oeisIx n = (oeisIx @203) n * n

-- instance OEIS 65003 where
--   oeis = elemIndices 0 $ map (oeisIx @214772) [0..43]

-- instance OEIS 65037 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @36552))

-- instance OEIS 65090 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @151763)

-- instance OEIS 65206 where
--   oeis = filter ((== 1) . (oeisIx @136522) . (oeisIx @56964)) (oeis @29742)

-- instance OEIS 65253 where
--   oeis = zipWith (+) (map ((* 10) . (subtract 1)) (oeis @64823)) (oeis @796)

-- instance OEIS 65254 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @65253))

-- instance OEIS 65306 where
--   oeis = map (subtract 2) $ f (concat (tabl @65305)) [] where
--      f (x:xs) ys = if x `elem` ys then f xs ys else x : f xs (x:ys)

-- instance OEIS 65307 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @65306)) + 1

-- instance OEIS 65330 where
--   oeisIx = (oeisIx @38502) . (oeisIx @265)

-- instance OEIS 65333 where
--   oeisIx = fromEnum . (== 1) . (oeisIx @38502) . (oeisIx @265)

-- instance OEIS 65338 where
--   oeisIx 1 = 1
--   oeisIx n = (spf `mod` 4) * (oeisIx @65338) (n `div` spf) where spf = (oeisIx @20639) n

-- instance OEIS 65339 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength [x | x <- (rowT @27746) n, mod x 4 == 3]

-- instance OEIS 65350 where
--   oeis = zipWith mod (tail (oeis @984)) (drop 2 (oeis @290))

-- instance OEIS 65383 where
--   oeisIx n = head $ dropWhile (< (oeisIx @217) n) (oeis @40)

-- instance OEIS 65500 where
--   oeisIx n = (oeisIx @3418) n + n - signum n

-- instance OEIS 65515 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @961)

-- instance OEIS 65516 where
--   oeis = zipWith (-) (tail (oeis @1358)) (oeis @1358)

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

-- instance OEIS 66054 where
--   oeis = iterate (oeisIx @56964) 10583

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

-- instance OEIS 66400 where
--   oeisIx = genericLength . (rowT @245499)

-- instance OEIS 66401 where
--   oeisIx = (oeisIx @196) . (oeisIx @245530)

-- instance OEIS 66446 where
--   oeisIx = (oeisIx @217) . subtract 1 . (oeisIx @5)'

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

-- instance OEIS 66729 where
--   oeisIx n = if pds == [1] then n else product pds
--               where pds = (rowT @27751) n

-- instance OEIS 66822 where
--   oeisIx = flip (oeisIx @38622) 3 . (+ 3)

-- instance OEIS 66938 where
--   oeis = map (oeisIx @40) $ filter ((> 0) . (oeisIx @67432)) [1..]

-- instance OEIS 66955 where
--   oeisIx n = genericLength [ (x,y,z) | x <- [1 .. (oeisIx @196) (div n 3)],
--                                 y <- [x .. div n x],
--                                 z <- [y .. div (n - x*y) (x + y)],
--                                 x * y + (x + y) * z == n]



-- instance OEIS 67046 where
--   oeisIx = (`div` 6) . (oeisIx @33931)

-- instance OEIS 67078 where
--   oeis = scanl (+) 1 (oeis @142)

-- instance OEIS 67139 where
--   oeis = 1 : map (+ 1) (elemIndices 1 (oeis @66376))

-- instance OEIS 67391 where
--   oeisIx n | n <= 2    = 1
--             | otherwise = foldl lcm 1 $ (rowT @173540) n

-- instance OEIS 67722 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10052) (n * (n + k)) == 1]

-- instance OEIS 67747 where
--   oeis = concat $ transpose [oeis, (oeis @2808)]

-- instance OEIS 67815 where
--   oeisIx n = gcd n $ (oeisIx @196) n

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

-- instance OEIS 68068 where
--   oeisIx = genericLength . filter odd . (rowT @77610)

-- instance OEIS 68074 where
--   oeisIx n | odd n     = - (oeisIx @48691) n
--             | otherwise = 2 * (oeisIx @48691) (n `div` 2) - (oeisIx @48691) n

-- instance OEIS 68101 where
--   oeisIx = sum . map (oeisIx @8683) . (rowT @161906)

-- instance OEIS 68148 where
--   oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @32981)

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

-- instance OEIS 68636 where
--   oeisIx n = min n $ (oeisIx @4086) n

-- instance OEIS 68637 where
--   oeisIx n = max n $ (oeisIx @4086) n

-- instance OEIS 68720 where
--   oeisIx = (oeisIx @3415) . (oeisIx @290)

-- instance OEIS 68781 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @261869

-- instance OEIS 68901 where
--   oeisIx n = head $
--      filter ((== 0) . (`mod` fi n) . (+ (oeisIx @40) n)) $ [0..]

-- instance OEIS 68919 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @56868)

-- instance OEIS 68936 where
--   oeis = [x | x <- [1..], (oeisIx @8472) x <= (oeisIx @1222) x]

-- instance OEIS 68997 where
--   oeis = filter (\x -> mod x (oeisIx @173557 x) == 0) [1..]

-- instance OEIS 69056 where
--   oeis = filter (\x -> x ^ 2 `mod` (oeisIx @46970) x == 0) [1..]

-- instance OEIS 69059 where
--   oeis = filter ((> 1) . (oeisIx @9194)) [1..]

-- instance OEIS 69104 where
--   oeis =
--      map (+ 1) $ elemIndices 0 $ zipWith mod (drop 2 (oeis @45)) [1..]

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

-- instance OEIS 69513 where
--   oeisIx 1 = 0
--   oeisIx n = (oeisIx @10055) n

-- instance OEIS 69536 where
--   oeis = map (* 8) (oeis @77495)

-- instance OEIS 69545 where
--   oeis = map length $ group (oeis @8836)

-- instance OEIS 69715 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @52423

-- instance OEIS 69835 where
--   oeisIx n = (oeisIx @81577) (2 * n) n

-- instance OEIS 69905 where
--   oeis = scanl (+) 0 (oeis @8615)

-- instance OEIS 69928 where
--   oeis = scanl1 (+) (oeis @245656)

-- instance OEIS 70005 where
--   oeis = filter ((== 0) . (oeisIx @10055)) (oeis @78174)

-- instance OEIS 70048 where
--   oeisIx = p (oeis @42968) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 70072 where
--   oeisIx n = genericLength [ () | x <- [1..n], y <- [1..x], (oeisIx @8966) (x*y) == 1]

-- instance OEIS 70168 where
--   oeis = tablList @70168
-- instance Table 70168 where
--   rowCol = rowCol_off @70168 @1 @1
--   tabf = map (rowT @70168) [1..]
--   rowT n = (takeWhile (/= 1) $ iterate (oeisIx @14682) n) ++ [1]



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

-- instance OEIS 70940 where
--   oeisIx = maximum . (rowT @80080)

-- instance OEIS 70960 where
--   oeisIx n = if n == 1 then 1 else 3 * (oeisIx @142) n `div` 2
--   oeis = map (flip div 2) fs where fs = 3 : zipWith (*) [2..] fs

-- instance OEIS 71139 where
--   oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) [2..]

-- instance OEIS 71140 where
--   oeis = filter (\x -> (oeisIx @8472) x `mod` (oeisIx @6530) x == 0) (oeis @24619)

-- instance OEIS 71188 where
--   oeisIx = (oeisIx @6530) . (oeisIx @5)

-- instance OEIS 71249 where
--   oeis = filter ((> 1) . (oeisIx @55483)) [1..]

-- instance OEIS 71295 where
--   oeisIx n = (oeisIx @120) n * (oeisIx @23416) n

-- instance OEIS 71321 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ zipWith (*) (oeis @33999) $ (rowT @27746) n

-- instance OEIS 71331 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @71330

-- instance OEIS 71521 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @3586)

-- instance OEIS 71574 where
--   oeisIx 1 = 0
--   oeisIx n = 2 * (oeisIx @71574) (if j > 0 then j + 1 else (oeisIx @49084) n) + 1 - signum j
--               where j = (oeisIx @66246) n

-- instance OEIS 71681 where
--   oeisIx n = sum $ map (oeisIx @10051) $
--      takeWhile (> 0) $ map (2 * (oeisIx @40) n -) $ drop n (oeis @40)

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

-- instance OEIS 72010 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map f $ (rowT @27746) n where
--      f 2 = 2
--      f p = p + 2 * (2 - p `mod` 4)

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

-- instance OEIS 72403 where
--   oeis = map denominator $ scanl1 (-) $
--     map ((1 %) . (oeisIx @244)) $ (oeis @29837)

-- instance OEIS 72452 where
--   oeis = 0 : map (oeisIx @4086) (zipWith (+) (oeis @72452) [1..])

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

-- instance OEIS 72595 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @72594

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

-- instance OEIS 72776 where

-- instance OEIS 72905 where
--   oeisIx n = head [k | k <- [n + 1 ..], (oeisIx @10052) (k * n) == 1]

-- instance OEIS 72941 where
--   oeisIx n = product $ zipWith (^) ps $ map (max 1) es where
--               (ps, es) = unzip $ dropWhile ((== 0) . snd) $
--                          zip (oeis @40) $ (rowT @67255) n

-- instance OEIS 73034 where
--   oeis = filter (`elem` [2,3,5,7]) (oeis @33308)

-- instance OEIS 73121 where
--   oeisIx n = (oeisIx @53644) n * (fi n + 2 * (oeisIx @53645) n)

-- instance OEIS 73138 where
--   oeisIx n = (oeisIx @38573) n * (oeisIx @80100) n

-- instance OEIS 73311 where
--   oeisIx = sum . map (oeisIx @8966) . (rowT @38566)

-- instance OEIS 73334 where
--   oeisIx 0 = 3
--   oeisIx n = (oeisIx @45) $ (oeisIx @5811) n + 4

-- instance OEIS 73353 where
--   oeisIx n = n + (oeisIx @7947) n

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
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @192280

-- instance OEIS 73490 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength $ filter (> 1) $ zipWith (-) (tail ips) ips
--      where ips = map (oeisIx @49084) $ (rowT @27748) n

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

-- instance OEIS 73576 where
--   oeisIx = p (oeis @5117) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 73642 where
--   oeisIx = sum . zipWith (*) [0..] . (rowT @30308)

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

-- instance OEIS 73846 where
--   oeis = concat $ transpose [oeis, (oeis @40)]

-- instance OEIS 73890 where
--   oeisIx n = numerator $ n % (oeisIx @196) n

-- instance OEIS 74235 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @227481

-- instance OEIS 74721 where
--   oeis = f 0 $ (oeis @33308) where
--      f c ds'@ (d:ds) | (oeisIx @10051)'' c == 1 = c : f 0 ds'
--                     | otherwise = f (10 * c + d) ds

-- instance OEIS 74819 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @92410

-- instance OEIS 74964 where
--   oeis = filter (\x -> (oeisIx @74963) x == (oeisIx @65764) x) [1..]

-- instance OEIS 74985 where
--   oeisIx = (oeisIx @290) . (oeisIx @1358)

-- instance OEIS 74989 where
--   oeisIx 0 = 0
--   oeisIx n = min (n - last xs) (head ys - n) where
--      (xs,ys) = span (< n) (oeis @578)

-- instance OEIS 75090 where
--   oeis = filter even (oeis @1597)

-- instance OEIS 75104 where
--   oeisIx n = gcd n $ (oeisIx @523) n

-- instance OEIS 75105 where
--   oeisIx n = numerator $ n % (oeisIx @523) n

-- instance OEIS 75106 where
--   oeisIx n = denominator $ n % (oeisIx @523) n

-- instance OEIS 75109 where
--   oeis = filter odd (oeis @1597)

-- instance OEIS 75119 where
--   oeisIx n = denominator $ n % (oeisIx @196) n

-- instance OEIS 75157 where
--   oeisIx 0 = 0
--   oeisIx n = product (zipWith (^) (oeis @40) rs') - 1 where
--      rs' = reverse $ r : map (subtract 1) rs
--      (r:rs) = reverse $ map length $ group $ (rowT @30308) n

-- instance OEIS 75158 where
--   oeisIx = fromJust . (`elemIndex` (oeis @75157))

-- instance OEIS 75180 where
--   oeis = map (denominator . sum) $ zipWith (zipWith (%))
--      (zipWith (map . (*)) (oeis @142) (tabf @242179)) (tabf @106831)

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

-- instance OEIS 75425 where
--   oeisIx n = snd $ until ((== 1) . fst)
--                           (\ (x, i) -> (oeisIx @75423 x, i + 1)) (n, 0)

-- instance OEIS 75426 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75425))

-- instance OEIS 76191 where
--   oeis = zipWith (-) (tail (oeis @1222)) (oeis @1222)

-- instance OEIS 76217 where
--   oeis = 1 : zipWith (+) (oeis @76217)
--      (zipWith (*) [2..] $ map (oeisIx @57427) $ zipWith (-) [2..] (oeis @76217))

-- instance OEIS 76259 where
--   oeis = zipWith (-) (tail (oeis @5117)) (oeis @5117)

-- instance OEIS 76396 where
--   oeisIx = (oeisIx @20639) . (oeisIx @25478)

-- instance OEIS 76397 where
--   oeisIx = (oeisIx @6530) . (oeisIx @25478)

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

-- instance OEIS 76948 where
--   oeisIx 1 = 1
--   oeisIx n = if null qs then 0 else head qs
--               where qs = filter ((> 0) . (oeisIx @37213) . subtract 1 . (* n)) [1..n]

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
--   rowCol n k = (rowT_off @77558) n !! (k- 1)
--   rowT n = n : genericTake (n - 1)
--                       (filter ((== (oeisIx @46523) n) . (oeisIx @46523)) [n + 1 ..])
--   tabf = map (rowT @77558) [1..]

-- instance OEIS 77582 where
--   oeisIx = sum . (rowT @77581)

-- instance OEIS 77609 where
--   oeis = tablList @77609
-- instance Table 77609 where
--   rowCol n k = (rowT @77609) n !! (k- 1)
--   rowT n = filter
--      (\d -> d == 1 || null (rowT @77609 d \\ (rowT @213925) n)) $ (rowT @27750) n
--   tabf = map (rowT @77609) [1..]

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

-- instance OEIS 77957 where
--   oeisIx = sum . (rowT @204293)

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

-- instance OEIS 78175 where
--   oeis = filter (\x -> (oeisIx @1414 x) `mod` (oeisIx @1222 x) == 0) [2..]

-- instance OEIS 78179 where
--   oeisIx n = n ^ (oeisIx @78178 n) + n - 1

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

-- instance OEIS 78430 where
--   oeisIx = sum . (rowT @245717)

-- instance OEIS 78440 where
--   oeis = filter notbp (oeis @196871) where
--      notbp x = m > 0 && x > 1 || m == 0 && notbp x' where
--         (x',m) = divMod x 2

-- instance OEIS 78442 where
--   oeisIx (succ->n) = fst $ until ((== 0) . snd)
--                 (\ (i, p) -> (i + 1, (oeisIx @49084) p)) (-2, (oeisIx @40) n)

-- instance OEIS 78613 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @5094

-- instance OEIS 78637 where
--   oeisIx n = (oeisIx @7947) $ product [n..n+2]

-- instance OEIS 78649 where
--   oeis = map (+ 1) $ (map succ $ elemIndices 0 $ tail $ oeis @54354)

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

-- instance OEIS 78823 where
--   oeisIx = sum . (rowT @119709)

-- instance OEIS 78826 where
--   oeisIx n | n <= 1 = 0
--             | otherwise = length $ (rowT @225243) n

-- instance OEIS 78829 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @78826

-- instance OEIS 78832 where
--   oeisIx = head . (rowT @225243)

-- instance OEIS 78833 where
--   oeisIx = last . (rowT @225243)

-- instance OEIS 78894 where
--   oeis = sieve (oeis @40) where
--      sieve (p:ps) = p : sieve [q | (i,q) <- zip [2..] ps, mod i p > 0]

-- instance OEIS 78972 where
--   oeis = filter brilliant (oeis @1358) where
--      brilliant x = (on (==) (oeisIx @55642)) p (x `div` p) where p = (oeisIx @20639) x

-- instance OEIS 79062 where
--   oeis = 2 : f 2 (tail (oeis @40)) where
--      f x ps = q : f q qs where
--        (q:qs) = dropWhile (\p -> (oeisIx @75802) (p - x) == 0 || p - x == 1) ps

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

-- instance OEIS 79579 where
--   oeisIx 1 = 1
--   oeisIx n = product $ zipWith (*) pfs $ map (subtract 1) pfs
--      where pfs = (rowT @27746) n

-- instance OEIS 79890 where
--   oeisIx n = head [x | x <- [n + 1 ..], (oeisIx @1222) x == 1 + (oeisIx @1222) n]

-- instance OEIS 80170 where
--   oeis = filter f [1..] where
--      f x = foldl1 gcd (map (flip (oeisIx @7318)' x) [2*x, 3*x .. x* (x+1)]) == 1

-- instance OEIS 80225 where
--   oeisIx n = genericLength [d | d <- takeWhile (<= n) (oeis @396), mod n d == 0]

-- instance OEIS 80239 where
--   oeis = 1 : 1 : zipWith (+)
--      (tail (oeis @11765)) (zipWith (+) (oeis @80239) $ tail (oeis @80239))

-- instance OEIS 80257 where
--   oeis = m (oeis @24619) (oeis @33942) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y  = x : m xs ys'
--                              | x == y = x : m xs ys
--                              | x > y  = y : m xs' ys

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

-- instance OEIS 80672 where
--   oeis = filter ((<= 7) . (oeisIx @20639)) [2..]

-- instance OEIS 80709 where
--   oeis = iterate (oeisIx @3132) 4

-- instance OEIS 80719 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 .
--              concat . mapMaybe (flip lookup bin) . (rowT @31298)
--               where bin = zip [0..9] (tabf @30308)

-- instance OEIS 80736 where
--   oeisIx n = if n `mod` 4 == 2 then 0 else (oeisIx @10) n

-- instance OEIS 80764 where
--   oeis = tail $ zipWith (-) (tail (oeis @49472)) (oeis @49472)

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
--   oeis = map succ $ elemIndices 2 $ tail $ oeis @80942

-- instance OEIS 80944 where
--   oeis = filter ((<= 2) . (oeisIx @80942)) [1..]

-- instance OEIS 80945 where
--   oeis = filter ((> 2) . (oeisIx @80942)) [1..]

-- instance OEIS 80946 where
--   oeis = map succ $ elemIndices 3 $ tail $ oeis @80942

-- instance OEIS 80947 where
--   oeis = filter ((> 3) . (oeisIx @80942)) [1..]

-- instance OEIS 80982 where
--   oeisIx n = (+ 1) $ fromJust $
--      findIndex ((== 0) . (`mod` (n ^ 2))) $ tail (oeis @217)

-- instance OEIS 80983 where
--   oeisIx = (oeisIx @217) . (oeisIx @80982)

-- instance OEIS 80995 where
--   oeisIx = (oeisIx @33683) . (+ 1) . (* 24)

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
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @81308

-- instance OEIS 81311 where
--   oeisIx n = (oeis @81310) !! (n - 1)
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @81308

-- instance OEIS 81312 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @81308

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

-- instance OEIS 81619 where
--   oeis = filter ((== 1) . (oeisIx @10054) . (oeisIx @5)) [1..]

-- instance OEIS 81729 where
--   oeisIx n = (oeisIx @209229) n + (oeisIx @33999) (n)

-- instance OEIS 81770 where
--   oeis = filter ((== 1) . (oeisIx @8966) . (`div` 4)) (oeis @17113)

-- instance OEIS 81827 where
--   oeis = zipWith (-) (tail (oeis @5185)) (oeis @5185)

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

-- instance OEIS 82416 where
--   oeis = map (`mod` 2) (oeis @73941)

-- instance OEIS 82495 where
--   oeisIx n = (oeisIx @15910) n + (oeisIx @48298) n - 1

-- instance OEIS 82582 where
--   oeis = 1 : 1 : f [1,1] where
--      f xs'@ (x:_:xs) = y : f (y : xs') where
--        y = x + sum (zipWith (*) xs' $ reverse xs)

-- instance OEIS 82587 where
--   oeis = concat $ transpose [tail (oeis @204), (oeis @204)]

-- instance OEIS 82763 where
--   oeis = filter (containsL . (oeisIx @61493)) [1..3999] where
--      containsL x = d == 4 || x > 0 && containsL x' where
--                    (x',d) = divMod x 10

-- instance OEIS 82766 where
--   oeis = concat $ transpose [oeis, tail (oeis @1333)]

-- instance OEIS 83025 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength [x | x <- (rowT @27746) n, mod x 4 == 1]

-- instance OEIS 83207 where
--   oeis = filter (z 0 0 . (rowT @27750)) $ [1..] where
--      z u v []     = u == v
--      z u v (p:ps) = z (u + p) v ps || z u (v + p) ps

-- instance OEIS 83347 where
--   oeis = filter ((< 0) . (oeisIx @168036)) [1..]

-- instance OEIS 83348 where
--   oeis = filter ((> 0) . (oeisIx @168036)) [1..]

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
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @84115

-- instance OEIS 84126 where
--   oeisIx = (oeisIx @20639) . (oeisIx @1358)

-- instance OEIS 84127 where
--   oeisIx = (oeisIx @6530) . (oeisIx @1358)

-- instance OEIS 84190 where
--   oeisIx 1 = 1
--   oeisIx n = foldl1 lcm $ map (subtract 1) $ tail $ (rowT @27750)' n

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

-- instance OEIS 84933 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @84937)) + 1

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

-- instance OEIS 85604 where
--   oeis = tablList @85604
-- instance Table 85604 where
--   rowCol = rowCol_off @85604 @2 @1
--   rowT 1 = [0]
--   rowT n = (rowT @115627) n ++ (take $ (oeisIx @62298) $ fi n) [0,0..]
--   tabl = map (rowT @85604) [1..]

-- instance OEIS 85721 where
--   oeis = [p*q | (p,q) <- zip (oeis @84126) (oeis @84127),
--                         oeisIx p == (oeisIx @70939) q]

-- instance OEIS 85730 where
--   oeisIx 1 = 1
--   oeisIx n = (p - 1) * p ^ (e - 1)
--      where p =  (oeisIx @25473) n; e =  (oeisIx @25474) n

-- instance OEIS 85731 where
--   oeisIx n = gcd n $ (oeisIx @3415) n

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

-- instance OEIS 86299 where
--   oeisIx = fromEnum . (<= 7) . (oeisIx @6530)

-- instance OEIS 86457 where
--   oeis = filter (\x -> (oeisIx @30) x == (oeisIx @30) (x^2) &&
--                                oeisIx x == (oeisIx @10879) (x^2)) [0..]

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

-- instance OEIS 86862 where
--   oeis = zipWith (-) (tail (oeis @2113)) (oeis @2113)

-- instance OEIS 86892 where
--   oeis = tail $ zipWith gcd (oeis @225) (oeis @3462)

-- instance OEIS 86971 where
--   oeisIx = sum . map (oeisIx @64911) . (rowT @27750)

-- instance OEIS 87039 where
--   oeisIx n | null ps   = 1
--             | otherwise = head ps
--             where ps = tail $ reverse $ (rowT @27746) n

-- instance OEIS 87188 where
--   oeisIx = p (oeis @5117) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 87207 where
--   oeisIx = sum . map ((2 ^) . (subtract 1) . (oeisIx @49084)) . (rowT @27748)

-- instance OEIS 87686 where
--   oeis = map succ $ findIndices (> 1) (oeis @51135)

-- instance OEIS 87713 where
--   oeisIx = (oeisIx @6530) . (oeisIx @84920)

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

-- instance OEIS 88209 where
--   oeis = zipWith (+) (oeis @45) $ tail (oeis @45925)

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

-- instance OEIS 88763 where
--   oeisIx = flip div 2 . (oeisIx @87695)

-- instance OEIS 88864 where
--   oeisIx 1 = 0
--   oeisIx n = maximum $ zipWith ((*) `on` foldr (\d v -> v * 2 + d) 0)
--               (init $ tail $ inits bs) (init $ tail $ tails bs)
--               where bs = (rowT @30308) n

-- instance OEIS 88957 where
--   oeisIx = sum . (rowT @88956)

-- instance OEIS 89224 where
--   oeisIx = (oeisIx @23416) . (oeisIx @23416)

-- instance OEIS 89341 where
--   oeis = filter (\x -> (oeisIx @6530) x < 2 * (oeisIx @20639) x) (oeis @24619)

-- instance OEIS 89625 where
--   oeisIx n = f n 0 (oeis @40) where
--      f 0 y _      = y
--      f x y (p:ps) = f x' (y + p * r) ps where (x',r) = divMod x 2

-- instance OEIS 89648 where
--   oeis = filter ((<= 1) . abs . (oeisIx @37861)) [0..]

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

-- instance OEIS 90419 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @90418

-- instance OEIS 90420 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @90418

-- instance OEIS 90421 where
--   oeis = filter ((> 0) . (oeisIx @90418)) [1..]

-- instance OEIS 90422 where
--   oeis = filter ((== 1) . (oeisIx @90418) . fromInteger) (oeis @40)

-- instance OEIS 90423 where
--   oeis = filter ((> 1 ) . (oeisIx @90418) . fromInteger) (oeis @40)

-- instance OEIS 90424 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @90418))

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

-- instance OEIS 92206 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @214295

-- instance OEIS 92246 where
--   oeis = filter odd (oeis @69)

-- instance OEIS 92495 where
--   oeisIx n = fromJust $ find ((== 0) . (`mod` n)) $ (oeis @142)

-- instance OEIS 92693 where
--   oeisIx 1 = 0
--   oeisIx n = (+ 1) $ sum $ takeWhile (/= 1) $ iterate (oeisIx @10) $ (oeisIx @10) n

-- instance OEIS 92954 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @92953))

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

-- instance OEIS 93446 where
--   oeisIx = maximum . (rowT @93445)

-- instance OEIS 93641 where
--   oeis = filter ((<= 2) . (oeisIx @1227)) [1..]

-- instance OEIS 93642 where
--   oeis = filter
--     (\x -> not $ all (`isInfixOf` b x) $ map b $ (rowT @27750) x) [1..] where
--     b = unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

-- instance OEIS 93703 where
--   oeis = filter
--      ((`elem` map (oeisIx @61493) [1..3999]) . (oeisIx @4086) . (oeisIx @61493)) [1..]

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

-- instance OEIS 94015 where
--   oeisIx = sum . (rowT @152842)

-- instance OEIS 94178 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @125203)

-- instance OEIS 94328 where
--   oeis = iterate (oeisIx @6369) 4

-- instance OEIS 94329 where
--   oeis = iterate (oeisIx @6369) 16

-- instance OEIS 94379 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @66955))

-- instance OEIS 94501 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @86793))

-- instance OEIS 94524 where
--   oeisIx = (+ 2) . (* 3) . (oeisIx @23208)

-- instance OEIS 94588 where
--   oeis = 0 : zipWith (+) (tail (oeis @45)) (zipWith (*) [1..] (oeis @45))

-- instance OEIS 94638 where
--   oeis = tablList @94638
-- instance Table 94638 where
--   rowCol = rowCol_off @94638 @1 @1
--   rowT   = rowT_off   @94638 @1
--   tabl = map reverse (tabl @130534)

-- instance OEIS 94784 where
--   oeis = [x | x <- [0..], (oeisIx @10052) x == 0, (oeisIx @10057) x == 0]

-- instance OEIS 95050 where
--   oeis = map (+ 1) $ elemIndices 10 $ map (oeisIx @95048) [1..]

-- instance OEIS 95072 where
--   oeis = filter ((== 1) . (oeisIx @10051.pred)) (oeis @31444)

-- instance OEIS 95114 where
--   oeis = 1 : f [1] 1 where
--      f xs@ (x:_) k = y : f (y:xs) (k+1) where
--        y = x + length [z | z <- xs, z <= k]

-- instance OEIS 95381 where
--   oeis = map (+ 1) $ elemIndices 1 $ map (oeisIx @209229) (oeis @25586)

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

-- instance OEIS 95916 where
--   oeis = zipWith (-) (tail (oeis @796)) (oeis @796)

-- instance OEIS 95960 where
--   oeisIx n = genericLength [x | x <- (rowT @27750) n, x < (oeisIx @7947) n]

-- instance OEIS 96138 where
--   oeis = 1 : g 2 1 where
--      g x y = z : g (x + 1) z where z = (oeisIx @4086) (x * y)

-- instance OEIS 96165 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @1222)) $ tail (oeis @961)

-- instance OEIS 96268 where
--   oeisIx = (subtract 1) . (oeisIx @56832) . (+ 1)

-- instance OEIS 96363 where
--   oeisIx = (oeisIx @1175) . (10 ^)

-- instance OEIS 96460 where
--   oeis = 1 : iterate (\x -> x + (oeisIx @8472) x) 2

-- instance OEIS 96461 where
--   oeis = 1 : iterate (oeisIx @75254) 2

-- instance OEIS 96494 where
--   oeisIx = (* 2) . (oeisIx @6)

-- instance OEIS 96780 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75383))

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

-- instance OEIS 97140 where
--   oeis = concat $ transpose [oeis, map (1 -) (oeis @1477)]

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

-- instance OEIS 97451 where
--   oeisIx n = p (oeis @47228) n where
--      p _  0         = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 97602 where
--   oeis = 1 : f 1 1 where
--      f x c = y : f y (c + (oeisIx @10052) y) where y = x + c

-- instance OEIS 97613 where
--   oeisIx n = (oeisIx @209561) (2 * n - 1) n

-- instance OEIS 97796 where
--   oeisIx = p (oeis @396) where
--      p _ 0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 97944 where
--   oeisIx = (oeisIx @55642) . (oeisIx @40)

-- instance OEIS 97974 where
--   oeisIx n = sum [p | p <- (rowT @27748) n, p ^ 2 <= n]

-- instance OEIS 97977 where
--   oeisIx n = head [p | p <- dropWhile (<= n) (oeis @40),
--   oeisIx (p + n) == n]

-- instance OEIS 98096 where
--   oeisIx n = (oeisIx @1248) n * (oeisIx @34785) n

-- instance OEIS 98237 where
--   oeis = filter ((== 0) . (oeisIx @109925)) (oeis @71904)

-- instance OEIS 98282 where
--   oeisIx n = f [n] where
--      f xs = if y `elem` xs then length xs else f (y:xs) where
--        y = genericIndex (map (oeisIx @87712) [1..]) (head xs - 1)

-- instance OEIS 98312 where
--   oeisIx = (oeisIx @98311) . (oeisIx @98311)

-- instance OEIS 98313 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98311))

-- instance OEIS 98314 where
--   oeisIx = (oeisIx @98313) . (oeisIx @98313)

-- instance OEIS 98430 where
--   oeisIx n = (oeisIx @302) n * (oeisIx @984) n

-- instance OEIS 98549 where
--   oeisIx = (oeisIx @98548) . (oeisIx @98548)

-- instance OEIS 98553 where
--   oeisIx = (oeisIx @98551) . (oeisIx @98551)

-- instance OEIS 98565 where
--   oeis = map (+ 2 ) $ elemIndices 3 (oeis @59233)

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

-- instance OEIS 99009 where
--   oeis = [x | x <- [0..], (oeisIx @151949) x == x]

-- instance OEIS 99188 where
--   oeisIx = (* 2) . (oeisIx @49474)

-- instance OEIS 99302 where
--   oeisIx n = genericLength $ filter (== n) $ map (oeisIx @3415) [1 .. (oeisIx @2620) n]

-- instance OEIS 99304 where
--   oeisIx n = succ $ fromJust $ elemIndex 0 $
--      zipWith (-) (drop (fromInteger n + 1) (oeis @3415))
--                  (map (+ n') $ tail (oeis @3415))
--      where n' = (oeisIx @3415) n

-- instance OEIS 99425 where
--   oeisIx = sum . (rowT @102413)

-- instance OEIS 99543 where
--   oeisIx = (oeisIx @1414) . (oeisIx @99542)

-- instance OEIS 99619 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @98962)) . (oeisIx @40)

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

-- instance OEIS 100320 where
--   oeisIx n = (oeisIx @124927) (2 * n) n

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

-- instance OEIS 100708 where
--   oeis = map abs $ zipWith (-) (tail (oeis @100707)) (oeis @100707)

-- instance OEIS 100732 where
--   oeisIx = (oeisIx @142) . (oeisIx @8585)

-- instance OEIS 100795 where
--   oeis = f 0 (oeis @2024) where
--      f x ws = v : f v (us ++ vs) where (us, v:vs) = span (== x) ws

-- instance OEIS 100861 where
--   oeis = tablList @100861
-- instance Table 100861 where
--   tabf = zipWith take (oeis @8619) (tabl @144299)

-- instance OEIS 100962 where
--   oeis = filter ((== 0) . (oeisIx @64911)) (oeis @14092)

-- instance OEIS 101048 where
--   oeisIx = p (oeis @1358) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 101082 where
--   oeis = filter ((> 0) . (oeisIx @49502)) [0..]

-- instance OEIS 101300 where
--   oeisIx = (oeisIx @151800) . (oeisIx @151800)

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

-- instance OEIS 102364 where
--   oeisIx 0 = 0
--   oeisIx n = genericLength $ filter (== 0) $ (rowT @213676) n

-- instance OEIS 102376 where
--   oeisIx = (4 ^) . (oeisIx @120)

-- instance OEIS 102466 where
--   oeis = [x | x <- [1..], (oeisIx @5) x == (oeisIx @1221) x + (oeisIx @1222) x]

-- instance OEIS 102467 where
--   oeis = [x | x <- [1..], (oeisIx @5) x /= (oeisIx @1221) x + (oeisIx @1222) x]

-- instance OEIS 102478 where
--   oeisIx = flip div 2 . (oeisIx @68700)

-- instance OEIS 103147 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @47160))

-- instance OEIS 103192 where
--   oeis = iterate (fromInteger . (oeisIx @102370)) 1

-- instance OEIS 103285 where
--   oeisIx = last . (rowT @103284)

-- instance OEIS 103371 where
--   oeis = tablList @103371
-- instance Table 103371 where
--   tabl = map reverse (tabl @132813)

-- instance OEIS 103747 where
--   oeis = iterate (fromInteger . (oeisIx @102370)) 2

-- instance OEIS 103889 where
--   oeisIx n = n - 1 + 2 * mod n 2
--   oeis = concat $ transpose [tail (oeis @5843), (oeis @5408)]

-- instance OEIS 103960 where
--   oeisIx n = sum [oeisIx' $ p * q - 2 |
--                    let p = (oeisIx @40) n, q <- takeWhile (<= p) (oeis @40)]

-- instance OEIS 104126 where
--   oeisIx n = p ^ (p + 1) where p = (oeisIx @40) n

-- instance OEIS 104235 where
--   oeis = [x | x <- [0..], (oeisIx @102370) x == x]

-- instance OEIS 104315 where
--   oeis = filter (\x -> (oeisIx @168046) x == 0 && (oeisIx @168046) (x ^ 2) == 1) [1..]

-- instance OEIS 104324 where
--   oeisIx = genericLength . map length . group . (rowT @213676)

-- instance OEIS 104684 where
--   oeis = tablList @104684
-- instance Table 104684 where
--   tabl = map (map abs) $ zipWith (zipWith (*)) (tabl @130595) (tabl @92392)

-- instance OEIS 104777 where
--   oeisIx = (^ 2) . (oeisIx @7310)

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

-- instance OEIS 105271 where
--   oeis = [x | x <- [0..], (oeisIx @105025) x == x]

-- instance OEIS 105321 where
--   oeisIx n = if n == 0 then 1 else (oeisIx @1316) n + (oeisIx @1316) (n - 1)

-- instance OEIS 105441 where
--   oeis = filter ((> 2) . (oeisIx @1227)) [1..]

-- instance OEIS 105571 where
--   oeis = [x | x <- [3..], (oeisIx @64911) (x - 2) == 1, (oeisIx @64911) (x + 2) == 1]

-- instance OEIS 105612 where
--   oeisIx = (subtract 1) . (oeisIx @224)

-- instance OEIS 106146 where
--   oeisIx = flip mod 10 . (oeisIx @1358)

-- instance OEIS 106151 where
--   oeisIx = foldr (\b v -> 2 * v + b) 0 . concatMap
--      (\bs'@ (b:bs) -> if b == 0 then bs else bs') . group . (rowT @30308)

-- instance OEIS 106315 where
--   oeisIx n = n * (oeisIx @5) n `mod` (oeisIx @203) n

-- instance OEIS 106372 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @106370))

-- instance OEIS 106404 where
--   oeisIx n = genericLength [d | d <- takeWhile (<= n) (oeis @100484), mod n d == 0]

-- instance OEIS 106799 where
--   oeisIx = (oeisIx @1222) . (oeisIx @65330)

-- instance OEIS 107711 where
--   oeis = tablList @107711
-- instance Table 107711 where
--   tabl = [1] : zipWith (map . flip div) [1..]
--                  (tail $ zipWith (zipWith (*)) (tabl @7318) (tabl @109004))

-- instance OEIS 107740 where
--   oeisIx n = genericLength [ () | let p = (oeisIx @40) n,
--                            m <- [max 0 (p - 9 * (oeisIx @55642) p) .. p - 1],
--                            oeisIx m == p]

-- instance OEIS 107741 where
--   oeisIx n = if null ms then 0 else head ms  where
--      ms = [m | let p = (oeisIx @40) n,
--                m <- [max 0 (p - 9 * (oeisIx @55642) p) .. p - 1], (oeisIx @62028) m == p]

-- instance OEIS 107750 where
--   oeis = 0 : f 0 where
--      f x = y : f y where
--        y = head [z | z <- [x + 1 ..], (oeisIx @23416) z /= (oeisIx @23416) x]

-- instance OEIS 107782 where
--   oeisIx n = (oeisIx @23416) n - (oeisIx @87116) n

-- instance OEIS 108309 where
--   oeisIx = sum . (map (oeisIx @10051 . pred)) . (rowT @176271)

-- instance OEIS 108348 where
--   oeis = 1 : f [2..] where
--      f (x:xs) = g (oeis @40) where
--        g (p:ps) = h 0 $ map ((`div` (p - 1)) . subtract 1) $
--                             iterate (* p) (p ^ 2) where
--          h i (pp:pps) | pp > x    = if i == 0 then f xs else g ps
--                       | pp < x    = h 1 pps
--                       | otherwise = x : f xs

-- instance OEIS 108546 where
--   oeis =  2 : concat
--      (transpose [oeis, (oeis @2144)])

-- instance OEIS 108647 where
--   oeisIx = flip (oeisIx @103371) 3 . (+ 3)

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

-- instance OEIS 108804 where
--   oeis = f [head (oeis @10060)] $ tail (oeis @10060) where
--      f xs (z:zs) = (sum $ zipWith (*) xs (reverse xs)) : f (z : xs) zs

-- instance OEIS 108906 where
--   oeis = zipWith (-) (tail (oeis @6899)) (oeis @6899)

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

-- instance OEIS 109983 where
--   oeis = tablList @109983
-- instance Table 109983 where
--   tabf = zipWith (++) (map (flip take (repeat 0)) [0..]) (tabl @63007)

-- instance OEIS 109984 where
--   oeisIx = sum . zipWith (*) [0..] . (rowT @109983)

-- instance OEIS 110085 where
--   oeis = filter (\x -> (oeisIx @51612) x < (oeisIx @110088) x) [1..]

-- instance OEIS 110086 where
--   oeis = filter (\x -> (oeisIx @51612) x <= (oeisIx @110088) x) [1..]

-- instance OEIS 110087 where
--   oeis = filter (\x -> (oeisIx @51612) x > (oeisIx @110088) x) [1..]

-- instance OEIS 110157 where
--   oeis = 0 : map ((+ 1) . (oeisIx @110157) . (+ 1)) (oeis @75423)

-- instance OEIS 110170 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @128966) (2 * n) n

-- instance OEIS 110240 where
--   oeisIx = foldl (\v d -> 2 * v + d) 0 . (rowT @70950)

-- instance OEIS 110353 where
--   oeisIx n = (+ 1) $ fromJust $
--      findIndex ((== 0) . (`mod` t)) $ dropWhile (<= t) (oeis @217)
--      where t = (oeisIx @217) n

-- instance OEIS 110475 where
--   oeisIx 1 = 0
--   oeisIx n = genericLength us - 1 + 2 * length vs where
--               (us, vs) = span (== 1) $ (rowT @118914) n

-- instance OEIS 110765 where
--   oeisIx = product . zipWith (^) (oeis @40) .  reverse . (rowT @30308)

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
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @261890

-- instance OEIS 114183 where
--   oeis = 1 : f [1] where
--      f xs@ (x:_) = y : f (y : xs) where
--        y = if z `notElem` xs then z else 2 * x where z = (oeisIx @196) x

-- instance OEIS 114228 where
--   oeisIx n = head [m | m <- [1..],
--                         (oeisIx @10051 . pred) (oeisIx @40 n + 2 * (oeisIx @40) m) == 1]

-- instance OEIS 114229 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (map (oeisIx @114228) [2..]))

-- instance OEIS 114233 where
--   oeisIx (succ->n) = head
--     [m | m <- [1 .. n]
--     , (oeisIx @10051 . pred) (2 * (oeisIx @40) n + (oeisIx @40) m) == 1]

-- instance OEIS 114235 where
--   oeisIx n = head [p | let q = (oeisIx @40) n,
--                         p <- reverse $ takeWhile (< q) (oeis @40),
--                         (oeisIx @10051 . pred) (2 * q + p) == 1]

-- instance OEIS 114236 where
--   oeisIx n = head [m | m <- [1..],
--                         (oeisIx @10051 . pred) (2 * (oeisIx @40) n + (oeisIx @40) (n - m)) == 1]

-- instance OEIS 114262 where
--   oeisIx n = head [q | let (p:ps) = drop (n - 1) (oeis @40),
--                         q <- ps, (oeisIx @10051 . pred) (p + 2 * q) == 1]

-- instance OEIS 114265 where
--   oeisIx n = head [p | let (q:qs) = drop (n - 1) (oeis @40), p <- qs,
--                         (oeisIx @10051 . pred) (2 * q + p) == 1]

-- instance OEIS 114266 where
--   oeisIx n = head [m | m <- [1..],
--                         (oeisIx @10051 . pred) (2 * (oeisIx @40) n + (oeisIx @40) (n + m)) == 1]

-- instance OEIS 114334 where
--   oeis = (rowT @27750) (6 ^ 6)

-- instance OEIS 114374 where
--   oeisIx = p (oeis @13929) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 115300 where
--   oeisIx n = (oeisIx @54054) n * (oeisIx @54055) n

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

-- instance OEIS 116933 where
--   oeisIx n = head [k | k <- [1..], (oeisIx @10051 . pred) (n + k * (oeisIx @79578) n) == 1]

-- instance OEIS 116934 where
--   oeisIx n = head [q | k <- [1..], let q = n + k * (oeisIx @79578) n,
--                         (oeisIx @10051 . pred) q == 1]

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

-- instance OEIS 117214 where
--   oeisIx n = product $
--      filter ((> 0) . (mod m)) $ takeWhile (< (oeisIx @6530) m) (oeis @40)
--      where m = (oeisIx @5117) n

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

-- instance OEIS 118416 where
--   oeis = tablList @118416
-- instance Table 118416 where
--   rowCol = rowCol_off @118416 @1 @1
--   rowT 1 = [1]
--   rowT n = (map (* 2) $ (rowT @118416) (n - 1)) ++ [oeisIx @14480 (n- 1)]
--   tabl = map (rowT @118416) [1..]

-- instance OEIS 118532 where
--   oeis = iterate ((+ 15) . (oeisIx @4086)) 1

-- instance OEIS 118882 where
--   oeis = findIndices (> 1) (oeis @161)

-- instance OEIS 118886 where
--   oeis = filter ((> 1) . (oeisIx @88534)) (oeis @3136)

-- instance OEIS 118959 where
--   oeis = filter
--      (\x -> let x' = (oeisIx @4086) x in x' /= x && x `mod` x' == 0) [1..]

-- instance OEIS 118965 where
--   oeisIx = sum . map (0 ^) . (rowT @128924)

-- instance OEIS 119246 where
--   oeis =
--       filter (\x -> (oeisIx @10888) x `elem` (rowT @31298) (fromInteger x)) [0..]

-- instance OEIS 119259 where
--   oeisIx n = (oeisIx @119258) (2 * n) n

-- instance OEIS 119347 where
--   oeisIx = genericLength . nub . map sum . tail . subsequences . (rowT @27750)'

-- instance OEIS 119354 where
--   oeisIx = fromJust . (`elemIndex` (oeis @119352))

-- instance OEIS 119387 where
--   oeisIx n = genericLength $ takeWhile (< (oeisIx @70940) n) [1..n]

-- instance OEIS 119467 where
--   oeis = tablList @119467
-- instance Table 119467 where
--   tabl = map (map (flip div 2)) $
--                  zipWith (zipWith (+)) (tabl @7318) (tabl @130595)

-- instance OEIS 119629 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @14631))

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

-- instance OEIS 120944 where
--   oeis = filter ((== 1) . (oeisIx @8966)) (oeis @2808)

-- instance OEIS 120960 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @24362)

-- instance OEIS 121065 where
--   oeisIx = fromJust . (`elemIndex` (oeis @85513))

-- instance OEIS 121369 where
--   oeis = 1 : 1 : zipWith ((+) `on` (oeisIx @7947))
--                          (oeis @121369) (tail (oeis @121369))

-- instance OEIS 121993 where
--   oeis = filter (\x -> (oeisIx @45918) x < x) [0..]

-- instance OEIS 122426 where
--   oeis = [x | x <- [1..], (oeisIx @122425) x < x]

-- instance OEIS 122427 where
--   oeisIx n = (oeis @122426) !! (n - 1)
--   oeis = [x | x <- [1..], (oeisIx @122425) x == x]

-- instance OEIS 122428 where
--   oeis = [x | x <- [1..], (oeisIx @122425) x == (oeisIx @6530) x]

-- instance OEIS 122535 where
--   oeisIx = (oeisIx @40) . (oeisIx @64113)

-- instance OEIS 122631 where
--   oeis =
--      1 : 2 : map (oeisIx @6530) (zipWith (+) (map ((2 *) . (oeisIx @40)) (oeis @122631))
--                                       (map (oeisIx @40) (tail (oeis @122631))))

-- instance OEIS 122768 where
--   oeis = 0 : f (tail (oeis @41)) [1] where
--      f (p:ps) rs = (sum $ zipWith (*) rs $ tail (oeis @41)) : f ps (p : rs)

-- instance OEIS 122797 where
--   oeis  = 1 : zipWith (+) (oeis @122797) (map ((1 -) . (oeisIx @10054)) [1..])

-- instance OEIS 122953 where
--   oeisIx = genericLength . (rowT @165416)

-- instance OEIS 123087 where
--   oeis = scanl (+) 0 (oeis @96268)

-- instance OEIS 123345 where
--   oeis = filter
--     (\x -> all (`isInfixOf` b x) $ map b $ (rowT @27750) x) [1..] where
--     b = unfoldr (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

-- instance OEIS 123581 where
--   oeis = iterate (oeisIx @70229) 3

-- instance OEIS 123976 where
--   oeis = map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @45) [1..]

-- instance OEIS 124056 where
--   oeis = 1 : f [1] where
--      f xs@ (x:_) = y : f (y : xs) where
--        y = length $ filter (flip elem $ (rowT @27750) x) xs

-- instance OEIS 124134 where
--   oeis = filter ((> 0) . (oeisIx @161) . (oeisIx @45)) [1..]

-- instance OEIS 124240 where
--   oeis = filter
--      (\x -> all (== 0) $ map ((mod x) . pred) $ (rowT @27748) x) [1..]

-- instance OEIS 124837 where
--   oeisIx n = (oeisIx @213998) (n + 2) (n - 1)

-- instance OEIS 124838 where
--   oeisIx n = (oeisIx @213999) (n + 2) (n - 1)

-- instance OEIS 124934 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @125203)

-- instance OEIS 124978 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (tail (oeis @2635)))

-- instance OEIS 125086 where
--   oeis = f [0, 2 ..] (oeis @36990) where
--      f (u:us) vs'@ (v:vs) = if u == v then f us vs else u : f us vs'

-- instance OEIS 125217 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @125203)

-- instance OEIS 125218 where
--   oeis = map (+ 1) $ findIndices (> 1) (oeis @125203)

-- instance OEIS 125290 where
--   oeis = filter ((> 1) . (oeisIx @43537)) (oeis @52382)

-- instance OEIS 125639 where
--   oeis = filter f [1..] where
--      f x = sx > x && (oeisIx @1065) sx > sx where sx = (oeisIx @1065) x

-- instance OEIS 125640 where
--   oeis = f (oeis @125639) [] where
--      f (x:xs) ys = if null (oeisIx_row' x `intersect` ys)
--                       then x : f xs (x : ys) else f xs ys

-- instance OEIS 125886 where
--   oeis = 1 : f 1 [10..] where
--      f u vs = g vs where
--        g (w:ws) = if (oeisIx @10879) w == iu then w : f w (delete w vs) else g ws
--        iu = (oeisIx @30) u

-- instance OEIS 126024 where
--   oeisIx = genericLength . filter ((== 1) . (oeisIx @10052) . sum) .
--                             subsequences . enumFromTo 1

-- instance OEIS 126027 where
--   oeisIx = genericLength . (rowT @30717)

-- instance OEIS 126596 where
--   oeisIx n = (oeisIx @5810) n * (oeisIx @5408) n `div` (oeisIx @16777) n

-- instance OEIS 126684 where
--   oeis = tail $ m (oeis @695) $ map (* 2) (oeis @695) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y     = x : m xs ys'
--                              | otherwise = y : m xs' ys

-- instance OEIS 126768 where
--   oeis = map length $ group (oeis @117872)

-- instance OEIS 126869 where
--   oeisIx n = (rowT @204293) (2*n) !! n

-- instance OEIS 126949 where
--   oeis = filter h [1..] where
--      h m = not $ null [ (x, e) | x <- [2 .. m - 2], gcd x m == 1,
--                                 e <- [2 .. (oeisIx @10) m `div` 2],
--                                 x ^ e `mod` m == m - 1]

-- instance OEIS 127354 where
--   oeisIx = (oeisIx @47842) . (oeisIx @40)

-- instance OEIS 127355 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @47842)) (oeis @40)

-- instance OEIS 127366 where
--   oeisIx n | even n'   = n'
--             | otherwise = 2*n - n'
--             where n' = n + (oeisIx @196) n

-- instance OEIS 127367 where
--   oeisIx n | even n    = n - m + 1
--             | otherwise = n + m
--             where m = length $ takeWhile (<= n) (oeis @2378)

-- instance OEIS 127626 where
--   oeis = tablList @127626
-- instance Table 127626 where
--   rowCol = rowCol_off @127626 @1 @1
--   rowT   = rowT_off   @127626 @1
--   tabl = map (map (\x -> if x == 0 then 0 else (oeisIx @18804) x)) (tabl @127093)

-- instance OEIS 127812 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @72594))

-- instance OEIS 128218 where
--   oeis = zipWith (-) (tail (oeis @128217)) (oeis @128217)

-- instance OEIS 128543 where
--   oeisIx = sum . (rowT @134239) . subtract 1

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

-- instance OEIS 129505 where
--   oeisIx n = abs $ (oeisIx @8275) (2 * n - 1) n

-- instance OEIS 129871 where
--   oeis = 1 : (oeis @58)

-- instance OEIS 129893 where
--   oeis = 1 : zipWith div (tail fs) fs where
--      fs = map (oeisIx @142) (oeis @124)

-- instance OEIS 130534 where
--   oeis = tablList @130534
-- instance Table 130534 where
--   tabl = map (map abs) (tabl @8275)

-- instance OEIS 130595 where
--   oeis = concat $ iterate ([-1,1] *) [1]
--   instance Num a => Num [a] where
--      fromInteger k = [fromInteger k]
--      (p:ps) + (q:qs) = p + q : ps + qs
--      ps + qs         = ps ++ qs
--      (p:ps) * qs'@ (q:qs) = p * q : ps * qs' + [p] * qs
--      _ * _               = []

-- instance OEIS 130883 where
--   oeisIx = (oeisIx @128918) . (* 2)

-- instance OEIS 130887 where
--   oeisIx = sum . map (oeisIx @225) . (rowT @27750)

-- instance OEIS 131577 where
--   oeisIx = (`div` 2) . (oeisIx @79)
--   oeis = 0 : (oeis @79)

-- instance OEIS 132090 where
--   oeisIx = (oeisIx @720) . (oeisIx @720)

-- instance OEIS 132157 where
--   oeis = (map length) (group (oeis @63882))

-- instance OEIS 132163 where
--   oeisIx_tabl = map (rowT @132163) [1..]
--   oeisIx n k = (rowT @132163) n !! (k-1)
--   oeisIx_row n = 1 : f 1 [n, n - 1 .. 2] where
--      f u vs = g vs where
--        g []                            = []
--        g (x:xs) | (oeisIx @10051 . pred) (x + u) == 1 = x : f x (delete x vs)
--                 | otherwise            = g xs

-- instance OEIS 132199 where
--   oeis = zipWith (-) (tail (oeis @106108)) (oeis @106108)

-- instance OEIS 132350 where
--   oeisIx 1 = 1
--   oeisIx n = 1 - (oeisIx @75802) n

-- instance OEIS 132431 where
--   oeisIx n = (oeisIx @60226) n - (oeisIx @62119) n + (oeisIx @2378) (n - 1)

-- instance OEIS 132995 where
--   oeis = tail $ f (oeis @40) 0 1 where
--      f (p:ps) u v = (gcd u v) : f ps (p + u) (p * v)

-- instance OEIS 133008 where
--   oeisIx n = genericLength [x | x <- takeWhile (< n) (oeis @28),
--                       n `mod` x == 0, let y = n `div` x, x < y,
--                       y `elem` takeWhile (<= n) (oeis @28)]

-- instance OEIS 133042 where
--   oeisIx = (^ 3) . (oeisIx @41)

-- instance OEIS 133466 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @57918)

-- instance OEIS 133610 where
--   oeis = scanl1 (+) (oeis @53616)

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

-- instance OEIS 134204 where
--   oeis = 2 : f 1 2 (tail (oeis @40)) where
--      f x q ps = p' : f (x + 1) p' (delete p' ps) where
--        p' = head [p | p <- ps, mod (p + q) x == 0]

-- instance OEIS 134287 where
--   oeisIx = flip (oeisIx @103371) 4 . (+ 4)

-- instance OEIS 134451 where
--   oeisIx = until (< 3) (oeisIx @53735)

-- instance OEIS 135093 where
--   oeisIx 0 = 4
--   oeisIx n = (+ 1) $ fromJust $ (`elemIndex` (oeis @46665)) $ (oeisIx @30173) n

-- instance OEIS 135282 where
--   oeisIx = (oeisIx @7814) . head . filter ((== 1) . (oeisIx @209229)) . (rowT @70165)

-- instance OEIS 135440 where
--   oeis = zipWith (-) (tail (oeis @14551)) (oeis @14551)

-- instance OEIS 135499 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @225693

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

-- instance OEIS 136119 where
--   oeis = f [1..] where
--      f zs@ (y:xs) = y : f (delete (zs !! y) xs)

-- instance OEIS 136183 where
--   oeisIx n = sum $ zipWith lcm ps $ tail ps where ps = (rowT @27750) n

-- instance OEIS 136414 where
--   oeis = zipWith (+) (tail (oeis @7376)) $ map (10 *) (oeis @7376)

-- instance OEIS 136446 where
--   oeis = map (+ 1) $ findIndices (> 1) (oeis @211111)

-- instance OEIS 136447 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @210455

-- instance OEIS 136480 where
--   oeisIx 0 = 1
--   oeisIx n = (oeisIx @7814) $ n + mod n 2

-- instance OEIS 136495 where
--   oeisIx n = (fromJust $ n `elemIndex` tail (oeis @5374)) + 1

-- instance OEIS 136655 where
--   oeisIx = product . (rowT @182469)

-- instance OEIS 137409 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @24362)

-- instance OEIS 137488 where
--   oeis = m (map (^ 24) (oeis @40)) (map (^ 4) (oeis @6881)) where
--      m xs'@ (x:xs) ys'@ (y:ys) | x < y = x : m xs ys'
--                              | otherwise = y : m xs' ys

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

-- instance OEIS 139127 where
--   oeisIx 1 = 1
--   oeisIx n = head [y | let z = (oeisIx @5244) n + 1,
--               y <- reverse $ takeWhile (<= z `div` (oeisIx @20639) z) (oeis @5244),
--               z `mod` y == 0]

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

-- instance OEIS 140480 where
--   oeis = filter
--       ((== 1) . (oeisIx @10052) . (\x -> (oeisIx @1157) x `div` (oeisIx @5) x)) (oeis @20486)

-- instance OEIS 140513 where
--   oeis = tablList @140513
--   rowCol = rowCol_off @140513 @1 @1
--   rowT   = rowT_off   @140513 @1
--   tabl = iterate (\xs@ (x:_) -> map (* 2) (x:xs)) [2]
--   oeis = concat (tabl @140513)

-- instance OEIS 141092 where
--   oeis = catMaybes $ zipWith div' (oeis @36691) (oeis @53767) where
--      div' x y | m == 0    = Just x'
--               | otherwise = Nothing where (x',m) = divMod x y

-- instance OEIS 141164 where
--   oeis = map succ $ elemIndices 1 $ map (oeisIx @188172) [1..]

-- instance OEIS 141258 where
--   oeisIx = sum . map (oeisIx @2322) . (rowT @27750)

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

-- instance OEIS 143215 where
--   oeisIx n = (oeisIx @40) n * (oeisIx @7504) n

-- instance OEIS 143344 where
--   oeis = zipWith (-) (tail (oeis @22941)) (oeis @22941)

-- instance OEIS 143667 where
--   oeis = f (oeis @3849) where
--      f (0:0:ws) = 0 : f ws; f (0:1:ws) = 1 : f ws; f (1:0:ws) = 2 : f ws

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

-- instance OEIS 144582 where
--   oeis = [x | x <- [0..], (oeisIx @30) x == (oeisIx @30) (x ^ 3)]

-- instance OEIS 144623 where
--   oeisIx = (subtract 1) . (oeisIx @78822)

-- instance OEIS 144624 where
--   oeisIx n = (oeisIx @78823) n - fi n

-- instance OEIS 144925 where
--   oeisIx = genericLength . (rowT @163870)

-- instance OEIS 145108 where
--   oeis = filter ((== 0) . (`mod` 4)) (oeis @133809)

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

-- instance OEIS 152271 where
--   oeisIx = (oeisIx @57979) . (+ 2)
--   oeis = concat $ transpose [repeat 1, [1..]]

-- instance OEIS 154691 where
--   oeis = 1 : zipWith (+)
--                      (oeis @154691) (drop 2 $ map (* 2) (oeis @45))

-- instance OEIS 155046 where
--   oeis = concat $ transpose [tail (oeis @1333), tail (oeis @129)]

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

-- instance OEIS 157104 where
--   oeisIx = (oeisIx @3415) . (oeisIx @4709)

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

-- instance OEIS 160676 where
--   oeis = filter (\x -> (oeisIx @6968) x == (oeisIx @6968) (2 * x)) [1..]

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

-- instance OEIS 161466 where
--   oeis = (rowT @27750) $ (oeisIx @142) 10

-- instance OEIS 161597 where
--   oeis = filter (\x -> (oeisIx @161594) x == x) [1..]

-- instance OEIS 161598 where
--   oeis = filter (\x -> (oeisIx @161594) x /= x) [1..]

-- instance OEIS 161600 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @161597)

-- instance OEIS 161764 where
--   oeisIx n = n - (oeisIx @199238) n

-- instance OEIS 162551 where
--   oeisIx n = (oeisIx @51601) (2 * n) n

-- instance OEIS 162643 where
--   oeis = filter ((== 0) . (oeisIx @209229) . (oeisIx @5)) [1..]

-- instance OEIS 163271 where
--   oeisIx = sum . (rowT @128966) . (subtract 1)

-- instance OEIS 163753 where
--   oeis = filter ((> 0) . (oeisIx @39997)) [0..]

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
--   oeisIx n = signum (oeisIx' n * (oeisIx' (n - 2) + (oeisIx @10051 . pred) (n + 2)))

-- instance OEIS 164296 where
--   oeisIx n = genericLength [m | let ts = (rowT @38566) n, m <- ts,
--                           all ((== 1) . gcd m) (ts \\ [m])]

-- instance OEIS 164297 where
--   oeisIx n = genericLength [m | let ts = (rowT @38566) n, m <- ts,
--                           any ((> 1) . gcd m) (ts \\ [m])]

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

-- instance OEIS 165153 where
--   oeisIx = product . (rowT @165416)

-- instance OEIS 165157 where
--   oeis = scanl (+) 0 (oeis @133622)

-- instance OEIS 165413 where
--   oeisIx = genericLength . nub . map length . group . (rowT @30308)

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

-- instance OEIS 166251 where
--   oeis = concat $ (filter ((== 1) . length)) $
--      map (filter ((== 1) . (oeisIx @10051 . pred))) $
--      zipWith enumFromTo (oeis @100484) (tail (oeis @100484))

-- instance OEIS 166474 where
--   oeis = 1 : 2 : zipWith (+)
--      (tail (oeis @166474)) (zipWith (*) (oeis @166474) $ drop 2 (oeis @217))

-- instance OEIS 166920 where
--   oeis = scanl (+) 0 (oeis @14551)

-- instance OEIS 167008 where
--   oeisIx = sum . (rowT @219206)

-- instance OEIS 167151 where
--   oeis = 0 : concat (transpose [oeis, (oeis @30124)])

-- instance OEIS 167489 where
--   oeisIx = product . map length . group . (rowT @30308)

-- instance OEIS 167535 where
--   oeis = filter ((> 0) . (oeisIx @193095)) (oeis @40)

-- instance OEIS 167700 where
--   oeisIx = p (oeis @16754) where
--      p _  0 = 1
--      p (q:qs) m = if m < q then 0 else p qs (m - q) + p qs m

-- instance OEIS 167832 where
--   b167832 n = (oeisIx @167831) n + n

-- instance OEIS 167878 where
--   oeisIx n = (oeisIx @167877) n + n

-- instance OEIS 168036 where
--   oeisIx n = (oeisIx @3415) n - n

-- instance OEIS 168223 where
--   oeisIx n = (oeisIx @6369) n - (oeisIx @6368) n

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
--                         oeisIx x == 1 || (oeisIx @10051 . pred) x == 1]

-- instance OEIS 171137 where
--   oeisIx n = head [m | m <- [1..], (oeisIx @171135) m == (oeisIx @40) n]

-- instance OEIS 171462 where
--   oeisIx n = div n p * (p - 1) where p = (oeisIx @6530) n

-- instance OEIS 171746 where
--   oeisIx = (+ 1) . length . takeWhile (== 0) .
--                              map (oeisIx @10052) . tail . iterate (oeisIx @28392)

-- instance OEIS 171862 where
--   oeisIx n = 1 + fromJust (elemIndex n (oeis @181391))

-- instance OEIS 171865 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @181391)

-- instance OEIS 171886 where
--   oeis = elemIndices 1 $ map (oeisIx @209229) $ concat (tabl @8949)

-- instance OEIS 171903 where
--   oeis = elemIndices 0 $
--                  zipWith (+) (oeis @196368) $ tail (oeis @196368)

-- instance OEIS 171904 where
--   oeisIx n = head [m | m <- (oeis @171901), (oeisIx @196368) (m + n) == 0]

-- instance OEIS 171942 where
--   oeisIx 1 = 0
--   oeisIx n = head [m | m <- [1..], (oeisIx @120) (m + n - 1) == (oeisIx @120) (n - 1)]

-- instance OEIS 171978 where
--   oeisIx n = q (fromInteger n) $ zipWith (%) [1..n] [2..] where
--      q 0 _         = 1
--      q _ []        = 0
--      q x ks'@ (k:ks)
--        | x < k     = fromEnum (x == 0)
--        | otherwise = q (x - k) ks' + q x ks

-- instance OEIS 172287 where
--   oeis = filter
--      (\p -> (oeisIx @10051 . pred) (2 * p - 3) + (oeisIx @10051 . pred) (3 * p - 2) == 1) (oeis @40)

-- instance OEIS 172471 where
--   oeisIx = (oeisIx @196) . (* 2)

-- instance OEIS 173018 where
--   oeis = tablList @173018
-- instance Table 173018 where
--   tabl = map reverse (tabl @123125)

-- instance OEIS 173517 where
--   oeisIx n = (1 - (oeisIx @10052) n) * (oeisIx @28391) n

-- instance OEIS 173525 where
--   oeisIx = (+ 1) . (oeisIx @53824) . (subtract 1)

-- instance OEIS 173557 where
--   oeisIx 1 = 1
--   oeisIx n = product $ map (subtract 1) $ (rowT @27748) n

-- instance OEIS 173694 where
--   oeis = filter ((== 1) . (oeisIx @10052) . (oeisIx @2322)) [1..]

-- instance OEIS 173732 where
--   oeis = f $ tail (oeis @25480) where f (x : _ : _ : xs) = x : f xs

-- instance OEIS 173927 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` map (+ 1) (oeis @185816))

-- instance OEIS 174332 where
--   oeisIx = (oeisIx @208238) . (oeisIx @40)

-- instance OEIS 174375 where
--   oeisIx n = n ^ 2 - (oeisIx @169810) n

-- instance OEIS 174429 where
--   oeisIx = (oeisIx @45) . (oeisIx @8908)

-- instance OEIS 174903 where
--   oeisIx n = genericLength [d | let ds = (rowT @27750) n, d <- ds,
--                       not $ null [e | e <- [d+1 .. 2*d-1] `intersect` ds]]

-- instance OEIS 174904 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` map (oeisIx @174903) [1..])

-- instance OEIS 174905 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @174903

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
--        g (q:qs) | (oeisIx @10051 . pred) (q - x + 1) == 1 = g qs
--                 | otherwise                 = q : f q qs

-- instance OEIS 175119 where
--   oeis = map (+ 1) $ zipWith (-) (tail (oeis @175118)) (oeis @175118)

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

-- instance OEIS 175872 where
--   oeisIx = f . (rowT @30308) where
--      f xs | all (== 1) xs = length xs
--           | otherwise     = f $ map genericLength $ group xs

-- instance OEIS 175911 where
--   oeisIx n = foldl1 (\v d -> b * v + d) rls where
--      b = maximum rls + 1
--      rls = (rowT @101211) n

-- instance OEIS 175944 where
--   oeis = concat $ zipWith ($) (map replicate (oeis @18252)) (oeis @18252)

-- instance OEIS 176995 where
--   oeis = filter ((> 0) . (oeisIx @230093)) [1..]

-- instance OEIS 177000 where
--   oeis = filter (all (\x -> even x || (oeisIx @10051 . pred) x == 1) .
--                          (init . (rowT @70165))) (oeis @40)

-- instance OEIS 177729 where
--   oeisIx = head . (rowT @192719)

-- instance OEIS 177904 where
--   oeis = 1 : 1 : 1 : (map (oeisIx @6530) $ zipWith (+)
--      (oeis @177904) (tail $ zipWith (+) (oeis @177904) $ tail (oeis @177904)))

-- instance OEIS 178361 where
--   oeis = [x | x <- [1..], (oeisIx @7953) x <= (oeisIx @55642) x]

-- instance OEIS 178609 where
--   oeisIx n = head [k | k <- [n - 1, n - 2 .. 0], let p2 = 2 * (oeisIx @40) n,
--                         (oeisIx @40) (n - k) + (oeisIx @40) (n + k) == p2]

-- instance OEIS 178799 where
--   oeis = zipWith (-) (tail (oeis @25487)) (oeis @25487)

-- instance OEIS 178953 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @178609

-- instance OEIS 179242 where
--   oeis = concatMap h $ drop 3 $ inits $ drop 2 (oeis @45) where
--      h is = reverse $ map (+ f) fs where
--             (f:_:fs) = reverse is

-- instance OEIS 179248 where
--   oeis = map succ $ elemIndices 9 $ tail $ oeis @7895

-- instance OEIS 179249 where
--   oeis = map succ $ elemIndices 9 $ tail $ oeis @7895

-- instance OEIS 179250 where
--   oeis = map succ $ elemIndices 10 $ tail $ oeis @7895

-- instance OEIS 179251 where
--   oeis = map succ $ elemIndices 11 $ tail $ oeis @7895

-- instance OEIS 179253 where
--   oeis = map succ $ elemIndices 13 $ tail $ oeis @7895

-- instance OEIS 179627 where
--   oeisIx = (+ 1) . (oeisIx @6666) . (oeisIx @40)

-- instance OEIS 180058 where
--   oeisIx = (+ 2) . fromJust . (`elemIndex` (oeis @59233))

-- instance OEIS 180077 where
--   oeisIx = fromJust . (`elemIndex` (oeis @180076))

-- instance OEIS 180110 where
--   oeis = map (+ 2) $ elemIndices True $ zipWith (&&) zs (tail zs)
--      where zs = zipWith (<) (oeis @180076) (tail (oeis @180076))

-- instance OEIS 180191 where
--   oeisIx n = if n == 1 then 0 else sum $ (rowT @116853) (n - 1)

-- instance OEIS 180197 where
--   oeis = f 1 where
--      f x = if length ps == 3 && nub ps == ps
--            then (2 ^ (ps!!0 * ps!!1) `mod` ps!!2) : f (x+2) else f (x+2)
--            where ps = (rowT @27746) x

-- instance OEIS 180663 where
--   oeis = tablList @180663
-- instance Table 180663 where
--   tabl = map reverse (tabl @180662)

-- instance OEIS 180853 where
--   oeis = iterate (oeisIx @6368) 4

-- instance OEIS 180864 where
--   oeis = iterate (oeisIx @6368) 13

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
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @81118)

-- instance OEIS 181819 where
--   oeisIx = product . map (oeisIx @40) . (rowT @124010) . succ

-- instance OEIS 181894 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ (rowT @213925) n

-- instance OEIS 181921 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @78350))

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

-- instance OEIS 182147 where
--   oeis = [w | w <- [1..] , sum (dropWhile (<= (oeisIx @196) w) $
--                                         (rowT @27751) $ fromInteger w) == w]

-- instance OEIS 182205 where
--   oeis = iterate (oeisIx @6368) 40

-- instance OEIS 182237 where
--   oeis = map (+ 2 ) $ elemIndices 2 (oeis @59233)

-- instance OEIS 182324 where
--   oeisIx n = n + (oeisIx @30) n [0]

-- instance OEIS 182402 where
--   oeis = map (sum . map (oeisIx @55642)) $ t 1 [1..] where
--      t i xs = ys : t (i + 1) zs where
--        (ys, zs) = splitAt i xs

-- instance OEIS 182426 where
--   oeis = concatMap f $ group $ zipWith (-) (tail ips) ips where
--      f xs | head xs == 1 = reverse $ enumFromTo 2 $ length xs + 1
--           | otherwise    = take (length xs) $ repeat 1
--      ips = map (oeisIx @49084) (oeis @166251)

-- instance OEIS 182472 where
--   oeisIx = fromJust . (`elemIndex` (oeis @182458))

-- instance OEIS 182584 where
--   oeisIx n = (oeisIx @182579) (2*n) n

-- instance OEIS 182834 where
--   oeisIx n = (oeisIx @196) (2 * n - 2) + n

-- instance OEIS 182850 where
--   oeisIx n = genericLength $ takeWhile (`notElem` [1,2]) $ iterate (oeisIx @181819) n

-- instance OEIS 182991 where
--   oeis = filter f [1..] where
--      f x = all (== 1) $ zipWith (+) dps $ tail dps where
--            dps = map (flip mod 2) $ (rowT @27750)' x

-- instance OEIS 183063 where
--   oeisIx = sum . map (1 -) . (rowT @247795)

-- instance OEIS 183091 where
--   oeisIx = product . (rowT @210208)

-- instance OEIS 183168 where
--   oeisIx n = z (drop (fromInteger (mod n 2)) (oeis @40)) (n ^ 2) 3 where
--      z _      m 1 = if m <= 0 then 0 else (oeisIx @10051 . pred) m
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

-- instance OEIS 185024 where
--   oeis = map (+ 2 ) $ elemIndices 1 (oeis @59233)

-- instance OEIS 185038 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @90895)

-- instance OEIS 185080 where
--   oeisIx (fi->n) = fi $ 6 * (oeisIx @7318) (2 * n) (n - 1) + (oeisIx @7318) (2 * n - 1) n

-- instance OEIS 185086 where
--   oeis = filter (\p -> any ((== 1) . (oeisIx @10052)) $
--                  map (p -) $ takeWhile (<= p) (oeis @1248)) (oeis @40)

-- instance OEIS 185154 where
--   oeis = catMaybes $ map f (oeis @6093) where
--      f x = g $ takeWhile (< x) (oeis @65091) where
--        g []  = Nothing
--        g [_] = Nothing
--        g (p:ps@ (_:qs)) | (x - p) `elem` qs = Just p
--                        | otherwise         = g ps

-- instance OEIS 185208 where
--   oeis =  (map succ $ elemIndices 1 $ tail $ oeis @141197)

-- instance OEIS 185212 where
--   oeisIx = (+ 1) . (* 4) . (oeisIx @567)

-- instance OEIS 185242 where
--   oeis = iterate (oeisIx @203907) 3

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

-- instance OEIS 186253 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @261301

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
--   oeis = map (+ 1) $ elemIndices 1 (oeis @186711)

-- instance OEIS 187059 where
--   oeisIx = (oeisIx @7814) . (oeisIx @1142)

-- instance OEIS 187085 where
--   oeisIx  n = (oeis @187085) !! (n - 1)
--   oeis = zipWith (+) (oeis @187072) $ tail (oeis @187072)

-- instance OEIS 187090 where
--   oeisIx n = until ((== 9) . (oeisIx @30)) (+ n) n

-- instance OEIS 187098 where
--   oeis = 1 : 2 : map (`div` 2) (oeis @187085)

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

-- instance OEIS 188069 where
--   oeis = (map succ $ elemIndices 2 $ tail $ oeis @7538)
--
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

-- instance OEIS 188264 where
--   oeis =
--      map (+ 1) $ elemIndices 0 $ zipWith mod [1..] $ map (oeisIx @66459) [1..]

-- instance OEIS 188654 where
--   oeis = map (+ 1) $ findIndices (/= 0) (oeis @225230)

-- instance OEIS 188666 where
--   oeis = g 1 (oeis @961) where
--      g n pps'@ (pp:pp':pps) | n < 2*pp  = pp  : g (n+1) pps'
--                            | otherwise = pp' : g (n+1) (pp':pps)
--   oeisIx' n = last $ elemIndices (f 1) $ map f [0..n] where
--      f from = foldl lcm 1 [from..n]

-- instance OEIS 188916 where
--   oeis = filter ((== 1) . (oeisIx @10052). (oeisIx @188915)) [0..]

-- instance OEIS 188917 where
--   oeis = filter ((== 1) . (oeisIx @209229). (oeisIx @188915)) [0..]

-- instance OEIS 188968 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @188967

-- instance OEIS 188969 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @188967

-- instance OEIS 189056 where
--   oeis = 0 : filter (\x -> (oeisIx @258682) x /= x ^ 2) [1..]

-- instance OEIS 189419 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @114183))

-- instance OEIS 189639 where
--   oeisIx n = (oeis @189710) !! (n - 1)
--   oeis = elemIndices 0 $
--      zipWith (-) (map (oeisIx @3415) (oeis @3415)) (map pred (oeis @3415))

-- instance OEIS 189710 where
--   oeis = elemIndices 0 $
--      zipWith (-) (map (oeisIx @3415) (oeis @3415)) (map pred (oeis @3415))

-- instance OEIS 189835 where
--   oeisIx n = (oeisIx @1157) n - (oeisIx @38040) n

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

-- instance OEIS 190127 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190126))) [1..10000]

-- instance OEIS 190129 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190128))) [1..10000]

-- instance OEIS 190131 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190130))) [1..10000]

-- instance OEIS 190133 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190132))) [1..10000]

-- instance OEIS 190135 where
--   oeis =
--      map (succ . fromJust . (`elemIndex` (oeis @190134))) [1..10000]

-- instance OEIS 190136 where
--   oeisIx n = maximum $ map (oeisIx @6530) [n..n+3]

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

-- instance OEIS 190944 where
--   oeisIx = (oeisIx @7088) . (* 3)

-- instance OEIS 191292 where
--   oeis = f (oeis @31443) where
--      f (x:x':xs) | x' == x+2 = (x+1) : f xs
--                  | otherwise = f (x':xs)

-- instance OEIS 191610 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ takeWhile (> 0) $ map ((n - 1) `div`) (oeis @351)

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

-- instance OEIS 192503 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @3309)

-- instance OEIS 192504 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @3309)

-- instance OEIS 192505 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @192607)

-- instance OEIS 192506 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @192607)

-- instance OEIS 192512 where
--   oeis = scanl1 (+) $ map (oeisIx @192490) [1..]

-- instance OEIS 192607 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @192490

-- instance OEIS 192817 where
--   oeis = [x | x <- [1..], gcd x (oeisIx @61601 x) == 1]

-- instance OEIS 192849 where
--   oeisIx n = if n < 3 then 0 else (oeisIx @245334) (n + 1) 4



-- instance OEIS 192977 where
--   oeis = f 0 $ group (oeis @68395) where
--      f n xss'@ (xs:xss)
--        | head xs `div` 9 == n = length xs : f (n+1) xss
--        | otherwise            = 0 : f (n+1) xss'

-- instance OEIS 192993 where
--   oeis = findIndices (> 1) $ map (oeisIx @193095) [0..]

-- instance OEIS 193096 where
--   oeis = elemIndices 0 $ map (oeisIx @193095) [0..]

-- instance OEIS 193097 where
--   oeis = elemIndices 1 $ map (oeisIx @193095) [0..]

-- instance OEIS 193159 where
--   oeis = map (+ 1) $ findIndices (<= 3) (oeis @50430)

-- instance OEIS 193166 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @192280

-- instance OEIS 193169 where
--   oeisIx = genericLength . filter odd . (rowT @27750) . (oeisIx @2322)

-- instance OEIS 193213 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @99267)

-- instance OEIS 193422 where
--   oeis = map (fromJust . (`elemIndex` (oeis @193358))) [1..]

-- instance OEIS 193428 where
--   oeisIx n = sum $ map ($ n) [oeisIx, (oeisIx @31288), (oeisIx @31289), (oeisIx @31290), (oeisIx @31291), (oeisIx @31292), (oeisIx @31293), (oeisIx @31294), (oeisIx @31295), (oeisIx @31296)]



-- instance OEIS 193460 where
--   oeis = elemIndices 0 $ 1 : zipWith (-) (oeis @193459) (oeis @5)

-- instance OEIS 193496 where
--   oeisIx = fromEnum . (>= 0) . (oeisIx @95916)

-- instance OEIS 193574 where
--   oeisIx n = head [d | d <- [1..sigma] \\ nDivisors, mod sigma d == 0]
--      where nDivisors = (rowT @27750) n
--            sigma = sum nDivisors



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

-- instance OEIS 193927 where
--   oeis = findIndices (< 0) (oeis @193926)

-- instance OEIS 193928 where
--   oeis = findIndices (0 <=) (oeis @193926)

-- instance OEIS 194081 where
--   oeis = map (fromJust . (`elemIndex` (oeis @5375))) [0..]

-- instance OEIS 194187 where
--   oeis = zipWith (-) (oeis @40) (oeis @70883)

-- instance OEIS 194189 where
--   oeisIx n = sum $ map (oeisIx @10051 . pred) [n* (n+1) `div` 2 + 1 .. n^2 - 1]

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
--   oeis = map succ $ elemIndices 10 $ tail $ oeis @46660

-- instance OEIS 195085 where
--   oeis = map (+ 1) $ elemIndices 2 (oeis @57918)

-- instance OEIS 195086 where
--   oeis = map succ $ elemIndices 2 $ tail $ oeis @46660

-- instance OEIS 195087 where
--   oeis = map succ $ elemIndices 3 $ tail $ oeis @46660

-- instance OEIS 195088 where
--   oeis = map succ $ elemIndices 4 $ tail $ oeis @46660

-- instance OEIS 195089 where
--   oeis = map succ $ elemIndices 5 $ tail $ oeis @46660

-- instance OEIS 195090 where
--   oeis = map succ $ elemIndices 6 $ tail $ oeis @46660

-- instance OEIS 195091 where
--   oeis = map succ $ elemIndices 7 $ tail $ oeis @46660

-- instance OEIS 195092 where
--   oeis = map succ $ elemIndices 8 $ tail $ oeis @46660

-- instance OEIS 195093 where
--   oeis = map succ $ elemIndices 9 $ tail $ oeis @46660

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

-- instance OEIS 195324 where
--   oeis = filter p [2,4..] where
--      p n = all ((== 0) . (oeisIx @10051 . pred)) $ takeWhile (> 1) $ map (n -) (oeis @5385)

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

-- instance OEIS 196149 where
--   oeis = filter f [1..] where
--      f n = all (<= 0) $ zipWith (-) (tail divs) (map (* 3) divs)
--                         where divs = (rowT @27750)' n

-- instance OEIS 196175 where
--   oeis = map (+ 2) $ elemIndices True $
--      zipWith (\x y -> x < 0 && y > 0) (oeis @36263) $ tail (oeis @36263)

-- instance OEIS 196276 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @196274)

-- instance OEIS 196277 where
--   oeis = map (+ 1) $ findIndices (> 1) (oeis @196274)



-- instance OEIS 196415 where
--   oeis =
--      map (+ 1) $ elemIndices 0 $ zipWith mod (oeis @36691) (oeis @53767)

-- instance OEIS 196526 where
--   oeisIx n = genericLength [c | let p = (oeisIx @40) n,
--                           c <- [-1,1..p-1], let b = p - c,
--                           gcd b c == 1,
--                           (oeisIx @6530) b ^ 2 < p || p == 3, (oeisIx @6530) c ^ 2 < p]

-- instance OEIS 196871 where
--   oeis = filter
--      (all (== 0) . map (oeisIx @10051 . pred) . takeWhile (> 2) . iterate (oeisIx @6370)) [1..]

-- instance OEIS 197182 where
--   oeisIx = (oeisIx @64986) . (oeisIx @290)

-- instance OEIS 197183 where
--   oeisIx = (oeisIx @115944) . (oeisIx @290)

-- instance OEIS 197877 where
--   oeis = map (fromJust . (`elemIndex` (oeis @96535))) [0..]

-- instance OEIS 197911 where
--   oeis = scanl (+) 0 (oeis @56832)

-- instance OEIS 198273 where
--   oeis = map (oeisIx @40) $ (map succ $ elemIndices 0 $ tail $ oeis @67432)

-- instance OEIS 198277 where
--   oeisIx n = (oeisIx @40) . (+ 1) . fromJust $ elemIndex n (oeis @67432)

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

-- instance OEIS 199771 where
--   oeisIx  = sum . (rowT @199332)

-- instance OEIS 199968 where
--   oeisIx = head . (rowT @173540)

-- instance OEIS 200087 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @79878)) + 1

-- instance OEIS 200379 where
--   oeisIx n = (tabl @56230) !! n !! (n - 1)

-- instance OEIS 200723 where
--   oeisIx = sum . zipWith (*) [1..] . map (oeisIx @63524) . (rowT @165430)

-- instance OEIS 200738 where
--   oeis = f (tabl @200737) where
--      f (rs:rss'@ (rs':rss)) =
--        (length $ takeWhile (== EQ) $ zipWith compare rs rs') : f rss'

-- instance OEIS 200742 where
--   oeis = f (tabl @200741) where
--      f (rs:rss'@ (rs':rss)) =
--        (length $ takeWhile (== EQ) $ zipWith compare rs rs') : f rss'

-- instance OEIS 201053 where
--   oeis = 0 : concatMap (\x -> genericReplicate (oeisIx @56107 x) (x ^ 3)) [1..]

-- instance OEIS 201217 where
--   oeis = elemIndices 0 (oeis @61023)

-- instance OEIS 61023 where
--   oeisIx n = abs (oeisIx @53187 n - oeisIx @201053 n)

-- instance OEIS 201462 where
--   oeis = [x | x <- [1..], gcd x (oeisIx @61601 x) > 1]

-- instance OEIS 202014 where
--   oeisIx n = (fromJust $ elemIndex n (oeis @63882)) + 1

-- instance OEIS 202016 where
--   oeis = map (+ 1) $ elemIndices 1 (oeis @132157)

-- instance OEIS 202022 where
--   oeisIx = fromEnum . (== 1) . (oeisIx @43537)

-- instance OEIS 202262 where
--   oeis = [4,6,8,9] ++ [x | u <- (oeis @202262), v <- [4,6,8,9],
--                          let x = 10 * u + v, v /= 9 || (oeisIx @10051 . pred) x == 0]

-- instance OEIS 202337 where
--   oeis = f (oeis @62723) where
--      f (x:xs'@ (x':xs)) = if x == x' then f xs' else x : f xs'

-- instance OEIS 202822 where
--   oeis = filter ((== 1) . flip mod 3) (oeis @3136)

-- instance OEIS 203069 where
--   oeis = 1 : f 1 [2..] where
--      f u vs = g vs where
--        g (w:ws) | odd z && (oeisIx @10051 . pred) z == 0 = w : f w (delete w vs)
--                 | otherwise = g ws
--                 where z = u + w

-- instance OEIS 203814 where
--   oeisIx n = genericLength [x | x <- [0..n], (oeisIx @43537) x == (oeisIx @43537) n]

-- instance OEIS 203967 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @9087)

-- instance OEIS 204093 where
--   oeis = map (* 6) (oeis @7088)

-- instance OEIS 204094 where
--   oeis = map (* 7) (oeis @7088)

-- instance OEIS 204095 where
--   oeis = map (* 8) (oeis @7088)

-- instance OEIS 204138 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ drop 6 (oeis @1945)

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

-- instance OEIS 205666 where
--   oeis = [x | x <- [1..], (oeisIx @65306) x == fi x]

-- instance OEIS 205745 where
--   oeisIx n = sum $ map ((`mod` 2) . (n `div`))
--      [p | p <- takeWhile (<= n) (oeis @40), n `mod` p == 0]

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

-- instance OEIS 206332 where
--   oeis = compl [1..] (oeis @92754) where
--      compl (u:us) vs'@ (v:vs) | u == v = compl us vs
--                              | u /= v = u : compl us vs'

-- instance OEIS 206368 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @206475)

-- instance OEIS 206475 where
--   oeis = zipWith (-) (tail (oeis @206369)) (oeis @206369)

-- instance OEIS 206498 where
--   oeisIx 1 = 0
--   oeisIx 2 = 2
--   oeisIx x = if t > 0 then (oeisIx @196062) t + t `mod` 2 else (oeisIx @196062) x
--               where t = (oeisIx @49084) x

-- instance OEIS 206553 where
--   oeisIx n = head [p | p <- drop 2 (oeis @40),
--                         (oeisIx @10051 . pred) (2^n + p*2^ (div (n+1) 2) - 1) == 1]

-- instance OEIS 206787 where
--   oeisIx = sum . filter odd . (rowT @206778)

-- instance OEIS 206941 where
--   oeisIx = (oeisIx @10) . (oeisIx @2322)

-- instance OEIS 207193 where
--   oeisIx 1 = 1
--   oeisIx n | p == 2 && e > 2 = 2 ^ (e - 2)
--             | otherwise       = (p - 1) * p ^ (e - 1)
--             where p = (oeisIx @25473) n; e = (oeisIx @25474) n

-- instance OEIS 207337 where
--   oeis = f (oeis @2522) where
--      f (x:xs) | m == 0 && (oeisIx @10051 . pred) y == 1 = y : f xs
--               | otherwise                = f xs
--               where (y,m) = divMod x 10

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
--   oeisIx = sum . map (oeisIx @10051 . pred) . (rowT @81118)

-- instance OEIS 208091 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @208083))

-- instance OEIS 208131 where
--   oeis = scanl (*) 1 $ (oeis @52901)

-- instance OEIS 208134 where
--   oeisIx = genericLength . filter (== 0) . (rowT @8975)

-- instance OEIS 208177 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) [1,129..]

-- instance OEIS 208178 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) [1,257..]

-- instance OEIS 208238 where
--   oeisIx = genericIndex (oeis @208238)
--   oeis = f nns $ filter ((== 1) . (oeisIx @10051 . pred) . fst) nns where
--      f mms'@ ((m,ms):mms) pps'@ ((p,ps):pps) =
--        if m == p then f mms' pps else q : f mms pps'
--        where q = fst $ fromJust $ find ((ms `isInfixOf`) . snd) pps'
--      nns = zip [0..] (tabf @30308)

-- instance OEIS 208247 where
--   oeisIx n = (oeis @95841) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @71330)) (oeis @961)

-- instance OEIS 208259 where
--   oeis = 1 : map ((+ 1) . (* 10)) (oeis @131835)

-- instance OEIS 208260 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @208259)

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
--      let q = 2 * m - p, (oeisIx @10051 . pred) q == 1,
--      all ((== 0) . (oeisIx @10051 . pred)) $ map (2 * m -) $ take (n - 1) (oeis @65091)]

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

-- instance OEIS 209211 where
--   oeis = filter (\x -> (x - 1) `gcd` (oeisIx @10) x == 1) [1..]

-- instance OEIS 210454 where
--   oeisIx = (`div` 3) . (subtract 1) . (4 ^) . (oeisIx @40) . (+ 2)

-- instance OEIS 210481 where
--   oeisIx n = sum [oeisIx' $ p * q - 2 |
--                    let p = (oeisIx @40) n, q <- takeWhile (< p) (oeis @40)]

-- instance OEIS 210490 where
--   oeis = filter chi [1..] where
--      chi x = all (== 1) es || all even es where es = (rowT @124010) x

-- instance OEIS 210719 where
--   oeis = f (zip [1..] (oeis @10)) [] where
--      f ((i,x):ixs) phis | x `elem` phis = f ixs phis
--                         | otherwise     = i : f ixs (x : phis)

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

-- instance OEIS 211890 where
--   oeis = tablList @211890
-- instance Table 211890 where
--   tabl = zipWith3 (\p k row -> map ((+ p) . (* k)) row)
--                           (oeis @8578) (0 : (oeis @211889)) (tabl @2262)

-- instance OEIS 211996 where
--   oeisIx n = genericLength [x | x <- [1..n], let (y, m) = divMod n x,
--                           m == 0, (oeisIx @10052) (x + y) == 1]

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

-- instance OEIS 213684 where
--   oeisIx n = (oeisIx @155161) (2*n) n

-- instance OEIS 213911 where
--   oeisIx = genericLength . filter ((== 0) . head) . group . (rowT @213676)

-- instance OEIS 213912 where
--   oeis = 1 : f [1] where
--      f xs@ (x:_) = y : f (y : xs) where
--        y = if z `notElem` xs then z else 3 * x where z = (oeisIx @196) x

-- instance OEIS 213913 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @213912))

-- instance OEIS 214295 where
--   oeisIx n = (oeisIx @10052) n - (oeisIx @10052) (3*n)

-- instance OEIS 214320 where
--   oeis = 1 : 1 : 1 : (map (oeisIx @6530) $
--      zipWith (+) (oeis @214320) (drop 2 $ (oeis @214320)))

-- instance OEIS 214360 where
--   oeis = [x | k <- [0..], let x = 3120613860*k+23, (oeisIx @10051 . pred) x == 1]

-- instance OEIS 214433 where
--   oeis = [x | x <- [0..], (oeisIx @105025) x == (oeisIx @105027) x]

-- instance OEIS 214489 where
--   oeis = [x | x <- [0..], (oeisIx @70939) x == (oeisIx @103586) x]

-- instance OEIS 214511 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @45698))

-- instance OEIS 214546 where
--   oeis = zipWith (-) (tail (oeis @140472)) (oeis @140472)

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
--      p i k2 x = x <= k2 || (gcd k2 x > 1 || (oeisIx @10051 . pred) (x - k2) == 1) &&
--                            p (i + 2) (k2 + i) x

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

-- instance OEIS 217398 where
--   oeis = map succ $ elemIndices 5 $ tail $ oeis @30

-- instance OEIS 217575 where
--   oeisIx = subtract 1 . (oeisIx @63657)

-- instance OEIS 217659 where
--   oeisIx = (oeisIx @151800) . fromInteger . (oeisIx @185934)

-- instance OEIS 217863 where
--   oeisIx = (oeisIx @10) . (oeisIx @3418)

-- instance OEIS 217921 where
--   oeisIx n = fst $ until (all (== 1) . snd) f (0, (rowT @30308) n) where
--      f (i, xs)  = (i + 1, map genericLength $ group xs)

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

-- instance OEIS 219244 where
--   oeis = map (`div`  6) $ zipWith (-) (oeis @217659) (oeis @185934)

-- instance OEIS 219462 where
--   oeisIx = sum . zipWith (*) (oeis @1906) . (rowT @34870)

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
--   oeisIx = foldr (\u v-> 2*v + u) 0 . (rowT @219463)

-- instance OEIS 219908 where
--   oeisIx n = (oeis @219907) !! (n - 1)
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @219907)

-- instance OEIS 219922 where
--   oeisIx n = (fromJust $ findIndex (n `elem`) (tabl @26835)) + 1

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

-- instance OEIS 220654 where
--   oeisIx = fromJust . (`elemIndex` (oeis @215244))

-- instance OEIS 220812 where
--   oeisIx = (oeisIx @11557) . (oeisIx @79)

-- instance OEIS 221264 where
--   oeis = filter ((< 0) . (oeisIx @5094)) [1..] is

-- instance OEIS 222208 where
--   oeis = 1 : 3 : f 3 (2 : [4 ..]) where
--      f u vs = g vs where
--        g (w:ws) = if all (== 0) $ map ((mod w) . (oeisIx @222208)) $ (rowT @27751) u
--                      then w : f (u + 1) (delete w vs) else g ws

-- instance OEIS 222209 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @222208))

-- instance OEIS 222493 where
--   oeisIx = (oeisIx @133500) . (oeisIx @221221)

-- instance OEIS 222581 where
--   oeis = map length $ group (oeis @93796)

-- instance OEIS 222622 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @113966))

-- instance OEIS 222623 where
--   oeis = filter (\x -> (oeisIx @113966) x == x) [1..]

-- instance OEIS 223456 where
--   oeis = filter ((== 1 ) . (oeisIx @10051 . pred) . (oeisIx @32741) . (oeisIx @32741)) (oeis @2808)

-- instance OEIS 223490 where
--   oeisIx = head . (rowT @213925)

-- instance OEIS 223491 where
--   oeisIx = last . (rowT @213925)

-- instance OEIS 224076 where
--   oeisIx = genericLength . (rowT @224075)

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

-- instance OEIS 224839 where
--   oeis = f [1..] [] where
--      f (x:xs) ys = if all ((== 0) . (oeisIx @10052)) $ map (x -) ys
--                       then x : f xs (x:ys) else f xs ys

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
--      zipWith ((*) `on` (oeisIx @10051 . pred)) (oeis @2522) (oeis @8865)

-- instance OEIS 225105 where
--   oeis = filter
--      ((== 1) . (oeisIx @10051 . pred) . maximum . filter odd . (rowT @70165)) (oeis @5408)

-- instance OEIS 225124 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @25586)) . (oeisIx @79)

-- instance OEIS 225126 where
--   oeisIx n = (oeisIx @48152) (2 * n - 1)  n

-- instance OEIS 225228 where
--   oeis = filter f [1..] where
--      f x = length es == 3 && sum es `elem` [3,5,7] &&
--                              maximum es - minimum es <= 1
--            where es = (rowT @124010) x

-- instance OEIS 225353 where
--   oeis = map (+ 1) $ elemIndices 0 (oeis @225245)

-- instance OEIS 225354 where
--   oeis = map (+ 1) $ findIndices (> 0) (oeis @225245)

-- instance OEIS 225395 where
--   oeisIx n = product $ zipWith (^)
--       (map (oeisIx @49084) $ (rowT @27748) n) (map (oeisIx @225395) $ (rowT @124010) n)

-- instance OEIS 225481 where
--   oeisIx n = product [p | p <- takeWhile (<= n + 1) (oeis @40),
--                            mod n (p - 1) == 0 || mod (n + 1) p == 0]

-- instance OEIS 225589 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @217122))

-- instance OEIS 225621 where
--   oeisIx n = (oeisIx @74911) (2 * n - 1) n

-- instance OEIS 225793 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @230093

-- instance OEIS 225840 where
--   oeisIx (succ->n) = maximum $ filter (< n) $ (rowT @70165) n

-- instance OEIS 225850 where
--   oeisIx = fromJust . (`elemIndex` (oeis @167151))

-- instance OEIS 226025 where
--   oeis = filter ((/= 2) . (oeisIx @100995)) (oeis @71904)

-- instance OEIS 226029 where
--   oeis = zipWith (-) (tail (oeis @182402)) (oeis @182402)

-- instance OEIS 226030 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226029))

-- instance OEIS 226047 where
--   oeisIx = maximum . (rowT @226078)

-- instance OEIS 226244 where
--   (oeis, (oeis @226245)) = unzip $ (1,1) : f 1 1 (oeis @5185) where
--      f i v (q:qs) | q > v = (q,i) : f (i + 1) q qs
--                   | otherwise = f (i + 1) v qs

-- instance OEIS 226245 where

-- instance OEIS 226387 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @85612))

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

-- instance OEIS 226569 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @226532))

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

-- instance OEIS 226778 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @55483

-- instance OEIS 226898 where
--   oeisIx = maximum . map length .
--      map (\ds@ (d:_) -> takeWhile (<= e' d) ds) . init . tails . (rowT @27750)
--      where e' = floor . (* e) . fi; e = exp 1

-- instance OEIS 226946 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @86

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

-- instance OEIS 227455 where
--   oeis = 1 : f [2..] [1] where
--      f (v:vs) ws = if any (`notElem` ws) $ map (subtract 1) $ (rowT @27748) v
--                       then v : f vs (v : ws) else f vs ws

-- instance OEIS 227481 where
--   oeisIx = sum . map (oeisIx @10052) . (rowT @69011)

-- instance OEIS 227632 where
--   (oeis, (oeis @227633)) = unzip $ (1,1) : f 1 1 (oeis @227617) where
--      f i v (q:qs) | q > v = (q,i) : f (i + 1) q qs
--                   | otherwise = f (i + 1) v qs

-- instance OEIS 227633 where

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

-- instance OEIS 227876 where
--   oeisIx n = fst $ until (null . snd) h (0, (rowT @31298) n) where
--               h (s, ds) = (s + sum ds, map abs $ zipWith (-) ds $ tail ds)

-- instance OEIS 227878 where
--   oeis = f (oeis @51701) where
--      f (p:ps@ (_:p':_)) = if p == p' then p : f ps else f ps

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

-- instance OEIS 228057 where
--   oeis = filter odd (oeis @228056)

-- instance OEIS 228078 where
--   oeisIx = subtract 1 . (oeisIx @99036)

-- instance OEIS 228446 where
--   oeisIx (succ->n) = head
--      [q | let m = 2 * n + 1,
--           q <- map (m -) $ reverse $ takeWhile (< m) $ tail (oeis @2378),
--           oeisIx @10051 q == 1]

-- instance OEIS 230097 where
--   oeis = 0 : f 0 0 where
--      f i m = if v > m then i : f (i + 1) v else f (i + 1) m
--              where v = (oeisIx @159918) i

-- instance OEIS 230116 where
--   oeisIx = foldr (\u v-> 2*v + u) 0 . (rowT @166360)

-- instance OEIS 230286 where
--   oeisIx = (flip div 3) . (oeisIx @16052)

-- instance OEIS 230287 where
--   oeis = zipWith (-) (tail (oeis @230286)) (oeis @230286)

-- instance OEIS 230504 where
--   oeisIx n = head $ filter ((== 1) . (oeisIx @10051 . pred)) rs where
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

-- instance OEIS 232643 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @232642))

-- instance OEIS 233281 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @1177)) [1..]

-- instance OEIS 233734 where
--   oeisIx n = (oeisIx @19538) (2 * n - 1) n

-- instance OEIS 233836 where
--   oeis = map length $ group (oeis @4539)

-- instance OEIS 234098 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) $
--                         map ((flip div 2) . (+ 1)) (oeis @46388)

-- instance OEIS 234324 where
--   oeisIx n = (oeisIx @8955) (2 * n) n

-- instance OEIS 234587 where

-- instance OEIS 234814 where
--   oeis = filter (\x -> x `mod` (oeisIx @7953) x == 0 &&
--                                x `mod` (oeisIx @10888) x /= 0) [1..]

-- instance OEIS 235224 where
--   oeisIx n = genericLength $ takeWhile (<= n) (oeis @2110)

-- instance OEIS 235249 where
--   oeisIx n = if y == n then n else (oeisIx @235249) y  where y = (oeisIx @1175) n

-- instance OEIS 235540 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @158034)

-- instance OEIS 235775 where
--   oeisIx = (oeisIx @47842) . (oeisIx @47842)

-- instance OEIS 235991 where
--   oeis = filter (odd . (oeisIx @3415)) [0..]

-- instance OEIS 235992 where
--   oeis = filter (even . (oeisIx @3415)) [0..]

-- instance OEIS 236246 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @229037

-- instance OEIS 236341 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @160855))

-- instance OEIS 236473 where
--   oeisIx = p (oeis @7422) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 237056 where
--   oeis = concat $ transpose [oeis, (oeis @192607)]

-- instance OEIS 237058 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @237056))

-- instance OEIS 237347 where
--   oeis = zipWith (-) (tail (oeis @78633)) (oeis @78633)

-- instance OEIS 237427 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @237126))

-- instance OEIS 237709 where
--   oeis = map length $ group (oeis @188666)

-- instance OEIS 237739 where
--   oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @71574))

-- instance OEIS 237860 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @237851))

-- instance OEIS 238246 where
--   oeis = map succ $ elemIndices 3 $ tail $ oeis @72219

-- instance OEIS 238247 where
--   oeis = map succ $ elemIndices 5 $ tail $ oeis @72219

-- instance OEIS 238248 where
--   oeis = map succ $ elemIndices 7 $ tail $ oeis @72219

-- instance OEIS 238327 where
--   oeis = iterate ((+ 2) . (oeisIx @151800)) 1

-- instance OEIS 238525 where
--   oeisIx n = mod n $ (oeisIx @1414) n

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
--   oeisIx n = sum $ filter ((== 1) . (oeisIx @10051 . pred)) $
--      map (2 * n -) $ takeWhile (<= 2 * n) (oeis @40)

-- instance OEIS 238862 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @239965))

-- instance OEIS 239070 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @132995)) . (oeisIx @5117)

-- instance OEIS 239122 where
--   oeis = drop 2 $ scanl1 (+) (oeis @61019)

-- instance OEIS 239324 where
--   oeis = scanl (+) 0 (oeis @90431)

-- instance OEIS 239433 where
--   oeis = filter
--      (\z -> any (== z) $ map (oeisIx @3415) $ takeWhile (<= (oeisIx @2620) z) (oeis @13929)) [2..]

-- instance OEIS 239508 where
--   oeisIx = p (oeis @469) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 239509 where
--   oeisIx = p (oeis @469) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

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

-- instance OEIS 239673 where
--   (oeis, (oeis @239674)) = unzip $ (12, 1) : f 1 12 (oeis @239656) where
--      f i v (q:qs) | q > v = (q, i) : f (i + 1) q qs
--                   | otherwise = f (i + 1) v qs

-- instance OEIS 239740 where
--   oeisIx n = gcd (sum fs) (product fs)
--               where fs = take n $ tail (oeis @45)

-- instance OEIS 239826 where
--   oeisIx n = sum $
--               filter ((flip isPrefixOf `on` (rowT @30308)) n) $ (rowT @27750) n

-- instance OEIS 239930 where
--   oeisIx = sum . map (oeisIx @240025) . (rowT @27750)

-- instance OEIS 239943 where
--   oeis = [x | x <- [1..], (oeisIx @239965) x == x]

-- instance OEIS 239968 where
--   oeis = unfoldr c (1, 1, (oeis @18252)) where
--      c (i, z, xs'@ (x:xs)) | i == x = Just (z, (i + 1, z + 1, xs))
--                           | i /= x = Just (0, (i + 1, z, xs'))

-- instance OEIS 240052 where
--   oeisIx = (oeisIx @68346) . (oeisIx @6094)

-- instance OEIS 240277 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @7456))

-- instance OEIS 240400 where
--   oeis = filter ((> 0) . (oeisIx @241759)) [0..]

-- instance OEIS 240508 where
--   oeisIx = genericLength . (rowT @174382)

-- instance OEIS 240844 where
--   oeisIx = p $ drop 3 (oeis @73) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 240883 where
--   oeisIx n = (oeisIx @240857) (2 * n) n

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

-- instance OEIS 241418 where
--   oeis = zipWith (-) (tail (oeis @99054)) (oeis @99054)

-- instance OEIS 241582 where
--   (oeis, (oeis @241583)) =  unzip $ f [1..] (oeis @131644) (-1) where
--      f (x:xs) (y:ys) r = if y > r then (y, x) : f xs ys y else f xs ys r

-- instance OEIS 241664 where
--   oeisIx n = fst $ until ((<= 1) . snd)
--                           (\ (u, v) -> (u + 1, (oeisIx @58026) v)) (0, n)

-- instance OEIS 241671 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @65806

-- instance OEIS 241673 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @229037)) [1..]

-- instance OEIS 241752 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @229037))

-- instance OEIS 241766 where
--   oeisIx = p $ tail (oeis @1047) where
--      p _          0 = 1
--      p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 241772 where
--   oeis = zipWith (-) (tail (oeis @65094)) (oeis @65094)

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

-- instance OEIS 242183 where
--   oeis = concatMap (\ (r,x) -> take r [x,x..]) $
--                            zip (oeis @242192) [1..]

-- instance OEIS 242186 where
--   oeis = filter ((> 1) . (oeisIx @242192)) [1..]

-- instance OEIS 242311 where
--   oeisIx = maximum . (rowT @96145)

-- instance OEIS 242314 where
--   oeisIx = maximum . (rowT @242312)

-- instance OEIS 242401 where
--   oeis = filter ((== 0) . (oeisIx @10054)) (oeis @37)

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

-- instance OEIS 242622 where
--   oeisIx = genericLength . (rowT @242614)

-- instance OEIS 242901 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @242885))

-- instance OEIS 243451 where
--   oeis = [x | x <- (oeis @241751), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 243757 where
--   oeis = scanl (*) 1 (oeis @60904)

-- instance OEIS 244080 where
--   oeisIx = (oeisIx @6530) . (oeisIx @166133)

-- instance OEIS 244365 where
--   oeis = tablList @244365
-- instance Table 244365 where
--   rowCol = rowCol_off @244365 @1 @1
--   rowT   = rowT_off @244365 @1
--   tabf = zipWith farideh (map (+ 1) (oeis @40)) (oeis @249669)
--                  where farideh u v = filter ((== 1) .  (oeisIx @10051 . pred)) [u..v]

-- instance OEIS 244479 where
--   oeisIx = (`div` 2) . (oeisIx @244478)

-- instance OEIS 244724 where
--   oeis = 1 : f 1 [2..] where
--      f x xs = f' xs where
--        f' (u:us) | (oeisIx @10051 . pred) (x + u) == 1 = g u (delete u xs)
--                  | otherwise             = f' us where
--           g y ys = g' ys where
--             g' (v:vs) | (oeisIx @10051 . pred) (y + v) == 0 = u : v : f v (delete v ys)
--                       | otherwise        = g' vs

-- instance OEIS 244731 where
--   oeis = [x | x <- [1..], (oeisIx @244724) x == x]

-- instance OEIS 244732 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @244724))

-- instance OEIS 244747 where
--   oeis = findIndices ((== 1) . (oeisIx @209229)) (oeis @51145)

-- instance OEIS 245057 where
--   oeisIx = fromJust . (`elemIndex` (oeis @249129))

-- instance OEIS 245066 where
--   oeisIx n = (oeisIx @1497) (2 * n) n

-- instance OEIS 245097 where
--   oeisIx n = sum $ map (oeisIx @10051 . pred) [n + 1 .. (oeisIx @7535) n]

-- instance OEIS 245234 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @238880))

-- instance OEIS 245304 where
--   oeis = map (pred . head) $ filter (all (== 1) . map (oeisIx @10051 . pred)) $
--      iterate (zipWith (+) [1, 1, 1, 1, 1]) [1, 3, 7, 9, 13]

-- instance OEIS 245305 where
--   oeis = map ((`div` 4) . (subtract 1) . head) $
--      filter (all (== 1) . map (oeisIx @10051 . pred)) $
--             iterate (zipWith (+) [4, 4, 6]) [1, 3, 5]

-- instance OEIS 245394 where
--   (oeis, (oeis @245395)) =  unzip $ f [0..] (oeis @125717) (-1) where
--      f (x:xs) (y:ys) r = if y > r then (y,x) : f xs ys y else f xs ys r

-- instance OEIS 245471 where
--   oeis = concat $ transpose [odds (oeis @65621), [1..]]
--      where odds [] = []; odds [x] = []; odds (_:x:xs) = x : odds xs

-- instance OEIS 245508 where
--   oeis = f (oeis @40) (oeis @1105) where
--      f ps'@ (p:ps) xs'@ (x:xs) = if p <= x then x : f ps xs' else f ps' xs

-- instance OEIS 245530 where
--   oeisIx = product . (rowT @245499)

-- instance OEIS 245543 where
--   oeis = scanl1 (+) (oeis @160239)

-- instance OEIS 245586 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @228276))

-- instance OEIS 245644 where
--   oeis = filter ((== 1) . (oeisIx @245656) . (^ 3)) [1..]

-- instance OEIS 245718 where
--   oeisIx n = (oeisIx @245677) n `div` (oeisIx @245678) n

-- instance OEIS 245722 where
--   oeisIx = product . (rowT @244365)

-- instance OEIS 245729 where
--   oeis = filter f [1..] where
--                         f x = p ^ 2 < q && (oeisIx' q == 1 || f q)
--                               where q = div x p; p = (oeisIx @20639) x

-- instance OEIS 245836 where
--   oeisIx = sum . (rowT @53398)

-- instance OEIS 246430 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @212300)) . (oeisIx @40)

-- instance OEIS 246431 where
--   oeisIx = fromJust . (`elemIndex` (oeis @101403))

-- instance OEIS 246433 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @113963))

-- instance OEIS 246436 where
--   oeisIx n = genericLength $ [1..n] \\ genericIndex (tabf @220237) (n - 1)

-- instance OEIS 246517 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @141036)) [0..]

-- instance OEIS 246518 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ (oeis @141036)

-- instance OEIS 246520 where
--   oeisIx = maximum . (rowT @246830)

-- instance OEIS 246558 where
--   oeisIx = (oeisIx @7954) . (oeisIx @45)

-- instance OEIS 246606 where
--   oeisIx n = (oeisIx @116853) (2 * n - 1) n

-- instance OEIS 246700 where
--   oeis = tablList @246700
-- instance Table 246700 where
--   rowCol n k = genericIndex (tabf @246700) (n - 1) !! (k-1)
--   rowT n = genericIndex (tabf @246700) (n - 1)
--   tabf = [1] : f 2  where
--      f x = (x : (rowT @246700) (oeisIx @2322 x)) : f (x + 1)

-- instance OEIS 246701 where
--   oeis = zipWith (-) (tail (oeis @246520)) (oeis @246520)

-- instance OEIS 246704 where
--   oeis = filter (\x -> (oeisIx @113963) x == x) [1..]

-- instance OEIS 246776 where
--   oeisIx n = (oeisIx @249669) n - (oeisIx @40) (n + 1)

-- instance OEIS 246781 where
--   oeis = map succ $ elemIndices 3 $ tail $ oeis @182134

-- instance OEIS 246782 where
--   oeis = map succ $ elemIndices 2 $ tail $ oeis @182134

-- instance OEIS 246785 where
--   oeisIx n = if null ms then 0 else head ms
--               where ms = [m | m <- [1 .. n - 1], (oeisIx @182134) (n - m) == m]

-- instance OEIS 246878 where
--   oeis = 1 : f [1] (oeis @523) where
--      f xs (k:ks) = y : f (xs ++ [y]) ks where y = sum $ genericDrop k xs

-- instance OEIS 247095 where
--   oeisIx = (+ 5) . fromJust . (`elemIndex` (oeis @250030))

-- instance OEIS 247104 where
--   oeis = filter ((== 1) . (oeisIx @8966)) $ tail (oeis @3052)

-- instance OEIS 247144 where
--   oeisIx = fromJust . (`elemIndex` (oeis @247143))

-- instance OEIS 247167 where
--   oeis = filter ((zipWith (==) [0..] (oeis @247143)) !!) [0..]

-- instance OEIS 247180 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @67029

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

-- instance OEIS 247363 where
--   oeisIx n = (oeisIx @247358) (2 * n - 1) n

-- instance OEIS 247367 where
--   oeisIx n = sum $ map ((1 -) . (oeisIx @10052) . (n -)) $
--                     takeWhile (<= n) (oeis @290)

-- instance OEIS 247414 where
--   oeis = zipWith (-) (tail (oeis @24431)) (oeis @24431)

-- instance OEIS 247419 where
--   oeis = concat $
--                  transpose [map (subtract 1) (oeis @3256), (oeis @3256)]

-- instance OEIS 247451 where
--   oeis = map (oeisIx @7947) (oeis @25487)

-- instance OEIS 247468 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247462))

-- instance OEIS 247499 where
--   oeisIx = sum . (rowT @247500)

-- instance OEIS 247503 where
--   oeisIx = product . filter (odd . (oeisIx @49084)) . (rowT @27746)

-- instance OEIS 247514 where
--   oeis = filter (\x -> (oeisIx @117767) x == (oeisIx @247485) x) [1..]

-- instance OEIS 247515 where
--   oeis = filter (\x -> (oeisIx @117767) x < (oeisIx @247485) x) [1..]

-- instance OEIS 247647 where
--   oeisIx = (oeisIx @7088) . (oeisIx @247648)

-- instance OEIS 247657 where
--   oeis = f 0 $ drop 2 (oeis @40) where
--      f z (p:ps) | (oeisIx @10051 . pred) z' == 1 = z' : f z' (delete z' ps)
--                 | otherwise        = f z' ps
--                 where z' = z + p

-- instance OEIS 247793 where
--   oeis = 2 : f (zip [2..] $ tail (oeis @40)) where
--      f ((x, p) : xps) = m : f xps where
--        m = head [y | y <- [1..], (p + (oeisIx @40) y) `mod` (oeisIx @720) (x * y) == 0]

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

-- instance OEIS 247815 where
--   oeisIx = sum . map (oeisIx @10051 . pred) . (rowT @77581)

-- instance OEIS 247824 where
--   oeis = f ips where
--      f ((x, p) : xps) = head
--        [y | (y, q) <- ips, (p + q) `mod` (x + y) == 0] : f xps
--      ips = zip [1..] (oeis @40)

-- instance OEIS 247857 where
--   oeis = concat $ zipWith replicate (oeis @256852) (oeis @40)

-- instance OEIS 247879 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @248025))

-- instance OEIS 247892 where
--   oeisIx n = n - (oeisIx @247815) n

-- instance OEIS 247894 where
--   oeisIx = (oeisIx @196) . (oeisIx @10807)

-- instance OEIS 248012 where
--   oeisIx = foldr1 (^) . (rowT @27748)

-- instance OEIS 248013 where
--   oeis = filter (\x -> (oeisIx @247796) x == x) [0..]

-- instance OEIS 248014 where
--   oeis = filter (\x -> (oeisIx @247796) x < x) [0..]

-- instance OEIS 248025 where
--   oeis = 1 : f 1 [2..] where
--     f x zs = g zs where
--       g (y:ys) = if (oeisIx @30) y == (oeisIx @10888) x
--                  then y : f y (delete y zs) else g ys

-- instance OEIS 248043 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @248024))

-- instance OEIS 248045 where
--   oeisIx n = (oeisIx @891) (n - 1) * (oeisIx @142) n

-- instance OEIS 248101 where
--   oeisIx = product . filter (even . (oeisIx @49084)) . (rowT @27746)

-- instance OEIS 248336 where
--   oeisIx = fromJust . (`elemIndex` map (oeisIx @248327) [0..])

-- instance OEIS 248353 where
--   oeis = filter k [1..] where
--      k x = elem x $ map (uncurry (+)) $
--            takeWhile ((> 0) . fst) $ map (divMod (x ^ 2)) (oeis @11557)

-- instance OEIS 248387 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247665)) . (oeisIx @40)

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

-- instance OEIS 248906 where
--   oeisIx = sum . map ((2 ^) . subtract 2 . (oeisIx @95874)) . tail . (rowT @210208)

-- instance OEIS 248907 where
--   oeisIx = (oeisIx @32810) . (oeisIx @185969)

-- instance OEIS 248918 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @247665)) . (oeisIx @961) . (+ 1)

-- instance OEIS 249032 where
--   oeis = zipWith (-) (tail (oeis @75326)) (oeis @75326)

-- instance OEIS 249034 where
--   oeis = filter odd (oeis @171946)

-- instance OEIS 249040 where
--   oeis = tail $ scanl (\i j -> i + 1 - mod j 2) 0 (oeis @249039)

-- instance OEIS 249041 where
--   oeis = tail $ scanl (\i j -> i + mod j 2) 0 (oeis @249039)

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

-- instance OEIS 249279 where
--   oeisIx = fromJust . (`elemIndex` (oeis @249278))

-- instance OEIS 249304 where
--   oeisIx n = if n == 0 then 0 else (oeisIx @48967) n + (oeisIx @48967) (n - 1)

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
--   oeis = map succ $ elemIndices 4 $ tail $ oeis @182134

-- instance OEIS 249575 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @84937)) [1..]

-- instance OEIS 249602 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @84937)) [1..]

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

-- instance OEIS 249648 where
--   oeisIx = fromJust . (`elemIndex` (oeis @249626)) . (oeisIx @11540)

-- instance OEIS 249680 where
--   oeisIx = (oeisIx @84937) . (+ 1) . (* 3)

-- instance OEIS 249681 where
--   oeisIx = (oeisIx @84937) . (+ 2) . (* 3)

-- instance OEIS 249682 where
--   oeisIx = (oeisIx @84937) . (* 3)

-- instance OEIS 249683 where
--   oeisIx = flip div 2 . (oeisIx @249681)

-- instance OEIS 249684 where
--   oeis = map succ $ elemIndices 0 $ tail $ oeis @249777

-- instance OEIS 249694 where
--   oeis = zipWith gcd (drop 3 (oeis @84937)) (oeis @84937)

-- instance OEIS 249858 where
--   oeisIx n = (oeisIx @249857) n - (oeisIx @249856) n

-- instance OEIS 249900 where
--   oeis = [1..4] ++ concatMap (uncurry (++))
--             (f [2] [3,4] (drop 2 (oeis @40)) (tail (oeis @2808))) where
--      f us@ (u:_) vs ps'@ (p:p':ps) cs'@ (c:c':cs)
--        | (oeisIx @10051 . pred) u == 1 = g ([c] ++ us ++ [c']) vs ps' cs
--        | otherwise      = g ([p] ++ us ++ [p']) vs ps cs'
--      g us vs@ (v:_) (p:ps) (c:cs) = (us, ws) : f us ws ps cs where
--        ws = if (oeisIx @10051 . pred) v == 1 then [c] ++ vs ++ [p] else [p] ++ vs ++ [c]

-- instance OEIS 249918 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @203069))

-- instance OEIS 249920 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @55266))

-- instance OEIS 249951 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @113630)) [1..]

-- instance OEIS 250007 where
--   oeis = map length $ group $ map (oeisIx @100618) [1..]

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

-- instance OEIS 251140 where
--   oeisIx = (oeisIx @1222) . (oeisIx @98550)

-- instance OEIS 251141 where
--   oeisIx = (oeisIx @1222) . (oeisIx @98548)

-- instance OEIS 251239 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @98550)) [1..]

-- instance OEIS 251240 where
--   oeis = filter ((== 2) . (oeisIx @62799) . (oeisIx @98550)) [1..]

-- instance OEIS 251241 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @98550)) [1..]

-- instance OEIS 251391 where
--   oeis = filter ((== 1) . (oeisIx @8966) . (oeisIx @98550)) [1..]

-- instance OEIS 251392 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred) . (oeisIx @98550)) [1..]

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
--                             (oeisIx @10051 . pred) v == 1]

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
--             | p == 2 && (oeisIx @10051 . pred) q == 1 = q
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

-- instance OEIS 251725 where
--   oeisIx 1 = 1
--   oeisIx n = if length ps == 1 then 1 else head $ filter f [2..]  where
--     f b = all (== len) lbs where len:lbs = map (length . d b) ps
--     ps = (rowT @27748) n
--     d b = unfoldr (\z -> if z == 0 then Nothing else Just $ swap $ divMod z b)

-- instance OEIS 251728 where
--   oeis = filter f [1..] where
--                         f x = q < p ^ 2 && (oeisIx @10051 . pred) q == 1
--                               where q = div x p; p = (oeisIx @20639) x

-- instance OEIS 251756 where
--   oeis = 0 : f 0 (oeis @2808) where
--      f x zs = g zs where
--        g (y:ys) | d == 1 || (oeisIx @10051 . pred) d == 1 = g ys
--                 | otherwise = y : f y (delete y zs)
--                 where d = gcd x y

-- instance OEIS 251767 where
--   oeis = map head $ group (oeis @251539)

-- instance OEIS 251768 where
--   oeis = map length $ group (oeis @251539)

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

-- instance OEIS 252448 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249990))

-- instance OEIS 252458 where
--   oeis = [x | x <- [1..], (oeisIx @249990) x == x]

-- instance OEIS 252837 where
--   oeis = f (oeis @98550) where
--      f us = (h 0 vs) : f vs where
--        (_:vs) = dropWhile ((== 0) . (oeisIx @10051 . pred)) us
--        h e (w:_:ws) = if even w then h (e + 1) ws else e

-- instance OEIS 252849 where
--   oeis = filter (even . (oeisIx @46951)) [1..]

-- instance OEIS 252865 where
--   oeis = 1 : 2 : 3 : f 2 3 (drop 3 (oeis @5117)) where
--      f u v ws = g ws where
--        g (x:xs) = if gcd x u > 1 && gcd x v == 1
--                      then x : f v x (delete x ws) else g xs

-- instance OEIS 252895 where
--   oeis = filter (odd . (oeisIx @46951)) [1..]

-- instance OEIS 252912 where
--   oeis = filter (\x -> (oeisIx @98550) x == (oeisIx @251555) x) [1..]

-- instance OEIS 252939 where
--   oeis = zipWith (-) (tail (oeis @252912)) (oeis @252912)

-- instance OEIS 252940 where
--   oeis = map length $ group (oeis @252939)

-- instance OEIS 253046 where
--   oeisIx n | i == 0 || p > 3 = n
--             | p == 2          = 3 * (oeisIx @40) (i + 1)
--             | otherwise       = 2 * (oeisIx @40) (i - 1)
--               where i = (oeisIx @49084) (div n p);  p = (oeisIx @20639) n

-- instance OEIS 253048 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) $ map (oeisIx @253049) [1..]

-- instance OEIS 253073 where
--   oeis = 0 : f 0 (oeis @18252) where
--      f u vs = g vs where
--        g (w:ws) | (oeisIx @10051 . pred) (u + w) == 1 = g ws
--                 | otherwise = w : f w (delete w vs)

-- instance OEIS 253074 where
--   oeis = 0 : f 0 [1..] where
--      f u vs = g vs where
--        g (w:ws) | (oeisIx @10051 . pred) (u + w) == 1 = g ws
--                 | otherwise = w : f w (delete w vs)

-- instance OEIS 253106 where
--   oeis = filter f [1..] where
--      f x = p <= 3 && (oeisIx @10051 . pred) (div x p) == 1  where p = (oeisIx @20639) x

-- instance OEIS 253138 where
--   oeisIx n = sum $ map (oeisIx @64911) $
--      takeWhile (> 0) $ map (2 * p -) $ dropWhile (< p) (oeis @1358)
--      where p = (oeisIx @40) n

-- instance OEIS 253169 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @256188))

-- instance OEIS 253253 where
--   oeis = f (oeis @1704) [] where
--      f (x:xs) ds = y : f xs (insert y ds) where
--                    y = head (oeisIx_row' x `minus` ds)

-- instance OEIS 253297 where
--   oeis = f (oeis @98550) where
--      f (u:vs@ (_:v:_)) = if (oeisIx @10051 . pred) v == 1 && div u v > 2
--                            then v : f vs else f vs

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

-- instance OEIS 253584 where
--   oeis = map head $ group (oeis @253443)

-- instance OEIS 253589 where
--   oeisIx = (oeisIx @120) . (oeisIx @252867)

-- instance OEIS 253603 where
--   oeisIx = fromJust . (`elemIndex` (oeis @253581))

-- instance OEIS 253607 where
--   oeis = zipWith (-) (tail (oeis @253580)) (oeis @253580)

-- instance OEIS 253717 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @106039)

-- instance OEIS 253721 where
--   oeisIx = flip mod 10 . (oeisIx @14612)

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

-- instance OEIS 255833 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @166133))

-- instance OEIS 255878 where
--   oeis = zipWith (-) (tail (oeis @256188)) (oeis @256188)

-- instance OEIS 255940 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @249167))

-- instance OEIS 255972 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @251604))

-- instance OEIS 256012 where
--   oeisIx = p (oeis @13929) where
--      p _      0 = 1
--      p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

-- instance OEIS 256152 where
--   256152_list = filter f (oeis @6881) where
--      f x = (oeisIx @10052)' ((spf + 1) * (x `div` spf + 1)) == 1
--            where spf = (oeisIx @20639) x

-- instance OEIS 256187 where
--   oeis = zipWith (-) (tail (oeis @4718)) (oeis @4718)

-- instance OEIS 256213 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @254077)) [1..]

-- instance OEIS 256248 where
--   oeis = filter ((== 1) . (oeisIx @209229) . (oeisIx @55744)) [1..]

-- instance OEIS 256283 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257905))

-- instance OEIS 256371 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @256210))

-- instance OEIS 256372 where
--   oeis = [x | x <- [1..], (oeisIx @256210) x == x]

-- instance OEIS 256405 where
--   oeis = 2 : 3 : f (3:[5..]) 4 where
--      f zs@ (z:_) x = z : f (delete y zs) y where
--                     y = head $ O.isect (oeisIx_row' (x ^ 2 - 1)) zs

-- instance OEIS 256406 where
--   oeis = f (oeis @166133) where
--      f (u:vs'@ (v:ws)) | u > v || v /= u ^ 2 - 1 = f vs'
--                       | otherwise               = u : f ws

-- instance OEIS 256414 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121217)) . (oeisIx @40)

-- instance OEIS 256415 where
--   oeisIx n | (oeisIx @10051 . pred) n == 1 = 2 * n
--             | r == 0 && (oeisIx @10051 . pred) n' == 1 = 2 * n'
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

-- instance OEIS 256618 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @121217))

-- instance OEIS 256628 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @251622))

-- instance OEIS 256673 where
--   oeis = filter odd (oeis @157037)

-- instance OEIS 256703 where
--   oeis = map (+ 1) $ findIndices (\ (u, v) -> v == u^2-1) $
--                              zip (oeis @166133) (tail (oeis @166133))

-- instance OEIS 256757 where
--   oeisIx n = fst $ until ((== 1) . snd)
--               (\ (i, x) -> (i + 1, fi $ (oeisIx @7733) x)) (0, n)

-- instance OEIS 256758 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex`  (oeis @256757))

-- instance OEIS 256775 where
--   oeis = [x | x <- map (+ 81) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256776 where
--   oeis = [x | x <- map (+ 256) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256777 where
--   oeis = [x | x <- map (+ 625) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256786 where
--   oeis = filter f (oeis @52382) where
--      f x = g x where
--        g z = z == 0 || x `mod` (oeisIx @40) d == 0 && g z'
--              where (z', d) = divMod z 10

-- instance OEIS 256834 where
--   oeis = [x | x <- map (+ 1296) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256835 where
--   oeis = [x | x <- map (+ 2401) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256836 where
--   oeis = [x | x <- map (+ 4096) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256837 where
--   oeis = [x | x <- map (+ 6561) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256838 where
--   oeis = [x | x <- map (+ 10000) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256839 where
--   oeis = [x | x <- map (+ 14641) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256840 where
--   oeis = [x | x <- map (+ 20736) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256841 where
--   oeis = [x | x <- map (+ 28561) (oeis @290), (oeisIx @10051 . pred) x == 1]

-- instance OEIS 256863 where
--   oeis = map (oeisIx @40) $ (map succ $ elemIndices 0 $ tail $ oeis @256852)

-- instance OEIS 256885 where
--   oeisIx n = (oeisIx @217) n - (oeisIx @720) n

-- instance OEIS 256914 where
--   oeisIx = last . (rowT @256913) . succ

-- instance OEIS 256915 where
--   oeisIx = genericLength . (rowT @256913)

-- instance OEIS 256918 where
--   oeisIx n = (oeis @257218) !! (n - 1)
--   oeis = zipWith gcd (oeis @257218) $ tail (oeis @257218)

-- instance OEIS 257046 where
--   oeis = elemIndices 1 (oeis @256914)

-- instance OEIS 257047 where
--   oeis = filter ((/= 1) . (oeisIx @256914)) [0..]

-- instance OEIS 257053 where
--   oeis = tablList @257053
-- instance Table 257053 where
--   rowCol = rowCol_off @257053 @1 @0
--   rowT   = rowT_off @257053 @1
--   tabf = map (rowT @257053) (oeis @40)

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

-- instance OEIS 257173 where
--   oeisIx = fromJust . (`elemIndex` (oeis @248737))

-- instance OEIS 257218 where
--   oeis = 1 : f 1 [2..] (oeis @4526) where
--      f x zs cds = g zs where
--        g (y:ys) | cd `member` cds = y : f y (delete y zs) (delete cd cds)
--                 | otherwise       = g ys
--                 where cd = gcd x y

-- instance OEIS 257279 where
--   oeis = filter ((== 1) . (oeisIx @168046)) (oeis @257278)

-- instance OEIS 257455 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257339))

-- instance OEIS 257456 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @257340))

-- instance OEIS 257457 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @257339)) [1..]

-- instance OEIS 257458 where
--   oeis = filter ((== 1) . (oeisIx @10055) . (oeisIx @257339)) [1..]

-- instance OEIS 257465 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @175498))

-- instance OEIS 257478 where
--   oeisIx n = (oeisIx @257475) n - (oeisIx @257120) n

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

-- instance OEIS 257762 where
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 2 $ tail $ oeis @258383)

-- instance OEIS 257773 where
--   oeisIx = genericLength . (rowT @257770)

-- instance OEIS 257778 where
--   oeisIx = head . (rowT @257770)

-- instance OEIS 257779 where
--   oeisIx = last . (rowT @257770)

-- instance OEIS 257782 where
--   oeis = filter ((> 0) . (oeisIx @257778)) [0..]

-- instance OEIS 257785 where
--   oeis = elemIndices 1 (oeis @257773)

-- instance OEIS 257815 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @64364))

-- instance OEIS 257831 where
--   oeisIx = foldr (\b v -> 10 * v + b) 0 .
--              concat . mapMaybe (flip lookup bin) . (rowT @31298)
--               where bin = zip [0..9] (tabf @30308)

-- instance OEIS 257892 where
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 4 $ tail $ oeis @258383)

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
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 5 $ tail $ oeis @258383)

-- instance OEIS 257956 where
--   oeisIx = sum . (rowT @232642)

-- instance OEIS 257971 where
--   oeis = zipWith (-) (tail (oeis @6921)) (oeis @6921)

-- instance OEIS 257999 where
--   oeis = filter (odd . flip mod 2 . (oeisIx @1222)) (oeis @3586)

-- instance OEIS 258032 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . flip div 10. (^ 3)) (oeis @40)

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

-- instance OEIS 258353 where
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @212306

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
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 3 $ tail $ oeis @258383)

-- instance OEIS 258469 where
--   oeis = map (oeisIx @258432) $ (map succ $ elemIndices 1 $ tail $ oeis @258383)

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
--   oeis = map succ $ elemIndices 1 $ tail $ oeis @74695

-- instance OEIS 258614 where
--   oeis = filter ((> 1) . (oeisIx @74695)) [1..]

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

-- instance OEIS 259046 where
--   oeisIx = fromJust . (`elemIndex` (map (oeisIx @259043) [0..]))

-- instance OEIS 259315 where
--   oeis = filter ((== 0) . (oeisIx @10051 . pred)) (oeis @52382)

-- instance OEIS 259366 where
--   oeis = filter (\x -> (oeisIx @60682) x < (oeisIx @5)' x - 1) [2..]

-- instance OEIS 259966 where
--   oeis = 0 : 0 : 2 : 7 : zipWith (+)
--      (zipWith3 (((+) .) . (+))
--                (oeis @259966) (drop 2 (oeis @259966)) (drop 3 (oeis @259966)))
--      (drop 2 $ zipWith (+)
--                (map (* 2) $ drop 2 (oeis @5251)) (map (* 3) (oeis @5251)))

-- instance OEIS 259969 where
--   oeisIx n = (oeis @259967) !! n
--   oeis = 3 : 2 : 2 : 5 : zipWith3 (((+) .) . (+))
--      (oeis @259967) (drop 2 (oeis @259967)) (drop 3 (oeis @259967))

-- instance OEIS 260020 where
--   oeis = filter
--                  (\x -> 2 * (oeisIx @10)' x == (oeisIx @10)' (2 * (oeisIx @203)' x)) [1..]

-- instance OEIS 260022 where
--   oeisIx = (oeisIx @6921) . (* 2)

-- instance OEIS 260273 where
--   oeis = iterate (\x -> x + (oeisIx @261461) x) 1

-- instance OEIS 260485 where
--   oeisIx = head . (rowT @260580)

-- instance OEIS 260664 where
--   oeisIx = sum . zipWith (*) (oeis @87960) . map (oeisIx @133042) . (rowT @260672)

-- instance OEIS 260669 where
--   oeisIx = flip div 2 . (oeisIx @54440)

-- instance OEIS 260682 where
--   oeis = filter ((== 1) . flip mod 6) (oeis @3136)

-- instance OEIS 260706 where
--   oeisIx = sum . (rowT @260672)

-- instance OEIS 260797 where
--   oeisIx = (oeisIx @98743) . (oeisIx @142)

-- instance OEIS 260822 where
--   oeis = f 1 [1..] where
--      f x zs = g zs where
--        g (y:ys) = if y /= x && (oeisIx @10051 . pred) (x + y) == 0
--                      then y : f (x + 1) (delete y zs) else g ys

-- instance OEIS 260895 where
--   oeisIx = sum . map (oeisIx @10051 . pred) . (rowT @77664)

-- instance OEIS 260933 where
--   oeis = f 1 [1..] where
--      f x zs = g zs where
--        g (y:ys) = if (oeisIx @10051 . pred) (x + y) == 0 && (oeisIx @10051 . pred) (x + y + 1) == 0
--                      then y : f (x + 1) (delete y zs) else g ys

-- instance OEIS 260936 where
--   oeis = [x | x <- [1..], (oeisIx @260933) x == x]

-- instance OEIS 260987 where
--   (oeis, (oeis @260633)) = unzip $ f 1 0 where
--      f x r = if y > r then (y, x) : f (x + 1) y else f (x + 1) r
--              where y = (oeisIx @8480) x

-- instance OEIS 261009 where
--   oeisIx = (oeisIx @53735) . (oeisIx @79)

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

-- instance OEIS 261466 where
--   (oeis, (oeis @261467)) = unzip $ (0, 1) : f 0 1 where
--      f i x | y > x     = (y, i) : f (i + 1) y
--            | otherwise = f (i + 1) x
--            where y = (oeisIx @261461) i

-- instance OEIS 261518 where
--   oeis = 1 : zipWith (-)
--                  (map (oeisIx @40) (zipWith (+) (oeis @261518) [1..])) (oeis @40) [1]

-- instance OEIS 261525 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @31131)) . (* 2)

-- instance OEIS 261585 where
--   oeisIx = genericLength . (rowT @261575)

-- instance OEIS 261587 where
--   oeisIx = sum . (rowT @261575)

-- instance OEIS 261598 where
--   oeisIx = product . (rowT @261575)

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

-- instance OEIS 261788 where
--   oeis = f 1 1 (oeis @261786)' where
--      f z k (x:xs) | x >= z    = k : f (3 * z) (k + 1) xs
--                   | otherwise = f z (k + 1) xs

-- instance OEIS 261789 where
--   oeis = zipWith (-) (tail (oeis @261786)') (oeis @261786)'

-- instance OEIS 261793 where
--   oeis = iterate (\x -> x + (oeisIx @261794) x) 1

-- instance OEIS 261795 where
--   oeis = zipWith (-) (tail (oeis @261793)') (oeis @261793)'

-- instance OEIS 261869 where
--   oeis = zipWith (-) (tail (oeis @55615)) (oeis @55615)

-- instance OEIS 261890 where
--   oeis = zipWith (-) (tail (oeis @261869)) (oeis @261869)

-- instance OEIS 261893 where
--   oeisIx n = n * (n * (n + 2) + 3) + 1
--   oeis = zipWith (-) (tail (oeis @578)) (oeis @290)

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

-- instance OEIS 262079 where
--   oeis = zipWith (-) (tail (oeis @262065)) (oeis @262065)

-- instance OEIS 262095 where
--   oeisIx = sum . map ((1 -) . (oeisIx @64911)) . (rowT @27750)

-- instance OEIS 262138 where
--   oeis = concat $ transpose [oeis, (oeis @36263)]

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

-- instance OEIS 262279 where
--   oeisIx = fromJust . (`elemIndex` (oeis @261923))

-- instance OEIS 262358 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @262356))

-- instance OEIS 262360 where
--   oeis = [x | x <- [1..], (oeisIx @262356) x == x]

-- instance OEIS 262363 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) (oeis @262356)

-- instance OEIS 262367 where
--   oeis = [x | x <- [1..], (oeisIx @262323) x == x]

-- instance OEIS 262371 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @262356)) [1..]

-- instance OEIS 262377 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred)) $ map (oeisIx @262358) [1..]

-- instance OEIS 262378 where
--   oeis = filter ((== 1) . (oeisIx @10051 . pred) . (oeisIx @262358)) [1..]

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

-- instance OEIS 262604 where
--   oeis = zipWith (-) (tail (oeis @252022)) (oeis @252022)

-- instance OEIS 262663 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @75348))

-- instance OEIS 262665 where
--   oeis = [x | x <- [1..], (oeisIx @75348)' x == x]

-- instance OEIS 262703 where
--   oeis = zipWith (-) (tail (oeis @252001)) (oeis @252001)

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

-- instance OEIS 263837 where
--   oeis = filter (\x -> (oeisIx @1065) x <= x) [1..]

-- instance OEIS 263845 where
--   oeis = filter (> 0) (oeis @258059)

-- instance OEIS 263847 where
--   oeis = 0 : zipWith (-)
--      (zipWith (-) (tail qs) qs) (drop 2 (oeis @41))
--      where qs = es $ tail (oeis @41)
--            es [] = []; es [x] = []; es (_:x:xs) = x : es xs

-- instance OEIS 263896 where
--   oeisIx n = (oeisIx @75383) (2 * n - 1) n

-- instance OEIS 263922 where
--   oeisIx = (oeisIx @51903) . (oeisIx @984)

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

-- instance OEIS 264647 where
--   oeisIx = (+ 1) . fromJust . (`elemIndex` (oeis @263856))

-- instance OEIS 264740 where
--   oeisIx = sum . map (oeisIx @265) . (rowT @27750)'

-- instance OEIS 264782 where
--   oeisIx n = sum $ zipWith (^) (map (oeisIx @8683) divs) (reverse divs)
--               where divs = (rowT @27750) n

-- instance OEIS 264856 where
--   oeisIx n = fromJust $ findIndex (elem n) (tabl @125605)

-- instance OEIS 265885 where
--   oeisIx n = n `bimpl` (oeisIx @40) n where
--      bimpl 0 0 = 0
--      bimpl p q = 2 * bimpl p' q' + if u <= v then 1 else 0
--                  where (p', u) = divMod p 2; (q', v) = divMod q 2

-- instance OEIS 273191 where
--   oeis = (map length . group) $ map (oeisIx @273190) [0..]

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

-- instance OEIS 277516 where
--   oeisIx n = (oeisIx @277278) n - n

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

-- instance OEIS 187769 where
--   oeisIx n k = (tabf @187769) !! n !! k
--   oeisIx_row n = (tabf @187769) !! n
--   oeisIx_tabf = [0] : [elemIndices (b, len - b) $
--      takeWhile ((<= len) . uncurry (+)) $ zip (oeis @120) (oeis @23416) |
--      len <- [1 ..], b <- [1 .. len]]
--   oeis = concat (tabf @187769)

-- instance OEIS 187786 where
--   oeisIx n k = (tabf @187786) !! n !! k
--   oeisIx_row n = fromJust $ find (elem n) (tabf @187769)
--   oeisIx_tabf = map (rowT @187786) [0..]

-- instance OEIS 196202 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 (p - 1) (p ^ 2) where p = (oeisIx @40) n

-- instance OEIS 232642 where
--   import Data.List.Ordered (member)
--   oeisIx n k = (tabf @232642) !! (n - 1) !! (k-1)
--   oeisIx_row n = (tabf @232642) !! (n - 1)
--   oeisIx_tabf = f (tabf @82560) [] where
--      f (xs:xss) zs = ys : f xss (sort (ys ++ zs)) where
--        ys = [v | v <- xs, not $ O.member v zs]
--   oeis = concat (tabf @232642)

-- instance OEIS 239293 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = head [c | c <- (oeis @2808), powerMod n c c == n]

-- instance OEIS 245970 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 (phi + (oeisIx @245970) phi) n
--               where phi = (oeisIx @10) n

-- instance OEIS 245971 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 4 (phi + (oeisIx @245971) phi) n
--               where phi = (oeisIx @10) n

-- instance OEIS 260580 where
--   import Data.List.Ordered (union)
--   oeisIx n k = (tabf @260580) !! (n - 1) !! (k-1)
--   oeisIx_row n = (tabf @260580) !! (n - 1)
--   oeisIx_tabf = zipWith (\\) (tail zss) zss where
--                               zss = scanl O.union [] (tabl @65305)

-- instance OEIS 263924 where
--   import Math.NumberTheory.Primes.Factorisation (factorise)
--   oeisIx n = (oeis @263924) !! (n - 1)
--   oeis = filter f [2..] where
--      f x = not (null pe23s) && any ((> e23) . snd) pes' where
--            e23 = maximum (map snd pe23s)
--            (pe23s, pes') = span ((<= 3) . fst) $ factorise $ (oeisIx @984) x

-- instance OEIS 265012 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 10 (p - 1) (p ^ 2) where p = (oeisIx @40) n

-- instance OEIS 7535 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = head [m | m <- dropWhile (<= n) (oeis @2808),
--                         powerMod n (m - 1) m == 1]


-- instance OEIS 45616 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = (oeis @45616) !! (n - 1)
--   oeis = filter
--                  (\p -> powerMod 10 (p - 1) (p ^ 2) == 1) (oeis @40)'

-- instance OEIS 214321 where
--   oeis_conjectured  = toList $ S.fromList $ take 100000 (oeis @214551)

-- instance OEIS 226950 where
--   oeis = f (oeis @76467) S.empty where
--      f (x:xs) s | S.size s'' <= 1 = f xs (x `S.insert` s)
--                 | otherwise     = x : f xs (x `S.insert` s)
--                 where s'' = S.filter ((`S.member` s) . (x -)) s'
--                       (s', _) = S.split (x `div` 2) s

-- instance OEIS 72774 where
--   import Data.Map (M.empty, M.findMin, M.deleteMin, M.insert)
--   import qualified Data.Map.Lazy as Map (M.null)
--   oeisIx n = (oeis @72774) !! (n - 1)
--   (oeis, (oeis @72775), (oeis @72776)) = unzip3 $
--      (1, 1, 1) : f (tail (oeis @5117)) M.empty where
--      f vs'@ (v:vs) m
--       | Map.M.null m || xx > v = (v, v, 1) :
--                                f vs (M.insert (v^2) (v, 2) m)
--       | otherwise = (xx, bx, ex) :
--                     f vs' (M.insert (bx*xx) (bx, ex+1) $ M.deleteMin m)
--       where (xx, (bx, ex)) = M.findMin m

-- instance OEIS 206925 where
--   oeis = 1 : f [0, 1] (M.fromList [ (Bin [0], 1), (Bin [1], 1)]) where
--      f bs'@ (b:bs) m = y : f (succ bs') (M.insert (Bin bs') y m) where
--        y = m M.! (Bin bs) +
--            length (filter (\ds -> ds == reverse ds) $ tail $ inits bs')
--        succ [] = [1]; succ (0:ds) = 1 : ds; succ (1:ds) = 0 : succ ds

-- instance OEIS 245340 where
--   import Data.IntMap (M.singleton, M.member, (!), M.insert)
--   oeisIx n = (oeis @245340) !! n
--   oeis = 0 : f [1..] [1..] 0 (M.singleton 0 0) where
--      f us'@ (u:us) vs'@ (v:vs) w m
--        | u `M.member` m = (m M.! u) : f us vs' w m
--        | otherwise    = g (reverse[w-v,w-2*v..1] ++ [w+v,w+2*v..]) where
--        g (x:xs) = if x `M.member` m then g xs else f us' vs x $ M.insert x v m


-- aaaa


-- instance OEIS 290151 where
--   oeisIx 1 = []
--   oeisIx n | l<-f $ n - 1 = l++[head [i |i<-[2..],gcd i n<2,all (/=i)l,abs (n-i)>1]]

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

-- instance OEIS 131644 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = (oeis @131644) !! (n - 1)
--   oeis = map fst $ iterate f (0, 2) where
--      f (v, w) = (powerMod 2 v w, w + 1)

-- instance OEIS 162247 where
--   oeisIx n k = (tabl @162247) !! (n - 1) !! (k-1)
--   oeisIx_row n = (tabl @162247) !! (n - 1)
--   oeisIx_tabl = map (concat . sortBy (comparing length)) $ tail fss where
--      fss = [] : map fact [1..] where
--            fact x = [x] : [d : fs | d <- [2..x], let (x',r) = divMod x d,
--                                     r == 0, fs <- fss !! x', d <= head fs]

-- instance OEIS 206702 where
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

-- instance OEIS 239452 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = head [m | m <- [2..], powerMod m n n == mod m n]

-- instance OEIS 260031 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = if x > 0 then x else f $ div (n ^ n) 12
--             where x = powerMod n n 12
--                   f z = if m == 0 then f z' else m
--                         where (z', m) = divMod z 12

-- instance OEIS 15910 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 n n

-- instance OEIS 62173 where
--   import Math.NumberTheory.Moduli (powerMod)
--   oeisIx n = powerMod 2 (n - 1) n

-- instance OEIS 2658 where
--   oeis = 1 : 1 : f [1,1] where
--      f (x:xs) = y : f (y:x:xs') where y = x * sum xs + x * (x + 1) `div` 2

-- instance OEIS 5428 where
--   oeis = (iterate j (1, 1)) where
--      j (a, s) = (a', (s + a') `mod` 2) where
--        a' = (3 * a + (1 - s) * a `mod` 2) `div` 2

-- instance OEIS 25480 where
--   interleave xs ys = concat . transpose $ [xs,ys]
--   oeisIx = interleave [0..] (oeisIx @25480)
--   oeisIx n k = (tabf @25480) !! n !! k
--   oeisIx_row n = (tabf @25480) !! n
--   oeisIx_tabf = iterate (\xs -> concat $
--      transpose [xs, [length xs .. 2 * length xs - 1]]) [0]
--   oeis = concat $ (tabf @25480)

-- instance OEIS 48004 where
--   tri n k | (k < 0) || (k > n) = 0
--           | (k == 0) || (k == n) = 1
--           | otherwise = 2*tri (n - 1) k + tri (n-1) (k-1) - 2*tri (n-2) (k-1)
--                               + tri (n-k-1) (k-1) - tri (n-k-2) k

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

-- instance OEIS 56792 where
--   c i = if i `mod` 2 == 0 then i `div` 2 else i - 1
--   b 0 foldCount = foldCount
--   b sheetCount foldCount = b (c sheetCount) (foldCount + 1)
--   oeisIx n = b n 0

-- instance OEIS 61282 where
--   c i = if i `mod` 3 == 0 then i `div` 3 else i - 1
--   b 0 foldCount = foldCount
--   b sheetCount foldCount = b (c sheetCount) (foldCount + 1)
--   oeisIx n = b n 0

-- instance OEIS 68119 where
--   oeisIx n = fst $ until ((== 1) . denominator . snd)
--                           (\ (i, x) -> (i + 1, f x)) (0, fromInteger n + 1%4)
--      where f x = x * fi (ceiling x)

-- instance OEIS 165476 where
--   oeisIx = flip legendreSymbol 131071

-- instance OEIS 170942 where
--   oeis = tablList @170942
-- instance Table 170942 where
--   rowCol n k = (tabf @170942) !! (n - 1) (k- 1)
--   rowT n = map fps $ sort $ permutations [1..n] where
--      fps perm = sum $ map (fi.fromEnum) $ zipWith (==) perm [1..n]
--   tabf = map (rowT @170942) [1..]

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

-- instance OEIS 177869 where
--   base_weight b g n | n == 0 = 0 | otherwise = (base_weight b g (n `div` b)) + (g $ n `mod` b)
--   interesting b g = filter f [1..] where f n = n `mod` (base_weight b g n) == 0
--   bin_interesting g = interesting 2 g
--   weights l n | (n >=0) && ((length l) > fromInteger n) = l !! fromInteger n | otherwise = 0
--   cnst = weights [1, 1]
--   let sequence = bin_interesting cnst

-- instance OEIS 181971 where
--   oeis = tablList @181971
-- instance Table 181971 where
--   tabl = map snd $ iterate f (1, [1]) where
--      f (i, row) = (1 - i, zipWith (+) ([0] ++ row) (row ++ [i])

-- instance OEIS 181988 where
--   interleave (hdx : tlx) y = hdx : interleave y tlx
--   oeis003602 = interleave [1..] oeis003602
--   oeis181988 = interleave [1..] (zipWith (+) oeis003602 oeis181988)

-- instance OEIS 182211 where
--   oddDigits 0 = True
--   oddDigits n = let (q,r) = quotRem n 10
--   ..............in (odd r) && oddDigits q
--   oddSet 0 = []
--   oddSet 1 = [1,3..9]
--   oddSet k = [n | i <- [1,3..9], x <- oddSet (k-1), let n = i*10^ (k-1) + x,
--   ...............oddDigits ((n^3) `mod` 10^k)]
--   main = putStrLn $ map (length . oddSet) [1..]

-- instance OEIS 188528 where
--   oeisIx n = succ $ fromJust $
--     findIndex (\m -> h n m 13 == 6) [1..12] where
--       h year month day
--         | month <= 2 = h  (year - 1)  (month + 12)  day
--         | otherwise  = (day + 26 * (month + 1) `div` 10 + y + y `div` 4
--                        + century `div` 4 - 2 * century) `mod` 7
--           where (century, y) = divMod year 100
--   -- For statistics (see example) ...
--   ff13_perMonth ys m = length $ filter (== m) (map (oeisIx @188528) ys)
--   century20 = map (ff13_perMonth [1901..2000]) [1..12]
--   century21 = map (ff13_perMonth [2001..2100]) [1..12]

-- instance OEIS 188715 where
--   let ext (c,l) = [ (tails.filter (\b->a* (a-1)`mod` (b-a)==0)$r,a:l) | (a:r)<-c] in map (last.snd.head) . tail . iterate (>>= ext) $ [ (map reverse (inits[2..]),[])]

-- instance OEIS 192734 where
--   oeisIx n = head [x | x <- [2^u + 2^v + 1 | u <- [2..], v <- [1..u-1]],
--                         oeisIx x == n]


-- instance OEIS 197704 where
--   base_weight b g n | n == 0 = 0 | otherwise = (base_weight b g (n `div` b)) + (g $ n `mod` b)
--   interesting b g = filter f [1..] where f n = n `mod` (base_weight b g n) == 0
--   bin_interesting g = interesting 2 g
--   weights l n | (n >=0) && ((length l) > fromInteger n) = l !! fromInteger n | otherwise = 0
--   original = weights [4,3]
--   let a = bin_interesting original

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


--- bbbb

-- instance OEIS 67458 where
--   oeisIx n = f 0 n where
--      f y 0 = y
--      f y x = if d == 0 then f y x' else f (y + mod n d) x'
--              where (x', d) = divMod x 10

-- instance OEIS 88314 where
--   oeisIx = sum . concat . ps 1 where
--      ps _ 0 = [[]]
--      ps i j = [t:ts | t <- [i..j], ts <- ps t (j - t)]

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

-- instance OEIS 105870 where
--   oeis = 1 : 1 : zipWith (\u v -> (u + v) `mod` 7)
--                                  (tail (oeis @105870)) (oeis @105870)

-- instance OEIS 132739 where
--   oeisIx (succ->n)
--     | r > 0     = pred n
--     | otherwise = (oeisIx @132739.pred) n'
--     where (n',r) = divMod n 5

-- instance OEIS 301851 where
--   oeis = tablList @301851
-- instance Table 301851 where
--   rowCol n k = length $ nub [i^2 + j^2 | i <- [0..n - 1], j <- [0..k- 1]]

-- cccc

