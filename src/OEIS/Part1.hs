module OEIS.Part1 where

import OEIS.OEIS
import OEIS.Common

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
import Data.Proxy
import Data.Tuple
import qualified Data.List.Ordered as O
import qualified Data.Map as M


instance OEIS 2 where
  oeis = let a = 1:2: drop 2 (concat . zipWith (replicate . fi) a . cycle $ [1, 2]) in a

instance OEIS 4 where
  oeis   = repeat 0
  oeisIx = const 0

instance OEIS 5 where
  oeisIx = genericLength . divisors . succ
  -- product . map (+1) . (rowT @124010)

-- TODO: This one is interesting
instance OEIS 6 where
  oeisIx = oeisIx @196 . oeisIx @40

instance OEIS 7 where
  oeis   = 1 : repeat 0
  oeisIx = (0 ^)

instance OEIS 8 where
  oeisIx = p [1,2,5,10] where
    p _          0 = 1
    p []         _ = 0
    p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m


instance OEIS 10 where
  oeisIx (succ->n) = totient n

instance OEIS 12 where
  oeis   = repeat 1
  oeisIx = const 1

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

instance OEIS 23 where
  oeisIx n = foldl g 1 [1..n]
    where g n m = n*m + (-2)^m

instance OEIS 26 where
  oeisIx (succ->n) = f primes n 1 (0^ (n - 1)) 1 where
    f _  1 q e y  = y * e * q
    f ps'@ (p:ps) x q e y
      | m == 0    = f ps' x' p (e+1) y
      | e > 0     = f ps x q 0 (y * e * q)
      | x < p * p = f ps' 1 x 1 y
      | otherwise = f ps x 1 0 y
      where (x', m) = divMod x p

instance OEIS 27 where
  oeis   = [1..]
  oeisIx = succ

instance OEIS 28 where
  oeis = filter (odd . sum . map (oeisIx @120) . rowT @124010) [1..]

instance OEIS 30 where
  oeisIx = until (< 10) (`div` 10)

instance OEIS 31 where
  oeisIx = f
    where
      f 0 = 1
      f n = (`div` n) $ sum $
        zipWith (*) (map (oeisIx @10 . pred) divs) (map (oeisIx @79) $ reverse divs)
        where divs = (rowT @27750) n


instance OEIS 32 where
  oeis = let r = 2 : 1 : do zipTail (+) r in r

instance OEIS 33 where
  oeisIx (succ->n) = sum $ map f [2..n]
    where f k = g k `div` h k
          g k = (-1)^k * n * fac (2*n - k - 1) * fac (n - k)
          h k = fac (2*n - 2*k) * fac (k - 2)
          fac = (oeisIx @142)

instance OEIS 34 where
  oeis   = cycle [1,2]
  oeisIx = succ . (`mod` 2)

instance OEIS 35 where
  oeis   = cycle [0,1]
  oeisIx = (`mod` 2)

instance OEIS 37 where
  oeisIx (succ->n) = n + f (n + f n)
    where f = oeisIx @196

instance OEIS 38 where
  oeis     = 2 : repeat 0
  oeisIx 0 = 2
  oeisIx n = 0


instance OEIS 40 where
  oeis = primes

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

instance OEIS 59 where
  oeis = [ fi i | i <- [1..], isPrime $ (2*i)^4 + 1 ]

instance OEIS 62 where
  oeisIx n = fi . floor $ fi (n + 1) / (exp 1 - 2)

instance OEIS 69 where
  oeis = [x | x <- [0..], odd $ oeisIx @120 x]

instance OEIS 70 where
  oeisIx = p (oeis @28310) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 71 where
  oeis = map (subtract 1) $ tail $ oeis @45

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

instance OEIS 81 where
  oeis = 0 : 1 : f 1 [1,0] where
   f x ys = y : f (x + 1) (y : ys) where
     y = sum (zipWith (*) (map h [1..x]) ys) `div` x
     h = sum . map (\d -> d * (oeisIx @81) d) . (rowT @27750)

instance OEIS 85 where
  oeis = xs
    where
      xs = 1 : 1 : zipWith (+) (zipWith (*) [1..] xs) (tail xs)

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

instance OEIS 108 where
  oeis = map last $ iterate (scanl1 (+) . (++ [0])) [1]

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

instance OEIS 120 where
  oeis = concat r
    where r = [0] : (map.map) (+1) (scanl1 (++) r)

  oeisIx = fi . popCount . fi

instance OEIS 123 where
  oeis = xs
    where xs = 1 : zipWith (+) xs (tail $ concat $ transpose [xs,xs])

instance OEIS 124 where
  oeisIx = succ . oeisIx @217

instance OEIS 127 where
  oeisIx = sum . take 5 . rowT @7318

instance OEIS 129 where
  oeis = xs where xs = 0 : 1 : zipWith (+) xs do map (2 *) $ tail xs

instance OEIS 141 where
  oeisIx 0 = 1
  oeisIx (pred->n) = 16 * (oeisIx @50470) n - 4 * (oeisIx @2173) n

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

instance OEIS 172 where
  oeisIx = sum . map (oeisIx @578) . rowT @7318

instance OEIS 178 where
  oeis = 1 : scanl1 (*) facts

instance OEIS 193 where
  oeisIx = round . log . succ . fi
  oeis   = concat [ replicate n i
                  | n <- 1 : do zipTail (-) $ oeis @219092
                  | i <- [0..] ]

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

instance OEIS 204 where
  oeis = xs where xs = 1 : 3 : do zipTail (+) xs

instance OEIS 208 where
  oeis = map (`div` 2) $ concat $ transpose
   [zipWith (+) (oeis @116) $ bis (oeis @116), bis $ tail (oeis @116)]
   where bis (x:_:xs) = x : bis xs

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

instance OEIS 216 where
  oeis = iterate (oeisIx @3132) 2

instance OEIS 217 where
  oeis     = scanl1 (+) [0..]
  oeisIx n = div (n* (n+1)) 2

instance OEIS 218 where
  oeis = iterate (oeisIx @3132) 3

instance OEIS 221 where
  oeis = iterate (oeisIx @3132) 5

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

instance OEIS 246 where
  oeis' (A r) = 1 : 1 : zipWith (+) (tail r) (zipWith (*) r $ oeis @2378)

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

instance OEIS 267 where
  oeisIx = oeisIx @196 . oeisIx @16813

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

instance OEIS 292 where
  oeisIx n = n * (n + 1) * (n + 2) `div` 6
  oeis = scanl1 (+) $ oeis @217

instance OEIS 295 where
  oeisIx n = 2^n - n - 1

instance OEIS 297 where
  oeisIx (pred->n) = (n + 1) * (n + 3) * (n+8) `div` 6

instance OEIS 301 where
  oeisIx      = oeisIx @79 . oeisIx @45
  oeis' (A r) = 1 : scanl (*) 2 r

instance OEIS 302 where
  oeis   = iterate (* 4) 1
  oeisIx = (4 ^)

instance OEIS 304 where
  oeis = fix \r -> 2 : 3 : zipTail (*) r

instance OEIS 312 where
  oeis     = zipWith (^) [0..] [0..]
  oeisIx n = n ^ n

instance OEIS 325 where
  oeis = zipWith (-) (oeis @79) [0..]
  oeisIx n = 2 ^ n - n

instance OEIS 326 where
  oeisIx n = n * (3 * n - 1) `div` 2

instance OEIS 328 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 <= n^2]

instance OEIS 330 where
  oeis = scanl1 (+) $ oeis @290
  oeisIx n = n * (n + 1) * (2 * n + 1) `div` 6

instance OEIS 340 where
  oeisIx n = div (3 ^ (n+2) - 2*n - 5) 4

instance OEIS 350 where
  oeis = map fi . elemIndices True $ zipWith (isSuffixOf `on` show) [0..] $ oeis @45

instance OEIS 351 where
  oeis   = iterate (* 5) 1
  oeisIx = (5 ^)

instance OEIS 379 where
  oeis = filter (even . sum . map (oeisIx @120) . rowT @124010) [1..]

instance OEIS 384 where
  oeisIx n = n * (2 * n - 1)
  -- oeis = scanl (+) 0 (oeis @16813)

instance OEIS 389 where
  oeis = 0 : 0 : f [] (oeis @217)
    where
      f xs (t:ts) = (sum $ zipWith (*) xs $ oeis @217) : f (t:xs) ts

instance OEIS 400 where
  oeisIx = (6 ^)
  oeis   = iterate (* 6) 1

instance OEIS 420 where
  oeisIx = (7 ^)
  oeis   = iterate (* 7) 1

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

instance OEIS 461 where
  oeisIx = fi . f . fi . succ
    where
      f n = (read $ concat $ replicate n $ show n) :: Integer

instance OEIS 462 where
  oeisIx (succ->fi->n) = fi . g [] n $ reverse $ takeWhile (<= n) $ tail (oeis @217)
    where
      g as 0 []     = read $ concat $ map show $ reverse as :: Integer
      g as x (t:ts) = g (a:as) r ts where (a,r) = divMod x t

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

instance OEIS 537 where
  oeisIx = oeisIx @290 . oeisIx @217

instance OEIS 538 where
  oeisIx n = (3 * n * (n + 1) - 1) * (2 * n + 1) * (n + 1) * n `div` 30

instance OEIS 540 where
  oeis = scanl1 (+) $ oeis @1014

instance OEIS 566 where
  oeisIx n = n * (5 * (n - 1) + 2) `div` 2
  -- oeis = scanl (+) 0 $ oeis @16861

instance OEIS 567 where
  oeisIx n = n * (3 * n - 2)

instance OEIS 578 where
  oeisIx = (^ 3)
  oeis =  0 : 1 : 8 : zipWith (+)
    (map (+ 6) $ oeis @578)
    (map (* 3) $ tail $ zipWith (-) (tail $ oeis @578) $ oeis @578)


instance OEIS 583 where
  oeisIx = (^ 4)
  -- oeis = scanl (+) 0 $ oeis @5917

instance OEIS 584 where
  oeisIx = (^ 5)

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

instance OEIS 603 where
  oeisIx n = genericLength [ (x,y) | x <- [0..n], y <- [0..n], x^2 + y^ 2 <= n^2]

instance OEIS 607 where
  oeisIx = p $ oeis @40 where
    p _      0 = 1
    p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 657 where
  oeisIx n = (rowCol @8280) (2 * n) n


instance OEIS 670 where
  oeis = 1 : f [1] (map tail $ tail (tabl @7318)) where
    f xs (bs:bss) = y : f (y : xs) bss where y = sum $ zipWith (*) xs bs

instance OEIS 689 where
  oeis = 1 : cycle [2,4,8,6]

instance OEIS 695 where
  oeisIx n = if n == 0 then 0 else 4 * oeisIx @695 n' + b
    where (n',b) = divMod n 2

instance OEIS 697 where
  oeisIx n = sum $ zipWith (*) (rowT @109449 n) (1 : tail (oeis @290))

instance OEIS 703 where
  oeisIx = fi . floor . (/ 2) . (+ 7) . sqrt . (+ 1) . (* 24) . fi

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

instance OEIS 749 where
  oeis = fix \r -> 0 : 0 : 0 : 1 : zipWith3 (\u v w -> 4 * u - 6 * v + 4 * w)
    (drop 3 r) (drop 2 r) (drop 1 r)

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

instance OEIS 925 where
  oeisIx n = sum $ map (oeisIx @10052 . (n -)) $ takeWhile (<= n) $ oeis @290

instance OEIS 930 where
  oeis = fix \r -> 1 : 1 : 1 : zipWith (+) r (drop 2 r)

instance OEIS 931 where
  oeis = fix \r -> 1 : 0 : 0 : zipWith (+) r (tail r)

instance OEIS 934 where
  oeisIx = floor . (/ 2) . (+ 7) . sqrt . (+ 1) . (* 48) . fi


instance OEIS 957 where
  oeis = fix \r -> 0 : 1 : (map (`div` 2) $ tail $ zipWith (-) (oeis @108) r)

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

instance OEIS 961 where
  oeis = 1 : g (S.singleton 2) (tail primes)
    where
      g s (p:ps) = m : g (S.insert (m * (oeisIx @20639 . pred) m) $ S.insert p s') ps
        where
          (m, s') = S.deleteFindMin s

instance OEIS 975 where
  oeis = fix \r -> 0 : 1 : map (+ 1) (zipWith (+) (tail r) (map (* 2) r))

instance OEIS 980 where
  oeisIx n = genericLength $ filter ((== 0) . sum) $ subsequences [-n..n]

instance OEIS 982 where
  oeisIx = (`div` 2) . (+ 1) . (^ 2)

instance OEIS 984 where
  oeisIx (fi->n) = rowCol @7318 (2*n) n

instance OEIS 992 where
  oeis = 1 : f 1 0 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ take x $ zipWith (*) zs $ reverse zs

-- instance OEIS 1006 where
--   oeisIx n = (oeis @1006) !! n
--  oeis = zipWith (+) (oeis @5043) $ tail (oeis @5043)

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

instance OEIS 1043 where
  oeis = zipTail (+) (oeis @40)

instance OEIS 1044 where
  oeis = fix \r -> 1 : zipWith (*) (tail (oeis @290)) r

instance OEIS 1045 where
  oeisIx = (`div` 3) . (+ 1) . oeisIx @79
  oeis   = fix \r -> 0 : 1 : zipWith (+) (map (2 *) r) (tail r)

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

instance OEIS 1088 where
  oeis = scanl1 (*) $ oeis @10

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

instance OEIS 1142 where
  oeisIx = product . rowT @7318

instance OEIS 1146 where
  oeisIx = (2 ^) . (2 ^)
  oeis = iterate (^ 2) 2

instance OEIS 1147 where
  oeisIx n = product [1, 3 .. 2 * n - 1]
  oeis     = fix \r -> 1 : zipWith (*) [1, 3 ..] r


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

instance OEIS 1177 where
  oeisIx (succ->n) = head [k | k <- [1..], oeisIx @45 k `mod` n == 0]

instance OEIS 1179 where
  oeisIx = f . succ
    where
      f 1 = 0
      f n = if p == n then ll (p `div` 24) 1 else f p
              where p = oeisIx @1175 (pred n)
                    ll x k = if x == 1 then k else ll (x `div` 5) (k + 1)

instance OEIS 1196 where
  oeisIx n = if n == 0 then 0 else 4 * oeisIx @1196 n' + 3 * b
              where (n',b) = divMod n 2

instance OEIS 1221 where
  oeisIx = genericLength . snd . unzip . factorise . fi . succ

instance OEIS 1222 where
  oeisIx = fi . sum . snd . unzip . factorise . fi . succ

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

instance OEIS 1286 where
  oeisIx ((+2)->n) = sum [1..n - 1] * product [1..n - 1]

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

instance OEIS 1333 where
  oeis = fix \r -> 1 : 1 : zipWith (+) r (map (* 2) $ tail r)


instance OEIS 1353 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (4 *) $ tail r) r

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

instance OEIS 1444 where
  oeisIx n = div (3 ^ n + 3 ^ (div n 2)) 2


instance OEIS 1462 where
  oeis = 1 : 2 : 2 : g 3  where
     g x = (genericReplicate (oeisIx @1462 $ pred x) x) ++ g (x + 1)

instance OEIS 1463 where
  oeis = scanl1 (+) $ oeis @1462

instance OEIS 1477 where
  oeisIx = id
  oeis   = [0..]

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

instance OEIS 1541 where
  oeis = fix \r -> 1 : 3 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1542 where
  oeis = fix \r -> 0 : 2 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1550 where
  oeisIx n = sum $ map (^ n) [1..3]


instance OEIS 1563 where
  oeis = zipWith (-) (tail $ oeis @142) $ oeis @142

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


instance OEIS 1602 where
  oeisIx n = fi . (+ 1) . fromJust $ findIndex ((== 0) . (`mod` (oeisIx @40) n)) $ tail $ oeis @45

instance OEIS 1608 where
  oeis = fix \r -> 3 : 0 : 2 : zipTail (+) r

instance OEIS 1610 where
  oeis = fix \r -> 0 : 2 : do map (+ 1) $ zipTail (+) r

instance OEIS 1611 where
  oeisIx = (+ 1) . oeisIx @45
  oeis = fix \r -> 1 : 2 : do map (subtract 1) $ zipTail (+) r

instance OEIS 1612 where
  oeis = fix \r -> 3 : 2 : do map (subtract 1) $ zipTail (+) r

instance OEIS 1616 where
  oeisIx (succ->n) = sum $ map (oeisIx @10 . pred) $ zipWith gcd ds $ reverse ds
    where ds = (rowT @27750) n

instance OEIS 1629 where
  oeis = f [] $ tail $ oeis @45 where
     f us (v:vs) = (sum $ zipWith (*) us $ oeis @45) : f (v:us) vs

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

instance OEIS 1654 where
  oeis = zipWith (*) (tail (oeis @45)) (oeis @45)

instance OEIS 1696 where
  oeis = fix \r -> 0 : 1 : zipWith (-)
     (zipWith (+) f $ map (^ 2) f)
     (zipWith (*) r f)
     where f = tail $ oeis @1696

instance OEIS 1697 where
  oeis = 1 : 1 : f [1,1] where
     f xs@ (x:_) = y : f (y : xs) where y = x * sum xs

instance OEIS 1700 where
  oeisIx (fi->n) = rowCol @7318 (2*n+1) (n+1)

instance OEIS 1704 where
  oeis = map fi f
    where f = map read (zipWith (++) iss $ tail iss) :: [Integer]
                 where iss = map show [1..]

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

instance OEIS 1817 where
  oeisIx (succ->n) = genericLength [d | d <- [1,4..n], mod n d == 0]

instance OEIS 1822 where
  oeisIx (succ->n) = genericLength [d | d <- [2,5..n], mod n d == 0]

instance OEIS 1834 where
  oeis = fix \r -> 1 : 5 : zipWith (-) (map (* 4) $ tail r) r

instance OEIS 1835 where
  oeis = fix \r -> 1 : 1 : zipWith (-) (map (4 *) $ tail r) r

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

instance OEIS 1845 where
  oeisIx n = (2 * n + 1) * (2 * n ^ 2 + 2 * n + 3) `div` 3


instance OEIS 1855 where
  oeis = fix \r -> 0 : zipWith (+) [1..] do zipTail (+) . concat $ transpose [r, r]

instance OEIS 1870 where
  oeis = uncurry c $ splitAt 1 $ tail (oeis @45) where
     c us vs'@ (v:vs) = (sum $ zipWith (*) us vs') : c (v:us) vs

instance OEIS 1906 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (* 3) $ tail r) r

instance OEIS 1911 where
  oeis = fix \r -> 0 : 1 : map (+ 2) do zipTail (+) r

instance OEIS 1923 where
  oeis = scanl (+) 0 $ tail $ oeis @312

instance OEIS 1924 where
  oeis = drop 3 $ zipWith (-) (tail $ oeis @45) [0..]

instance OEIS 1945 where
  oeis = fix \r -> 0 : 1 : 1 : 1 : 5 : 1 : zipWith6
     (\u v w x y z -> - u + v + 3*w + x - y - z)
       (drop 5 r) (drop 4 r) (drop 3 r)
       (drop 2 r) (drop 1 r) (drop 0 r)

instance OEIS 1950 where
  oeisIx n = oeisIx @201 n + n + 1

instance OEIS 1951 where
  oeisIx = floor . (* sqrt 2) . fi

instance OEIS 1952 where
  oeisIx = floor . (* (sqrt 2 + 2)) . fi . succ

instance OEIS 1969 where
  oeis = [x | x <- [0..], even $ oeisIx @120 x]

instance OEIS 1971 where
  oeisIx = floor . (+ 0.5) . (/ 8) . fi . (^ 2)

instance Table 2024 where
  tabl   = iterate (\xs@ (x:_) -> map (+ 1) (x : xs)) [1]
  rowT   = rowT_off   @2024 @1
  rowCol = rowCol_off @2024 @1 @1

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

instance OEIS 2062 where
  oeisIx n = oeisIx @45 n + n
  oeis = fix \r -> 0 : 2 : 3 : (map (subtract 1) $ zipWith (-) (map (* 2) $ drop 2 r) r)

instance OEIS 2064 where
  oeisIx n = n * 2 ^ n + 1
  oeis = 1 : 3 : (map (+ 1) $ zipWith (-) (tail xs) xs)
     where xs = map (* 4) $ oeis @2064

instance OEIS 2081 where
  oeis = filter ((`elem` [2,4,8,16]) . (`mod` 20)) [1..]

instance OEIS 2083 where
  oeis = 1 : f [1] where
     f xs = x : f (x:xs) where x = sum $ take (div (1 + length xs) 2) xs

instance OEIS 2088 where
  oeis = scanl (+) 0 $ oeis @10

instance OEIS 2104 where
  oeisIx = genericLength . filter (\xs -> head xs == minimum xs)
         . tail . choices . enumFromTo 1

instance OEIS 2109 where
  oeis = scanl1 (*) $ oeis @312

instance OEIS 2110 where
  oeisIx n = product . genericTake n $ oeis @40
  oeis = scanl (*) 1 $ oeis @40

instance OEIS 2131 where
  oeisIx (succ->n) = sum [d | d <- [1..n], mod n d == 0, odd $ div n d]

instance OEIS 2144 where
  oeis = map succ $ filter ((== 1) . oeisIx @10051) [0,4..]

instance OEIS 2173 where
  oeisIx n = (oeisIx @50450) n - (oeisIx @50453 n)

instance OEIS 2183 where
  oeis = 1 : unfoldr f (1, map (oeisIx @5) (oeis @61799))
    where
      f (m, (dropWhile (m>=)->(x:xs)))
        = Just (x, (x, xs))

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

instance OEIS 2315 where
  oeis' (A r) = 1 : 7 : zipWith (-) (map (* 6) (tail r)) r

instance OEIS 2321 where
  oeis = scanl1 (+) (oeis @8683)

instance OEIS 2324 where
  oeisIx n = oeisIx @1817 n - oeisIx @1822 n

instance OEIS 2326 where
  oeisIx n = fi . (+ 1) $ fromJust $ findIndex ((== 0) . (`mod` (2 * n + 1))) $ tail $ oeis @225

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


instance OEIS 2411 where
  oeisIx n = n * oeisIx @217 n

instance OEIS 2450 where
  -- oeisIx = (`div` 3) . (oeisIx @24036)
  oeis = iterate ((+ 1) . (* 4)) 0

instance OEIS 2472 where
  oeisIx (succ->n) = genericLength [x | x <- [1..n], gcd n x == 1, gcd n (x + 2) == 1]

instance OEIS 2476 where
  oeis = filter ((== 1) . (`mod` 6)) $ oeis @40

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


instance OEIS 2805 where
  oeisIx = denominator . sum . map (1 %) . enumFromTo 1 . succ
  oeis = map denominator $ scanl1 (+) $ map (1 %) [1..]

instance OEIS 2808 where
  oeis = map (+1) $ filter ((== 1) . (oeisIx @66247)) [2..]

instance OEIS 2814 where
  oeis = 1 : zipWith div (tail xs) xs
     where xs = map (oeisIx @45) (oeis @244)

instance OEIS 2821 where
  oeisIx = round . sqrt . fi . (^ 3)

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

instance OEIS 2943 where
  oeisIx n = 2 * n * (2 * n + 1)

instance OEIS 2965 where
  oeis = concat $ transpose [oeis @129, (oeis @1333)]

instance OEIS 2977 where
  oeis = f $ S.singleton 1 where
     f s = m : (f $ S.insert (3*m+1) $ S.insert (2*m+1) s') where
          (m, s') = S.deleteFindMin s

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

instance OEIS 3059 where
  oeis = concat $ zipWith ($) (map replicate [1,3..]) [1..]

instance OEIS 3101 where
  oeisIx n = sum $ zipWith (^) [0 ..] [n + 1, n .. 1]

instance OEIS 3105 where
  oeisIx n = p 1 n where
     p k m | m == 0 = 1 | m < k = 0 | otherwise = q k (m-k) + p (k+2) m
     q k m | m == 0 = 1 | m < k = 0 | otherwise = p (k+2) (m-k) + p (k+2) m

instance OEIS 3108 where
  oeisIx = p $ tail (oeis @578) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 3132 where
  oeisIx 0 = 0
  oeisIx x = d ^ 2 + (oeisIx @3132) x' where (x', d) = divMod x 10

instance OEIS 3148 where
  oeis = 1 : 1 : zipWith (+) (tail (oeis @3148))
                            (zipWith (*) (tail (oeis @2943)) (oeis @3148))

instance OEIS 3159 where
  oeis = f [1..] where f (x:xs) = x : f (delete  (2*x) xs)

instance OEIS 3168 where
  oeisIx 0 = 1
  oeisIx n = sum (zipWith (*)
     (tail $ (tabl @7318) !! fi n)
     ((transpose $ take (3 * fi n + 1) (tabl @7318)) !! (2 * fi n + 1)))
     `div` fi n

instance OEIS 3188 where
  oeisIx n = fi $ (fi n) `xor` (shiftR (fi n) 1 :: Integer)

instance OEIS 3215 where
  oeisIx n = 3 * n * (n + 1) + 1

instance OEIS 3229 where
  oeis = 1 : 1 : 3 : zipWith (+) (map (* 2) (oeis @3229)) (drop 2 (oeis @3229))

instance OEIS 3231 where
  oeisIx = floor . (/ 2) . (* (sqrt 5 + 5)) . (+ 1) . fi

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

instance OEIS 3269 where
  oeis = 0 : 1 : 1 : 1 : zipWith (+) (oeis @3269) (drop 3 (oeis @3269))

instance OEIS 3275 where
  oeisIx = (oeisIx @10) . fi . (oeisIx @1274)

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

instance OEIS 3415 where
--   oeisIx 0 = 0
  oeisIx n = ad n (oeis @40) where
    ad n _ | n < 2     = 0
    ad n ps'@ (p:ps)
       | n < p * p     = 1
       | r > 0         = ad n ps
       | otherwise     = n' + p * ad n' ps' where
         (n',r) = divMod n p

instance OEIS 3418 where
  oeisIx = foldl lcm 1 . enumFromTo 2

instance OEIS 3422 where
  oeis = scanl (+) 0 (oeis @142)

instance OEIS 3434 where
  oeisIx n = fst $ until ((== 1) . snd)
                          (\ (i, x) -> (i + 1, (oeisIx @10 . pred) x)) (0, succ n)

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

instance Table 3506 where
  tabl   = scanl1 (\xs ys -> zipWith (+) (zipWith (+) ([0] ++ xs) (xs ++ [0])) ys) (tabl @7318)
  rowT   = rowT_off   @3506 @1
  rowCol = rowCol_off @3506 @1 @1

instance OEIS 3506 where
  oeis = tablList @3506

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

instance OEIS 3602 where
  oeisIx = (`div` 2) . (+ 1) . (oeisIx @265)

instance OEIS 3619 where
  oeisIx (succ->n) = n + floor (log (x + fi (floor $ log x)))
    where x = fi n + 1

instance OEIS 3622 where
  oeis = filter ((elem 1) . (rowT @35516)) [1..]

instance OEIS 3627 where
  -- oeisIx n = (oeis @3627) !! (n - 1)
  oeis = filter ((== 2) . (`mod` 3)) (oeis @40)

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

instance OEIS 3817 where
--   oeisIx n = if n == 0 then 0 else 2 * (oeisIx @53644) n - 1
  oeis = map fi (scanl (.|.) 0 [1..] :: [Integer])

instance OEIS 3842 where
  oeis = tail $ concat fws where
     fws = [2] : [1] : (zipWith (++) fws $ tail fws)

instance OEIS 3849 where
  oeis = tail $ concat fws where
     fws = [1] : [0] : (zipWith (++) fws $ tail fws)

instance OEIS 3893 where
  oeis = 0 : 1 : zipWith (\u v -> (u + v) `mod` 10)
                         (tail (oeis @3893)) (oeis @3893)

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

instance OEIS 4001 where
  oeis = 1 : 1 : h 3 1  {- memoization -}
    where h n x = x' : h (n + 1) x'
            where x' = (oeisIx @4001 . pred) x + (oeisIx @4001 . pred) (n - x)

instance OEIS 4006 where
  oeisIx 0 = 0
  oeisIx (pred->n) = (oeisIx @292) n + n + 1

instance OEIS 4019 where
  oeis = iterate (oeisIx @290 . (+ 1)) 0

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

instance OEIS 4125 where
  oeisIx (succ->n) = sum $ map (mod n) [1..n]

instance OEIS 4148 where
  oeis = 1 : f [1] where
    f xs'@ (x:xs) = y : f (y : xs') where
      y = x + sum (zipWith (*) xs $ reverse $ tail xs)

instance OEIS 4149 where
  oeis = 1 : 1 : 1 : f [1,1,1] where
     f xs = y : f (y : xs) where
       y = head xs + sum (zipWith (*) (init $ init $ tail xs) (reverse xs))

instance OEIS 4151 where
  oeisIx = until ((> 0) . (`mod` 10)) (`div` 10) . succ

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

instance OEIS 4185 where
  oeisIx n = fi (read $ sort $ show (fi n) :: Integer)

instance OEIS 4186 where
  oeisIx n = fi (read $ sortOn Down $ show (fi n) :: Integer)

instance Table 4197 where
  rowT n = hs ++ drop (1 - fi n `mod` 2) (reverse hs) where hs = [0..n `div` 2]
  tabl = map (rowT @4197) [0..]

instance OEIS 4197 where
  oeis = tablList @4197

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

instance OEIS 4648 where
  oeis = zipWith mod (oeis @40) [1..]

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

instance Table 4736 where
  rowCol n k = n - k + 1
  rowT = rowT @4736
  tabl = map reverse (tabl @2260)

instance OEIS 4736 where
  oeis = tablList @4736

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

instance OEIS 5013 where
  oeis = alt (oeis @45) (oeis @32) where
     alt (f:_:fs) (_:l:ls) = f : l : alt fs ls

instance OEIS 5041 where
  oeis = 1 : f 1 1 (tail ts) where
     f y i gs'@ ((j,a):gs) | i < j  = y : f y (i+1) gs'
                          | i == j = a : f a (i+1) gs
     ts = [ (6*k + 3*k* (k - 1) `div` 2 + r* (k+2), 3*k+r+1) |
           k <- [0..], r <- [0,1,2]]

instance OEIS 5043 where
  oeis = 1 : 0 : zipWith div
     (zipWith (*) [1..] (zipWith (+)
         (map (* 2) $ tail (oeis @5043)) (map (* 3) (oeis @5043)))) [3..]

instance OEIS 5044 where
  oeisIx = p [2,3,4] . (subtract 3) where
    p _ 0 = 1
    p [] _ = 0
    p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 5055 where
  oeisIx = (* 7) . (5 ^)
  oeis = iterate (* 5) 7

instance OEIS 5087 where
  oeisIx (succ->n) = (oeisIx @1221 . pred) n + n `mod` 2 - 1

instance OEIS 5132 where
  oeis = 0 : recaman (S.singleton 0) 1 0 where
     -- recaman :: Set Integer -> Integer -> Integer -> [Integer]
     recaman s n x = if x > n && (x - n) `S.notMember` s
                        then (x - n) : recaman (S.insert (x-n) s) (n+1) (x-n)
                        else (x + n) : recaman (S.insert (x+n) s) (n+1) (x+n)

instance OEIS 5150 where
  oeis = map fi look_and_say

say :: Integer -> Integer
say = read . concatMap saygroup . group . show
  where saygroup s = (show $ length s) ++ [head s]
look_and_say :: [Integer]
look_and_say = 1 : map say look_and_say


instance OEIS 5151 where
  oeis = map fi (1 : f [1] :: [Integer]) where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map length zss, map head zss]
            zss = group $ sort xs

instance OEIS 5165 where
  oeis = 0 : zipWith (-) (tail (oeis @142)) (oeis @5165)

instance OEIS 5171 where
  oeisIx = (1 -) . (oeisIx @10051)

instance OEIS 5179 where
  oeisIx (succ->n) = fi . succ $ fromJust $ elemIndex n $ map (oeisIx @5) [0..]

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

instance OEIS 5238 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $ zipWith (+) ds $ tail ds where
     ds = map abs $ zipWith (-) (tail (oeis @5)) (oeis @5)

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

instance OEIS 5350 where
  oeis = 1 : 1 : 1 : h 4 1 where
     h x y = z : h (x + 1) z
       where z = (oeisIx @5350 . pred) y + (oeisIx @5350 . pred) (x - y)

instance OEIS 5351 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @5351) n' * 2 + m where
     (n', m) = if r < 0 then (q + 1, r + 2) else (q, r)
               where (q, r) = quotRem n (negate 2)

instance OEIS 5352 where
  oeisIx = (oeisIx @5351) . negate . succ

instance OEIS 5361 where
  oeisIx 0 = 1
  oeisIx n = product . (rowT @124010) . succ $ n

instance OEIS 5369 where
  oeisIx = (oeisIx @10052) . (+ 1) . (* 4)

instance OEIS 5448 where
  oeisIx (succ->n) = 3 * n * (n - 1) `div` 2 + 1
  oeis = 1 : zipWith (+) (oeis @5448) [3, 6 ..]

instance OEIS 5732 where
  oeis = 1 : 8 : f (drop 5 (tabl @7318)) where
     f (us:pss@ (vs:_:ws:_)) = (us !! 5 + vs !! 5 + ws !! 6) : f pss

instance OEIS 5809 where
  oeisIx (fi->n) = rowCol @7318 (3*n) n

instance OEIS 5810 where
  oeisIx (fi->n) = rowCol @7318 (4*n) n

instance OEIS 5912 where
  oeisIx n = (n * (n * (77 * n + 69) + 19) + 3) `div` 3

instance OEIS 5920 where
  oeisIx n = (n * (n * (3 * n + 7) + 6) + 2) `div` 2

instance OEIS 5942 where
  oeis = 1 : 2 : 4 : 6 : zipWith (+) (drop 6 ts) (drop 5 ts) where
     ts = concat $ transpose [oeis @5942, (oeis @5942)]

instance OEIS 6003 where
  oeisIx n = n * (n ^ 2 + 1) `div` 2
  oeis = scanl (+) 0 (oeis @5448)

instance OEIS 6012 where
  oeis = 1 : 2 : zipWith (-) (tail $ map (* 4) (oeis @6012))
    (map (* 2) (oeis @6012))

instance OEIS 6013 where
  oeisIx (fi->n) = fi $ rowCol @7318 (3 * n + 1) n `div` (n + 1)
  -- oeisIx' n = (oeisIx @258708) (2 * n + 1) n

instance OEIS 6049 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $
     zipWith (-) (tail (oeis @1221)) (oeis @1221)

instance OEIS 6053 where
  oeis = 0 : 0 : 1 : zipWith (+) (drop 2 (oeis @6053))
     (zipWith (-) (map (2 *) $ tail (oeis @6053)) (oeis @6053))

instance OEIS 6054 where
  oeis = 0 : 0 : 1 : zipWith (+) (map (2 *) $ drop 2 (oeis @6054))
     (zipWith (-) (tail (oeis @6054)) (oeis @6054))

instance OEIS 6218 where
  oeisIx n = sum $ map (div n) [1..n]

instance OEIS 6221 where
  oeisIx n = (17 * n * (n + 1) + 5) * (2 * n + 1)

instance OEIS 6261 where
  oeisIx = sum . take 6 . rowT @7318

instance OEIS 6331 where
  oeisIx n = sum $ zipWith (*) [2*n - 1, 2*n - 3 .. 1] [2, 4 ..]

instance OEIS 6364 where
  oeis = filter (even . (oeisIx @120). (`div` 2)) [0..]

instance OEIS 6463 where
  oeis = 0 : scanl1 (+) (oeis @3056)

instance OEIS 6478 where
  oeis = scanl1 (+) $ drop 2 (oeis @1629)


instance OEIS 6497 where
  oeis = 2 : 3 : zipWith (+) (map (* 3) $ tail (oeis @6497)) (oeis @6497)

instance OEIS 6498 where
  oeis = 1 : 1 : 1 : 2 : zipWith (+) (drop 3 (oeis @6498))
     (zipWith (+) (tail (oeis @6498)) (oeis @6498))

instance OEIS 6504 where
  oeisIx (succ->n) = n * (42 + n * (59 + n * (18 + n))) `div` 24

instance OEIS 6508 where
  oeis = iterate (oeisIx @2808 . pred) 1

instance OEIS 6519 where
  oeisIx (succ->fi->n) = fi (n .&. (-n) :: Integer)

instance OEIS 6527 where
  oeisIx n = n * (n ^ 2 + 2) `div` 3

instance OEIS 6564 where
  oeisIx (succ->n) = n * (5 * n * (n - 1) + 2) `div` 2

instance OEIS 6899 where
  oeis = 1 : m (tail (oeis @79)) (tail (oeis @244)) where
     m us'@ (u:us) vs'@ (v:vs) = if u < v then u : m us vs' else v : m us' vs

instance OEIS 6906 where
  oeisIx n = p 1 n 1 where
     p _ 0 s = s
     p k m s | m<k = 0 | otherwise = p k (m-k) (k*s) + p (k+1) m s

instance OEIS 6918 where
  oeis = scanl (+) 0 (oeis @8805)

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

instance OEIS 7097 where
  oeis = iterate (oeisIx @40) 1

instance OEIS 7283 where
  oeisIx = (* 3) . (2 ^)
  oeis = iterate (* 2) 3

instance OEIS 7290 where
  oeisIx (fi->n) = if n < 3 then 0 else 2 * rowCol @7318 n 3

instance OEIS 7294 where
  oeisIx = p $ tail (oeis @217) where
     p _      0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 7302 where
  oeisIx (fi->n) = fi do (oeisIx @120) $ xor n (3 * n) :: Integer

instance OEIS 7310 where
  oeis = 1 : 5 : map (+ 6) (oeis @7310)


instance Table 7318 where
  tabl = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

instance OEIS 7318 where
  oeis = tablList @7318

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

instance OEIS 7406 where
  oeis = map numerator $ scanl1 (+) $ map (1 %) $ tail (oeis @290)

instance OEIS 7407 where
  oeis = map denominator $ scanl1 (+) $ map (1 %) $ tail (oeis @290)

instance OEIS 7417 where
  oeis = s [1..] where
     s (x:xs) = x : s (delete (3*x) xs)

instance OEIS 7421 where
  oeisIx = (2 -) . (`mod` 2) . (oeisIx @1222)

instance OEIS 7423 where
  oeisIx = (+ 1) . (oeisIx @8683)

instance OEIS 7464 where
  oeis = 1 : 1 : f [1,1] where
     f xs = y : f (y:xs) where y = sum $ zipWith gcd xs $ reverse xs

instance OEIS 7489 where
  oeis = scanl (+) 0 $ tail (oeis @142)

instance OEIS 7494 where
  oeisIx =  flip div 2 . (+ 1) . (* 3)

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

instance OEIS 7531 where
  oeisIx n = product [n - 2..n]

instance OEIS 7559 where
  oeis = scanl (*) 1 (oeis @16777)

instance OEIS 7583 where
  oeisIx = (`div` 3) . (+ 1) . (oeisIx @4171)

instance OEIS 7590 where
  oeisIx = flip div 2 . (^ 2)

instance OEIS 7598 where
  oeisIx = (^ 2) . (oeisIx @45)

instance OEIS 7604 where
  oeis = concat $ map tail $ tail (tabl @46936)

instance OEIS 7605 where
  oeis = map (oeisIx @7953) (oeis @40)

instance OEIS 7661 where
  oeis = 1 : 1 : 2 : zipWith (*) (oeis @7661) [3..]
instance Table 7661 where
  rowCol n k = (oeis @7661) `genericIndex` n
  tabl = undefined

instance OEIS 7675 where
  oeis = f 1 (oeis @8966) where
     f n (u:xs'@ (v:w:x:xs)) | u == 1 && w == 1 && v == 1 = n : f (n+4) xs
                            | otherwise = f (n+1) xs'

instance OEIS 7745 where
  oeisIx (succ->fi->n) = fi $ n .|. (n ^ 2)

instance OEIS 7805 where
  oeisIx = (`div` 2) . (oeisIx @45) . (* 3) . (+ 1) . (* 2)

instance OEIS 7882 where
  oeisIx (succ->n) = genericLength [ (x, y) | x <- [1..n], y <- [1..n], x^2 + y^2 < n^2]

instance OEIS 7895 where
  oeisIx 0 = 0
  oeisIx n = genericLength $ rowT @35516 n

instance OEIS 7908 where
  oeisIx = fi . (read . concatMap show . enumFromTo 1 :: Integer -> Integer) . fi . succ

instance OEIS 7916 where
  oeis = filter ((== 1) . foldl1 gcd . (rowT @124010)) [2..]

instance OEIS 7953 where
  oeisIx n | n < 10 = n | otherwise = (oeisIx @7953) n' + r where (n',r) = divMod n 10

instance OEIS 7954 where
  oeisIx n | n < 10 = n
           | otherwise = m * (oeisIx @7954) n' where (n', m) = divMod n 10

instance OEIS 7955 where
  oeisIx = product . (rowT @27750) . succ

instance OEIS 8133 where
  oeis = zipWith (*) (tail ts) ts where ts = map (`div` 3) [0..]

instance OEIS 8217 where
  oeis = zipWith (*) (tail qs) qs where qs = map (`div` 4) [0..]

instance OEIS 8233 where
  oeisIx n = product $ map (`div` 4) [n..n+3]

instance OEIS 8280 where
  oeis = tablList @8280
instance Table 8280 where
  tabl = ox True (tabl @8281) where
    ox turn (xs:xss) = (if turn then reverse xs else xs) : ox (not turn) xss

instance OEIS 8281 where
  oeis = tablList @8281
instance Table 8281 where
  tabl = iterate (scanl (+) 0 . reverse) [1]

instance OEIS 8318 where
  oeis = f [1] (S.singleton 1) where
     f xs s =
       m : f (m:xs) (foldl (flip S.insert) s' (map (+ m^2) (map (^ 2) xs)))
       where (m,s') = S.deleteFindMin s

instance OEIS 8486 where
  oeisIx 0 = 1
  oeisIx n = 3 * n
  oeis = 1 : [3, 6 ..]

instance OEIS 8544 where
  oeis = scanl (*) 1 (oeis @16789)

instance OEIS 8545 where
  oeis = scanl (*) 1 (oeis @4767)

instance OEIS 8578 where
  oeis = 1 : (oeis @40)

instance OEIS 8590 where
  oeisIx = (* 8)
  oeis = [0,8..]

instance OEIS 8592 where
  oeisIx = (10 *)

instance OEIS 8615 where
  oeisIx n = n `div` 2 - n `div` 3

instance OEIS 8620 where
  oeisIx = (+ 1) . (`div` 3)
  oeis   = concatMap (replicate 3) [1..]

instance OEIS 8683 where
  oeisIx = fi . mu . snd . unzip . factorise . fi . succ
    where
      mu [] = 1; mu (1:es) = - mu es; mu (_:es) = 0

instance OEIS 8687 where
  oeis = 0 : 1 : c [1] where c (e:es) = e : c (es ++ [e+1,e])

instance OEIS 8805 where
  oeisIx = (oeisIx @217) . (`div` 2) . (+ 2)
  oeis = drop 2 $ concat $ transpose [oeis @217, oeis @217]

instance OEIS 8851 where
  oeis = [10*n + m | n <- [0..], m <- [0,1,5,6]]

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

instance OEIS 9947 where
  oeis = concatMap (\x -> [2 * x, x, 2 * x + 1]) [0..]

instance OEIS 10051 where
  oeis = unfoldr ch (1, (oeis @40)) where
     ch (i, ps'@ (p:ps))
       = Just (fi $ fromEnum (i == p), (i + 1, if i == p then ps else ps'))

instance OEIS 10052 where
  oeis     = concat (iterate (\xs -> xs ++ [0,0]) [1])
  oeisIx n = fi . fromEnum $ oeisIx @196 n ^ 2 == n

instance OEIS 10060 where
  oeis =
     0 : interleave (complement (oeis @10060)) (tail (oeis @10060))
     where complement = map (1 - )
           interleave (x:xs) ys = x : interleave ys xs

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

instance OEIS 10888 where
  oeisIx = until (< 10) (oeisIx @7953)

instance OEIS 11199 where
  oeisIx n = product $ map ((+ 1) . (* n)) [1, 2, 3]

instance OEIS 11371 where
  oeisIx n = n - (oeisIx @120) n

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

instance OEIS 14113 where
  oeis = 0 : 2 : zipWith (+) (map (* 2) (oeis @14113)) (tail (oeis @14113))

instance OEIS 14261 where
  oeis = filter (all (`elem` "13579") . show . fi) [1,3..]

instance OEIS 14263 where
  oeis = filter (all (`elem` "02468") . show . fi) [0,2..]

instance OEIS 14284 where
  oeis = scanl1 (+) (oeis @8578)

instance OEIS 14499 where
  oeisIx = (oeisIx @120) . (oeisIx @40)

instance OEIS 14601 where
  oeis = [x | x <- [0..], mod x 4 `elem` [0, 3]]

instance OEIS 14616 where
  oeisIx (succ->n) = (n * (n + 6) + 1) `div` 4

instance OEIS 16125 where
  oeis = iterate ((+ 1) . (* 12)) 1

instance OEIS 16754 where
  oeis = scanl (+) 1 $ tail (oeis @8590)

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

instance OEIS 18892 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n], n^2 `mod` d == 0]

instance OEIS 20338 where
  oeisIx (succ->n) = fi (read (ns ++ ns) :: Integer) where ns = show $ fi n

instance OEIS 20639 where
  oeisIx (succ->n) = spf primes where
    spf (p:ps) | n < p^2      = n
               | mod n p == 0 = p
               | otherwise    = spf ps

instance OEIS 20652 where
  oeis = map fst [ (u,v) | v <- [1..], u <- [1..v - 1], gcd u v == 1]

instance OEIS 20914 where
  oeisIx = (oeisIx @70939) . (oeisIx @244)

instance OEIS 20915 where
  oeisIx = (oeisIx @81604) . (oeisIx @79)

instance OEIS 20944 where
  oeis = -1 : f [1,0] where f (x:y:xs) = x : f (y:xs ++ [x,x+y])

instance OEIS 20951 where
  oeis = 1 : ws where
     ws = 0 : 1 : concat (zipWith (\u v -> [u, u + v]) ws $ tail ws)

instance OEIS 20985 where
  oeis = 1 : 1 : f (tail (oeis @20985)) (-1) where
     f (x:xs) w = x : x*w : f xs (0 - w)

instance OEIS 20986 where
  oeis = scanl1 (+) (oeis @20985)

instance OEIS 20987 where
  oeisIx = (`div` 2) . (1 -) . (oeisIx @20985)

instance OEIS 20990 where
  oeis = scanl1 (+) $ zipWith (*) (oeis @33999) (oeis @20985)

instance OEIS 22112 where
  oeis = 2 : 6 : zipWith (+) (tail (oeis @22112)) (oeis @22112)

instance OEIS 22155 where
  oeis = map fi $ elemIndices (- 1) (oeis @20985)

instance OEIS 22290 where
  oeisIx 0 = 0
  oeisIx n = h n 0 $ drop 2 (oeis @45) where
     h 0 y _      = y
     h x y (f:fs) = h x' (y + f * r) fs where (x',r) = divMod x 2

instance OEIS 22319 where
  oeis = 1 : 5 : zipWith (+)
     (map (+ 1) (oeis @22319)) (tail (oeis @22319))

(oeis22328, (oeis22329)) = unzip $ f $ S.singleton (1, (0, 0)) where
    f s = (i, j) :
          f (S.insert (2 * y, (i + 1, j)) $ S.insert (3 * y, (i, j + 1)) s')
          where ((y, (i, j)), s') = S.deleteFindMin s

instance OEIS 22328 where
  oeis = oeis22328

instance OEIS 22329 where
  oeis = oeis22329

instance OEIS 22340 where
  oeisIx = (* 2) . (oeisIx @3714)

instance OEIS 22342 where
  oeis = filter ((notElem 1) . (rowT @35516)) [0..]

instance OEIS 22449 where
  oeisIx = (oeisIx @2808 . pred) . (oeisIx @8578)
  oeis = map (oeisIx @2808 . pred) (oeis @8578)

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

instance OEIS 22998 where
  oeisIx n = (oeisIx @34) (n + 1) * n
  oeis = zipWith (*) [0..] $ tail (oeis @34)

instance OEIS 23022 where
  oeisIx ((+2)->n) = genericLength [ (u, v) | u <- [1 .. div n 2],
                               let v = n - u, gcd u v == 1]

instance OEIS 23105 where
  oeisIx n = f (n) where
    f 0 = 1
    f 1 = 2
    f n | even n = 2 * f (n - 1) - 2
    f n | odd  n = 2 * f (n - 1) - 1

instance OEIS 23201 where
  oeis = filter ((== 1) . (oeisIx @10051) . (+ 5)) (oeis @40)

instance OEIS 23416 where
  oeisIx 0 = 1
  oeisIx 1 = 0
  oeisIx n = (oeisIx @23416) n' + 1 - m where (n', m) = divMod n 2
  oeis = 1 : c [0] where c (z:zs) = z : c (zs ++ [z+1,z])

instance OEIS 23431 where
  oeis = 1 : 1 : f [1,1] where
     f xs'@ (x:_:xs) = y : f (y : xs') where
       y = x + sum (zipWith (*) xs $ reverse $ xs')

instance OEIS 23432 where
  oeis = 1 : 1 : f [1,1] where
     f xs'@ (x:_:xs) = y : f (y : xs') where
       y = x + sum (zipWith (*) xs $ reverse $ tail xs)

instance OEIS 23811 where
  oeisIx (succ->n) = foldl (\val dig -> val * n + dig) 0 [0 .. n - 1]

instance OEIS 23855 where
  oeisIx (succ->n) = sum $ zipWith (*) [1 .. div (n+1) 2] [n, n - 1 ..]

instance OEIS 23895 where
  oeisIx = p (oeis @2808) where
     p _          0 = 1
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 24004 where
  oeisIx = (1 -) . (^ 6)

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

instance OEIS 24916 where
  oeisIx (succ->n) = sum $ map (\k -> k * div n k) [1..n]

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
  oeisIx = fi . fromJust . (`elemIndex` (oeis @25427))

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

instance OEIS 25550 where
  oeisIx (succ->n) = numerator $ sum $ map (1 %) $ take (fi n) [1, 3 ..]

instance OEIS 25555 where
  oeis = scanl1 lcm $ tail (oeis @217)

instance OEIS 25613 where
  oeis = f $ S.singleton 1
     where f s = m : (f $ S.insert (3*m) $ S.insert (4*m) s')
               where (m, s') = S.deleteFindMin s

instance OEIS 26741 where
  oeis = concat $ transpose [[0..], [1,3..]]

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

instance OEIS 27611 where
  oeisIx (succ->n) = denominator $ sum $ map (n %) [1..n]

instance OEIS 27612 where
  oeisIx (succ->n) = numerator $ sum $ zipWith (%) [1 .. n] [n, n - 1 .. 1]

instance OEIS 27649 where
  oeis = map fst $ iterate (\ (u, v) -> (3 * u + v, 2 * v)) (1, 1)

instance OEIS 27665 where
  oeisIx (succ->n) = round $ 100000 * log (fi n) / (log 10)

instance OEIS 27748 where
  oeis = 1 : (map (fi.fst) do factorise =<< [2..])

instance Table 27748 where
  rowCol = rowCol_off @27748 @1 @1
  tabl = rowT @27748 <$> [1..]
  rowT 1 = [1]
  rowT n = unfoldr fact n where
     fact 1 = Nothing
     fact x = Just (p, until ((> 0) . (`mod` p)) (`div` p) x)
              where p = (oeisIx @20639) x

instance OEIS 27750 where
  oeis = tablList @27750
instance Table 27750 where
  rowCol n k = (rowT @27750) n `genericIndex` (k - 1)
  rowT = divisors
  tabf = map (rowT @27750) [1..]

instance OEIS 28310 where
  oeisIx n = 0 ^ n + n
  oeis = 1 : [1..]

instance OEIS 28387 where
  oeisIx n = n + (n + 1) ^ 2

instance OEIS 28560 where
  oeisIx n = n * (n + 6)

instance OEIS 28905 where
  oeisIx = (oeisIx @4185) . (oeisIx @40)

instance OEIS 30229 where
--   oeisIx n = (oeis @30229) !! (n - 1)
  oeis = map (fi . succ) $ elemIndices 1 (oeis @8683)

instance OEIS 30308 where
  oeis = tablList @30308
instance Table 30308 where
  -- rowCol n k = (tabf @30308) !! n !! k
  -- rowT n = (tabf @30308) !! n
  tabf = iterate bSucc [0] where
     bSucc []       = [1]
     bSucc (0 : bs) = 1 : bs
     bSucc (1 : bs) = 0 : bSucc bs

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

instance OEIS 31972 where
  oeisIx n = sum $ take (fi n) $ iterate (* n) n

instance OEIS 32766 where
  oeisIx n = div n 2 + n

instance OEIS 33264 where
  oeisIx = f 0 . (rowT @30308) . succ where
     f c [] = c
     f c (0 : 1 : bs) = f (c + 1) bs
     f c (_ : bs) = f c bs

instance OEIS 33307 where
  oeis = concatMap (map (fi . read . return) . show . fi) [1..]

instance OEIS 33581 where
  oeisIx = (* 6) . (^ 2)

instance OEIS 33638 where
  oeisIx = (+ 1) . (`div` 4) . (^ 2)

instance OEIS 33860 where
  oeis = iterate (oeisIx @70196) 1

instance OEIS 33931 where
  oeisIx (succ->n) = lcm n (lcm (n + 1) (n + 2))

instance OEIS 33942 where
  oeis = map (+1) $ filter ((> 2) . (oeisIx @1222)) [1..]

instance OEIS 33949 where
  oeis = filter (\x -> any ((== 1) . (`mod` x) . (^ 2)) [2 .. x - 2]) [1..]

instance OEIS 33992 where
  oeis = map (+1) $ filter ((== 3) . (oeisIx @1221)) [1..]

instance OEIS 33999 where
  oeisIx = (1 -) . (* 2) . (`mod` 2)
  oeis = cycle [1,-1]

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

instance OEIS 36988 where
  oeisIx = (oeisIx @63524) . (oeisIx @36989)

instance OEIS 36989 where
  oeis = 1 : concat (transpose
     [map (+ 1) (oeis @36989), map ((max 1) . pred) $ tail (oeis @36989)])

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

instance OEIS 37015 where
  oeis = filter (all (> 0) . ds) [0..] where
     ds x = zipWith (-) (tail gs) gs where
        gs = map length $ group $ (rowT @30308) x

instance OEIS 37016 where
  oeis = 0 : filter
     (all (>= 0) . (\x -> zipWith (-) (tail $ rls x) $ rls x)) [1..] where
         rls = map length . group . unfoldr
               (\x -> if x == 0 then Nothing else Just $ swap $ divMod x 2)

instance OEIS 37124 where
  oeis = f [1..9] where f (x:xs) = x : f (xs ++ [10*x])

instance OEIS 37201 where
  oeis = f (oeis @1223) where
     f (x:xs@ (x':_)) | x == x'   = f xs
                     | otherwise = x : f xs

oeis37372_37407 a b
  = filter f [1..] where
      f x = null $ nub (ds a x) \\ nub (ds b x)
      ds b x = if x > 0 then d : ds b x' else []  where (x', d) = divMod x b

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

instance OEIS 38664 where
  oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @1223)) . (* 2) . succ

instance OEIS 38712 where
  oeisIx (succ-> (fi->n)) = fi (n `xor` (n - 1) :: Integer)

instance OEIS 39943 where
  oeis = [0,1,4,16,20,37,42,58,89,145]

instance OEIS 39995 where
  oeisIx n = fi . sum $
     map (oeisIx @10051) $ nub $ map (read :: String -> Integer) (tail $ subsequences $ show $ fi n)

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

instance OEIS 43537 where
  oeisIx = genericLength . nub . show . fi . succ

instance OEIS 45661 where
  oeisIx (succ->n) = product [n'+d | d <- [1..n], let (n',m) = divMod n d, m == 0]

instance OEIS 45776 where
  oeis = iterate f 1 where
     f x = head $ dropWhile (<= x) [q,2*q..] where q = (oeisIx @7953) x

instance OEIS 45887 where
  oeisIx n = genericLength $ filter (`isInfixOf` (show $ fi n)) $ map (show . fi) [0,2..n - 1]

instance OEIS 45888 where
  oeisIx (succ->n) = genericLength $ filter (`isInfixOf` (show $ fi n)) $ map (show . fi) [1,3..n - 1]

instance OEIS 45896 where
  oeisIx n = denominator $ n % ((n + 1) * (n + 2))

instance OEIS 45925 where
  oeis = zipWith (*) [0..] (oeis @45)

instance OEIS 45926 where
  oeis = filter (all (`elem` "2468") . show . fi) [2, 4..]

instance OEIS 45928 where
  oeisIx (succ->n) = 3 * n - 2 * floor (1 + sqrt (fi n - 1))

instance OEIS 46034 where
  oeis = filter (all (`elem` "2357") . show . fi ) [0..]

instance OEIS 46109 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 == n^2]

instance OEIS 46523 where
  oeisIx = product . zipWith (^) (oeis @40) . reverse . sort . (rowT @124010) . succ

instance OEIS 46901 where
  oeis = scanl1 (\u v -> if u > v then u - v else u + v) [1..]

instance OEIS 46933 where
  oeis = map (subtract 1) (oeis @1223)

instance OEIS 46934 where
  oeis = tablList @46934
instance Table 46934 where
  tabl = [1] : iterate (\row -> scanl (+) (last row) row) [0,1]

instance OEIS 46935 where
  oeis = concatMap tail $ tail (tabl @46934)

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

instance OEIS 47211 where
  oeis = filter ((`elem` [2,4]) . (`mod` 5)) [1..]

instance OEIS 47229 where
  oeis = filter ((`notElem` [1,5]) . (`mod` 6)) [0..]

instance OEIS 47471 where
  oeis = [n | n <- [1..], mod n 8 `elem` [1,3]]

instance OEIS 47476 where
  oeis = [n | n <- [0..], mod n 8 <= 3]

instance OEIS 47566 where
  oeis = [n | n <- [1..], mod n 8 > 3]

instance OEIS 47726 where
  oeisIx n = genericLength $ nub $ permutations $ show $ fi $ succ n

instance OEIS 47800 where
  oeisIx n = genericLength $ nub [i^2 + j^2 | i <- [0..n], j <- [i..n]]

instance OEIS 48105 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n], mod n d == 0, gcd d (n `div` d) > 1]

instance OEIS 48158 where
  oeis = tablList @48158
instance Table 48158 where
  rowCol = mod
  rowT   = rowT_off @48158 @1
  tabl   = zipWith (map . mod) [1..] (tabl @2260)

instance OEIS 48724 where
  oeisIx (fi->n) = fi do n `xor` shiftL n 1 :: Integer

instance OEIS 48881 where
  oeis = c [0] where c (x:xs) = x : c (xs ++ [x,x+1])

instance OEIS 48896 where
  oeis = f [1] where f (x:xs) = x : f (xs ++ [x,2*x])

instance OEIS 48993 where
  oeis = tablList @48993
instance Table 48993 where
  tabl = iterate (\row ->
     [0] ++ (zipWith (+) row $ zipWith (*) [1..] $ tail row) ++ [1]) [1]

instance OEIS 49001 where
  oeisIx = subtract 2 . (oeisIx @1248)

instance OEIS 49341 where
  oeis = 3 : 6 : map (oeisIx @7953) (zipWith (+) (oeis @49341) $ tail (oeis @49341))

instance OEIS 49363 where
  oeisIx (succ->n) = foldl (\v d -> n * v + d) 0 (1 : 0 : [2..n - 1])

instance OEIS 49451 where
  oeisIx n = n * (3 * n + 1)

instance OEIS 49472 where
  oeisIx = floor . (/ sqrt 2) . fi

instance OEIS 49474 where
  oeisIx = ceiling . (/ sqrt 2) . fi

instance OEIS 50407 where
  oeisIx n = n * (n ^ 2 - 6 * n + 11) `div` 6

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

instance OEIS 51132 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 < n^2]

instance OEIS 51176 where
  oeisIx n = if m == 0 then n' else n  where (n',m) = divMod n 3

instance OEIS 51190 where
  oeisIx (succ->n) = product $ map (gcd n) [1..n - 1]

instance OEIS 51426 where
  oeisIx (succ->n) = foldl lcm 1 [2,4..2*n]

instance OEIS 51631 where
  oeis = tablList @51631
instance Table 51631 where
  tabl = iterate (\row -> zipWith (+) ([1] ++ row) (row ++[1])) [-1]

instance OEIS 51674 where
  oeis = map (\p -> p ^ p) (oeis @40)

instance OEIS 51731 where
  oeis = tablList @51731
instance Table 51731 where
  rowCol n k = 0 ^ mod n k
  rowT   = rowT_off @51731 @1
  tabl = map (map (oeisIx @7)) (tabl @48158)

instance OEIS 51885 where
  oeisIx n = (m + 1) * 10^n' - 1 where (n',m) = divMod n 9

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

instance OEIS 52901 where
  oeis = cycle [3,2,2]

instance OEIS 53187 where
  oeis = 0 : concatMap (\x -> genericReplicate (2*x) (x ^ 2)) [1..]

instance OEIS 53621 where
  oeisIx = round . (\x -> x / (log x - 1)) . fi . succ

instance OEIS 53645 where
  -- oeisIx 0 = 0
  -- oeisIx n = 2 * (oeisIx @53645) n' + b  where (n', b) = divMod n 2
  oeis = concatMap (0 `enumFromTo`) (oeis @225)

instance OEIS 53696 where
  oeis = filter ((> 1) . (oeisIx @88323) . subtract 2) [2..]

instance OEIS 53755 where
  oeisIx = (+ 1) . (* 4) . (^ 2)

instance OEIS 54028 where
  oeisIx (succ.succ->n) = head [k | k <- [2..], 2^k >= k^n]

instance OEIS 54868 where
  oeisIx = (oeisIx @120) . (oeisIx @120)

instance OEIS 55067 where
  oeisIx (succ->n) = product [k | k <- [1..n], mod n k /= 0]

instance OEIS 55098 where
  oeisIx (succ->n) = genericLength $ nub $ filter ((> '0') . head) $ permutations $ show $ fi n

instance OEIS 55612 where
  oeisIx = product . map (+ 1) . tail . rowT @7318 . fi

instance OEIS 55640 where
  oeisIx n = genericLength $ filter (/= '0') $ show $ fi n

instance OEIS 55874 where
  oeisIx (succ->n) = genericLength $ takeWhile ((== 0) . (mod n)) [1..]

instance OEIS 55897 where
  oeisIx (succ->n) = n * (n - 1) ^ (n - 1)

instance OEIS 56106 where
  oeisIx n = n * (3 * n - 1) + 1

instance OEIS 56169 where
  oeisIx = genericLength . filter (== 1) . (rowT @124010 . succ)

instance OEIS 56170 where
  oeisIx = genericLength . filter (> 1) . (rowT @124010 . succ)

instance OEIS 57078 where
  oeisIx = (1 -) . (`mod` 3)

instance OEIS 57168 where
  oeis = f 2 $ tail (oeis @120) where
     f x (z:zs) = (x + genericLength (takeWhile (/= z) zs)) : f (x + 1) zs

instance OEIS 57655 where
  oeisIx n = genericLength [ (x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 <= n]

instance OEIS 57918 where
  oeisIx (succ->n) = sum $ map ((0 ^) . (`mod` n) . (^ 2)) [1..n - 1]

instance OEIS 58084 where
  oeisIx (succ->n) = fi . fromJust $ findIndex (elem n) (tabl @7318)

instance OEIS 58207 where
  oeis = f [0,1,2,3,2] where f xs = xs ++ f (map (+ 1) xs)

instance OEIS 58212 where
  oeisIx n = 1 + n * (n - 3) `div` 6

instance OEIS 60692 where
  oeisIx (succ->n) = uncurry (+) $ divMod (3 ^ n) (2 ^ n)

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

instance OEIS 61293 where
  oeisIx = floor . (** exp 1) . fi . succ

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

instance OEIS 62725 where
  oeisIx n = n * (9 * n + 5) `div` 2

instance OEIS 62806 where
  oeisIx (succ->n) = sum $ zipWith (*) [1..n] $ iterate (* n) n

instance OEIS 62813 where
  oeisIx (succ->n) = foldr (\dig val -> val * n + dig) 0 [0 .. n - 1]

instance OEIS 63524 where
  oeisIx = fi . fromEnum . (== 1)

instance OEIS 63985 where
  oeisIx (succ->n) = genericLength [ ()| x <- [1..n], y <- [x..n], gcd x y > 1]

instance OEIS 64680 where
  oeis = zipWith ($) (cycle [ (`div` 2), (* 2)]) [0..]

instance OEIS 65033 where
  oeisIx n = 0 ^ n + div (n + 1) 2

instance OEIS 65039 where
  oeisIx n = sum $ map (fi . read) $ tail $ inits $ show $ fi n

instance OEIS 65075 where
  oeis = 1 : 1 : f 2 where
     f x = y : f (x + y) where y = (oeisIx @7953) x

instance OEIS 65076 where
  oeis = 0 : 1 : zipWith (+)
                  (oeis @65076) (map (oeisIx @7953) $ tail (oeis @65076))

instance OEIS 65502 where
  oeis = filter ((> 1) . (gcd 10)) [1..]

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

instance OEIS 67079 where
  oeisIx = product . map (fi . read) . init . tails . show . fi

instance OEIS 67251 where
  oeis = filter ((> 0) . flip mod 10) [0..]

instance OEIS 67259 where
  oeis = map (+1) $ filter ((== 2) . (oeisIx @51903)) [1..]

instance OEIS 68475 where
  oeisIx n = sum $ zipWith (*) [1..n] $ iterate (* n) 1

instance OEIS 68498 where
  oeis = f [0..] (oeis @45) where
     f (u:us) (v:vs) = if u < (oeisIx @7953) v then v : f us vs else f us vs

instance OEIS 68722 where
  oeisIx n = (1 + 2 * n + 2 * n ^ 2) * (1 + 3 * n + 3 * n ^ 2)

instance OEIS 69482 where
  oeis = zipWith (-) (tail (oeis @1248)) (oeis @1248)

instance OEIS 70196 where
  oeisIx n = n + (oeisIx @4185) n

instance OEIS 70198 where
  oeis = map (subtract 1) $ scanl lcm 1 [2..]

instance OEIS 70635 where
  oeisIx (succ->n) = n `mod` (oeisIx @7953 n)

instance OEIS 70939 where
  oeisIx n = if n < 2 then 1 else (oeisIx @70939) (n `div` 2) + 1
  oeis = 1 : 1 : l [1] where
     l bs = bs' ++ l bs' where bs' = map (+ 1) (bs ++ bs)

instance OEIS 71178 where
  oeisIx = last . (rowT @124010) . succ

instance OEIS 71222 where
  oeisIx n = head [k | k <- [1..], gcd (n + 1) (k + 1) == gcd n k]

instance OEIS 71364 where
  oeisIx = product . zipWith (^) (oeis @40) . (rowT @124010) . succ

instance OEIS 72065 where
  oeis = filter ((`elem` [0,2,9,11]) . (`mod` 12)) [0..]

instance OEIS 72502 where
  oeis = f (S.singleton 9) $ drop 2 (oeis @1248) where
     f s (x:xs) = m : f (S.insert (2 * m) $ S.insert x s') xs where
                  (m,s') = S.deleteFindMin s

instance OEIS 72587 where
  oeis = tail $ filter (any even . (rowT @124010)) [1..]

instance OEIS 72588 where
  oeis = filter f [1..] where
     f x = any odd es && any even es  where es = (rowT @124010) x

instance OEIS 73015 where
  oeis = iterate (\x -> (x - 1) ^ 2) 3

instance OEIS 73776 where
  oeis = 1 : f [1] where
     f xs = y : f (y : xs) where y = sum $ zipWith (*) xs ms
     ms = map negate $ tail (oeis @8683)

instance OEIS 74066 where
  oeis = 1 : xs where xs = 4 : 3 : 2 : map (+ 3) xs

instance OEIS 74067 where
  oeis = 1 : 2 : xs where xs = 7 : 6 : 5 : 4 : 3 : map (+ 5) xs

instance OEIS 75311 where
  oeis = 1 : f 2 [1] where
     f x ys = if (oeisIx @120) x `elem` ys then f (x + 1) ys
                                     else x : f (x + 1) (x : ys)

instance OEIS 75517 where
  oeis = [0..9] ++ f 1 [0..9] where
     f x ys = if (oeisIx @7953) x `elem` ys then f (x + 1) ys
                                     else x : f (x + 1) (x : ys)

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

instance OEIS 76467 where
  oeis = 1 : filter ((> 2) . foldl1 gcd . (rowT @124010)) [2..]

instance OEIS 76493 where
  oeisIx n = genericLength $ (intersect `on` nub . show . fi) n (n^2)

instance OEIS 77436 where
  oeis = map fi $ elemIndices 0
     $ zipWith ((-) `on` (oeisIx @120)) [0..] (oeis @290)

instance OEIS 78608 where
  oeisIx = ceiling . (2 /) . (subtract 1) . (2 **) . recip . fi . succ

instance OEIS 78633 where
  oeisIx (succ->n) = 2 * n + ceiling (2 * sqrt (fi n))

instance OEIS 79578 where
  oeisIx (succ->n) = head [m | m <- [n + 2 ..], gcd m n == 1]

instance OEIS 79588 where
  oeisIx n = product $ map ((+ 1) . (* n)) [1, 2, 4]

instance OEIS 79704 where
  oeisIx = (* 2) . (oeisIx @1248)

instance OEIS 79944 where
  oeis =  f [0,1] where f (x:xs) = x : f (xs ++ [x,x])

instance OEIS 79978 where
  oeisIx = fi . fromEnum . (== 0) . (`mod` 3)
  oeis   = cycle [1,0,0]

instance OEIS 81604 where
  oeisIx n = if n < 3 then 1 else (oeisIx @81604) (div n 3) + 1

instance OEIS 81757 where
  oeisIx (succ->n) = genericLength [ () | j <- [2..n], i <- [1..j - 1], i * j + i - j == n]

instance OEIS 83329 where
  oeis = 1 : iterate ((+ 1) . (* 2)) 2

instance OEIS 83420 where
  oeisIx = subtract 1 . (* 2) . (4 ^)

instance OEIS 83542 where
  oeisIx n = (oeisIx @10) n * (oeisIx @10) (n + 1)
  oeis = zipWith (*) (tail (oeis @10)) (oeis @10)

instance OEIS 83652 where
  oeis = scanl1 (+) (oeis @70939)

instance OEIS 84228 where
  oeis = 1 : 2 : f 3 where
     f x = y : f (x + y) where y = (oeisIx @7953) x

instance OEIS 84640 where
  oeis = 0 : 1 : (map (+ 4) $
     zipWith (+) (map (* 2) (oeis @84640)) (tail (oeis @84640)))

instance OEIS 84662 where
  oeis = 4 : zipWith (+) (oeis @84662) (zipWith gcd (oeis @84662) [2..])

instance OEIS 84663 where
  oeis = 8 : zipWith (+) (oeis @84663) (zipWith gcd (oeis @84663) [2..])

instance OEIS 84920 where
  oeisIx n = (p - 1) * (p + 1) where p = (oeisIx @40) n

instance OEIS 84921 where
  oeisIx n = lcm (p - 1) (p + 1)  where p = (oeisIx @40) n

instance OEIS 84984 where
  oeis = filter (not . any (`elem` "2357") . show . fi) [0..]

instance OEIS 85059 where
  oeis = 1 : f 1 1 where
     f v w = y : f (v + 1) y where
       y = if w > v then w - v else w + v

instance OEIS 85084 where
  oeis = 1 : f 1 (oeis @2808) where
     f x cs = y : f y (delete y cs) where
              y = fromJust $ find ((== 1) . (gcd x)) cs

instance OEIS 85104 where
  oeis = filter ((> 1) . (oeisIx @88323 . subtract 2)) (oeis @40)

instance OEIS 85144 where
  oeis = 0 : concat
     (transpose [map negate (oeis @85144), map (+ 1) $ tail (oeis @85144)])

instance OEIS 86099 where
  oeisIx (fi->n) = fi do foldl1 (.|.) $ zipWith (.&.) [0..] $ reverse [0..n] :: Integer

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

instance OEIS 87811 where
  oeisIx (succ->n) = (n + n `mod` 2) * (n + 2 - n `mod` 2) `div` 4

instance OEIS 88157 where
  oeisIx n = mod (div (n ^ n) (60 ^ n)) 60

instance OEIS 88323 where
  oeisIx ((+2)->n) = sum $ map (f n) [2 .. n - 1] where
     f x b = if x == 0 then 1 else if d /= 1 then 0 else f x' b
                                   where (x',d) = divMod x b

instance OEIS 88705 where
  oeis = 0 : zipWith (-) (tail (oeis @120)) (oeis @120)

instance OEIS 89128 where
  oeisIx = gcd 6

instance OEIS 89633 where
  oeis = [2 ^ t - 2 ^ k - 1 | t <- [1..], k <- [t - 1,t - 2..0]]

instance OEIS 91090 where
  oeis = 1 : f [1,1] where f (x:y:xs) = y : f (x:xs ++ [x,x+y])

instance OEIS 92338 where
  oeisIx (succ->n) = genericLength $ filter (<= 1) $ map (mod n) [1..n]

instance OEIS 92410 where
  oeis = zipWith (+) (oeis @8683) $ tail (oeis @8683)

instance OEIS 93391 where
  oeisIx n = sum $ map ((flip div 16) . (+ n)) [0..3]

instance OEIS 93485 where
  oeisIx n = (9 * n * (3 * n + 1) + 2) `div` 2

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

instance OEIS 97080 where
  oeisIx (succ->n) = 2 * n * (n - 1) + 3

instance OEIS 97764 where
  oeis = f 0 (S.singleton (4, 2, 2)) $
                   tail $ zip (oeis @51674) (oeis @40) where
     f m s ppps'@ ((pp, p) : ppps)
       | pp < qq   = f m (S.insert (pp, p, 2) s) ppps
       | qq == m   = f m (S.insert ((k * q) ^ q, q, k + 1) s') ppps'
       | otherwise = qq : f qq (S.insert ((k * q) ^ q, q, k + 1) s') ppps'
       where ((qq, q, k), s') = S.deleteFindMin s

instance OEIS 99996 where
  oeisIx = foldl lcm 1 . enumFromTo 2 . (* 2)

instance OEIS 100613 where
  oeisIx (succ->n) = genericLength [ ()| x <- [1..n], y <- [1..n], gcd x y > 1]

instance OEIS 100830 where
  oeisIx (succ->n)  = n + 9 * (-1) ^ ((n - 1) `div` 9)

instance OEIS 101986 where
  oeisIx n = sum $ zipWith (*) [1,3..] (reverse [2..n+1])

instance OEIS 102683 where
  oeisIx =  genericLength . filter (== '9') . show . fi

instance OEIS 103127 where
  oeis = [x | x <- [1..], x `mod` 16 `elem` [1,3,5,15]]

instance OEIS 103586 where
  oeisIx n = (oeisIx @70939) (n + (oeisIx @70939) n)
  oeis = 1 : concat
     (zipWith (replicate . fromInteger) (tail (oeis @225)) [2..])

instance OEIS 104249 where
  oeisIx n = n* (3*n+1) `div` 2 + 1

instance OEIS 105279 where
  oeis = iterate ((* 10) . (+ 1)) 0

instance OEIS 105317 where
  oeis = 0 : 1 : h 1 (drop 4 (oeis @45)) (S.singleton (2, 2)) where
    h y xs'@ (x:xs) s
      | x < ff    = h y xs (S.insert (x, x) s)
      | ff == y   = h y xs' s'
      | otherwise = ff : h ff xs' (S.insert (f * ff, f) s')
      where ((ff, f), s') = S.deleteFindMin s

instance OEIS 106318 where
  oeisIx = (* 2) . (^ 6) . succ

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

instance OEIS 107846 where
  oeisIx = genericLength . concatMap tail . group . sort . show . fi

instance OEIS 108398 where
  oeisIx n = n * (1 + n ^ n) `div` 2

instance OEIS 108411 where
  oeisIx = (3 ^) . flip div 2

instance OEIS 109045 where
  oeisIx = lcm 4

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

instance OEIS 113630 where
  oeisIx n = sum $ zipWith (*) [1..9] $ iterate (* n) 1

instance OEIS 116385 where
  oeisIx n = (rowCol @51631) (n+1) $ (n+1) `div` 2

instance OEIS 116470 where
  oeis = 0 : 1 : 2 : concat (transpose [drop 4 (oeis @45), drop 3 (oeis @32)])

instance OEIS 116472 where
  oeisIx = (oeisIx @149) . (* 2)

instance OEIS 116478 where
  oeis = 1 : f [2..] [1] where
     f (x:xs) ys = y : f xs (y:ys) where y = sum $ map (div x) ys

instance OEIS 116520 where
  oeis = 0 : zs where
     zs = 1 : (concat $ transpose
                        [zipWith (+) vs zs, zipWith (+) vs $ tail zs])
        where vs = map (* 4) zs

instance OEIS 118950 where
  oeis = filter (any (`elem` "2357") . show . fi) [0..]

instance OEIS 120004 where
  oeisIx n = fi . sum $ map (fromEnum . (`isInfixOf` show do fi n) . show . fi) [0..n]

instance OEIS 121022 where
  oeis = filter (('2' `elem`) . show . fi) [0, 2 ..]

instance OEIS 121384 where
  oeisIx = ceiling . (* exp 1) . fi

instance OEIS 122196 where
  oeis = concatMap (\x -> enumFromThenTo x (x - 2) 1) [1..]

instance OEIS 123010 where
  oeis = 1 : 0 : 4 : 16 : f 0 4 16
    where f a b c = let x = 5*c + b - 5*a in x : f b c x

instance OEIS 123684 where
  oeis = concat $ transpose [oeis @16777, (oeis @27)]

instance OEIS 123866 where
  oeisIx = (subtract 1) . (^ 6) . (+1)

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

instance OEIS 128918 where
  oeisIx n = (n + m - 1) * n' + m * n - m + 1  where (n', m) = divMod n 2

instance OEIS 129296 where
  oeisIx (succ->n) = genericLength [d | d <- [1..n], (n ^ 2 - 1) `mod` d == 0]

instance OEIS 131179 where
  oeisIx n = (n + 1 - m) * n' + m  where (n', m) = divMod n 2

instance OEIS 131524 where
  oeis = concat $ transpose [tail (oeis @71), tail (oeis @71)]

instance OEIS 132141 where
  oeis = filter ((== 1) . until (< 3) (flip div 3)) [1..]

instance OEIS 134734 where
  oeis = zipWith (-) (tail (oeis @84662)) (oeis @84662)

instance OEIS 134735 where
  oeis = concat $ transpose [oeis @40, oeis @1223]

instance OEIS 134736 where
  oeis = 5 : zipWith (+) (oeis @134736) (zipWith gcd (oeis @134736) [2..])

instance OEIS 134743 where
  oeis = zipWith (-) (tail (oeis @134736)) (oeis @134736)

instance OEIS 134744 where
  oeis = zipWith (-) (tail (oeis @84663)) (oeis @84663)

instance OEIS 136392 where
  oeisIx (succ->n) = 2 * n * (3*n - 5) + 5

instance OEIS 136399 where
  oeis = filter (any (> '1') . show . fi) [0..]

instance OEIS 136409 where
  oeisIx = floor . (* logBase 3 2) . fi

instance OEIS 136412 where
  oeisIx = (`div` 3) . (+ 1) . (* 5) . (4 ^)

instance OEIS 137688 where
  oeis = concat $ zipWith ($) (map replicate [1..]) (map (2^) [0..])

instance OEIS 138791 where
  oeisIx n = (fi . fromJust $ elemIndex n (oeis @70635)) + 1

instance OEIS 141046 where
  oeisIx = (* 4) . (^ 4)

instance OEIS 143164 where
  oeis = filter ((== 13) . (oeisIx @7953)) [0..]

instance OEIS 143689 where
  oeisIx n = n* (3*n - 1) `div` 2 + 1

instance OEIS 153158 where
  oeis = filter ((== 2) . foldl1 gcd . (rowT @124010)) [2..]

instance OEIS 155587 where
  oeis = scanl (+) 1 (oeis @108)

instance OEIS 156301 where
  oeisIx = ceiling . (* logBase 3 2) . fi

instance OEIS 157671 where
  oeis = filter ((== 2) . until (< 3) (flip div 3)) [1..]

instance OEIS 161382 where
  oeis = concatMap (\x -> genericReplicate (x ^ 2) (1 - mod x 2)) [1..]

instance OEIS 163617 where
  oeisIx (fi->n) = fi do n .|. shiftL n 1 :: Integer

instance OEIS 163866 where
  oeis = scanl1 (+) $ concat (tabl @7318)

instance OEIS 164514 where
  oeis = 1 : (oeis @37)

instance OEIS 164632 where
  oeis = 1 : concatMap (\x -> genericReplicate (2^ (2*x - 1)) (2^x)) [1..]

instance OEIS 165900 where
  oeisIx n = n * (n - 1) - 1

instance OEIS 166060 where
  oeis = map fst $ iterate (\ (u, v) -> (3 * (u + v), 2 * v)) (1, 1)

instance OEIS 166370 where
  oeis = filter ((== 17) . (oeisIx @7953)) [0..]

instance OEIS 166459 where
  oeis = filter ((== 19) . (oeisIx @7953)) [0..]

instance OEIS 168046 where
  oeisIx = fi . fromEnum . ch0 where
     ch0 x = x > 0 && (x < 10 || d > 0 && ch0 x') where (x', d) = divMod x 10

instance OEIS 169810 where
  oeisIx (fi->n) = fi do n ^ 2 `xor` n :: Integer

instance OEIS 171974 where
  oeisIx = floor . (/ 3) . (* sqrt 6) . fi . succ

instance OEIS 171975 where
  oeisIx = floor . (/ 4) . (* sqrt 6) . fi . succ

instance OEIS 173639 where
  oeis = filter (odd . (oeisIx @7953) . (* 11)) [0..]

instance OEIS 174452 where
  oeisIx = (`mod` 1000) . (^ 2)

instance OEIS 176059 where
  oeisIx = (3 -) . (`mod` 2)

instance OEIS 178788 where
  oeisIx (fi->n) = fi .fromEnum $ nub (show n) == show n

instance OEIS 179070 where
  oeis = 1 : zs where zs = 1 : 1 : 3 : zipWith (+) zs (drop 2 zs)

instance OEIS 180410 where
  oeisIx = fi . read . sort . nub . show . fi . succ

instance OEIS 181482 where
  oeis = scanl1 (+) $ zipWith (*) [1..] $ cycle [1, 1, -1]

instance OEIS 181765 where
  oeisIx (succ->n) = genericLength [xs | xs <- subsequences [-n..n], sum xs > 0]

instance OEIS 182323 where
  oeis = filter ((== 43) . (`mod` 97) . (^ 2)) [0..]

instance OEIS 182469 where
  oeis = tablList @182469
instance Table 182469 where
  rowCol = rowCol_off @182469 @1 @1
  rowT = rowT @27750 . oeisIx @265
  tabf = map (rowT @182469) [0..]

instance OEIS 185549 where
  oeisIx = ceiling . (** (3 / 2)) . fi

instance OEIS 185670 where
  oeisIx (fi->succ->n) = genericLength [ (x,y) | x <- [1..n - 1], y <- [x+1..n], gcd x y > 1]

instance OEIS 185950 where
  oeisIx n = (4 * n - 1) * n - 1

instance OEIS 186809 where
  oeis = cycle [0, 1, 2, 0, -2, -1]

instance OEIS 187844 where
  oeisIx n = product $ map (negate . fi . digitToInt) $ show $ fi n

instance OEIS 188172 where
  oeisIx (succ->fi->n) = genericLength $ filter ((== 0) . mod n) [7,15..n]

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

instance OEIS 189144 where
  oeisIx n = (foldl1 lcm [n..n+6]) `div` 420

instance OEIS 193238 where
  oeisIx n = genericLength $ filter (`elem` "2357") $ show $ fi n

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

instance OEIS 195150 where
  oeisIx (succ->fi->n) = genericLength [d | d <- [3..n], mod n d == 0, mod n (d - 1) /= 0]

instance OEIS 196563 where
  oeisIx (fi->n) = genericLength [d | d <- show n, d `elem` "02468"]

instance OEIS 196564 where
  oeisIx (fi->n) = genericLength [d | d <- show n, d `elem` "13579"]

instance OEIS 199682 where
  oeisIx = (+ 1) . (* 2) . (10 ^)

instance OEIS 199799 where
  oeis = [x | x <- [1..111111], gcd x 111111 == 1]

instance OEIS 202138 where
  oeis = [17, 78, 19, 23, 29, 77, 95, 77, 1, 11, 13, 15, 1, 55]

instance OEIS 203363 where
  oeis = [91, 85, 51, 38, 33, 29, 23, 19, 17, 13, 11, 2, 7, 1]

instance OEIS 204457 where
  oeis = [x | x <- [1, 3 ..], mod x 13 > 0]

instance OEIS 204674 where
  oeisIx n = n * (n * (4 * n + 5) + 2) + 1

instance OEIS 204675 where
  oeisIx n = 2 * n * (8 * n + 1) + 1

instance OEIS 211866 where
  oeisIx = (flip div 4) . (subtract 5) . (9 ^) . succ

instance OEIS 213541 where
  oeisIx (fi->n) = fi do n .&. n ^ 2 :: Int

instance OEIS 214085 where
  oeisIx n = n^2 * (n^4 - n^2 + n + 1) `div` 2

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

instance OEIS 215646 where
  oeisIx n = n * (n * (11*n + 6) + 1) `div` 6

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

instance OEIS 219531 where
  oeisIx = sum . take 12 . rowT @7318

instance OEIS 219606 where
  oeis = concat $ transpose [oeis @1223, oeis @40]

instance OEIS 221639 where
  oeisIx n = 5 ^ (5 * n + 1) + 4 ^ (5 * n + 2) + 3 ^ (5 * n)

instance OEIS 226538 where
  oeis = concat $ transpose [drop 2 (oeis @71), tail (oeis @1911)]

instance OEIS 227362 where
  oeisIx = fi . (read . reverse . sort . nub . show :: Integer -> Integer) . fi

instance OEIS 228056 where
  oeis = filter f [1..] where
     f x = length us == 1 && (head us > 1 || not (null vs)) where
           (us,vs) = Data.List.partition odd $ (rowT @124010) x

instance OEIS 229762 where
  oeisIx (fi->n) = fi do (n `xor` shiftR n 1) .&. shiftR n 1 :: Int

instance OEIS 229763 where
  oeisIx (fi->n) = fi do (shiftL n 1 `xor` n) .&. n :: Int

instance OEIS 230089 where
  oeisIx (succ->n) = if odd n then n else if mod n 4 == 0 then 4 else 2

instance OEIS 230099 where
  oeisIx n = (oeisIx @7954) n + n

instance OEIS 230102 where
  oeis = iterate (oeisIx @230099) 1

instance OEIS 235052 where
  oeisIx (succ->n) = head [x | x <- [2..], show (fi x) `isInfixOf` (show $ fi $ x ^ n)]

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

instance OEIS 235702 where
  oeisIx (succ->n) = if n == 1 then 1 else 24 * 5 ^ (n - 2)
  oeis = 1 : iterate (* 5) 24

instance OEIS 235933 where
  oeis = filter ((== 1) . gcd 35) [1..]

instance OEIS 236046 where
  oeisIx ((+2)->n) = head [x | x <- [2..], not $ show (fi x) `isInfixOf` (show $ fi $ x ^ n)]

instance OEIS 238324 where
  oeis = scanl1 (\u v -> if u > v then u - v else u + v) [1, 3 ..]

instance OEIS 239426 where
  oeisIx n = (((21 * n - 36) * n + 25) * n - 8) * n + 1

instance OEIS 239449 where
  oeisIx n = (7 * n - 5) * n + 1

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

instance OEIS 242216 where
  oeisIx = p [1,2,3,7,11,19,43,67,163] where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 242217 where
  oeisIx = p [1,2,3,7,11,19,43,67,163] where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 242627 where
  oeisIx n = genericLength $ filter ((== 0) . mod n) [1..9]

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

instance OEIS 245940 where
  oeisIx n = n^3 * (2 * n^3 + 2 * n^2 - 3 * n - 1) * (n + 1) `div` 24

instance OEIS 245941 where
  oeisIx n = n * (16*n^5 - 24*n^4 + 2*n^3 + 11*n^2 - 6*n + 1) `div` 6

instance OEIS 247616 where
  oeis = filter f [100 .. 9876543210] where
     f x = head vs /= 0 && all (== 0) ws where
           ws = zipWith (-) (tail vs) vs
           vs = zipWith (-) (tail us) us
           us = map (read . return) $ show $ fi x

instance OEIS 247665 where
  oeis = 2 : 3 : f [3] [4..] where
     f (x:xs) zs = ys ++ f (xs ++ ys) (zs \\ ys) where
       ys = [v, head [w | w <- vs, gcd v w == 1]]
       (v:vs) = filter (\u -> gcd u x == 1 && all ((== 1) . (gcd u)) xs) zs

instance OEIS 247796 where
  oeisIx = f 0 where
     f s 0 = s
     f s x = if s + d < 10 then f (s + d) x' else (f d x') * 10 + s
             where (x', d) = divMod x 10

instance OEIS 249357 where
  oeis = 1 : 2 : 3 : f 2 3 where
     f u v = y : f v y where
       y = head [x | x <- [u + v ..], gcd x u > 1, gcd x v == 1]

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
  oeisIx = sum . map (flip mod 2) . (uss `genericIndex`)

instance OEIS 249857 where
  oeisIx = sum . map ((1 -) . flip mod 2) . (uss `genericIndex`) . succ

instance OEIS 254077 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v ws = g ws where
       g (x:xs) = if gcd x u > gcd x v then x : f v x (delete x ws) else g xs

instance OEIS 254429 where
  oeis = iterate ((+ 1) . (2 ^)) 0

instance OEIS 254649 where
  oeis = f (oeis @7376) [0] where
     f (x:xs) ys = g x xs where
       g y zs'@ (z:zs) | y `elem` ys = g (y + z) zs
                      | otherwise   = y : f zs' (y:ys)

instance OEIS 254719 where
  oeis = g (0 : drop 2 (oeis @45)) (oeis @105317) where
     g fs'@ (f:fs) (x:xs) = if x == f then g fs xs else x : g fs' xs

instance OEIS 254732 where
  oeisIx (succ->n) = head [k | k <- [n + 1 ..], mod (k ^ 2) n == 0]

instance OEIS 254744 where
  oeis = 1 : f 2 [1] where
     f x ys = y : f (x * 2) (y : ys) where
              y = x * (sum $ zipWith (*) ys $ reverse ys)

instance OEIS 254784 where
  oeis = (iterate (scanl1 (+)) (oeis @40)) !! 5

instance OEIS 254788 where
  oeis = 1 : f [2..] 1 [] where
     f xs y zs = g xs where
       g (w:ws) | s `elem` zs || d `elem` zs = g ws
                | otherwise = w : f (delete w xs) w (d : s : zs)
                where s = y + w; d = abs (y - w)

instance OEIS 254790 where
  oeisIx = fi . (+ 1) . fromJust . (`elemIndex` (oeis @254788)) . succ

instance OEIS 254792 where
  oeis = map abs $ zipWith (-) (oeis @254788) $ tail (oeis @254788)

instance OEIS 254793 where
  oeis = zipWith (+) (oeis @254788) $ tail (oeis @254788)

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

instance OEIS 255582 where
  oeis = 1 : 2 : 3 : f 2 3 [4..] where
     f u v ws = y : f v y (delete y ws) where
                y = head [z | z <- ws, let d = gcd u z, d > 1, gcd v z <= d]

instance OEIS 256100 where
  oeis = tail $ f (oeis @7376) $ take 10 $ repeat 1 where
     f (d:ds) counts = y : f ds (xs ++ (y + 1) : ys) where
                             (xs, y:ys) = splitAt d counts

instance OEIS 256512 where
  oeisIx n = n * (1 + (2 * n) ^ n)

instance OEIS 256523 where
  oeis = [x | x <- [0..], let i = (oeisIx @30) x,
                      (oeisIx @30) (x ^ 2) == i, (oeisIx @30) (x ^ 3) == i]

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

instance OEIS 258073 where
  oeisIx = (+ 1) . (* 78557) . (2 ^) . succ

instance OEIS 258703 where
  oeisIx = floor . (/ 2) . subtract 1 . (* sqrt 2) . fi

instance OEIS 258721 where
  oeisIx n = 4 * n * (6 * n + 13) + 29

instance OEIS 259022 where
  oeis = cycle [1, -1, -1, 1, 0, -1, 1, 1, -1]

instance OEIS 259525 where
  oeis = zipWith (-) (tail pascal) pascal
    where pascal = concat (tabl @7318)

instance OEIS 260112 where
  oeisIx n = b n 0
    where
      c i = if i `mod` 4 == 0 then i `div` 4 else i - 1
      b 0 foldCount = foldCount
      b sheetCount foldCount = b (c sheetCount) (foldCount + 1)

instance OEIS 260194 where
  oeis = 1 : 1 : 1 : f 1 1 1 where
     f u v v' = w : f w u v where w = u + gcd u (u + v')

instance OEIS 260416 where
  oeis = f 1 (oeis @40) where
     f x (p:ps) = g ps where
         g (q:qs) = if (q - x) `mod` p == 0 then q : f (x + 1) ps else g qs

instance OEIS 261470 where
  oeis = zipWith (-) (drop 2 (oeis @1223)) (oeis @1223)

instance OEIS 262564 where
  oeis = [2, 3, 5, 4] ++ [6..]

instance OEIS 262565 where
  oeis = cycle [2,3,5,5,3,2]

instance OEIS 264749 where
  oeisIx n = div n $ (oeisIx @70939) n

instance Table 265705 where
  tabl = map (rowT @265705) [0..]
  rowT n = map (rowCol @265705 n) [0..n]
  rowCol n k = k `bimpl` n
instance OEIS 265705 where
  oeis = tablList @265705

instance OEIS 265716 where
  oeisIx n = n `bimpl` (2 * n)

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

instance OEIS 269347 where
  oeis = 1 : map a [2..] where
    a n = sum $ filter ((==) 0 . mod n . (oeisIx @269347 . pred)) [1..n - 1]

instance OEIS 271268 where
  oeis = 8 : cycle [88, 1664, 17144, 17112, 1214]

instance OEIS 273156 where
  oeisIx = product . (rowT @35516)

instance OEIS 275673 where
  oeis = scanl (+) 1 $ concatMap (replicate 6) [1..]

instance OEIS 276163 where
  oeisIx n = maximum $ map minimax $ permutations [1..n]

instance OEIS 276168 where
  oeisIx n = minimum $ map minimax $ permutations [1..n]

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

instance OEIS 305461 where
  oeisIx (succ->n) = genericLength $ filter (\i -> (i^3 - i^2) `mod` n == 0) [0..n - 1]

instance OEIS 306216 where
  oeis = 1 : 1 : concat (unfoldr nextGeneration [1,1]) where
    nextGeneration l = Just (diff l, l ++ diff l)
    diff xs =  zipWith subtract xs (tail xs)

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
