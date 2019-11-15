module OEIS.Prime () where

import OEIS.OEIS
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
import Data.Maybe (fromJust)


choices = concat . map permutations . subsequences

zipTail f = zipWith f =<< tail

fact n = product [2..n]
facts  = scanl1 (*) [1..]

divisors 1 = [1]
divisors n = (1:filter ((==0) . rem n) [2..n `div` 2]) ++ [n]

totient n = genericLength $ filter (==1) $ map (gcd n) [1..n]

sumTo n = (n + r) * q
  where
    -- (q,r) = quotRem (n + 1) 2
    (q,r) = (shiftR (n + 1) 1, (n + 1) .&. 1)

bin n 0 = 1
bin 0 k = 0
bin n k = bin (n - 1) (k - 1) * n `div` k

fibs = 0 : 1 : zipTail (+) fibs

fib :: Int -> Integer
fib n = snd . foldl_ fib_ (1, 0) . dropWhile not $
            [testBit n k | k <- let s = finiteBitSize n in [s - 1,s - 2..0]]
    where
        fib_ (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g
        foldl_ = foldl' -- '


instance OEIS 40 where
  oeis = primes

instance OEIS 45 where
  oeis   = fibs
  oeisIx = fi . fib . fi


instance OEIS 2 where
  oeis = let a = 1:2: drop 2 (concat . zipWith (replicate . fi) a . cycle $ [1, 2]) in a

instance OEIS 4 where
  oeis   = repeat 0
  oeisIx = const 0

instance OEIS 5 where
  oeisIx = genericLength . divisors . succ
  -- product . map (+1) . a124010_row

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
    p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 10 where
  oeisIx (succ->n) = totient (n)
  -- oeisIx n = genericLength (filter (==1) (map (gcd n) [1..n]))

instance OEIS 12 where
  oeis   = repeat 1
  oeisIx = const 1

instance OEIS 13 where
  oeisIx 0 = 1
  oeisIx n = sum (zipWith (*)
   (map (oeisIx @10 . pred . (* 2)) ds) (map (2 ^) $ reverse ds)) `div` (2 * n)
    where ds = a027750_row n

instance OEIS 15 where
  oeis = 1 : concat do zipTail f $ oeis @961
    where
      f pp qq = replicate (fi (pp - qq)) pp

-- instance OEIS 16 where
--   oeisIx 0 = 1
--   oeisIx n =  (`div` (2 * n)) $ sum $
--    zipWith (*) (map (oeisIx @10) oddDivs) (map ((2 ^) . (div n)) $ oddDivs)
--     where oddDivs = a182469_row n

instance OEIS 21 where
  oeisIx n = genericLength [() | k <- [1..2^n],
        sum [oeisIx @10052 (k - 12*y^2) | y <- [0..oeisIx @196 (k `div` 12)]] > 0]

instance OEIS 23 where
  oeisIx n = foldl g 1 [1..n]
    where g n m = n*m + (-2)^m

instance OEIS 26 where
  oeisIx (succ->n) = f primes n 1 (0^(n - 1)) 1 where
    f _  1 q e y  = y * e * q
    f ps'@(p:ps) x q e y
      | m == 0    = f ps' x' p (e+1) y
      | e > 0     = f ps x q 0 (y * e * q)
      | x < p * p = f ps' 1 x 1 y
      | otherwise = f ps x 1 0 y
      where (x', m) = divMod x p

instance OEIS 27 where
  oeis   = [1..]
  oeisIx = id

instance OEIS 28 where
  oeis = filter (odd . sum . map (oeisIx @120) . a124010_row) [1..]

instance OEIS 30 where
  oeisIx = until (< 10) (`div` 10)

instance OEIS 27750 where
  oeis = a027750_row =<< [1..]

a027750_row n = filter ((== 0) . mod n) [1..n]


instance OEIS 10052 where
  oeis     = concat (iterate (\xs -> xs ++ [0,0]) [1])
  oeisIx n = fi . fromEnum $ oeisIx @196 n ^ 2 == n

instance OEIS 123010 where
  oeis = 1 : 0 : 4 : 16 : f 0 4 16
    where f a b c = let x = 5*c + b - 5*a in x : f b c x

instance OEIS 124010 where
  oeis = a124010_row =<< [1..]

a124010_row 1 = [0]
a124010_row n = f n primes where
   f 1 _      = []
   f u (p:ps) = h u 0 where
     h v e
      | (v',m) <- divMod v p
      , m == 0 = h v' (e + 1)
      | e  > 0 = e : f v ps
      | let    = f v ps

instance OEIS 20639 where
  oeisIx (succ->n) = spf primes where
    spf (p:ps) | n < p^2      = n
               | mod n p == 0 = p
               | otherwise    = spf ps

instance OEIS 961 where
  oeis = 1 : g (S.singleton 2) (tail primes)
    where
      g s (p:ps) = m : g (S.insert (m * (oeisIx @20639 . pred) m) $ S.insert p s') ps
        where
          (m, s') = S.deleteFindMin s

instance OEIS 182469 where
  oeis = a182469_row =<< [0..]

a182469_row = a027750_row . oeisIx @265

instance OEIS 120 where
  oeis = concat r
    where r = [0] : (map.map) (+1) (scanl1 (++) r)

  oeisIx = fi . popCount . fi

instance OEIS 31 where
  oeisIx = f
    where
      f 0 = 1
      f n = (`div` n) $ sum $
        zipWith (*) (map (oeisIx @10 . pred) divs) (map (oeisIx @79) $ reverse divs)
        where divs = a027750_row n


instance OEIS 32 where
  oeis = let r = 2 : 1 : do zipTail (+) r in r

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
  oeisIx n = n


instance OEIS 42 where
  oeis = iterate (\x -> 10*x + 1) 1
  oeisIx n = (10 ^ n - 1) `div` 9

-- TODO: Lucas Lehmer
instance OEIS 43 where
  oeis = [ i | (i, n) <- iterate (\(i,n) -> (i+1, shiftL n 1)) (1,2), isPrime (n - 1) ]

instance OEIS 44 where
  oeis = rabs where
    rabs = 1 : take 12 (tail fibs)
        ++ zipWith3 (\x y z -> x + y - z)
             (drop 12 rabs)
             (drop 11 rabs)
                      rabs

instance OEIS 50 where
  oeisIx n = foldl f 0 [1..2^n]
    where
      f i j | a000050' j > 0 = i + 1 | let = i
      a000050' k = foldl f 0 (h k)
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

instance OEIS 71 where
  oeis = map (subtract 1) $ tail $ oeis @45

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
     h = sum . map (\d -> d * (oeisIx @81) d) . a027750_row

-- a000082 n = product $ zipWith (\p e -> p ^ (2*e - 1) * (p + 1))
--                               (a027748_row n) (a124010_row n)

instance OEIS 85 where
  oeis = xs
    where
      xs = 1 : 1 : zipWith (+) (zipWith (*) [1..] xs) (tail xs)

-- a000086 n = if n `mod` 9 == 0 then 0
--   else product $ map ((* 2) . a079978 . (+ 2)) $ a027748_row $ a038502 n

-- a000089 n = product $ zipWith f (a027748_row n) (a124010_row n) where
--    f 2 e = if e == 1 then 1 else 0
--    f p _ = if p `mod` 4 == 1 then 2 else 0

instance OEIS 93 where
  oeisIx = oeisIx @196 . oeisIx @578

-- a000095 n = product $ zipWith f (a027748_row n) (a124010_row n) where
--    f 2 e = if e == 1 then 2 else 0
--    f p _ = if p `mod` 4 == 1 then 2 else 0

instance OEIS 96 where
  oeisIx n = n * (n + 3) `div` 2
-- a000096_list = [x | x <- [0..], a023531 x == 1]

instance OEIS 100 where
  oeis = f (tail $ oeis @45) [head $ oeis @45] where
   f (x:xs) ys = (sum . zipWith (*) ys $ oeis @73) : f xs (x:ys)

instance OEIS 73 where
  oeis = xs where xs = 0 : 0 : 1 : zipWith (+) xs do tail $ zipTail (+) xs


instance OEIS 106 where
  oeis = drop 2 $ conv (oeis @81) [] where
    conv (v:vs) ws = (sum $ zipWith (*) ws' $ reverse ws') : conv vs ws'
      where ws' = v : ws

instance OEIS 108 where
  oeis = map last $ iterate (scanl1 (+) . (++ [0])) [1]

-- a000110 = sum . a048993_row 

-- a000111 0 = 1
-- a000111 n = sum $ a008280_row (n - 1)

instance OEIS 116 where
  oeis = bis $ oeis @13 where bis (x:_:xs) = x : bis xs

-- a000118 0 = 1
-- a000118 n = 8 * a046897 n 

instance OEIS 119 where
  oeisIx = p $ drop 2 $ oeis @45 where
    p _      0 = 1
    p (f:fs) m = if m < f then 0 else p fs (m - f) + p fs m

instance OEIS 123 where
  oeis = xs
    where xs = 1 : zipWith (+) xs (tail $ concat $ transpose [xs,xs])

instance OEIS 124 where
  oeisIx = succ . oeisIx @217

-- a000127 = sum . take 5 . a007318_row

instance OEIS 129 where
  oeis = xs where xs = 0 : 1 : zipWith (+) xs do map (2 *) $ tail xs

-- a000139 0 = 2
-- a000139 n = ((3 * n) `a007318` (2 * n + 1)) `div` a000217 n

-- a000141 0 = 1
-- a000141 n = 16 * a050470 n - 4 * a002173 n

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

-- a000161 n = sum $ map (a010052 . (n -)) $ takeWhile (<= n `div` 2) a000290_list

instance OEIS 165 where
  oeisIx n = product [2, 4 .. 2 * n]

instance OEIS 166 where
  oeis = xs where xs = 1 : 0 : zipWith (*) [1..] do zipTail (+) xs

instance OEIS 169 where
  oeisIx 0 = 1
  oeisIx (succ->n) = n ^ (n - 1)

-- a000172 = sum . map a000578 . a007318_row

instance OEIS 178 where
  oeis = 1 : scanl1 (*) facts

-- a000179 n = a000179_list !! n
-- a000179_list = 1 : 0 : 0 : 1 : zipWith5
--    (\v w x y z -> (x * y + (v + 2) * z - w) `div` v) [2..] (cycle [4,-4])
--    (drop 4 a067998_list) (drop 3 a000179_list) (drop 2 a000179_list)

-- a000188 n = product $ zipWith (^)
--                       (a027748_row n) $ map (`div` 2) (a124010_row n)

instance OEIS 193 where
  oeisIx = round . log . succ . fi
  oeis   = concat [ replicate n i
                  | n <- 1 : do zipTail (-) $ oeis @219092
                  | i <- [0..] ]

-- TODO: finite precision
instance OEIS 1113 where
  oeis = step $ sum $ take 100 $ map (1%) $ 1 : facts
    where
      step r | x <- div (numerator r) (denominator r) = x : step ((r - x%1)*10)

-- instance OEIS 1113 where
--   oeis = eStream (1, 0, 1)
--      [(n, a * d, d) | (n, d, a) <- map (\k -> (1, k, 1)) [1..]] where
--      eStream z xs'@(x:xs)
--        | lb /= approx z 2 = eStream (mult z x) xs
--        | otherwise = lb : eStream (mult (10, -10 * lb, 1) z) xs'
--        where lb = approx z 1
--              approx (a, b, c) n = div (a * n + b) c
--              mult (a, b, c) (d, e, f) = (a * d, a * e + b * f, c * f)

instance OEIS 219092 where
  oeisIx n = floor (exp 1 ** (fi n + 0.5))

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

-- a000203 n = product $ zipWith (\p e -> (p^(e+1)-1) `div` (p-1)) (a027748_row n) (a124010_row n)

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

-- a000216_list = iterate a003132 2 

instance OEIS 217 where
  oeis = scanl1 (+) [0..]

-- a000218_list = iterate a003132 3

-- a000221_list = iterate a003132 5

-- a000224 n = product $ zipWith f (a027748_row n) (a124010_row n) where
--    f 2 e = 2 ^ e `div` 6 + 2
--    f p e = p ^ (e + 1) `div` (2 * p + 2) + 1

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

-- a000246 n = a000246_list !! n
-- a000246_list = 1 : 1 : zipWith (+)
--    (tail a000246_list) (zipWith (*) a000246_list a002378_list)

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

-- a000262_list = 1 : 1 : zipWith (-)
--                (tail $ zipWith (*) a005408_list a000262_list)
--                       (zipWith (*) a002378_list a000262_list)

instance OEIS 265 where
  oeisIx = until odd (`div` 2) . succ

-- a000267 = a000196 . a016813

instance OEIS 272 where
  oeisIx 0 = 1
  oeisIx 1 = 1
  oeisIx n = n ^ (n - 2)


instance OEIS 277 where
  oeisIx n = 3*n - 2*oeisIx @196 (4 * n + 5) + 5

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
  oeisIx n = genericLength [(x,y) | x <- [-n..n], y <- [-n..n], x^2 + y^2 <= n^2]

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

-- instance OEIS 360 where
--   oeis = fix \r -> 1 : concat (transpose
--     [ zipWith (+) r $ drop 2 $ oeis @57078
--     , zipTail (+) r ])

instance OEIS 379 where
  oeis = filter (even . sum . map (oeisIx @120) . a124010_row) [1..]

instance OEIS 384 where
  oeisIx n = n * (2 * n - 1)
  -- oeis = scanl (+) 0 (oeis @16813)

-- instance OEIS 385 where
--   oeisIx n = sum $ zipWith (*) sigmas $ reverse sigmas
--     where
--       sigmas = take (fi n) $ oeis @203

instance OEIS 389 where
  oeis = 0 : 0 : f [] (oeis @217)
    where
      f xs (t:ts) = (sum $ zipWith (*) xs $ oeis @217) : f (t:xs) ts

-- instance OEIS 396 where
--   oeis = [ x | x <- [1..], oeisIx @203 x == 2 * x ]

instance OEIS 400 where
  oeisIx = (6 ^)
  oeis   = iterate (* 6) 1

-- instance OEIS 404 where
--   oeis = map fi . findIndices (> 0) $ oeis @25426

-- instance OEIS 408 where
--   oeis = filter ((> 0) . oeisIx @25427) [1..]

-- instance OEIS 419 where
--   oeis = filter ((== 3) . oeisIx @2828) [1..]

instance OEIS 420 where
  oeisIx = (7 ^)
  oeis   = iterate (* 7) 1

-- instance OEIS 430 where
--   oeis = m (oeis @40) (oeis @1248)
--     where
--       m (x:xs) (y:ys)
--         | x < y = x : m xs (y:ys)
--         | x > y = y : m (x:xs) ys

instance OEIS 433 where
  oeisIx 0 = 0
  oeisIx n = fi . read $ map intToDigit $ t n $ reverse $ takeWhile (<= n) $ tail $ oeis @578
    where
      t _ []          = []
      t m (x:xs)
          | x > m     = 0 : t m xs
          | otherwise = (fi m') : t r xs
          where (m',r) = divMod m x

-- instance OEIS 447 where
--   oeis = scanl1 (+) $ oeis @16754

instance OEIS 461 where
  oeisIx = fi . a000461 . fi . succ
    where
      a000461 n = (read $ concat $ replicate n $ show n) :: Integer

instance OEIS 462 where
  oeisIx (succ->fi->n) = fi . g [] n $ reverse $ takeWhile (<= n) $ tail (oeis @217)
    where
      g as 0 []     = read $ concat $ map show $ reverse as :: Integer
      g as x (t:ts) = g (a:as) r ts where (a,r) = divMod x t

instance OEIS 463 where
  oeis = concatMap (\x -> [x,x^2]) [1..]

-- instance OEIS 469 where
--   oeis = filter ((== 0) . oeisIx @10051) (oeis @5117)

instance OEIS 472 where
  oeis = 2 : 5 : zipWith (+) (map (^ 2) $ tail $ oeis @472)
    do zipWith (*) (map (+ 1) $ oeis @472)
                $ zipWith (-) (tail $ oeis @472)
                $ map (^ 2) $ oeis @472

instance OEIS 522 where
  oeisIx = genericLength . choices . enumFromTo 1 . fi

instance OEIS 523 where
  oeisIx 1 = 0
  oeisIx n = 1 + oeisIx @523 (div n 2)
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

-- instance OEIS 587 where
--   oeis =  1 : f (oeis @7318) [1] where
--    f (bs:bss) xs = y : f bss (y : xs)
--      where y = - sum (zipWith (*) xs bs)

instance OEIS 593 where
  oeisIx = sum . a182469_row

instance OEIS 603 where
  oeisIx n = genericLength [(x,y) | x <- [0..n], y <- [0..n], x^2 + y^ 2 <= n^2]

instance OEIS 607 where
  oeisIx = p $ oeis @40 where
    p _      0 = 1
    p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 657 where
--   oeisIx n = oeisIx @8280 (2 * n) n


-- instance OEIS 658 where
--   oeisIx n = sum $ map c3 [0..n] where
--      c3 k = (a007318' n k)^2 * (a007318' (2*k) k)^2 * a007318' (2*k) (n-k)

-- instance OEIS 660 where
--   oeisIx n = sum $ zipWith (*) (a109449_row n) (1 : [1..])

-- instance OEIS 667 where
--   oeisIx n = if x == 1 then last xs else x
--               where xs@(x:_) = a227862_row n

-- instance OEIS 670 where
--   oeis = 1 : f [1] (map tail $ tail a007318_tabl) where
--     f xs (bs:bss) = y : f (y : xs) bss where y = sum $ zipWith (*) xs bs

-- -- 674
-- a000674 n = sum $ zipWith (*) (a109449_row n) (1 : repeat 2)

-- -- 688
-- a000688 = product . map a000041 . a124010_row

instance OEIS 689 where
  oeis = 1 : cycle [2,4,8,6]

instance OEIS 695 where
  oeisIx n = if n == 0 then 0 else 4 * oeisIx @695 n' + b
    where (n',b) = divMod n 2

-- -- 697
-- a000697 n = sum $ zipWith (*) (a109449_row n) (1 : tail a000290_list)

instance OEIS 703 where
  oeisIx = fi . floor . (/ 2) . (+ 7) . sqrt . (+ 1) . (* 24) . fi

-- instance OEIS 712 where
--   oeis = p $ oeis @8619 where
--     p _          0 = 1
--     p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 718 where
--   oeisIx n = sum $ zipWith (*) (a109449_row n) (1 : tail a000217_list)

-- instance OEIS 720 where
--   oeis = scanl1 (+) $ oeis @10051

-- -- 726
-- a000726 n = p a001651_list n where
--    p _  0 = 1
--    p ks'@(k:ks) m | m < k     = 0
--                   | otherwise = p ks' (m - k) + p ks m

-- -- 732
-- a000732 n = sum $ zipWith (*) (a109449_row n) a008578_list

-- -- 733
-- a000733 n = sum $ zipWith (*) (a109449_row n) (1 : a000041_list)

-- -- 734
-- a000734 n = sum $ zipWith (*) (a109449_row n) (1 : a000079_list)

-- -- 736
-- a000736 n = sum $ zipWith (*) (a109449_row n) (1 : a000108_list)

-- -- 737
-- a000737 n = sum $ zipWith (*) (a109449_row n) [1..]

-- -- 738
-- a000738 n = sum $ zipWith (*) (a109449_row n) a000045_list

-- -- 744
-- a000744 n = sum $ zipWith (*) (a109449_row n) $ tail a000045_list

-- -- 745
-- a000745 n = sum $ zipWith (*) (a109449_row n) $ tail a000290_list

-- -- 746
-- a000746 n = sum $ zipWith (*) (a109449_row n) $ tail a000217_list

-- instance OEIS 747 where
--   oeisIx n = sum $ zipWith (*) (a109449_row n) $ oeis @40

instance OEIS 749 where
  oeis = fix \r -> 0 : 0 : 0 : 1 : zipWith3 (\u v w -> 4 * u - 6 * v + 4 * w)
    (drop 3 r) (drop 2 r) (drop 1 r)


-- instance OEIS 751 where
--   a000751 n = sum $ zipWith (*) (a109449_row n) a000041_list

-- instance OEIS 752 where
--   a000752 n = sum $ zipWith (*) (a109449_row n) a000079_list

-- instance OEIS 753 where
--   a000753 n = sum $ zipWith (*) (a109449_row n) a000108_list

-- instance OEIS 754 where
--   a000754 n = sum $ zipWith (*) (a109449_row n) [1, 3 ..]

-- instance OEIS 756 where
--   a000756 n = sum $ zipWith (*) (a109449_row n) (1 : 1 : [0, 0 ..])

instance OEIS 788 where
  oeis = scanl1 (+) $ oeis @120
  oeisIx 0 = 0
  oeisIx n = (oeisIx @788) n2 + (oeisIx @788) (n - n2 - 1) + (n - n2)
    where n2 = n `div` 2

-- instance OEIS 790 where
--   oeisIx n = head [c | c <- oeis @2808, powMod n c c == mod n c]

instance OEIS 792 where
  oeis = 1 : f [1] where
     f xs = y : f (y:xs) where y = maximum $ zipWith (*) [1..] xs

instance OEIS 793 where
  oeisIx = maximum . map (foldl lcm 1) . partitions where
     partitions n = ps 1 n where
        ps x 0 = [[]]
        ps x y = [t:ts | t <- [x..y], ts <- ps t (y - t)]

-- instance OEIS 796 where
--   a000796 n = a000796_list (n + 1) !! (n + 1)
--   a000796_list len = map digitToInt $ show $ machin' `div` (10 ^ 10) where
--      machin' = 4 * (4 * arccot 5 unity - arccot 239 unity)
--      unity = 10 ^ (len + 10)
--      arccot x unity = arccot' x unity 0 (unity `div` x) 1 1 where
--        arccot' x unity summa xpow n sign
--         | term == 0 = summa
--         | otherwise = arccot'
--           x unity (summa + sign * term) (xpow `div` x ^ 2) (n + 2) (- sign)
--         where term = xpow `div` n

instance OEIS 803 where
  oeis = fix \r -> 0 : 0 : 8 : zipWith (+)
                 (tail $ zipWith (+) (tail r) r)
                 (map (subtract 4) r)


-- instance OEIS 891 where
--   a000891 n = a001263 (2 * n - 1) n  

-- instance OEIS 894 where
--   a000894 n = a132813 (2 * n) n  

instance OEIS 898 where
  oeis = fix \r -> 1 : 2 : do map (* 2) $ zipWith (+) (tail r) (zipWith (*) [1..] r)

instance OEIS 902 where
  oeis = fix \r -> 1 : 1 : 3 : map (* 2) (zipWith (+)
     (drop 2 r) (zipWith (*) [2..] $ tail r))

instance OEIS 904 where
  oeis = fix \r -> 0 : 3 : 13 : zipWith (+) r
     (zipWith (+) (zipWith (*) [6..] $ drop 1 r)
                  (zipWith (*) [5..] $ drop 2 r))

-- instance OEIS 914 where
--   oeis = scanl1 (+) a006002_list

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

-- instance OEIS 967 where
--   a000967 n = round $ sum $
--               zipWith ((/) `on` fromIntegral) (a258993_row n) [1, 3 ..]

-- instance OEIS 969 where
--   a000969 = flip div 3 . a014105 . (+ 1)

-- instance OEIS 970 where
--   a000970 n = a258708 n (n - 5)  

-- instance OEIS 971 where
--   a000971 n = a258708 n (n - 6)  

-- instance OEIS 972 where
--   a000972 n = a258708 n (n - 7)  

-- instance OEIS 973 where
--   a000973 n = a258708 n (n - 8)  

instance OEIS 975 where
  oeis = fix \r -> 0 : 1 : map (+ 1) (zipWith (+) (tail r) (map (* 2) r))

-- instance OEIS 977 where
--   oeis = filter ((> 2) . oeisIx @1221) [1..]

-- instance OEIS 978 where
--   a000978 n = a000978_list !! (n-1)
--   a000978_list = filter ((== 1) . a010051 . a001045) a065091_list

-- instance OEIS 979 where
--   a000979 n = a000979_list !! (n-1)
--   a000979_list = filter ((== 1) . a010051) a007583_list

instance OEIS 980 where
  oeisIx n = genericLength $ filter ((== 0) . sum) $ subsequences [-n..n]

instance OEIS 982 where
  oeisIx = (`div` 2) . (+ 1) . (^ 2)

-- instance OEIS 984 where
--   oeisIx n = a007318_row (2*n) !! n

-- instance OEIS 989 where
--   a000989 = a007949 . a000984

-- instance OEIS 990 where
--   a000990 = p $ tail a008619_list where
--      p _          0 = 1
--      p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 992 where
  oeis = 1 : f 1 0 [1] where
     f x y zs = z : f (x + y) (1 - y) (z:zs) where
       z = sum $ take x $ zipWith (*) zs $ reverse zs

-- instance OEIS 994 where
--   a000994 n = a000994_list !! n
--   a000994_list = 1 : 0 : us where
--     us = 1 : 1 : f 2 where
--       f x = (1 + sum (zipWith (*) (map (a007318' x) [2..x]) us)) : f (x + 1)

-- instance OEIS 995 where
--   a000995 n = a000995_list !! n
--   a000995_list = 0 : 1 : vs where
--     vs = 0 : 1 : g 2 where
--       g x = (x + sum (zipWith (*) (map (a007318' x) [2..x]) vs)) : g (x + 1)

-- instance OEIS 1003 where
--   a001003 = last . a144944_row

-- instance OEIS 1006 where
--   a001006 n = a001006_list !! n
--   a001006_list = zipWith (+) a005043_list $ tail a005043_list

instance OEIS 1008 where
  oeisIx = numerator . sum . map (1 %) . enumFromTo 1
  oeis   = map numerator $ scanl1 (+) $ map (1 %) [1..]

-- instance OEIS 1013 where
--   import Data.Set (empty, fromList, deleteFindMin, union)
--   import qualified Data.Set as Set (null)
--   a001013 n = a001013_list !! (n-1)
--   a001013_list = 1 : h 0 empty [1] (drop 2 a000142_list) where
--      h z s mcs xs'@(x:xs)
--       | Set.null s || x < m = h z (union s (fromList $ map (* x) mcs)) mcs xs
--       | m == z = h m s' mcs xs'
--       | otherwise = m : h m (union s' (fromList (map (* m) $ init (m:mcs)))) (m:mcs) xs'
--       where (m, s') = deleteFindMin s

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

-- instance OEIS 1031 where
--   oeisIx n = sum (map a010051 gs) + fromEnum (1 `elem` gs)
--      where gs = map (2 * n -) $ takeWhile (<= n) a008578_list

-- instance OEIS 1037 where
--   a001037 0 = 1
--   a001037 n = (sum $ map (\d -> (a000079 d) * a008683 (n `div` d)) $
--                          a027750_row n) `div` n

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
  oeis = map fst $ iterate (\(u, v) -> (3 * u + v, 2 * v)) (0, 1)

instance OEIS 1053 where
  oeis = fix \r -> 1 : 0 : zipWith (+) r (zipWith (*) [1..] $ tail r)

-- instance OEIS 1055 where
--   oeisIx = (map last a066032_tabl !!) . (subtract 1)

instance OEIS 1056 where
  oeis = fix \r -> 1 : 3 : (map (+ 1 ) $ zipWith (*) r $ tail r)

instance OEIS 1057 where
  oeisIx n = (n' + m) * (-1) ^ (1 - m) where (n',m) = divMod n 2
  oeis     = 0 : concatMap (\x -> [x,-x]) [1..]

instance OEIS 1064 where
  oeis = fix \r -> 1 : 1 : 0 : zipWith (+) r (tail $ zipWith (*) r (tail r))

-- instance OEIS 1065 where
--   oeisIx n = oeisIx @203 n - n

-- instance OEIS 1066 where
--   import Data.Set (deleteFindMin, fromList, insert)
--   a001066 n = a001066_list !! (n-1)
--   a001066_list = f (fromList [h, 2 * h]) $ tail a003038_list where
--      h = head a003038_list
--      f s (x:xs) = m : f (x `insert` (( 2 * x) `insert` s')) xs where
--        (m, s') = deleteFindMin s

instance OEIS 1075 where
  oeis = fix \r -> 1 : 2 : zipWith (-) (map (4 *) $ tail r) r

instance OEIS 1078 where
  oeis = fix \r -> 0 : 2 : zipWith (-) (map (10*) $ tail r) r

-- instance OEIS 1082 where
--   oeis = scanl (+) 0 $ tail a022998_list

instance OEIS 1088 where
  oeis = scanl1 (*) $ oeis @10

instance OEIS 1093 where
  oeisIx = (+ 1) . (^ 3) . pred

-- instance OEIS 1097 where
--   oeis = filter ((== 1) . a164292) [1..]

-- instance OEIS 1101 where
--   a001101 n = a001101_list !! (n-1)
--   a001101_list = map succ $ findIndices p [1..] where
--      p n = m == 0 && a010051 n' == 1 where
--         (n', m) = divMod n (a007953 n)

-- instance OEIS 1102 where
--   a001102 n = a001102_list !! (n-1)
--   a001102_list =
--      filter (\x -> a010052 (x `div` (a007953 x)) == 1) a005349_list

-- instance OEIS 1103 where
--   a001103 n = a001103_list !! (n-1)
--   a001103_list = filter f a052382_list where
--      f x = m == 0 && (x' == 1 || a010051 x' == 1) where
--          (x',m) = divMod x $ a007954 x

-- instance OEIS 1105 where
--   a001105 = a005843 . a000290
--   [2*n^2 for n in (0..20)] # _G. C. Greubel_, Feb 22 2019

instance OEIS 1106 where
  -- a001106 n = length [(x,y) | x <- [-n+1..n-1], y <- [-n+1..n-1], x + y <= n]
  oeisIx n = n*(7*n - 5) `div` 2

instance OEIS 1108 where
  oeis = fix \r -> 0 : 1 : map (+ 2) (zipWith (-) (map (* 6) (tail r)) r)

instance OEIS 1109 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1110 where
  oeis = fix \r -> 0 : 1 : (map (+ 2) $ zipWith (-) (map (* 34) (tail r)) r)

-- instance OEIS 1127 where
--   oeis = iterate a056964 1

-- instance OEIS 1129 where
--   a001129 n = a001129_list !! n
--   a001129_list = 0 : 1 : zipWith (+) iccanobifs (tail iccanobifs)
--   where iccanobifs = map a004086 a001129_list

-- instance OEIS 1132 where
--   a001132 n = a001132_list !! (n-1)
--   a001132_list = [x | x <- a047522_list, a010051 x == 1]

instance OEIS 1140 where
  oeis = fix \r -> 4 : map (fi . say . fi) r where
     say :: Integer -> Integer
     say = read . concatMap saygroup . group . show
           where saygroup s = (show $ length s) ++ [head s]

-- instance OEIS 1142 where
--   oeis = product . a007318_row

instance OEIS 1146 where
  oeisIx = (2 ^) . (2 ^)
  oeis = iterate (^ 2) 2

instance OEIS 1147 where
  oeisIx n = product [1, 3 .. 2 * n - 1]
  oeis     = fix \r -> 1 : zipWith (*) [1, 3 ..] r


instance OEIS 1156 where
  oeisIx = p (tail $ oeis @290) where
     p _          0 = 1
     p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 1157 where
  oeisIx n = s n 1 1 $ oeis @40 where
     s 1 1 y _          = y
     s m x y ps'@(p:ps)
       | m `mod` p == 0 = s (m `div` p) (x * p^2) y ps'
       | x > 1          = s m 1 (y * (x * p^2 - 1) `div` (p^2 - 1)) ps
       | otherwise      = s m 1 y ps

-- instance OEIS 1158 where
--   a001158 n = product $ zipWith (\p e -> (p^(3*e + 3) - 1) `div` (p^3 - 1))
--                         (a027748_row n) (a124010_row n)

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

-- instance OEIS 1178 where
--   oeisIx = f 0 where
--      f j x = if x == y then j else f (j + 1) y  where y = oeisIx @1175 x

instance OEIS 1179 where
  oeisIx = f . succ
    where
      f 1 = 0
      f n = if p == n then ll (p `div` 24) 1 else f p
              where p = oeisIx @1175 (pred n)
                    ll x k = if x == 1 then k else ll (x `div` 5) (k + 1)

-- instance OEIS 1181 where
--   a001181 0 = 0
--   a001181 n =
--      (sum $ map (\k -> product $ map (a007318 (n+1)) [k-1..k+1]) [1..n])
--       `div` (a006002 n)

instance OEIS 1196 where
  oeisIx n = if n == 0 then 0 else 4 * oeisIx @1196 n' + 3 * b
              where (n',b) = divMod n 2

-- instance OEIS 1220 where
--   oeis = map (oeis @40 . (+ 1)) $ elemIndices 1 a196202_list

instance OEIS 1221 where
  oeisIx = genericLength . snd . unzip . factorise . fi . succ

instance OEIS 1222 where
  oeisIx = fi . sum . snd . unzip . factorise . fi . succ

instance OEIS 1223 where
  oeis = zipWith (-) (tail $ oeis @40) $ oeis @40

-- instance OEIS 1227 where
--   a001227 = sum . a247795_row

instance OEIS 1248 where
  oeis = map (^ 2) $ oeis @40

-- instance OEIS 1250 where
--   oeisIx n = if n == 1 then 1 else 2 * oeisIx @111 n


-- instance OEIS 1255 where
--   oeisIx = (^ 2) . oeisIx @41

-- instance OEIS 1263 where
--   a001263 n k = a001263_tabl !! (n-1) !! (k-1)
--   a001263_row n = a001263_tabl !! (n-1)
--   a001263_tabl = zipWith dt a007318_tabl (tail a007318_tabl) where
--      dt us vs = zipWith (-) (zipWith (*) us (tail vs))
--                             (zipWith (*) (tail us ++ [0]) (init vs))

instance OEIS 1274 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $ zipWith (-) (tail $ oeis @10) $ oeis @10

-- instance OEIS 1285 where
--   a001285 n = a001285_list !! n
--   a001285_list = map (+ 1) a010060_list

instance OEIS 1286 where
  oeisIx ((+2)->n) = sum [1..n - 1] * product [1..n - 1]

instance OEIS 1299 where
  oeisIx = p [1,5,10,25] where
     p _          0 = 1
     p []         _ = 0
     p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 1300 where
  oeisIx = p [1,5,10,25,50] where
     p _          0 = 1
     p []         _ = 0
     p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 1316 where
  -- oeisIx = sum . a047999_row
  oeis = 1 : zs where
     zs = 2 : (concat $ transpose [zs, map (* 2) zs])

-- instance OEIS 1317 where
--   a001317 = foldr (\u v-> 2*v + u) 0 . map toInteger . a047999_row

-- instance OEIS 1318 where
--   a001318 n = a001318_list !! n
--   a001318_list = scanl1 (+) a026741_list 

instance OEIS 1333 where
  oeis = fix \r -> 1 : 1 : zipWith (+) r (map (* 2) $ tail r)


instance OEIS 1353 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (4 *) $ tail r) r

instance OEIS 1358 where
  oeis = map (+1) $ filter ((== 2) . oeisIx @1222) [1..]

-- instance OEIS 1359 where
--   a001359 n = a001359_list !! (n-1)
--   a001359_list = filter ((== 1) . a010051' . (+ 2)) a000040_list

-- instance OEIS 1370 where
--   a001370 = a007953 . a000079

instance OEIS 1399 where
  oeisIx = p [1,2,3] where
     p _      0 = 1
     p []     _ = 0
     p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 1400 where
--   a001400 n = a001400_list !! n
--   a001400_list = scanl1 (+) a005044_list 

-- instance OEIS 1405 where
--   a001405 n = a007318_row n !! (n `div` 2) 

-- instance OEIS 1414 where
--   oeisIx 1 = 0
--   oeisIx n = sum $ a027746_row n

instance OEIS 1444 where
  oeisIx n = div (3 ^ n + 3 ^ (div n 2)) 2


-- instance OEIS 1461 where
--   a001461 n = a001461_list !! (n-1)
--   a001461_list = scanl1 (+) a006206_list 

instance OEIS 1462 where
  oeis = 1 : 2 : 2 : g 3  where
     g x = (genericReplicate (oeisIx @1462 $ pred x) x) ++ g (x + 1)

instance OEIS 1463 where
  oeis = scanl1 (+) $ oeis @1462

-- instance OEIS 1468 where
--   a001468 n = a001468_list !! n
--   a001468_list = map length $ group a005206_list

instance OEIS 1477 where
  oeisIx = id
  oeis   = [0..]

-- instance OEIS 1479 where
--   a001479 n = a000196 $ head $
--      filter ((== 1) . a010052) $ map (a007645 n -) $ tail a033428_list

-- instance OEIS 1480 where
--   a001480 n = a000196 $ (`div` 3) $ (a007645 n) - (a001479 n) ^ 2

-- instance OEIS 1481 where
--   oeis = [x | x <- [0..], oeisIx @161 x > 0]

instance OEIS 1494 where
  oeis = map (fi . (+ 1)) $ elemIndices 0 $ zipWith (-) (drop 2 (oeis @10)) (oeis @10)

instance OEIS 1497 where
  oeis = a001497_row =<< [0..]

a001497 n k = a001497_tabl !! n !! k
a001497_row n = a001497_tabl !! n
a001497_tabl = [1] : f [1] 1 where
    f xs z = ys : f ys (z + 2) where
               ys = zipWith (+) ([0] ++ xs) (zipWith (*) [z, z - 1 ..] (xs ++ [0]))

-- instance OEIS 1498 where
--   a001498 n k = a001498_tabl !! n !! k
--   a001498_row n = a001498_tabl !! n
--   a001498_tabl = map reverse a001497_tabl

-- instance OEIS 1499 where
--   a001499 n = a001499_list !! n
--   a001499_list = 1 : 0 : 1 : zipWith (*) (drop 2 a002411_list)
--      (zipWith (+) (zipWith (*) [3, 5 ..] $ tail a001499_list)
--                   (zipWith (*) (tail a000290_list) a001499_list))

instance OEIS 1511 where
  oeisIx (succ->n) = genericLength $ takeWhile ((== 0) . (mod n)) $ oeis @79
  -- oeisIx n | odd n = 1 | otherwise = 1 + (oeisIx @1511) (n `div` 2)

instance OEIS 1515 where
  oeisIx = sum . a001497_row . fi

-- instance OEIS 1519 where
--   a001519 n = a001519_list !! n
--   a001519_list = 1 : zipWith (-) (tail a001906_list) a001906_list
--   a001519_list = 1 : f a000045_list where f (_:x:xs) = x : f xs

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

-- instance OEIS 1583 where
--   a001583 n = a001583_list !! (n-1)
--   a001583_list = filter
--      (\p -> mod (a000045 $ div (p - 1) 5) p == 0) a030430_list

instance OEIS 1595 where
  oeis = fix \r -> 1 : 1 : do map (+ 1) $ zipTail (+) r

-- instance OEIS 1597 where
--   import Data.Map (singleton, findMin, deleteMin, insert)
--   a001597 n = a001597_list !! (n-1)
--   (a001597_list, a025478_list, a025479_list) =

instance OEIS 1599 where
  oeis = filter ((== 1) . denominator . hm) [1..] where
     hm x = genericLength ds * recip (sum $ map (recip . fromIntegral) ds)
            where ds = a027750_row x

instance OEIS 1600 where
  oeis =
     [numerator m | x <- [1..], let m = hm x, denominator m == 1] where
     hm x = genericLength divs * recip (sum $ map recip divs)
            where divs = map fromIntegral $ a027750_row x

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

-- instance OEIS 1614 where
--   oeis = f 0 0 a057211_list where
--      f c z (x:xs) = z' : f x z' xs where z' = z + 1 + 0 ^ abs (x - c)

-- instance OEIS 1615 where
--   oeis n = numerator (fromIntegral n * (product $
--               map ((+ 1) . recip . fromIntegral) $ a027748_row n))

instance OEIS 1616 where
  oeisIx (succ->n) = sum $ map (oeisIx @10 . pred) $ zipWith gcd ds $ reverse ds
    where ds = a027750_row n

instance OEIS 1629 where
  oeis = f [] $ tail $ oeis @45 where
     f us (v:vs) = (sum $ zipWith (*) us $ oeis @45) : f (v:us) vs

-- instance OEIS 1633 where
--   a001633_list = filter (odd . a055642) [0..]

instance OEIS 1634 where
  oeis = fix \r -> 0 : 2 : 3 : 6 : zipWith (+) r (zipWith (+) (tail r) (drop 2 r))

-- instance OEIS 1637 where
--   a001637 n = a001637_list !! (n-1)
--   a001637_list = filter (even . a055642) [0..]

instance OEIS 1644 where
  oeis = fix \r -> 3 : 1 : 3 : zipWith3 (((+) .) . (+)) r (tail r) (drop 2 r)

-- instance OEIS 1650 where
--   a001650 n k = a001650_tabf !! (n-1) !! (k-1)
--   a001650_row n = a001650_tabf !! (n-1)
--   a001650_tabf = iterate (\xs@(x:_) -> map (+ 2) (x:x:xs)) [1]
--   a001650_list = concat a001650_tabf

instance OEIS 1651 where
  oeisIx = (`div` 2) . (subtract 1) . (* 3)
  oeis   = filter ((/= 0) . (`mod` 3)) [1..]

instance OEIS 1652 where
  oeis = fix \r -> 0 : 3 : map (+ 2) (zipWith (-) (map (* 6) (tail r)) r)

instance OEIS 1653 where
  oeis = fix \r -> 1 : 5 : zipWith (-) (map (* 6) $ tail r) r

instance OEIS 1654 where
  oeis = zipWith (*) (tail (oeis @45)) (oeis @45)

-- instance OEIS 1682 where
--   a001682 n = a001682_list !! (n-1)
--   a001682_list = [k | k <- [0..], let m = 3^k, a055642 m == a055642 (9*m)]

-- instance OEIS 1690 where
--   a001690 n = a001690_list !! (n-1)
--   a001690_list = filter ((== 0) . a010056) [0..]

-- instance OEIS 1692 where
--   a001692 n = flip div n $ sum $
--               zipWith (*) (map a008683 divs) (map a000351 $ reverse divs)
--               where divs = a027750_row n

-- instance OEIS 1694 where
--   a001694 n = a001694_list !! (n-1)
--   a001694_list = filter ((== 1) . a112526) [1..]

instance OEIS 1696 where
  oeis = fix \r -> 0 : 1 : zipWith (-)
     (zipWith (+) a001696_list' $ map (^ 2) a001696_list')
     (zipWith (*) r a001696_list')
     where a001696_list' = tail $ oeis @1696

instance OEIS 1697 where
  oeis = 1 : 1 : f [1,1] where
     f xs@(x:_) = y : f (y : xs) where y = x * sum xs

-- instance OEIS 1700 where
--   a001700 n = a007318 (2*n+1) (n+1)

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

-- instance OEIS 1751 where
--   a001751 n = a001751_list !! (n-1)
--   a001751_list = 2 : filter (\n -> (a010051 $ div n $ gcd 2 n) == 1) [1..]


-- instance OEIS 1764 where
--   a001764 n = a001764_list !! n
--   a001764_list = 1 : [a258708 (2 * n) n | n <- [1..]]

-- instance OEIS 1768 where
--   a001768 n = n * (z - 1) - (2 ^ (z + 2) - 3 * z) `div` 6
--      where z = a085423 $ n + 1

-- instance OEIS 1783 where
--   a001783 = product . a038566_row

instance OEIS 1787 where
  oeisIx n = n * 2 ^ (n - 1)
  oeis = zipWith (*) [0..] $ 0 : oeis @79

instance OEIS 1788 where
  oeisIx n = if n < 2 then n else n * (n + 1) * 2 ^ (n - 2)
  oeis = zipWith (*) (oeis @217) $ 1 : oeis @79

instance OEIS 1789 where
  -- oeisIx n = oeisIx @7318 n 3 * 2 ^ (n - 3)
  oeis     = 1 : zipWith (+) (map (* 2) $ oeis @1789) (drop 2  $ oeis @1788)

-- instance OEIS 1792 where
--   a001792 n = a001792_list !! n
--   a001792_list = scanl1 (+) a045623_list

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

-- instance OEIS 1840 where
--   oeis = scanl (+) 0 a008620_list

instance OEIS 1844 where
  oeisIx n = 2 * n * (n + 1) + 1
  oeis = zipWith (+) (oeis @290) $ tail (oeis @290)

instance OEIS 1845 where
  oeisIx n = (2 * n + 1) * (2 * n ^ 2 + 2 * n + 3) `div` 3


instance OEIS 1855 where
  oeis = fix \r -> 0 : zipWith (+) [1..] do zipTail (+) . concat $ transpose [r, r]

-- instance OEIS 1857 where
--   a001857 n = a001857_list !! (n-1)
--   a001857_list = 2 : 3 : ulam 2 3 a001857_list

-- instance OEIS 1859 where
--   a001859 n = a000217 n + a002620 (n + 1)

instance OEIS 1870 where
  oeis = uncurry c $ splitAt 1 $ tail (oeis @45) where
     c us vs'@(v:vs) = (sum $ zipWith (*) us vs') : c (v:us) vs

instance OEIS 1906 where
  oeis = fix \r -> 0 : 1 : zipWith (-) (map (* 3) $ tail r) r

instance OEIS 1911 where
  oeis = fix \r -> 0 : 1 : map (+ 2) do zipTail (+) r

instance OEIS 1923 where
  oeis = scanl (+) 0 $ tail $ oeis @312

instance OEIS 1924 where
  oeis = drop 3 $ zipWith (-) (tail $ oeis @45) [0..]

-- instance OEIS 1935 where
--   a001935 = p a042968_list where
--      p _          0 = 1
--      p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

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

-- instance OEIS 1970 where
--   a001970 n = a001970_list !! (n-1)
--   a001970_list = 1 : f 1 [1] where
--      f x ys = y : f (x + 1) (y : ys) where
--               y = sum (zipWith (*) ys a061259_list) `div` x

instance OEIS 1971 where
  oeisIx = floor . (+ 0.5) . (/ 8) . fi . (^ 2)

-- instance OEIS 1983 where
--   a001983 n = a001983_list !! (n-1)
--   a001983_list = [x | x <- [0..], a025435 x > 0]

-- instance OEIS 2019 where
--   a002019 n = a002019_list !! n
--   a002019_list = 1 : 1 : zipWith (-)
--      (tail a002019_list) (zipWith (*) a002019_list a002378_list)

instance OEIS 2024 where
--   a002024 n k = a002024_tabl !! (n-1) !! (k-1)
--   a002024_row n = a002024_tabl !! (n-1)
--   a002024_tabl = iterate (\xs@(x:_) -> map (+ 1) (x : xs)) [1]
--   a002024_list = concat a002024_tabl
  oeisIx = round . sqrt . (* 2) . fi
  oeis = [1..] >>= \n -> genericReplicate n n
--   a002024 = (!!) $ [1..] >>= \n -> replicate n n

-- instance OEIS 2034 where
--   oeisIx 1 = 1
--   oeisIx n = fromJust (a092495 n `elemIndex` a000142_list)

-- instance OEIS 2035 where
--   a002035 n = a002035_list !! (n-1)
--   a002035_list = filter (all odd . a124010_row) [1..]

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

-- instance OEIS 2095 where
--   a002095 = p a018252_list where
--      p _          0 = 1
--      p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 2100 where
--   a002100 = p a006881_list where
--      p _          0 = 1
--      p ks'@(k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m


instance OEIS 2104 where
  oeisIx = genericLength . filter (\xs -> head xs == minimum xs)
         . tail . choices . enumFromTo 1

instance OEIS 2109 where
  oeis = scanl1 (*) $ oeis @312

instance OEIS 2110 where
  oeisIx n = product . genericTake n $ oeis @40
  oeis = scanl (*) 1 $ oeis @40

-- instance OEIS 2113 where
--     a002113 n = a002113_list !! (n-1)
--     a002113_list = filter ((== 1) . a136522) [1..] 

-- instance OEIS 2121 where
--   oeis = 1 : 0 : -1 : f 0 (-1) 3 where
--      f v w x = y : f w y (x + 1) where
--        y = sum (map (a002121 . (x -)) $ takeWhile (<= x) a065091_list) - v

-- instance OEIS 2122 where
--   oeis = uncurry conv $ splitAt 1 $ oeis @2121 where
--      conv xs (z:zs) = sum (zipWith (*) xs $ reverse xs) : conv (z:xs) zs

-- instance OEIS 2123 where
--   a002123_list = 0 : 0 : f 3 where
--      f x = y : f (x + 1) where
--        y = a061397 x -
--            sum (map (a002123 . (x -)) $ takeWhile (< x) a065091_list)

-- instance OEIS 2124 where
--   a002124_list = 1 : f 1 [] a065091_list where
--      f x qs ps'@(p:ps)
--        | p <= x    = f x (p:qs) ps
--        | otherwise = sum (map (a002124 . (x -)) qs) : f (x + 1) qs ps'

-- instance OEIS 2125 where
--   oeis = uncurry conv $ splitAt 1 a002124_list where
--      conv xs (z:zs) = sum (zipWith (*) xs $ reverse xs) : conv (z:xs) zs

instance OEIS 2131 where
  oeisIx (succ->n) = sum [d | d <- [1..n], mod n d == 0, odd $ div n d]

-- instance OEIS 2144 where
--   oeis = filter ((== 1) . a010051) [1,5..]

-- instance OEIS 2145 where
--   a002145 n = a002145_list !! (n-1)
--   a002145_list = filter ((== 1) . a010051) [3, 7 ..]

-- instance OEIS 2173 where
--   a002173 n = a050450 n - a050453 n

-- instance OEIS 2180 where
--   oeisIx = flip div 2 . oeisIx @2202

-- instance OEIS 2183 where
--   oeis = nub $ map (a000005 . a061799) [1..]

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


-- instance OEIS 2202 where
--   import Data.List.Ordered (insertSet)
--   a002202 n = a002202_list !! (n-1)
--   a002202_list = f [1..] (tail a002110_list) [] where
--      f (x:xs) ps'@(p:ps) us
--        | x < p = f xs ps' $ insertSet (a000010' x) us
--        | otherwise = vs ++ f xs ps ws
--        where (vs, ws) = span (<= a000010' x) us

instance OEIS 2203 where
  oeis' (A r) = 2 : 2 : zipWith (+) (map (* 2) $ tail r) r

instance OEIS 2260 where
  oeis = a002260_row =<< [0..]
a002260 n k = k
a002260_row n = [1..n]
a002260_tabl = iterate (\row -> map (+ 1) (0 : row)) [1]

instance OEIS 2262 where
  oeis = a002262_row =<< [0..]
--   a002262 n k = a002262_tabl !! n !! k
a002262_row n = a002262_tabl !! n
a002262_tabl = map (enumFromTo 0) [0..]
a002262_list = concat a002262_tabl

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

-- instance OEIS 2294 where
--   oeis = [a258708 (3 * n) (2 * n) | n <- [1..]]

-- instance OEIS 2296 where
--   a002296 n = a002296_list !! n
--   a002296_list = [a258708 (4 * n) (3 * n) | n <- [1..]]


instance OEIS 2311 where
  oeis = filter f [1..] where
     f x = not $ null $ intersect txs $ map (tx -) $ txs where
         txs = takeWhile (< tx) $ oeis @292; tx = oeisIx @292 x

-- instance OEIS 2312 where
--   a002312 n = a002312_list !! (n-1)
--   a002312_list = filter (\x -> 2 * x > a006530 (x ^ 2 + 1)) [1..]

instance OEIS 2313 where
  oeis = filter ((`elem` [1,2]) . (`mod` 4)) $ oeis @40

instance OEIS 2315 where
  oeis' (A r) = 1 : 7 : zipWith (-) (map (* 6) (tail r)) r

-- instance OEIS 2321 where
--   import Data.List (genericIndex)
--   a002321 n = genericIndex a002321_list (n-1)
--   a002321_list = scanl1 (+) a008683_list

-- instance OEIS 2322 where
--   a002322 n = foldl lcm 1 $ map (a207193 . a095874) $
--                             zipWith (^) (a027748_row n) (a124010_row n)

instance OEIS 2324 where
  oeisIx n = oeisIx @1817 n - oeisIx @1822 n

instance OEIS 2326 where
  oeisIx n = fi . (+ 1) $ fromJust $ findIndex ((== 0) . (`mod` (2 * n + 1))) $ tail $ oeis @225

-- instance OEIS 2327 where
--   a002327 n = a002327_list !! (n-1)
--   a002327_list = filter ((== 1) .  a010051') a028387_list

-- instance OEIS 2348 where
--   a002348 n = product (zipWith d ps es) * 4 ^ e0 `div` 8 where
--      d p e = (p ^ 2 - 1) * p ^ e
--      e0 = if even n then head $ a124010_row n else 0
--      es = map ((* 2) . subtract 1) $
--               if even n then tail $ a124010_row n else a124010_row n
--      ps = if even n then tail $ a027748_row n else a027748_row n

-- instance OEIS 2372 where
--   a002372 n = sum $ map (a010051 . (2*n -)) $ takeWhile (< 2*n) a065091_list

-- instance OEIS 2373 where
--   a002373 n = head $ dropWhile ((== 0) . a010051 . (2*n -)) a065091_list 

-- instance OEIS 2375 where
--   a002375 n = sum $ map (a010051 . (2 * n -)) $ takeWhile (<= n) a065091_list

instance OEIS 2378 where
  oeisIx n = n * (n + 1)
  oeis = zipWith (*) [0..] [1..]

instance OEIS 2379 where
  oeisIx n = 3^n `div` 2^n  

instance OEIS 2380 where
  oeisIx n = 3^n `mod` 2^n  

-- instance OEIS 2385 where
--   a002385 n = a002385_list !! (n-1)
--   a002385_list = filter ((== 1) . a136522) a000040_list

instance OEIS 2387 where
  oeis = f 0 1 where
     f x k = if genericIndex hs k > fi x
             then k : f (x + 1) (k + 1) else f x (k + 1)
             where hs = scanl (+) 0 $ map recip [1..]


instance OEIS 2411 where
  oeisIx n = n * oeisIx @217 n

-- instance OEIS 2426 where
--   a002426 n = a027907 n n  

instance OEIS 2450 where
  -- oeisIx = (`div` 3) . a024036
  oeis = iterate ((+ 1) . (* 4)) 0

-- instance OEIS 2457 where
--   a002457 n = a116666 (2 * n + 1) (n + 1)

-- instance OEIS 2471 where
--   a002471 n = sum $ map (a010051 . (n -)) $ takeWhile (< n) a000290_list

instance OEIS 2472 where
  oeisIx (succ->n) = genericLength [x | x <- [1..n], gcd n x == 1, gcd n (x + 2) == 1]

-- instance OEIS 2473 where
--   import Data.Set (singleton, deleteFindMin, fromList, union)
--   a002473 n = a002473_list !! (n-1)
--   a002473_list = f $ singleton 1 where
--      f s = x : f (s' `union` fromList (map (* x) [2,3,5,7]))
--            where (x, s') = deleteFindMin s

instance OEIS 2476 where
  oeis = filter ((== 1) . (`mod` 6)) $ oeis @40

-- instance OEIS 2479 where
--   a002479 n = a002479_list !! (n-1)
--   a002479_list = 0 : filter f [1..] where
--      f x = all (even . snd) $ filter ((`elem` [5,7]) . (`mod` 8) . fst) $
--                               zip (a027748_row x) (a124010_row x)

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

-- instance OEIS 2496 where
--   a002496 n = a002496_list !! (n-1)
--   a002496_list = filter ((== 1) . a010051') a002522_list


-- instance OEIS 2503 where
--   a002503 n = a002503_list !! (n-1)
--   a002503_list = map (+ 1) $ elemIndices 0 a065350_list

-- instance OEIS 2516 where
--   a002516 n = a002516_list !! n
--   a002516_list = 0 : concat (transpose
--   [a004767_list, f a002516_list, a017089_list, g $ drop 2 a002516_list])
--   where f [z] = []; f (_:z:zs) = 2 * z : f zs

instance OEIS 2517 where
  oeis' (A r) = 0 : concat (transpose [[2, 5 ..], [3, 12 ..], map (* 3) $ tail r])

instance OEIS 2522 where
  oeisIx = (+ 1) . (^ 2)
  oeis   = scanl (+) 1 [1,3..]

instance OEIS 2541 where
  oeisIx (succ->n) = sum $ zipWith div [n - 1, n - 2 ..] [1 .. n - 1]

-- instance OEIS 2577 where
--   import Data.MemoCombinators (memo2, list, integral)
--   a002577 n = a002577_list !! n
--   a002577_list = f [1] where
--      f xs = (p' xs $ last xs) : f (1 : map (* 2) xs)
--      p' = memo2 (list integral) integral p
--      p _ 0 = 1; p [] _ = 0
--      p ks'@(k:ks) m = if m < k then 0 else p' ks' (m - k) + p' ks m


instance OEIS 2605 where
  oeis' (A r) = 0 : 1 : map (* 2) (zipTail (+) r)

-- instance OEIS 2616 where
--   oeisIx = flip div 2 . oeisIx @2322

-- instance OEIS 2618 where
--   oeisIx n = oeisIx @10 n * n

instance OEIS 2620 where
  oeisIx = (`div` 4) . (^ 2)

instance OEIS 2627 where
  oeis' (A r) = 0 : map (+ 1) (zipWith (*) [1..] r)

instance OEIS 2635 where
  oeisIx = p (tail $ oeis @290) 4 where
    p ks'@(k:ks) c m = if m == 0 then 1 else
      if c == 0 || m < k then 0 else p ks' (c - 1) (m - k) + p ks c m

-- instance OEIS 2645 where
--   a002645 n = a002645_list !! (n-1)
--   a002645_list = 2 : (map a000040 $ filter ((> 1) . a256852) [1..])

-- instance OEIS 2646 where
--   a002646 n = a002646_list !! (n-1)
--   a002646_list = [hqp | x <- [1, 3 ..], y <- [1, 3 .. x - 1],
--                         let hqp = div (x ^ 4 + y ^ 4) 2, a010051' hqp == 1]

-- instance OEIS 2654 where
--   a002654 n = product $ zipWith f (a027748_row m) (a124010_row m) where
--      f p e | p `mod` 4 == 1 = e + 1
--            | otherwise      = (e + 1) `mod` 2
--      m = a000265 n

-- instance OEIS 2658 where
--   oeis = 1 : 1 : f [1,1] where
--      f (x:xs) = y : f (y:x:xs') where y = x * sum xs + x * (x + 1) `div` 2

-- instance OEIS 2662 where
--   a002662 n = a002662_list !! n
--   a002662_list = map (sum . drop 3) a007318_tabl

-- instance OEIS 2663 where
--   a002663 n = a002663_list !! n
--   a002663_list = map (sum . drop 4) a007318_tabl

-- instance OEIS 2664 where
--   a002664 n = a002664_list !! n
--   a002664_list = map (sum . drop 5) a007318_tabl

-- instance OEIS 2690 where
--   a002690 n = a245334 (2 * n) n  

-- instance OEIS 2694 where
--   a002694 n = a007318' (2 * n) (n - 2)  

-- instance OEIS 2731 where
--   oeis = filter ((== 1) . a010051 . a000982) [1, 3 ..]

-- instance OEIS 2733 where
--   oeisIx = oeisIx @196 . (subtract 1) . (* 10) . a207337

-- instance OEIS 2778 where
--   a002778 n = a002778_list !! (n-1)
--   a002778_list = filter ((== 1) . a136522 . (^ 2)) [0..]

-- instance OEIS 2779 where
--   a002779 n = a002778_list !! (n-1)
--   a002779_list = filter ((== 1) . a136522) a000290_list

-- instance OEIS 2782 where
--   a002782 n = a002782_list !! (n-1)
--   a002782_list = f 1 1 (map toInteger $ tail a007376_list) where
--      f x y (d:ds) | mod y x == 0 = y : f y d ds
--                   | otherwise    = f x (10*y + d) ds

instance OEIS 2796 where
  oeis = filter f [1..] where
     f x = all ((== 0) . mod x) ds where
       ds = map (fi.digitToInt) (if c == '0' then cs else cs')
       cs'@(c:cs) = nub $ sort $ (show :: Int -> String) (fi x)


instance OEIS 2805 where
  oeisIx = denominator . sum . map (1 %) . enumFromTo 1
  oeis = map denominator $ scanl1 (+) $ map (1 %) [1..]

-- instance OEIS 2808 where
--   a002808 n = a002808_list !! (n-1)
--   a002808_list = filter ((== 1) . a066247) [2..]

instance OEIS 2814 where
  oeis = 1 : zipWith div (tail xs) xs
     where xs = map (oeisIx @45) (oeis @244)

-- instance OEIS 2815 where
--   a002815 0 = 0
--   a002815 n = a046992 n + toInteger n  

-- instance OEIS 2819 where
--   a002819_list = scanl (+) 0 a008836_list

instance OEIS 2821 where
  oeisIx = round . sqrt . fi . (^ 3)

instance OEIS 2822 where
  oeis = f $ oeis @40 where
     f (q:ps'@(p:ps)) | p > q + 2 || r > 0 = f ps'
                      | otherwise = y : f ps where (y,r) = divMod (q + 1) 6

-- instance OEIS 2828 where
--   a002828 0 = 0
--   a002828 n | a010052 n == 1 = 1
--             | a025426 n > 0 = 2 | a025427 n > 0 = 3 | otherwise = 4

-- instance OEIS 2858 where
--   a002858 n = a002858_list !! (n-1)
--   a002858_list = 1 : 2 : ulam 2 2 a002858_list
--   ulam :: Int -> Integer -> [Integer] -> [Integer]
--   ulam n u us = u' : ulam (n + 1) u' us where
--      u' = f 0 (u+1) us'
--      f 2 z _                         = f 0 (z + 1) us'
--      f e z (v:vs) | z - v <= v       = if e == 1 then z else f 0 (z + 1) us'
--                   | z - v `elem` us' = f (e + 1) z vs
--                   | otherwise        = f e z vs
--      us' = take n us

-- instance OEIS 2859 where
--   a002859 n = a002859_list !! (n-1)
--   a002859_list = 1 : 3 : ulam 2 3 a002859_list

-- instance OEIS 2868 where
--   a002868 n = if n == 0 then 1 else maximum $ map abs $ a008297_row n

-- instance OEIS 2869 where
--   a002869 0 = 1
--   a002869 n = maximum $ a019538_row n

instance OEIS 2878 where
  oeis = zipWith (+) (tail $ oeis @1906) $ oeis @1906

-- instance OEIS 2889 where
--   oeis' (A r) = 1 : 10 : 56 : zipWith (+)
--      (zipWith (-) (map (* 2) $ drop 2 r) r)
--      (drop 2 $ zipWith (+) (tail $ oeis @2941) $ oeis @2941)
