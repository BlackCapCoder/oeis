module OEIS.Broken where

{-

instance OEIS 242192 where
  oeisIx n = sum $ map (oeisIx . (n ^ 4 -)) $
                        takeWhile (< n ^ 4) $ map (^ 3) [1..]

instance OEIS 244408 where
  oeis = map (* 2) $ filter f [2..] where
     f x = sqrt (fi $ 2 * x) <= fi (oeisIx x)

instance OEIS 45532 where
  oeisIx n = read $ show n ++ show (oeisIx n) :: Integer

instance OEIS 45981 where
  oeis = 1 : f 1 [] where
     f x zs = y : f y zs' where
       y = read (concatMap show zs')
       zs' = zs ++ [oeisIx x]

instance OEIS 48385 where
  oeisIx 0 = 0
  oeisIx n = read (show (oeisIx n') ++ show (m ^ 2)) :: Integer
              where (n', m) = divMod n 10

instance OEIS 48766 where
  oeisIx = round . (** (1/3)) . fi
  oeis = concatMap (\x -> take (oeisIx x) $ repeat x) [0..]

instance OEIS 50480 where
  oeis = filter chi [2..] where
     chi x = xs `elem` (map concat $ choices divs) where
        choices = concat . (map permutations) . subsequences
        divs = filter (`isInfixOf` xs)
                      $ map show $ filter ((== 0) . mod x) [1..oeisIx x]
        xs = show x

instance OEIS 52018 where
  oeis = filter f [0..] where
     f x = show (oeisIx x) `isInfixOf` show x

instance OEIS 52109 where
  oeis = 1 : f 2 [1] where
     f n xs = z : f (n+1) (z:xs) where
       z = sum $ map (oeisIx . fromInteger) $
                     dropWhile (<= 0) $ map (n -) xs

instance OEIS 52227 where
  oeisIx n = (oeisIx n) * (oeisIx n) `div` (oeisIx n)

instance OEIS 57683 where
  oeis = filter (all (== 1) . p) [1..] where
     p x = map (oeisIx . (+ (x + 1)) . (x ^)) [2..4]

instance OEIS 61984 where
  oeis = 0 : map (+ 1) (zipWith (+)
     (map (oeisIx . (`div` 2)) [1..]) (map (oeisIx . (`div` 3)) [1..]))

instance OEIS 62251 where
  oeisIx n = (oeisIx n + 1) * n - 1

instance OEIS 62289 where
  oeis = 2 : g 2 where
     g n = nM n : g (n+1)
     nM k = maximum $ map (\i -> i + min i (oeisIx $ k-i+1)) [2..k]


instance OEIS 63510 where
  oeisIx 1 = 1
  oeisIx n = (oeisIx @63510) (oeisIx n) + 1

instance OEIS 65435 where
  oeis = 2 : 3 : zipWith (+) xs (tail xs) where
                 xs = map (oeisIx . fromInteger) (oeis @65435)

instance OEIS 68341 where
  oeis = f 1 [] where
     f x ms = (sum $ zipWith (*) ms' $ reverse ms') : f (x + 1) ms' where
       ms' = (oeisIx x) : ms

instance OEIS 69720 where
  oeisIx n = (oeisIx $ n - 1) * (oeisIx $ n - 1)

instance OEIS 71176 where
  oeisIx n = fromJust $ findIndex (== 1) $
              map (oeisIx . read . (show n ++) . show) [0..]

instance OEIS 72046 where
  oeisIx n = gcd (oeisIx n) (oeisIx n)

instance OEIS 74963 where
  oeisIx n = maximum [oeisIx (x*y) | x <- [1..n], y <- [x..n]]

instance OEIS 75110 where
  oeisIx (fi->n) = fi . read $ show (oeisIx n) ++ show n

instance OEIS 75680 where
  oeisIx n = snd $ until ((== 1) . fst)
              (\ (x, i) -> (oeisIx (3 * x + 1), i + 1)) (2 * n - 1, 0)

instance OEIS 76846 where
  oeisIx n = n ^ (oeisIx n) + n - 1

instance OEIS 76941 where
  oeisIx n = 2 ^ (oeisIx n) * 3 ^ (oeisIx n)

instance OEIS 80080 where
  oeisIx :: Int -> Int -> Int
  oeisIx n k = addc n k 0 where
     addc x y z | y == 0    = z - 1
                | otherwise = addc (x `xor` y) (shiftL (x .&. y) 1) (z + 1)
  oeisIx_row n = map (oeisIx n) [1..n]
  oeisIx_tabl = map (rowT @80080) [1..]

instance OEIS 80655 where
  oeis = 1 : 1 : f 3 where
     f n = (oeisIx (n - 1) + (oeisIx @80655) (oeisIx (n-1))) : f (n+1)

instance OEIS 82143 where
  oeisIx 0 = 1
  oeisIx n = (oeisIx $ n - 1) * (oeisIx n)

instance OEIS 86417 where
  oeisIx n = (2 ^ (oeisIx n + 1) - 1) * (3 ^ (oeisIx n + 1) - 1) `div` 2

instance OEIS 86793 where
  oeisIx = f 0 where
     f y x = if x == 15 then y else f (y + 1) (oeisIx x)

instance OEIS 86799 where
  oeisIx n | even n    = (oeisIx $ div n 2) * 2 + 1
            | otherwise = n

instance OEIS 89072 where
  oeisIx = flip (^)
  oeisIx_row n = map (oeisIx n) [1..n]
  oeisIx_tabl = map (rowT @89072) [1..]

instance OEIS 90425 where
  oeisIx n = snd $ until ((== 1) . fst)
                          (\ (u, v) -> (oeisIx u, v + 1)) (oeisIx n, 1)

instance OEIS 92694 where
  oeisIx n = snd $ until ((== 1) . fst) f (oeisIx n, 1) where
     f (x, p) = (oeisIx x, p * x)

instance OEIS 93483 where
  oeis = f ([2..7] ++ [8,14..]) [] where
     f (x:xs) ys = if all (== 1) $ map (oeisIx . (+ x)) ys
                      then x : f xs ((x+1):ys) else f xs ys

instance OEIS 93640 where
  oeisIx n  = genericLength [d | d <- [1..n], mod n d == 0,
                           show (oeisIx d) `isInfixOf` show (oeisIx n)]

instance OEIS 95890 where
  oeis = tablList @95890
instance Table 95890 where
  rowCol n k = (n - k + 1) ^ (n - k)
  rowT n = map (oeisIx n) [1..n]
  tabl = map (rowT @95890) [1..]

instance OEIS 96781 where
  oeisIx = a . a where a = (oeis `genericIndex`) . subtract 1

instance OEIS 97364 where
  oeis = tablList @97364
instance Table 97364 where
  rowCol n k = length [qs | qs <- pss !! n, last qs - head qs == k] where
     pss = [] : map parts [1..] where
           parts x = [x] : [i : ps | i <- [1..x],
                                     ps <- pss !! (x - i), i <= head ps]
  rowT n = map (oeisIx n) [0..n - 1]
  tabl = map (rowT @97364) [1..]

instance OEIS 99245 where
  oeisIx n = numerator $ (oeisIx n) % (oeisIx n)

instance OEIS 99246 where
  oeisIx n = denominator $ (oeisIx n) % (oeisIx n)

instance OEIS 99305 where
  oeis = f 1 $ h 1 empty where
     f x ad = y : f (x + 1) (h (3 * x + 1) ad)  where
              y = length [ () | k <- [1 .. 2 * x],
                               let x' = ad ! x, ad ! (x + k) == x' + ad ! k]
     h z = insert z (oeisIx z) .
            insert (z+1) (oeisIx (z+1)) . insert (z+2) (oeisIx (z+2))

instance OEIS 103689 where
  oeisIx n = min (oeisIx n) (oeisIx n)

instance OEIS 106371 where
  oeis = map fromJust $ takeWhile (/= Nothing) $ map f [1..] where
     f n = g 2 n where
       g b x = h x 0 where
         h 0 y = if b <= 10 then Just (oeisIx y) else Nothing
         h z y = if r == 0 then g (b + 1) n else h z' (10 * y + r)
                 where (z', r) = divMod z b

instance OEIS 106747 where
  oeisIx n = if n == 0 then 0 else 10 * (oeisIx n') + div d 2
              where (n', d) = divMod n 10

instance OEIS 108775 where
  oeisIx n = div (oeisIx n) n

instance OEIS 108839 where
  oeis = 1 : f 2 [1] where
     f x zs = z : f (x + 1) (z : zs) where
       z = toInteger $ sum $ map (oeisIx . (+ x)) zs

instance OEIS 114897 where
  oeis = 1 : f 1 [1] where
     f x zs = z : f (x + 1) (z : zs) where
       z = toInteger $ sum $ map (oeisIx . (+ x)) zs

instance OEIS 122840 where
  oeisIx n = if n < 10 then 0 ^ n else 0 ^ d * (oeisIx n' + 1)
              where (n', d) = divMod n 10

instance OEIS 139366 where
  oeisIx 1 1               = 0
  oeisIx n k | gcd n k > 1 = 0
              | otherwise   = head [r | r <- [1..], k ^ r `mod` n == 1]
  oeisIx_row n = map (oeisIx n) [1..n]
  oeisIx_tabl = map (rowT @139366) [1..]

instance OEIS 139399 where
  oeisIx = f 0 where
     f k x = if x `elem` [1,2,4] then k else f (k + 1) (oeisIx x)

instance OEIS 152723 where
  oeisIx n = min (oeisIx n) (oeisIx n)

instance OEIS 152724 where
  oeisIx n = max (oeisIx n) (oeisIx n)

instance OEIS 175688 where
  oeis = filter f [0..] where
     f x = m == 0 && ("0123456789" !! avg) `elem` show x
           where (avg, m) = divMod (oeisIx x) (oeisIx x)

instance OEIS 178358 where
  oeisIx (fi->n) = fi do read $ show (oeisIx n) ++ show n :: Integer

instance OEIS 178649 where
  oeisIx n = div (oeisIx n) (oeisIx n)

instance OEIS 180094 where
  oeisIx n = snd $ until ((< 2) . fst) (\ (x, c) -> (oeisIx x, c+1)) (n,0)

instance OEIS 185137 where
  oeis = 1 : 1 : 1 : 1 : 1 : 1 : f 7 1 1 where
     f x u v = w : f (x + 1) v w where
               w = (oeisIx . (oeisIx @185137) . (oeisIx @185137)) (x - 1) +
                   (oeisIx @185137) (x - (oeisIx . (oeisIx @185137)) (x - 3))

instance OEIS 185816 where
  oeisIx n = if n == 1 then 0 else (oeisIx @185816) (oeisIx n) + 1

instance OEIS 194020 where
  oeisIx n = p (oeisIx n) n where
     p _  0 = 1
     p k m | m < k     = 0
           | otherwise = p k (m - k) + p (k+1) m

instance OEIS 198259 where
  oeisIx n = sum $ map (oeisIx . (mod n)) [1..n]

instance OEIS 200612 where
  oeis = filter f [2..] where
     f x = r == 0 && x' == 3 where (x',r) = divMod (oeisIx x) (oeisIx x)

instance OEIS 200996 where
  oeisIx n = max (oeisIx n) (oeisIx n)

instance OEIS 209297 where
  oeis = tablList @209297
instance Table 209297 where
  rowCol n k = k * n + k - n
  rowT n = map (oeisIx n) [1..n]
  tabl = map (rowT @209297) [1..]

instance OEIS 212133 where
  oeisIx n = if n == 0 then 0 else (oeisIx n + 1) `div` 2

instance OEIS 219117 where
  oeis = filter (all (== 1) . p) [1..] where
     p x = map (oeisIx . (+ (x + 1)) . (x ^)) [1..4]

instance OEIS 230107 where
  oeisIx = fromMaybe (-1) . f (10^5) 1 1 1 where
     f k i u j v | k <= 0    = Nothing
                 | u < v     = f (k - 1) (i + 1) (oeisIx u) j v
                 | u > v     = f (k - 1) i u (j + 1) (oeisIx v)
                 | otherwise = Just j

instance OEIS 245300 where
  oeis = tablList @245300
  rowCol n k = (n + k) * (n + k + 1) `div` 2 + k
  rowT n = map (oeisIx n) [0..n]
  tabl = map (rowT @245300) [0..]
  oeis = concat (tabl @245300)

instance OEIS 248024 where
  oeis = 1 : f 1 [2..] where
    f x zs = g zs where
      g (y:ys) = if x `mod` (oeisIx y) == 0
                 then y : f y (delete y zs) else g ys

instance OEIS 249278 where
  oeis = 0 : f 0 [1..] where
     f u vs = g vs where
       g (x:xs) = if (oeisIx x) `mod` 2 == u `mod` 2
                     then x : f x (delete x vs) else g xs

instance OEIS 249669 where
  oeisIx n = floor $ fi (oeisIx n) ** (1 + recip (fi n))

instance OEIS 249873 where
  oeisIx n = if n == 0 then 0 else 100*oeisIx n' + 10*oeisIx (2*t) + d
              where (n', td) = divMod n 100; (t, d) = divMod td 10

instance OEIS 250030 where
  oeisIx n = snd $ until ((== 5) . fst)
                    (\ (x, s) -> (oeisIx x, s + 1)) (oeisIx n, 1)

instance OEIS 6889 where
  oeisIx = fromJust . (`elemIndex` (oeis @224782))
instance OEIS 6942 where
  oeis = [6,2,5,5,4,5,6,3,7,6] ++ f 10 where
     f x = (oeisIx x' + (oeisIx @6942) d) : f (x + 1)
           where (x',d) = divMod x 10

instance OEIS 10371 where
  oeis = [6,2,5,5,4,5,6,4,7,6] ++ f 10 where
     f x = (oeisIx x' + (oeisIx @10371) d) : f (x + 1)
           where (x',d) = divMod x 10

instance OEIS 11776 where
  oeisIx 1 = 1
  oeisIx n = genericLength $
     takeWhile ((== 0) . (mod (oeisIx n))) $ iterate (* n) n

instance OEIS 14684 where
  oeisIx n = n - fi (oeisIx n)

instance OEIS 18896 where
  oeis = replicate 8 1 ++ f 8 where
     f x = ((oeisIx (x - 1) * (oeisIx @18896) (x - 7) + (oeisIx @18896) (x - 4) ^ 2)
           `div` (oeisIx @18896) (x - 8)) : f (x + 1)

instance OEIS 19294 where
  oeisIx n = snd $ until ((== 0) . (`mod` n) . fst)
                          (\ (x, i) -> (oeisIx x, i + 1)) (oeisIx n, 1)

instance OEIS 25492 where
  oeisIx n = fst $ until (uncurry (==)) (\ (x,x') -> (oeisIx x, x)) (n,0)

instance OEIS 34970 where
  oeis = 2 : 3 : (map (oeisIx . (subtract 1)) $
                          zipWith (*) (oeis @34970) $ tail (oeis @34970))

instance OEIS 36829 where
  oeisIx n = sum $ map
     (\k -> (oeisIx (3*k) k) * (oeisIx (3*n - 3*k-2) (n-k-1))) [0..n-1]

instance OEIS 38556 where
  oeisIx (fi->n) = fi do n `xor` (oeisIx $ 2 * n + 1) :: Integer

instance OEIS 38573 where
  oeisIx 0 = 0
  oeisIx n = (m + 1) * (oeisIx n') + m where (n', m) = divMod n 2

instance OEIS 38585 where
  oeisIx 0 = 0
  oeisIx n = (9 * m + 1) * (oeisIx n') + m where (n', m) = divMod n 2


instance OEIS 55742 where
  oeis = [x | x <- [1..], (oeisIx @1221 . pred) x == (oeisIx @1221 . pred) (oeisIx x)]

instance OEIS 55744 where
  oeis = 1 : filter f [2..] where
     f x = all ((== 0) . mod x) (concatMap (oeisIx_row . subtract 1) ps) &&
           all ((== 0) . mod (oeisIx x))
               (map fst $ filter ((== 1) . snd) $ zip ps $ (rowT @124010) x)
           where ps = (rowT @27748) x

instance OEIS 56045 where
  oeisIx n = sum $ map (oeisIx n) $ (rowT @27750) n

instance OEIS 58971 where
  oeisIx n = f [n % 1] where
     f xs@ (x:_) | denominator y == 1 = numerator y
                | y `elem` xs        = 0
                | otherwise          = f (y : xs)
                where y = (oeisIx x') % (oeisIx x')
                      x' = numerator x + denominator x

instance OEIS 59514 where
  oeisIx n = f [n % 1] where
     f xs@ (x:_)
       | denominator y == 1 = numerator y
       | y `elem` xs        = 0
       | otherwise          = f (y : xs)
       where y = (numerator x * denominator x) %
                 (oeisIx (numerator x) + (oeisIx @7953) (denominator x) - 1)


instance OEIS 60837 where
  oeisIx n = (oeisIx n ^ 2) *
     product (zipWith (^) (oeisIx_row m)
                          (map ((subtract 1) . (* 2)) (oeisIx_row m)))
     where m = (oeisIx @20653) n

instance OEIS 61797 where
  oeisIx 0 = 1
  oeisIx n = head [k | k <- [1..], let x = k * n,
                   all (`elem` "02468") $ show x, (oeisIx @136522) (oeisIx x) == 1]

instance OEIS 61909 where
  oeis = filter (\x -> (oeisIx @4086) (x^2) == (oeisIx x)^2) [0..]

instance OEIS 62293 where
  oeisIx 0 = 0
  oeisIx n = head [x | x <- map (* n) [1..],
                   all (`elem` "02468") $ show x, (oeisIx @136522) (oeisIx x) == 1]

instance OEIS 64272 where
  oeisIx n = sum $
     map (oeisIx . (n -)) $ takeWhile (< n) $ tail (oeis @290)

instance OEIS 64989 where
  oeisIx 1 = 1
  oeisIx n = product $ map (oeisIx . (oeisIx @49084)) $ (rowT @27746) n

instance OEIS 65371 where
  oeisIx 1 = 1
  oeisIx n = product $ map (oeisIx . (oeisIx @49084)) $ (rowT @27746) n

instance OEIS 65428 where
  oeis = filter f [1..] where
     f x = all (== 0) $
           map (oeisIx' . (`mod` x) . (oeisIx @290)) [oeisIx x .. x-1]

instance OEIS 65602 where
  oeis = tablList @65602
instance Table 65602 where
  rowCol n k = sum
     [ (k-1+2*j) * (oeisIx @7318)' (2*n-k-1-2*j) (n - 1) `div` (2*n-k-1-2*j) |
      j <- [0 .. div (n-k) 2]]
  rowT n = map (oeisIx n) [2..n]
  tabl = map (rowT @65602) [2..]

instance OEIS 67513 where
  oeisIx = sum . map (oeisIx . (+ 1)) . (rowT @27750)

instance OEIS 69090 where
  oeis = filter
     (all (== 0) . map (oeisIx . read) . init . tail . inits . show)
     (oeis @40)

instance OEIS 71330 where
  oeisIx n = sum $
     map (oeisIx . (n -)) $ takeWhile (<= n `div` 2) (oeis @961)

instance OEIS 72779 where
  oeisIx n = (oeisIx @1157) n + (oeisIx n) * (oeisIx n)

instance OEIS 72911 where
  oeisIx = product . map (oeisIx . fi) . (rowT @124010)

instance OEIS 73046 where
  oeisIx n = head $ dropWhile (== 0) $
                     zipWith (*) prims $ map (oeisIx . (2*n -)) prims
     where prims = takeWhile (<= n) (oeis @40)

instance OEIS 73601 where
  oeisIx n = 2 + length
     (takeWhile ((oeisIx n /=) . (oeisIx @30)) $ iterate (* n) (n^2))

instance OEIS 74206 where
  oeisIx n | n <= 1 = n
  | otherwise = 1 + (sum $ map (oeisIx . (div n)) $
  tail $ (rowT @27751) n)

instance OEIS 76399 where
  oeisIx n = (oeisIx @1222) (oeisIx n) * (oeisIx @25479) n

instance OEIS 77040 where
  oeis = map (oeisIx . (+ 1)) $ findIndices (<= 0) $
     zipWith (\s p -> abs s - p) (oeis @77039) (oeis @40)

instance OEIS 77041 where
  oeis = map (oeisIx . (+ 1)) $ findIndices (> 0) $
     zipWith (\s p -> abs s - p) (oeis @77039) (oeis @40)

instance OEIS 78465 where
  oeisIx n = (oeis @78465) `genericIndex` (n - 1)
  oeis = 1 : 1 : f 3 where
     f x = (sum $ map (oeisIx . (x -)) $
           takeWhile (< x) (oeis @40)) : f (x + 1)

instance OEIS 79070 where
  oeisIx n = genericLength $ elemIndices (oeisIx n) $ map (oeisIx @23416) [1..n - 1]

instance OEIS 80688 where
  oeis = tablList @80688
  rowCol n k = (rowT @80688) n !! (k-1)
  rowT n = map (+ 1) $ take (oeisIx n) $
                  elemIndices n $ map fromInteger (oeis @64553)
  tabl = map (rowT @80688) [1..]
  oeis = concat (tabl @80688)

instance OEIS 83368 where
  oeis = concat $ h $ drop 2 (oeis @71) where
     h (a:fs@ (a':_)) = (map (oeisIx . (a' -)) [a .. a' - 1]) : h fs

instance OEIS 83910 where
  oeisIx = sum . map (oeisIx . (oeisIx @10879)) . (rowT @27750)

instance OEIS 84349 where
  oeis = 1 : filter (\x -> all (== 0) $ map (oeisIx . (x -)) $
                             takeWhile (<= x) (oeis @290)) (oeis @5117)

instance OEIS 88956 where
  oeis = tablList @88956
instance Table 88956 where
  rowCol n k =  (oeisIx @95890) (n + 1) (k + 1) * (oeisIx @7318)' n k `div` (n - k + 1)
  rowT n = map (oeisIx n) [0..n]
  tabl = map (rowT @88956) [0..]

instance OEIS 89233 where
  oeisIx n = sum $ [oeisIx $ gcd u v | let ds = tail $ (rowT @27750) n,
                                         u <- ds, v <- dropWhile (<= u) ds]

instance OEIS 94048 where
  oeisIx n = head [m | m <- map (oeisIx . subtract 1 . (* (oeisIx @2144) n))
                                 (tail (oeis @290)), m > 0]

instance OEIS 97356 where
  oeisIx n = p [1..oeisIx n] n where
     p [] _ = 0
     p _  0 = 1
     p ks'@ (k:ks) m | m < k     = 0
                    | otherwise = p ks' (m - k) + p ks m

instance OEIS 98006 where
  oeisIx n = (oeisIx @5097) (n - 1) - (oeisIx @10) (oeisIx n)

instance OEIS 98294 where
  oeisIx 0  = 0
  oeisIx n  = fromJust (oeisIx n `elemIndex` (rowT @227048) n) + 1

instance OEIS 98983 where
  oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (< n) (oeis @40)

instance OEIS 99620 where
  oeisIx n = f (p - 1) $ drop (oeisIx n) (oeis @98962) where
     f c (x:xs) | c == 1 = if m == 0 then x else f 1 xs
                | m /= 0 = f c xs
                | m == 0 = f (c - if x' == p then 2 else 1) xs
                where (x',m) = divMod x p
     p = (oeisIx @40) n

instance OEIS 100949 where
  oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (< n) (oeis @1358)

instance OEIS 102370 where
  oeis = 0 : map (oeisIx . toInteger) (oeis @62289)

instance OEIS 102371 where
  oeis = map (oeisIx . toInteger) $ tail (oeis @225)

instance OEIS 103339 where
  oeisIx = numerator . uhm where uhm n = (n * (oeisIx @34444) n) % (oeisIx n)

instance OEIS 103340 where
  oeisIx = denominator . uhm where uhm n = (n * (oeisIx @34444) n) % (oeisIx n)

instance OEIS 105179 where
  oeis = 1 : filter (\x -> (oeisIx @10879) (oeisIx x) == (oeisIx @10879) x) [2..]

instance OEIS 105221 where
  oeisIx n = (oeisIx @8472) n - n * fi (oeisIx n)

instance OEIS 107345 where
  oeisIx n = (oeisIx @7318)' (oeisIx n) (oeisIx n)

instance OEIS 108655 where
  oeis = filter f (oeis @40) where
     f p = any (> 0) $ map (oeisIx . (oeisIx @37213) . (p -)) $
                           takeWhile (< p) (oeis @74985)

instance OEIS 114263 where
  oeisIx n = head [m | m <- [1..n],
                        (oeisIx @10051 . pred) (oeisIx n + 2 * (oeisIx @40) (n + m)) == 1]

instance OEIS 118478 where
  oeisIx n = (+ 1) . fromJust $ elemIndex 0 $
              map (flip mod (oeisIx n)) $ tail (oeis @2378)

instance OEIS 118954 where
  oeis = filter f [1..] where
     f x = all (== 0) $ map (oeisIx . (x -)) $ takeWhile (< x) (oeis @79)

instance OEIS 118955 where
  oeis = filter f [1..] where
     f x = any (== 1) $ map (oeisIx . (x -)) $ takeWhile (< x) (oeis @79)

instance OEIS 119416 where
  oeisIx n = n * (oeisIx $ (oeisIx @6530) n)

instance OEIS 120880 where
  oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (<= n) (oeis @5836)

instance OEIS 124665 where
  oeis = filter
     (\x -> all (== 0) $ map (oeisIx . (10*x +)) [1..9]) (oeis @65502)

instance OEIS 129363 where
  oeisIx n = sum $ map (oeisIx . (2*n -)) $ takeWhile (<= n) (oeis @1097)

instance OEIS 130897 where
  oeis = filter
     (any (== 0) . map (oeisIx . fi) . (rowT @124010)) [1..]

instance OEIS 132188 where
  oeisIx 0 = 0
  oeisIx n = (oeisIx @132345) n + (oeisIx $ fromInteger n)

instance OEIS 132345 where
  oeisIx n = sum $ zipWith (*)
     (tail (oeis @10)) (map ((div n) . (^ 2)) [2..oeisIx n])

instance OEIS 135141 where
  oeis = 1 : map f [2..] where
     f x | iprime == 0 = 2 * (oeisIx $ (oeisIx @66246) x) + 1
         | otherwise   = 2 * (oeisIx iprime)
         where iprime = (oeisIx @49084) x

instance OEIS 141197 where
  oeisIx = sum . map (oeisIx . (+ 1)) . (rowT @27750)

instance OEIS 147812 where
  oeis = map (oeisIx . (+ 1)) $ findIndices (< 0) (oeis @36263)

instance OEIS 147813 where
  oeis = map (oeisIx . (+ 1)) $ findIndices (>= 0) (oeis @36263)

instance OEIS 158294 where
  oeisIx n = (oeisIx $ (oeisIx @20486) n) `div` (oeisIx $ (oeisIx @20486) n)

instance OEIS 165430 where
  oeis = tablList @165430
instance Table 165430 where
  rowCol n k = last (oeisIx_row n `intersect` (rowT @77610) k)
  rowT n = map (oeisIx n) [1..n]
  tabl = map (rowT @165430) [1..]

instance OEIS 165634 where
  oeis = concatMap (\x ->
     if (oeisIx @10051 . pred) x == 1 then [oeisIx x, x] else [x]) [1..]

instance OEIS 179909 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 1 (oeis @79066)

instance OEIS 179910 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 2 (oeis @79066)

instance OEIS 179911 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 3 (oeis @79066)

instance OEIS 179912 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 4 (oeis @79066)

instance OEIS 179913 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 5 (oeis @79066)

instance OEIS 179914 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 6 (oeis @79066)

instance OEIS 179915 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 7 (oeis @79066)

instance OEIS 179916 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 8 (oeis @79066)

instance OEIS 179917 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 9 (oeis @79066)

instance OEIS 179918 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 10 (oeis @79066)

instance OEIS 179919 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 11 (oeis @79066)

instance OEIS 179922 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 12 (oeis @79066)

instance OEIS 179924 where
  oeis = map (oeisIx . (+ 1)) $ findIndices (> 0) (oeis @79066)

instance OEIS 180304 where
  oeisIx n = fi (oeisIx n) * n
  oeis = map sum $ group (oeis @6)

instance OEIS 180477 where
  oeis = filter (\x -> mod (x * (oeisIx @55642) x) (oeisIx x) == 0) [1..]

instance OEIS 183079 where
  oeis = tablList @183079
instance Table 183079 where
  rowCol = rowCol_off @183079 @1 @1
  tabf = [1] : iterate (\row -> concatMap f row) [2]
     where f x = [oeisIx x, (oeisIx @14132) x]
  oeis = concat (tabf @183079)

instance OEIS 185934 where
  oeis = map (oeisIx . (+ 1)) $
     elemIndices 1 $ zipWith (*) (oeis @39701) $ tail (oeis @39701)

instance OEIS 186102 where
  oeisIx n = f (oeis @40) where
     f (q:qs) = if (q - n) `mod` (oeisIx n) == 0 then q else f qs

instance OEIS 193314 where
  oeisIx n = head [k | k <- [1..], let kk' = (oeisIx @2378) k,
                        mod kk' (oeisIx n) == 0, (oeisIx @6530) kk' == (oeisIx @40) n]

instance OEIS 193315 where
  oeisIx 1 = 1
  oeisIx n = maximum $ zipWith (*) prims $ map (oeisIx . (2*n -)) prims
     where prims = takeWhile (<= n) (oeis @8578)

instance OEIS 196046 where
  oeis = 0 : g 2 where
    g x = y : g (x + 1) where
      y | t > 0     = max (oeisIx t) (oeisIx t + 1)
        | otherwise = maximum [oeisIx r, (oeisIx @196046) s, (oeisIx @1222) r + (oeisIx @1222) s]
        where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 196063 where
  oeis = 0 : 1 : g 3 where
     g x = y : g (x + 1) where
       y | t > 0     = (oeisIx @196063) t * (oeisIx t + 1) `div` (oeisIx @1222) t
         | otherwise = (oeisIx @196063) r * (oeisIx @196063) s * (oeisIx @1222) x `div`
                       (oeisIx r * (oeisIx @1222) s)
         where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 198328 where
  oeis = 1 : 1 : g 3 where
     g x = y : g (x + 1) where
       y = if t > 0 then (oeisIx @40) (oeisIx t) else (oeisIx @198328) r * (oeisIx @198328) s
           where t = (oeisIx @49084) x; r = (oeisIx @20639) x; s = x `div` r

instance OEIS 199045 where
  oeisIx n = head $
     filter ((<= 2) . (oeisIx @54055)) $ map (* 2^n) [oeisIx (n - 1)..]

instance OEIS 199745 where
  oeis = filter (\x -> 2 * (oeisIx x) == (oeisIx @8472) x) [1..]

instance OEIS 199921 where
  oeis = map length $ group $ sort $ map (oeisIx . (oeisIx @61493)) [1..3999]

instance OEIS 201009 where
  oeisIx = (oeis @201009)
  oeis = 1 : filter
     (\x -> (rowT @27748) x == (rowT @27748) (oeisIx x)) [2..]

instance OEIS 201651 where
  oeisIx :: Integer -> Integer -> Integer
  oeisIx n 0 = n
  oeisIx n k = (oeisIx @54240) (n `xor` k) (shift (n .&. k) 2)
  oeisIx_row n = map (oeisIx n) [0..n]
  oeisIx_tabl = map (rowT @201651) [0..]

instance OEIS 205565 where
  oeisIx n = sum $ map (oeisIx . (n -)) $
                    takeWhile (<= n `div` 2) (oeis @5836)

instance OEIS 206462 where
  oeis = map (oeisIx . (+ 1)) $
                     elemIndices 1 $ map (oeisIx @8966) (oeis @1043)

instance OEIS 207432 where
  oeisIx n = (fromJust $ elemIndex (oeisIx n) (oeis @66680)) + 1

instance OEIS 209061 where
  oeis = filter
     (all (== 1) . map (oeisIx . fi) . (rowT @124010)) [1..]

instance OEIS 210494 where
  oeis = filter
     (\x -> (oeisIx x + (oeisIx @38040) x) `mod` (oeisIx @74400) x == 0) [1..]

instance OEIS 210582 where
  oeis = filter (\x -> mod x (oeisIx x) == (oeisIx @30) x) (oeis @67251)

instance OEIS 214282 where
  oeisIx n = (oeisIx @7318) (n - 1) (oeisIx (n - 1))

instance OEIS 214283 where
  oeisIx 1 = 0
  oeisIx n = - (oeisIx @7318) (n - 1) (oeisIx n)

instance OEIS 216155 where
  oeis = filter
     (\x -> (oeisIx @196) (oeisIx x) == (oeisIx @196) (oeisIx x) + 1) [1..]

instance OEIS 226227 where
  oeis = filter (all (== 1) .
                 map (oeisIx . genericLength) .
                     other . tail . reverse . group . (rowT @30308)) [1..]
     where other [] = []; other [x] = [x]; other (x:_:xs) = x : other xs

instance OEIS 226228 where
  oeis = filter (all (== 1) .
                 map (oeisIx . genericLength) .
                     other . reverse . group . (rowT @30308)) [1..]
     where other [] = []; other [x] = [x]; other (x:_:xs) = x : other xs

instance OEIS 226229 where
  oeis = filter
     (all (== 1) . map (oeisIx . genericLength) . group . (rowT @30308)) [1..]

instance OEIS 229139 where
  oeisIx 1 = 0
  oeisIx n = head $
     dropWhile (== 0) $ map (oeisIx . (t -) . (^ 2)) [s, s - 1 ..]
     where t = (oeisIx @45) (2 * n - 1); s = (oeisIx @196) t

instance OEIS 232501 where
  oeis = filter f [1..] where
     f x = all ((== 1) . (oeisIx @10054)) $ init $ sort $
           map (abs . (x -) . (^ 2) . (+ (oeisIx x))) [-1..2]

instance OEIS 232608 where
  oeis = filter f $ tail (oeis @217) where
     f x = all ((== 1) . (oeisIx @10054)) $ init $ sort $
           map (abs . (x -) . (^ 2) . (+ (oeisIx x))) [-1..2]

instance OEIS 235353 where
  oeis = filter (\x -> mod x (oeisIx x) == 0) (oeis @7694)

instance OEIS 237126 where
  oeis = 0 : es where
     es = 1 : concat (transpose [map (oeisIx @192607) es, map (oeisIx . (+ 1)) es])

instance OEIS 239969 where
  oeisIx n = head [k | k <- [1..],
                        (oeisIx @10054) (oeisIx n + (oeisIx @217) (n + k)) == 1]

instance OEIS 239970 where
  oeisIx n = head [k | k <- [1..],
                        (oeisIx @10054) (oeisIx k + (oeisIx @217) (n + k)) == 1]

instance OEIS 242342 where
  oeisIx n = if n <= 2 then 0 else (oeisIx @7318)' n (oeisIx n)

instance OEIS 245193 where
  oeisIx n = head [p | p <- (oeis @40),
                        (isSuffixOf `on` show) (oeisIx n) p]

instance OEIS 245396 where
  oeisIx n = (oeisIx @244365) n (oeisIx n)

instance OEIS 246588 where
  oeisIx = product . map (oeisIx . length) .
            filter ((== 1) . head) . group . (rowT @30308)

instance OEIS 247628 where
  oeis = filter f (oeis @216155) where
     f x = any ((== 1) . (oeisIx @10057)) [oeisIx x .. (oeisIx @2378) x - 1]

instance OEIS 247714 where
  oeisIx = (+ 1) . fromJust .
                    (`elemIndex` (oeis @3586)) . (oeis !!)

instance OEIS 247869 where
  oeisIx n = (oeisIx (oeisIx n) + (oeisIx @40) n) `div` (oeisIx n + n)

instance OEIS 252596 where
  oeis = iterate (\x -> (oeisIx @40) (oeisIx x + mod x 3)) 5

instance OEIS 253569 where
  oeis = filter f [1..] where
                      f x = (p ^ 2 < (oeisIx @20639) q) && (oeisIx' q == 1 || f q)
                            where q = div x p; p = (oeisIx @20639) x
  oeisIx n = (oeis @253569) !! (n - 1)
  oeis = filter (not . f''') (oeis @2808) where
     f''' x = p ^ 2 > (oeisIx @20639) q || (oeisIx q == 0 && f''' q)
              where q = div x p; p = (oeisIx @20639) x

instance OEIS 256751 where
  oeisIx n = (+ 1) $ fromJust $
              (oeisIx @166133) n `elemIndex` (rowT @27750)' (oeisIx (n - 1) ^ 2 - 1)

instance OEIS 256852 where
  oeis = f (oeis @40) [] $ tail (oeis @583) where
     f ps'@ (p:ps) us vs'@ (v:vs)
       | p > v     = f ps' (v:us) vs
       | otherwise = (sum $ map (oeisIx . (p -)) us) : f ps us vs'

instance OEIS 258193 where
  oeisIx n = head $ (filter ((== 1) . (oeisIx @10051 . pred)) $
                      scanl1 (<+>) (oeisIx_row n)) ++ [0]
              where (<+>) = (oeisIx .) . on (+) (oeisIx @265)

instance OEIS 262675 where
  oeis = filter
     (all (== 1) . map (oeisIx . fi) . (rowT @124010)) [1..]

instance OEIS 263774 where
  oeisIx 1 = 1
  oeisIx n = foldl (-) (oeisIx n) $ zipWith (^) (map a' $ reverse ds) ds
              where a' x = if x == n then 0 else (oeisIx @263774) x
                    ds = (rowT @27750) n

instance OEIS 240162 where
  import Math.NumberTheory.Moduli (powerMod)
  oeisIx n = powerMod 3 (oeisIx $ (oeisIx @10) n) n

instance OEIS 48889 where
  import Numeric (readInt)
  oeisIx n = (oeis @48889) !! (n - 1)
  oeis = filter f (oeis @2808) where
     f n = n `mod` 10 > 0 &&
           null ("23547" `intersect` show n)  &&
           (oeisIx (fst $ head $ readInt 10 (const True) ud $ ns) == 1)
         where ns = reverse $ show n
               ud '6' = 9
               ud '9' = 6
               ud z = digitToInt z

instance OEIS 3325 where
  oeis = filter c2 [1..] where
     c2 x = any (== 1) $ map (oeisIx . fromInteger) $
                         takeWhile (> 0) $ map (x -) $ tail (oeis @578)

instance OEIS 3459 where
  oeis = filter isAbsPrime (oeis @40) where
     isAbsPrime = all (== 1) . map (oeisIx . read) . permutations . show

instance OEIS 5845 where
  oeis = filter (\x -> (oeisIx x - 1) `mod` x == 0) (oeis @2808)

instance OEIS 6022 where
  oeisIx 1 = 0
  oeisIx n = (+ 1) $ sum $ takeWhile (> 1) $
            iterate (\x -> x `div` (oeisIx @20639) x) (oeisIx n)

instance OEIS 6087 where
  oeis = map numerator $ filter ((== 1) . denominator) $
     map uhm [1..]  where uhm n = (n * (oeisIx @34444) n) % (oeisIx n)

instance OEIS 6343 where
  oeisIx 0 = 1
  oeisIx n = sum $ zipWith div
     (zipWith (*) (map (oeisIx n) ks)
                  (map (\k -> (oeisIx @7318) (2*n - 3*k - 4) (n - 2*k - 2)) ks))
     (map ((n - 1 -)) ks)
     where ks = [0 .. (n - 2) `div` 2]

instance OEIS 6711 where
  oeis = iterate (oeisIx . (oeisIx @4086)) 1

instance OEIS 7664 where
  oeisIx = sum . map (oeisIx . (oeisIx @3056)) . enumFromTo 0 . subtract 1

instance OEIS 11773 where
  oeisIx n = foldl lcm 1 $ map (oeisIx . (oeisIx @95874)) $
                            zipWith (^) (oeisIx_row n) (oeisIx_row n)

instance OEIS 19269 where
  oeisIx n = snd $ until ((== 1) . (oeisIx @65333) . fst)
                          (\ (x, i) -> (oeisIx x, i+1)) (n, 0)

instance OEIS 20893 where
  oeis = filter (\x -> any (== 1) $ map (oeisIx . (x -)) $
                               takeWhile (<= x) (oeis @290)) (oeis @5117)

instance OEIS 24770 where
  oeis = filter (\x ->
     all (== 1) $ map (oeisIx . read) $ tail $ inits $ show x) (oeis @38618)

instance OEIS 24785 where
  oeis = filter (\x ->
     all (== 1) $ map (oeisIx . read) $ init $ tails $ show x) (oeis @38618)

instance OEIS 25530 where
  oeisIx n = sum $ map (div (oeisIx $ fromInteger n))
                        (zipWith (*) [1..n] (oeis @33999))

instance OEIS 25583 where
  oeis = filter f (oeis @2808) where
     f x = all (== 0) $ map (oeisIx . (x -)) $ takeWhile (< x) (oeis @40)

instance OEIS 33274 where
  oeis = map (oeisIx . (+ 1)) $ elemIndices 0 (oeis @79066)

instance OEIS 34302 where
  oeis = filter f $ drop 4 (oeis @38618) where
     f x = all (== 1) $ map (oeisIx . read) $
               zipWith (++) (inits $ show x) (tail $ tails $ show x)

instance OEIS 34303 where
  oeis = filter f $ drop 4 (oeis @38618) where
     f x = all (== 0) $ map (oeisIx . read) $
               zipWith (++) (inits $ show x) (tail $ tails $ show x)

instance OEIS 36916 where
  oeisIx n = sum $ map
     (\k -> (oeisIx (2*n - 2*k) (n - k))^2 * (oeisIx @7318 n k)^2) [0..n]

instance OEIS 36917 where
  oeisIx n = sum $ map
     (\k -> (oeisIx (2*n - 2*k) (n - k))^2 * (oeisIx @7318 (2*k) k)^2) [0..n]

instance OEIS 37445 where
  oeisIx = product . map (oeisIx . (oeisIx @120)) . (rowT @124010)

instance OEIS 38199 where
  oeisIx n = sum [oeisIx (n `div` d) * (oeisIx d)| d <- (rowT @27750) n]

instance OEIS 39638 where
  oeisIx 0 = 1
  oeisIx (succ->n) = until ((== 1) . (oeisIx @10051.pred)) (flip div 2) (oeisIx n - 1)

instance OEIS 39640 where
  oeisIx 0 = 1
  oeisIx (succ->n) = until ((== 1) . (oeisIx @10051.pred)) (flip div 2 . (+ 1)) (oeisIx n - 1)

instance OEIS 39642 where
  oeisIx 0 = 1
  oeisIx (succ->n) = snd $ until ((== 1) . (oeisIx @10051.pred) . fst)
                    (\ (x, i) -> (x `div` 2 , i + 1)) (oeisIx n - 1, 1)

instance OEIS 39643 where
  oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
                    (\ (x, i) -> (x `div` 2 , i + 1)) (oeisIx n + 1, 1)

instance OEIS 39644 where
  oeisIx 1 = 1
  oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
              (\ (x, i) -> ((x + 1) `div` 2 , i + 1)) (oeisIx n - 1, 1)

instance OEIS 39645 where
  oeisIx n = snd $ until ((== 1) . (oeisIx @10051) . fst)
              (\ (x, i) -> ((x + 1) `div` 2 , i + 1)) (oeisIx n + 1, 1)

instance OEIS 45917 where
  oeisIx n = sum $ map (oeisIx . (2 * n -)) $ takeWhile (<= n) (oeis @40)

instance OEIS 45974 where
  oeisIx n = g n n where
     g x y = product [oeisIx (oeisIx pi + (oeisIx @49084) pj) ^ (ei * ej) |
                      (pi,ei) <- zip (oeisIx_row x) (oeisIx_row x),
                      (pj,ej) <- zip (oeisIx_row y) (oeisIx_row y)]

instance OEIS 46922 where
  oeisIx n = sum $ map (oeisIx . (n -)) $ takeWhile (< n) (oeis @1105)

instance OEIS 48761 where
  oeisIx n = (oeisIx n + 1 - (oeisIx @10052) n) ^ 2
  oeis = 0 : concat (f 1 1) where
     f u v = (take v $ repeat u) : f (u + v + 2) (v + 2)

instance OEIS 49419 where
  oeisIx = product . map (oeisIx . fi) . (rowT @124010)

instance OEIS 50328 where
  oeis = f 1 where
     f x = (if x == 1 then 1 else
           sum $ map (oeisIx . (div x)) $ tail $ (rowT @206778) x) : f (x + 1)

instance OEIS 51362 where
  oeis = filter p $ drop 4 (oeis @40) where
     p x = all (== 1) $ map (oeisIx . read) $
               zipWith (++) (inits $ show x) (tail $ tails $ show x)

instance OEIS 54584 where
  oeisIx n = (oeisIx @5) n + 3 * (oeisIx @79978) n * (oeisIx @5) (oeisIx n) + (oeisIx @35191) n


instance OEIS 3557 where
  oeisIx n = product $ zipWith (^)
                        (oeisIx_row n) (map (subtract 1) $ (rowT @124010) n)

instance OEIS 3963 where
- oeisIx n = product $
-    zipWith (^) (map (oeisIx @49084) $ (rowT @27748) n) (oeisIx_row n)

instance OEIS 6753 where
  oeis = [x | x <- (oeis @2808),
                      oeisIx x == sum (map (oeisIx @7953) (oeisIx_row x))]

instance OEIS 8474 where
  oeisIx n = sum $ zipWith (+) (oeisIx_row n) (oeisIx_row n)
instance OEIS 8784 where
  oeis = 1 : 2 : O.union (oeis @4613) (map (* 2) (oeis @4613))

instance OEIS 11262 where
  oeisIx n = product $ zipWith (^)
                        (oeisIx_row n) (map (oeisIx @103889) $ (rowT @124010) n)

instance OEIS 11264 where
  oeisIx n = product $ zipWith (^)
                        (oeisIx_row n) (map (oeisIx @4442) $ (rowT @124010) n)

instance OEIS 19506 where
  oeis = [x | x <- (oeis @2808),
                      oeisIx x == sum (map (oeisIx @7953) (oeisIx_row x))]

instance OEIS 19554 where
  oeisIx n = product $ zipWith (^)
              (oeisIx_row n) (map ((`div` 2) . (+ 1)) $ (rowT @124010) n)

instance OEIS 19565 where
  oeisIx n = product $ zipWith (^) (oeis @40) (oeisIx_row n)

instance OEIS 29885 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) (1 : map fi (oeis @1285))

instance OEIS 36288 where
  oeisIx n = 1 + sum (zipWith (*)
              (oeisIx_row n) (map fi $ (rowT @124010) n))

instance OEIS 45966 where
  oeisIx 1 = 3
  oeisIx n = product $ zipWith (^)
              (map (oeisIx @101300) $ (rowT @27748) n) (oeisIx_row n)

instance OEIS 48103 where
  oeis = filter (\x -> and $
     zipWith (>) (oeisIx_row x) (map toInteger $ (rowT @124010) x)) [1..]

instance OEIS 49417 where
  oeisIx 1 = 1
  oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
     f p e = product $ zipWith div
             (map (subtract 1 . (p ^)) $
                  zipWith (*) (oeis @79) $ map (+ 1) $ (rowT @30308) e)
             (map (subtract 1 . (p ^)) (oeis @79))

instance OEIS 50252 where
  oeisIx 1 = 1
  oeisIx n = sum $ map (oeisIx @55642) $
              (oeisIx_row n) ++ (filter (> 1) $ (rowT @124010) n)

instance OEIS 51377 where
  oeisIx n = product $ zipWith sum_e (oeisIx_row n) (oeisIx_row n) where
     sum_e p e = sum [p ^ d | d <- (rowT @27750) e]

instance OEIS 51378 where
  oeisIx n = product $ zipWith sum_1e (oeisIx_row n) (oeisIx_row n)
     where sum_1e p e = 1 + sum [p ^ d | d <- (rowT @27750) e]

instance OEIS 52410 where
  oeisIx n = product $ zipWith (^)
                        (oeisIx_row n) (map (`div` (foldl1 gcd es)) es)
              where es = (rowT @124010) n

instance OEIS 54744 where
  oeis = filter (\x -> and $
     zipWith (<=) (oeisIx_row x) (map fi $ (rowT @124010) x)) [1..]

instance OEIS 56239 where
  oeisIx n = sum $ zipWith (*) (map (oeisIx @49084) $ (rowT @27748) n) (oeisIx_row n)

instance OEIS 57562 where
  oeisIx n = p (oeisIx_row n) n where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 58035 where
  oeisIx n = product $
     zipWith (^) (oeisIx_row n) (map (min 3) $ (rowT @124010) n)

instance OEIS 62537 where
  oeisIx 1 = 0
  oeisIx n = sum $ map (+ 1) $
     zipWith ((+) `on` (oeisIx @62537)) (map (oeisIx @49084) $ (rowT @27748) n) (oeisIx_row n)

instance OEIS 66990 where
  oeisIx n = product $ zipWith (^)
             (oeisIx_row n) (map ((2 -) . (`mod` 2)) $ (rowT @124010) n)

instance OEIS 69799 where
  oeisIx n = product $ zipWith (^) (oeisIx_row n) (reverse $ (rowT @124010) n)

instance OEIS 69915 where
  oeisIx n = product $ zipWith sum_1phi (oeisIx_row n) (oeisIx_row n)
     where sum_1phi p e = 1 + sum [p ^ k | k <- (rowT @38566) e]

instance OEIS 70965 where
  oeis = 1 : f 1 where
     f x = y : f (x + 1) where
       y = sum $ zipWith (*) (map (oeisIx @70965) $ (rowT @27750) x) (oeisIx_row x)

instance OEIS 71974 where
  oeisIx n = product $ zipWith (^) (oeisIx_row n) $
     map (\e -> (1 - e `mod` 2) * e `div` 2) $ (rowT @124010) n

instance OEIS 71975 where
  oeisIx n = product $ zipWith (^) (oeisIx_row n) $
     map (\e -> (e `mod` 2) * (e + 1) `div` 2) $ (rowT @124010) n

instance OEIS 72965 where
  oeisIx n = f 1 (oeisIx_row n) where
     f y []      = y
     f y [p]     = p * y
     f y (2:ps)  = f (2 * y) ps
     f y (3:5:_) = (oeisIx @72965) (n `div` 15)
     f y (p:qs@ (q:ps)) | q == p + 2 = f y ps
                       | otherwise  = f (p * y) qs

instance OEIS 80079 where
  oeisIx n = (length $ takeWhile (< (oeisIx @70940) n) (oeisIx_row n)) + 1

instance OEIS 85079 where
  oeisIx n = product $ zipWith (^) (oeisIx_row n) (sort $ (rowT @124010) n)

instance OEIS 89247 where
  oeisIx n = product $ zipWith (^)
                        (oeisIx_row n) (reverse $ sort $ (rowT @124010) n)

instance OEIS 100716 where
  oeis = filter (\x -> or $
     zipWith (<=) (oeisIx_row x) (map toInteger $ (rowT @124010) x)) [1..]

instance OEIS 167772 where
  oeis = tablList @167772
instance Table 167772 where
  rowCol n k = genericIndex (oeisIx_row n) k
  rowT n = genericIndex (tabl @167772) n
  tabl = [1] : [0, 1] :
                 map (\xs@ (_:x:_) -> x : xs) (tail (tabl @65602))

instance OEIS 167831 where
  oeisIx n = head [x | let ds = (rowT @31298) n, x <- [n, n - 1 ..],
                        all (< 10) $ zipWith (+) ds (oeisIx_row x)]

instance OEIS 167877 where
  oeisIx n = head [x | let ts = (rowT @30341) n, x <- [n, n - 1 ..],
                        all (< 3) $ zipWith (+) ts (oeisIx_row x)]

instance OEIS 182183 where
  oeis = f (oeis @209933) [1] where
     f (x:xs) ys =
       if null (oeisIx_row x \\ ys) then x : f xs (x : ys) else f xs ys

instance OEIS 182938 where
  oeisIx n = product $ zipWith (oeisIx @7318)'
     (oeisIx_row n) (map toInteger $ (rowT @124010) n)

instance OEIS 185359 where
  oeis = [x | x <- [1..], or $ zipWith (<)
                      (oeisIx_row x) (map toInteger $ (rowT @124010) x)]

instance OEIS 192719 where
  oeis = tablList @192719
instance Table 192719 where
  rowCol = rowCol_off @192719 @1 @1
  rowT   = rowT_off @192719 @1
  tabf = f [1..] where
     f (x:xs) = (oeisIx_row x) : f (del xs $ (rowT @220237) x)
     del us [] = us
     del us'@ (u:us) vs'@ (v:vs) | u > v     = del us' vs
                               | u < v     = u : del us vs'
                               | otherwise = del us vs

instance OEIS 197863 where
  oeisIx n = product $
     zipWith (^) (oeisIx_row n) (map (max 2) $ (rowT @124010) n)

instance OEIS 202387 where
  oeis = [x | x <- (oeis @120944),
                      oeisIx x == sum (map (oeisIx @7953) (oeisIx_row x))]

instance OEIS 206369 where
  oeisIx n = product $
     zipWith h (oeisIx_row n) (map toInteger $ (rowT @124010) n) where
             h p e = sum $ take (fromInteger e + 1) $
                           iterate ((* p) . negate) (1 - 2 * (e `mod` 2))

instance OEIS 206497 where
  oeis = 1 : g 2 where
    g x = y : g (x + 1) where
      y | t > 0     = (oeisIx @206497) t
        | otherwise = product $ zipWith (\p e -> (oeisIx @142) e * (oeisIx @206497) p ^ e)
                                        (oeisIx_row x) (oeisIx_row x)
        where t = (oeisIx @49084) x

instance OEIS 207481 where
  oeis = [x | x <- [1..], and $ zipWith (<=)
                      (map toInteger $ (rowT @124010) x) (oeisIx_row x) ]

instance OEIS 210442 where
  oeisIx n = p (oeisIx_row n) n where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 211889 where
  oeisIx n = head [k | let p = (oeisIx @40) n, k <- [1..],
              all ((== 1) . (oeisIx @10051 . pred)) $ map ((+ p) . (* k)) (oeisIx_row n)]

instance OEIS 220432 where
  oeis = filter (\x -> null $
     intersect (oeisIx_row x) (takeWhile (<= x) (oeis @219908))) (oeis @7310)

instance OEIS 225244 where
  oeisIx n = p (oeisIx_row n) n where
     p _          0 = 1
     p []         _ = 0
     p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

instance OEIS 225860 where
  oeisIx n = p (oeisIx_row n) (2 ^ n) where
     p _          0 = 1
     p []         _ = 0
     p bs'@ (b:bs) m = if m < b then 0 else p bs' (m - b) + p bs m

instance OEIS 230950 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @10060)

instance OEIS 230951 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @10059)

instance OEIS 230952 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @120)

instance OEIS 230953 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ tail (oeis @40)

instance OEIS 230954 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @2808)

instance OEIS 230955 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @18252)

instance OEIS 230957 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @9)

instance OEIS 230958 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ map fi (oeis @1285)

instance OEIS 230960 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) (oeis @142)

instance OEIS 230961 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ tail (oeis @142)

instance OEIS 261461 where
  oeisIx x = f $ tail (tabf @30308) where
     f (cs:css) = if isInfixOf cs (oeisIx_row x)
                     then f css else foldr (\d v -> 2 * v + d) 0 cs

instance OEIS 261787 where
  oeisIx x = f $ tail (tabf @30341) where
     f (cs:css) = if isInfixOf cs (oeisIx_row x)
                     then f css else foldr (\d v -> 3 * v + d) 0 cs

instance OEIS 261794 where
  oeisIx x = f $ tail (tabf @31298) where
     f (cs:css) = if isInfixOf cs (oeisIx_row x)
                     then f css else foldr (\d v -> 10 * v + d) 0 cs

instance OEIS 261922 where
  oeisIx x = f (tabf @30308) where
     f (cs:css) = if isInfixOf cs (oeisIx_row x)
                     then f css else foldr (\d v -> 2 * v + d) 0 cs

instance OEIS 256232 where
  oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n)
     where f 2 e = e - 1; f 3 e = 1; f _ e = e + 1

instance OEIS 269526 where
  oeisIx n = head $ [1..] \\ map (oeisIx @269526) (oeisIx_row n)

instance OEIS 279968 where
  oeis = map count [1..] where
    count n = genericLength $ filter (odd . (n+)) adjacentLabels where
      adjacentLabels = map (oeisIx @279968) (oeisIx_row n)

instance OEIS 8477 where
  oeisIx n = product $ zipWith (^) (oeisIx_row n) (oeisIx_row n)

instance OEIS 7623 where
  oeisIx n | n <= 36287999 = read $ concatMap show (oeisIx_row n) :: Int
           | otherwise     = error "representation would be ambiguous"

instance OEIS 8473 where
  oeisIx n = product $ zipWith (+) (oeisIx_row n) (oeisIx_row n)

instance OEIS 30057 where
  oeisIx n = head $ filter ((== 0) . p (oeisIx_row n)) [1..] where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) x = if x < k then 0 else p ks (x - k) + p ks x

instance OEIS 33630 where
  oeisIx 0 = 1
  oeisIx n = p (oeisIx_row n) n where
     p _  0 = 1
     p [] _ = 0
     p (d:ds) m = if d > m then 0 else p ds (m - d) + p ds m

instance OEIS 35316 where
  oeisIx n = product $
     zipWith (\p e -> (p ^ (e + 2 - mod e 2) - 1) `div` (p ^ 2 - 1))
             (oeisIx_row n) (oeisIx_row n)

instance OEIS 46028 where
  oeis = f 1 where
     f x | null zs   = f (x + 1)
         | otherwise = (fst $ head zs) : f (x + 1)
         where zs = reverse $ filter ((> 1) . snd) $
                    zip (oeisIx_row x) (oeisIx_row x)

instance OEIS 49345 where
  oeisIx n | n < 2100  = read $ concatMap show (oeisIx_row n) :: Int
           | otherwise = error "ambiguous primorial representation"

instance OEIS 55229 where
  oeisIx n = product $ zipWith (^) ps (map (flip mod 2) es) where
     (ps, es) = unzip $
                filter ((> 1) . snd) $ zip (oeisIx_row n) (oeisIx_row n)

instance OEIS 58026 where
  oeisIx n = product $ zipWith (\p e -> p ^ (e - 1) * (p - 2))
                                (oeisIx_row n) (oeisIx_row n)

instance OEIS 60652 where
  oeis = filter h [1..] where
     h x = any (> 2) (map snd pfs) || any (== 1) pks where
       pks = [p ^ k `mod` q | (p,e) <- pfs, q <- map fst pfs, k <- [1..e]]
       pfs = zip (oeisIx_row x) (oeisIx_row x)

instance OEIS 62161 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ cycle [0,1]

instance OEIS 62272 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ cycle [1,0]

instance OEIS 62327 where
  oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
     f 2 e                  = 2 * e + 1
     f p e | p `mod` 4 == 1 = (e + 1) ^ 2
           | otherwise      = e + 1

instance OEIS 65205 where
  oeisIx n = p (oeisIx_row n) n where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 67599 where
  oeisIx n = read $ foldl1 (++) $
     zipWith ((++) `on` show) (oeisIx_row n) (oeisIx_row n) :: Integer

instance OEIS 80367 where
  oeisIx n = if null us then 0 else fst $ last us
    where us = filter ((== 1) . snd) $ zip (oeisIx_row n) (oeisIx_row n)

instance OEIS 80368 where
  oeisIx n = if null us then 0 else fst $ head us
    where us = filter ((== 1) . snd) $ zip (oeisIx_row n) (oeisIx_row n)

instance OEIS 80670 where
  oeisIx 1 = 1
  oeisIx n = read $ foldl1 (++) $
  zipWith (c `on` show) (oeisIx_row n) (oeisIx_row n) :: Integer
  where c ps es = if es == "1" then ps else ps ++ es

instance OEIS 99751 where
  oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n)
     where f 2 e = e - 1; f 3 e = 1; f _ e = e + 1

instance OEIS 101035 where
  oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
     f p 1 = 1 - 2 * p
     f p e = (p - 1) ^ 2

instance OEIS 143520 where
  oeisIx n = product $ zipWith (\p e -> (e + 2 * mod p 2 - 1) * p ^ e)
                                (oeisIx_row n) (oeisIx_row n)

instance OEIS 178212 where
  oeis = filter f [1..] where
     f x = length (oeisIx_row x) == 3 && any (> 1) (oeisIx_row x)

instance OEIS 188999 where
  oeisIx n = product $ zipWith f (oeisIx_row n) (oeisIx_row n) where
     f p e = (p ^ (e + 1) - 1) `div` (p - 1) - (1 - m) * p ^ e' where
             (e', m) = divMod e 2

instance OEIS 210455 where
  oeisIx n = fromEnum $ p (oeisIx_row n) n where
     p ks m = m == 0 || not (null ks) && head ks <= m &&
              (p (tail ks) (m - head ks) || p (tail ks) m)

instance OEIS 225245 where
  oeisIx n = p (oeisIx_row n) n where
     p _      0 = 1
     p []     _ = 0
     p (k:ks) m = if m < k then 0 else p ks (m - k) + p ks m

instance OEIS 226555 where
  oeisIx n = numerator $ sum $
              zipWith ((%) `on` toInteger) (oeisIx_row n) (oeisIx_row n)

instance OEIS 228058 where
  oeis = filter f [1, 3 ..] where
     f x = length us == 1 && not (null vs) &&
           fst (head us) `mod` 4 == 1 && snd (head us) `mod` 4 == 1
           where (us,vs) = partition (odd . snd) $
                           zip (oeisIx_row x) (oeisIx_row x)

instance OEIS 231179 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) [0..]

instance OEIS 231200 where
  oeisIx n = sum $ zipWith (*) (oeisIx_row n) $ [0, 2 ..]

instance OEIS 236563 where
  oeisIx n = foldl lcm 1 $ zipWith (\p e -> p ^ (e + 1) * (p - 1))
                                    (oeisIx_row n) (oeisIx_row n)

instance OEIS 241816 where
  oeisIx n = f (oeisIx_row n) [] where
     f [] _ = n
     f (0 : 1 : us) vs = foldr (\b y -> 2 * y + b) 0 $
                               reverse vs ++ 1 : 0 : us
     f (u : us) vs     = f us (u : vs)

instance OEIS 187099 where
  oeisIx = (+ 1) . fromJust . (`elemIndex` oeis) . fi

instance OEIS 181363 where
  oeis = concat $ transpose [oeis, (oeis @181363)]


instance OEIS 110766 where
  oeis = concat $ transpose [oeis, (oeis @110766)]

instance OEIS 106435 where
  oeis = 0 : 3 : map (* 3) (zipWith (+) (oeis @106435) (tail oeis))

instance OEIS 97557 where
  oeis = 1 : f 1 0 where
     f x z = y : f y z' where
       y = x + z'; z' = z + 1 - fi (oeisIx' x)

instance OEIS 33485 where
  oeis = 1 : zipWith (+)
     (oeis @33485) (concat $ transpose [oeis, (oeis @33485)])

instance OEIS 26150 where
  oeis = 1 : 1 : map (* 2) (zipWith (+) (oeis @26150) (tail oeis))

instance OEIS 234586 where
  oeis = concat (transpose [oeis, [2, 4 ..]])
  oeis = 1 : 1 : (drop 2 $
                 map abs $ zipWith (-) (oeis @234586) $ tail (oeis @234586))

instance OEIS 192734 where
  oeisIx n = head [x | x <- [2^u + 2^v + 1 | u <- [2..], v <- [1..u-1]],
                        oeisIx x == n]

instance OEIS 63759 where
  oeis = concat $ transpose [oeis, (oeis @7283)]

instance OEIS 6872 where
  oeis = filter (\x -> (oeisIx @10) x == (oeisIx @10) (oeisIx' x)) [1..]

instance OEIS 7428 where
  oeisIx n = product
     [oeisIx' 3 e * cycle [1,-1] !! fi e | e <- (rowT @124010) n]

instance OEIS 27383 where
  oeis = concat $ transpose [oeis, drop 2 (oeis @918)]

instance OEIS 38754 where
  oeis = concat $ transpose [oeis, (oeis @8776)]

instance OEIS 45910 where
  oeis =  [x | x <- takeWhile (<= 999999999) $ (oeis @9994),
                       oeisIx x == 1]

instance OEIS 193428 where
  oeisIx n = sum $ map ($ n) [oeisIx, (oeisIx @31288), (oeisIx @31289), (oeisIx @31290), (oeisIx @31291), (oeisIx @31292), (oeisIx @31293), (oeisIx @31294), (oeisIx @31295), (oeisIx @31296)]

-}
