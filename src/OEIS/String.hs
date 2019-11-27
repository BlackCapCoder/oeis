module OEIS.String where


-- instance OEIS 78822 where
--   import Numeric (showIntAtBase)
--   oeisIx = genericLength . (rowT @119709)

-- instance OEIS 78834 where
--   import Numeric (showIntAtBase)
--   oeisIx n = fromMaybe 1 $ find (\p -> showIntAtBase 2 ("01" !!) p ""
--                             `isInfixOf` showIntAtBase 2 ("01" !!) n "") $
--                    reverse $ (rowT @27748) n

instance OEIS 249591 where
  oeis = 1 : f 1 [2..] where
     f x zs = g zs where
       g (y:ys) = if null $ show y `intersect` show (x - 1)
                     then g ys else y : f y (delete y zs)

instance OEIS 262530 where
  oeis = filter f [1..] where
     f = g d10' . show where
       g _ [] = True
       g ts (d:ds) = elem d ts && g (delete d ts) ds
     d10' = d10 ++ d10; d10 = "0123456789"

instance OEIS 309415 where
  oeisIx n = genericLength $ nub $ permutations $ show $ fact n
    map (oeisIx @309415) [0..]

instance OEIS 190016 where
  oeisIx n = (oeis @190016) !! (n - 1)
  oeis = sortBy (comparing show) [1..10000]

-- instance OEIS 190132 where
--   import Numeric (showIntAtBase)
--   oeisIx n = (oeis @190132) !! (n - 1)
--   oeis =
--      sortBy (comparing (flip (showIntAtBase 12 intToDigit) "")) [1..10000]

-- instance OEIS 190134 where
--   import Numeric (showHex)
--   oeisIx n = (oeis @190134) !! (n - 1)
--   oeis = sortBy (comparing (flip showHex "")) [1..10000]

instance OEIS 796 where
  oeisIx len = map (fi . digitToInt) $ show . fi $ machin' `div` (10 ^ 10) where
     machin' = 4 * (4 * arccot 5 unity - arccot 239 unity)
     unity = 10 ^ (len + 10)
     arccot x unity = arccot' x unity 0 (unity `div` x) 1 1 where
       arccot' x unity summa xpow n sign
        | term == 0 = summa
        | otherwise = arccot'
          x unity (summa + sign * term) (xpow `div` x ^ 2) (n + 2) (- sign)
        where term = xpow `div` n

instance OEIS 7629 where
  oeis = filter isKeith [10..] where
     isKeith n = repfigit $ reverse $ map digitToInt $ show n where
        repfigit ns = s == n || s < n && (repfigit $ s : init ns) where
           s = sum ns

instance OEIS 38769 where
  oeisIx n = genericLength $ filter (== 0)
              $ map ((mod n) . digitToInt) $ filter (> '0') $ show n

instance OEIS 38772 where
  oeis = filter p [1..] where
     p n = all (> 0) $ map ((mod n) . digitToInt) $ filter (> '0') $ show n

instance OEIS 45537 where
  oeisIx n = 2 + length
     (takeWhile (not . ((show n) `isInfixOf`) . show) $ iterate (* n) (n^2))

instance OEIS 51248 where
  oeisIx n = 2 + length
     (takeWhile (not . (show n `isPrefixOf`) . show) $ iterate (* n) (n^2))

instance OEIS 52191 where
  oeisIx n = head $
     filter ((> 1) . minimum . map length . group . show) $ [0,n..]

instance OEIS 52192 where
  oeisIx n = fromJust $
     findIndex ((> 1) . minimum . map length . group . show) $ [0,n..]

-- instance OEIS 59708 where
--   oeis = filter sameParity [0..] where
--      sameParity n = all (`elem` "02468") ns
--                  || all (`elem` "13579") ns where ns = show n

instance OEIS 67581 where
  oeis = 1 : f 1 [2..] where
     f u vs = v : f v (delete v vs)
       where v : _ = filter (null . (intersect `on` show) u) vs

instance OEIS 116700 where
  oeis = filter early [1 ..] where
     early z = not (reverse (show (z - 1)) `isPrefixOf` fst bird) where
        bird = fromJust $ find ((show z `isPrefixOf`) . snd) xys
     xys = iterate (\ (us, v : vs) -> (v : us, vs))
                   ([], concatMap show [0 ..])

instance OEIS 121041 where
  oeisIx n = genericLength $ filter (\d -> n `mod` d == 0
                                     && show d `isInfixOf` show n) [1..n]

-- instance OEIS 125887 where
--   oeis = 1 : f '1' (filter ((> 0) . (`mod` 10)) [11..]) where
--      f d zs = y : f (last $ show y) (xs ++ ys) where
--          (xs, y:ys) = span ((/= d) . head . show) zs

instance OEIS 129845 where
  oeis =
     filter (\x -> not $ null (show (2*x) `intersect` show x)) [1..]

instance OEIS 184992 where
  oeis = 1 : f 1 [2..] where
     f u vs = v : f v (delete v vs)
       where v : _ = filter (not . null . (intersect `on` show) u) vs

instance OEIS 185817 where
  oeisIx n = pref101pow 0 1 where
     pref101pow e pow101 = if isPrefixOf (show n) (show pow101)
                              then e
                              else pref101pow (e + 1) (101 * pow101)

-- instance OEIS 190137 where
--   oeisIx n = head [k | k <- [1..9],
--                         all (<= "0123456789" !! k) $ show (k * n)]

-- instance OEIS 192825 where
--   oeis = filter (\x ->
--      '0' `elem` show x && null (show (2*x) `intersect` show x)) [1..]

-- instance OEIS 193513 where
--   oeisIx n = p "0123456789" n 1 where
--      p "" _ _      = 0
--      p _  0 _      = 1
--      p cds m k
--        | m < k     = 0
--        | otherwise = p (cds `intersect` show k) (m - k) k + p cds m (k + 1)

-- instance OEIS 226219 where
--   oeisIx n = head [k | k <- [2..],
--                         isInfixOf (show n) (show (k*n)), not $ p10 k]
--      where p10 = flip isPrefixOf ('1' : repeat '0') . show  :: Int -> Bool

-- instance OEIS 226277 where
--   oeis = sort [w | u <- [0..9], v <- [0..9], let w = u ^ v,
--      "0123456789" !! u `elem` show w, "0123456789" !! v `elem` show w]

instance OEIS 228276 where
  oeis = 1 : f 1 [2..] where
     f x zs = g zs where
       g (y:ys) = if null $ show (x + y) \\ (show x ++ show y)
                     then y : f y (delete y zs) else g ys

-- instance OEIS 229363 where
--   oeis = f "" [0, 2 ..] where
--      f xs (e:es) = if null $ intersect xs ys then e : f ys es else f xs es
--                    where ys = show e

-- instance OEIS 229364 where
--   oeis = f "" [1, 3 ..] where
--      f xs (o:os) = if null $ intersect xs ys then o : f ys os else f xs os
--                    where ys = show o

instance OEIS 234932 where
  oeis = 1 : f 1 [2..] where
     f x zs = g zs where
       g (y:ys) = if null $ show (x * y) \\ (show x ++ show y)
                     then y : f y (delete y zs) else g ys

-- instance OEIS 238880 where
--   oeis = f [0..] where
--      f (u:us) = u : g us where
--        g vs = h vs where
--          h (w:ws) = if reverse ys == ys then w : f (delete w vs) else h ws
--                     where ys = xs ++ show w
--        xs = show u

-- instance OEIS 239664 where
--   oeisIx n = (oeis @239664) `genericIndex` (n - 1)
--   oeis = 1 : f 1 [2..] where
--      f v ws = g ws where
--        g (x:xs) = if gcd v x == 1 && ((intersect `on` show) v x == "")
--                      then x : f x (delete x ws) else g xs

-- instance OEIS 3226 where
--   oeis = filter (\x -> show x `isSuffixOf` show (x^2)) (oeis @8851)

-- instance OEIS 14563 where
--   oeis = 1 : f 1 (drop 2 (oeis @290)) where
--      f x (q:qs) | null $ xs \\ (show q) = y : f y qs
--                 | otherwise             = f x qs
--                 where y = (oeisIx @196) q; xs = show (x * x)

-- instance OEIS 18856 where
--   oeisIx n =
--      fromJust $ findIndex (show n `isPrefixOf`) $ map show (oeis @79)

-- instance OEIS 30000 where
--   oeisIx n = fromJust $ findIndex (show n `isInfixOf`) $ map show (oeis @79)

-- instance OEIS 30001 where
--   oeisIx n = head $ filter ((show n `isInfixOf`) . show) (oeis @79)

-- instance OEIS 30079 where
--   oeis = filter f (oeis @40) where
--      f p = pd == pd `intersect` (nub $ show (p^2)) where
--          pd = nub $ show p

-- instance OEIS 30091 where
--   oeis =
--      filter (\x -> ((==) `on` (nub . sort . show)) x (x^2)) (oeis @40)

-- instance OEIS 30284 where
--   oeis = f [] (oeis @40) where
--      f xs (p:ps) = if null $ intersect xs ys then p : f ys ps else f xs ps
--                    where ys = show p

-- instance OEIS 34844 where
--   oeis = filter (not . any  (`elem` "2357") . show ) (oeis @40)

-- instance OEIS 36433 where
--   oeis = filter f [1..] where
--      f x = d < 10 && ("0123456789" !! d) `elem` show x where d = (oeisIx @5) x

-- instance OEIS 45953 where
--   oeis = filter chi (oeis @8851) where
--      chi n = (x == y && xs `isSub'` ys) where
--         x:xs = show $ div n 10
--         y:ys = show $ div (n^2) 10
--         isSub' us vs = any id $ zipWith (&&)
--                                 (map (`isPrefixOf` vs) $ inits us)
--                                 (map (`isSuffixOf` vs) $ tails us)

-- instance OEIS 46447 where
--   oeis = 1 : filter f [1..] where
--      f x = length ps > 1 && ps' == reverse ps'
--            where ps' = concatMap show ps; ps = (rowT @27746) x

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

-- instance OEIS 47814 where
--   oeisIx n = if null ips then 0 else head ips
--      where ips = [p | p <- reverse $ takeWhile (<= n) (oeis @40),
--                       show p `isInfixOf` show n]

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

-- instance OEIS 59436 where
--   oeisIx n = head [x | x <- [1..],
--      let dds = map length $ group $ sort $ concatMap show $ (rowT @27750) x,
--      minimum dds == n, length dds == 10]

-- instance OEIS 60417 where
--   oeisIx = genericLength . nub . show . (oeisIx @40)

-- instance OEIS 61509 where
--   oeisIx n = product $ zipWith (^)
--     (oeis @40) (map digitToInt $ filter (/= '0') $ show n)

-- instance OEIS 61827 where
--   oeisIx n =
--      p n (map digitToInt $ nub $ sort $ filter (/= '0') $ show n) where
--         p _ []        = 0
--         p 0 _         = 1
--         p m ds'@ (d:ds)
--           | m < d     = 0
--           | otherwise = p (m - d) ds' + p m ds

-- instance OEIS 62584 where
--   oeisIx n = head [p | p <- (oeis @40), show n `isInfixOf` show p]

-- instance OEIS 62634 where
--   oeis = filter
--      (and . map ((elem '1') . show) . (rowT @27750)) (oeis @11531)

-- instance OEIS 64236 where
--   oeisIx = genericLength . show . (oeisIx @1042)

-- instance OEIS 65297 where
--   oeis = 1 : f 1 (drop 2 (oeis @290)) where
--      f x (q:qs) | null (xs \\ sq) && sort xs /= sort sq = y : f y qs
--                 | otherwise                             = f x qs
--                 where y = (oeisIx @196) q; sq = show q; xs = show (x * x)

-- instance OEIS 66825 where
--   oeis = 1 : f 1 (drop 2 (oeis @290)) where
--      f x (q:qs) | all (`elem` show q) xs = y : f y qs
--                 | otherwise              = f x qs
--                 where y = (oeisIx @196) q; xs = show (x * x)

-- instance OEIS 67109 where
--   oeisIx n = sum $
--      map (fromEnum . (show n `isPrefixOf`)) (tails $ show $ (oeisIx @142) n)

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

-- instance OEIS 68690 where
--   oeis = filter (all (`elem` "02468") . init . show) (oeis @40)

-- instance OEIS 69567 where
--   oeis = f (oeis @40) where
--      f (p:ps@ (p':_)) = if sort (show p) == sort (show p')
--                        then p : f ps else f ps

-- instance OEIS 75093 where
--   oeis = f (oeis @40) where
--      f (p:ps@ (q:r:_)) =
--        if sort (show p) == sort (show q) && sort (show q) == sort (show r)
--           then p : f ps else f ps

-- instance OEIS 76490 where
--   oeis = map (length . nub) $
--      zipWith (intersect `on` show) (tail (oeis @40)) (oeis @40)

-- instance OEIS 91871 where
--   oeis = f [1..] (oeis @40) where
--      f (i:is) (p:ps) = if (null $ show p `intersect` "024568")
--                           then i : f is ps else f is ps

-- instance OEIS 95048 where
--   oeisIx = genericLength . group . sort . concatMap show . (rowT @27750)

-- instance OEIS 105417 where
--   oeis = filter ((== "1234567") . sort . nub . show . (oeisIx @61493)) [1..3999]

-- instance OEIS 106432 where
--   oeis = zipWith (levenshtein `on` show)
--                          (oeis @79) $ tail (oeis @79) where
--      levenshtein us vs = last $ foldl transform [0..length us] vs where
--         transform xs@ (x:xs') c = scanl compute (x+1) (zip3 us xs xs') where
--            compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

-- instance OEIS 107801 where
--   oeis = 2 : f 2 (tail (oeis @40)) where
--      f x ps = g ps where
--        g (q:qs) | null (show x `intersect` show q) = g qs
--                 | otherwise                        = q : f q (delete q ps)

-- instance OEIS 119393 where
--   oeis = filter
--      (\x -> not $ null $ show x `intersect` (show $ (oeisIx @40) x)) [1..]

-- instance OEIS 119999 where
--   oeisIx n = p (filter ((`isInfixOf` show n) . show) [1..n]) n where
--      p _  0 = 1
--      p [] _ = 0
--      p ks'@ (k:ks) m | m < k     = 0
--                     | otherwise = p ks' (m - k) + p ks m

-- instance OEIS 121032 where
--   oeis = filter ((isInfixOf "12") . show) (oeis @8594)

-- instance OEIS 131361 where
--   oeisIx n = p [r | r <- tail (oeis @10785), head (show r) `elem` show n] n
--      where p _          0 = 1
--            p ks'@ (k:ks) m = if m < k then 0 else p ks' (m - k) + p ks m

-- instance OEIS 132080 where
--   oeis = [x | x <- [2..], all null $
--                       map (show x `intersect`) $ map show $ (rowT @27751) x]

-- instance OEIS 137580 where
--   oeisIx = genericLength . nub . show . (oeisIx @142)

-- instance OEIS 166573 where
--   oeis = filter (("13" `isInfixOf`) . show) (oeis @40)

-- instance OEIS 171492 where
--   oeis = filter f [1..] where
--      f x = any ((> 0) . mod x) ds where
--        ds = map digitToInt (if c == '0' then cs else cs')
--        cs'@ (c:cs) = nub $ sort $ show x

-- instance OEIS 178318 where
--   oeis = 2 : 5 : filter f (oeis @62332) where
--      f p = null (show p `intersect` "34679") && (oeisIx @10051 . pred) (r 0 p) == 1
--      r y 0 = y
--      r y x = r (10 * y + genericIndex [0,1,5,0,0,2,0,0,8,0] d) x'
--                where (x', d) = divMod x 10

-- instance OEIS 179336 where
--   oeis = filter (any (`elem` "2357") . show ) (oeis @40)

-- instance OEIS 189398 where
--   oeisIx n = product $ zipWith (^) (oeis @40) (map digitToInt $ show n)

-- instance OEIS 197945 where
--   oeisIx n = genericLength $ takeWhile (`isPrefixOf` (show $ (oeisIx @96095) $ n+1)) $
--                                  tail $ inits $ show $ (oeisIx @96095) n

-- instance OEIS 199713 where
--   oeisIx n = f ps where
--      f (q:qs) = if sort (show q) `contains` sort (show p) then q else f qs
--      contains _  []                         = True
--      contains [] _                          = False
--      contains (u:us) vs'@ (v:vs) | u == v    = contains us vs
--                                 | otherwise = contains us vs'
--      p : ps = drop (n - 1) (oeis @40)

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

-- instance OEIS 211396 where
--   oeisIx n = if null ips then 0 else head ips
--      where ips = [p | p <- takeWhile (<= n) (oeis @40),
--                       show p `isInfixOf` show n]

-- instance OEIS 224782 where
--   oeis = map (foldl h 0 . group . show) (oeis @79) where
--      h x zs@ (z:_) = if z == '0' then max x $ length zs else x

-- instance OEIS 238593 where
--   oeisIx n = (+ 1) $ fromJust $ findIndex
--      (isInfixOf $ show $ (oeisIx @40) n) (scanl1 (++) $ map show (oeis @40))

-- instance OEIS 240913 where
--   oeis = filter (not . elem '1' . show) (oeis @69715)

-- instance OEIS 243355 where
--   oeis = filter
--      (\x -> null $ show x `intersect` (show $ (oeisIx @40) x)) [1..]

-- instance OEIS 246398 where
--   oeis = f 0 $ map show (oeis @40) where
--      f x pss = (length ps - length xs) :
--                f (x + 1) (dropWhile (== xs) pss)
--        where ps = head [qs | qs <- pss, isin xs qs]; xs = show x
--      isin [] _  = True
--      isin _  [] = False
--      isin (u:us) vs = not (null ws) && isin us ws
--                       where ws = dropWhile (/= u) vs

-- instance OEIS 248327 where
--   oeisIx 0 = 0
--   oeisIx n = levenshtein (show n) (dropWhile (== '0') $ reverse $ show n)
--   levenshtein :: (Eq t) => [t] -> [t] -> Int
--   levenshtein us vs = last $ foldl transform [0..length us] vs where
--      transform xs@ (x:xs') c = scanl compute (x+1) (zip3 us xs xs') where
--         compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

-- instance OEIS 249854 where
--   oeis = sortBy (compare `on` show) $
--                         takeWhile (<= 10^6) (oeis @93018)

-- instance OEIS 249855 where
--   oeis = sortBy (compare `on` f) $ takeWhile (<= 10^6) (oeis @93018)
--                  where f x = (head $ reverse ds, ds) where ds = show x

-- instance OEIS 253172 where
--   oeis = filter f [2..] where
--      f x = g divs $ reverse divs where
--            g (d:ds) (q:qs) = d <= q &&
--              (sort (nub $ xs ++ show d ++ show q) == decs || g ds qs)
--            xs = show x
--            divs = (rowT @27750) x
--      decs = "0123456789"

-- instance OEIS 254679 where
--   oeis = tablList @254679
-- instance Table 254679 where
--   rowCol = rowCol_off @254679 @1 @1
--   rowT   = rowT_off @254679 @1
--   tabf = map (sortBy (comparing show)) (tabf @27750)

-- instance OEIS 258083 where
--   oeis = f 1 $ tail $ zip
--      (oeis @8585) $ map (reverse . show) (oeis @8585) where
--      f x ws = g ws where
--        g ((u, vs) : uvs) = if isPrefixOf xs vs
--                            then u : f (x + 1) (delete (u, vs) ws) else g uvs
--        xs = reverse $ show x

-- instance OEIS 258188 where
--   oeis = f 1 $ tail $ zip
--      (oeis @8589) $ map (reverse . show) (oeis @8589) where
--      f x ws = g ws where
--        g ((u, vs) : uvs) = if isPrefixOf xs vs
--                            then u : f (x + 1) (delete (u, vs) ws) else g uvs
--        xs = reverse $ show x

-- instance OEIS 258217 where
--   oeis = f 1 $ tail $ zip (oeis @8589) $ map show (oeis @8589) where
--      f x ws = g ws where
--        g ((u, vs) : uvs) = if isPrefixOf (show x) vs
--                            then u : f (x + 1) (delete (u, vs) ws) else g uvs

-- instance OEIS 258738 where
--   oeis = f [1..] where
--      f (x:xs) = if show x `elem` zipWith (++) kss (map show $ (rowT @27750)' x)
--                    then x : f xs else f xs
--      kss = map show [1..]

-- instance OEIS 259143 where
--   oeis = [length $ nub $ show m ++ show d |
--                   m <- [1 .. 12], d <- [1 .. (oeisIx @8685) m]]

-- instance OEIS 262257 where
--   oeis = zipWith (levenshtein `on` show) [0..] (oeis @261423) where
--      levenshtein us vs = last $ foldl transform [0..length us] vs where
--        transform xs@ (x:xs') c = scanl compute (x+1) (zip3 us xs xs') where
--          compute z (c', x, y) = minimum [y+1, z+1, x + fromEnum (c' /= c)]

-- instance OEIS 263856 where
--   oeis = f [] (oeis @4676) where
--      f bps (x:xs) = y : f bps' xs where
--        y = fromJust (elemIndex x bps') + 1
--        bps' = insertBy (compare `on` (reverse . show)) x bps

-- instance OEIS 190126 where
--   oeisIx n = (oeis @190126) !! (n - 1)
--   oeis = sortBy (comparing (show . (oeisIx @7088))) [1..10000]

-- instance OEIS 190128 where
--   oeisIx n = (oeis @190128) !! (n - 1)
--   oeis = sortBy (comparing (show . (oeisIx @7089))) [1..10000]

-- instance OEIS 190130 where
--   oeisIx n = (oeis @190130) !! (n - 1)
--   oeis = sortBy (comparing (show . (oeisIx @7094))) [1..10000]

-- instance OEIS 50430 where
--   import Numeric (showIntAtBase)
--   oeisIx n = (oeis @50430) !! (n - 1)
--   oeis = f 1 where
--      f n = g (showIntAtBase 2 intToDigit n "") : f (n+1)
--      g zs | zs == reverse zs = length zs
--           | otherwise        = max (h $ init zs) (h $ tail zs)
--      h zs@ ('0':_) = g zs
--      h zs@ ('1':_) = (oeisIx @50430) $ foldl (\v d -> digitToInt d + 2*v) 0 zs

-- instance OEIS 38680 where
--   oeis = filter (any ((== 1) . (oeisIx @10051). read) .
--                              init . tail . tails . show) (oeis @40)

-- instance OEIS 39999 where
--   oeisIx n = genericLength $ filter ((== 1) . (oeisIx @10051))
--                      (map read (nub $ permutations $ show n) :: [Integer])

-- instance OEIS 45533 where
--   oeis = f $ map show (oeis @40) :: [Integer] where
--      f (t:ts@ (t':_)) = read (t ++ t') : f ts

-- instance OEIS 45982 where
--   oeis = 1 : f [1] where
--      f xs = y : f (xs ++ [y]) where
--        y = (oeisIx @45918) $ read (concatMap show xs)

-- instance OEIS 46810 where
--   oeisIx n = genericLength $ filter ((== 1) . (oeisIx @10051))
--                      $ map read (nub $ filter ((> '0') . head)
--                                               $ permutations $ show n)

-- instance OEIS 48653 where
--   oeis = filter (f . show . (^ 2)) [1..] where
--      f zs = g (init $ tail $ inits zs) (tail $ init $ tails zs)
--      g (xs:xss) (ys:yss)
--        | h xs      = h ys || f ys || g xss yss
--        | otherwise = g xss yss
--        where h ds = head ds /= '0' && (oeisIx @10052) (read ds) == 1
--      g _ _ = False

-- instance OEIS 48794 where
--   oeis = map (read . concatMap show) (tabf @48793) :: [Integer]

-- instance OEIS 50805 where
--   oeis = filter ((all (== 0)) . f) (oeis @40) where
--      f p = map (i $ show p) "0123456789"
--      i ps d = (oeisIx @10051)' (read $ intersperse d ps :: Integer)

-- instance OEIS 50806 where
--   oeis = filter ((== 1) . sum . f) (oeis @40) where
--      f p = map (i $ show p) "0123456789"
--      i ps d = (oeisIx @10051)' (read $ intersperse d ps :: Integer)

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

-- instance OEIS 68863 where
--   oeis = f "x" (map show (oeis @40)) where
--      f p ps = g ps where
--        g (q:qs)
--          | and $ zipWith (/=) p q = (read q :: Int) : f q (delete q ps)
--          | otherwise = g qs

-- instance OEIS 73646 where
--   oeisIx :: Integer -> Integer
--   oeisIx = read . concat . sort . map show . (rowT @27746)

-- instance OEIS 85732 where
--   oeisIx n = (oeis @85732) !! (n - 2)
--   oeis = map f $ drop 2 $ inits $ concatMap show (oeis @796)
--      where f xs = minimum $ init $ tail $
--                   zipWith (on (+) read) (inits xs) (tails xs)

-- instance OEIS 87712 where
--   oeisIx 1 = 1
--   oeisIx n = read $ concatMap (show . (oeisIx @49084)) $ (rowT @27746) n :: Integer

-- instance OEIS 95958 where
--   oeis = f $ map show (oeis @77800) :: [Integer] where
--      f (t:t':ts) = read (t ++ t') : f ts

-- instance OEIS 106001 where
--   oeisIx n = (oeis @250310) !! (n - 1)
--   oeis = [1..9] ++ [11] ++ f ([0..9] ++ [1,1]) 11 (10 : [12..])
--       where f ss i zs = g zs where
--           g (x:xs) = if ss !! i /= mod x 10
--               then g xs
--               else x : f (ss ++ map (read . return) (show x))
--                   (i + 1) (delete x zs)

-- instance OEIS 106708 where
--   oeisIx 1           = 0
--   oeisIx n
--      | (oeisIx @10051 . pred) n == 1 = 0
--      | otherwise = read $ concat $ (map show) $ init $ tail $ (rowT @27750) n

-- instance OEIS 125002 where
--   oeisIx n = sum $ map (oeisIx' . read) $
--                     tail $ concatMap (f pds) [0 .. length pds - 1] where
--      pds = show $ (oeisIx @40) n
--      f ws k = [us ++ [y] ++ vs |
--               let (us, v:vs) = splitAt k ws, y <- delete v "0123456789"]

-- instance OEIS 128783 where
--   oeis = filter (f . show . (^ 2)) [1..] where
--      f zs = g (init $ tail $ inits zs) (tail $ init $ tails zs)
--      g (xs:xss) (ys:yss)
--        | (oeisIx @10052) (read xs) == 1 = (oeisIx @10052) (read ys) == 1 || f ys || g xss yss
--        | otherwise              = g xss yss
--      g _ _ = False

-- instance OEIS 129800 where
--   oeis = filter ((== 1) . length . f) (oeis @40) where
--     f x = filter (\ (us, vs) ->
--                  (oeisIx @10051 . pred) (read us :: Integer) == 1 &&
--                  (oeisIx @10051 . pred) (read vs :: Integer) == 1) $
--                  map (flip splitAt $ show x) [1 .. length (show x) - 1]

-- instance OEIS 134948 where
--   oeis = filter h [0..] where
--      h x = all (`isInfixOf` xs)
--                (map (fss !!) $ map (read . return) $ sort $ nub xs)
--            where xs = show x
--      fss = map show $ take 10 (oeis @142)

-- instance OEIS 162711 where
--   oeis = tablList @162711
-- instance Table 162711 where
--   rowCol = rowCol_off @162711 @1 @1
--   rowT   = rowT_off   @162711 @1
--   tabl = map (map (read . concatMap show) . tail . inits) $
--                  zipWith take [1..] $ tails (oeis @7376)

-- instance OEIS 171797 where
--   oeisIx n = read $ concatMap (show . ($ n))
--                      [oeisIx, (oeisIx @196563), (oeisIx @196564)] :: Integer

-- instance OEIS 171798 where
--   oeisIx n = read $ concatMap (show . ($ n))
--                      [oeisIx, (oeisIx @23416), (oeisIx @120)] :: Integer

-- instance OEIS 178158 where
--   oeis = filter (\suff -> all ((== 0) . (mod suff))
--      (map read $ tail $ init $ tails $ show suff :: [Integer])) (oeis @67251)

-- instance OEIS 193095 where
--   oeisIx n = sum $ map c [1.. (length $ show n) - 1] where
--      c k | head ys == '0' = 0
--          | otherwise      = (oeisIx @10052) (read xs) * (oeisIx @10052) (read ys) where
--          (xs,ys) = splitAt k $ show n

-- instance OEIS 193890 where
--   oeis = filter f (oeis @107715) where
--      f n = (all ((== 1) . (oeisIx @10051 . pred)) $
--                  zipWith (\ins (t:tns) -> read $ (ins ++ x3 t ++ tns))
--                          (init $ inits $ show n) (init $ tails $ show n))
--          where x3 '0' = "0"
--                x3 '1' = "3"
--                x3 '2' = "6"
--                x3 '3' = "9"

-- instance OEIS 224841 where
--   oeis = tablList @224841
-- instance Table 224841 where
--   rowCol = rowCol_off @224841 @1 @1
--   rowT   = rowT_off   @224841 @1
--   tabl = map
--      (reverse . map (read . concatMap show) . init . tails) $
--      tail $ inits (oeis @7376) :: [[Integer]]

-- instance OEIS 238056 where
--   oeis = filter ((== 1) . length . f) (oeis @40) where
--     f x = filter (\ (us, vs) ->
--                  head vs /= '0' &&
--                  (oeisIx @10051 . pred) (read us :: Integer) == 1 &&
--                  (oeisIx @10051 . pred) (read vs :: Integer) == 1) $
--                  map (flip splitAt $ show x) [1 .. length (show x) - 1]

-- instance OEIS 238057 where
--   oeis = filter ((== 2) . length . f) (oeis @40) where
--     f x = filter (\ (us, vs) ->
--                  head vs /= '0' &&
--                  (oeisIx @10051 . pred) (read us :: Integer) == 1 &&
--                  (oeisIx @10051 . pred) (read vs :: Integer) == 1) $
--                  map (flip splitAt $ show x) [1 .. length (show x) - 1]

-- instance OEIS 238332 where
--   oeis = f [] $ drop 4 (oeis @40) where
--      f xs (p:ps) | (oeisIx @10051 . pred) t == 1 || t `elem` xs = f xs ps
--                  | otherwise = p : f (t:xs) ps
--                  where t = read $ tail $ show p

-- instance OEIS 238333 where
--   oeis = f [] $ drop 4 (oeis @40) where
--      f xs (p:ps) | (oeisIx @10051 . pred) t == 1 || t `elem` xs = f xs ps
--                  | otherwise = t : f (t:xs) ps
--                  where t = read $ tail $ show p

-- instance OEIS 247896 where
--   oeis = filter f (oeis @40) where
--      f p = any ((== 1) . (oeisIx @10051 . pred)) $
--                map (+ p) $ filter (> 0) $ map (read . return) $ show p

-- instance OEIS 252942 where
--   oeisIx n = head [y | m <- [1..],
--      let y = read (show m ++ show n ++ show m) :: Integer, (oeisIx @10051 . pred) y == 1]

-- instance OEIS 253910 where
--   oeisIx n = (oeis @253911) !! (n - 1)
--   oeis = map read $
--      zipWith ((++) `on` show) (oeis @18252) (oeis @40) :: [Integer]

-- instance OEIS 253911 where
--   oeis = map read $
--      zipWith ((++) `on` show) (oeis @18252) (oeis @40) :: [Integer]

-- instance OEIS 258337 where
--   oeis = f 1 $ map show (oeis @40) where
--      f x pss = g pss where
--        g (qs:qss) = if show x `isPrefixOf` qs
--                        then (read qs :: Int) : f (x + 1) (delete qs pss)
--                        else g qss

-- instance OEIS 258706 where
--   oeis = f (oeis @40) where
--      f ps'@ (p:ps) | any (== 0) (map (oeisIx @10051 . pred) dps) = f ps
--                   | otherwise = p : f (ps' \\ dps)
--                   where dps = map read $ permutations $ show p

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

-- instance OEIS 7961 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = read $ map intToDigit $
--     t n $ reverse $ takeWhile (<= n) $ tail (oeis @290) where
--       t _ []          = []
--       t m (x:xs)
--           | x > m     = 0 : t m xs
--           | otherwise = (fromInteger m') : t r xs
--           where (m',r) = divMod m x


-- instance OEIS 205956 where
--   oeis = sort $ filter ((== 1) . (oeisIx @10051 . pred)) $
--                         nub $ map read (tail $ subsequences "39467139")

-- instance OEIS 36299 where
--   oeis = map read rabbits :: [Integer] where
--      rabbits = "1" : "10" : zipWith (++) (tail rabbits) rabbits

-- instance OEIS 60109 where
--   oeisIx n = if n == 0 then 22222 else read (conv n) :: Integer where
--      conv 0 = []
--      conv x = conv x' ++ mCode !! d where (x', d) = divMod x 10
--      mCode = map ('0' :) (mc ++ (reverse $ init $ tail $ map reverse mc))
--      mc = zipWith (++) (inits "111111") (tails "22222")

-- instance OEIS 190619 where
--   oeis = map read $ f 2 1 :: [Integer] where
--     f m k
--       | k < m - 1 = ((take k ones) ++ "0" ++ (take (m-k) ones)) : f m (k+1)
--       | otherwise = ((take k ones) ++ "01") : f (m + 1) 1
--     ones = repeat '1'

-- instance OEIS 246999 where
--   oeisIx n = read $ s ++ "21" ++ s ++ "211" ++ s ++ "2" :: Integer
--               where s = replicate n '1'

-- instance OEIS 262356 where
--   oeis = 1 : f "" (S.singleton "1") where
--      f xs s = (read ys :: Int) : f (dropWhile (== '0') ys') (S.insert ys s)
--        where ys@ (_:ys') = head
--                [vs | vs <- zss, isPrefixOf xs vs, S.notMember vs s]
--      zss = map show [2..]

instance OEIS 248131 where
  oeis = 1 : (map (* 3) $
                 concatMap (map (read . return) . show) (oeis @248131))

instance OEIS 257770 where
  oeis = tablList @257770
instance Table 257770 where
  rowT n = filter belge [0..9] where
     belge k = n == (head $ dropWhile (< n) $
                    scanl (+) k $ cycle $ (map (read . return) . show) n)
  tabf = map (rowT @257770) [0..]

instance OEIS 257860 where
  oeis = 1 : filter f [1..] where
     f x = any (\d -> member (x - q + d) $ ps d) $ filter (> 1) $ nub ds
           where q = sum ds; ds = (map (read . return) . show . fi) x
     ps x = iterate (* x) (x ^ 2)

instance OEIS 258682 where
  oeisIx n = read ('0' : minus (show (n ^ 2)) (show n)) :: Int  where
     minus [] _  = []
     minus us [] = us
     minus (u:us) vs | elem u vs = minus us $ delete u vs
                     | otherwise = u : minus us vs

instance OEIS 262323 where
  oeis = 1 : f "1" (map show [2..]) where
     f xs zss = g zss where
       g (ys:yss) | null (intersect its $ tail $ inits ys) &&
                    null (intersect tis $ init $ tails ys) = g yss
                  | otherwise = (read ys :: Int) : f ys (delete ys zss)
       its = init $ tails xs; tis = tail $ inits xs

instance OEIS 262557 where
  oeis = 0 : f [[0]] where
     f xss = if x < 9 then (map (read . concatMap show) zss) ++ f zss else []
             where zss = (map (z :) $ map tail xss) ++ (map (z :) xss)
                   z = x + 1; x = head $ head xss

instance OEIS 263808 where
  oeis = filter essicran [1..] where
     essicran x = last (takeWhile (>= -x) es) == -x where
       es = scanl (-) x (cycle $ map (read . return) $ show x)

instance OEIS 264646 where
  oeis = 11 : f 2 [0, 1, 1] where
     f x digs = (foldl (\v d -> 10 * v + d) 0 ys) : f (x + 1) (digs ++ ys)
       where ys = map (read . return) (show x) ++ [genericIndex digs x]

-- instance OEIS 309979 where
--   hash :: Double -> Inthash = read . sort . take 6 . filter (/='0') . drop 1 . dropWhile (/='.') . show . (** 0.03125)
--   main :: IO ()main = print $ map (floor . fst) . filter ((==234477) . snd) $ map (\x -> (x, hash x)) [2..1000000]

-- instance OEIS 63171 where
--   newtype Word = Word String deriving (Eq, Show, Read)
--   instance Ord Word where
--      Word us <= Word vs | length us == length vs = us <= vs
--                         | otherwise              = length us <= length vs
--   oeisIx n = (oeis @63171) !! (n - 1)
--   oeis = dyck $ S.singleton (Word "S") where
--      dyck s | null ws   = (read w :: Integer) : dyck s'
--             | otherwise = dyck $ S.union s' (S.fromList $ concatMap gen ws)
--             where ws = filter ((== 'S') . head . snd) $
--                               map (`splitAt` w) [0..length w - 1]
--                   (Word w, s') = S.deleteFindMin s
--      gen (us,vs) = map (Word . (us ++) . (++ tail vs)) ["10", "1S0", "SS"]

-- instance OEIS 215244 where
--   import Data.Map (Map, M.singleton, (!), M.insert)
--   newtype Bin = Bin [Int] deriving (Eq, Show, Read)
--   instance Ord Bin where
--      Bin us <= Bin vs | length us == length vs = us <= vs
--                       | otherwise              = length us <= length vs
--   oeisIx n = (oeis @215244) !! n
--   oeis = 1 : f [1] (M.singleton (Bin [0]) 1) where
--      f bs m | last bs == 1 = y : f (succ bs) (M.insert (Bin bs) y m)
--             | otherwise    = f (succ bs) (M.insert (Bin bs) y m) where
--        y = fromEnum (pal bs) +
--            sum (zipWith (\us vs -> if pal us then m M.! Bin vs else 0)
--                         (init $ drop 1 $ inits bs) (drop 1 $ tails bs))
--        pal ds = reverse ds == ds
--        succ [] = [0]; succ (0:ds) = 1 : ds; succ (1:ds) = 0 : succ ds

instance OEIS 7627 where
  oeis = filter modest' [1..] where
     modest' x = or $ zipWith m
                 (map read $ (init $ tail $ inits $ show x) :: [Integer])
                 (map read $ (tail $ init $ tails $ show x) :: [Integer])
        where m u v = u < v && (x - u) `mod` v == 0 && gcd u v == 1

instance OEIS 22488 where
  oeis = 2 : f [2] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22514 where
  oeis = 3 : f [3] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22515 where
  oeis = 4 : f [4] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22516 where
  oeis = 5 : f [5] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22517 where
  oeis = 6 : f [6] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22518 where
  oeis = 7 : f [7] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22519 where
  oeis = 8 : f [8] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

instance OEIS 22520 where
  oeis = 9 : f [9] :: [Integer] where
     f xs = (read $ concatMap show ys) : f ys where
            ys = concat $ transpose [map head zss, map length zss]
            zss = reverse $ group xs

-- instance OEIS 23989 where
--   oeis = 2 : f [2] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f (ys) where
--             ys = concat $ transpose [map length zss, map head zss]
--             zss = group $ sort xs

-- instance OEIS 32762 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) [0..]

-- instance OEIS 32763 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) [0, 2 ..]

-- instance OEIS 32764 where
--   oeis = 1 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) [1, 3 ..]

-- instance OEIS 32981 where
--   oeis = map read $ filter f $ map show [1..] :: [Int] where
--      f ps = all (`elem` neighbours) $ zipWith ((. return) . (:)) ps (tail ps)
--      neighbours = "09" : "90" : zipWith ((. return) . (:))
--         (digs ++ tail digs ++ init digs) (digs ++ init digs ++ tail digs)
--      digs = "0123456789"

instance OEIS 35930 where
  oeisIx n | n < 10    = 0
           | otherwise = maximum $ zipWith (*)
              (map read $ init $ tail $ inits $ show $ fi n)
              (map read $ tail $ init $ tails $ show $ fi n)

-- instance OEIS 36059 where
--   oeis = map (read . concatMap show) fss :: [Integer] where
--      fss = [1] : [1] : zipWith h (tail fss) fss where
--            h vs ws = concatMap (\us -> [length us, head us]) $
--                      group $ reverse $ sort $ vs ++ ws

instance OEIS 36103 where
  oeis = 0 : 1 : map (read . concatMap say . group . reverse . sort)
                 (zipWith ((++) `on` show) (oeis @36103) $ tail (oeis @36103))
                 where say w = (show $ length w) ++ [head w]

instance OEIS 36106 where
  oeis = 1 : 2 : map (read . concatMap say . reverse . group . sort)
                 (zipWith ((++) `on` show) (oeis @36106) $ tail (oeis @36106))
                 where say ws = (show $ length ws) ++ [head ws]

-- instance OEIS 38447 where
--   oeis = f $ S.fromList [11111] where
--      f s = m : f (S.union s' $ S.fromList $ g [] $ show $ fi m) where
--           (m, s') = S.deleteFindMin s
--      g _  []       = []
--      g us ('0':vs) = g (us ++ ['0']) vs
--      g us ('1':vs) = (read (us ++ "10" ++ vs)) : g (us ++ ['1']) vs

instance OEIS 45541 where
  oeis = 2 : f 2 where
     f x = x' : f x'
         where x' = read $ filter (`notElem` show x) $ show (x^2)

-- instance OEIS 47842 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = read $ concat $
--      zipWith ((++) `on` show) (map length xs) (map head xs)
--      where xs = group $ sort $ map (read . return) $ show n

-- instance OEIS 48377 where
--   oeisIx :: Integer -> Integer
--   oeisIx n =
--      read $ concat $ zipWith replicate (map ((+ 1) . digitToInt) ns) ns
--         where ns = show n

-- instance OEIS 51883 where
--   oeis = 1 : f 2 "1" where
--      f :: Integer -> String -> [Int]
--      f x zs = y : f (x + 1) (zs ++ show y) where
--        y = fromJust $ findIndex
--            ((== 0) . (`mod` x) . read . (zs ++)) $ map show [0..]

-- instance OEIS 53392 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = if ys == "" then 0 else read ys where
--      ys = foldl (++) "" $ map show $ zipWith (+) (tail ds) ds
--      ds = (map (read . return) . show) n

-- instance OEIS 56524 where
--   oeis = [read (ns ++ reverse ns) :: Integer |
--                   n <- [0..], let ns = show n]

-- instance OEIS 56525 where
--   oeis = [1..9] ++ [read (ns ++ [z] ++ reverse ns) |
--                   n <- [1..], let ns = show n, z <- "0123456789"]

instance OEIS 59632 where
  oeisIx n = foldl (\v d -> 10 * v + d) 0 $
                    map (flip mod 10) $ zipWith (+) ([0] ++ ds) (ds ++ [0])
              where ds = map (read . return) $ show n

-- instance OEIS 60857 where
--   oeis = 1 : f [1] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f (xs ++ ys) where
--             ys = concat $ transpose [map length zss, map head zss]
--             zss = group $ sort xs

instance OEIS 60979 where
  oeis = filter (\x -> let digs = map (read . return) $ show x in
                               evens digs /= odds digs) [11, 22 ..]
     where evens [] = 0; evens [x] = x; evens (x:_:xs) = x + evens xs
           odds [] = 0; odds [x] = 0; odds (_:x:xs) = x + odds xs

-- instance OEIS 64770 where
--   oeisIx :: Integer -> Integer
--   oeisIx = read . map (("0111222223" !!) . digitToInt) . show

-- instance OEIS 68861 where
--   oeis = f "x" (map show [1..]) where
--      f u us = g us where
--        g (v:vs)
--          | and $ zipWith (/=) u v = (read v :: Int) : f v (delete v us)
--          | otherwise = g vs

instance OEIS 102251 where
  oeis = 1 : (map (* 2) $
                 concatMap (map (read . return) . show) (oeis @102251))

instance OEIS 106039 where
  oeis = filter belge0 [0..] where
     belge0 n = n == (head $ dropWhile (< n) $
                      scanl (+) 0 $ cycle ((map (read . return) . show) n))

instance OEIS 106439 where
  oeis = filter belge1 [1..] where
     belge1 x = x == (head $ dropWhile (< x) $
                      scanl (+) 1 $ cycle (map (read . return) $ show x))

instance OEIS 106518 where
  oeis = filter belge2 [2..] where
     belge2 x = x == (head $ dropWhile (< x) $
                      scanl (+) 2 $ cycle (map (read . return) $ show x))

instance OEIS 106596 where
  oeis = filter belge3 [3..] where
     belge3 x = x == (head $ dropWhile (< x) $
                      scanl (+) 3 $ cycle (map (read . return) $ show x))

instance OEIS 106631 where
  oeis = filter belge4 [4..] where
     belge4 x = x == (head $ dropWhile (< x) $
                      scanl (+) 4 $ cycle (map (read . return) $ show x))

instance OEIS 106792 where
  oeis = filter belge5 [5..] where
     belge5 x = x == (head $ dropWhile (< x) $
                      scanl (+) 5 $ cycle ((map (read . return) . show) x))

instance OEIS 107014 where
  oeis = filter belge6 [6..] where
     belge6 x = x == (head $ dropWhile (< x) $
                      scanl (+) 6 $ cycle (map (read . return) $ show x))

instance OEIS 107018 where
  oeis = filter belge7 [7..] where
     belge7 x = x == (head $ dropWhile (< x) $
                       scanl (+) 7 $ cycle (map (read . return) $ show x))

instance OEIS 107032 where
  oeis = filter belge8 [8..] where
     belge8 x = x == (head $ dropWhile (< x) $
                      scanl (+) 8 $ cycle (map (read . return) $ show x))

instance OEIS 107043 where
  oeis = filter belge9 [9..] where
     belge9 x = x == (head $ dropWhile (< x) $
                      scanl (+) 9 $ cycle (map (read . return) $ show x))

-- instance OEIS 110745 where
--   oeisIx n = read (concat $ transpose [ns, reverse ns]) :: Integer
--               where ns = show n

-- instance OEIS 118628 where
--   oeis = 3 : f [3] :: [Integer] where
--      f xs = (read $ concatMap show ys) : f (ys) where
--             ys = concat $ transpose [map length zss, map head zss]
--             zss = group $ sort xs

instance OEIS 131293 where
  oeis = 0 : 1 : map read
                 (zipWith ((++) `on` show) (oeis @131293) $ tail (oeis @131293))

instance OEIS 135643 where
  oeis = filter f [100..] where
     f x = all (== 0) ws where
           ws = zipWith (-) (tail vs) vs
           vs = zipWith (-) (tail us) us
           us = map (read . return) $ show x

-- instance OEIS 139337 where
--   oeisIx n = read $ concatMap show $ mapMaybe (flip lookup ls) ds :: Int
--      where ls = zip (map head zss) (map length zss)
--            zss = group $ sort ds
--            ds = map (read . return) $ show n :: [Int]

instance OEIS 143473 where
  oeisIx n = foldl (\v d -> 10 * v + d) 0 $ (10 - z) : zs where
     (z:zs) = map (read . return) $ show n

-- instance OEIS 184989 where
--   oeisIx n = read $ interleave (show n) (show (n - 1)) :: Integer where
--      interleave []     ys = ys
--      interleave (x:xs) ys = x : interleave ys xs

-- instance OEIS 222222 where
--   oeisIx = foldl f 0 . map (read . return) . show :: Integer -> Integer
--             where f v d = 10 * v + if d == 1 || d == 5 then 6 - d else d

instance OEIS 226272 where
  oeis = tablList @226272
instance Table 226272 where
  rowT n = sort $ nub [u ^ v | u <- digs, v <- digs]
                  where digs = nub $ map (read . return) $ show n
  tabf = map (rowT @226272) [0..]

instance OEIS 237851 where
  oeis = 1 : f 1 [2..] where
     f x zs = g zs where
       g (u:us) | all ((== 0) . (mod u)) ds = u : f u (delete u zs)
                | otherwise = g us
                where ds = dropWhile (<= 1) $
                           sort $ nub $ map (read . return) $ show x

-- instance OEIS 244112 where
--   oeisIx :: Integer -> Integer
--   oeisIx n = read $ concat $
--      zipWith ((++) `on` show) (map length xs) (map head xs)
--      where xs = group $ reverse $ sort $ map (read . return) $ show n

-- instance OEIS 18800 where
--   oeisIx n = read $ fromJust $
--               find (show n `isPrefixOf`) $ map show (oeis @40) :: Int

-- instance OEIS 19518 where
--   oeis = map read $ scanl1 (++) $ map show (oeis @40) :: [Integer]

-- instance OEIS 19521 where
--   oeis = f "" $ tail (oeis @290) where
--      f xs (q:qs) = (read ys :: Integer) : f ys qs
--        where ys = xs ++ show q

-- instance OEIS 19523 where
--   oeisIx n = read $ concatMap show $ take n $ tail (oeis @45) :: Integer

-- instance OEIS 19546 where
--   oeis = filter (all (`elem` "2357") . show )
--                         ([2,3,5] ++ (drop 2 (oeis @3631)))
--   oeis = filter ((== 1) . (oeisIx @10051)) $
--                         [2,3,5,7] ++ h ["3","7"] where
--      h xs = (map read xs') ++ h xs' where
--        xs' = concat $ map (f xs) "2357"
--        f xs d = map (d :) xs

-- instance OEIS 32760 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) (oeis @290)

-- instance OEIS 32761 where
--   oeis = 0 : map read (zipWith (++) vs (tail us)) :: [Integer]
--      where (us,vs) = unzip $ map ((splitAt 1) . show) (oeis @578)

-- instance OEIS 33308 where
--   oeis = concatMap (map (read . return) . show) (oeis @40) :: [Int]

-- instance OEIS 33664 where
--   oeis = filter (all ((== 1) . (oeisIx @10051). read) .
--                              init . tail . tails . show) (oeis @40)

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

-- instance OEIS 35514 where
--   oeis = map (read . concatMap show) (tabf @35516) :: [Integer]

-- instance OEIS 35515 where
--   oeis = map (read . concatMap show) (tabf @35517) :: [Integer]

-- instance OEIS 36746 where
--   oeis = map (+ 1) $ mapMaybe (`elemIndex` (oeis @61493))
--      (map (read . reverse) $ tail $ inits $ reverse $ show $ (oeisIx @61493) 3888)

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
--     | (oeisIx @209229) n == 1 = 0
--     | (oeisIx @10051) n == 1 = 0
--     | otherwise = read $ concat $ (map show) $ delete n $ tail $ (rowT @182469) n

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

