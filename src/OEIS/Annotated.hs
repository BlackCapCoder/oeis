module OEIS.Annotated where


-- instance OEIS 8523 where
--   import Data.Text (Text); import qualified Data.Text as T (all)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @8523) !! (n - 1)
--   oeis = filter (T.all (/= 't') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

-- instance OEIS 85513 where
--   import Data.Text (Text); import qualified Data.Text as T (unpack))
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx = genericLength . filter (== 'e') . T.unpack . numeral where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

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

-- instance OEIS 5282 where
--   oeis = sMianChowla [] 1 S.empty where
--      sMianChowla :: [Integer] -> Integer -> Set Integer -> [Integer]
--      sMianChowla sums z s | s' == S.empty = sMianChowla sums (z+1) s
--                           | otherwise   = z : sMianChowla (z:sums) (z+1) s
--         where s' = try (z:sums) s
--               try :: [Integer] -> Set Integer -> Set Integer
--               try []     s                      = s
--               try (x:sums) s | (z+x) `member` s = S.empty
--                              | otherwise        = try sums $ S.insert (z+x) s

-- instance OEIS 169936 where
--   import Data.Map (M.empty, M.insertWith, M.elems)
--   import Data.Text (unpack); import Data.Maybe (fromJust)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @169936) !! (n - 1)
--   oeis = sort $ concat $ filter ((> 1) . length) $
--      M.elems $ fill [1..999] M.empty where
--         fill [] m = m
--         fill (z:zs) m = fill zs $ M.insertWith (++) (sort $ engl z) [z] m
--         engl :: Integer -> String
--         engl = unpack . fromJust . EN.us_cardinal defaultInflection

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

-- instance OEIS 40051 where
--   oeisIx n = p 1 n :: Int where
--      p _ 0 = 1
--      p k m | k <= m = p k (m - k) `xor` p (k+1) m | k > m  = 0

-- instance OEIS 51145 where
--   oeis = 0 : 1 : f 1 1 where
--      f x b = y : f y z where
--        (y, z) = head [ (y, z) | y <- [1..],
--                                let z = x .|. y :: Integer, z > b]

-- instance OEIS 54240 where
--   oeisIx :: Integer -> Integer -> Integer
--   oeisIx x 0 = x
--   oeisIx x y = (oeisIx @54240) (x `xor` y) (shift (x .&. y) 2)
--   oeisIx_adiag n =  map (\k -> (oeisIx @54240) (n - k) k) [0..n]
--   oeisIx_square = map (oeisIx @54240)_adiag [0..]

-- instance OEIS 58842 where
--   oeis = map numerator (renyi 1 []) where
--      renyi :: Rational -> [Rational] -> [Rational]
--      renyi x xs = r : renyi r (x:xs) where
--         r = q - fromInteger ((numerator q) `div` (denominator q))
--         q = 3%2 * x

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

-- instance OEIS 64418 where
--   oeis = 1 : 2 : 3 : 4 : (f 4 [5..]) where
--      f :: Integer -> [Integer] -> [Integer]
--      f x xs = m : (f m $ delete m xs) where
--         m = head $ dropWhile ((< 4) . (gcd x)) xs

-- instance OEIS 65621 where
--   oeisIx (succ->fi->n) = fi do n `xor` 1 * (n - n .&. negate n) :: Integer

-- instance OEIS 66376 where
--   oeisIx :: Int -> Int
--   oeisIx n = genericLength [d | d <- [1..n - 1], any ((== n) . (orm d)) [1..n]] where
--      orm 1 v = v
--      orm u v = orm (shiftR u 1) (shiftL v 1) .|. if odd u then v else 0

-- instance OEIS 67018 where
--   oeis =  [1,4,3,2] ++ f [2,3,4,1] where
--     f xs = mexi : f (mexi : xs) where
--       mexi = head $ [0..] \\ zipWith xor xs (reverse xs) :: Integer

-- instance OEIS 67398 where
--   oeisIx :: Integer -> Integer
--   oeisIx 0 = 0
--   oeisIx n = orm n n where
--      orm 1 v = v
--      orm u v = orm (shiftR u 1) (shiftL v 1) .|. if odd u then v else 0

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

-- instance OEIS 109812 where
--   oeis = f 0 [1..] :: [Int] where
--      f v ws = g ws where
--        g (x:xs) = if v .&. x == 0 then x : f x (delete x ws) else g xs

-- instance OEIS 142149 where
--   oeisIx :: Integer -> Integer
--   oeisIx = foldl xor 0 . zipWith (.|.) [0..] . reverse . enumFromTo 1

-- instance OEIS 142151 where
--   oeisIx :: Integer -> Integer
--   oeisIx = foldl (.|.) 0 . zipWith xor [0..] . reverse . enumFromTo 1

-- instance OEIS 160700 where
--   oeis = [0..15] ++ map f [16..] where
--      f x = (oeisIx @160700) x' `xor` m :: Int where (x', m) = divMod x 16

-- instance OEIS 167939 where
--   a :: [Integer]
--   a = scanl1 (+) . (!! 1) . transpose . fix $ map ((1:) . zipWith (*) (scanl1 (*) l) . zipWith poly (scanl1 (+) l)) . scanl (flip (:)) [] . zipWith (zipWith (*)) pascal where l = iterate (2*) 1
--   pascal :: [[Integer]]
--   pascal = iterate (\l -> zipWith (+) (0: l) l) (1: repeat 0)
--   -- evaluate a polynomial at a given value
--   poly :: (Num a) => a -> [a] -> a
--   poly t = foldr (\e i -> e + t*i) 0

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

-- instance OEIS 182182 where
--   oeis = 0 : 1 : zipWith xor [2..]
--                  (zipWith xor (oeis @182182) $ tail (oeis @182182)) :: [Integer]

-- instance OEIS 182242 where
--   oeis = map fst $ iterate f (0,1) where
--      f (y,x) = ((x + y) .&. x, x + 1) :: (Integer,Integer)

-- instance OEIS 182243 where
--   oeis = map fst $ iterate f (0,1) where
--      f (y,x) = ((x .&. y) + x, x + 1) :: (Integer,Integer)

-- instance OEIS 182248 where
--   oeis = map fst $ iterate f (0,1) where
--      f (y,x) = ((x .|. y) + x, x + 1) :: (Integer,Integer)

-- instance OEIS 182310 where
--   oeis = 0 : map (+ 1)
--      (zipWith xor (oeis @182310) $ map (`div` 2) (oeis @182310)) :: [Integer]

-- instance OEIS 182388 where
--   oeis = f 0 1 where
--      f x y = y' : f (x + 1) y' :: [Integer] where y' = (x `xor` y) + x

-- instance OEIS 182560 where
--   oeis = 0 : 1 : 2 : zipWith xor [3..]
--      (tail $ zipWith (.&.) (oeis @182560) $ tail (oeis @182560)) :: [Integer]

-- instance OEIS 192484 where
--   oeis = 1 : 2 : f [2,1] where
--      f xs = y : f (y : xs) where
--        y = sum $ zipWith xor xs $ reverse xs :: Integer

-- instance OEIS 199770 where
--   oeis = 1 : f [1] where
--      f xs = y : f (y : xs) where
--        y = sum $ zipWith xor xs $ reverse xs :: Integer

-- instance OEIS 220894 where
--   t :: Int -> Int -> Integer
--   t 0 m = (fi m)
--   t n m = t (n - 1) (m+1) + foldl (+) 0 (let tt = [t i m | i<- [0.. (n-1)]] in
--                                         (map (uncurry (*)) (zip tt (reverse tt))))

-- instance OEIS 224345 where
--   gtab :: [[Integer]]
--   gtab = [0..] : [[s n m |  m <- [0..]] | n <- [1..]]
--     where s n m  = let fi =  [ftab !! i !! m | i <- [0.. (n - 1)]]
--                        gi =  [gtab !! i !! m | i <- [0.. (n - 1)]]
--                    in foldl (+) 0 (map (uncurry (*)) (zip fi (reverse gi)))
--   ftab :: [[Integer]]
--   ftab = [0..] : [[ftab !! (n - 1) !! (m+1) + gtab !! n !! m | m<-[0..]] | n<-[1..]]
--   f (n,m) = ftab !! n !! m

-- instance OEIS 225183 where
--   zipl :: [[x]] -> [x]
--   zipl (s:ss) = head s : zipl (ss ++ [ (tail s)])
--   oeisIx = s where
--     s = 0 : x
--     x = 1 : zipl [x,y]
--     y = 0 : 1 : zipl [z,x,y]
--     z = 0 : zipl [y,x]

-- instance OEIS 6068 where
--   oeisIx n = foldl xor 0 $ map (div n) $ takeWhile (<= n) (oeis @79) :: Integer

-- instance OEIS 49782 where
--   oeisIx :: Int -> Integer
--   oeisIx n = (sum $ take n (oeis @142)) `mod` (fi n)

-- instance OEIS 72137 where
--   oeisIx :: Int -> Int
--   oeisIx = genericLength . fst . spanCycle (abs . (oeisIx @56965)) where
--      spanCycle :: Eq a => (a -> a) -> a -> ([a],[a])
--      spanCycle f x = fromJust $ find (not . null . snd) $
--                                 zipWith (span . (/=)) xs $ inits xs
--                      where xs = iterate f x

-- instance OEIS 72594 where
--   oeisIx = foldl1 xor . (rowT @27746) :: Integer -> Integer

-- instance OEIS 77854 where
--   oeis = scanl1 xor $ tail (oeis @975) :: [Integer]

-- instance OEIS 128630 where
--   oeis = map (minimum . map (sum . map (gpfs !!))) $ tail pss where
--      pss = [] : map parts [1..] :: [[[Int]]] where
--            parts u = [u] : [v : ps | v <- [1..u],
--                                      ps <- pss !! (u - v), v < head ps]
--      gpfs = map fromInteger (0 : map (oeisIx @6530) [1..])

-- instance OEIS 154771 where
--   oeisIx = sum . (rowT @218978) :: Integer -> Integer

-- instance OEIS 178910 where
--   oeisIx = foldl1 xor . (rowT @27750) :: Integer -> Integer

-- instance OEIS 180076 where
--   oeis :: [Integer]
--   oeis = 0 : f 0 [1..] where
--      f x zs = y : f y (delete y zs) where
--        y = if null ys then 3 * x + 1 else head ys
--        ys = [y | y <- takeWhile (< x) zs, binInfix y x]
--      binInfix u v = ib v where
--        ib w = w `mod` m == u || w > u && ib (w `div` 2)
--        m = (oeisIx @62383) u

-- instance OEIS 182145 where
--   oeis = zipWith xor (oeis @1223) $ tail (oeis @1223) :: [Integer]

-- instance OEIS 218388 where
--   oeisIx = foldl1 (.|.) . (rowT @27750) :: Integer -> Integer

-- instance OEIS 218403 where
--   oeisIx = foldl (.|.)  0 . (rowT @27751) :: Integer -> Integer

-- instance OEIS 219463 where
--   oeis = tablList @219463
--   rowCol n k = (tabl @219463) !! n !! k :: Int
--   rowT n = (tabl @219463) !! n
--   tabl = map (map (1 -)) (tabl @47999)

-- instance OEIS 226273 where
--   oeisIx = genericLength . (rowT @226272) :: Integer -> Int

-- instance OEIS 226532 where
--   oeisIx n = product $ zipWith (^)
--               (oeis @40) (scanr1 xor $ (rowT @67255) n :: [Integer])

-- instance OEIS 245656 where
--   oeisIx = (0 ^) . (oeisIx @54025) :: (Integral a, Integral t) => a -> t

-- instance OEIS 253581 where
--   oeis = zipWith (.&.) (oeis @252867) $ drop 2 (oeis @252867) :: [Int]

-- instance OEIS 253582 where
--   oeis = zipWith (.|.) (oeis @252867) $ tail (oeis @252867) :: [Int]
--   oeis' = zipWith (+) (oeis @252867) $ tail (oeis @252867)

-- instance OEIS 162910 where
--   import Ratio
--   bird :: [Rational]
--   bird = branch (recip . succ) (succ . recip) 1
--   branch f g a = a : branch f g (f a) \/ branch f g (g a)
--   (a : as) \/ bs = a : (bs \/ as)
--   (oeisIx @162909) = map numerator bird
--   (oeisIx @162910) = map denominator bird

-- instance OEIS 252867 where
--   oeis = 0 : 1 : 2 : f 1 2 [3..] where
--      f :: Int -> Int -> [Int] -> [Int]
--      f u v ws = g ws where
--        g (x:xs) = if x .&. u > 0 && x .&. v == 0
--                      then x : f v x (delete x ws) else g xs

-- instance OEIS 253315 where
--   oeisIx :: Integer -> Integer
--   oeisIx = f 0 0 where
--      f _ y 0 = y
--      f z y x = f (z + 1) (y `xor` b * z) x' where (x', b) = divMod x 2

-- instance OEIS 273620 where
--   oeisIxT :: Integral a => a -> a -> a
--   oeisIxT n k = floor $ sqrt k' * c where
--     (n', k') = (fi n, fi k)
--     c = fi $ floor $ n' / sqrt k' + 1

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

-- instance OEIS 160638 where
--   import Data.Word (Word8)
--   oeisIx :: Word8 -> Word8
--   oeisIx n = rev 0 0 where
--      rev 8 y = y
--      rev i y = rev (i + 1) (if testBit n i then setBit y (7 - i) else y)

-- instance OEIS 217589 where
--   import Data.Word (Word16)
--   oeisIx :: Word16 -> Word16
--   oeisIx n = rev 0 0 where
--      rev 16 y = y
--      rev i y = rev (i + 1) (if testBit n i then setBit y (15 - i) else y)

-- instance OEIS 308267 where
--   bintodec :: [Int] -> Int
--   bintodec = sum . zipWith (*) (iterate (*2) 1) . reverse
--   decomp :: (Integer, [Integer]) -> (Integer, [Integer])
--   decomp (x, ys) = if even x then (x `div` 2, 0:ys) else (x - 1, 1:ys)
--   zeck :: Integer -> String
--   zeck n = bintodec (1 : snd (last $ takeWhile (\ (x, ys) -> x > 0) $ iterate decomp (n, [])))
--   output :: [Integer]
--   output = filter (\x -> 0 == zeck x `mod` x) [1..100]

-- instance OEIS 6933 where
--   import Data.Text (Text); import qualified Data.Text as T (unpack)
--   import Text.Numeral.Grammar.Reified (defaultInflection)
--   import qualified Text.Numeral.Language.EN as EN
--   oeisIx n = (oeis @6933) !! (n - 1)
--   oeis = filter (T.all (/= 'e') . numeral) [0..] where
--      numeral :: Integer -> Text
--      numeral = fromJust . EN.gb_cardinal defaultInflection

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

-----------------

-- instance OEIS 72591 where
--   oeisIx = foldl1 (.&.) . (rowT @27746)

-- instance OEIS 14081 where
--   oeisIx n = (oeisIx @120) (n .&. div n 2)

-- instance OEIS 72593 where
--   oeisIx = foldl1 (.|.) . (rowT @27746)

-- instance OEIS 105029 where
--   oeisIx n = foldl (.|.) 0 $ zipWith (.&.) (oeis @79) $
--      map (\x -> (len + 1 - (oeisIx @70939) x) * x)
--          (reverse $ enumFromTo n (n - 1 + len))  where len = (oeisIx @103586) n

-- instance OEIS 51146 where
--   oeis = zipWith (.|.) (oeis @51145) $ tail (oeis @51145)

-- instance OEIS 38554 where
--   oeisIx n = foldr (\d v -> v * 2 + d) 0 $ zipWith xor bs $ tail bs
--      where bs = (rowT @30308) n

-- instance OEIS 70883 where
--   oeis = zipWith xor [1..] (oeis @40)

-- instance OEIS 100892 where
--   oeisIx n = (2 * n - 1) `xor` (2 * n + 1)
--   oeis = zipWith xor (tail (oeis @5408)) (oeis @5408)

-- instance OEIS 248663 where
--   oeisIx = foldr (xor) 0 . map (\i -> 2^ (i - 1)) . (rowT @112798)

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

-- instance OEIS 56973 where
--   oeisIx = f 0 where
--      f y x = if x == 0 then y else f (y + 0 ^ (mod x 4)) $ div x 2
--   a (n) = { my (x = bitor (n, n>>1));
--            if (x == 0, 0, 1 + logint (x, 2) - hammingweight (x)) }

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

-- instance OEIS 259361 where
--   oeisIx = floor . subtract (1 / 2) . sqrt . (+ 1 / 4) . fi
--   oeis = concat xss where
--      xss = iterate (\ (xs@ (x:_)) -> map (+ 1) (x : x : xs)) [0, 0]

-- instance OEIS 277278 where
--   oeisIx n
--     | isSquare n = n
--     | otherwise = last $ fromJust $ find (isSquare . sum) s
--     where
--       s = map ((n:) . map (n+)) (tabf @48793)
--       isSquare m = m == (integerRoot * integerRoot) where integerRoot = isqrtA m

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

-- instance OEIS 194733 where
--   oeisIx n = genericLength $ filter (nTau <) $
--               map (snd . properFraction . (* tau) . fromInteger) [1..n]
--      where (_, nTau) = properFraction (tau * fromInteger n)
--            tau = (1 + sqrt 5) / 2

-- instance OEIS 74976 where
--   oeis = map (round . recip) $ zipWith (-) (tail rs) rs
--                  where rs = map (sqrt . fi) (oeis @40)

-- instance OEIS 121924 where
--   oeisIx (fi->n) = fi $ (oeisIx @7318) b 3 + (n - (oeisIx @7318) b 2) * (b* (b+3) - 2* (n+1)) `div` 4
--               where b = round $ sqrt $ 2 * fi n + 1/4

-- instance OEIS 171971 where
--   oeisIx = floor . (/ 4) . (* sqrt 3) . fromInteger . (oeisIx @290)

-- instance OEIS 171972 where
--   oeisIx = floor . (* sqrt 3) . fromInteger . (oeisIx @290)

-- instance OEIS 171973 where
--   oeisIx = floor . (/ 12) . (* sqrt 2) . fromInteger . (oeisIx @578)

-- instance OEIS 247485 where
--   oeisIx = (+ 1) . floor . (* 2) . sqrt . fi . (oeisIx @40)

-- instance OEIS 252477 where
--   oeis = map (floor . recip) $ zipWith (-) (tail rs) rs
--                  where rs = map (sqrt . fi) (oeis @40)

