module OEIS.Common where

import OEIS.OEIS

import qualified Math.NumberTheory.ArithmeticFunctions as A
import Math.NumberTheory.Recurrences
import Data.List
import Data.Bits
import Data.Proxy


class Table (n :: Nat) where
  tabl :: Integral i => [[i]]
  tabl = tabf @n

  tabf :: Integral i => [[i]]
  tabf = tabl @n

  rowT :: Integral i => i -> [i]
  rowT = genericIndex $ tabl @n

  rowCol :: Integral i => i -> i -> i
  rowCol n k = rowT @n n `genericIndex` k

  tablList :: Integral i => [i]
  tablList = concat (tabl @n)


rowT_off :: forall n (o :: Nat) i. (Table n, KnownNat o, Integral i) => i -> [i]
rowT_off n = tabl @n `genericIndex` (n - do fi . natVal $ Proxy @o)

rowCol_off :: forall n (o1 :: Nat) (o2 :: Nat) i. (Table n, KnownNat o1, KnownNat o2, Integral i) => i -> i -> i
rowCol_off n k = (tabl @n `genericIndex` (n - do fi . natVal $ Proxy @o1)) `genericIndex` (k - do fi . natVal $ Proxy @o2)




choices = concat . map permutations . subsequences

zipTail f = zipWith f =<< tail

fact n = product [2..n]
facts = tail factorial
-- facts  = scanl1 (*) [1..]

divisors n = map fi $ A.divisorsList (fi n :: Int)
-- divisors 1 = [1]
-- divisors n = (1:filter ((==0) . rem n) [2..n `div` 2]) ++ [n]
--
totient 0 = 0
totient n = fi . A.totient $ (fi n :: Int)
-- totient n = genericLength $ filter (==1) $ map (gcd n) [1..n]

sumTo n = (n + r) * q
  where
    (q,r) = (shiftR (n + 1) 1, (n + 1) .&. 1)
    -- (q,r) = quotRem (n + 1) 2

bin n 0 = 1
bin 0 k = 0
bin n k = bin (n - 1) (k - 1) * n `div` k

fibs = 0 : 1 : zipTail (+) fibs

fib = fibonacci
-- fib :: Int -> Integer
-- fib n = snd . foldl_ fib_ (1, 0) . dropWhile not $
--             [testBit n k | k <- let s = finiteBitSize n in [s - 1,s - 2..0]]
--     where
--         fib_ (f, g) p
--             | p         = (f* (f+2*g), ss)
--             | otherwise = (ss, g* (2*f-g))
--             where ss = f*f+g*g
--         foldl_ = foldl'


minimax [] = 0
minimax as = max (head as - minimax (tail as)) (last as - minimax (init as))

bimpl 0 0 = 0
bimpl p q = 2 * bimpl p' q' + if u <= v then 1 else 0
            where (p', u) = divMod p 2; (q', v) = divMod q 2

uss = [] : [] : [] : f 2 1 [3..] where
  f x y zs = g zs [] where
      g (v:vs) ws | gcd v y > 1 || gcd v x > 1 = g vs (v : ws)
                  | otherwise = ws : f v x (delete v zs)

