


module Lib
    ( someFunc,euclid
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

euclid::Integer->Integer->Integer
euclid n p
  | n < 0            = 0
  | n `mod` 17 == 2  = -15
  | otherwise        = n + p

--rest = print $ (euclid 2 3)





--main :: IO ()
--main = putStrLn "Test suite not yet implemented"


-- https://serokell.io/blog/introduction-to-template-haskell

-- http://wiki.haskell.org/Template_Haskell

-- https://downloads.haskell.org/~ghc/7.0.2/docs/html/users_guide/template-haskell.html


-- https://srid.ca/haskell-template

-- https://www.parsonsmatt.org/2021/07/12/template_haskell_performance_tips.html


-- https://www.tweag.io/blog/2021-01-07-haskell-dark-arts-part-i/

-- https://github.com/PHPirates/haskell-template-project/blob/master/Setup.hs


-- https://www.joachim-breitner.de/blog/772-Template_Haskell_recompilation

 
-- https://downloads.haskell.org/~ghc/6.0/docs/html/users_guide/template-haskell.html




--opgave 4

-- Ord  voor de conversie van letter naar een ascii

import Data.Char
ord 'a'
chr 98
--
import Data.Char (ord)
main = print (ord 'a')

---
import Data.Char

main = do
  print (ord 'a')
  print (chr 97)
  print (ord '#')
  print (chr 960)

-- chr voor de conversie van ascii naar letter




--opgave 3 b
rsadecrypt::(Integer,Integer)->Integer->Integer
rsadecrypt (e,m) x



--opgave 3a
import Data.Char (ord)
import Data.Char ( chr )

{-
rsaencrypt::(Integer,Integer)->Integer->Integer
rsaencrypt (e,m) x
main = print (ord 'I')
-}



encode :: String -> [Integer]
encode s = map (toInteger . fromEnum ) s

rsa_encode :: Integer -> Integer -> [Integer] -> [Integer]
rsa_encode n e numbers = map (\num -> mod ( num ^ e ) n ) numbers

rsa_decode :: Integer -> Integer -> [Integer] -> [Integer]
rsa_decode d n ciphers = map (\c -> mod ( c ^ d ) n ) ciphers

decode :: [Integer] -> String
decode encoded = map ( chr . fromInteger ) encoded

divisors :: Integer -> [Integer]
divisors n = [m | m <- [1..n] , mod n m == 0 ]

isPrime :: Integer -> Bool
isPrime n = divisors n == [1,n]

totient :: Integer -> Integer -> Integer
totient prime1 prime2 = (prime1 - 1 ) * ( prime2 - 1 )

myE :: Integer -> Integer
myE tot = head [n | n <- [2..tot - 1] , gcd n tot == 1]

myD :: Integer -> Integer -> Integer  -> Integer
myD e n phi = head [d | d <- [1..n] , mod ( d * e ) phi == 1]


main = do
   putStrLn "Enter a test text!"
   text <- getLine
   let primes = take 90 $ filter isPrime [1..]
       p1     = last primes
       p2     = last $ init primes
       tot    = totient p1 p2
       e      =  myE tot
       n   = p1  * p2
       rsa_encoded  =  rsa_encode n e $ encode text
       d  =  myD e n tot
       encrypted = concatMap show rsa_encoded
       decrypted = decode $ rsa_decode d n rsa_encoded
   putStrLn ("Encrypted: " ++ encrypted )
   putStrLn ("And now decrypted: " ++ decrypted )


------
-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

main :: IO ()
main = mapM_ print [13 `modInv`7344, 42 `modInv` 2017]





import qualified Data.Set as Set

minus :: [Int] -> [Int] -> [Int]
minus xs@(x:xt) ys@(y:yt) = case compare x y of
    LT -> x : minus xt ys
    EQ ->     minus xt yt
    GT ->     minus xs yt
minus a         _         = a

union :: [Int] -> [Int] -> [Int]
union xs@(x:xt) ys@(y:yt) = case compare x y of
    LT -> x : union xt ys
    EQ -> x : union xt yt
    GT -> y : union xs yt
union a         []        = a
union []        b         = b

uniq :: Ord a => [a] -> [a]
uniq xs = uniq' Set.empty xs where
    uniq' _ [] = []
    uniq' set (y:ys) | Set.member y set = uniq' set ys
                     | otherwise = y : uniq' (Set.insert y set) xs

primes :: [Int]
primes = 2:3:5:7: gaps 11 wheel (fold3t $ roll 11 wheel primes_)
 where
   primes_ = 11: gaps 13 (tail wheel) (fold3t $ roll 11 wheel primes_)     -- separate feed
   fold3t ((x:xs): ~(ys:zs:t)) = x : union xs (union ys zs)
                                      `union` fold3t (pairs t)              -- fold3t: 5% ~ 10% speedup
   pairs ((x:xs):ys:t)         = (x : union xs ys) : pairs t
   wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
           4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
   gaps k ws@(w:t) cs@ ~(c:u) | k==c  = gaps (k+w) t u              -- (*  better fold, w/ Wheel!   *)
                              | True  = k : gaps (k+w) t cs
   roll k ws@(w:t) ps@ ~(p:u) | k==p  = scanl (\c d->c+p*d) (p*p) ws
                                          : roll (k+w) t u
                              | True  = roll (k+w) t ps

factorize :: Int -> [Int]
factorize n = primeFactors n primes where
    primeFactors 1 _ = []
    primeFactors _ [] = []
    primeFactors m (p:ps) | m < p * p = [m]
                          | r == 0 = p : primeFactors q (p:ps)
                          | otherwise = primeFactors m ps
                          where (q, r) = quotRem m p

totient :: Int -> Double
totient 1 = 1.0
totient n = (fromIntegral n) * product [1.0 - (1.0 / (fromIntegral p)) | p <- uniq $ factorize n]

main :: IO ()
main = print $ snd $ maximum [((fromIntegral n) / (totient n), n) | n <- [1..1000000]]






{-# LANGUAGE BangPatterns #-}

import Control.Monad (when)
import Data.Bool (bool)

totient
  :: (Integral a)
  => a -> a
totient n
  | n == 0 = 1 -- by definition phi(0) = 1
  | n < 0 = totient (-n) -- phi(-n) is taken to be equal to phi(n)
  | otherwise = loop n n 2 --
  where
    loop !m !tot !i
      | i * i > m = bool tot (tot - (tot `div` m)) (1 < m)
      | m `mod` i == 0 = loop m_ tot_ i_
      | otherwise = loop m tot i_
      where
        i_
          | i == 2 = 3
          | otherwise = 2 + i
        m_ = nextM m
        tot_ = tot - (tot `div` i)
        nextM !x
          | x `mod` i == 0 = nextM $ x `div` i
          | otherwise = x

main :: IO ()
main = do
  putStrLn "n\tphi\tprime\n---------------------"
  let loop !i !count
        | i >= 10 ^ 6 = return ()
        | otherwise = do
          let i_ = succ i
              tot = totient i_
              isPrime = tot == pred i_
              count_
                | isPrime = succ count
                | otherwise = count
          when (25 >= i_) $
            putStrLn $ show i_ ++ "\t" ++ show tot ++ "\t" ++ show isPrime
          when
            (i_ `elem`
             25 :
             [ 10 ^ k
             | k <- [2 .. 6] ]) $
            putStrLn $ "Number of primes up to " ++ show i_ ++ " = " ++ show count_
          loop (i + 1) count_
  loop 0 0




-- Problem 31: Determine whether a given integer number is prime.
-- Armed with our experience from Project Euler, this is not very daunting.
-- We make sure n is not divided by any "candidates" up to sqrt(n). Which candidates?
-- Well, the prime numbers, of course! Well, we're not gonna spend time tracking out the
-- prime numbers in a primality test, though - we're going to use the P6 candidates;
-- numbers of the form 6*n±1 :
isprime :: (Integral a) => a -> Bool
isprime n | n < 4 = n > 1
isprime n = all ((/=0) . mod n) $ takeWhile (<=m) candidates
    where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1, 1]])
          m = floor . sqrt $ fromIntegral n

-- Problem 32: Determine the greatest common divisor of two positive integers.
-- Enter Euclid:
mygcd a 0 = abs a
mygcd a b = mygcd b (a `mod` b)

-- Problem 33: Determine whether two positive integer numbers are coprime.
-- Straight from the definition of coprimality:
coprime a b = gcd a b == 1

-- Problem 34: Calculate Euler's totient function.
-- The definition makes it easy for us, again:
phi n = length [m | m <- [1..n-1], coprime m n]


------
-- Given a and m, return Just x such that ax = 1 mod m.
-- If there is no such x return Nothing.
modInv :: Int -> Int -> Maybe Int
modInv a m
  | 1 == g = Just (mkPos i)
  | otherwise = Nothing
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

main :: IO ()
main = mapM_ print [13 `modInv`7344, 42 `modInv` 2017]




