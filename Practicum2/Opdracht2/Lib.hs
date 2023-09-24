


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
