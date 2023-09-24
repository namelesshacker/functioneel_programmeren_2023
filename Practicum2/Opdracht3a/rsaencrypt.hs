


module rsaencrypt
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





-- rsa
-- https://github.com/dmpalyvos/rsa-haskell
-- https://gist.github.com/jds375/8247318
-- https://rosettacode.org/wiki/RSA_code#Haskell
-- python rsa encrypt
-- https://sahandsaba.com/cryptography-rsa-part-1.html
-- haskell modular power
-- https://stackoverflow.com/questions/15098510/can-someone-explain-this-integer-modular-behavior-to-me-in-haskell
-- http://hackage.haskell.org/package/arithmoi-0.11.0.1/docs/Math-NumberTheory-Powers-Modular.html
-- http://hackage.haskell.org/package/arithmoi-0.11.0.1/docs/Math-NumberTheory-Powers-Modular.html#v:powModWord
-- haskell powermod example
-- https://hackage.haskell.org/package/arithmoi-0.4.3.0/src/Math/NumberTheory/Moduli.hs
-- https://hackage.haskell.org/package/arithmoi-0.2.0.4/docs/src/Math-NumberTheory-Moduli.html
-- http://5.9.10.113/66249173/haskell-mod-malfunction
-- https://github.com/osklunds/Crypto/tree/master/src
-- https://github.com/osklunds/Crypto/blob/master/src/Math/PowerModulo.hs
-- https://titanwolf.org/Network/Articles/Article?AID=4578c52e-30a6-42fd-a2c4-d2ce865a5426#gsc.tab=0
-- https://stackoverflow.com/questions/64806248/last-digit-of-a-huge-number
-- http://hackage.haskell.org/package/arithmoi-0.11.0.1/docs/Math-NumberTheory-Powers.html
-- https://crypto.stanford.edu/pbc/notes/numbertheory/exp.html
-- haskell
-- https://rosettacode.org/wiki/Modular_exponentiation#Haskell
-- https://stackoverflow.com/questions/6400568/exponentiation-in-haskell
-- https://hackage.haskell.org/package/arithmoi-0.2.0.4/docs/src/Math-NumberTheory-Moduli.html
-- https://www.mathematik.uni-marburg.de/~lobachev/code/primes/jacobiSum/ModArithmetik.html
-- powermod
-- https://reinerm.wordpress.com/programming/haskell/
-- powermod
-- http://mech.math.msu.su/~vvb/Dush/Haskell/myGcd.hs
-- https://hackage.haskell.org/package/arithmoi-0.4.1.1/docs/Math-NumberTheory-Moduli.html
-- https://wiki.haskell.org/Power_function
-- https://www.reddit.com/r/haskell/comments/mqtk6/fast_power_function/
-- https://stackoverflow.com/questions/66249173/haskell-mod-malfunction
-- https://stackoverflow.com/questions/27019906/type-inference-interferes-with-referential-transparency
-- http://www.ma.rhul.ac.uk/~uvah099/Talks/FuncProgTalk.pdf
-- https://www.mathematik.uni-marburg.de/~lobachev/papers/lobachev-phd-thesis.pdf
-- https://julia-ylwu.readthedocs.io/_/downloads/en/latest/pdf/
-- http://hdiff.luite.com/cgit/cryptostore/commit?id=0.1.0.0



-- https://www.geeksforgeeks.org/eulers-totient-function/
-- https://nl.wikipedia.org/wiki/Indicator_(getaltheorie)
-- https://cp-algorithms.com/algebra/phi-function.html
-- https://www.dcode.fr/euler-totient
-- https://wiki.haskell.org/99_questions/Solutions/34
-- https://mail.haskell.org/pipermail/haskell-cafe/2011-May/091898.html
-- https://stackoverflow.com/questions/49591643/rewriting-euler-totient-function-in-haskell
-- https://codegolf.stackexchange.com/questions/83533/calculate-eulers-totient-function
-- https://www.macs.hw.ac.uk/~hwloidl/Courses/F21DP/tutorial0.html
-- https://rosettacode.org/wiki/Totient_function#Haskell
-- https://www.javaer101.com/en/article/18677294.html
-- https://github.com/eklitzke/evan-haskell-euler/blob/master/polyomino/ArithmeticFunctions.hs
-- https://www.debugcn.com/en/article/18677294.html
-- https://blog-c7ff.kxcdn.com/problem/algorithm/eulers-totient-function/?layout=old
-- http://blog.vmchale.com/article/ats-totient
-- makkelijkste voorbeeld
-- https://mfukar.github.io/2015/07/28/haskell4.html
-- https://www.hpmuseum.org/cgi-sys/cgiwrap/hpmuseum/archv021.cgi?read=223680
-- https://mathoverflow.net/questions/3274/how-hard-is-it-to-compute-the-euler-totient-function
-- euler totient solution
-- https://zach.se/project-euler-solutions/69/
-- c solution
-- https://www.codegrepper.com/code-examples/cpp/euler%27s+totient+function+c%2B%2B
-- euler totient haskell solution
-- http://www.progsoc.uts.edu.au/wiki/Euler_Solution_69




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






-- file: ch06/JSONClass.hs
doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id
	
	
	
	
-- file: ch06/Overlap.hs
class Borked a where
    bork :: a -> String

instance Borked Int where
    bork = show

instance Borked (Int, Int) where
    bork (a, b) = bork a ++ ", " ++ bork b

instance (Borked a, Borked b) => Borked (a, b) where
    bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"
	




-- file: ch06/SimpleClass.hs
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import Data.List

class Foo a where
    foo :: a -> String

instance Foo a => Foo [a] where
    foo = concat . intersperse ", " . map foo

instance Foo Char where
    foo c = [c]

instance Foo String where
    foo = id
	
	
-- file: ch06/JSONClass.hs
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was [(String, JValue)]
            | JArray (JAry JValue)    -- was [JValue]
              deriving (Eq, Ord, Show)
			  
			  
-- file: ch06/JSONClass.hs
jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

jaryToJValue :: (JSON a) => JAry a -> JValue

instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue
	

-- file: ch06/JSONClass.hs
type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right
	


-- file: ch06/DataEither.hs
data Maybe a = Nothing
             | Just a
               deriving (Eq, Ord, Read, Show)

data Either a b = Left a
                | Right b
                  deriving (Eq, Ord, Read, Show)
				  
				  


-- file: ch06/JSONClass.hs
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"
	
	
	
class Session a where
    getLocation :: a -> (Int, Int)

class Person a where
    getName :: a -> String

class (Session a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

data GCDApp = Session | Person deriving (Show, Eq)

class (Person a) => Changeable a where
    setName :: (String) -> a 

instance Session Message where
    getLocation p = (pointX p, pointY p)

instance Movable Message where
    setLocation (x, y) p = p { pointX = x, pointY = y }
	
	
	


class (Session a) => Movable a where
    setLocation :: (Int, Int) -> a -> a
	
	
instance Movable Message where
    setLocation (x, y) p = p { pointX = x, pointY = y }
	
	
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)
let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}


printName :: Teacher -> IO ()
printName teacher = putStrLn $ personName teacher








module Main where

class IsString a where
    fromString :: String -> a

instance IsString [Char] where
    fromString cs = cs
	
	
newtype MyString = MyString String deriving (Eq, Show)
instance IsString MyString where
    fromString = MyString

greet :: MyString -> MyString
greet "hello" = "world"
greet other = other

main = do
    print $ greet "hello"
    print $ greet "fool"
	
	
--
import Data.Char
import Control.Monad
import System.Environment



bar :: Num a => [a] -> [a]
bar xs = foldr (\ x r f g -> f x (r g f)) 
               (\ _ _ -> []) 
               xs 
               (:)
               ((:) . (*2))

-- pubkey :: Integer -> Integer -> Integer -> Integer
-- pubkey e m d  = if e then m else d

myIfStatement :: Int -> Int
myIfStatement a = if a <= 2 then a + 2 else a - 2

pubkey :: Int -> Int -> Int -> Int
pubkey e m d =  if e< m && e+m==1 then m else d


main :: IO ()
main = do
putStrLn ("You rolled: ")
print(bar [1..9])





-----




import Data.Char
import Control.Monad
import System.Environment

-- Location, in two dimensions.
class Located a where
    getLocation :: a -> (Int, Int)

class (Located a) => Movable a where
    setLocation :: (Int, Int) -> a -> a

-- An example type, with accompanying instances.
data NamedPoint = NamedPoint
    { pointName :: String
    , pointX    :: Int
    , pointY    :: Int
    } deriving (Show)

instance Located NamedPoint where
    getLocation p = (pointX p, pointY p)

instance Movable NamedPoint where
    setLocation (x, y) p = p { pointX = x, pointY = y }

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
    where
    (x, y) = getLocation p


class IsString a where
    fromString :: String -> a

instance IsString [Char] where
    fromString cs = cs
	
	
newtype MyString = MyString String deriving (Eq, Show)
instance IsString MyString where
    fromString = MyString

greet :: MyString -> MyString
greet "hello" = "world"
greet other = other


bar :: Num a => [a] -> [a]
bar xs = foldr (\ x r f g -> f x (r g f)) 
               (\ _ _ -> []) 
               xs 
               (:)
               ((:) . (*2))

-- pubkey :: Integer -> Integer -> Integer -> Integer
-- pubkey e m d  = if e then m else d

myIfStatement :: Int -> Int
myIfStatement a = if a <= 2 then a + 2 else a - 2

pubkey :: Int -> Int -> Int -> Int
pubkey e m d =  if e< m && e+m==1 then m else d


main :: IO ()
main = do
putStrLn ("You rolled: ")
print(bar [1..9])

    print $ greet "hello"
    print $ greet "fool"
    
haskell sending data from lass to another
https://stackoverflow.com/questions/15220061/make-a-class-an-instance-of-another-class
http://learnyouahaskell.com/making-our-own-types-and-typeclasses
haskell class with function
https://www.haskell.org/tutorial/classes.html
https://en.wikibooks.org/wiki/Haskell/Classes_and_types
https://serokell.io/blog/haskell-typeclasses
https://stackoverflow.com/questions/63752748/how-to-add-a-new-function-to-an-exsiting-type-class-in-haskell
haskell add function to custom typclass
https://tylerreckart.gitbooks.io/haskell/content/notes/learn_you_a_haskell/07-customTypes.html
https://mmhaskell.com/blog/2018/1/1/general-functions-with-typeclasses#google_vignette
https://codeahoy.com/learn/appliedfp/ch6/
http://downloads.haskell.org/~ghc/7.4.2/docs/html/users_guide/type-class-extensions.html
http://igm.univ-mlv.fr/~vialette/teaching/2021-2022/haskell/lectures/lecture-08.pdf
https://tgdwyer.github.io/haskell2/
http://users.umiacs.umd.edu/~hal/docs/daume02yaht.pdf
haskel other class as object
https://stackoverflow.com/questions/15283429/in-haskell-can-you-create-an-object-of-a-class
haskell multivariable class
https://wiki.haskell.org/Multi-parameter_type_class
https://wiki.haskell.org/Mutable_variable
http://downloads.haskell.org/~ghc/4.08/docs/set/multi-param-type-classes.html
haskell instance multiple types
https://stackoverflow.com/questions/40813463/haskell-multiple-type-classes-for-one-argument
https://wiki.haskell.org/Multiple_instances#:~:text=You%20can%20define%20multiple%20type,are%20therefore%20called%20orphan%20instances.
https://book.realworldhaskell.org/read/using-typeclasses.html
Haskell instance multiple types stack overflow
https://stackoverflow.com/questions/58682986/how-to-declare-multiple-instances-for-the-same-type
https://codereview.stackexchange.com/questions/275452/haskell-implementing-read-for-a-custom-dynamic-value-type
https://softwareengineering.stackexchange.com/questions/386051/placing-haskell-typeclass-instances



http://www2.informatik.uni-freiburg.de/~thiemann/haskell/haskell98-report-html/basic.html
https://www.aostudies.com.sg/wp-content/uploads/2022/01/Appreciating-functional-programming-A-beginner%E2%80%99s-tutorial-to-HASKELL-illustrated-with-applications-in-numerical-methods.pdf
https://www.classes.cs.uchicago.edu/archive/2023/winter/22300-1/qh.pdf
haskell riennman sum
http://users.umiacs.umd.edu/~hal/docs/daume02yaht.pdf
https://stackoverflow.com/questions/38934920/find-least-fixpoint-of-a-function
haskell calculate second-degree function zeros
https://stackoverflow.com/questions/36387272/returning-doubles-in-haskell
haskell double a tuple
https://www.tutorialspoint.com/haskell-program-to-return-multiple-values-from-the-function#:~:text=In%20Haskell%2C%20there%20are%20several,programmer%20to%20perform%20specific%20operations.
https://stackoverflow.com/questions/25043077/how-could-i-return-multiple-values-from-a-function-in-haskell
https://stackoverflow.com/questions/66813015/change-an-element-of-a-list-of-tuples-in-haskell
haskell create list and return list with doubles
https://stackoverflow.com/questions/17383169/haskell-double-every-2nd-element-in-list
https://www.reddit.com/r/haskell/comments/x2omve/double_each_element_in_a_list/
haskellreturn list with doubles
https://stackoverflow.com/questions/36387272/returning-doubles-in-haskell
https://stackoverflow.com/questions/7947981/how-do-i-import-the-foldable-class-into-my-module
haskell relative prime
https://stackoverflow.com/questions/53337362/nth-prime-and-prime-factors
https://wiki.haskell.org/Prime_numbers
https://doisinkidney.com/posts/2018-11-10-a-very-simple-prime-sieve.html
haskell gcd
http://zvon.org/other/haskell/Outputprelude/gcd_f.html
https://www.tutorialspoint.com/haskell-program-to-find-the-gcd-of-two-numbers
https://programming-idioms.org/idiom/74/compute-gcd/947/haskell
https://stackoverflow.com/questions/36317482/getting-the-gcd-of-a-list
haskell rsa
https://gist.github.com/jds375/8247318
https://github.com/dmpalyvos/rsa-haskell
https://stackoverflow.com/questions/71550301/haskell-rsa-encryption
https://codereview.stackexchange.com/questions/179407/a-public-and-private-key-generator-for-rsa
https://web.ecs.syr.edu/~royer/cis675/code/rsa.hs
https://mmhaskell.com/liftoff/syntax?utm_content=cmp-true


 import Data.List
------------------------------------------------------------------------
-- A simple RSA implementation in Haskell
-- For information on the Haskell language, see: http://www.haskell.org
------------------------------------------------------------------------

------------------------------------------------------------------------
-- the list of all primes (slow but short), the sieve of Eratosthenes
primes = sieve [2 ..]
    where sieve (x:xs) = x : sieve [ y | y <-xs, y `mod` x > 0]

-----------------------------------------------------------------------
-- expm a b n = a^b (mod n)
expm :: Integer -> Integer -> Integer -> Integer
expm a 0 n = 1
expm a b n = if even b then (k * k) `mod` n
                       else (a * k * k) `mod` n
    where k =  expm a (b `div` 2) n
-----------------------------------------------------------------------
-- fermat n = the list of all (a^(n-1) mod n) for each a in {1,...,n-1}
--   with gcd(a,n) = 1.
fermat n = [expm a (n-1) n | a <- [1..n-1], gcd a n == 1]

-- FYI: 561 and 1106 are the two smallest Carmichael numbers.
-- See: http://en.wikipedia.org/wiki/Carmichael_number

-----------------------------------------------------------------------
-- xgcd a b = (x,y,d) where d = g.c.d of a and b and d = a*x + b*y
-- Assumes a,b >= 0
xgcd :: Integer -> Integer -> (Integer,Integer,Integer)
xgcd a b = if (b==0)
           then (1,0,a)
           else (y',x'-y'*q,d)
                 where  (q,r)     = divMod a b -- a = q*b+r & 0<=r<b
                        (x',y',d) = xgcd b r

-----------------------------------------------------------------------
-- invert a n = a^(-1) (mod n) , if a and n are rel. prime
--            = 0              , if a and n are not rel. prim
invert :: Integer -> Integer -> Integer
invert a n = let (x,y,d) = xgcd a n
             in if (d/=1) then 0 else x `mod` n

------------------------------------------------------------------------
------------------------------------------------------------------------
-- RSA setup for Alice

p = 2011
q = 2003
n = p * q
phi = (p-1) * (q-1)

e = 1861
d = invert e phi

-- the encryption and decryption functions for Alice
encryptA m = expm m e n
decryptA c = expm c d n

mess = 10203





 
 
haskell sending data from lass to another
https://stackoverflow.com/questions/15220061/make-a-class-an-instance-of-another-class
http://learnyouahaskell.com/making-our-own-types-and-typeclasses
haskell class with function
https://www.haskell.org/tutorial/classes.html
https://en.wikibooks.org/wiki/Haskell/Classes_and_types
https://serokell.io/blog/haskell-typeclasses
https://mmhaskell.com/blog/2018/1/1/general-functions-with-typeclasses
https://stackoverflow.com/questions/63752748/how-to-add-a-new-function-to-an-exsiting-type-class-in-haskell
haskell add function to custom typclass
https://tylerreckart.gitbooks.io/haskell/content/notes/learn_you_a_haskell/07-customTypes.html

-- from https://en.wikibooks.org/wiki/Haskell/Classes_and_types#A_concerted_example
-- Location, in two dimensions.
class Session a where
    getGroundNumbers :: a -> (Int, Int)

class (Session a) => Movable a where
    setEncryption :: (Int, Int) -> a -> a

-- An example type, with accompanying instances.
data Message = Message
    { pointName :: String
    , groundX    :: Int
    , moduloY    :: Int
    } deriving (Show)

instance Session Message where
    getGroundNumbers p = (groundX p, moduloY p)

instance Movable Message where
    setEncryption (x, y) p = p { groundX = x, moduloY = y }

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including Message.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setEncryption (x + dx, y + dy) p
    where
    (x, y) = getGroundNumbers p


data Student = Student String Int
data Deelnemer = Deelnemer String Session


data Teacher = Teacher
  { teacherName:: String
  , teacherAge:: Int
  , teacherDepartment :: String
  , teacherSalary :: Int
  }
 

class IsPerson a where
  personName :: a -> String
  personAge :: a -> Int
  


instance IsPerson Student where
  personName (Student name _) = name
  personAge (Student _ age) = age
  
class Persoon a where
  persoonsNaam :: a -> String
  encryptiesessie :: a -> Session

 
instance IsEenPersoon Deelnemer where
  persoonsNaam (Deelnemer name _) = name
  encryptiesessie (Deelnemer _ sessie) = sessie

  
printName :: Teacher -> IO ()
printName teacher = putStrLn $ personName teacher


printName :: (IsPerson a) => a-> IO ()
printName person = putStrLn $ personName person

printName :: (IsEenPersoon a) => a-> IO ()
printName person = putStrLn $ persoonsNaam person


instance Located Message where
    getGroundNumbers p = (pointX p, pointY p)

instance Movable Message where
    setEncryption (x, y) p = p { pointX = x, pointY = y }


-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including Message.
send :: (Movable a) => (Int, Int) -> a -> a
send (dx, dy) p = setEncryption (x + dx, y + dy) p
    where
    (x, y) = getGroundNumbers p



-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including Message.
receive :: (Movable a) => (Int, Int) -> a -> a
receive (dx, dy) p = setEncryption (x + dx, y + dy) p
    where
    (x, y) = getGroundNumbers p
	
	



data (IsPerson a) => EmployeeRecord a = EmployeeRecord
  { employee :: a
  , employeeTenure :: Int
  }
  
  
  
t = Message "mag ik van jou de sleutel" 15 13	
	
https://codeahoy.com/learn/appliedfp/ch6/

{-# LANGUAGE OverlappingInstances #-}
module Help where

    class MyShow a where
      myshow :: a -> String

    instance MyShow a => MyShow [a] where
      myshow xs = concatMap myshow xs

    showHelp :: MyShow a => [a] -> String
    showHelp xs = myshow xs

{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
module Main where
    import Help

    data T = MkT

    instance MyShow T where
      myshow x = "Used generic instance"

    instance MyShow [T] where
      myshow xs = "Used more specific instance"

    main = do { print (myshow [MkT]); print (showHelp [MkT]) }
	
	


module Main where

import GHC.Exts( IsString(..) )

newtype MyString = MyString String deriving (Eq, Show)
instance IsString MyString where
    fromString = MyString

greet :: MyString -> MyString
greet "hello" = "world"
greet other = other

main = do
    print $ greet "hello"
    print $ greet "fool"
	
	
http://downloads.haskell.org/~ghc/7.4.2/docs/html/users_guide/type-class-extensions.html
http://igm.univ-mlv.fr/~vialette/teaching/2021-2022/haskell/lectures/lecture-08.pdf
https://tgdwyer.github.io/haskell2/
http://users.umiacs.umd.edu/~hal/docs/daume02yaht.pdf



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


