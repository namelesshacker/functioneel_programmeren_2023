


module MIMA
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




{-

opdracht 6
-}



{-Rsa encryptie werkt met twee sleutels:
* de private sleutel: strikt geheim en in jouw persoonlijke bezit.
* de publieke sleutel: deze mag iedereen hebben.
Voor het genereren van sleutels zijn twee priemgetallen, p en q nodig. Kies
twee priemgetallen. Hou deze getallen klein, d.w.z. tussen de 100 en 500. In
echte toepassingen van rsa encryptie zijn de getallen veel groter en worden veel
efficientere algoritmen gebruikt. Deze algoritmen maken gebruik van geavanceerdere
getaltheorie. In deze opdracht houden we het bij de basale werking
van rsa encryptie.
De volgende berekeningen moeten worden uitgevoerd:
* De modulus: m = p * q
* Eulers totient functie: m0 = phi(m) = (p - 1) * (q - 1)
Vervolgens kiezen we een getal e dat relatief priem is met m0. Het getal e
voldoet dus aan de volgende twee voorwaarden:
* e < m0
* ggd(e;m0) = 1
Zodra we een geschikt getal e gekozen hebben, moeten we een bijbehorende d
berekenen. Voor d geldt:
- e * d = 1(mod m0)
We hebben nu de private sleutel, de publieke sleutel en de modulus!
* De private sleutel is e
* De publieke sleutel is d
* De modulus is m-}





module Lib
    ( someFunc
    ) where
-- import qualified Data.Set as Set
import Control.Monad (when)
import Data.Bool (bool)
-- import System.Random
-- import Crypto.Random
-- import Crypto.Random.DRBG
-- import Control.Monad.CryptoRandom
import Data.Bits
-- import qualified Data.ByteString as BS
import Data.List
someFunc :: IO ()
someFunc = putStrLn "someFunc"



minus :: [Int] -> [Int] -> [Int]
minus xs@(x:xt) ys@(y:yt) = case compare x y of
    LT -> x : minus xt ys
    EQ ->     minus xt yt
    GT ->     minus xs yt
minus a         _         = a

{-union :: [Int] -> [Int] -> [Int]
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



totient :: Int -> Double
totient 1 = 1.0
totient n = (fromIntegral n) * product [1.0 - (1.0 / (fromIntegral p)) | p <- uniq $ factorize n]

main :: IO ()
main = print $ snd $ maximum [((fromIntegral n) / (totient n), n) | n <- [1..1000000]]-}





{-

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
-}





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


-- opgave 2

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


-- De modulus: m = p * q

modulus_m :: Double -> Double  -> Double
modulus_m p q = p * q

-- Eulers totient functie: m0 = phi(m) = (p - 1) * (q - 1)
euler_totient :: Double -> Double  -> Double
euler_totient p q = (p - 1) * (q - 1)

--Vervolgens kiezen we een getal e dat relatief priem is met m0. Het getal e
--voldoet dus aan de volgende twee voorwaarden:
-- * e < m'
-- * ggd(e,m') = 1


--

-- function declaration
gcd5 :: Int->Int->Int

-- function definition
gcd5 a 0 = a
gcd5 a b = gcd5 b (mod a b)



 --https://www.tutorialspoint.com/haskell-program-to-find-the-gcd-of-two-numbers

gcd x y =  gcd' (abs x) (abs y)
	where
	gcd' a 0  =  a
	gcd' a b  =  gcd' b (a `rem` b)




gcd a b
    |   a==b =a
    |   a>b = gcd5(a-b) b
    |   otherwise = gcd5 a (b-a)

-- https://programming-idioms.org/idiom/74/compute-gcd/947/haskell

-- Voor d geldt:  e * d = 1(mod m')


{-
pubkey :: Integer -> Integer -> Integer
pubkey e m  = if e*d = 1 mod m  && gcd e m ==1 then e
-}



{-pubkey :: Int -> Int -> Int -> Int
pubkey e m d =  if e*d == 1 mod m  && gcd e m ==1 then e-}


{-pubkey2 :: Int -> Int -> Int -> Int
pubkey2 e m d =  if e*d == (1 mod m)  && (gcd5 e m) ==1-}

{-privkey :: Integer -> Integer -> Integer
privkey e m  = if e < m && gcd e m ==1 then e-}


-- https://gist.github.com/jds375/8247318



-- https://github.com/dmpalyvos/rsa-haskell

rsaencrypt :: (Integer, Integer) -> Integer -> Integer
rsaencrypt (e, m) x = x^e `mod` m

rsadecrypt :: (Integer, Integer) -> Integer -> Integer
rsadecrypt (d, m) x = x^d `mod` m


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






-- https://stackoverflow.com/questions/71550301/haskell-rsa-encryption


data PublicKey  = PublicKey Integer Integer deriving (Show,Eq)
data PrivateKey = PrivateKey Integer Integer deriving (Show,Eq)

defaultExp = 65537

modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  where t = if testBit e 0 then b `mod` m else 1




totient :: Integer -> Integer -> Integer
totient p q = lcm (p-1) (q-1)

-- Find GCD of two numbers plus the Coefficients of Bezouts Identity.
-- Used to find modular inverse.
euclideanAlg :: Integer -> Integer -> (Integer, Integer, Integer)
euclideanAlg a b
  | b > a     = tripFlip $ euclideanAlg2 b 1 0 a 0 1
  | otherwise = euclideanAlg2 a 1 0 b 0 1
  where
    tripFlip (a,b,c) = (a,c,b)
    euclideanAlg2 rk0 sk0 tk0 0 sk1 tk1 = (rk0,sk0,tk0)
    euclideanAlg2 rk0 sk0 tk0 rk1 sk1 tk1 =
        let qi = rk0 `div` rk1 in
        euclideanAlg2 rk1 sk1 tk1 (rk0 - qi*rk1) (sk0 - qi*sk1) (tk0 - qi*tk1)

-- Modular inverse, d, of a such that a.d = 1 mod m
modMultInv :: Integer -> Integer -> Integer
modMultInv m a = let (r,_,d) = euclideanAlg m a
                 in d `mod` m




-----------------
-- Prime Number Generator using Secure RNG
-----------------




-- https://codereview.stackexchange.com/questions/179407/a-public-and-private-key-generator-for-rsa

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
fermat n = [expm a (n-1) n | a <- [1..n-1], mygcd a n == 1]

-- FYI: 561 and 1106 are the two smallest Carmichael numbers.
-- See: http://en.wikipedia.org/wiki/Carmichael_number

-----------------------------------------------------------------------
-- xgcd a b = (x,y,d) where d = g.c.d of a and b and d = a*x + b*y
-- Assumes a,b >= 0



-- Problem 32: Determine the greatest common divisor of two positive integers.
-- Enter Euclid:
mygcd a 0 = abs a
mygcd a b = mygcd b (a `mod` b)

-- Problem 33: Determine whether two positive integer numbers are coprime.
-- Straight from the definition of coprimality:
coprime a b = mygcd a b == 1

-- Problem 34: Calculate Euler's totient function.
-- The definition makes it easy for us, again:
phi n = length [m | m <- [1..n-1], coprime m n]
-----------------------------------------------------------------------
-- invert a n = a^(-1) (mod n) , if a and n are rel. prime
--            = 0              , if a and n are not rel. prim


------------------------------------------------------------------------
------------------------------------------------------------------------
-- RSA setup for Alice

p = 2011
q = 2003
n = p * q
my_phi = (p-1) * (q-1)

e = 1861
d = invert e my_phi

-- the encryption and decryption functions for Alice
encryptA m = expm m e n
decryptA c = expm c d n

mess = 10203


--  https://web.ecs.syr.edu/~royer/cis675/code/rsa.hs



-- from https://en.wikibooks.org/wiki/Haskell/Classes_and_types#A_concerted_example
-- Location, in two dimensions.



data Shape = Circle Float Float Float | Rectangle Float Float Float Float
data RSAKey = PubKey Float Float Float | PrivKey Float Float Float Float

-- Circle :: Float -> Float -> Float -> Shape
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


encroAnna :: RSAKey -> Float
encroAnna (PubKey _ _ r) = pi * r ^ 2
encroAnna (PrivKey x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

encroBob :: RSAKey -> Float
encroBob (PubKey _ _ r) = pi * r ^ 2
encroBob (PrivKey x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

encroMiTM :: Shape -> Float
encroMiTM (Circle _ _ r) = pi * r ^ 2
encroMiTM (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


-- An example type, with accompanying instances.
data Message = Message
    { pointName :: String
    , pointX    :: Int
    , pointY    :: Int
    } deriving (Show)

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


-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including NamedPoint.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setLocation (x + dx, y + dy) p
    where
    (x, y) = getLocation p




class MITMProtocol a where
    message :: a -> Message
    person :: a -> String

elem = Message "dit is de sleutel" 12 13


data Student = Student String String Int Int


class IsPerson a where
  personName :: a -> String
  bericht :: a -> String
  privesleutel :: a -> Int
  publiekesleutel :: a -> Int

instance IsPerson Student where
  personName (Student name _ _ _) = name
  bericht (Student _ msg _ _) = msg
  privesleutel (Student _ _ privKey _) = privKey
  publiekesleutel (Student _ _ _ pubKey) = pubKey

printName :: (IsPerson a) => a-> IO ()
printName person = putStrLn $ personName person

printBericht :: (IsPerson a) => a-> IO ()
printBericht person = putStrLn $ bericht person

{-printPriveSleutel :: (IsPerson a) => a-> IO ()
printPriveSleutel person = putStrLn $ privesleutel person

printpubKey :: (IsPerson a) => a-> IO ()
printpubKey person = putStrLn $ publiekesleutel person-}

-- An example type, with accompanying instances.
data MITM_Message = MITM_Message
    { msg :: String
    , eNum    :: Int
    , k    :: Int
    } deriving (Show)

class Sessie a where
    setGroundNumbers :: a -> (Int, Int)
    setDeelnemerA :: a -> String
    setDeelnemerB :: a -> String
    setDeelnemerC :: a -> String
    getLocan :: a -> (Int, Int)
    
   
class (Sessie a) => Sendable a where
    sendFromAtoB :: (Int, Int) -> a -> a
    sendFromBtoA :: (Int, Int) -> a -> a
    sendFromAtoC :: (Int, Int) -> a -> a
    sendFromCtoA :: (Int, Int) -> a -> a
    sendFromBtoC :: (Int, Int) -> a -> a
    sendFromCtoB :: (Int, Int) -> a -> a
    setEncryptie :: (Int, Int) -> a -> a


class (Sessie a) => Moving a where
    setter :: (Int, Int) -> a -> a
 
instance Sessie MITM_Message where
    getLocan p = (eNum p, k p)
    setGroundNumbers p = (eNum p, k p)
    
instance Sendable MITM_Message where
    setEncryptie (x, y) p = p { eNum = x, k = y }
    
instance Moving MITM_Message where
    setter (x, y) p = p { eNum = x, k = y }
	
 










{-class (Session a) => Movable a where
    setLocation :: (Int, Int) -> a -> a-}



{-

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

{-
instance Session Message where
    getGroundNumbers p = (groundX p, moduloY p)

instance Movable Message where
    setEncryption (x, y) p = p { groundX = x, moduloY = y }
-}

data Deelnemer = Deelnemer String
  deriving (Eq)


data Student = Student String Int



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

---
class Persoon a where
  persoonsNaam :: a -> String
  -- encryptiesessie :: a -> Session -> (Int, Int)



instance Persoon Deelnemer where
  persoonsNaam (Deelnemer name ) = name
  -- encryptiesessie (Deelnemer _ sessie) = sessie


printNameTeacher :: Teacher -> IO ()
printNameTeacher teacher = putStrLn $ personName teacher


printName :: (IsPerson a) => a-> IO ()
printName person = putStrLn $ personName person

printNaam :: (Persoon a) => a-> IO ()
printNaam person = putStrLn $ persoonsNaam person


instance Session Message where
    getGroundNumbers p = (pointX p, pointY p)

instance Movable Message where
    setEncryption (x, y) p = p { groundX = x, moduloY = y }


-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including Message.
send :: (Movable a) => (Int, Int) -> a -> a
send (dx, dy) p = setEncryption (x + dx, y + dy) p
    where
    (x, y) = getGroundNumbers p

-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including Message.
move :: (Movable a) => (Int, Int) -> a -> a
move (dx, dy) p = setEncryption (x + dx, y + dy) p
    where
    (x, y) = getGroundNumbers p



-- Moves a value of a Movable type by the specified displacement.
-- This works for any movable, including Message.
receive :: (Movable a) => (Int, Int) -> a -> a
receive (dx, dy) p = setEncryption (x + dx, y + dy) p
    where
    (x, y) = getGroundNumbers p





-}


--t = Message "mag ik van jou de sleutel" 15 13

-- https://codeahoy.com/learn/appliedfp/ch6/

{-p = 2011
q = 2003
n = p * q
phi = (p-1) * (q-1)

e = 1861
d = invert e phi

-- the encryption and decryption functions for Alice
encryptA m = expm m e n
decryptA c = expm c d n

mess = 10203-}


main :: IO()
main = do
   let a = 10
   let b = 20

-- printing the output by invoking the gcd5 function
   putStr("GCD of "++ (show a) ++ " and "++ (show b) ++" is:")
   print (gcd5 a b)
  -- putStrLn "n\tphi\tprime\n---------------------"
{-  let loop !i !count
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
          loop 0 0-}


{-
genPrime :: CryptoRandomGen g => g -> Int -> Integer -> Integer -> Integer
genPrime g k minPrime maxPrime = head $ filter (fermatPrimeTest g' k) ns
  where
    Right (i,g'') = crandom g
    g'            = mkStdGen i
    Right (n,_)   = crandomR (minPrime, maxPrime) g''
    ns            = iterate ((+) 2) (n .|. 1)


genPQ :: CryptoRandomGen g => g -> Int -> Integer -> Integer -> (Integer,Integer)
genPQ g k minPrime maxPrime = (p,q)
  where
    Right (g1,g2) = splitGen g
    p = genPrime g1 k minPrime maxPrime
    q = genPrime g2 k minPrime maxPrime

-- genRSAKeys e g k minPrime maxPrime
-- e is public exponent, g is random seed,
-- k is number of iterations to run Rabin-Miller test.
-- minPrime, maxPrime is range to search for primes.
genRSAKeys :: CryptoRandomGen g => Integer -> g
           -> Int -> Integer -> Integer
           -> (PublicKey, PrivateKey)
genRSAKeys e g k minPrime maxPrime = let
                                        (p,q) = genPQ g k minPrime maxPrime
                                        n    = p*q
                                        t    = lcm (p-1) (q-1)
                                        d    = modMultInv t e
                                     in (PublicKey n e, PrivateKey n d)

-}




gcde :: Integer→Integer→(Integer,Integer,Integer)
gcde a b=
let 
gcd_f (r1,x1,y1) (r2,x2,y2)
| r2==0=(r1,x1,y1)
| otherwise=
let
q=r1 ‘div‘ r2
r=r1 ‘mod‘ r2
in
gcd_f (r2,x2,y2) (r,x1−q∗x2,y1−q∗y2)
(d,x,y)=gcd_f (a,1,0) (b,0,1)
in
if   d<0then (−d,−x,−y)
else (d,x,y)


invm :: Integer→Integer→Integer
invm m a
| g/=1=error "No inverse exists"
| otherwise=x ‘mod‘ m
where (g,x,_)=gcde a m


expm :: Integer→Integer→Integer→Integer
expm m b k=(b∧k) ‘mod‘ m


expm :: Integer→Integer→Integer→Integer
expm m b k
let
ex a k s
| k==0=s
| k ‘mod‘ 2==0=((ex (a∗a ‘mod‘ m)) (k ‘div‘ 2)) s
| otherwise=((ex (a∗a ‘mod‘ m)) (k ‘div‘ 2)) (s∗a ‘mod‘ m)
in ex b k 1
data RSAPublicKey=PUB  Integer Integer−−(n,e)
data RSAPrivateKey=PRIV Integer Integer−−(n,d)





genRSAKey :: Integer→Integer→(RSAPrivateKey,RSAPublicKey)
genRSAKey p q=
 
letphi
n=p∗q
e=find (phi ‘div‘ 5)
d=invm phi e
find x
| g==1=x
| otherwise=find ((x+1) ‘mod‘ phi)
where (g,_,_)=gcde x phi
in


computeRSAKey :: Integer→Integer→Integer→(RSAPrivateKey,RSAPublicKey)
computeRSAKey p q e=letphi=(p−1)∗(q−1)(g,_,_)=gcde e phin=p∗qd=invm phi einif   (g/=1)then error "Public exponent not acceptable"else (PRIV n d,PUB n e)Figure 7: RSA key generation with a given public exponentersa :: PublicKey→Integer→Integerersa (n,e) x=expm n x edrsa :: PrivateKey→Integer→Integerdrsa (n,d) x=expm n x d




class Split a wheresplit   :: Integer→a→[Integer]combine :: Integer→[Integer]→aFigure 9: The split classe_rsa :: (Split a)⇒RSAPublicKey→a→[Integer]e_rsa k@(PUB n _) x=map (ersa k) (split n x)d_rsa :: (Split a)⇒RSAPrivateKey→[Integer]→ad_rsa k@(PRIV n _) x=combine n (map (drsa k) x)

data RSAPrivateKey=PRIV Integer Integer−−(n,d)| CRT  Integer Integer Integer Integer Integer Integer Integer−−(n,d,p,q,d mod (p−1),d mod (q−1),(inverse q) mod p)

drsa :: RSAPrivateKey→Integer→Integerdrsa (PRIV n d) x=expm n x ddrsa (CRT n d p q exp1 exp2 coeff) x=let(a1,a2)=(expm p x exp1,expm q x exp2)u=((a2−a1)∗coeff) ‘mod‘ qina1+u∗p



class PRNG g wherenextB       :: g→(Integer,g)nextI       :: Int→g→(Integer,g)nextM       :: Integer→g→(Integer,g)Figure 13: ThePRNGclassdata BBSRand=BBS Integer Integer−−(modulus,x)seedBBSRand :: Integer→Integer→BBSRandseedBBSRand modulus seed=let(g,_,_)=gcde seed modulusinif   g/=1then seedBBSRand modulus (seed+1)else BBS modulus ((seed∗seed) ‘mod‘ modulus)nextBBSBit :: BBSRand→(Integer,BBSRand)nextBBSBit (BBS modulus x)=(x ‘mod‘ 2,BBS modulus ((x∗x) ‘mod‘ modulus))instance PRNG BBSRand wherenextB=nextBBSBit


data  PRNG g⇒SecureRandom g a=SecureRandom (g→(a,g))thenRandom :: PRNG g⇒(SecureRandom g a)→(a→(SecureRandom g b))→(SecureRandom g b)thenRandom (SecureRandom r) f=(SecureRandom (\g→let (v,g’)=r g(SecureRandom r’)=f vin r’ g’))instance PRNG g⇒Monad (SecureRandom g) where(>>=)=thenRandomreturn a=(SecureRandom (\g→(a,g)))


newtype PRNG g⇒ProbabilityTest g=PTEST (Integer→(SecureRandom g Bool))isFermat :: PRNG g⇒Int→(ProbabilityTest g)type PrimeFilter=Integer→BoolmkPrime :: (PRNG g)⇒(ProbabilityTest g)→PrimeFilter→Int→(SecureRandom g Integer)mkPrime (PTEST pTest) filter s=lettry p=dob←pTest p(if b && (filter p)then return pelse try (p+2))indop←nextOddInteger stry p


mkRSAKey :: (PRNG g)⇒Int→SecureRandom g (RSAPrivateKey,RSAPublicKey)mkRSAKey s=dop←mkPrime (isFermat 20) (\p→(p−1) ‘mod‘ 3/=0) (s ‘div‘ 2)q←mkPrime (isFermat 20) (\p→(p−1) ‘mod‘ 3/=0) (s ‘div‘ 2)return (computeRSAKey p q 3)Figure 17: The functionmkRSAKeysign :: RSAPrivateKey→String→Integersign k d=drsa k (sha1 d)verify :: RSAPublicKey→String→Integer→Boolverify k d s=(sha1 d)==(ersa k s)

-- https://docplayer.net/6449300-Implementing-public-key-cryptography-in-haskell.html



ghci> import Crypto.Hash
ghci> hash ("hello world"::ByteString) :: Digest SHA1
2aae6c35c94fcfb415dbe95f408b9ce91ee846ed
ghci> hash ("hello world"::ByteString) :: Digest MD5
5eb63bbbe01eeed093cb22bb8f5acdc3
ghci> hash ("hello world"::ByteString) :: Digest SHA256
b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9
ghci> hash ("hello world"::ByteString) :: Digest SHA3_256
644bcc7e564373040999aac89e7622f3ca71fba1d972fd94a31c3bfbf24e3938
ghci> hash ("hello world"::ByteString) :: Digest Blake2s_256
9aec6806794561107e594b1f6a8a6b0c92a0cba9acf5e5e93cca06f781813b0b
ghci> hash ("hello world"::ByteString) :: Digest Blake2b_256
256c83b297114d201b30179f3f0ef0cace9783622da5974326b436178aeef610

ghci> import Crypto.MAC.HMAC
ghci> import Crypto.Hash
ghci> hmacGetDigest $ hmac ("secret key"::ByteString) ("hello world"::ByteString) :: Digest SHA256
c61b5198df58639edb9892514756b89a36856d826e5d85023ab181b48ea5d018
ghci> hmacGetDigest $ hmac ("secret key"::ByteString) ("hello world"::ByteString) :: Digest Blake2b_256
198e317eba56eee5056b88f527c895d6235ace9153fdf6467e38c2758073328c


ghci> import Crypto.KDF.PBKDF2
ghci> generate (prfHMAC SHA256 :: PRF ByteString) (Parameters {iterCounts = 1000, outputLength = 32}) ("password":: ByteString) ("salt"::ByteString) :: ByteString
"c,(\DC2\228mF\EOT\DLE+\167a\142\157m}/\129(\246&kJ\ETX&M*\EOT`\183\220\179"


ghci> convertToBase Base16 $ (generate (prfHMAC SHA256 :: PRF ByteString) (Parameters {iterCounts = 1000, outputLength = 32}) ("password":: ByteString) ("salt"::ByteString) :: ByteString) :: ByteString
"632c2812e46d4604102ba7618e9d6d7d2f8128f6266b4a03264d2a0460b7dcb3"


ghci> import Crypto.KDF.Scrypt
ghci> generate (Parameters {n=1024,r=8,p=2,outputLength=32}) ("password":: ByteString) ("salt"::ByteString) ::ByteString
"\ETBeHl\244\197Y\DEL\181\&0\141\SYN\185\151\148\215\211\160\189.\148d\185\172\177\202\&2\ETX\SUB\133\223\237"


ghci> import Crypto.KDF.HKDF
ghci> import Crypto.Hash (SHA256)
ghci> let pkr = extract ("salt" :: ByteString) ("secret" :: ByteString) :: PRK SHA256
ghci| in expand pkr ("payload" :: ByteString) 32 :: ByteString
"\DC4\147\223\v%\175\f\177\143\132\202\142\233\236\135\153\253\CANs\213wh\149\193\128\240\192t\DC1\UST,"

ghci> import Crypto.Random
ghci> import Crypto.Cipher.Types
ghci> import Crypto.Cipher.AES (AES256)
ghci> import Crypto.Error
ghci> do
ghci| cipher <- (getRandomBytes 32 :: IO ByteString) >>= (throwCryptoErrorIO . cipherInit) :: IO AES256
ghci| return $ ctrCombine cipher nullIV ("message"::ByteString)
ghci| 
"\208\207\SI\191\206\DELN"



ghci> import Crypto.Cipher.AESGCMSIV
ghci> do
ghci| key :: ByteString <- getRandomBytes 32
ghci| nonce <- generateNonce
ghci| throwCryptoErrorIO $ do
ghci| aes :: AES256 <- cipherInit key
ghci| return $ encrypt aes nonce ("context" :: ByteString) ("message" :: ByteString) 
ghci| 
(AuthTag {unAuthTag = "\239|\229V\USNT3\ACKf\NAK\STXC\251\134\FS"},"\149\229\142SW\209Z")


ghci> import Crypto.Error
ghci> import Crypto.Cipher.ChaChaPoly1305
ghci| do
ghci| key <- getRandomBytes 32 :: IO ByteString
ghci| nonce <- getRandomBytes 12 :: IO ByteString
ghci| throwCryptoErrorIO $ do
ghci| st1 <- nonce12 nonce >>= initialize key
ghci| let
ghci|   st2 = finalizeAAD $ appendAAD ("context":: ByteString) st1
ghci|   (out, st3) = encrypt ("message":: ByteString) st2
ghci|   auth = finalize st3
ghci| return $ (convertToBase Base16 out :: ByteString, convertToBase Base16 auth :: ByteString)
("f0dd593fb3cac0","4a29dd7ae8b51ac748b37092ed485e88")




ghci> import Crypto.PubKey.ECC.DH
ghci> import Crypto.PubKey.ECC.Types
ghci> let curve = getCurveByName SEC_p384r1
ghci> do
ghci| alicePrivateKey <- generatePrivate curve
ghci| let alicePublicKey = calculatePublic curve alicePrivateKey
ghci| bobPrivateKey <- generatePrivate curve
ghci| let bobPublicKey = calculatePublic curve bobPrivateKey
ghci| let aliceSharedKey = getShared curve alicePrivateKey bobPublicKey
ghci| let bobSharedKey = getShared curve bobPrivateKey alicePublicKey
ghci| return (aliceSharedKey == bobSharedKey)
ghci| 
True



do
alicePrivateKey <- generateSecretKey
let alicePublicKey = toPublic alicePrivateKey
bobPrivateKey <- generateSecretKey
let bobPublicKey = toPublic bobPrivateKey
let aliceSharedKey = dh bobPublicKey alicePrivateKey
let bobSharedKey = dh alicePublicKey bobPrivateKey
return (aliceSharedKey == bobSharedKey)




ghci> import Crypto.PubKey.ECC.ECDSA
ghci> import Crypto.PubKey.ECC.Generate
ghci> import Crypto.Hash.Algorithms
ghci> do
ghci| (alicePublicKey, alicePrivateKey) <- generate curve
ghci| toBob <- sign alicePrivateKey SHA256 ("message to Bob"::ByteString)
ghci| return $ verify SHA256 alicePublicKey toBob ("message to Bob"::ByteString)
ghci| 
True




ghci> import Crypto.PubKey.RSA
ghci> import Crypto.PubKey.RSA.PKCS15
ghci> import Crypto.Hash.Algorithms
ghci> (publicKey, privateKey) <- generate 256 65537
ghci> sign Nothing (Just SHA256) privateKey ("to Bob"::ByteString)
Right "=\161\243j\STX]\251g=\234\GSx>\159\248\128#\DEL\235\188\240\221U\232{\176\DLE\231\210\229z2\SI\212\212\216\235\239bO\205&t\248\SOH\249K.\191R\169\175#\183\f\239\141\142\201\144\&0\174[\ESC\228T`\136\239\221\EOT\214\235\&8\NAK\255\245l\148\228?\FS\254N'pO\221\"n'\249<\242\RS\146\DELl\210\157~[\254)\SYN@\249\232\ENQs\RSl\177u\213\207\239\129\159\ETX&\141\DC13\243\250\232\187\222\DLEF\207\222\231\EM\186M\245Hv\238\n\ACK\SOw\254\196\173&\209N\245\217\&4m\234\161l\158\173\225;\SYN\187\217\v\r\199\b\193\v\167{Dk\234\194\174\133\150@\148\DEL\190\193\141\DC1\140S\141\GS\135\n\161)\253\179\241\199\230k<G\135\158\225\DC2?h\211\240#;\183\192\182\168eS\\\195V\135mZ\rA\173\197\139\193\135\174\233\138&r\SIK\197\156\r\a\DELV\ETX\218\FS\188d\NAKw\222\252\ESC\SO"








--https://blog.oyanglul.us/devops/cryptography







--https://www.stephendiehl.com/posts/haskell_2017.html






--https://www.fpcomplete.com/blog/cryptographic-hashing-haskell/



--https://hackage.haskell.org/package/cpsa-2.2.7/src/doc/cpsasdha.pdf





























