module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

fact n = if n == 0 then 1 else n * fact(n-1)


{-


Opdracht 2d
Schrijf de functie:
integreer::(Double->Double)->Double->Double->Double->Double
integreer f a b p
Deze functie integreert de functie f op het interval a, b met een precisie p.
• Hint 1: bestudeer wat men verstaat onder Riemannintegratie.
• Hint 2: kies voor p een klein getal, maar niet te klein, bijv. 1
10.000
uitwerking

Voorbeeld 1

functie a = do
x <- a
let y = x*x
let z = y+x
return (2*z)


functie = do
putStrln "Hallo"
return "Doei..."


schelden = do
a <- getLine >>= (\x -> return (map toUpper x))
return a



module Main where
import Control.Monad
import System.IO
main = do
putStrLn "Geef de naam van het bestand"
inp <- getLine
bestand<- readFile inp
putStrLn (take 100 bestand)

class Functor f where
fmap :: (a -> b) -> f a -> f b

kwadraat x = return (x*x)
wortel x = return (sqrt x)

class (Functor f) => Applicative f where
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

som::Int->Int->Int
som x y = x+y


import Control.Monad.Cont

main :: IO ()
main = do
  putStrLn "Start"
  let a=3
  let b=3
  let n=10
  let  width = (b-a)/n
  let som x y = x+y
  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      let first_mid_p=a+(width/2.0) + i * width
      --som += som + (first_mid_p*first_mid_p-first_mid_p+3)
      liftIO . putStrLn $ "Loop: " ++ show i
      when (i == n) $ break ()
  let returnValue = som * width
  putStrLn "End"
  where
    withBreak = flip runContT pure . callCC


import Control.Monad (unless)

lorem_ipsum =
    let i = 10
        x = 2
    in loop i x []
    where
      loop i x result =
        if not (i > 0) then reverse result
        else
          let result' = (i * x: result)
              i' = i - 1
          in
           loop i' x result'
lorem_ipsum2 =
    loop 10 2 []
    where
      loop 0 _ acc = reverse acc
      loop i x acc = loop (i - 1) x (x * i: acc)
intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))
   print (lorem_ipsum)



import Control.Monad.Cont



main :: IO ()
main = do
  putStrLn "Start"
  let a=3
  let b=3
  let n=10
  let  width = (b-a)/n
  let som =0.0
  let j = som
  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      let first_mid_p=a+(width/2.0) + i * width
      let som = j+(first_mid_p*first_mid_p-first_mid_p+3)
      --som += som + (first_mid_p*first_mid_p-first_mid_p+3)
      liftIO . putStrLn $ "Loop: " ++ show first_mid_p
      when (i == n) $ break ()
  print som
  putStrLn "End"
  where
    withBreak = flip runContT pure . callCC





evensom:: Int->Int->Bool
evensom x y = if even (x+y) then True else False

deel::(Floating x)=>x->x->x
deel x y = x/y

x onderdeel van de typeclass Floating:
deel::(Floating x)=>x->x->x
deel x y = x/y
x gedefinieerd als concrete typen:
deel::Float->Float->Float
deel x y = x/y
x onderdeel van de typeclass Integral
deel::(Integral x)=>x->x->x
deel x y = x ‘div‘ y
Onmogelijk (waarom):
deel::(Integral x)=>x->x->x
deel x y = x/y


oop :: Int -> Int
loop n = loop' n 0
  where loop' 0 a = a
        loop' n a = loop' (n - 1) (1 - a)

		for i in 1..3
  puts "Inside the times method."
end




intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))


import Control.Monad (unless)

intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0
   for (x:xs) f = do
     f x
     unless (null xs) $ for xs f
   for [1..10] (\i -> putStrLn ("Hello: " ++ show i))
    returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))



import Control.Monad.Cont



main :: IO ()
main = do
  putStrLn "Start"
  let a=3
  let b=3
  let n=10
  let  width = (b-a)/n
  let som =0.0
  let j = som
  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      let first_mid_p=a+(width/2.0) + i * width
      --i <- return (i+1)
      som  <- return (first_mid_p*first_mid_p-first_mid_p+3)
      --som += som + (first_mid_p*first_mid_p-first_mid_p+3)
      liftIO . putStrLn $ "Loop: " ++ show som
      when (i == n) $ break ()
  print som
  putStrLn "End"
  where
    withBreak = flip runContT pure . callCC


import Control.Monad (unless)
import Control.Monad.Cont


helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

tester = do
  putStrLn "Start"
  let a=3
  let b=3
  let n=10
  let  width = (b-a)/n
  let som =0.0
  let j = som
  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      let first_mid_p=a+(width/2.0) + i * width
      --i <- return (i+1)
      som  <- return (first_mid_p*first_mid_p-first_mid_p+3)
      --som += som + (first_mid_p*first_mid_p-first_mid_p+3)
      liftIO . putStrLn $ "Loop: " ++ show som
      when (i == n) $ break ()
  print som
  putStrLn "End"
  where
    withBreak = flip runContT pure . callCC


roots''' a b c =
  let
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d  = sqrt (b ^ 2 - 4 * a * c)
    aTwice = a * 2
  in (x1, x2)



intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))




Voorbeeld 2
import Control.Monad (unless)
import Control.Monad.Cont




tester :: (Int, Int, Int) -> (Int)
tester a b n = (som) where
  a=3
  let b=3
  let nn=10
  let  width = (b-a)/n
  let som =0.0
  let j = som
  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      let first_mid_p=a+(width/2.0) + i * width
      --i <- return (i+1)
      --som  <- return (first_mid_p*first_mid_p-first_mid_p+3)
      let uitkomst = (first_mid_p*first_mid_p-first_mid_p+3)
      let som =  j+  uitkomst
      liftIO . putStrLn $ "Loop: " ++ show som
      when (i == n) $ break ()
  where
    withBreak = flip runContT pure . callCC


roots''' a b c =
  let
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d  = sqrt (b ^ 2 - 4 * a * c)
    aTwice = a * 2
  in (x1, x2)



intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))





Voorbeeld 3


import Control.Monad (unless)
import Control.Monad.Cont

helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

tester = do
  putStrLn "Start"
  let a=3
  let b=3
  let n=10
  let  width = (b-a)/n
  let som =0.0
  let j = som
  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      let first_mid_p=a+(width/2.0) + i * width
      --i <- return (i+1)
      som  <- return (first_mid_p*first_mid_p-first_mid_p+3)
      --som += som + (first_mid_p*first_mid_p-first_mid_p+3)
      liftIO . putStrLn $ "Loop: " ++ show som
      when (i == n) $ break ()
  print som
  putStrLn "End"
  where
    withBreak = flip runContT pure . callCC


roots''' a b c =
  let
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d  = sqrt (b ^ 2 - 4 * a * c)
    aTwice = a * 2
  in (x1, x2)



intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))

   Voorbeeld 4
import Control.Monad (unless)
import Control.Monad.Cont


helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)


tester a b n = do
  putStrLn "Start"
  let a=3
  let b=3
  let n=10
  let  width = (b-a)/n
  let som =0.0
  let j = som
  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      let first_mid_p=a+(width/2.0) + i * width
      --i <- return (i+1)
      --som  <- return (first_mid_p*first_mid_p-first_mid_p+3)
      let uitkomst = (first_mid_p*first_mid_p-first_mid_p+3)
      let som =  j+  uitkomst
      liftIO . putStrLn $ "Loop: " ++ show som
      when (i == n) $ break ()
  where
    withBreak = flip runContT pure . callCC


roots''' a b c =
  let
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d  = sqrt (b ^ 2 - 4 * a * c)
    aTwice = a * 2
  in (x1, x2)



intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))


Voorbeeld 5

import Control.Monad (unless)
import Control.Monad.Cont


for list action = mapM_ action list

simpleFunction a b n =  do
    let width = (b - a) / n
    let som = 0.0

    let returnVal = som * width
    for [0..10] (\ i -> do

        print(i^2)
        )
    return (22)


helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

dain a b c = do
  putStrLn "Start"

  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      liftIO . putStrLn $ "Loop: " ++ show i
      when (i == 5) $ break ()

  putStrLn "End"
  return 22
  where
    withBreak = flip runContT pure . callCC

nameReturn :: IO String
nameReturn = do putStr "What is your first name? "
                first <- getLine
                putStr "And your last name? "
                last <- getLine
                let full = first ++ " " ++ last
                putStrLn ("Pleased to meet you, " ++ full ++ "!")
                return full

roots''' a b c =
  let
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d  = sqrt (b ^ 2 - 4 * a * c)
    aTwice = a * 2
  in (x1, x2)



intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

main = do
   putStrLn "The roots of our intRiemann are:"
   print (intRiemann(1,-8,6))
   third = (simpleFunction 1 2 3)
Voorbeeld 6

import Control.Monad (unless)
import Control.Monad.Cont


for list action = mapM_ action list

simpleFunction a b n =  do
    let width = (b - a) / n
    let som = 0.0

    let returnVal = som * width
    for [0..10] (\ i -> do

        print(i^2)
        )
    return (22)





helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

dain a b c = do
  putStrLn "Start"

  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      liftIO . putStrLn $ "Loop: " ++ show i
      when (i == 5) $ break ()

  putStrLn "End"
  return 22
  where
    withBreak = flip runContT pure . callCC

nameReturn :: IO String
nameReturn = do putStr "What is your first name? "
                first <- getLine
                putStr "And your last name? "
                last <- getLine
                let full = first ++ " " ++ last
                putStrLn ("Pleased to meet you, " ++ full ++ "!")
                return full

roots''' a b c =
  let
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d  = sqrt (b ^ 2 - 4 * a * c)
    aTwice = a * 2
  in (x1, x2)

oppervlakenstraal r =
    let op = pi*r^2
        om = 2*pi*r
    in (op,om)

intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

simpleFunction3 a b n =  do
    let width = (b - a) / n
    let som = 0.0
        j = som

    for [0..n] (\ i -> do

        let first_mid_p=a + (width / 2.0) + i * width;
        --print  first_mid_p
        som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))
        --print som

        return som
        )
    let returnVal = som * width
    print returnVal
    print j


loop :: Int -> (IO()) -> IO()
loop 0 _ = return ()
loop n f =
 do
  f
  loop (n - 1) f

exmple = do
 loop 5 (do
  putStr "hello "
  putStrLn "there")

main = do
   putStrLn "The roots of our intRiemann are:"
   --print (intRiemann(1,-8,6))
   let rest = (simpleFunction3 1 2 3)
   rest
   --let opp = oppervlakenstraal 3
   --print "oppervlaktestraal"
   --print opp


Voorbeeld 7

import Control.Monad (unless)

for list action = mapM_ action list

loop :: Int -> (IO()) -> IO()
loop 0 _ = return ()
loop n f =
 do
  f
  loop (n - 1) f

example = do
 loop 5 (do
  putStr "hello "
  putStrLn "there")

main :: IO Int
main = do
    for [0..10] (\ i -> do

        print(i^2)
        )
    example
    return 0

Voorbeeld 8

import Control.Monad (unless)

for list action = mapM_ action list

loop :: Int -> (IO()) -> IO()
loop 0 _ = return ()
loop n f =
 do
  f
  loop (n - 1) f


iter = 0.0
b=2
a=5
n=3
width = (b - a) / n
som =0.0
j=som
example = do
 loop 5 (do


  --print iter
  let first_mid_p=a + (width / 2.0) + iter * width;
  som <- return(j + (first_mid_p*first_mid_p-first_mid_p+3))
  print som
  )



data Nat = Zero | Succ Nat
natToInt Zero = 0
natToInt (Succ n) = natToInt n + 1

factorial 1 = 1
factorial n = n * factorial (n-1)

lst = [2,3,5,7,11]

total = sum (map (3*) lst)

main = print total

simpleFunction3 a b n =  do
    let width = (b - a) / n
    let som = 0.0
        j = som

    for [0..n] (\ i -> do

        let first_mid_p=a + (width / 2.0) + i * width;
        --print  first_mid_p
        som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))
        --print som

        return som
        )
    let returnVal = som * width
    print returnVal
    print j


main :: IO Int
main = do
    for [0..10] (\ i -> do

        print(i^2)
        )
    example
    simpleFunction3 1 2 3
    return 0

Voorbeeld 9

import Control.Monad (unless)
import Control.Monad.Cont


for list action = mapM_ action list

simpleFunction a b n =  do
    let width = (b - a) / n
    let som = 0.0

    let returnVal = som * width
    for [0..10] (\ i -> do

        print(i^2)
        )
    return (22)





helper acc 0 = acc
helper acc n = helper (acc * n) (n - 1)

dain a b c = do
  putStrLn "Start"

  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      liftIO . putStrLn $ "Loop: " ++ show i
      when (i == 5) $ break ()

  putStrLn "End"
  return 22
  where
    withBreak = flip runContT pure . callCC

nameReturn :: IO String
nameReturn = do putStr "What is your first name? "
                first <- getLine
                putStr "And your last name? "
                last <- getLine
                let full = first ++ " " ++ last
                putStrLn ("Pleased to meet you, " ++ full ++ "!")
                return full

roots''' a b c =
  let
    x1 = (-b - d) / aTwice
    x2 = (-b + d) / aTwice
    d  = sqrt (b ^ 2 - 4 * a * c)
    aTwice = a * 2
  in (x1, x2)

oppervlakenstraal r =
    let op = pi*r^2
        om = 2*pi*r
    in (op,om)

intRiemann :: (Float, Float, Float) -> (Float)
intRiemann (a,b,n) = (returnVal) where
   width = (b - a) / n
   som = 0.0

   returnVal = som * width

simpleFunction3 a b n =  do
    let width = (b - a) / n
    let som = 0.0
        j = som

    for [0..n] (\ i -> do

        let first_mid_p=a + (width / 2.0) + i * width;
        --print  first_mid_p
        som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))
        --print som

        return som
        )
    let returnVal = som * width
    print returnVal
    print j


loop :: Int -> (IO()) -> IO()
loop 0 _ = return ()
loop n f =
 do
  f
  loop (n - 1) f

exmple = do
 loop 5 (do
  putStr "hello "
  putStrLn "there")

main = do
   putStrLn "The roots of our intRiemann are:"
   --print (intRiemann(1,-8,6))
   let rest = (simpleFunction3 1 2 3)
   rest
   --let opp = oppervlakenstraal 3
   --print "oppervlaktestraal"
   --print opp





Voorbeeld 10

import Control.Monad (unless)
import Control.Monad.Cont

roots :: (Float, Float, Float) -> (Float, Float)
roots (a,b,c) = (x1, x2) where
   x1 = e + sqrt d / (2 * a)
   x2 = e - sqrt d / (2 * a)
   d = b * b - 4 * a * c
   e = - b / (2 * a)

loop :: Int -> (IO()) -> IO()
loop 0 _ = return ()
loop n f =
 do
  f
  loop (n - 1) f

let iter =0
example = do
  loop 5 (do
  print iter
  iter <- return (iter +1)
  print iter
  )

dain a b c = do
  putStrLn "Start"

  withBreak $ \break ->
    forM_ [1..] $ \i -> do
      liftIO . putStrLn $ "Loop: " ++ show i
      when (i == 5) $ break ()

  putStrLn "End"
  return 22
  where
    withBreak = flip runContT pure . callCC

for (x:xs) f = do
    f x
    unless (null xs) $ for xs f
for [1..10] (\i -> putStrLn ("Hello: " ++ show i))

functie a = do
    x <- a
    let y = x*x
    let z = y+x
    withBreak $ \break ->
      forM_ [1..] $ \i -> do
        liftIO . putStrLn $ "Loop: " ++ show i
        when (i == 5) $ break ()

    putStrLn "End"
    where
      withBreak = flip runContT pure . callCC


main = do
   putStrLn "The roots of our Polynomial equation are:"
   print (roots(1,-8,6))

Voorbeld 11

import Control.Monad (unless)

for list action = mapM_ action list

let x = for i in [1, 4, 3, 2] {

    x = i + 10
}

[ x+5 | x <- [1,2,3] ]

Voorbeeld 12


import Control.Monad (unless)

for list action = mapM_ action list
loop :: Int -> IO ()

simpleFunction3 a b n =  do
    let width = (b - a) / n
    let som = 0.0
        j = som

    for [0..n] (\ i -> do

        let first_mid_p=a + (width / 2.0) + i * width;
        --print  first_mid_p
        som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))
        --print som

        return som
        )

    let returnVal = som * width
    print returnVal
    print j


loop  n = do
    if n <=5
    then do
        putStrLn (show n)

        loop (n + 1)
    else
        return ()



main :: IO ()
main = do
    loop 0
    print "finished"

Voorbeeld 13

import Control.Monad (unless)
import Data.Char (digitToInt) -- we'll need ord shortly
for list action = mapM_ action list
loop :: Int -> IO ()

bar = let a = 1
          b = 2
          c = 3
      in a + b + c

foo = let { a = 1;  b = 2; som =0.0;
        n = 3 ;width = (b - a) / n ;}

      in a + b + n

signum x =
    if x < 0
      then -1
      else if x > 0
        then 1
        else 0

application n = do
    for [0..n] (\ i -> do

        print i
        )
    print "finished"
simpleFunction3 a b n =  do
    let width = (b - a) / n
    let som = 0.0
        j = som

    for [0..n] (\ i -> do

        let first_mid_p=a + (width / 2.0) + i * width;
        --print  first_mid_p
        som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))
        --print som

        return som
        )
    let returnVal = som * width
    print returnVal
    print j

        --let first_mid_p=a + (width / 2.0) + i * width;
        --print  first_mid_p
      --  som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))
        --print som



--asInt :: String -> Int
--loop :: Int -> String -> Int
--asInt xs = loop 0 xs
--loop acc [] = acc
--loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
--                  in loop acc' xs

fact :: Integer ->Integer -> Integer
fact n som | n == 0 = 1
           | n /= 0 = n * fact (n-1) som

inner = let { a = 1;  b = 2; som =0.0;
        n = 3 ;width = (b - a) / n ;}

      in a + b + n

innerWidth_p a b n = (b - a) / n ;
innerWidth = (innerWidth_p 1 2 3)
resultsom a b 3 = a+b*b-b+3
som = 0.0
loop  n = do
    if n <=5
    then do
        putStrLn (show som)
        let {this_a=1;this_b=3;this_n=10; innerWidth = (innerWidth_p this_a this_b this_n); }
        let som = (resultsom this_a innerWidth );
        putStrLn (show innerWidth)
        loop (n + 1)
    else
        return ()

while :: Int -> Int -> (Int -> Int) -> Int
while start number f
  | val >= number = val
  | otherwise = while (start + 1) number f
    where
      val = f start

data Nat = Zero | Succ Nat
natToInt Zero = 0
natToInt (Succ n) = natToInt n + 1


main = do
    print foo
    (loop 1)

    print "finished"

Voorbeeld 14


import Control.Monad (unless)
import Data.Char (digitToInt) -- we'll need ord shortly
for list action = mapM_ action list

loop :: Int -> Int -> IO ()

loop n result = do
    if n <= 10
    then do
        let result = (n*n)
        putStrLn (show (result))
        loop (n + 1) result
    else
        return ()

final_s = f 0 0  -- the initial values
  where
  f i s | i <=5   = f (i+1) (s+i)  -- increment i, augment s
        | otherwise = s              -- return s at the end


let x = for i in [1..n] {

     i + 10
}
main :: IO ()
main = do
    let outcome =(loop 1 0)

    outcome

Voorbeeld 15
import Control.Monad (when)
for list action = mapM_ action list


loop :: Int -> Int -> IO ()
exaple :: Int  -> IO ()
--foo :: Int  ->  IO ()
loop n result = do
    if n <= 10
    then do
        let result = (n*n)
        putStrLn (show (result))
        loop (n + 1) result
    else
        return ()

final_s = f 0 0  -- the initial values
  where
  f i s | i <=5   = f (i+1) (s+i)  -- increment i, augment s
        | otherwise = s              -- return s at the end

exaple n =
  do
    if n <= 10
      then do
        putStrLn "7 is even"
        let a = n*2
        exaple (n + 1)
      else  putStrLn "7 is even"

--if foo a then bar b else baz c

odds n = for [0..n-1](\ i -> do

        return i
        )

foo  = do
         let func x = x + 1
         --mapM_ func aList
         let a = func 4
         return a

function aList aValue = do
    mapM_ func aList
    return aValue
    where func x = x + 1
--test = \x.\then.\else. x then else


test0 :: IO Int
test0 = do
  let a =6

  return a

main :: IO ()
main = do

    --exaple 9
    --test0
    --let rest = (foo 4)
    --print rest
    let outcome =(loop 1 6)
    outcome

Voorbeeld 16



let k = for i in &mut v {
    if i == x { break }
    i
} noloop {
    x
}

let x = while i < 10 {
    if i == 6 {
        break "found";  // x == "found"
    }
    if i == 3 {
        return Err("i was odd <= 3"); // x will never be set
    }
    if i == -1 {
        panic!("negative numbers!"); // x will never be set
    }
    i += 2;
} !break {
    // only run if `break` never returned a value in above
    // loop (including if the loop was never entered)
    "not found" // x == "not found"
};


let x = if x > 10 {
    "small"
} else {
    "big"
};



let x = for i in [1, 4, 3, 2] {

    x = i + 10
}



let x = for i in [1, 4, 3, 2] {

    x = i + 10
} noloop {
    0
}

 let x = for i in [1, 4, 3, 2] {
    if i == 6 {
        break 6;
    }
    i + 10
} noloop {
    0
}


main = do
   putStrLn "The roots of our Polynomial equation are:"

Voorbeeld 17

import Control.Monad
import Control.Monad (unless)
import Data.Char (digitToInt) -- we'll need ord shortly
import Data.Foldable
for list action = mapM_ action list

loop :: Int -> Int -> IO ()

loop n result = do
    if n <= 10
    then do
        let result = (n*n)
        putStrLn (show (result))
        loop (n + 1) result
    else
        return ()

final_s = f 0 0  -- the initial values
  where
  f i s | i <=5   = f (i+1) (s+i)  -- increment i, augment s
        | otherwise = s              -- return s at the end

		-- https://stackoverflow.com/questions/42580085/integration-with-riemann-sum-midpoint-java
		-- https://stackoverflow.com/questions/1452620/unable-to-compute-product-quotient-and-difference-in-java
a=2
b=3
n=3
width = (b - a) / n
som = 0.0
count size = do
    let l = [0..size]
        iter acc element = do
            --putStrLn $ "Executing side effect " ++ show element
            let first_mid_p=a + (width / 2.0) + element * width;
            som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))

            return (acc + som)
    total <- foldM iter 0 l
    putStrLn $ "Total is " ++ show total

main :: IO ()
main = do
    let outcome =(loop 1 0)

    outcome
    count 6

Vooreeld 18
import Control.Monad (when)
for list action = mapM_ action list


loop :: Int -> Int -> IO ()
exaple :: Int  -> IO ()
--foo :: Int  ->  IO ()
loop n result = do
    if n <= 10
    then do
        let result = (n*n)
        putStrLn (show (result))
        loop (n + 1) result
    else
        return ()

final_s = f 0 0  -- the initial values
  where
  f i s | i <=5   = f (i+1) (s+i)  -- increment i, augment s
        | otherwise = s              -- return s at the end

exaple n =
  do
    if n <= 10
      then do
        putStrLn "7 is even"
        let a = n*2
        exaple (n + 1)
      else  putStrLn "7 is even"

--if foo a then bar b else baz c

odds n = for [0..n-1](\ i -> do

        return i
        )

foo  = do
         let func x = x + 1
         --mapM_ func aList
         let a = func 4
         return a

function aList aValue = do
    mapM_ func aList
    return aValue
    where func x = x + 1
--test = \x.\then.\else. x then else


test0 :: IO Int
test0 = do
  let a =6

  return a

main :: IO ()
main = do

    --exaple 9
    --test0
    --let rest = (foo 4)
    --print rest
    --let outcome =(loop 1 6)
    --outcome

Voorbeeld 19

import Control.Monad
import Control.Monad (unless)
import Data.Char (digitToInt) -- we'll need ord shortly
import Data.Foldable
for list action = mapM_ action list

loop :: Int -> Int -> IO ()

loop n result = do
    if n <= 10
    then do
        let result = (n*n)
        putStrLn (show (result))
        loop (n + 1) result
    else
        return ()

final_s = f 0 0  -- the initial values
  where
  f i s | i <=5   = f (i+1) (s+i)  -- increment i, augment s
        | otherwise = s              -- return s at the end

a=2
b=3
n=3
width = (b - a) / n
som = 0.0
count size = do
    let l = [0..size]
        iter acc element = do
            --putStrLn $ "Executing side effect " ++ show element
            let first_mid_p=a + (width / 2.0) + element * width;
            som <- return(som + (first_mid_p*first_mid_p-first_mid_p+3))

            return (acc + som)
    total <- foldM iter 0 l
    putStrLn $ "Total is " ++ show total

main :: IO ()
main = do
    let outcome =(loop 1 0)

    outcome
    count 6

-}
