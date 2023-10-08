


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





class Tree t where



    nil   :: t a



    node  :: t a -> a -> t a -> t a







class  Enum a  where



    succ, pred     :: a -> a



    toEnum         :: Int -> a



    fromEnum       :: a -> Int



    enumFrom       :: a -> [a]            -- [n..]



    enumFromThen   :: a -> a -> [a]       -- [n,n'..]



    enumFromTo     :: a -> a -> [a]       -- [n..m]



    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]







addThree :: Int -> Int -> Int -> Int



addThree x y z = x + y + z







main :: IO ()



main = print ( addThree 1 3 13)




