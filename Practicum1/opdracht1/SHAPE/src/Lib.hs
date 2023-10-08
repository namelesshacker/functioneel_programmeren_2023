

module Main where



class Eq a => Ord a where

  compare :: a -> a -> Ordering

  (<) :: a -> a -> Bool

  (<=) :: a -> a -> Bool

  (>) :: a -> a -> Bool

  (>=) :: a -> a -> Bool

  max :: a -> a -> a

  min :: a -> a -> a



instance Ord Boek where

   compare p1 p2 = titel compare <= titel p2



data Person = Person

  { firstName :: String

  , lastName :: String

  , age :: Int

  , hometown :: String

  , homestate :: String }
main :: IO ()

main = putStrLn ( "Start hier")
