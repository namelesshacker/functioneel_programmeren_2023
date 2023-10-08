


{-
--opgave 7
--data Shape = Square {length:: Float, color:: Color}
--          | Rectangle {length:: Float, widht:: Float, color :: Color}

--getByColor :: Color -> [Shape] -> [Shape]
--getByColor colorParam shapes = filter (\shape -> color shape == colorParam) shapes


--database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
 --   (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
--    (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]
https://stackoverflow.com/questions/22935071/filtering-list-based-on-items-with-record-syntax
-}
 
 

data Kleur = Yellow | Green | Blue
            deriving (Eq, Show)

data Shape = Circle Float Kleur |
         Rectangle Float Kleur |
         Triangle Float  Kleur

database :: [Shape]
database = [(Circle 6 Green), (Circle 7 Green), (Circle 7 Green),
    (Rectangle 5 Blue), (Rectangle 6 Blue),(Rectangle 2 Blue)]



--data Kleur = Yellow | Green | Blue

data Figuur = Square {length:: Float, color:: Kleur}
           | Rect {length:: Float, widht:: Float, color :: Kleur}
           deriving (Eq, Show)

figures = [(Square 6 Green), (Square 7 Green), (Square 7 Green),
    (Square 5 Blue), (Square 6 Blue),(Square 2 Blue)]

getByColor :: Kleur -> [Figuur] -> [Figuur]
getByColor colorParam shapes = filter ((== colorParam) . color) shapes


--let temp =Square 12 Yellow
main = print "Hello"


{-
Bijlage


import Data.Set

module Main where





data Boek = Boek { prijs :: Int

                     , titel :: String

                     , auteur :: String

                     } deriving (Ord, Eq, Show)



instance Eq Boek where

   prijs == prijs = True

   auteur == auteur =true

   titel == titel = true



instance Ord Boek where

    compare x y = compare (prijs x) (prijs y)





main = do

        print "hello"





let mikeD = Boek {auteur = "Michael", titel = "Diamond", prijs = 43}

let mikeC = Boek {auteur = "Shabby", titel = "Naf Sranang", prijs = 14}


Bijlage


module Main where





data Boek = Boek { prijs :: Int

                     , titel :: String

                     , auteur :: String

                     } deriving ( Show)



instance Eq Boek where

   (==) :: Boek -> Boek -> Bool











instance Ord Boek where

  compare x y = compare (prijs x) (prijs y)



comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering

comparing p x y = compare (p x) (p y)



main = do

        print "hello"

-}