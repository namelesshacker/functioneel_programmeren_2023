module Opdracht2
where

--OPDRACHT 1
type Prijs = Double
type Titel = String
type Auteur = String

data Boek = Boek {
                    prijs :: Prijs,
                    titel :: Titel,
                    auteur :: Auteur
                 } deriving (Show, Eq)

--OPDRACHT 2
instance Ord Boek where
  a > b = titel a > titel b
  a >= b = titel a >= titel b
  a < b = titel a < titel b
  a <= b = titel a <= titel b

--OPDRACHT 3  
boekenLijst = [(Boek 15.00 "The Alchemist" "Paulo Coelho"),
               (Boek 17.00 "Garden of Truth" "Ruth Chou Simons"),
               (Boek 29.00 "The Fallen" "David Baldacci"),
               (Boek 20.00 "War Storm" "Victoria Aveyard"),
               (Boek 30.00 "The Great Alone" "Kristin Hannah")]

--OPDRACHT 4  
data Box a = EmptyBox 
           | Box a 
           deriving (Show)

--OPDRACHT 5
inpakken b = map (Box) b
uitpakken b = map (\(Box a) -> a) b

--OPDRACHT 6
data Zak a = EmptyZak 
           | Zak a 
           deriving (Show)
   
--OPDRACHT 7
instance Functor Box where
  fmap f (Box x) = Box (f x)
  fmap f EmptyBox = EmptyBox

instance Functor Zak where
  fmap f (Zak x) = Zak (f x)
  fmap f EmptyZak = EmptyZak
  
--OPDRACHT 8





--OPDRACHT 9
data List a = EmptyList 
            | Cons a (List a) 
            deriving (Show, Eq, Ord)

instance Functor List where
  fmap f EmptyList = EmptyList
  fmap f (Cons a listTail) = Cons (f a) (fmap f listTail)
  
boxMetGetallen = (Box) <$> (foldr (Cons) EmptyList [1..10])
zakMetGetallen = (\(Box b) -> (Zak b)) <$> boxMetGetallen