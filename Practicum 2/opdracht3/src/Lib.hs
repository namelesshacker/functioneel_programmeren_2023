

main :: IO ()  

main = putStrLn ( "Start hier")  

  

-- let p1 = Boek 35.99 "Oorlog en vrede" "Tolstoj" 

-- let p2 = Boek 8.99 "Erotische gedichten" "Cicero" 

-- p1==p2 

--let p_1 = Person "erik" "Richarlison" 14 "Rotterdam" "Nederland" 

-- let p_2 = Person "Leen" "Rucksigloos" 14 "Hamburg" "Deutslacnd" 

-- sort [p_1, p_2]





:{

--import BasicPrelude

--module Main where









data Person = Person

  { firstName :: String

  , lastName :: String

  , age :: Int

  , hometown :: String

  , homestate :: String } deriving (Eq, Show)



instance Ord Person where

  compare p1 p2 = if ageComparison == EQ

    then if lastNameComparison == EQ

      then lastName p1 `compare` lastName p2

      else lastNameComparison

    else ageComparison

    where

      ageComparison = (age p1) `compare` (age p2)

      lastNameComparison = (lastName p1) `compare` (lastName p2)



main :: IO ()

main = putStrLn ( "Start hier")



:}





data Person = Person

  { firstName :: String

  , lastName :: String

  , age :: Int

  , hometown :: String

  , homestate :: String } deriving (Eq)



instance Ord Person where

  compare p1 p2 = if ageComparison == EQ

    then if lastNameComparison == EQ

      then lastName p1 `compare` lastName p2

      else lastNameComparison

    else ageComparison

    where

      ageComparison = (age p1) `compare` (age p2)

      lastNameComparison = (lastName p1) `compare` (lastName p2)



instance Show Person where

  show p1 = (firstName p1) ++ " "

    ++ (lastName p1) ++ ", age: "

    ++ (show (age p1)) ++ ", born in: "

    ++ (hometown p1) ++ ", "

    ++ (homestate p1)





main :: IO ()

main = putStrLn ( "Start hier")



   --let p_2 = Person "Dierik" "Rucksigloos" 45 "Hamburg" "Amerika"

  -- let p_3 = Person "Johanna" "Rucksigloos" 75 "Hamburg" "Griekenland"

  -- sort [p_1, p_2, p_3]




-- werkend voor haskell online IDE voor oop persoon object



import Data.List

data Person = Person

  { firstName :: String

  , lastName :: String

  , age :: Int

  , hometown :: String

  , homestate :: String } deriving (Eq)



instance Ord Person where

  compare p1 p2 = if ageComparison == EQ

    then if lastNameComparison == EQ

      then lastName p1 `compare` lastName p2

      else lastNameComparison

    else ageComparison

    where

      ageComparison = (age p1) `compare` (age p2)

      lastNameComparison = (lastName p1) `compare` (lastName p2)



instance Show Person where

  show p1 = (firstName p1) ++ " "

    ++ (lastName p1) ++ ", age: "

    ++ (show (age p1)) ++ ", born in: "

    ++ (hometown p1) ++ ", "

    ++ (homestate p1)





main :: IO ()

main = putStrLn ( "Start hier")



   --let p_2 = Person "Dierik" "Rucksigloos" 45 "Hamburg" "Amerika"

  -- let p_3 = Person "Johanna" "Rucksigloos" 75 "Hamburg" "Griekenland"

  -- sort [p_1, p_2, p_3]




module Main where


data Person = Person

  { firstName :: String

  , lastName :: String

  , age :: Int

  , hometown :: String

  , homestate :: String } deriving (Eq, Show)



instance Ord Person where

  compare p1 p2 = if ageComparison == EQ

    then if lastNameComparison == EQ

      then lastName p1 `compare` lastName p2

      else lastNameComparison

    else ageComparison

    where

      ageComparison = (age p1) `compare` (age p2)

      lastNameComparison = (lastName p1) `compare` (lastName p2)

main :: IO ()

main = putStrLn ( "Start hier")



module Main where

data Person = Person

  { firstName :: String

  , lastName :: String

  , age :: Int

  , hometown :: String

  , homestate :: String } deriving (Eq, Show)



instance Ord Person where
  compare p1 p2 = firstName p1 compare firstName p2

main :: IO ()

main = putStrLn ( "Start hier")

   Let p_1 = Person "erik" "Richarlison" 14 "Rotterdam" "Nederland"

   let p_1 = Person "erik" "Richarlison" 14 "Rotterdam" "Nederland"

   let p_2 = Person "Leen" "Rucksigloos" 14 "Hamburg" "Deutslacnd"

   sort [p_1, p_2]





module Main where

class Eq a => Ord a where

  compare :: a -> a -> Ordering

  (<) :: a -> a -> Bool

  (<=) :: a -> a -> Bool

  (>) :: a -> a -> Bool

  (>=) :: a -> a -> Bool

  max :: a -> a -> a

  min :: a -> a -> a


data Person = Person

  { firstName :: String

  , lastName :: String

  , age :: Int

  , hometown :: String

  , homestate :: String }



instance Show Person where

  show p1 = (firstName p1) ++ " "

    ++ (lastName p1) ++ ", age: "

    ++ (show (age p1)) ++ ", born in: "

    ++ (hometown p1) ++ ", "

    ++ (homestate p1)



data Foo = Foo {x :: Integer, str :: String}

    deriving (Eq, Ord, Show)


main :: IO ()

main = putStrLn ( "Start hier")



--let p1 = Person "John" "Doe" 23 "El Paso" "Texas"

--let p2 = Person "Jane" "Doe" 24 "Houston" "Texas"

--let p3 = Person "Patrick" "Green" 22 "Boston" "Massachusetts"

--sort [p1, p2, p3]




  module Main where





data Foo = Foo {prijs :: Integer, titel :: String,auteur :: String} deriving (Eq, Show)



instance Ord Foo where

  compare p1 p2 = if ageComparison == EQ

    then if lastNameComparison == EQ

      then titel p1 `compare` titel p2

      else lastNameComparison

    else ageComparison

    where

      ageComparison = (prijs p1) `compare` (prijs p2)

      lastNameComparison = (auteur p1) `compare` (auteur p2)

main :: IO ()

main = putStrLn ( "Start hier")




data Person =



  Adult



    { Prijs :: Double



    , Titel :: String



    , Auteur :: String



    } |



  Child



    { Prijs :: Double



    , Titel :: String



    , Auteur :: String



    }
