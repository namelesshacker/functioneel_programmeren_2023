{-
Opdracht 2
Leidt je Boek af van de typeclasses Eq en Ord. We willen boeken onderling
kunnen vergelijken (Eq) en op volgorde kunnen zetten (Ord). Voorwaarden:
 Twee boeken zijn aan elkaar gelijk als alle velden gelijk zijn.
 Boeken dienen alfabetisch gesorteerd te kunnen worden op de titel van
het boek. Prijs en auteur intesseren ons niet.

-}

{-
module Main (main) where

import MIMA



--rest = print $ (euclid 2 3)
main :: IO ()
main = print $ (euclid 36 15)

-}

--C:\Users\gally\Documents\FuncProg\Practicum2\Opdracht1a
--fact n = if n == 0 then 1 else n * fact(n-1)
--main = print (fact 5)


 


--ghc --make -XQuasiQuotes main.hs -o main


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
