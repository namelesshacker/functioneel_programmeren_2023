{-
Opdracht 1
Schrijf het datatype Boek. Dit datatype bevat drie velden:
 Prijs: De prijs van het boek.
 Titel: De titel van het boek.
 Auteur: De schrijver van het boek.
Gebruik type synoniemen om de code leesbaar te houden.

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

