
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