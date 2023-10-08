
   

instance Eq Boek where 

  (==) p1 p2 = titel p1 == titel p2  

   

  

instance Ord Boek where 

   (<=) p1 p2 = titel p1 <= titel p2  

  

  

instance Ord Person where 

  compare p1 p2 = firstName p1 compare firstName p2  

   

   

  

data Boek = Boek 

  { prijs :: Double 

  , titel :: String 

  , auteur :: String 

   }  

     

main :: IO ()  

  

main = putStrLn ( "Start hier")  

  

-- let p1 = Boek 35.99 "Oorlog en vrede" "Tolstoj" 

-- let p2 = Boek 8.99 "Erotische gedichten" "Cicero" 

-- p1==p2 