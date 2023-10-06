{-
Opdracht 2
Leidt je Boek af van de typeclasses Eq en Ord. We willen boeken onderling
kunnen vergelijken (Eq) en op volgorde kunnen zetten (Ord). Voorwaarden:
 Twee boeken zijn aan elkaar gelijk als alle velden gelijk zijn.
 Boeken dienen alfabetisch gesorteerd te kunnen worden op de titel van
het boek. Prijs en auteur intesseren ons niet.

-}
module Main (main) where

import MIMA

--rest = print $ (euclid 2 3)
main :: IO ()
main = print $ (euclid 36 15)

--C:\Users\gally\Documents\FuncProg\Practicum2\Opdracht1a
--fact n = if n == 0 then 1 else n * fact(n-1)
--main = print (fact 5)


 


--ghc --make -XQuasiQuotes main.hs -o main


