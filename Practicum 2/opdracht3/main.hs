{-
Opdracht 3
Maak een lijst van tenminste vijf boeken. Test of je implementatie van de EQ
en Ord typeclasses correct werkt door:
 enkele boeken met elkaar te vergelijken.
 de lijst met boeken te sorteren middels het gebruik van de sort functie.
Deze functie zit in de library Data.List.
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


