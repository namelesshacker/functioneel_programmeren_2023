{-
Opdracht 9
Schrijf het datatype List. Dit datatype is geparameteriseerd en denieert recursief
een list.
 Leidt de List af van de Functor typeclass.
 Vul de List met dozen met getallen. Gebruik daarvoor de foldr of
foldl functie.
 Gebruik de functionaliteit van Functor om een met Box gevulde list te
vervangen door een met Zak gevulde list.

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


