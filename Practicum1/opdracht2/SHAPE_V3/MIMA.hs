


module MIMA
    ( someFunc,euclid
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

euclid::Integer->Integer->Integer
euclid n p
  | n < 0            = 0
  | n `mod` 17 == 2  = -15
  | otherwise        = n + p

--rest = print $ (euclid 2 3)








module Main where









data Anniversary = Vierkant String Int Int Int



                 | Rechthoek String String Int Int Int



                 | Diehoek String String Int Int Int



                 | Cirkel String String Int Int Int



message :: String -> String



message name



  | name == "Dave" = "I can't do that."



  | name == "Sam"  = "Play it again."



  | True           = "Hello."


