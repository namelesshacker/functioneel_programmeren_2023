

opgave 3
module Main where

main = putStrLn "Hello, World!"


data Color = Red | Green | Blue deriving (Eq, Show)
 

type Breedte = Integer
type Lengte = Integer
type Straal = Integer

 dat :: Breedte -> Integer

type Vierkant=  (Lengte,Color) 
type Rechthoek= (Lengte, Breedte,Color)
type Driehoek=  (Lengte,Color) 

type Cirkel=  (Straal,Color)

factorial n = if n == 0 then 1 else n * factorial (n - 1)


data Thing = Vierkant 
           | Rechthoek 
           | Driehoek 
           | Cirkel 

  deriving Show
  
  
  
data Shape = Vierkant Float Float Float | Rectangle Float Float Float Float   | Driehoek Float Float Float Float| Cirkel Float Float Float Float
:t Cirkel 
surface :: Shape -> Float  
surface (Vierkant _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 
surface (Driehoek x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 
surface (Cirkel x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 