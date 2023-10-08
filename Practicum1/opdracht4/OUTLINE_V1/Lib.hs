

opgave 4
data Shape = Vierkant Float Float Float | Rectangle Float Float Float Float   | Driehoek Float Float Float Float| Cirkel Float Float Float Float

omtrek :: Shape -> Float  
omtrek (Vierkant _ _ r) = pi * r ^ 2  
omtrek (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 
omtrek (Driehoek x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 
omtrek (Cirkel x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 
http://learnyouahaskell.com/making-our-own-types-and-typeclasses
https://mmhaskell.com/liftoff/data-types