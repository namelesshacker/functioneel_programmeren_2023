
module Main where

{-

data GeoAbstract = Vierkant | Rechthoek | Diehoek |Cirkel



chooseMove :: Int -> GeoAbstract



chooseMove i = if i == 1



  then Vierkant



  else if i == 2



    then Rechthoek



    else Diehoek











main :: IO ()



main = putStrLn m





module Main where
berekenOpp :: Double -> Double
berekenOpp x = x * x

main = do putStrLn "Wat is de oppervlakete"

          x <- readLn

          if x == berekenOpp 5



              then putStrLn "Goed!"



              else putStrLn "Fout!"



opgave 5

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float |
         Rectangle Point Point |
         Triangle Point Point Point  deriving (Show)

database :: [Shape]
database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
    (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
    (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]


isCircle :: Shape -> Bool
isCircle (Circle _ _) = True
isCircle _ = False

databaseCircles = filter isCircle database

main = print $ databaseCircles
https://stackoverflow.com/questions/47179462/haskell-how-to-get-a-list-of-figures-of-specified-type


data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float |
         Rectangle Point Point |
         Triangle Point Point Point  deriving (Show)

database :: [Shape]
database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
    (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
    (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]


isCircle :: Shape ->String -> Bool
isCircle (String _ _) = True
isCircle _ = False

databaseCircles = filter isCircle database

main = print $ databaseCircles


----

data Shape = Square {length:: Float, color:: Color}
           | Rectangle {length:: Float, widht:: Float, color :: Color}

data Color = Yellow | Green | Blue


getByColor :: String -> [Shape] -> [Shape]
getByColor shapeNameParam shapes = filter (\shape ->  shape == colorParam) shapes


https://stackoverflow.com/questions/22935071/filtering-list-based-on-items-with-record-syntax
https://stackoverflow.com/questions/52691646/haskell-filtering-a-list-of-strings
https://stackoverflow.com/questions/8712208/how-do-i-use-the-filter-function-in-haskell


-}



data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | 
         Rectangle Point Point | 
         Triangle Point Point Point  deriving (Show) 
         
database :: [Shape]
database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
    (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
    (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]
    
    
isCircle :: Shape  -> Bool
isCircle (Circle _ _) = True
isCircle _ = False

databaseCircles = filter isCircle database

---main = print $ databaseCircles