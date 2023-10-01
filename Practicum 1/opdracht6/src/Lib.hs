
opgave 6
--result n= [ s | s@(n _ _) <- database ]



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


----

result  = do  [ s | s@(Circle _ _) <- database ]

geofig :: Integer -> Integer
geofig n
  | n < 0            = 0
  | n `mod` 17 == 2  = -43
  | otherwise        = n + 3
  
myScore x list_items
    | x == "Vierkant" = [ s | s@(Vierkant _ _) <- list_items ]
    | x == "Circle" = [ s | s@(Cirkel _ _) <- list_items ]
    | x == "Triangle" = [ s | s@(Driehoek _ _) <- list_items ]
    | x == "Rectangle" = [ s | s@(Rectangle _ _) <- list_items ]
    | otherwise = [ s | s@(Circle _ _) <- list_items ]
    
y = (myScore "hello" database)
rest = print $ (y)
main = print $ (result)
		   
--getByName :: String -> [Shape] -> [Shape]
--getByName shapeNameParam shapes = filter (\shape ->  shape == shapeNameParam) shapes
