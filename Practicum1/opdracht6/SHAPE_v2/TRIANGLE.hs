


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