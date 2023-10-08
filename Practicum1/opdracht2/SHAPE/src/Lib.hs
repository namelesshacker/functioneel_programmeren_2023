


{-# LANGUAGE DataKinds #-}







data Shapes = Circle Double | Square Double | Rectangle Double Double



  deriving (Show)







class Shape s where



  area :: s -> Double







instance Shape Shapes where



  area (Circle r) = pi * r * r



  area (Square l) = l * l



  area (Rectangle l w) = l * w







shapeList :: [Shapes]



shapeList = [Square 2, Circle 1, Rectangle 2 3]







main :: IO ()



main = do



  print $ area (Circle 1)



  print $ area (Square 2)



  print shapeList



  print $ map area shapeList






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







class Tree t where



    nil   :: t a



    node  :: t a -> a -> t a -> t a







class  Enum a  where



    succ, pred     :: a -> a



    toEnum         :: Int -> a



    fromEnum       :: a -> Int



    enumFrom       :: a -> [a]            -- [n..]



    enumFromThen   :: a -> a -> [a]       -- [n,n'..]



    enumFromTo     :: a -> a -> [a]       -- [n..m]



    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]







addThree :: Int -> Int -> Int -> Int



addThree x y z = x + y + z







main :: IO ()



main = print ( addThree 1 3 13)







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







class Tree t where



    nil   :: t a



    node  :: t a -> a -> t a -> t a







class  Enum a  where



    succ, pred     :: a -> a



    toEnum         :: Int -> a



    fromEnum       :: a -> Int



    enumFrom       :: a -> [a]            -- [n..]



    enumFromThen   :: a -> a -> [a]       -- [n,n'..]



    enumFromTo     :: a -> a -> [a]       -- [n..m]



    enumFromThenTo :: a -> a -> a -> [a]  -- [n,n'..m]







addThree :: Int -> Int -> Int -> Int



addThree x y z = x + y + z







data Shape = Circle Float



           | Rect Float Float



area :: Shape -> Float



area (Circle r) = pi * r * r



area (Rect d h) = d * h







circ :: Shape -> Float



circ (Circle r) = 2.0 * pi * r



circ (Rect d h) = 2.0 * (d + h)











main :: IO ()



main = do (circ 1 3)


