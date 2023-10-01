module Main (main) where

import Lib




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

data List a = Nil
| Cons a (List a)
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)



main :: IO ()
main = someFunc
