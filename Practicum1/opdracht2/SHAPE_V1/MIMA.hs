


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


