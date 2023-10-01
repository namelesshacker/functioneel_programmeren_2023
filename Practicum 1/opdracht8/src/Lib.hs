
{- 

 // Schrijf een functie, die een geometrisch object als parameter krijgt en de oppervlakte van het betreffende object uitrekent.
-}
myOpp :: Vierkant
mOpp = (15)
berekenOpp :: Double -> Double
berekenOpp x = x * x
berekenOpp :: Double -> Double -> Double
berekenOpp x y = x * y

berekenOpp :: Double -> Double

berekenOpp x = x * x

berekenOpp :: Double -> Double

berekenOpp x = x * x



data BasicShape = BasicCircle Float | BasicRect Float Float
deriving Show

BasicCircle 5

basicArea :: BasicShape -> Float
basicArea (BasicCircle r) = pi * r^2
basicArea (BasicRect w h) = w * h
basicArea (BasicCircle 2)
basicCircum (BasicRect 2 4)
basicCircum :: BasicShape -> Float
basicCircum (BasicCircle r) = 2 * pi * r
basicCircum (BasicRect w h) = 2 * (w + h)

{-


module Main where







data GeoAbstract = Vierkant String Int Int Int



                 | Rechthoek String String Int Int Int



                 | Diehoek String String Int Int Int



                 | Cirkel String String Int Int Int



message :: String -> String



message name



  | name == "Dave" = "I can't do that."



  | name == "Sam"  = "Play it again."



  | True           = "Hello."







class GeoObject t where



    Vierkant   :: t a



    Rechthoek  :: t a -> a -> t a -> t a











main :: IO ()



main = putStrLn (message "Vierkant")


-}