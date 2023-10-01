
-- {-
-- Opdracht 10
-- Schrijf een functie die een lijst objecten als parameter krijgt. De functie geeft
-- een lijst terug, waarin getallen staan. Ieder van deze getallen stelt de oppervlakte
-- van het geometrisch figuur voor als percentage van de totale oppervlakte van
-- alle figuren. Alle getallen bij elkaar opgeteld leveren derhalve de waarde 100 op.
-- -}


import Data.List (foldl') -- '
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM, forM_)
    
import Control.Monad.Cont







class Shapes a where
  area          :: a -> Float
  circumference :: a -> Float
  draw          :: a -> IO ()

data Shape = Circle {pos :: (Float, Float), radius :: Float} |
             Rect   {pos :: (Float, Float), width :: Float, height :: Float} |
             Square {pos :: (Float, Float), width :: Float}

instance Shapes Shape where
  area (Circle _ radius)              = pi * radius ^ 2
  area (Rect _ width height)          = width * height
  area (Square _ width)               = width ^ 2
  circumference (Circle _ radius)     = 2 * pi * radius
  circumference (Rect _ width height) = 2 * (width + height)
  circumference (Square _ width)      = 4 * width
  draw (Circle pos radius)            = putStrLn (" Circle drawn @ " ++ show pos ++ " with radius " ++ show radius)
  draw (Rect pos width height)        = putStrLn (" Rectangle drawn @ " ++ show pos ++ " with (w,h) " ++ show (width,height))
  draw (Square pos width)             = putStrLn (" Square drawn @ " ++ show pos ++ " with width " ++ show width)

instance Show Shape where
  show (Circle pos radius)     = "Circle with radius " ++ show radius ++ " @ " ++ show pos
  show (Rect pos width height) = "Rect (w,h)" ++ show (width, height) ++ " @ " ++ show pos
  show (Square pos width)      = "Square with edge " ++ show width ++ " @ " ++ show pos

 


listOfOjects :: [Shape]
listOfOjects = [(Circle (20,20) 5),(Rect (20,20) 4 7),(Square (20,20) 8) ]


isCircle :: Shape  -> Bool
isCircle (Circle _ _) = True
isCircle _ = False

isRect :: Shape  -> Bool
isRect (Rect _ _ _) = True
isRect _ = False


isSquare :: Shape  -> Bool
isSquare (Square _ _) = True
isSquaree _ = False



--isTriangle :: Shape  -> Bool
--isTriangle (Triangle _ _ _) = True
--isTriangle _ = False


databaseCircles = filter isCircle listOfOjects
--databaseRectangles = filter isRectangle listOfOjects
--databaseTriangles= filter isTriangle listOfOjects

list_edit []=[]
list_edit (x:xs)= if isCircle x
                  then circumference x:list_edit(xs)
                  else if isRect x 
                    then circumference x:list_edit(xs)
                  else circumference x:list_edit(xs)
                  
getMeanAndPercents :: [Int] -> [Int] -> [Float]
getMeanAndPercents thresholds depths
  = let depths2 = map f depths
        counts  = foldl1 (zipWith (+)) depths2
        len     = fromIntegral $ length depths
    in fromIntegral (head counts) / len : map (\c -> fromIntegral c / len) (tail counts)
 where f d = d : map (\t -> if d >= t then 1 else 0) thresholds
 
outcome = list_edit[(Circle (20,20) 5),(Rect (20,20) 4 7),(Square (20,20) 8) ]  
result=sum   outcome 


                          
cutOffPercentages []=[]
cutOffPercentages (x:xs)= if result >= 0
                  then x/result:cutOffPercentages(xs)
                  else x/result:cutOffPercentages(xs)
newoutcome = cutOffPercentages outcome
main=print("Changed List ",cutOffPercentages outcome)
--main=print("Changed List ",list_edit[(Circle (20,20) 5),(Rect (20,20) 4 7),(Square (20,20) 8) ])
{-

main = do
    forM_ listOfOjects $ \i -> do
        print i

    forM_ [7..9] $ \j -> do
        print j

    withBreak $ \break ->
        forM_ [1..] $ \_ -> do
            p "loop"
            break ()

    where
    withBreak = (`runContT` return) . callCC
    p = liftIO . putStrLn
    
    -}
	
{-
 Second attempt
-}	
	

-- foo :: Maybe Int -> Int
-- foo Nothing = 0
-- foo (Just x) = 1 + x
-- bar :: Maybe (Maybe String) -> String
-- bar Nothing = "None"
-- bar (Just Nothing) = "Sorta"
-- bar (Just (Just x)) = "Yes: " ++ x
-- -- N.B. ++ concatenates lists


-- data Point = Pt Float Float
-- pointx :: Point -> Float
-- pointx (Pt x _) = x
-- pointy :: Point -> Float
-- pointy (Pt _ y) = y


-- --main = putStrLn "hello world"

-- data vierkant :: Int -> Int
-- data rechthoek :: Int -> Int
-- data driehoek :: Int -> Int
-- data cirkel :: Int -> Int
-- data Point = Pt { pointx :: Float,pointy :: Float }

-- abs :: Int -> Int
-- abs 0 = 0
-- abs x | x < 0 = -x -- '|' starts a guard
 -- | otherwise = x

-- factorial :: Integer -> Integer
-- factorial 0 = 1
-- factorial n = n * factorial (n – 1)




-- --Geofig



-- absPoint :: Point -> Float
-- absPoint (Pt {pointx = x, pointy = y})
-- = sqrt (x*x + y*y)


-- data Color = Red | Green | Blue
-- instance Show (Color) where
 -- show Red = "Red"
 -- show Green = "Green"
 -- show Blue = "Blue"


 -- instance Monad Maybe where
-- (Just x) >>= f = f x
-- Nothing >>= f = Nothing
-- return = Just
-- instance Monad [] where
 -- lst >>= f = concat (map f lst)
-- return x = [x]
-- -- and IO monad is mostly built-in



-- data Color = Red | Green | Blue
 -- deriving (Eq, Show)


-- --http://courses.cms.caltech.edu/cs11/material/haskell/lectures/haskell_lecture_5.pdf


-- --main = putStrLn "hello world"



-- data Color = Red | Green | Blue
 -- deriving (Eq, Show)



-- geofig :: Integer -> Integer
-- geofig 0 = 16
-- geofig 1
  -- | "Haskell" > "C++" = 3
  -- | otherwise         = 4
-- geofig n
  -- | n < 0            = 0
  -- | n `mod` 17 == 2  = -43
  -- | otherwise        = n + 3

-- vierkant :: Integer -> Integer
-- vierkant n
  -- |  n < 0            = 0

-- rechthoek :: Integer -> Integer
-- rechthoek n
  -- |  n < 0            = 0

-- driehoek :: Integer -> Integer
-- driehoek n
  -- |  n < 0            = 0

-- cirkel :: Integer -> Integer
-- cirkel n
  -- |  n < 0            = 0

-- data Thing = Shoe
           -- | Ship
           -- | SealingWax
           -- | Cabbage
           -- | King
  -- deriving Show
  
  -- {-

-- Vierkant: parameters: Lengte (=de lengte van een zijde)
-- { Rechthoek: parameters: Lengte, Breedte
-- { Driehoek: parameters: Lengte (=de lengte van een zijde), de driehoek
-- is gelijkzijdig.
-- { Cirkel: parameters: Straal

-- -}


-- type Name = String

-- data Employee = Employee { name    :: Name
                         -- , phone   :: String }
                -- deriving Show

-- --https://www.seas.upenn.edu/~cis194/spring13/lectures/10-applicative.html

-- --type Name = String

-- data BigRecord = BR { getName         :: Name
                    -- , getSSN          :: String
                    -- , getSalary       :: Integer
                    -- , getPhone        :: String
                    -- , getLicensePlate :: String
                    -- , getNumSickDays  :: Int
                    -- }

-- r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2
-- --https://www.seas.upenn.edu/~cis194/spring13/lectures/11-applicative2.html





-- data Point = Point Float Float deriving (Show)
-- data Shape = Circle Point Float | 
         -- Rectangle Point Point | 
         -- Triangle Point Point Point  deriving (Show) 
         
-- database :: [Shape]
-- database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
    -- (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
    -- (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]
    
    
-- isCircle :: Shape  -> Bool
-- isCircle (Circle _ _) = True
-- isCircle _ = False

-- databaseCircles = filter isCircle database

-- ---main = print $ databaseCircles





-- --myOpp :: Vierkant
-- --mOpp = (15)
-- berekenOppSquare :: Double -> Double
-- berekenOppSquare x = x * x
-- berekenOpp :: Double -> Double -> Double
-- berekenOpp x y = x * y

-- main = print $ databaseCircles

 


-- data BasicShape = BasicCircle Float | BasicRect Float Float
-- deriving Show

-- BasicCircle 5

-- basicArea :: BasicShape -> Float
-- basicArea (BasicCircle r) = pi * r^2
-- basicArea (BasicRect w h) = w * h
-- basicArea (BasicCircle 2)
-- basicCircum (BasicRect 2 4)
-- basicCircum :: BasicShape -> Float
-- basicCircum (BasicCircle r) = 2 * pi * r
-- basicCircum (BasicRect w h) = 2 * (w + h)




-- stats_h :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
-- stats_h p [] = p
-- stats_h (len, sum, sumsq) (x:xs) =
  -- stats_h (len+1, sum + x, sumsq + x*x) xs
 
-- stats :: [Int] -> (Int, Int, Int)
-- stats = stats_h (0, 0, 0)

-- λ stats_h (0, 0, 0) [1, 2, 3]
-- (3,6,14)
-- λ stats [1, 2, 3]
-- (3,6,14)
-- ----

-- result  = do  [ s | s@(Circle _ _) <- database ]

-- geofig :: Integer -> Integer
-- geofig n
  -- | n < 0            = 0
  -- | n `mod` 17 == 2  = -43
  -- | otherwise        = n + 3
  
-- myScore x list_items
    -- | x == "Vierkant" = [ s | s@(Vierkant _ _) <- list_items ]
    -- | x == "Circle" = [ s | s@(Cirkel _ _) <- list_items ]
    -- | x == "Triangle" = [ s | s@(Driehoek _ _) <- list_items ]
    -- | x == "Rectangle" = [ s | s@(Rectangle _ _) <- list_items ]
    -- | otherwise = [ s | s@(Circle _ _) <- list_items ]
    
-- y = (myScore "hello" database)
-- rest = print $ (y)
-- main = print $ (result)
		   
-- --getByName :: String -> [Shape] -> [Shape]
-- --getByName shapeNameParam shapes = filter (\shape ->  shape == shapeNameParam) shapes

-- -- een lijst als input parameter
-- filter _ [] = []
-- filter p (h:l)
 -- | p h        = h : filter p l
 -- | otherwise  = filter p l
 
 
-- -- bereken opp
 
-- -- tell alle oppervlaten op
-- foldr (++) "u" ["a", "b", "c", "d", "e"]

-- -- add to new list met darin Naam van figuur, de parameters, oppervlakte en opprvlakte als percentale van geheel


-- --loop
-- iter2d :: [[Char]] -> Something
-- iter2d xs = foldl' outerStep outerInit xs
  -- where outerInit = ... -- same as outerInit above
        -- outerStep acc row = fst $ foldl' innerStep innerInit' row)
          -- where innerInit' = ((adjust1 acc row), innerInit row)
        -- innerInit row = ... -- same as innerInit above
        -- innerStep (outAcc, inAcc) c = (outAcc', inAcc')
          -- where inAcc' = adjust2 inAcc c
                -- outAcc' = adjust3 outAcc c inAcc'
				
-- list_edit []=[]
-- list_edit (x:xs)= if x == 2
                  -- then 3:list_edit(xs)
                  -- else x:list_edit(xs)
                  
-- main=print("Changed List ",list_edit[1, 2 , 3 , 2 , 4 , 5 , 2 , 6 ])
				
-- --
-- loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  -- in loop acc' xs
				  
-- square :: [Double] -> [Double]

-- square (x:xs) = x*x : square xs
-- square []     = []


-- square2 xs = map squareOne xs
    -- where squareOne x = x * x
	
	
-- -- voeg het totaal toe aan de print




{-
 Third attempt
-}




-- import Data.List (foldl') -- '
-- import Data.STRef
-- import Control.Monad.ST
-- import Control.Monad (forM, forM_)


-- data Color = Red | Green | Blue
 -- deriving (Eq, Show)



-- geofig :: Integer -> Integer
-- geofig 0 = 16
-- geofig 1
  -- | "Haskell" > "C++" = 3
  -- | otherwise         = 4
-- geofig n
  -- | n < 0            = 0
  -- | n `mod` 17 == 2  = -43
  -- | otherwise        = n + 3

-- vierkant :: Integer -> Integer
-- vierkant n
  -- |  n < 0            = 0

-- rechthoek :: Integer -> Integer
-- rechthoek n
  -- |  n < 0            = 0

-- driehoek :: Integer -> Integer
-- driehoek n
  -- |  n < 0            = 0

-- cirkel :: Integer -> Integer
-- cirkel n
  -- |  n < 0            = 0

-- data Thing = Shoe
           -- | Ship
           -- | SealingWax
           -- | Cabbage
           -- | King
  -- deriving Show
  


-- leeftijd x
    -- |x<0 = "U bestaat nog niet!"
    -- |x>0&&x<12 = "U bent een kind."
    -- |x>=12&&x<18 = "U bent een puber."
    -- |x>=18&&x<70 = "U bent volwassen."
    -- |otherwise = "Bent u er uberhaubt?"


-- optelsom 0 = 0
-- optelsom n = n+optelsom(n-1)
-- optelsom n
    -- | n==0 = 0
    -- | n>0 = n+optelsom(n-1)


-- takken 1 = 0
-- takken n = n-1+takken(n-1)
-- takken n
    -- |n==1 = 0
    -- |n>1 = n-1+takken(n-1)



-- oppervlak r x
    -- |x<pi*r^2 = False
    -- |x>=pi*r^2 = True
-- oppervlak r x
    -- |x<op = False
    -- |x>=op = True
    -- where op = pi*r^2


-- oppervlakenstraal r =
    -- let op = pi*r^2
        -- om = 2*pi*r
        -- in (op,om)



-- berekencijfer s = case s of "Wessel" -> 1
                            -- "Henk" -> 10
                            -- "Jaap" -> 5
                            -- "Klaas" -> 8

-- {-
-- myScore x list_items
    -- | x == "Vierkant" = do [ s | do s@(vierkant _ _) <- list_items ]
    -- | x == "Circle" = do [ s | s@(Circle _ _) <- list_items ]
    -- | x == "Triangle" = do [ s | s@(driehoek _ _) <- list_items ]
    -- | x == "Rectangle" = do [ s | s@(Rectangle _ _) <- list_items ]
    -- | otherwise = do [ s | s@(Circle _ _) <- list_items ]
    
-- y = (myScore "hello" database)
-- -}
  

  -- {-
-- data State = A | B | C

-- type family IsAorB (s :: State) where
  -- IsAorB 'A = 'True
  -- IsAorB 'B = 'True
  -- IsAorB _  = 'False

-- -- type AorB s = IsAorB s ~ 'True

-- class (IsAorB s ~ 'True) => AorB s
-- instance (IsAorB s ~ 'True) => AorB s

-- data Some (s :: State) where
  -- SomeA :: Some 'A
  -- SomeB :: Some 'B
  -- SomeC :: Some 'C

-- data Ex c f where
  -- Ex :: c a => f a -> Ex c f

-- bar :: Ex AorB Some
-- bar = Ex SomeA
-- -}


  -- {-

-- Vierkant: parameters: Lengte (=de lengte van een zijde)
-- { Rechthoek: parameters: Lengte, Breedte
-- { Driehoek: parameters: Lengte (=de lengte van een zijde), de driehoek
-- is gelijkzijdig.
-- { Cirkel: parameters: Straal

-- -}


-- type Name = String

-- data Employee = Employee { name    :: Name
                         -- , phone   :: String }
                -- deriving Show

-- --https://www.seas.upenn.edu/~cis194/spring13/lectures/10-applicative.html

-- --type Name = String

-- data BigRecord = BR { getName         :: Name
                    -- , getSSN          :: String
                    -- , getSalary       :: Integer
                    -- , getPhone        :: String
                    -- , getLicensePlate :: String
                    -- , getNumSickDays  :: Int
                    -- }

-- r = BR "Brent" "XXX-XX-XXX4" 600000000 "555-1234" "JGX-55T3" 2
-- --https://www.seas.upenn.edu/~cis194/spring13/lectures/11-applicative2.html





-- data Point = Point Float Float deriving (Show)
-- data Shape = Circle Point Float | 
         -- Rectangle Point Point | 
         -- Triangle Point Point Point  deriving (Show) 
         
-- database :: [Shape]
-- database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
    -- (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
    -- (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]
    
    
-- isCircle :: Shape  -> Bool
-- isCircle (Circle _ _) = True
-- isCircle _ = False

-- isRectangle :: Shape  -> Bool
-- isRectangle (Rectangle _ _) = True
-- isRectangle _ = False



-- isTriangle :: Shape  -> Bool
-- isTriangle (Triangle _ _ _) = True
-- isTriangle _ = False



-- ---main = print $ databaseCircles





-- --myOpp :: Vierkant
-- --mOpp = (15)
-- berekenOppSquare :: Double -> Double
-- berekenOppSquare x = x * x
-- berekenOpp :: Double -> Double -> Double
-- berekenOpp x y = x * y




-- --data BasicCircle = Circle Float deriving (Show)


-- data BasicShape = BasicCircle Float | BasicRect Float Float
    -- deriving Show

-- --BasicCircle 5

-- {-

-- basicArea :: BasicShape -> Float
-- basicArea (BasicCircle r) = pi * r^2
-- basicArea (BasicRect w h) = w * h
-- basicArea (BasicCircle 2.0)
-- basicCircum (BasicRect 2 4)
-- basicCircum :: BasicShape -> Float
-- basicCircum (BasicCircle r) = 2 * pi * r
-- basicCircum (BasicRect w h) = 2 * (w + h)
-- -}



-- {-

-- stats_h :: (Int, Int, Int) -> [Int] -> (Int, Int, Int)
-- stats_h p [] = p
-- stats_h (len, sum, sumsq) (x:xs) =
  -- stats_h (len+1, sum + x, sumsq + x*x) xs
 
-- stats :: [Int] -> (Int, Int, Int)
-- stats = stats_h (0, 0, 0)

-- λ stats_h (0, 0, 0) [1, 2, 3]
-- (3,6,14)
-- λ stats [1, 2, 3]
-- (3,6,14)
-- -}



-- --result  = do  [ s | s@(Circle _ _) <- database ]
-- {-
-- geofig :: Integer -> Integer
-- geofig n
  -- | n < 0            = 0
  -- | n `mod` 17 == 2  = -43
  -- | otherwise        = n + 3
-- -}
  

-- --rest = print $ (y)
-- --main = print $ (result)
-- --getByName :: String -> [Shape] -> [Shape]
-- --getByName shapeNameParam shapes = filter (\shape ->  shape == shapeNameParam) shapes
-- -- een lijst als input parameter

-- {-
-- filter _ [] = []
-- filter p (h:l)
 -- | p h        = h : filter p l
 -- | otherwise  = filter p l
-- -}
 
-- -- bereken opp
 
-- -- tell alle oppervlaten op
-- --foldr (++) "u" ["a", "b", "c", "d", "e"]

-- -- add to new list met darin Naam van figuur, de parameters, oppervlakte en opprvlakte als percentale van geheel


-- --loop
-- {-
-- iter2d :: [[Char]] -> Something
-- iter2d xs = foldl' outerStep outerInit xs
  -- where outerInit = outerInit -- same as outerInit above
        -- outerStep acc row = (fst $ foldl' innerStep innerInit' row)
          -- where innerInit' = ((adjust1 acc row), innerInit row)
        -- innerInit row = innerInit -- same as innerInit above
        -- innerStep (outAcc, inAcc) c = (outAcc', inAcc')
          -- where inAcc' = adjust2 inAcc c
                -- outAcc' = adjust3 outAcc c inAcc'
-- -}
-- list_edit []=[]
-- list_edit (x:xs)= if x == 2
                  -- then 3:list_edit(xs)
                  -- else x:list_edit(xs)
                  
                  
                  
-- list_surfaces [] =[]
-- list_surfaces (x:xs)= if x == 2
                  -- then 3:list_surfaces(xs)
                  -- else x:list_surfaces(xs)
-- --main=print("Changed List ",list_edit[1, 2 , 3 , 2 , 4 , 5 , 2 , 6 ])
-- {-
-- loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  -- in loop acc' xs
				  
-- square :: [Double] -> [Double]

-- square (x:xs) = x*x : square xs
-- square []     = []


-- square2 xs = map squareOne xs
    -- where squareOne x = x * x
-- -}
-- databaseCircles = filter isCircle database
-- databaseRectangles = filter isRectangle database
-- databaseTriangles= filter isTriangle database

-- stdev :: [Float] -> Float
-- stdev xs = sqrt . average . map ((^2) . (-) axs) $ xs
           -- where average = (/) <$> sum <*> realToFrac . length
                 -- axs     = average xs
                 
 
-- --main=print("Changed List ",list_edit[1, 2 , 3 , 2 , 4 , 5 , 2 , 6 ])

-- -- i is start index, l is length of each list, ms is list of means,
-- -- xs is Matrix
-- stdDev i l ms xs
    -- | i < l = sqrt(fromIntegral(sumOfMinusMeans i (ms!!i) xs) /
    -- fromIntegral(l)):(stdDev (i+1) l ms xs)
    -- | otherwise = []

 
-- --i is index, m is mean for the index
-- sumOfMinusMeans i m (x:xs)
    -- | xs == [] = (x!!i - m)**2
    -- | i < length x = (x!!i - m)**2 + (sumOfMinusMeans i m xs)
    -- | otherwise = 0
    
-- data Pair a b = Pair !a !b

-- sumLen :: [Double] -> Pair Double Double
-- sumLen = fiof2 . foldl' (\(Pair s l) x -> Pair (s+x) (l+1)) (Pair 0.0 0) --'
  -- where fiof2 (Pair s l) = Pair s (fromIntegral l)

-- divl :: Pair Double Double -> Double
-- divl (Pair _ 0.0) = 0.0
-- divl (Pair s   l) = s / l

-- sd :: [Double] -> Double
-- sd xs = sqrt $ foldl' (\a x -> a+(x-m)^2) 0 xs / l --'
  -- where p@(Pair s l) = sumLen xs
        -- m = divl p
        
-- Surfacevierkant :: Integer -> Integer
-- Surfacevierkant n
-- |  n < 0            = 0

-- Surfacerechthoek :: Integer -> Integer
-- Surfacerechthoek n
  -- |  n < 0            = 0

-- Surfacedriehoek :: Integer -> Integer
-- Surfacedriehoek n
  -- |  n < 0            = 0

-- Surfacecirkel :: Integer -> Integer
-- Surfacecirkel n
  -- |  n < 0            = 0

-- mkSD :: ST s (Double -> ST s Double)
-- mkSD = go <$> newSTRef []
  -- where go acc x =
          -- modifySTRef acc (x:) >> (sd <$> readSTRef acc)
          
-- calcSurface :: ST s (Double -> ST s Double)
-- calcSurface = go <$> newSTRef []
  -- where go acc x =
          -- modifySTRef acc (x:) >> (sd <$> readSTRef acc)
          
-- --surfaceResult = mapM_ print $ runST $
-- --  calcSurface >>= forM database
-- --main = print $ databaseCircles

-- main = mapM_ print $ runST $
  -- mkSD >>= forM [2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0]