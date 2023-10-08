module Opdracht1
where

--OPDRACHT 1
type Lengte = Float
type Breedte = Float
type Straal = Float

data Kleur = Rood | Blouw | Geel | Paars
    deriving (Show,Eq)

data Geofig = Vierkant Lengte Kleur
            | Rechthoek Lengte Breedte Kleur
            | Driehoek Lengte Kleur
            | Cirkel Straal Kleur
   deriving (Show)

--OPDRACHT 2
rodeVierkant = Vierkant 3 Rood
blouweVierkant = Vierkant 4 Blouw
blouweRechthoek = Rechthoek 4 2 Blouw
geleDriehoek = Driehoek 2 Geel
paarseCirkel = Cirkel 2 Paars
lijst = [rodeVierkant, blouweVierkant, blouweRechthoek, geleDriehoek, paarseCirkel]

--OPDRACHT 3
oppGeofig::Geofig->Float
oppGeofig (Vierkant l _) = l^2
oppGeofig (Rechthoek l b _) = l * b
oppGeofig (Driehoek l _) = a*b
   where a = (1/2)*l
         b = sqrt(l^2-a^2)
oppGeofig (Cirkel r _) = pi*r^2

--OPDRACHT 4
omGeofig::Geofig->Float
omGeofig (Vierkant l _) = l*4
omGeofig (Rechthoek l b _) = l*2+b*2
omGeofig (Driehoek l _) = l*3
omGeofig (Cirkel r _) = 2*pi*r

--OPDRACHT 5
geofigVierkant (Vierkant _ _) = True
geofigVierkant (Rechthoek _ _ _) = False
geofigVierkant (Driehoek _ _) = False
geofigVierkant (Cirkel _ _) = False

geofigRechthoek (Vierkant _ _) = False
geofigRechthoek (Rechthoek _ _ _) = True
geofigRechthoek (Driehoek _ _) = False
geofigRechthoek (Cirkel _ _) = False

geofigDriehoek (Vierkant _ _) = False
geofigDriehoek (Rechthoek _ _ _) = False
geofigDriehoek (Driehoek _ _) = True
geofigDriehoek (Cirkel _ _) = False

geofigCirkel (Vierkant _ _) = False
geofigCirkel (Rechthoek _ _ _) = False
geofigCirkel (Driehoek _ _) = False
geofigCirkel (Cirkel _ _) = True

alleenVierkant :: [Geofig]->[Geofig]
alleenVierkant [] = []
alleenVierkant l = filter geofigVierkant l
alleenVierkant' (h:t) = if geofigVierkant h == True then h:alleenVierkant t
                        else alleenVierkant t

alleenRechthoek :: [Geofig]->[Geofig]
alleenRechthoek [] = []
alleenRechthoek l = filter geofigRechthoek l

alleenDriehoek  :: [Geofig]->[Geofig]
alleenDriehoek [] = []
alleenDriehoek l = filter geofigDriehoek l

alleenCirkel :: [Geofig]->[Geofig]
alleenCirkel [] = []
alleenCirkel l = filter geofigCirkel l

--OPDRACHT 6
filterGeofig :: String->[Geofig]->[Geofig]
filterGeofig figuur l
   | figuur == "Vierkant" = alleenVierkant l
   | figuur == "Rechthoek" = alleenRechthoek l
   | figuur == "Driehoek" = alleenDriehoek l
   | figuur == "Cirkel" = alleenCirkel l

--OPDRACHT 7
kleur (Vierkant _ k) = k
kleur (Rechthoek _ _ k) = k
kleur (Driehoek _ k) = k
kleur (Cirkel _ k) = k

filterGeofigColor :: Kleur->[Geofig]->[Geofig]
filterGeofigColor k [] = []
filterGeofigColor k (h:t) = if kleur h == k then h:filterGeofigColor k t
                            else filterGeofigColor k t

--OPDRACHT 8
maxOpp :: [Geofig] -> Geofig
maxOpp [] = error "lege lijst, geen object met maxOpp"
maxOpp (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail n (p:ps)
          | (oppGeofig n) < (oppGeofig p) = maxTail p ps
          | otherwise = maxTail n ps

maxOm :: [Geofig] -> Geofig
maxOm [] = error "lege lijst, geen object met maxOm"
maxOm (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail n (p:ps)
          | (omGeofig n) < (omGeofig p) = maxTail p ps
          | otherwise = maxTail n ps

--OPDRACHT 9
addGeofig :: Geofig -> [Geofig] -> [Geofig]
addGeofig x l = l ++ [x]

--OPDRACHT 10
lijstGetallen :: [Geofig] -> [Double]
lijstGetallen [] = []