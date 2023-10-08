{-
Opdracht 1
Denieer het datatype Geofig. Dit datatype stelt een geometriche guur voor.
Enkele eigenschappen:
 Het datatype is algebrasch: Er bestaan dus verschillende waarden (value
constructoren) van. Deze zijn:
{ Vierkant: parameters: Lengte (=de lengte van een zijde)
{ Rechthoek: parameters: Lengte, Breedte
{ Driehoek: parameters: Lengte (=de lengte van een zijde), de driehoek
is gelijkzijdig.
{ Cirkel: parameters: Straal
 Elke geometrische guur heeft een kleur. Mogelijke kleuren zijn: Rood, Blauw, Geel
Ook voor de mogelijke kleuren wordt een nieuw datatype gedenieerd,
d.w.z. een enumeratietype.
Opdracht 2
Maak in de sourcele een aantal geometrische guren aan (tenminste een van
elke kleur), zodat je wat data krijgt, waarmee je kunt testen.
Opdracht 3
Schrijf een functie, die een geometrisch object als parameter krijgt en de oppervlakte
van het betreende object uitrekent.
Opdracht 4
Schrijf een functie, die een geometrisch object als parameter krijgt en de "omtrek"
(=totale lengte van alle zijden) van het betreende object uitrekent.
Opdracht 5
Schrijf een functie die een lijst van geometrische objecten als parameter krijgt
en een lijst met alleen de Vierkanten teruggeeft. Schrijf soortgelijke functies
voor de overige soorten objecten.
Opdracht 6
Schrijf een functie die een String en een lijst geometrische objecten als parameters
krijgt. Indien de String de waarde "Driehoek" heeft, geeft de functie
alleen de driehoeken terug. Uiteraard werkt dit ook bij de overige geometrische
objecten. Deze functie kan beschouwd worden als een iets algemenere versie
van de functies geschreven in opdracht 5.
Opdracht 7
Schrijf een functie die een Kleur en een lijstje objecten als parameters heeft. De
functie geeft een lijst terug van alleen die objecten met de betreende kleur.
Opdracht 8
Schrijf een functie die een lijstje objecten als parameter krijgt en het object met
de grootste oppervlakte teruggeeft. Idem voor de grootste omtrek.
Opdracht 9
Schrijf een functie die een geometrisch object aan een bestaande lijst toevoegt.
Opdracht 10
Schrijf een functie die een lijst objecten als parameter krijgt. De functie geeft
een lijst terug, waarin getallen staan. Ieder van deze getallen stelt de oppervlakte
van het geometrisch guur voor als percentage van de totale oppervlakte van
alle guren. Alle getallen bij elkaar opgeteld leveren derhalve de waarde 100 op.

-}
--module Main (main) where

--import MIMA

--rest = print $ (euclid 2 3)
--main :: IO ()
--main = print $ (euclid 36 15)

--C:\Users\gally\Documents\FuncProg\Practicum2\Opdracht1a
--fact n = if n == 0 then 1 else n * fact(n-1)
--main = print (fact 5)


 


--ghc --make -XQuasiQuotes main.hs -o main


data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | 
            Vierkant Point Float | 
         Rectangle Point Point | 
         Triangle Point Point Point  deriving (Show) 
 
database :: [Shape]
database = [(Circle (Point 2 5) 5), (Circle (Point 1 4) 3), (Circle (Point 8 3) 4),
    (Rectangle (Point 0 5) (Point 10 0)), (Rectangle (Point 3 5) (Point 10 0)),(Rectangle (Point 0 10) (Point 20 0)),
    (Triangle (Point 1 1) (Point 2 2) (Point 3 1)), (Triangle (Point 2 5) (Point 5 8) (Point 9 1))]
    
myScore x list_items
    | x == "Vierkant" = [ s | s@(Vierkant _ _) <- list_items ]
    | x == "Circle" = [ s | s@(Circle _ _) <- list_items ]
    | x == "Triangle" = [ s | s@(Triangle _ _ _) <- list_items ]
    | x == "Rectangle" = [ s | s@(Rectangle _ _) <- list_items ]
    | otherwise = [ s | s@(Circle _ _) <- list_items ]
    
y = (myScore "hello" database)
rest = print $ (y)
main = print $ (y)

