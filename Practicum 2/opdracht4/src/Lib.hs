Opdracht 1
Schrijf het datatype Boek. Dit datatype bevat drie velden:
uitwerking



data Geschrift = Boek Titel Auteur Prijs
| Weekblad Titel Uitgever
deriving Show


data Geschrift = Boek Titel Auteur Prijs deriving Show

titel (Boek t a p) = t
auteur (Boek t a p) = a
prijs (Boek t a p) = p


titel (Boek t _ _) = t
auteur (Boek _ a _) = a
prijs (Boek _ _ p) = p


bladofboek (Boek _ _ _) =True

data Doos a = Leeg
| Doos a


data MisschienFloat = Ja Float
| Nee
deriving Show

data Doos a = Niets
| Boek a
x = Boek 25

data Lijst a =
|Leeg
|Hoofd a (Lijst a)

push :: a -> Lijst a -> Lijst a
push a Leeg = Hoofd a Leeg
push a (Hoofd h rest) = Hoofd a (Hoofd h rest)

pushlist :: Lijst a -> [a] -> Lijst a
pushlist Leeg lijst = foldr push Leeg lijst
pushlist (Hoofd h rest) lijst = foldr push (Hoofd h rest) lijst
Eenvoudigere versie?
pushlist l lijst = foldr push l lijst


type Prijs=Int
type Titel=String
type Auteur=String
data Boek = Boek Titel, Auteur Prijs deriving (Show)


instance Eq Boek where
(Boek a) == (Boek b) = a == b
_ /= _ = False

instance Ord Boek where
compare (Boek a) (Boek b) = compare a b

boekInDoos :: (Doos Lijst) -> (Doos Lijst)
boekInDoos Leeg = Leeg
boekInDoos (Doos a) = Doos (a)

