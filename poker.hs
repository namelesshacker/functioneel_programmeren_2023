

{-

https://www.cs.cornell.edu/courses/cs100/2003su/assignment5/solution/PokerHand.java
-}


import Data.List (sort)

data Card = Card { suit :: String, value :: Int } deriving (Eq, Ord)

data PokerHand = PokerHand { cards :: [Card], rank :: Int }

instance Eq PokerHand where
    (==) hand1 hand2 = rank hand1 == rank hand2

instance Ord PokerHand where
    compare hand1 hand2 = compare (rank hand1) (rank hand2)

instance Show Card where
    show (Card suit value) = suit ++ " " ++ show value

instance Show PokerHand where
    show (PokerHand cards rank) = "Rank: " ++ show rank ++ "\n" ++ showCards cards

showCards :: [Card] -> String
showCards [] = ""
showCards (c:cs) = "Card: " ++ show c ++ "\n" ++ showCards cs

pokerHand :: Card -> Card -> Card -> Card -> Card -> PokerHand
pokerHand card1 card2 card3 card4 card5 = PokerHand [card1, card2, card3, card4, card5] (calculateRank [card1, card2, card3, card4, card5])

calculateRank :: [Card] -> Int
calculateRank cards
    | isPair cards = 1
    | isTwoPair cards = 2
    | isThreeOfAKind cards = 3
    | isStraight cards = 4
    | isFlush cards = 5
    | isFullHouse cards = 6
    | isFourOfAKind cards = 7
    | isStraightFlush cards = 8
    | otherwise = 0

isPair :: [Card] -> Bool
isPair cards = any (\x -> length x == 2) (groupCards cards)

isTwoPair :: [Card] -> Bool
isTwoPair cards = length (filter (\x -> length x == 2) (groupCards cards)) == 2

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind cards = any (\x -> length x == 3) (groupCards cards)

isStraight :: [Card] -> Bool
isStraight cards = all (\(x, y) -> y - x == 1) (zip sortedValues (tail sortedValues))
    where sortedValues = sort (map value cards)

isFlush :: [Card] -> Bool
isFlush cards = all (\x -> x == suit (head cards)) (map suit cards)

isFullHouse :: [Card] -> Bool
isFullHouse cards = isThreeOfAKind cards && isPair cards

isFourOfAKind :: [Card] -> Bool
isFourOfAKind cards = any (\x -> length x == 4) (groupCards cards)

isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isStraight cards && isFlush cards

groupCards :: [Card] -> [[Card]]








--Schrijf een aantal functies ,waarmee je de kanen kunt berekenen  op uitkomsten
--filter, any, map, all, foldr,
s = [1..6]
stenen =[[a,b,c,d,e] | a <-s, b<-s, c<-s, d<-s, e<-s]

-- onderstaande functie retourneert het aantal voorkomens van c in eenlijst
count :: Integer-> [Integer]-> Integer ()
count c [] =0
count c (x:xs) 
  | c==x= 1 + (count c xs)
  | otherwise = count c xs
  
  
 -- converteer een lijst in een aantal tuples met voorkomens
convert list = ([a,b,c,d,e,f],list) where
  a = count 1 list
  b = count 2 list
  c = count 3 list
  d = count 4 list
  e = count 5list
  f = count 6 list
