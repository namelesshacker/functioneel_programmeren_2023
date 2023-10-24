

{-
https://stackoverflow.com/questions/32643124/running-java-poker-game

-}
module Card where

data Card = Card { rank :: Int, suit :: Int }

suits :: [String]
suits = ["hearts", "spades", "diamonds", "clubs"]

ranks :: [String]
ranks = ["Ace", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Jack", "Queen", "King"]

rankAsString :: Int -> String
rankAsString __rank = ranks !! __rank

instance Show Card where
    show (Card rank suit) = ranks !! rank ++ " of " ++ suits !! suit

getRank :: Card -> Int
getRank (Card rank _) = rank

getSuit :: Card -> Int
getSuit (Card _ suit) = suit

data Deck = Deck { cards :: [Card] }

newDeck :: Deck
newDeck = Deck [Card suit rank | suit <- [0..3], rank <- [0..12]]

drawFromDeck :: Deck -> (Card, Deck)
drawFromDeck (Deck (card:cards)) = (card, Deck cards)

getTotalCards :: Deck -> Int
getTotalCards (Deck cards) = length cards

data Hand = Hand { cards :: [Card], value :: [Int] }

newHand :: Deck -> Hand
newHand (Deck deck) = Hand (take 5 deck) (evaluateHand (take 5 deck))

evaluateHand :: [Card] -> [Int]
evaluateHand cards = [0, 0, 0, 0, 0, 0] -- TODO: Implement hand evaluation logic

display :: Hand -> IO ()
display hand = putStrLn (formatHand hand)

formatHand :: Hand -> String
formatHand (Hand cards value) = case value !! 0 of
    1 -> "high card"
    2 -> "pair of " ++ rankAsString (value !! 1) ++ "'s"
    3 -> "two pair " ++ rankAsString (value !! 1) ++ " " ++ rankAsString (value !! 2)
    4 -> "three of a kind " ++ rankAsString (value !! 1) ++ "'s"
    5 -> rankAsString (value !! 1) ++ " high straight"
    6 -> "flush"
    7 -> "full house " ++ rankAsString (value !! 1) ++ " over " ++ rankAsString (value !! 2)
    8 -> "four of a kind " ++ rankAsString (value !! 1)
    9 -> "straight flush " ++ rankAsString (value !! 1) ++ " high"
    _ -> "error in Hand.display: value[0] contains invalid value"

displayAll :: Hand -> IO ()
displayAll (Hand cards _) = mapM_ print cards

compareTo :: Hand -> Hand -> Int
compareTo (Hand _ value1) (Hand _ value2) = compareLists value1 value2

compareLists :: [Int] -> [Int] -> Int
compareLists [] [] = 0
compareLists (x:xs) (y:ys)
    | x > y = 1
    | x < y = -1
    | otherwise = compareLists xs ys

{-
https://easycodestuff.blogspot.com/2014/07/card-deck-hand-class-in-poker-game.html

-}



{-
http://www.cs.williams.edu/~freund/cs136-053/lectures/lecture2/Poker/PokerHand.java

-}




{-
https://mblogscode.wordpress.com/2016/11/23/texas-holdem-poker-in-java-part-1-cards-decks-and-hands/

-}




{-
https://gist.github.com/suufi/5d5cc454d641ab2ae43ecb0eb0035503

-}


{-


-}


{-


-}



{-


-}



{-


-}



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