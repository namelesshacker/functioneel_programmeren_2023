
{-
example 2
https://stackoverflow.com/questions/23257373/java-poker-game-change-diamonds-and-hearts-to-red

-}

import System.Random

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Show, Enum)
data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Enum)

data Card = Card { value :: Value, suit :: Suit } deriving (Show)

hasStraight :: [Card] -> Bool
hasStraight cards = True

data Deck = Deck { deck :: [Card], hand :: [Card] }
data Player = Player { name :: String, chips :: Int, playerDeck :: Deck }

instance Show Player where
  show (Player name chips _) = "Player: " ++ name ++ ", Chips: " ++ show chips

instance Show Deck where
  show (Deck deck hand) = "Deck: " ++ show deck ++ ", Hand: " ++ show hand

instance Show Card where
  show (Card value suit) = show value ++ " of " ++ show suit

draw :: Deck -> IO Deck
draw (Deck deck hand) = do
  let handSize = 5
  gen <- newStdGen
  let randomIndices = take handSize $ randomRs (0, length deck - 1) gen
  let newHand = [hand !! index | index <- randomIndices]
  let newDeck = [card | (card, index) <- zip deck [0..], not (index `elem` randomIndices)]
  return (Deck newDeck newHand)

main :: IO ()
main = do
  let player = Player "John" 100 (Deck [] [])
  newDeck <- draw (playerDeck player)
  putStrLn $ "Player: " ++ show player
  putStrLn $ "New Deck: " ++ show newDeck
  





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
  
  
  
