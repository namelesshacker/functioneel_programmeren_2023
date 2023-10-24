
{-
https://codereview.stackexchange.com/questions/65134/basic-oop-poker-deck-cards-and-hands

-}

import System.Random
import Data.List

data Deck = Deck { suits :: [String], ranks :: [String], deckLength :: Int, fullDeck :: [String] }

createDeck :: Deck
createDeck = Deck { suits = ["H", "D", "C", "S"], ranks = ["A", "K", "Q", "J", "10", "9", "8", "7", "6", "5", "4", "3", "2"], deckLength = length suits * length ranks, fullDeck = [] }

initializeDeck :: Deck -> Deck
initializeDeck deck = deck { fullDeck = [rank ++ suit | suit <- suits deck, rank <- ranks deck] }

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
  shuffled <- shuffleM (fullDeck deck)
  return (deck { fullDeck = shuffled })

showDeck :: Deck -> IO ()
showDeck deck = do
  mapM_ (\card -> putStr (card ++ " ")) (take (deckLength deck) (fullDeck deck))
  putStrLn ""

import System.Random

data Hands = Hands { hand :: [String] }

getHand :: [String] -> IO [String]
getHand fullDeck = do
  let deckLength = length fullDeck
  hand1 <- randomRIO (0, deckLength-1)
  hand2 <- randomRIO (0, deckLength-1)
  return [fullDeck !! hand1, fullDeck !! hand2]

showHand :: Hands -> IO ()
showHand hands = do
  mapM_ putStrLn (hand hands)

import System.Random
import Data.List

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    i <- getStdRandom $ randomR (0, length xs - 1)
    let (left, (a:right)) = splitAt i xs
    fmap (a:) (shuffle (left ++ right))

getHand :: [a] -> [a]
getHand = take 5

showHand :: Show a => [a] -> IO ()
showHand = mapM_ print

main :: IO ()
main = do
    cards <- shuffle []
    let deck = cards
    let hands = getHand deck
    showHand hands
	
	
	


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