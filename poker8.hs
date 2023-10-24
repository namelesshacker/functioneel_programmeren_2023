  
  {-
  example 3
  
  https://coderanch.com/t/736696/java/Draw-Poker-Java
  -}
  
  


import Data.List (sort)

data Card = Card { rank :: Int, suit :: Int }

data Deck = Deck

data Player = Player

data DrawPoker = DrawPoker { handSize :: Int, again :: Int, scan :: IO Int, deck :: Deck, player :: Player, hand :: [Card] }

-- instantiate Deck and Player
drawPoker :: DrawPoker
drawPoker = DrawPoker { handSize = 5, again = 1, scan = readLn, deck = Deck, player = Player, hand = [] }

-- plays the game
play :: DrawPoker -> IO ()
play dp = do
    let dp' = fillDeck dp
    dp'' <- shuffle dp'
    dp''' <- draw dp''
    dp'''' <- checkHand dp'''
    dp''''' <- redraw dp''''
    dp'''''' <- checkHand dp'''''
    dp''''''' <- evaluate dp''''''
    again <- again dp'''''''
    if again == 1
        then play dp'''''''
        else putStrLn "Thanks for playing! =]"

-- fills the deck
fillDeck :: DrawPoker -> DrawPoker
fillDeck dp = dp { deck = Deck }

-- shuffles the deck
shuffle :: DrawPoker -> IO DrawPoker
shuffle dp = do
    putStrLn "Shuffling the deck..."
    -- implementation of shuffling algorithm
    return dp

-- player draws
draw :: DrawPoker -> IO DrawPoker
draw dp = do
    putStrLn "Player is drawing..."
    -- implementation of drawing cards
    return dp { hand = [Card 1 1, Card 2 1, Card 3 1, Card 4 1, Card 5 1] }

-- sorts the hand
sortHand :: DrawPoker -> DrawPoker
sortHand dp = dp { hand = sort (hand dp) }

-- checks the hand
checkHand :: DrawPoker -> IO DrawPoker
checkHand dp = do
    putStrLn "Checking hand..."
    mapM_ display (hand dp)
    return dp

-- asks if player wants to redraw
redraw :: DrawPoker -> IO DrawPoker
redraw dp = do
    putStrLn "Redrawing cards..."
    -- implementation of redrawing cards
    return dp

-- evaluates the hand
evaluate :: DrawPoker -> IO DrawPoker
evaluate dp = do
    putStrLn "Evaluating the hand..."
    -- implementation of evaluating the hand
    return dp

-- checks for a royal flush
royalFlush :: [Card] -> Bool
royalFlush hand = 
    rank (hand !! 0) == 1 && rank (hand !! 1) == 10 && rank (hand !! 2) == 11 && rank (hand !! 3) == 12 && rank (hand !! 4) == 13

-- checks for a straight flush
straightFlush :: [Card] -> Bool
straightFlush hand = 
    all (\c -> suit c == suit (hand !! 0)) hand &&
    all (\(c1, c2) -> rank c1 == rank c2 - 1) (zip hand (tail hand))

-- checks for four of a kind
fourOfaKind :: [Card] -> Bool
fourOfaKind hand = 
    (rank (hand !! 0) /= rank (hand !! 3) && rank (hand !! 1) /= rank (hand !! 4))

-- checks for full house
fullHouse :: [Card] -> Bool
fullHouse hand = 
    let comparison = length [() | (c1, c2) <- zip hand (tail hand), rank c1 == rank c2]
    in comparison == 3

-- checks for flush
flush :: [Card] -> Bool
flush hand = 
    all (\c -> suit c == suit (hand !! 0)) hand

-- checks for straight
straight :: [Card] -> Bool
straight hand = 
    all (\(c1, c2) -> rank c1 == rank c2 - 1) (zip hand (tail hand))

-- checks for triple
triple :: [Card] -> Bool
triple hand = 
    rank (hand !! 0) == rank (hand !! 2) || rank (hand !! 2) == rank (hand !! 4)

-- checks for two pairs
twoPairs :: [Card] -> Bool
twoPairs hand = 
    let check = length [() | (c1, c2) <- zip hand (tail hand), rank c1 == rank c2]
    in check == 2

-- checks for pair
pair :: [Card] -> Bool
pair hand = 
    let check = length [() | (c1, c2) <- zip hand (tail hand), rank c1 == rank c2]
    in check == 1

-- checks for no pairs
noPair :: [Card] -> Bool
noPair hand = 
    let check = length [() | (c1, c2) <- zip hand (tail hand), rank c1 == rank c2]
    in check == 0

-- asks user if they want to play again
again :: DrawPoker -> IO DrawPoker
again dp = do
    putStrLn "Play again? (1 for yes, 0 for no)"
    a <- scan dp
    return dp { again = a }

-- generates string for each card in hand
display :: Card -> IO ()
display card = do
    putStr $ case rank card of
        1 -> "Ace of "
        2 -> "Two of "
        3 -> "Three of "
        4 -> "Four of "
        5 -> "Five of "
        6 -> "Six of "
        7 -> "Seven of "
        8 -> "Eight of "
        9 -> "Nine of "
        10 -> "Ten of "
        11 -> "Jack of "
        12 -> "Queen of "
        13 -> "King of "
    putStrLn $ case suit card of
        1 -> "Spades"
        2 -> "Hearts"
        3 -> "Diamonds"
        4 -> "Clubs"

main :: IO ()
main = play drawPoker









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