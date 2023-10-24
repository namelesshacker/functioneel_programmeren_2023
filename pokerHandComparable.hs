{-
https://copyprogramming.com/howto/a-simple-way-to-calculate-outs-on-a-poker-game-in-java#google_vignette
-}

module Poker where

import qualified Data.Set as Set
import qualified Data.Map as Map

data Suit = CLUBS | DIAMONDS | HEARTS | SPADES deriving (Show, Eq, Ord, Enum)
data Rank = TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | TEN | JACK | QUEEN | KING | ACE deriving (Show, Eq, Ord, Enum)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq, Ord)

data HandRank = STRAIGHT_FLUSH | FOUR_OF_A_KIND | FULL_HOUSE | FLUSH | STRAIGHT | THREE_OF_A_KIND | TWO_PAIR | PAIR | HIGH_CARD deriving (Show, Eq)

data PokerHand = PokerHand { handRank :: HandRank, handCards :: [Card] } deriving (Show)

data HandRanker = HandRanker { handCards :: [Card], pokerHand :: PokerHand }

fullHand :: Int
fullHand = 5

flush :: Int
flush = fullHand

straight :: Int
straight = fullHand

fullHouse :: Int
fullHouse = fullHand

quads :: Int
quads = 4

set :: Int
set = 3

pair :: Int
pair = 2

highCard :: Int
highCard = 1

straights :: Set.Set (Set.Set Rank)
straights = Set.fromList [
    Set.fromList [TWO, THREE, FOUR, FIVE, ACE],
    -- Add other possible straights here
    ]

setPossibleStraights :: Set.Set (Set.Set Rank)
setPossibleStraights = undefined -- Implement this function

getRank :: [Card] -> PokerHand
getRank allCards
    | length allCards /= 7 = error "You have to pass 7 cards"
    | isStraightFlush allCards = undefined -- Implement this function
    | isFourOfAKind allCards = undefined -- Implement this function
    | isFullHouse allCards = undefined -- Implement this function
    | isFlush allCards True = undefined -- Implement this function
    | isStraight allCards = undefined -- Implement this function
    | isSet allCards = undefined -- Implement this function
    | isTwoPair allCards = undefined -- Implement this function
    | isPair allCards = undefined -- Implement this function
    | isHighCard allCards = undefined -- Implement this function
    | otherwise = error "Invalid hand"

isStraightFlush :: [Card] -> Bool
isStraightFlush allCards = undefined -- Implement this function

isFlush :: [Card] -> Bool -> Bool
isFlush allCards finalResult = undefined -- Implement this function

isStraight :: [Card] -> Bool
isStraight allCards = undefined -- Implement this function

isFourOfAKind :: [Card] -> Bool
isFourOfAKind allCards = undefined -- Implement this function

isFullHouse :: [Card] -> Bool
isFullHouse allCards = undefined -- Implement this function

isSet :: [Card] -> Bool
isSet allCards = undefined -- Implement this function

isTwoPair :: [Card] -> Bool
isTwoPair allCards = undefined -- Implement this function

isPair :: [Card] -> Bool
isPair allCards = undefined -- Implement this function

isHighCard :: [Card] -> Bool
isHighCard allCards = undefined -- Implement this function

getHighestCards :: [Card] -> Int -> [Card]
getHighestCards allCards count = undefined -- Implement this function

getMultipleHighestCards :: [Card] -> Int -> [Card]
getMultipleHighestCards allCards count = undefined -- Implement this function

getMostPopularSuit :: Map.Map Suit Int -> Suit
getMostPopularSuit suits = undefined -- Implement this function

getSuitMap :: [Card] -> Map.Map Suit Int
getSuitMap allCards = undefined -- Implement this function

getRankMap :: [Card] -> Map.Map Rank Int
getRankMap allCards = undefined -- Implement this function

data Suit = Spades | Hearts | Clubs | Diamonds deriving (Show, Eq)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq)

instance Ord Rank where
    compare r1 r2 = compare (getValue r1) (getValue r2)

getValue :: Rank -> Int
getValue Two = 2
getValue Three = 3
getValue Four = 4
getValue Five = 5
getValue Six = 6
getValue Seven = 7
getValue Eight = 8
getValue Nine = 9
getValue Ten = 10
getValue Jack = 11
getValue Queen = 12
getValue King = 13
getValue Ace = 14

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq)

instance Ord Card where
    compare c1 c2 = compare (rank c1) (rank c2)

data Pokerhand = Pokerhand { cards :: [Card] }

instance Eq Pokerhand where
    (==) ph1 ph2 = cards ph1 == cards ph2

instance Ord Pokerhand where
    compare ph1 ph2 = compare (cards ph1) (cards ph2)

instance Show Pokerhand where
    show ph = "Pokerhand " ++ show (cards ph)

main :: IO ()
main = do
    let card1 = Card Two Spades
    let card2 = Card Three Hearts
    let card3 = Card Four Clubs
    let card4 = Card Five Diamonds
    let pokerhand = Pokerhand [card1, card2, card3, card4]
    print pokerhand

data HAND_RANK = HIGH_CARD | PAIR | TWO_PAIR | THREE_OF_A_KIND | STRAIGHT | FLUSH | FULL_HOUSE | FOUR_OF_A_KIND | STRAIGHT_FLUSH deriving (Eq, Ord, Show)

data PokerHand = PokerHand { handRank :: HAND_RANK, cards :: [Card] }

instance Eq PokerHand where
    (==) hand1 hand2 = handRank hand1 == handRank hand2 && cards hand1 == cards hand2

instance Ord PokerHand where
    compare hand1 hand2
        | hand1 == hand2 = EQ
        | handRank hand1 > handRank hand2 = GT
        | handRank hand1 < handRank hand2 = LT
        | otherwise = compareCards (cards hand1) (cards hand2)
        where
            compareCards [] [] = EQ
            compareCards (c1:cs1) (c2:cs2)
                | rank c1 > rank c2 = GT
                | rank c1 < rank c2 = LT
                | otherwise = compareCards cs1 cs2

instance Show PokerHand where
    show hand = "HandValue { handRank = " ++ show (handRank hand) ++ ", allCards = " ++ show (cards hand) ++ " }"

data Card = Card { rank :: Int, suit :: String } deriving (Eq, Ord, Show)

pokerHand :: HAND_RANK -> [Card] -> PokerHand
pokerHand handRank cards
    | length cards /= 5 = error "You have to pass five cards"
    | otherwise = PokerHand handRank (sort cards)