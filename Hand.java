package com.example.poker;
 
import java.util.Arrays;
import java.util.Map.Entry;
 
import com.example.poker.Card.Rank;
 
import static java.util.Comparator.comparingInt;
import static java.util.Comparator.naturalOrder;
import static java.util.Comparator.nullsFirst;
 
import static java.util.stream.Collectors.counting;
import static java.util.stream.Collectors.groupingBy;
 
final class Hand {
   
  private static final int HAND_SIZE = 5;
 
  private final Card[] cards;
   
  Hand(Card... cards) {
    if (cards == null)
      throw new IllegalArgumentException("The array of cards may not be null.");
     
    if (cards.length != HAND_SIZE)
      throw new IllegalArgumentException("The number of cards must match the hand size.");
     
    this.cards = Arrays.copyOf(cards, HAND_SIZE);
     
    Arrays.sort(this.cards, nullsFirst(naturalOrder()));
     
    if (this.cards[0] == null)
      throw new IllegalArgumentException("None of the cards may be null.");
  }
   
  Card getHighCard() {
    return cards[HAND_SIZE - 1];
  }
 
  private boolean isKind(Kind kind) {
    return kind.findInHand(this);
  }
 
  Kind getKind() {
    return Arrays.stream(Kind.values())
      .sorted(comparingInt(Kind::points).reversed())
      .filter(this::isKind)
      .findFirst().get();
  }
 
  int evaluate() {
    return getKind().points();
  }
   
  private int countTuple(int tuple) {
    var rankCounts = Arrays.stream(cards).collect(groupingBy(Card::rank, counting()));
    var tupleCounts = rankCounts.entrySet().stream().collect(groupingBy(Entry::getValue, counting()));
    return tupleCounts.get(Long.valueOf(tuple)).intValue();
  }
   
  private int countSuits() {
    return (int) Arrays.stream(cards).map(Card::suit).distinct().count();
  }
 
  static enum Kind {
 
    HIGH_CARD (-5) {
      @Override
      protected boolean findInHand(Hand hand) {
        return true;
      }
    },
 
    ONE_PAIR (2) {
      @Override
      protected boolean findInHand(Hand hand) {
        return hand.countTuple(2) == 1;
      }
    },
 
    TWO_PAIRS (10) {
      @Override
      protected boolean findInHand(Hand hand) {
        return hand.countTuple(2) == 2;
      }
    },
 
    THREE_OF_A_KIND (20) {
      @Override
      protected boolean findInHand(Hand hand) {
        return hand.countTuple(3) == 1;
      }
    },
 
    STRAIGHT (25) {
      @Override
      protected boolean findInHand(Hand hand) {
        var ranks = Arrays.stream(hand.cards).map(Card::rank).distinct().toArray(Rank[]::new);
        return ranks.length == HAND_SIZE && ranks[0].ordinal() + HAND_SIZE - 1 == ranks[HAND_SIZE - 1].ordinal(); 
      }
    },
 
    FLUSH (35) {
      @Override
      protected boolean findInHand(Hand hand) {
        return hand.countSuits() == 1;
      }
    },
 
    FULL_HOUSE (50) {
      @Override
      protected boolean findInHand(Hand hand) {
        return hand.countTuple(2) == 1 && hand.countTuple(3) == 1;
      }
    },
 
    FOUR_OF_A_KIND (75) {
      @Override
      protected boolean findInHand(Hand hand) {
        return hand.countTuple(4) == 1;
      }
    },
 
    STRAIGHT_FLUSH (100) {
      @Override
      protected boolean findInHand(Hand hand) {
        return FLUSH.findInHand(hand) && STRAIGHT.findInHand(hand);
      }
    },
 
    ROYAL_FLUSH (200) {
      @Override
      protected boolean findInHand(Hand hand) {
        return STRAIGHT_FLUSH.findInHand(hand) && hand.getHighCard().rank() == Rank.ACE;
      }
    };
 
    private final int points;
 
    private Kind(int points) {
      this.points = points;
    }
 
    int points() {
      return points;
    }
 
    protected abstract boolean findInHand(Hand hand);
 
  }
}




module Poker where

import Data.List (sort, groupBy)
import Data.Function (on)
import Data.Ord (comparing)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Show)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show)

data Hand = Hand { cards :: [Card] }

handSize :: Int
handSize = 5

newHand :: [Card] -> Hand
newHand cards
  | length cards /= handSize = error "The number of cards must match the hand size."
  | any (== Nothing) maybeCards = error "None of the cards may be null."
  | otherwise = Hand (sort (map (\(Just c) -> c) maybeCards))
  where
    maybeCards = map (\c -> if c == Nothing then Nothing else Just (fromJust c)) cards

getHighCard :: Hand -> Card
getHighCard (Hand cards) = last cards

isKind :: Hand -> Kind -> Bool
isKind hand kind = findInHand kind hand

getKind :: Hand -> Kind
getKind hand = head (filter (isKind hand) (reverse (sort allKinds)))

evaluate :: Hand -> Int
evaluate hand = points (getKind hand)

countTuple :: Hand -> Int -> Int
countTuple (Hand cards) tuple = length (filter (\g -> length g == tuple) groupedRanks)
  where
    groupedRanks = groupBy ((==) `on` rank) cards

countSuits :: Hand -> Int
countSuits (Hand cards) = length (groupBy ((==) `on` suit) cards)

data Kind = HighCard | OnePair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush
  deriving (Eq, Show)

points :: Kind -> Int
points HighCard = -5
points OnePair = 2
points TwoPairs = 10
points ThreeOfAKind = 20
points Straight = 25
points Flush = 35
points FullHouse = 50
points FourOfAKind = 75
points StraightFlush = 100
points RoyalFlush = 200

findInHand :: Kind -> Hand -> Bool
findInHand HighCard _ = True
findInHand OnePair hand = countTuple hand 2 == 1
findInHand TwoPairs hand = countTuple hand 2 == 2
findInHand ThreeOfAKind hand = countTuple hand 3 == 1
findInHand Straight hand = isStraight hand
findInHand Flush hand = countSuits hand == 1
findInHand FullHouse hand = countTuple hand 2 == 1 && countTuple hand 3 == 1
findInHand FourOfAKind hand = countTuple hand 4 == 1
findInHand StraightFlush hand = findInHand Flush hand && findInHand Straight hand
findInHand RoyalFlush hand = findInHand StraightFlush hand && rank (getHighCard hand) == Ace

isStraight :: Hand -> Bool
isStraight (Hand cards) = length ranks == handSize && head ranks == minimum ranks + handSize - 1
  where
    ranks = map rank cards