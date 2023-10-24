 

import java.util.*;  //for Arrays.sort()



public class PokerHand implements Comparable

{



   private Card[] cards;

   private int rank;

   

   public PokerHand(Card card1, Card card2, Card card3, Card card4, Card card5)

   {

      cards = new Card[5];

      cards[0] = card1;

      cards[1] = card2;

      cards[2] = card3;

      cards[3] = card4;

      cards[4] = card5;

      rank = calculateRank();

   }



   public PokerHand(Card[] c)

   {

      cards = c;

      rank = calculateRank();

   }



   public int getRank(){

      return rank;

   }  



   public void sortHand(){

      Arrays.sort(cards);

   }



   public int calculateRank(){

      int temp = 0;



      if(isPair())

         temp = 1;

      if(isTwoPair())

         temp = 2;

      if(isThreeOfAKind())

         temp = 3;

      if(isStraight())

         temp = 4;

      if(isFlush())

         temp = 5;

      if(isFullHouse())

         temp = 6;

      if(isFourOfAKind())

         temp = 7;

      if(isStraightFlush())

         temp = 8;

    

      return temp;

   }



   public int compareTo(Object otherHand){

      if(otherHand.getClass().equals(this.getClass())){  //not sure I need this

         int thisRank = ((PokerHand)this).getRank();

         int otherRank = ((PokerHand)otherHand).getRank(); 

         if(thisRank != otherRank){ 

            return thisRank - otherRank;

         }



         //Create two new integer arrays to keep track of the 

         //values of the cards in each hand.  Will use these arrays

         //for most of the tie-breaker cases below.



         int[] thisCardValues = new int[13];

         for(int i = 0; i < ((PokerHand)this).cards.length; i++){

            thisCardValues[((PokerHand)this).cards[i].getValue() - 1]++;

         }

         int[] otherCardValues = new int[13];

         for(int i = 0; i < ((PokerHand)otherHand).cards.length; i++){

            otherCardValues[((PokerHand)otherHand).cards[i].getValue() - 1]++;

         }



         switch(thisRank){  //Break a tie between two hands with same rank

            case 0:   //two hands with "high card" only

            {

               //compare kickers (all five cards can be considered to be kickers here)

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);              

            }

            case 1:  //two hands each with one pair

            {

               int thisLoc = 12;

               int otherLoc = 12;

               while(thisCardValues[thisLoc]!=2){  //find pair in this hand

                  thisLoc--;

               }

               while(otherCardValues[otherLoc]!=2){  //find pair in other hand

                  otherLoc--;

               }

               if(thisLoc > otherLoc){  //compare pairs

                     return 1;    

               }

               if(otherLoc > thisLoc){

                  return -1;

               }

               //if pairs are same, compare kickers:

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);                 

            }

            case 2:  //two hands each with two pair

            {

               int thisLoc = 12;

               int otherLoc = 12;

               for(int i = 0; i < 2; i++){  //for each of the two pairs

                  while(thisCardValues[thisLoc]!=2){  //find next highest pair

                     thisLoc--;                       //in this hand

                  }

                  while(otherCardValues[otherLoc]!=2){  //find next highest pair

                     otherLoc--;                        //in other hand

                  }

                  if(thisLoc > otherLoc){    //compare next highest pairs

                       return 1;    

                  }

                  if(otherLoc > thisLoc){

                     return -1;

                  }

                  thisLoc--;  //get ready to look for the other pair if this

                  otherLoc--; //pair was the same in both hands

               }

               //if both pairs are same in both hands, compare kickers:

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);                                    

            }

            case 3:  //two hands each with three of a kind

            {

               int thisLoc = 12;

               int otherLoc = 12;

               while(thisCardValues[thisLoc]!=3){  //find three of a kind

                  thisLoc--;

               }

               while(otherCardValues[otherLoc]!=3){  //find three of a kind

                  otherLoc--;

               }

               if(thisLoc > otherLoc){  //compare three of a kinds

                     return 1;    

               }

               if(otherLoc > thisLoc){

                  return -1;

               }

               //if three of a kinds are the same value, compare the kickers:

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);

            }

            case 4:  //two hands each with a straight

            {

               //compare kickers (all five cards can be considered to be kickers here)

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);

            }

            case 5:  //two hands each with a flush

            {

               //compare kickers (all five cards can be considered to be kickers here)

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);



            }

            case 6:  //two hands each with a full house

            {

               int thisLoc = 12;

               int otherLoc = 12;

               while(thisCardValues[thisLoc]!=3){  //find the three of a kind

                  thisLoc--;

               }

               while(otherCardValues[otherLoc]!=3){  //find three of a kind

                  otherLoc--;

               }

               if(thisLoc > otherLoc){  //compare values of the three of a kinds

                     return 1;    

               }

               if(otherLoc > thisLoc){

                  return -1;

               }



               //if three of a kinds are the same, look at the pairs: 

               thisLoc = 12;

               otherLoc = 12;

               while(thisCardValues[thisLoc]!=2){  //find pair

                  thisLoc--;

               }

               while(otherCardValues[otherLoc]!=2){  //find pair

                  otherLoc--;

               }

               if(thisLoc > otherLoc){  //compare pairs

                     return 1;    

               }

               if(otherLoc > thisLoc){

                  return -1;

               }

               return 0;  //if three of a kinds and pairs have same value, return 0

            }

            case 7:  //two hands each with four of a kind

            {

               int thisLoc = 12;

               int otherLoc = 12;

               while(thisCardValues[thisLoc]!=4){  //find four of a kind in this hand

                  thisLoc--;

               }

               while(otherCardValues[otherLoc]!=4){  //find four of a kind in other hand

                  otherLoc--;

               }

               if(thisLoc > otherLoc){  //compare four of a kinds

                     return 1;    

               }

               if(otherLoc > thisLoc){

                  return -1;

               }

               //Otherwise, compare kickers:

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);

            }

            case 8:  //two hands each with a straight flush

            {

               //compare kickers (all five cards can be considered to be kickers here)

               return ((PokerHand)this).compareKickers((PokerHand)otherHand);

            }

        }      

        return 0;  //to keep compiler happy

      }

      else{  //not sure I need this either...

         throw new ClassCastException("Not a poker hand.");  

      }

   }



   private int compareKickers(PokerHand otherHand){

      //Create two new integer arrays to keep track of the 

      //values of the cards in each hand. 



      int[] thisCardValues = new int[13];

      for(int i = 0; i < ((PokerHand)this).cards.length; i++){

         thisCardValues[((PokerHand)this).cards[i].getValue() - 1]++;

      }

      int[] otherCardValues = new int[13];

      for(int i = 0; i < ((PokerHand)otherHand).cards.length; i++){

         otherCardValues[((PokerHand)otherHand).cards[i].getValue() - 1]++;

      }



      int thisLoc = 12;

      int otherLoc = 12;

      int numKickers = 5;  //by default, all cards are "kickers"

      int rank = this.getRank();



      //different kinds of hands have different numbers of kickers:

      if(rank == 1)

         numKickers = 3;

      if(rank == 2)

         numKickers = 1;

      if(rank == 3)

         numKickers = 2;

      if(rank == 6)

         numKickers = 0;

      if(rank == 7)

         numKickers = 1;

      

      for(int i = 0; i < numKickers; i++){//for each of the kickers in each hand

         while(thisCardValues[thisLoc]!=1){

            thisLoc--;  //find next highest kicker in this hand

         }

         while(otherCardValues[otherLoc]!=1){

            otherLoc--;  //find next highest kicker in other hand

         }

         if(thisLoc > otherLoc){  //compare the two kickers

            return 1;    

         }

         if(otherLoc > thisLoc){

            return -1;

         }

         thisLoc--;  //the cards above had the same value, so 

         otherLoc--; //get ready to find each hand's next highest kicker

      }

      return 0;  //all kickers had the same value, return zero for a tie.

   }//end compareKickers()



   public String toString()

   {

      String temp = "Card 1: ";

      for(int i = 0; i < cards.length - 1; i++)

      {

         temp += (cards[i] + "\nCard " + (i+2) + ": "); 

      }

      temp += cards[cards.length - 1];

      return temp;

   }

   

   public boolean hasAce(){

      boolean temp = false;

      for(int i = 0; i < cards.length; i++){

         if(cards[i].getValue()==13){

            temp = true;

         }

      }

      return temp;

   }

   

   public void exchange(int card, Card newCard){

      cards[card - 1] = newCard;

      rank = calculateRank();

   }



   //Returns true if the PokerHand is a flush (and not a straight flush)

   //false otherwise.

   public boolean isFlush()

   {

      //check if all the suits are the same:

      if(cards[0].getSuit().equals(cards[1].getSuit()) && 

         cards[0].getSuit().equals(cards[2].getSuit()) &&

         cards[0].getSuit().equals(cards[3].getSuit()) && 

         cards[0].getSuit().equals(cards[4].getSuit()))

      {

          if(!isStraightFlush())  //if all five suits are same and not a straight

          {

            return true;

          }

      } 

      return false;  //otherwise it's not a flush

   }

   

   //Returns true if PokerHand is a straight (and not a straight flush) 

   //returns false otherwise.



   public boolean isStraight()

   {

       int[] cardValues = new int[13];

       for(int i = 0; i < cards.length; i++){

          cardValues[cards[i].getValue() - 1]++;

       }

       int firstValue = -1, inARow = 0, lastValue = -5;

       for(int i = 0; i < cardValues.length; i++){

          if(firstValue == -1 && cardValues[i] != 0){

             firstValue = i;

             lastValue = firstValue;

             inARow++;

          }

          if(cardValues[i] != 0 && (i - 1) == lastValue){

             lastValue = i;

             inARow++;

          }  

       }

       if(inARow == 5)  //if it's a straight, make sure it's not a flush:

       {

         if(!isStraightFlush())

         {

           return true;

         }

       }

       return false;

   }

   

   //Returns true if PokerHand is four of a kind

   //false otherwise.

   public boolean isFourOfAKind(){

      int[] cardValues = new int[13];

      for(int i = 0; i < cards.length; i++){

         cardValues[cards[i].getValue() - 1]++;

      }

      for(int i = 0; i < cardValues.length; i++){

         if(cardValues[i] == 4){

            return true;

         }

      }

      return false;

   }

   

   //Returns true if PokerHand is three of a kind (and not 4 of a kind)

   //false otherwise.

   public boolean isThreeOfAKind(){

      int[] cardValues = new int[13];

      for(int i = 0; i < cards.length; i++){

         cardValues[cards[i].getValue() - 1]++;

      }

      for(int i = 0; i < cardValues.length; i++){

         if(cardValues[i] == 3){

            if(!isFullHouse())

            {   

               return true;

            }

         }

      }

      return false;

   }

 

   //Returns true if PokerHand is one pair (not two pair and not 3 or 4 of a kind

   //and not Full House) false otherwise:

   public boolean isPair(){

      int[] cardValues = new int[13];

      for(int i = 0; i < cards.length; i++){

         cardValues[cards[i].getValue() - 1]++;

      }

      for(int i = 0; i < cardValues.length; i++){

         if(cardValues[i] == 2){

            if(!isTwoPair() && !isFullHouse())

            {   

               return true;

            }

         }

      }

      return false;

   }



   //Returns true if PokerHand is two pair (not full house)

   //false otherwise.

   public boolean isTwoPair(){

      int[] cardValues = new int[13];

      int numberOfPairs = 0;

      for(int i = 0; i < cards.length; i++){

         cardValues[cards[i].getValue() - 1]++;

      }

      for(int i = 0; i < cardValues.length; i++){

         if(cardValues[i] == 2){

            numberOfPairs++;

         }

      }

      if(numberOfPairs == 2){

        return true;

      }

      return false;

   }



   //Returns true if PokerHand is a Full House

   //false otherwise

   public boolean isFullHouse(){

      int[] cardValues = new int[13];

      boolean hasThreeOfAKind = false;

      boolean hasAPair = false;

      for(int i = 0; i < cards.length; i++){

         cardValues[cards[i].getValue() - 1]++;

      }

      for(int i = 0; i < cardValues.length; i++){

         if(cardValues[i] == 3){

            hasThreeOfAKind = true;

         }

         if(cardValues[i] == 2){

            hasAPair = true;

         }

      }

      if(hasAPair && hasThreeOfAKind){

         return true;

      }

      return false;

   }

   

   //returns true if PokerHand is a straight flush

   public boolean isStraightFlush(){

      if(cards[0].getSuit().equals(cards[1].getSuit()) &&  //if flush

         cards[0].getSuit().equals(cards[2].getSuit()) &&

         cards[0].getSuit().equals(cards[3].getSuit()) && 

         cards[0].getSuit().equals(cards[4].getSuit()))

      {  

         //check if it's a straight

         int[] cardValues = new int[13];

         for(int i = 0; i < cards.length; i++){

            cardValues[cards[i].getValue() - 1]++;

         }

         int firstValue = -1, inARow = 0, lastValue = -5;

         for(int i = 0; i < cardValues.length; i++){

            if(firstValue == -1 && cardValues[i] != 0){

               firstValue = i;

               lastValue = firstValue;

               inARow++;

            }

            if(cardValues[i] != 0 && (i - 1) == lastValue){

               lastValue = i;

               inARow++;

            }  

         }

         if(inARow == 5)  //it's a straight and a flush so

            return true;  

      }

      return false;  //it's not a straight or not a flush

   }



   public static void main(String[] args)

   {

      DeckOfCards myDeck = new DeckOfCards();

      myDeck.Shuffle();

      PokerHand myHand = new PokerHand(myDeck.Deal(),myDeck.Deal(),myDeck.Deal(),

         myDeck.Deal(),myDeck.Deal());

      Arrays.sort(myHand.cards);

      PokerHand otherHand = new PokerHand(myDeck.Deal(),myDeck.Deal(),myDeck.Deal(),

         myDeck.Deal(),myDeck.Deal());

      Arrays.sort(otherHand.cards);

      System.out.println(myHand);

      System.out.println("rank = " + myHand.getRank());

      if(myHand.compareTo(otherHand)>0){

         System.out.println("Beats");

      }

      else if(myHand.compareTo(otherHand)<0){

         System.out.println("Loses to");

      }

      else{

         System.out.println("Ties with");

         System.out.println(otherHand);

         System.out.println("rank = " + otherHand.getRank());

      }

      System.out.println(otherHand);

      System.out.println("rank = " + otherHand.getRank());

   }//end main()

}//end PokerHand







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
groupCards cards = groupBy (\x y -> value x == value y) (sort cards)



