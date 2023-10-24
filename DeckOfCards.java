public class DeckOfCards

{

   private Card[] cards;

   private int topCard;



   public DeckOfCards()

   {

      cards = new Card[52];

      topCard = 51;  //set the top card to be the card in index 51



      String[] suits = {"Hearts", "Clubs", "Diamonds", "Spades"};



      for(int value = 1; value <= 13; value++)

      {

         for(int mySuit = 0; mySuit < suits.length; mySuit++)

         { 

            cards[4*(value - 1) + mySuit] = new Card(value, suits[mySuit]);

         }

      }

   }





   //Input:  none

   //Output:  The card objects in the DeckOfCards calling object's 

   //   array of cards are placed in random order, and topCard is reset

   //   to 51

   public void Shuffle()

   {

      topCard = 51;  //reset top card.

      for(int i = 0; i < 500; i++)

      {

         int firstCardIndex = (int) (52*Math.random());

         int secondCardIndex = (int) (52*Math.random());

         Card temp = cards[secondCardIndex];

         cards[secondCardIndex] = cards[firstCardIndex];

         cards[firstCardIndex] = temp;

      }

 

   }



   //Input:  none

   //Output:  returns next card in deck (top card)

   //   Returns null if no cards remain in deck.



   public Card Deal()

   {

      if(topCard >= 0)

      {

         return cards[topCard--];

      }

      else

      {

         return null;

      }   

   }



   public static void main(String[] args){

      DeckOfCards myDeck = new DeckOfCards();

      myDeck.Shuffle();

      for(int i = 0; i < myDeck.cards.length + 1; i++){

         System.out.println(i + 1 + " -- " + myDeck.Deal());

      }

      

   } 



}