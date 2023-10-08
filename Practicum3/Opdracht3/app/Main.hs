module Main (main) where

import Lib




import Data.List (foldl') -- '
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM, forM_)
    
import Control.Monad.Cont

class Hands a where
  size          :: a -> Float
  circumference :: a -> Float

  toString      :: IO ()

data Hand = Poker {pos :: (Float, Float), radius :: Float} |
			Four_of_a_kind {pos :: (Float, Float), radius :: Float} |
			Three_of_a_kind {pos :: (Float, Float), radius :: Float} |
			Full_house {pos :: (Float, Float), radius :: Float} |
			Two_pair {pos :: (Float, Float), radius :: Float} |
			One_pair {pos :: (Float, Float), radius :: Float} |
			Straight {pos :: (Float, Float), radius :: Float} |
             Bust {pos :: (Float, Float), width :: Float}

instance Hands Hand where
  size (Circle _ radius)              = pi * radius ^ 2
  circumference (Square _ width)      = 4 * width
  toString              = putStrLn (" Square drawn @ " ++ show pos ++ " with width " ++ show width)

instance Show Hand where
  show (Poker pos radius)     = "Circle with radius " ++ show radius ++ " @ " ++ show pos
  show (Four_of_a_kind pos width height) = "Rect (w,h)" ++ show (width, height) ++ " @ " ++ show pos
  show (Three_of_a_kind pos width height)      = "Square with edge " ++ show width ++ " @ " ++ show pos
  show (Full_house pos width)      = "Square with edge " ++ show width ++ " @ " ++ show pos

 


listOfOjects :: [Shape]
listOfOjects = [(Circle (20,20) 5),(Rect (20,20) 4 7),(Square (20,20) 8) ]


isCircle :: Shape  -> Bool
isCircle (Circle _ _) = True
isCircle _ = False

isRect :: Shape  -> Bool
isRect (Rect _ _ _) = True
isRect _ = False


isSquare :: Shape  -> Bool
isSquare (Square _ _) = True
isSquaree _ = False



--isTriangle :: Shape  -> Bool
--isTriangle (Triangle _ _ _) = True
--isTriangle _ = False


databaseCircles = filter isCircle listOfOjects
--databaseRectangles = filter isRectangle listOfOjects
--databaseTriangles= filter isTriangle listOfOjects

list_edit []=[]
list_edit (x:xs)= if isCircle x
                  then circumference x:list_edit(xs)
                  else if isRect x 
                    then circumference x:list_edit(xs)
                  else circumference x:list_edit(xs)


main=print("Changed List ",list_edit[(Circle (20,20) 5),(Rect (20,20) 4 7),(Square (20,20) 8) ])
{-

main = do
    forM_ listOfOjects $ \i -> do
        print i

    forM_ [7..9] $ \j -> do
        print j

    withBreak $ \break ->
        forM_ [1..] $ \_ -> do
            p "loop"
            break ()

    where
    withBreak = (`runContT` return) . callCC
    p = liftIO . putStrLn
    
    -}

-- *Main> let c1 = Circle (20,20) 5
-- *Main> draw c1
 -- Circle drawn @ (20.0,20.0) with radius 5.0
-- *Main> let c2 = c1 {pos = (10,10)}
-- *Main> draw c2
 -- Circle drawn @ (10.0,10.0) with radius 5.0
-- *Main> draw c1 -- c1 is immutable
 -- Circle drawn @ (20.0,20.0) with radius 5.0
-- *Main> area c1
-- 78.53982


{-
main :: IO ()
main = someFunc
-}



 Casino poker




{-
https://github.com/andyxhadji/Simple-Poker/blob/master/Player.java
https://www.cs.princeton.edu/courses/archive/fall14/cos126/docs/PokerHand.java.html
https://stackoverflow.com/questions/32643124/running-java-poker-game
https://www.codeproject.com/Articles/38821/Make-a-poker-hand-evalutator-in-Java
https://www.coderscampus.com/java-practice-assignment-6/
http://www.cs.williams.edu/~freund/cs136-053/lectures/lecture2/Poker/PokerHand.java
https://jkrupnicki.wordpress.com/java-programs/java-poker-game/
https://www.instructables.com/How-to-Make-a-Poker-Game-in-Java/
https://www.cs.mcgill.ca/~gkazam/cs303/
https://coderanch.com/t/736696/java/Draw-Poker-Java
https://codereview.stackexchange.com/questions/65134/basic-oop-poker-deck-cards-and-hands
http://www.cs.cornell.edu/courses/cs100/2003su/assignment5/solution/PokerHand.java
https://www.mycompiler.io/view/IrwBmZt
https://copyprogramming.com/howto/poker-game-straight-in-java
https://www.daniweb.com/programming/software-development/threads/194495/using-inheritance-to-make-a-poker-game
https://easycodestuff.blogspot.com/2014/07/card-deck-hand-class-in-poker-game.html
https://mblogscode.wordpress.com/2016/11/23/texas-holdem-poker-in-java-part-1-cards-decks-and-hands/

-}
-- import Data.List (foldl') -- '
-- import Data.STRef
-- import Control.Monad.ST
-- import Control.Monad (forM, forM_)
-- import Control.Monad.Cont
-- import Lib
-- import Data.List (foldl') -- '
-- import Data.STRef
-- import Control.Monad.ST
-- import Control.Monad (forM, forM_)
-- import Control.Monad.Cont
{-
public class Card{
    private short rank, suit;

    private static String[] suits = { "hearts", "spades", "diamonds", "clubs" };
    private static String[] ranks  = { "Ace", "2", "3", "4", "5", "6", "7", 
                                       "8", "9", "10", "Jack", "Queen", "King" };

    public static String rankAsString( int __rank ) {
        return ranks[__rank];
    }

    Card(short suit, short rank)
    {
        this.rank=rank;
        this.suit=suit;
    }

    public @Override String toString()
    {
          return ranks[rank] + " of " + suits[suit];
    }

    public short getRank() {
         return rank;
    }

    public short getSuit() {
        return suit;
    }
}
-}
-- class Cards a where
  -- --ranks :: [String]
  -- --ranks = ["hearts", "spades", "diamonds", "clubs" ]
  -- --ranks :: [String]
  -- --ranks = ["Ace", "2", "3", "4", "5", "6", "7",  "8", "9", "10", "Jack", "Queen", "King" ]
  -- cardsToString      :: a ->IO ()
{-
    Deck()
    {
        cards = new ArrayList<Card>();
        int index_1, index_2;
        Random generator = new Random();
        Card temp;

        for (int a=1; a<=4; a++)
        {
            for (int b=1; b<=13; b++)
             {
               cards.add( new Card(a,b) );
             }
        }

       int size       

        for (int i=0; i<100; i++)
        {
            index_1 = generator.nextInt( cards.size() - 1 );
            index_2 = generator.nextInt( cards.size() - 1 );

            temp = cards.get( index_2 );
            cards.set( index_2 , cards.get( index_1 ) );
            cards.set( index_1, temp );
        }
    }
    public Card drawFromDeck()
    public int getTotalCards()
    
-}  
-- class Decks a where
  -- deck_size          :: a -> IO ()
  -- fillDeck      :: a ->IO ()
  -- shuffle      :: a ->IO ()
  -- deal      :: a ->IO ()
  -- redeal      :: a ->IO ()
  -- shuffle_exanges      :: a ->IO ()
  -- hand_size      :: a ->IO ()
  -- restOfDeck      :: a ->IO ()
{-
 private Card[] cards;

   private int rank;
   public PokerHand(Card card1, Card card2, Card card3, Card card4, Card card5)
   public PokerHand(Card[] c)
   public int getRank(){
   public void sortHand(){
   public int calculateRank(){
   public int compareTo(Object otherHand){
   private int compareKickers(PokerHand otherHand){
   public String toString()
   public boolean hasAce(){
   public void exchange(int card, Card newCard){
   public boolean isFlush()
   public boolean isStraight()
   public boolean isFourOfAKind(){
   public boolean isThreeOfAKind(){
   public boolean isPair(){
   public boolean isTwoPair(){
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
   public boolean isStraightFlush(){

-}
-- class Hands a where
  -- size          :: a -> Float
  -- toString      :: a ->IO ()
  -- display      :: a ->IO ()
  -- displayAll      :: a ->IO ()
  -- compareTo      :: a ->IO () -- Object otherHand

-- {-

-- -}  
-- class Players a where
-- --  name      :: a ->IO  ()
  -- draw          :: a -> IO ()
  -- redraw      :: a ->IO ()

-- class Games a where
  -- play          ::a ->IO()
  -- makeHand      ::a ->  IO()
  -- checkHand      ::a ->  IO()
  -- gameRedraw      ::a ->  IO()
  -- evaluate      ::a ->  IO()

-- data Card = Ruiten {kaartid :: Float} 
             
-- data Deck = RuitenSet {aantal :: Float}              

-- data Hand = Poker {handsize :: Float} |
			-- Four_of_a_kind {pos :: (Float, Float), width :: Float, height :: Float} |
			-- Three_of_a_kind {pos :: (Float, Float), radius :: Float} |
			-- Full_house {pos :: (Float, Float), radius :: Float} |
			-- Two_pair {pos :: (Float, Float), radius :: Float} |
			-- One_pair {pos :: (Float, Float), radius :: Float} |
			-- Straight {pos :: (Float, Float), radius :: Float} |
             -- Bust {pos :: (Float, Float), width :: Float}
             
-- data Player = Speler {playersName :: String} 

-- data Game = Ronde {rondeNummer :: Float} 


-- instance Cards Card where
  -- cardsToString  ( _)            = putStrLn (" Square drawn @ " )



-- instance Show Card where
  -- show (Ruiten kaartid )     = "Circle with radius " 

  
  
-- instance Decks Deck where
  -- deck_size (RuitenSet  aantal)          = putStrLn (" Square drawn @ "++ show aantal )
  -- fillDeck (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )
  -- shuffle (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )
  -- deal  (RuitenSet  aantal)    = putStrLn (" Square drawn @ " )
  -- redeal  (RuitenSet  aantal)    = putStrLn (" Square drawn @ " )
  -- shuffle_exanges  (RuitenSet  aantal)    = putStrLn (" Square drawn @ " )
  -- hand_size (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )
  -- restOfDeck (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )


-- instance Show Deck where
  -- show (RuitenSet aantal)     = "Aantal with radius " ++ show aantal 


-- instance Hands Hand where
  -- size (Poker  handsize)              = handsize
  -- toString (Poker  radius)             = putStrLn (" Square drawn  " )
  -- toString (Four_of_a_kind pos width heigh)             = putStrLn (" Square drawn @ " )
  -- toString (Three_of_a_kind pos width )             = putStrLn (" Square drawn @ " )
  -- toString (Full_house pos radius)             = putStrLn (" Square drawn @ pos @ radius" )
  -- toString (Two_pair pos radius)             = putStrLn (" Square drawn @ " )
  -- toString (One_pair pos radius)             = putStrLn (" Square drawn @ " )
  -- toString (Straight pos radius)             = putStrLn (" Square drawn @ " )
  -- toString (Bust pos radius)             = putStrLn (" Square drawn @ " )
  -- display   (Bust pos radius)             = putStrLn (" Square drawn @ " )
  -- displayAll (Bust pos radius)             = putStrLn (" Square drawn @ " )
  -- compareTo (Bust pos radius)             = putStrLn (" Square drawn @ " )

-- instance Show Hand where
  -- show (Poker handsize)     = "Poker " ++ show handsize  
  -- show (Four_of_a_kind pos width height) = "Hand is a Four_of_a_kind" ++ show (width, height) ++ " @ " ++ show pos
  -- show (Three_of_a_kind pos width)      = "Hand is a Three_of_a_kind " ++ show width ++ " @ " ++ show pos
  -- show (Full_house pos radius)      = "Hand is a Full_house "  ++ show radius ++ " @ " ++ show pos



-- instance Players Player where
-- --  name (Speler  playersName)              = putStrLn (" Square drawn @ playersName" )
  -- draw (Speler playersName)             = putStrLn (" Square drawn @ playersName" )
  -- redraw (Speler playersName)             = putStrLn (" Square drawn @ playersName" )


-- instance Show Player where
  -- show (Speler playersName)     =  "Square with edge "   ++ show playersName 

  
  
  {-
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
  -}
-- instance Games Game where
  -- play (Ronde  rondeNummer)      = putStrLn (" Square drawn  " )
  -- makeHand  (Ronde  rondeNummer)    = putStrLn (" Square drawn  @ rondeNummer" )
  -- checkHand (Ronde  rondeNummer)     = putStrLn (" Square drawn  @ rondeNummer" )
  -- gameRedraw (Ronde  rondeNummer)     = putStrLn (" Square drawn  @ rondeNummer" )
  -- evaluate (Ronde  rondeNummer)     = putStrLn (" Square drawn @ rondeNummer " )


-- instance Show Game where
  -- show (Ronde rondeNummer)     = "Circle with radius " ++ show rondeNummer  

{-
foo :: a -> ()
foo _ = ()

bar :: a -> b -> ()
bar _ _ = ()


main :: IO ()
main = putStrLn "Hello, world!"
-}



{-
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
  
  -}

-- listOfOjects :: [Player]
-- listOfOjects = [(Speler "Henk"),(Speler "Richard"),(Speler "Jan") ]

{-
isCircle :: Shape  -> Bool
isCircle (Circle _ _) = True
isCircle _ = False

isRect :: Shape  -> Bool
isRect (Rect _ _ _) = True
isRect _ = False


isSquare :: Shape  -> Bool
isSquare (Square _ _) = True
isSquaree _ = False
-}


--isTriangle :: Shape  -> Bool
--isTriangle (Triangle _ _ _) = True
--isTriangle _ = False


--databaseCircles = filter isCircle listOfOjects
--databaseRectangles = filter isRectangle listOfOjects
--databaseTriangles= filter isTriangle listOfOjects

{-
list_edit []=[]
list_edit (x:xs)= if isCircle x
                  then circumference x:list_edit(xs)
                  else if isRect x 
                    then circumference x:list_edit(xs)
                  else circumference x:list_edit(xs)
-}

--main=print("Changed List ",list_edit[(Circle (20,20) 5),(Rect (20,20) 4 7),(Square (20,20) 8) ])
--main=print("Changed List ")
{-

main = do
    forM_ listOfOjects $ \i -> do
        print i

    forM_ [7..9] $ \j -> do
        print j

    withBreak $ \break ->
        forM_ [1..] $ \_ -> do
            p "loop"
            break ()

    where
    withBreak = (`runContT` return) . callCC
    p = liftIO . putStrLn
    
    -}

-- *Main> let c1 = Circle (20,20) 5
-- *Main> draw c1
 -- Circle drawn @ (20.0,20.0) with radius 5.0
-- *Main> let c2 = c1 {pos = (10,10)}
-- *Main> draw c2
 -- Circle drawn @ (10.0,10.0) with radius 5.0
-- *Main> draw c1 -- c1 is immutable
 -- Circle drawn @ (20.0,20.0) with radius 5.0
-- *Main> area c1
-- 78.53982


{-
main :: IO ()
main = someFunc
-}


 



{-
https://github.com/andyxhadji/Simple-Poker/blob/master/Player.java
https://www.cs.princeton.edu/courses/archive/fall14/cos126/docs/PokerHand.java.html
https://stackoverflow.com/questions/32643124/running-java-poker-game
https://www.codeproject.com/Articles/38821/Make-a-poker-hand-evalutator-in-Java
https://www.coderscampus.com/java-practice-assignment-6/
http://www.cs.williams.edu/~freund/cs136-053/lectures/lecture2/Poker/PokerHand.java
https://jkrupnicki.wordpress.com/java-programs/java-poker-game/
https://www.instructables.com/How-to-Make-a-Poker-Game-in-Java/
https://www.cs.mcgill.ca/~gkazam/cs303/
https://coderanch.com/t/736696/java/Draw-Poker-Java
https://codereview.stackexchange.com/questions/65134/basic-oop-poker-deck-cards-and-hands
http://www.cs.cornell.edu/courses/cs100/2003su/assignment5/solution/PokerHand.java
https://www.mycompiler.io/view/IrwBmZt
https://copyprogramming.com/howto/poker-game-straight-in-java
https://www.daniweb.com/programming/software-development/threads/194495/using-inheritance-to-make-a-poker-game
https://easycodestuff.blogspot.com/2014/07/card-deck-hand-class-in-poker-game.html
https://mblogscode.wordpress.com/2016/11/23/texas-holdem-poker-in-java-part-1-cards-decks-and-hands/


https://stackoverflow.com/questions/46395396/calculating-standard-deviation-in-haskell
https://stackoverflow.com/questions/26533045/why-can-you-define-function-without-parameter-in-haskell
https://stackoverflow.com/questions/51460945/returning-a-subset-of-types-in-haskell
https://stackoverflow.com/questions/49546210/haskell-function-that-returns-sublists-of-a-list
https://chercher.tech/haskell/list
https://ggbaker.ca/prog-langs/content/functional.html
https://stackoverflow.com/questions/22882835/creating-figures-in-haskell
https://stackoverflow.com/questions/11574157/how-to-write-surface-function-for-shape-circle-with-decleration-of-data-type-r
https://stackoverflow.com/questions/22935071/filtering-list-based-on-items-with-record-syntax
https://stackoverflow.com/questions/44202092/haskell-how-to-implement-a-processing-on-a-list-of-different-but-related-types
https://stackoverflow.com/questions/52907501/creating-list-of-values-of-the-same-typeclass-but-different-types?rq=1
https://stackoverflow.com/questions/50473205/how-do-i-use-if-then-else-statement-with-no-else-condition-in-haskell
https://www2.ki.informatik.uni-frankfurt.de/doc/html/Haskell1.3/exps.html
https://mail.haskell.org/pipermail/haskell-cafe/2005-December/013311.html
https://github.com/tweag/guides/blob/master/style/Haskell.md
https://stackoverflow.com/questions/51279298/haskell-sum-up-the-first-n-elements-of-a-list
https://stackoverflow.com/questions/65652852/how-to-sum-elements-of-two-lists-haskell
https://cs.fit.edu/~ryan/cse4250/haskell-syntax.html
https://www.haskellforall.com/2013/08/composable-streaming-folds.html





https://stackoverflow.com/questions/24202756/how-to-define-and-use-global-array-in-haskell

https://downloads.haskell.org/~ghc/7.0.1/docs/html/users_guide/type-class-extensions.html

https://downloads.haskell.org/ghc/7.8.4/docs/html/users_guide/type-class-extensions.html

https://en.m.wikibooks.org/wiki/Haskell/Classes_and_types

http://learnyouahaskell.com/making-our-own-types-and-typeclasses

https://joelburget.com/data-newtype-instance-class/

https://book.realworldhaskell.org/read/using-typeclasses.html

https://www.cse.chalmers.se/edu/year/2016/course/TDA452/lectures/OverloadingAndTypeClasses.html

https://www.cis.upenn.edu/~cis1940/fall16/lectures/04-typeclasses.html

https://medium.com/geekculture/a-random-tour-of-typeclass-in-haskell-87a5a2125e1a

https://www.classes.cs.uchicago.edu/archive/2012/spring/22300-1/lectures/typeclass.txt

https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/classes.html

https://mmhaskell.com/blog/2018/1/1/general-functions-with-typeclasses

https://tgdwyer.github.io/haskell2/


Haskell oop
https://stackoverflow.com/questions/34641364/instances-and-class-in-haskell
https://stackoverflow.blog/2020/09/02/if-everyone-hates-it-why-is-oop-still-so-widely-spread/

https://gist.github.com/sacundim/8511f98d6173d8d46533

https://hackingwithhaskell.com/basics/defining-constants/

https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/5-type-classes

https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/classes.html

https://book.realworldhaskell.org/read/using-typeclasses.html

https://github.com/gibiansky/haskell-course-notes/blob/master/typeclasses.adoc

https://www.reddit.com/r/haskellquestions/comments/4k4bs7/constant_in_a_typeclass/

https://downloads.haskell.org/~ghc/7.0.1/docs/html/users_guide/type-class-extensions.html

https://stackoverflow.com/questions/74479429/selecting-a-type-class-instance-based-on-the-context-once-again



https://github.com/msabramo/Haskell-Poker
https://hackage.haskell.org/package/general-games-1.1.1/docs/Game-Game-Poker.html
https://codereview.stackexchange.com/questions/249913/poker-hand-evaluator-in-haskell
https://hackage.haskell.org/package/java-poker-0.1.2.0/docs/src/Game-Poker-Hands.html




  import           Data.List
  import           System.IO
 
  -- simple algebraic data types for card values and suites
  data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack\
   | Queen | King | Ace
    deriving (Eq, Enum)
  data CardSuite = Clubs | Spades | Diamonds | Hearts deriving (Eq, Enum)
  
 -- our card type - merely combining CardValue and CardSuite
 data Card = Card CardValue CardSuite deriving(Eq)
 
 -- synonym for list of cards to store decks
 type Deck = [Card]
 
 instance Show CardValue where
   show c = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"] !! (fr\
 omEnum c)
 
 instance Show CardSuite where
   show Spades   = "♠"
   show Clubs    = "♣"
   show Diamonds = "♦"
   show Hearts   = "♥"
 
 -- defining show function that is a little nicer then default
 instance Show Card where
   show (Card a b) = show a ++ show b
 
 -- defining full deck of cards via comprehension; how cool is that?! :)
 fullDeck :: Deck
 fullDeck = [ Card x y | y <- [Clubs .. Hearts], x <- [Two .. Ace] ]
 
 smallDeck = [Card Ace Spades, Card Two Clubs, Card Jack Hearts]
 
 main = print smallDeck >> putStrLn "Press Enter to deal the full deck" >> getLine >>\
  mapM_ print fullDeck

-}
-- import Data.List (foldl') -- '
-- import Data.STRef
-- import Control.Monad.ST
-- import Control.Monad (forM, forM_)
-- import Control.Monad.Cont
-- import Lib
-- import Data.List (foldl') -- '
-- import Data.STRef
-- import Control.Monad.ST
-- import Control.Monad (forM, forM_)
-- import Control.Monad.Cont
{-
public class Card{
    private short rank, suit;

    private static String[] suits = { "hearts", "spades", "diamonds", "clubs" };
    private static String[] ranks  = { "Ace", "2", "3", "4", "5", "6", "7", 
                                       "8", "9", "10", "Jack", "Queen", "King" };

    public static String rankAsString( int __rank ) {
        return ranks[__rank];
    }

    Card(short suit, short rank)
    {
        this.rank=rank;
        this.suit=suit;
    }

    public @Override String toString()
    {
          return ranks[rank] + " of " + suits[suit];
    }

    public short getRank() {
         return rank;
    }

    public short getSuit() {
        return suit;
    }
}
-}

-- safe divide function
-- compareToOtherCard :: Card -> Card 
-- compareToOtherCard x 0 = Nothing
-- compareToOtherCard x y = if x == 0 then 1 else n * fact (n - 1)

-- class Cards a where
  -- --ranks :: [String]
  -- --ranks = ["hearts", "spades", "diamonds", "clubs" ]
  -- --ranks :: [String]
  -- --ranks = ["Ace", "2", "3", "4", "5", "6", "7",  "8", "9", "10", "Jack", "Queen", "King" ]
  -- cardsToString      :: a ->IO ()
  -- compareToOtherCard      :: a ->IO ()
{-
    Deck()
    {
        cards = new ArrayList<Card>();
        int index_1, index_2;
        Random generator = new Random();
        Card temp;

        for (int a=1; a<=4; a++)
        {
            for (int b=1; b<=13; b++)
             {
               cards.add( new Card(a,b) );
             }
        }

       int size       

        for (int i=0; i<100; i++)
        {
            index_1 = generator.nextInt( cards.size() - 1 );
            index_2 = generator.nextInt( cards.size() - 1 );

            temp = cards.get( index_2 );
            cards.set( index_2 , cards.get( index_1 ) );
            cards.set( index_1, temp );
        }
    }
    public Card drawFromDeck()
    public int getTotalCards()
    
-}  
-- class Decks a where
  -- deck_size          :: a -> IO ()
  -- fillDeck      :: a ->IO ()
  -- shuffle      :: a ->IO ()
  -- deal      :: a ->IO ()
  -- redeal      :: a ->IO ()
  -- shuffle_exanges      :: a ->IO ()
  -- hand_size      :: a ->IO ()
  -- restOfDeck      :: a ->IO ()
{-
 private Card[] cards;

   private int rank;
   public PokerHand(Card card1, Card card2, Card card3, Card card4, Card card5)
   public PokerHand(Card[] c)
   public int getRank(){
   public void sortHand(){
   public int calculateRank(){
   public int compareTo(Object otherHand){
   private int compareKickers(PokerHand otherHand){
   public String toString()
   public boolean hasAce(){
   public void exchange(int card, Card newCard){
   public boolean isFlush()
   public boolean isStraight()
   public boolean isFourOfAKind(){
   public boolean isThreeOfAKind(){
   public boolean isPair(){
   public boolean isTwoPair(){
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
   public boolean isStraightFlush(){

-}
-- class Hands a where
  -- size          :: a -> Float
  -- toString      :: a ->IO ()
  -- display      :: a ->IO ()
  -- displayAll      :: a ->IO ()
  -- compareTo      :: a ->IO () -- Object otherHand

-- {-

-- -}  
-- class Players a where
-- --  name      :: a ->IO  ()
  -- draw          :: a -> IO ()
  -- redraw      :: a ->IO ()

-- class Games a where
  -- play          ::a ->IO()
  -- makeHand      ::a ->  IO()
  -- checkHand      ::a ->  IO()
  -- gameRedraw      ::a ->  IO()
  -- evaluate      ::a ->  IO()

-- data Card = Ruiten {kaartid :: Float} 
             
-- data Deck = RuitenSet {aantal :: Float}              

-- data Hand = Poker {handsize :: Float} |
			-- Four_of_a_kind {pos :: (Float, Float), width :: Float, height :: Float} |
			-- Three_of_a_kind {pos :: (Float, Float), radius :: Float} |
			-- Full_house {pos :: (Float, Float), radius :: Float} |
			-- Two_pair {pos :: (Float, Float), radius :: Float} |
			-- One_pair {pos :: (Float, Float), radius :: Float} |
			-- Straight {pos :: (Float, Float), radius :: Float} |
             -- Bust {pos :: (Float, Float), width :: Float}
             
-- data Player = Speler {playersName :: String} 

-- data Game = Ronde {rondeNummer :: Float} 


-- instance Cards Card where
  -- cardsToString  ( _)            = putStrLn (" Square drawn @ " )



-- instance Show Card where
  -- show (Ruiten kaartid )     = "Circle with radius " 

  
  
-- instance Decks Deck where
  -- deck_size (RuitenSet  aantal)          = putStrLn (" Square drawn @ "++ show aantal )
  -- fillDeck (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )
  -- shuffle (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )
  -- deal  (RuitenSet  aantal)    = putStrLn (" Square drawn @ " )
  -- redeal  (RuitenSet  aantal)    = putStrLn (" Square drawn @ " )
  -- shuffle_exanges  (RuitenSet  aantal)    = putStrLn (" Square drawn @ " )
  -- hand_size (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )
  -- restOfDeck (RuitenSet  aantal)     = putStrLn (" Square drawn @ " )


-- instance Show Deck where
  -- show (RuitenSet aantal)     = "Aantal with radius " ++ show aantal 


-- instance Hands Hand where
  -- size (Poker  handsize)              = handsize
  -- toString (Poker  radius)             = putStrLn (" Square drawn  " )
  -- toString (Four_of_a_kind pos width heigh)             = putStrLn (" Square drawn @ " )
  -- toString (Three_of_a_kind pos width )             = putStrLn (" Square drawn @ " )
  -- toString (Full_house pos radius)             = putStrLn (" Square drawn @ pos @ radius" )
  -- toString (Two_pair pos radius)             = putStrLn (" Square drawn @ " )
  -- toString (One_pair pos radius)             = putStrLn (" Square drawn @ " )
  -- toString (Straight pos radius)             = putStrLn (" Square drawn @ " )
  -- toString (Bust pos radius)             = putStrLn (" Square drawn @ " )
  -- display   (Bust pos radius)             = putStrLn (" Square drawn @ " )
  -- displayAll (Bust pos radius)             = putStrLn (" Square drawn @ " )
  -- compareTo (Bust pos radius)             = putStrLn (" Square drawn @ " )

-- instance Show Hand where
  -- show (Poker handsize)     = "Poker " ++ show handsize  
  -- show (Four_of_a_kind pos width height) = "Hand is a Four_of_a_kind" ++ show (width, height) ++ " @ " ++ show pos
  -- show (Three_of_a_kind pos width)      = "Hand is a Three_of_a_kind " ++ show width ++ " @ " ++ show pos
  -- show (Full_house pos radius)      = "Hand is a Full_house "  ++ show radius ++ " @ " ++ show pos



-- instance Players Player where
-- --  name (Speler  playersName)              = putStrLn (" Square drawn @ playersName" )
  -- draw (Speler playersName)             = putStrLn (" Square drawn @ playersName" )
  -- redraw (Speler playersName)             = putStrLn (" Square drawn @ playersName" )


-- instance Show Player where
  -- show (Speler playersName)     =  "Square with edge "   ++ show playersName 

  
  
  {-
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
  -}
-- instance Games Game where
  -- play (Ronde  rondeNummer)      = putStrLn (" Square drawn  " )
  -- makeHand  (Ronde  rondeNummer)    = putStrLn (" Square drawn  @ rondeNummer" )
  -- checkHand (Ronde  rondeNummer)     = putStrLn (" Square drawn  @ rondeNummer" )
  -- gameRedraw (Ronde  rondeNummer)     = putStrLn (" Square drawn  @ rondeNummer" )
  -- evaluate (Ronde  rondeNummer)     = putStrLn (" Square drawn @ rondeNummer " )


-- instance Show Game where
  -- show (Ronde rondeNummer)     = "Circle with radius " ++ show rondeNummer  

{-
foo :: a -> ()
foo _ = ()

bar :: a -> b -> ()
bar _ _ = ()


main :: IO ()
main = putStrLn "Hello, world!"
-}



{-
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
  
  -}

--listOfOjects :: [Player]
--listOfOjects = [(Speler "Henk"),(Speler "Richard"),(Speler "Jan") ]

{-
isCircle :: Shape  -> Bool
isCircle (Circle _ _) = True
isCircle _ = False

isRect :: Shape  -> Bool
isRect (Rect _ _ _) = True
isRect _ = False


isSquare :: Shape  -> Bool
isSquare (Square _ _) = True
isSquaree _ = False
-}


--isTriangle :: Shape  -> Bool
--isTriangle (Triangle _ _ _) = True
--isTriangle _ = False


--databaseCircles = filter isCircle listOfOjects
--databaseRectangles = filter isRectangle listOfOjects
--databaseTriangles= filter isTriangle listOfOjects

{-
list_edit []=[]
list_edit (x:xs)= if isCircle x
                  then circumference x:list_edit(xs)
                  else if isRect x 
                    then circumference x:list_edit(xs)
                  else circumference x:list_edit(xs)
-}

--main=print("Changed List ",list_edit[(Circle (20,20) 5),(Rect (20,20) 4 7),(Square (20,20) 8) ])
--main=print("Changed List ")
{-

main = do
    forM_ listOfOjects $ \i -> do
        print i

    forM_ [7..9] $ \j -> do
        print j

    withBreak $ \break ->
        forM_ [1..] $ \_ -> do
            p "loop"
            break ()

    where
    withBreak = (`runContT` return) . callCC
    p = liftIO . putStrLn
    
    -}

-- *Main> let c1 = Circle (20,20) 5
-- *Main> draw c1
 -- Circle drawn @ (20.0,20.0) with radius 5.0
-- *Main> let c2 = c1 {pos = (10,10)}
-- *Main> draw c2
 -- Circle drawn @ (10.0,10.0) with radius 5.0
-- *Main> draw c1 -- c1 is immutable
 -- Circle drawn @ (20.0,20.0) with radius 5.0
-- *Main> area c1
-- 78.53982


{-
main :: IO ()
main = someFunc
-}

