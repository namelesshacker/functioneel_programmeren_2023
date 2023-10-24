package game;
 
import java.util.Arrays;
import java.util.Scanner;
 
public class DrawPoker {
    /*
     * Jian C Mitchell
     * 
     * Jian.mitchell@student.mendocino.edu
     * 
     * 11/14/20
     * 
     */
 
      private final int HAND_SIZE = 5;
        private int again = 1;
         
        // instantiate Deck and Player
        Scanner scan = new Scanner(System.in);
        Deck deck = new Deck();
        Player player = new Player();
        Card[] hand;
         
        // plays the game
        public void play()
        {
            while (again == 1)
            {
                // fill deck
                deck.fillDeck();
                 
                // shuffle
                deck.shuffle();
                 
                // player draws
                hand = player.draw(deck);
                 
                // sort hand        
                Arrays.sort(hand);
                 
                // player redraws
                this.checkHand();
                hand = this.redraw();
                 
                // display hand again
                // this.makeHand(); 
                this.checkHand();
                 
                // sort hand        
                Arrays.sort(hand);
                 
                // evaluate the hand
                this.evaluate();
             
                // play again?
                this.again();
            }
            System.out.println("Thanks for playing! =]");
        }
         
        // makes a hand
        public void makeHand()
        {
            hand[0].rank = 1;
            hand[1].rank = 2;
            hand[2].rank = 3;
            hand[3].rank = 4;
            hand[4].rank = 5;
             
            hand[0].suit = 1;
            hand[1].suit = 1;
            hand[2].suit = 1;
            hand[3].suit = 1;
            hand[4].suit = 1;
        }
         
        // tells player cards in hand
        public void checkHand()
        {
            for (int handCounter = 0; handCounter < HAND_SIZE; handCounter++)
            {
                this.display(hand[handCounter]);
            }
        }
         
        // asks if player wants to redraw
        public Card[] redraw()
        {
            for (int counter = 0; counter < 5; counter++)
            {
                System.out.print("Redraw card " + (counter + 1) + "?" +
                        " (1 for yes, 0 for no)");
                int answer = scan.nextInt();
                if (answer == 1)
                {
                    hand[counter] = player.redraw(counter, deck);
                }
            }
            deck.refreshDeckPosition();
            return hand;
        }
         
         
        // evaluates the hand
        public void evaluate()
        {
            if (this.royalFlush() == 1)
            {
                System.out.println("You have a royal flush! +200");
            }
            else if (this.straightFlush() == 1)
            {
                System.out.println("You have a straight flush! +100");
            }
            else if (this.fourOfaKind() == 1)
            {
                System.out.println("You have four of a kind! +75");
            }
            else if (this.fullHouse() == 1)
            {
                System.out.println("You have a full house! +50");
            }
            else if (this.flush() == 1)
            {
                System.out.println("You have a flush! +35");
            }
            else if (this.straight() == 1)
            {
                System.out.println("You have a straight! +25");
            }
            else if (this.triple() == 1)
            {
                System.out.println("You have a triple! +20");
            }
            else if (this.twoPairs() == 1)
            {
                System.out.println("You have two pairs! +10");
            }
            else if (this.pair() == 1)
            {
                System.out.println("You have a pair! +2");
            }
            else if (this.noPair() == 1) 
            {
                System.out.println("You have no matching cards! -5");
            }
        }
         
        // checks for a royal flush
        public int royalFlush()
        {
            if (hand[0].rank == 1 && hand[1].rank == 10 && hand[2].rank == 11 &&
                    hand[3].rank == 12 && hand[4].rank == 13)
            {
                System.out.println("Game Score: 200");
                return 1;
            }
            else
            {
                return 0;
            }
        }
         
        // checks for a straight flush
        public int straightFlush()
        {
            for (int counter = 1; counter < 5; counter++)
            {
                if (hand[0].suit != hand[counter].suit)
                {
                    return 0;
                }
            }
            for (int counter2 = 1; counter2 < 5; counter2++)
            {
                if (hand[counter2 - 1].rank != (hand[counter2].rank - 1))
                {
                    return 0;
                }
                     
            }
            System.out.println("Game Score: 100");
            return 1;
             
        }
         
        // checks for four of a kind
        public int fourOfaKind()
        {
            if (hand[0].rank != hand[3].rank && hand[1].rank != hand[4].rank)
            {
                return 0;
            }
            else
            {
                System.out.println("Game Score: 75");
                return 1;
            }
        }
         
        // checks for full house
        public int fullHouse()
        {
            int comparison = 0;
            for (int counter = 1; counter < 5; counter++)
            {
                if (hand[counter - 1].rank == hand[counter].rank)
                {
                    comparison++;
                }
            }
            if (comparison == 3)
            {
                System.out.println("Game Score: 50");
                return 1;
            }
            else
            {
                return 0;
            }
        }
         
        // checks for flush
        public int flush()
        {
            for (int counter = 1; counter < 5; counter++)
            {
                if (hand[0].suit != hand[counter].suit)
                {
                    return 0;
                }
            }
            System.out.println("Game Score: 35");
            return 1;
        }
         
        // check for straight
        public int straight()
        {
            for (int counter2 = 1; counter2 < 5; counter2++)
            {
                if (hand[counter2 - 1].rank != (hand[counter2].rank - 1))
                {
                    return 0;
                }
                     
            }
            System.out.println("Game Score: 25");
            return 1;
        }
         
        // checks for triple
        public int triple()
        {
            if (hand[0].rank == hand[2].rank || hand[2].rank == hand[4].rank)
            {
                System.out.println("Game Score: 20");
                return 1;
            }
            return 0;
        }
         
        // checks for two pairs
        public int twoPairs()
        {
            int check = 0;
            for(int counter = 1; counter < 5; counter++)
            {
                if (hand[counter - 1].rank == hand[counter].rank)
                {
                    check++;
                }
            }
            if (check == 2)
            {
                System.out.println("Game Score: 10");
                return 1;
            }
            else
            {
                return 0;
            }
        }
         
        // check for pair
        public int pair()
        {
            int check = 0;
            for(int counter = 1; counter < 5; counter++)
            {
                if (hand[counter - 1].rank == hand[counter].rank)
                {
                    check++;
                }
            }
            if (check == 1)
            {
                System.out.println("Game Score: 2");
                return 1;
            }
            else
            {
                return 0;
            }
        }
         
        // check for no pairs
        public int noPair() 
        {
            int check = 0;
            for(int counter = 1; counter < 5; counter++)
            {   
                if (hand[counter - 1].rank == hand[counter].rank)
                {
                    check++;    
                }
            }
            if (check == 0)
            {
                System.out.println("Game Score: 0");
                return 1;
            }
            else
            {
                return 0;
            }
 
        }
         
        // asks user if they want to play again
        public void again()
        {       
            System.out.print("Play again? (1 for yes, 0 for no)");
            again = scan.nextInt();
        }
     
        // generates string for each card in hand
        public void display(Card card)
        {
            if (card.rank == 1)
            {
                System.out.print("Ace of ");
            }
            if (card.rank == 2)
            {
                System.out.print("Two of ");
            }
            if (card.rank == 3)
            {
                System.out.print("Three of ");
            }
            if (card.rank == 4)
            {
                System.out.print("Four of ");
            }
            if (card.rank == 5)
            {
                System.out.print("Five of ");
            }
            if (card.rank == 6)
            {
                System.out.print("Six of ");
            }
            if (card.rank == 7)
            {
                System.out.print("Seven of ");
            }
            if (card.rank == 8)
            {
                System.out.print("Eight of ");
            }
            if (card.rank == 9)
            {
                System.out.print("Nine of ");
            }
            if (card.rank == 10)
            {
                System.out.print("Ten of ");
            }
            if (card.rank == 11)
            {
                System.out.print("Jack of ");
            }
            if (card.rank == 12)
            {
                System.out.print("Queen of ");
            }
            if (card.rank == 13)
            {
                System.out.print("King of ");
            }
            if (card.suit == 1)
            {
                System.out.print("Spades");
                System.out.println();
            }
            if (card.suit == 2)
            {
                System.out.print("Hearts");
                System.out.println();
            }
            if (card.suit == 3)
            {
                System.out.print("Diamonds");
                System.out.println();
            }
            if (card.suit == 4)
            {
                System.out.print("Clubs");
                System.out.println();
            }
             
        }
    }
	
	
	
module Game where


import Data.List (sort)
import System.IO

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Show, Eq, Ord)
data Card = Card Rank Suit deriving (Show, Eq)
data Deck = Deck [Card] deriving (Show)
data Player = Player deriving (Show)

handSize :: Int
handSize = 5

again :: Int
again = 1

-- instantiate Deck and Player
deck :: Deck
deck = Deck []

player :: Player
player = Player

hand :: [Card]
hand = []




-- plays the game
main :: IO ()
main = do
    putStrLn "Enter 1 to play again or 0 to quit: "
    input <- getLine
    let again =1
    if again == 1
        then main
        else return ()
        else return ()
		






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