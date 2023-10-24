


public static boolean hasStraight(Card [] cards) {
boolean isTrue = true; 
for(int atPos =0; atPos



public Card(Value value, Suit suit) {
    this.value = value;
    this.suit = suit;
}




private ArrayList deck = new ArrayList();
private ArrayList hand = new ArrayList();
private final int HANDSIZE = 5;
public Deck() {
    for(Suit suit : Suit.values()) {
        for(Value value : Value.values()){
            Card card = new Card(value, suit);
            deck.add(card);
        }
    }
}
public ArrayList draw() {
    Random rng = new Random();
    for(int i = 0; i < HANDSIZE; i++){
        int getCard = rng.nextInt(deck.size());
        Card addCard = deck.get(getCard);
        hand.add(addCard);
        deck.remove(getCard);
    }
    return hand;
}




public class Player extends Deck {
    private String name;
    private int chips;
    public Player(String name, int chips) {
        this.name = name;
        this.chips = chips;
    }
	



public class Player {
    private String name;
    private int chips;
    private Deck deck;
    public Player(String name, int chips, Deck deck) {
        this.name = name;
        this.chips = chips;
        this.deck = deck;
    }
}





private ArrayList deck = new ArrayList();
private final int HANDSIZE = 5;
private static Deck deck = new Deck();
public static Deck getInstance() {
  return deck;
}
// Private Constructor
private Deck() {
    for(Suit suit : Suit.values()) {
        for(Value value : Value.values()){
            Card card = new Card(value, suit);
            deck.add(card);
        }
    }
}
public ArrayList draw() {
    Random rng = new Random();
    ArrayList hand = new ArrayList();
    for(int i = 0; i < HANDSIZE; i++){
    int getCard = rng.nextInt(deck.size());
    Card addCard = deck.get(getCard);
    hand.add(addCard);
    deck.remove(getCard);
    }
    return hand;
}






import java.awt.*;
import java.awt.event.*;
import java.util.*;
public class PokerHand extends Frame implements ActionListener {
    Button b = new Button("Click for new hand");
    boolean dealt[] = new boolean[52];
    String[] playercard = new String[10];
    String[] playersuit = new String[10];
    Random r = new Random();
    int drawn;
    public static void main(String args[]) {
        PokerHand ph = new PokerHand();
        ph.doIt();
    }
    public void doIt() {
        int ct;
        for (ct = 0; ct <= 9; ++ct) { // first loop that you learned tonight
            playercard[ct] = " ";
            playersuit[ct] = " ";
        } // ends for loop
        b.addActionListener(this);
        this.setLayout(new FlowLayout());
        this.add(b);
        this.setSize(500, 500);
        this.setVisible(true);
    }// ends doIt method
    public void paint(Graphics g) {
        g.setFont(new Font(null, 1, 30));
        g.drawString("Me", 400, 100);
        g.drawString(playercard[0], 30, 100);
        g.drawString(playersuit[0], 60, 100);
        g.drawString(playercard[1], 90, 100);
        g.drawString(playersuit[1], 120, 100);
        g.drawString(playercard[2], 150, 100);
        g.drawString(playersuit[2], 180, 100);
        g.drawString(playercard[3], 210, 100);
        g.drawString(playersuit[3], 240, 100);
        g.drawString(playercard[4], 270, 100);
        g.drawString(playersuit[4], 300, 100);
        g.drawString("You", 400, 200);
        g.drawString(playercard[5], 30, 200);
        g.drawString(playersuit[5], 60, 200);
        g.drawString(playercard[6], 90, 200);
        g.drawString(playersuit[6], 120, 200);
        g.drawString(playercard[7], 150, 200);
        g.drawString(playersuit[7], 180, 200);
        g.drawString(playercard[8], 210, 200);
        g.drawString(playersuit[8], 240, 200);
        g.drawString(playercard[9], 270, 200);
        g.drawString(playersuit[9], 300, 200);
    }
    public void actionPerformed(ActionEvent ae) {
        int ct;
        ct = 0;
        int card;
        int suit;
        // we draw 10 cards here
        while (ct < 10) { // a while loop in practice
            drawn = r.nextInt(52);
            if (dealt[drawn] != true) { // if in practice
                card = drawn % 13 + 1;
                suit = drawn / 13;
                dealt[drawn] = true;
                playercard[ct] = String.valueOf(card);
                if (card == 1) {
                    playercard[ct] = "A";
                }
                if (card == 11) {
                    playercard[ct] = "J";
                }
                if (card == 12) {
                    playercard[ct] = "Q";
                }
                if (card == 13) {
                    playercard[ct] = "K";
                }
                if (suit == 0) {
                    playersuit[ct] = "\u2660";
                }
                if (suit == 1) {
                    playersuit[ct] = "\u2661"; //change to red heart
                }
                if (suit == 2) {
                    playersuit[ct] = "\u2662"; //change to red diamond
                }
                if (suit == 3) {
                    playersuit[ct] = "\u2663";
                }
                ct = ct + 1;
            } // ends if
        }// ends while
        repaint();
        for (int x = 0; x <= 51; ++x)
            dealt[x] = false;
    }// ends method
}// ends the class





public enum Suit {
    SPADES("\u2660\uFE0F"), HEARTS("\u2665\uFE0F"), DIAMONDS("\u2666\uFE0F"), CLUBS("\u2663\uFE0F");
    private final String icon;
    Suit(String icon) {
        this.icon = icon;
    }
    public String getIcon() {
        return icon;
    }
}






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

data Suit = SPADES | HEARTS | DIAMONDS | CLUBS deriving (Show)

instance Eq Suit where
    SPADES == SPADES = True
    HEARTS == HEARTS = True
    DIAMONDS == DIAMONDS = True
    CLUBS == CLUBS = True
    _ == _ = False

instance Enum Suit where
    toEnum 0 = SPADES
    toEnum 1 = HEARTS
    toEnum 2 = DIAMONDS
    toEnum 3 = CLUBS
    toEnum _ = error "Invalid Suit"

    fromEnum SPADES = 0
    fromEnum HEARTS = 1
    fromEnum DIAMONDS = 2
    fromEnum CLUBS = 3

instance Bounded Suit where
    minBound = SPADES
    maxBound = CLUBS

getIcon :: Suit -> String
getIcon SPADES = "\u2660\uFE0F"
getIcon HEARTS = "\u2665\uFE0F"
getIcon DIAMONDS = "\u2666\uFE0F"
getIcon CLUBS = "\u2663\uFE0F"











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