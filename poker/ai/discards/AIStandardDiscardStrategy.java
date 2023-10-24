package ai.discards;

import java.util.ArrayList;
import java.util.Iterator;

import ai.AIDiscardStrategy;
import ai.util.AIHandValuator;
import ai.util.DiscardProbability;

import scoring.HandValue;
import util.Card;
import util.Hand;

/**
 * @author david kawrykow and gen kazama
 * 
 * This class implements the AIDiscardStrategy Interface. It is intended as 
 * the standard implementation of said interface. 
 * 
 * All possible discards are attempted. For each discard, all possible incoming
 * card-sets and their impact on the remaining cards are evaluated using the
 * AIHandValuator.
 *  
 * For example, suppose a hand contained A K Q J J, all suited except J. 
 * Every possible discard is simulated i.e. first all singles {A, K, Q, J, J}, 
 * then all doubles {AK, AQ, ... , QJ, JJ}, then all triples
 * {AKQ, AKJ, ... , KJJ, QJJ}, and the empty discard { }. For each discard
 * all possible incoming card-sets are put into the hand and the resulting 
 * hand value is computed. A table keeps track of how many times which hand value
 * was computed. At the end of each discard, the function scoreFromCategories
 * is called with the original hand Value as input argument. 
 * 
 * This method can then do what ever it wishes with the current discard information.
 * Whatever it does, it must return a float representing the "goodness" of the current
 * information. This method is the first of two which children can overwrite.  
 * 
 * After all discards have been evaluated, the best is chosen using the getBestDiscard
 * method. This method simply takes the highest discard of all and returns it. In the
 * Standard class (i.e. this class), it also determines whether the likelihood of getting
 * anything from the best discard is high or "bad". The SimpleDiscardStrategy overwrites
 * this and ignores bad probabilities and chooses second-rate discards when these are not
 * too far away from the primary discard. This then allows "simple" behavior. 
 */

public class AIStandardDiscardStrategy implements AIDiscardStrategy {

	protected final int NUM_DISCARDS_POSSIBLE=26;
	protected final int CORRECT_HAND_SIZE = 5;
	protected final int LAST = 4; 
	protected final int CARDS_IN_DECK=52;
	protected final int SIZE_OF_ONE_RANK=4;
	protected final int NUMBER_OF_HANDS=9;
	
	protected final float DENY_STDEV = (float)0.15; 
	protected final float DENY_AVG   = (float)0.65;
	protected final float BAD_PROBABILITY = (float)-1.00000000;
	protected final float GOOD_PROBABILITY = (float) 0.0; 
	
	protected DiscardProbability[] table;
	protected Card[] Deck = new Card[CARDS_IN_DECK];
	protected int[] Categories = new int[NUMBER_OF_HANDS]; 
	
	protected AIHandValuator ZZ; 
	
	//counter
	int c;
	
	public AIStandardDiscardStrategy()
	{
		// set up the return table
		table=new DiscardProbability[NUM_DISCARDS_POSSIBLE];
		table[NUM_DISCARDS_POSSIBLE-1] = new DiscardProbability();
		
		Card.Rank[] ranks = Card.Rank.values(); 
		Card.Suit[] suits = Card.Suit.values();
		
		// initialize all 52 cards for the deck
		for(int i = 0; i< suits.length; i++)
			for(int j = 0; j < ranks.length; j++)
				Deck[j*SIZE_OF_ONE_RANK + i] = new Card(ranks[j], suits[i]);
		
		resetCategoryCounts();
		
		// Create the hand valuator
		ZZ = new AIHandValuator();
	}
	
	public final float badProbability()
	{
		return BAD_PROBABILITY;
	}
	
	public final float goodProbability()
	{
		return GOOD_PROBABILITY; 
	}
	
	/**
	 * @pre hand.size() == 5
	 */
	public final DiscardProbability bestDiscard(Hand hand) 
	{
		c=0;
		
		// Assert correctness of the input hand size
		assert(hand.size() == CORRECT_HAND_SIZE);
		
		// We hold the hand in a random access construct
		Card theHand[] = new Card[CORRECT_HAND_SIZE];
		for(Iterator j = hand.iterator(); j.hasNext(); c++)
			theHand[c] = (Card) j.next();
		
		HandValue.Category std = ZZ.getHandValueCategory(theHand); 
		
		ArrayList<Card> bestCards = new ArrayList<Card>();
		bestCards.add(null);
		
		c=0;
		removeDuplicates(theHand);
		singleDiscardProbabilities(theHand, std);
		doubleDiscardProbabilities(theHand, std);
		tripleDiscardProbabilities(theHand, std);
		resetDeck(theHand);
		c=0;
		
		return getBestDiscard();
	}
	
	private final void resetCategoryCounts()
	{
		// initialize all 9  hand value counts for the categories
		for(int i = 0; i < NUMBER_OF_HANDS; i++)
			Categories[i] = 0; 
	}
	
	private final void removeDuplicates(Card[] hand)
	{
		int C = 0; 
		
		for(int i = 0; i < CARDS_IN_DECK; i++)
			if(Deck[i] == hand[C])
			{
				Deck[i] = null; 
				C++;			// pun
			}			
	}
	
	private final void resetDeck(Card[] hand)
	{
		int C = 0; 
		
		for(int i = 0; i < CARDS_IN_DECK; i++)
			if(Deck[i] == null)
			{
				Deck[i] = new Card(hand[C].getRank(), hand[C].getSuit()); 
				C++;
			}
	}
	
	protected DiscardProbability getBestDiscard()
	{
		int maxi = -1; 
		float avg = (float)0.0;
		float max = (float)0.0; 
		float cur;
		
		for(int i = 0; i < NUM_DISCARDS_POSSIBLE - 1; i++)
		{
			cur = table[i].getProbability();
			avg += cur;
			
			if( i > 14 )
			{
				if(cur > max)
				{
					max = cur;
					maxi=i;
				}
			}
			else
			{
				if(cur >= max)
				{
					max = cur;
					maxi=i;
				}
			}
			
		}
		
		avg /= (NUM_DISCARDS_POSSIBLE - 1);
		
		// if all were negative, then return the empty discard
		if(maxi == -1)
		{
			table[(NUM_DISCARDS_POSSIBLE - 1)].setProbability(goodProbability());
			return table[(NUM_DISCARDS_POSSIBLE - 1)];
		}
		
		if(getStandardDeviationFromTable(avg) < DENY_STDEV
				&& table[maxi].getProbability() - avg < DENY_AVG)	
		{
			table[maxi].setProbability(badProbability());
		}

		return table[maxi];
	}
	
	protected final float getStandardDeviationFromTable(float avg)
	{
		float cur;
		float tot = (float)0.0;
		
		for(int i = 0; i < NUM_DISCARDS_POSSIBLE - 1; i++)
		{
			cur = table[i].getProbability();
			cur -= avg; 
			tot += cur*cur;
		}
		
		return (float)java.lang.Math.sqrt(tot)/(NUM_DISCARDS_POSSIBLE-1);
	}
	
	private final void tripleDiscardProbabilities(Card[] cards, HandValue.Category std)
	{
		Card[] myHand = new Card[CORRECT_HAND_SIZE]; 
		 
		for(int i = 0; i < CORRECT_HAND_SIZE; i++)
			for(int k = i + 1; k < CORRECT_HAND_SIZE; k++)
				for(int m = k + 1; m < CORRECT_HAND_SIZE; m++, c++)
				{
					// Collect cards in myHand
					for(int j = 0; j < CORRECT_HAND_SIZE; j++)
						myHand[j] = cards[j];
			
					table[c]=new DiscardProbability();
					table[c].addCard(myHand[i]);
					table[c].addCard(myHand[k]);
					table[c].addCard(myHand[m]);
					
					// remove cards of interest
					myHand[i] = null;
					myHand[k] = null;
					myHand[m] = null;
					
					// Compute probable success for removing these three cards
					probableSuccessForHandTriple(myHand);
					
					// Then set the personalized value in the table
					table[c].setProbability(scoreFromCategories(std));
					resetCategoryCounts();
				}
	}
	
	private final void doubleDiscardProbabilities(Card[] cards, HandValue.Category std)
	{
		Card[] myHand = new Card[CORRECT_HAND_SIZE]; 
		 
		for(int i = 0; i < CORRECT_HAND_SIZE; i++)
			for(int k = i + 1; k < CORRECT_HAND_SIZE; k++, c++)
			{
				// Collect cards in myHand
				for(int j = 0; j < CORRECT_HAND_SIZE; j++)
					myHand[j] = cards[j];
				
				table[c]=new DiscardProbability();
				table[c].addCard(myHand[i]);
				table[c].addCard(myHand[k]);
				
				// remove cards of interest
				myHand[i] = null;
				myHand[k] = null;
				
				// Compute probable success for removing these two cards
				probableSuccessForHandDouble(myHand);
				
				// Then set the personalized value in the table
				table[c].setProbability(scoreFromCategories(std));
				resetCategoryCounts();
			}
	}
	
	/**
	 * This method computes the probability of success for discarding 
	 * a single card from hand, for each possible discard. This measure
	 * of success if defined in the helper method probableSuccessForHand 
	 */
	private final void singleDiscardProbabilities(Card[] cards, HandValue.Category std)
	{
		Card[] myHand = new Card[CORRECT_HAND_SIZE]; 
		 
		for(int i = 0; i < CORRECT_HAND_SIZE; i++, c++)
		{
			// Collect cards in myHand
			for(int j = 0; j < CORRECT_HAND_SIZE; j++)
				myHand[j] = cards[j];
			
			table[c]=new DiscardProbability();
			table[c].addCard(myHand[i]);
			
			// remove card of interest
			myHand[i] = null;
			
			// Compute probable success for removing these three cards
			probableSuccessForHandSingle(myHand);
			
			// Then set the personalized value in the table
			table[c].setProbability(scoreFromCategories(std));
			resetCategoryCounts();
		}
	}
	
	private final void probableSuccessForHandTriple(Card[] twoCardHand)
	{		
		int fEmptySlot = -1;
				
		// Find the empty slot we always fill
		for(int i = 0; i < CORRECT_HAND_SIZE; i++)
			if(twoCardHand[i] == null)
			{
				fEmptySlot = i;
				break;
			}		
		assert(fEmptySlot != -1);
		
		// Convert 2-Card Hand into 3-Card Hand with all possible cards in deck 
		for(int i = 0; i< CARDS_IN_DECK; i++)
		{	
			// three Card Hand now becomes four Card Hand
			if(Deck[i] != null)
			{
				twoCardHand[fEmptySlot] = Deck[i];
			
				// evaluate the hand using all possible double discard options
				probableSuccessForHandDouble(twoCardHand);
			}
		}
		
		// restore the hand to what it was before
		twoCardHand[fEmptySlot] = null;
	}
	
	private final void probableSuccessForHandDouble(Card[] threeCardHand)
	{
		int fEmptySlot = -1; 
				
		// Find the empty slot we always fill
		for(int i = 0; i < CORRECT_HAND_SIZE; i++)
			if(threeCardHand[i] == null)
			{
				fEmptySlot = i;
				break;
			}		
		assert(fEmptySlot != -1);
		
		// Convert 3-Card Hand into 4-Card Hand with all possible cards in deck 
		for(int i = 0; i< CARDS_IN_DECK; i++)
		{
			if(Deck[i] != null)
			{
				// three Card Hand now becomes four Card Hand
				threeCardHand[fEmptySlot] = Deck[i];
				
				// evaluate the hand using all possible double discard options
				probableSuccessForHandSingle(threeCardHand);				
			}
		}
		
		// restore the hand to what it was before
		threeCardHand[fEmptySlot] = null;		
	}
	
	/**
	 * Finally calls the hand valuator on the five card hand
	 * @param fourCardHand we range over all inputs to this four card hand
	 */
	private final void probableSuccessForHandSingle(Card[] fourCardHand)
	{		
		// Find the empty slot we always fill
		for(int i = 0; i < CORRECT_HAND_SIZE; i++)
			if(fourCardHand[i] == null)
			{
				fourCardHand[i]    = fourCardHand[LAST];
				fourCardHand[LAST] = null;
				break;
			}		
		
		// Set the hand valuator cards to this!
		ZZ.setConstantCards(fourCardHand);
		
		// Evaluate and store
		for(int i = 0; i< CARDS_IN_DECK; i++)
			if(Deck[i] != null)
				Categories[ZZ.getHandValueCategory(Deck[i]).ordinal()]++;
	}
	
	protected int updateScore(int score)
	{
		if(score < 0)
			return score*score*-1; 
		
		return score*score; 
	}
	
	protected float scoreFromCategories(HandValue.Category std)
	{
		int t = 0, diff, ord = std.ordinal();  
		float r = (float)0.0;
		
		for(int i = 0; i < ord; i++)
		{
			t += Categories[i];
			diff = ord - i; 
			r -= diff * diff * Categories[i];
		}
		
		t += Categories[ord];
		
		for(int i = ord + 1; i < Categories.length; i++)
		{
			t += Categories[i];
			diff = i - ord; 
			r += diff * diff * Categories[i];
		}
		
		return r/t; 
	}
	
	public String toString()
	{
		String result="";
		for(int i=0; i<table.length; i++)
			result+=table[i]+"\n";
		return result;
	}
	
}
