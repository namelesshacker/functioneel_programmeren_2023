package ai.util;

import util.*;
import scoring.*;

/**
 * Is used by AI to quickly evaluate hands.
 * There are two versions. One takes a hand. The other takes
 * a four card hand and then you swap in the fifth card (faster) 
 * @author david kawrykow and gen kazama
 *
 */
public class AIHandValuator {

	// some constants
	private final int CORRECT_HAND_SIZE = 5; 
	private final int CORRECT_NUM_RANKS = 13;
	private final int CORRECT_NUM_SUITS = 4; 
	private final int CORRECT_FOUR_OF_A_KIND = 4; 
	private final int CORRECT_THREE_OF_A_KIND = 3;
	private final int CORRECT_PAIR = 2; 
	
	// hashtables keeping ranks and suits
	private int Ranks[];
	private int Suits[];
	
	// hack index remembering flush information
	private boolean maybeFlush;
	private int     maybeFlushIndex;
	
	// constructor
	public AIHandValuator()
	{
		Ranks = new int[CORRECT_NUM_RANKS];
		Suits = new int[CORRECT_NUM_SUITS];
	}
	
	public void setConstantCards(Card[] pHand)
	{
		assert ( pHand != null );
		
		// Clear the hashTables
		for( int i = 0; i < CORRECT_NUM_RANKS; i++)
			Ranks[i] = 0;
		for( int i = 0; i < CORRECT_NUM_SUITS; i++)
			Suits[i] = 0; 
		
		// Clear the boolean
		maybeFlush = false; 
		
		// Collect the ranks of the four card hand
		Ranks[pHand[0].getRank().ordinal()]++;
		Ranks[pHand[1].getRank().ordinal()]++;
		Ranks[pHand[2].getRank().ordinal()]++;
		Ranks[pHand[3].getRank().ordinal()]++;
		
		// Collect the suits of the four card hand
		Suits[pHand[0].getSuit().ordinal()]++;
		Suits[pHand[1].getSuit().ordinal()]++;
		Suits[pHand[2].getSuit().ordinal()]++;
		Suits[pHand[3].getSuit().ordinal()]++;
		
		// remember the suit information
		for( int i = 0; i < CORRECT_NUM_SUITS; i++)
		{
			if(Suits[i] == 4)
			{
				maybeFlush = true;
				maybeFlushIndex = i;
				break;
			}
			else if(Suits[i] != 0)
			{
				maybeFlush = false;
				break;
			}
		}
	}
	
	/**
	 * An optimized version of the original (now deprecated) method. Note
	 * we could optimize more by retaining more than just the hashtables
	 * but that would be slightly more difficult. 
	 * @param pCard
	 * @return
	 */
	public HandValue.Category getHandValueCategory(Card pCard)
	{
		assert(pCard != null);
		
		// Add card information to the hashtable. 
		Ranks[pCard.getRank().ordinal()]++;
		
		// Evaluate the hash tables
		HandValue.Category x = evaluateHashTables();
		
		// Remove card information from the hashtable.
		Ranks[pCard.getRank().ordinal()]--;
		
		// combine info with flush info
		// we didnt see a flush chance before - just return what we had
		if( !maybeFlush )
		{
			return x;
		}
		// we saw a flush chance but x became a pair - just return the pair
		else if( x == HandValue.Category.PAIR ) 
		{
			return x;
		}
		// otherwise x is high card or straight. if its index is not the
		// flush index, just return x
		else if(pCard.getSuit().ordinal() != maybeFlushIndex)
		{
			return x; 
		}
		// if x was a high card, it is now a flush
		else if(x == HandValue.Category.HIGH_CARD)
		{
			return HandValue.Category.FLUSH;
		}
		// otherwise it must be a straight flush
		else
		{
			return HandValue.Category.STRAIGHT_FLUSH;
		}
		
		
	}
	
	private HandValue.Category evaluateHashTables()
	{
		
		// Used for Straight Checking
		int lowestRank = -1; 
		
		// Simple boolean measures
		boolean isStraight = true;
		boolean isThreeOfAKind = false;
		boolean isPair = false;
		
		//Check simple amalgamation setups
		for(int i = 0; i < CORRECT_NUM_RANKS; i++)
		{
			int rankVal = Ranks[i];			// save the value hopefully in a register
			
			// don't bother computing anything unless there's an actual card here
			// Note that at least 8/13 times this saves the entire computation below
			if(rankVal != 0)
			{
				// termination criteria
				if(rankVal == CORRECT_FOUR_OF_A_KIND)
					return HandValue.Category.FOUR_OF_A_KIND;
				if(rankVal == CORRECT_THREE_OF_A_KIND && isPair)
					return HandValue.Category.FULL_HOUSE;
				if(rankVal == CORRECT_PAIR && isThreeOfAKind)
					return HandValue.Category.FULL_HOUSE;
				if(rankVal == CORRECT_PAIR && isPair)
					return HandValue.Category.TWO_PAIRS;
				
				// discoveries: amalgamate pairs, 3-of-a-kinds, and partial straight
				// note that we can skip straight checking once these
				// discoveries have been made
				if(rankVal != 1)
				{
					if(rankVal == CORRECT_THREE_OF_A_KIND)
					{
						isThreeOfAKind = true;
						isStraight = false;			// we can cancel straight checking
					}
					else if(rankVal == CORRECT_PAIR)
					{
						isPair = true;
						isStraight = false;			// we can cancel straight checking
					}
				}
				
				// normal straight checking - isStraight true means still in running
				// for a normal straight (we check low straight at very end)
				if(isStraight)
				{
					// start straight checking from this card on
					if(lowestRank == -1)
					{
						if(i >= 9)				// ignore face card only hands
							isStraight = false;
						else 
							lowestRank = i;
					}
					// if previous last card was not one away, stop checking straights
					else if(lowestRank + 1 != i)
						isStraight = false;
					// otherwise start from this card next time
					else
						lowestRank++;
				}
			}
		}
		
		// report on pairs and royal pairs
		if(isPair)
			return HandValue.Category.PAIR;
		if(isThreeOfAKind)
			return HandValue.Category.THREE_OF_A_KIND;
		
		// Now check for a low straight
		if(Ranks[12] == 1)
			if(Ranks[1] == 1)
				if(Ranks[2] == 1)
					if(Ranks[3] == 1)
						if(Ranks[0] == 1)
							isStraight = true;
				
		// decipher whether Straight or Nothing
		if(!isStraight)
			return HandValue.Category.HIGH_CARD;
		else
			return HandValue.Category.STRAIGHT;
	}
	
	/**
	 * @pre pHand != null && pHand.length == 5
	 * @param pHand
	 * @return
	 */
	public HandValue.Category getHandValueCategory(Card[] pHand)
	{
		assert(pHand != null);
		assert(pHand.length == CORRECT_HAND_SIZE);
		
		// Used for Straight Checking
		int lowestRank = -1; 
		
		// Simple boolean measures
		boolean isFlush = false; 
		boolean isStraight = true;
		boolean isThreeOfAKind = false;
		boolean isPair = false;
						
		// Hash by rank first - flushes are too rare to check for
		// until we know nothing better exists
		int rankVal;
		int Ranks[] = new int[CORRECT_NUM_RANKS];
		
		Ranks[pHand[0].getRank().ordinal()]++;
		Ranks[pHand[1].getRank().ordinal()]++;
		Ranks[pHand[2].getRank().ordinal()]++;
		Ranks[pHand[3].getRank().ordinal()]++;
		Ranks[pHand[4].getRank().ordinal()]++;
		
		// Check simple amalgamation setups
		for(int i = 0; i < CORRECT_NUM_RANKS; i++)
		{
			rankVal = Ranks[i];			// save the value hopefully in a register
			
			// don't bother computing anything unless there's an actual card here
			// Note that at least 8/13 times this saves the entire computation below
			if(rankVal != 0)
			{
				// termination criteria
				if(rankVal == CORRECT_FOUR_OF_A_KIND)
					return HandValue.Category.FOUR_OF_A_KIND;
				if(rankVal == CORRECT_THREE_OF_A_KIND && isPair)
					return HandValue.Category.FULL_HOUSE;
				if(rankVal == CORRECT_PAIR && isThreeOfAKind)
					return HandValue.Category.FULL_HOUSE;
				if(rankVal == CORRECT_PAIR && isPair)
					return HandValue.Category.TWO_PAIRS;
				
				// discoveries: amalgamate pairs, 3-of-a-kinds, and partial straight
				// note that we can skip straight checking once these
				// discoveries have been made
				if(rankVal != 1)
				{
					if(rankVal == CORRECT_THREE_OF_A_KIND)
					{
						isThreeOfAKind = true;
						isStraight = false;			// we can cancel straight checking
					}
					else if(rankVal == CORRECT_PAIR)
					{
						isPair = true;
						isStraight = false;			// we can cancel straight checking
					}
				}
				
				// normal straight checking - isStraight true means still in running
				// for a normal straight (we check low straight at very end)
				if(isStraight)
				{
					// start straight checking from this card on
					if(lowestRank == -1)
					{
						if(i >= 9)				// ignore face card only hands
							isStraight = false;
						else 
							lowestRank = i;
					}
					// if previous last card was not one away, stop checking straights
					else if(lowestRank + 1 != i)
						isStraight = false;
					// otherwise start from this card next time
					else
						lowestRank++;
				}
			}
		}
		
		// report on pairs and royal pairs
		if(isPair)
			return HandValue.Category.PAIR;
		if(isThreeOfAKind)
			return HandValue.Category.THREE_OF_A_KIND;
		
		// Now check flush 
		// The odds of flush are very low, so we approach 
		// this from a pessimistic perspective
		int fSuit = pHand[0].getSuit().ordinal();
		if(pHand[1].getSuit().ordinal() == fSuit)
			if(pHand[2].getSuit().ordinal() == fSuit)
				if(pHand[3].getSuit().ordinal() == fSuit)
					if(pHand[4].getSuit().ordinal() == fSuit)
							isFlush = true;
		
		// Now check for a low straight in the same way - 
		// because the odds are truly low
		if(Ranks[12] == 1)
			if(Ranks[1] == 1)
				if(Ranks[2] == 1)
					if(Ranks[3] == 1)
						if(Ranks[0] == 1)
							isStraight = true;
				
		// decipher whether Straight, Straight Flush, Flush, or Nothing
		// Note that Nothing tends to occur more often, so we check this first
		// we check straight, flush, straight flush afterward - i.e. in order
		// of likelihood of appearing
		if(!isStraight && !isFlush)
			return HandValue.Category.HIGH_CARD;
		else if(isStraight && !isFlush)
			return HandValue.Category.STRAIGHT;
		else if(!isStraight && isFlush )
			return HandValue.Category.FLUSH;
		else
			return HandValue.Category.STRAIGHT_FLUSH;
	}	
}
