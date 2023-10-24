package ai.util;

import scoring.*;
import util.*;

import java.util.*;

/**
 * @author david kawrykow and gen kazama
 *
 * Is used by AI to evaluate the current content of a hand before discard. 
 * It checks for nothing, pair, two pairs, three of a kind, and four of a kind. 
 *
 */

public class AIPreBetHandValuator {
	private final int NUM_RANKS_IN_DECK = 13;
	
	public AIPreBetHandValuator()
	{
		
	}
	
	/**
	 * This checks if the un-made hand (as calculated by DiscardProbability is 
	 * something (i.e. pair, two-pair, three-of-a-kind, four-of-a-kind) or 
	 * nothing 
	 * @param probability the discard information
	 * @param hand the hand that you currently have 
	 * @return
	 */
	public HandValue.Category evaluateHand(DiscardProbability probability, Hand hand)
	{
		int[] ranks = new int[NUM_RANKS_IN_DECK];
		ArrayList<Card> cards = new ArrayList<Card>(hand.size());
		ArrayList<Card> discardCards = probability.getArrayListCards();
		int numPairs=0;
		
		//Add the cards in the hand into the card array
		int c=0;
		for(Iterator j = hand.iterator(); j.hasNext(); c++)
		{
			cards.add((Card) j.next());
		}
		
		//remove the cards discarded
		for(int i=0; i<discardCards.size(); i++)
			for(int j=0; j<cards.size(); j++)
				if(cards.get(j).getRank()==discardCards.get(i).getRank() &&
				   cards.get(j).getSuit()==discardCards.get(i).getSuit())
					cards.remove(j);
		
		//Set the ranks array to all 0s
		for(int i=0; i<ranks.length; i++)
			ranks[i]=0;
		
		for(int i=0; i<cards.size(); i++)
			ranks[cards.get(i).getRank().ordinal()]++;
		
		for(int i=0; i<ranks.length; i++)
			if(ranks[i]==4)
				return HandValue.Category.FOUR_OF_A_KIND;
			else if(ranks[i]==3)
				return HandValue.Category.THREE_OF_A_KIND;
			else if(ranks[i]==2)
				numPairs++;
		
		if(numPairs==1)
			return HandValue.Category.PAIR;
		else if(numPairs==2)
			return HandValue.Category.TWO_PAIRS;
		return HandValue.Category.HIGH_CARD;

	}
	
}
