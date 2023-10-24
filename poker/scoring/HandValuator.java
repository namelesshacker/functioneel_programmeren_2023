/**
 * @author Gen Kazama & David Kawrykow
 * @date September 15, 2007
 */

package scoring;

import util.Hand;
import util.Card;

/**
 * Default implementation of IHandValuator.
 */
public class HandValuator implements IHandValuator 
{	
	public HandValue valuateHand( Hand pHand ) 
	{
		Card[] myHand = new Card[pHand.size()];
		
		/* Exception Set Up */
		if(pHand == null)
			junit.framework.Assert.fail("Input Hand was a null pointer");
		if(!pHand.isFull())
			junit.framework.Assert.fail("Input Hand is not full.");
		
		for(int i = myHand.length - 1; i >= 0; i--)
		{
			myHand[i] = pHand.getHighCard();
			pHand.remove(myHand[i]);
		}
		
		for(int i=0; i < myHand.length; i++)
			pHand.add(myHand[i]);
		
		if(isStraightFlush(myHand))
			return new HandValue(HandValue.Category.STRAIGHT_FLUSH, pHand);
		
		if(isFourOfAKind(myHand))
			return new HandValue(HandValue.Category.FOUR_OF_A_KIND, pHand);
		
		if(isFullHouse(myHand))
			return new HandValue(HandValue.Category.FULL_HOUSE, pHand);
		
		if(isFlush(myHand))
			return new HandValue(HandValue.Category.FLUSH, pHand);
		
		if(isStraight(myHand))
			return new HandValue(HandValue.Category.STRAIGHT, pHand);
		
		if(isThreeOfAKind(myHand))
			return new HandValue(HandValue.Category.THREE_OF_A_KIND, pHand);
		
		if(isTwoPairs(myHand))
			return new HandValue(HandValue.Category.TWO_PAIRS, pHand);

		if(isPair(myHand))
			return new HandValue(HandValue.Category.PAIR, pHand);
			
		return new HandValue(HandValue.Category.HIGH_CARD, pHand);
	}
	
	private boolean isStraightFlush(Card[] myHand)
	{	
		for(int i = 0; i < myHand.length - 1; i++)	
		{
			if(myHand[i].getSuit() != myHand[i+1].getSuit())
				return false;
			
			if(	myHand[i].compareTo(myHand[i+1]) != -1 
				&& 
				( myHand[i+1].getRank() != Card.Rank.Ace
				||   myHand[i].getRank() != Card.Rank.Five)
			)
				return false;
		}
		
		return true;
	}
	
	private boolean isFourOfAKind(Card[] myHand)
	{
		int c = 0; 
		
		if(myHand[0].compareTo(myHand[1]) == 0)
			c++;
		
		for(int i = 1; i < myHand.length - 1; i++)
			if(myHand[i].compareTo(myHand[i+1]) == 0)
				c++;
			else
				break;
		
		return (c >= 3);
	}
	
	private boolean isFullHouse(Card[] myCards)
	{
		if(myCards[0].compareTo(myCards[1]) != 0)
			return false;
		
		if(myCards[3].compareTo(myCards[4]) != 0)
			return false;
		
		if(		myCards[2].compareTo(myCards[1]) != 0
			&&  myCards[2].compareTo(myCards[3]) != 0 
		)
			return false;
	
		return true;
	}
	
	private boolean isFlush(Card[] myHand)
	{	
		for(int i = 0; i < myHand.length - 1; i++)
		{
			if(myHand[i].getSuit() != myHand[i+1].getSuit())
				return false;
		}
		
		return true;
	}
	
	private boolean isStraight(Card[] myHand)
	{
		for(int i = 0; i < myHand.length - 1; i++)	
			if(	myHand[i].compareTo(myHand[i+1]) != -1 
				&& ( myHand[i+1].getRank() != Card.Rank.Ace
				|| myHand[i].getRank() != Card.Rank.Five)
			)
				return false;
		return true;
	}
	
	private boolean isThreeOfAKind(Card[] myCards)
	{	
		if(myCards[2].compareTo(myCards[1]) != 0)
		{
			if(myCards[2].compareTo(myCards[3]) != 0)
				return false;
			
			if(myCards[2].compareTo(myCards[4]) != 0)
				return false;
		}
		else
		{
			if(		myCards[2].compareTo(myCards[3]) != 0
				&&	myCards[2].compareTo(myCards[0]) != 0)
				return false;		
		}

		return true;
	}
	
	private boolean isTwoPairs(Card[] myCards)
	{	
		if(myCards[0].compareTo(myCards[1]) != 0)
		{
			if(		myCards[1].compareTo(myCards[2]) != 0 
				||	myCards[3].compareTo(myCards[4]) != 0)
				return false;
		}
		else
		{
			if(		myCards[2].compareTo(myCards[3])!= 0
				&&	myCards[3].compareTo(myCards[4])!= 0)
				return false;
		}
		
		return true;
	}
	
	private boolean isPair(Card[] myCards)
	{	
		for(int i = 0; i < myCards.length - 1; i++)
			if(myCards[i].compareTo(myCards[i+1]) == 0)	
				return true;
		
		return false;
	}
}