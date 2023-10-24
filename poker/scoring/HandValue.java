package scoring;

import util.Card;
import util.Hand;

/**
 * Represents the value of a poker hand.  The strength of a hand is 
 * represented by its category and a list of tie breakers. If two 
 * hands are in the same category, the list of tie breakers is inspected in
 * order to determine the strongest hand. The tie breakers should be as follows:
 * <UL>
 * <LI>Straight-flush: The top card (only). If the ACE is used low, the 5 is the top card
 * <LI>4-of-a-kind: Any one card from the series of 4
 * <LI>Full house: Any one card from the series of 3, and any one card from the series of 2
 * <LI>Flush: The five cards of the hand in descending order
 * <LI>Straight: The top card (only). If the ACE is used low, the 5 is the top card
 * <LI>3-of-a-kind: Any one card from the triplet
 * <LI>2-pairs: Any one card from the top pair, followed by any one card from the low pair, followed by the singleton
 * <LI>1-pair: Any one card from the pair, followed by the three singletons in descending order.
 * <LI>High card: The five cards in the hand in descending order
 * </UL
 */
public class HandValue implements Comparable<HandValue>
{
	
	/**
	 * The categories of hand for the purpose of assessing 
	 * the strength of the hand.
	 */
	public enum Category 
	{ HIGH_CARD, PAIR, TWO_PAIRS, THREE_OF_A_KIND, STRAIGHT, FLUSH, FULL_HOUSE, FOUR_OF_A_KIND, STRAIGHT_FLUSH }
	
	private Category mHandValue;	
	private Card[] mTopCards;
	
	private HandValue()
	{ }
	
	public HandValue(Category xCategory, Hand xHand) throws NullPointerException
	{ 
		if(xCategory == null || xHand == null)
			throw new NullPointerException();
		
		Card[] myHand;
		
		mHandValue = xCategory;
		myHand=new Card[5];
		mTopCards=new Card[6];
		
		for(int i = 0; i < mTopCards.length; i++)
			mTopCards[i] = null;
		
		for(int i = myHand.length - 1; i >= 0; i--)
		{
			myHand[i] = xHand.getHighCard();
			xHand.remove(myHand[i]);
		}
		
		for(int i=0; i < myHand.length; i++)
			xHand.add(myHand[i]);
		
		
		if(mHandValue==Category.STRAIGHT_FLUSH)
			setStraightFlush(myHand);
		
		else if (mHandValue==Category.FOUR_OF_A_KIND)
			setFourOfAKind(myHand);
		
		else if (mHandValue==Category.FULL_HOUSE)
			setFullHouse(myHand);
		
		else if (mHandValue==Category.FLUSH)
			setFlush(myHand);
		
		else if (mHandValue==Category.STRAIGHT)
			setStraight(myHand);
		
		else if (mHandValue==Category.THREE_OF_A_KIND)
			setThreeOfAKind(myHand);
	
		else if (mHandValue==Category.TWO_PAIRS)
			setTwoPairs(myHand);
		
		else if (mHandValue==Category.PAIR)
			setPair(myHand);
		
		else
			setHighCard(myHand);	
	}
	
	/**
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo( HandValue pValue ) throws NullPointerException
	{
		if(getCategory() == null || pValue.getCategory() == null ||
		   mTopCards == null)		
			throw new NullPointerException();
		
		int toReturn = getCategory().compareTo(pValue.getCategory());
		
		if(toReturn != 0)
			return toReturn;
		
		
		for(int i = 0; getCard(i) != null; i++)
		{
			toReturn = getCard(i).compareTo(pValue.getCard(i));
			
			if(toReturn != 0)
				return toReturn;
		}
		
		return 0;
	}
	
	public Category getCategory()
	{
		return mHandValue;
	}
	
	public Card getCard(int i) throws IllegalArgumentException
	{	
		if(i < 0 || i > mTopCards.length)
			throw new IllegalArgumentException();
		
		return mTopCards[i];
	}
	
	//hand methods
	
	private void setStraightFlush(Card[] myHand)
	{
		if(myHand[3].getRank() == Card.Rank.Five 
				&& myHand[4].getRank() == Card.Rank.Ace )
			mTopCards[0] = myHand[3];
		else
			mTopCards[0] = myHand[4];
	}
	
	private void setFourOfAKind(Card[] myHand)
	{
		mTopCards[0] = myHand[2];
	}
	
	private void setFullHouse(Card[] myHand)
	{
		mTopCards[0] = myHand[2];
		
		if(myHand[1].compareTo(myHand[2]) == 0)
		{
			mTopCards[1] = myHand[4];
		}
		else
		{
			mTopCards[1] = myHand[1];
		}
	}

	private void setFlush(Card[] myHand)
	{
		for(int i = 0; i < myHand.length; i++)
			mTopCards[4-i]= myHand[i];
	}
	
	private void setStraight(Card[] myHand)
	{
		if(myHand[3].getRank() == Card.Rank.Five 
				&& myHand[4].getRank() == Card.Rank.Ace )
			mTopCards[0] = myHand[3];
		else
			mTopCards[0] = myHand[4];
	}
	
	private void setThreeOfAKind(Card[] myHand)
	{
		mTopCards[0] = myHand[2];
	}
	
	private void setTwoPairs(Card[] myHand)
	{
		if(myHand[4].compareTo(myHand[3]) != 0)
			mTopCards[2] = myHand[4];
		else if(myHand[0].compareTo(myHand[1]) != 0)
			mTopCards[2] = myHand[0];
		else
			mTopCards[2] = myHand[2];		// debu!
		
		mTopCards[1] = myHand[1];
		mTopCards[0] = myHand[3];
	}
	
	private void setPair(Card[] myHand)
	{
		int p=3;
		
		for(int i = 0; i < myHand.length - 1; i++)
			if(myHand[i].compareTo(myHand[i+1]) == 0)
			{
				for(int j = 0; j < i; j++, p--)
					mTopCards[p] = myHand[j];
		
				for(int j = i+2; j < myHand.length; j++, p--)
					mTopCards[p] = myHand[j];

				mTopCards[p] = myHand[i];
			}
	}

	private void setHighCard(Card[] myHand)
	{
		for(int i = 0; i < myHand.length; i++)
			mTopCards[4-i]= myHand[i];
	}
	
	public String toString()
	{
		String Plural = "s";
		String Return;
		
		try
		{
			if(getCategory() == Category.HIGH_CARD)
			{
				return mTopCards[0].getRank() + " High + " + mTopCards[1].getRank() + ", " 
				+ mTopCards[2].getRank() + ", " + mTopCards[3].getRank() + " & " 
				+ mTopCards[4].getRank();
			}
			
			if(getCategory() == Category.PAIR)
			{
				 if(mTopCards[0].getRank() == Card.Rank.Six)
					 Plural = "es";
				 
				 return "a Pair of " + mTopCards[0].getRank() + Plural + " + " 
				 + mTopCards[1].getRank() + ", " + mTopCards[2].getRank() + " & " 
				 + mTopCards[3].getRank();
			}
			
			if(getCategory() == Category.TWO_PAIRS)
			{
				if(mTopCards[0].getRank() == Card.Rank.Six)
					Plural = "es";
				
				Return = mTopCards[0].getRank() + Plural + " over ";
				
				if(mTopCards[1].getRank() == Card.Rank.Six)
					Plural = "es";
				else
					Plural = "s";
				
				Return += mTopCards[1].getRank() + Plural + " + " + mTopCards[2].getRank();
				
				return Return;
			}
			
			if(getCategory() == Category.THREE_OF_A_KIND)
			{
				if(mTopCards[0].getRank() == Card.Rank.Six)
					Plural = "es";
				  
				return "Three " + mTopCards[0].getRank() + Plural; 
			}
			
			if(getCategory() == Category.STRAIGHT)
			{
				return "Straight to " + mTopCards[0].getRank();
			}
			
			if(getCategory() == Category.FLUSH)
			{
				return "Flush";
			}
			
			if(getCategory() == Category.FULL_HOUSE)
			{
				if(mTopCards[0].getRank() == Card.Rank.Six)
					Plural = "es";
				
				Return = mTopCards[0].getRank() + Plural + " full of ";
				
				if(mTopCards[1].getRank() == Card.Rank.Six)
					Plural = "es";
				else
					Plural = "s";
				
				Return += mTopCards[1].getRank() + Plural;
				
				return Return;
			}
			
			if(getCategory() == Category.FOUR_OF_A_KIND)
			{
				if(mTopCards[0].getRank() == Card.Rank.Six)
					Plural = "es";
				
				return "Four " + mTopCards[0].getRank() + Plural;
			}
			
			if(getCategory() == Category.STRAIGHT_FLUSH)
			{
				return "Straight flush to " + mTopCards[0].getRank();
			}
			
		} catch (IllegalArgumentException e)
		{
			
		}
		
		return ""+mHandValue; 
	}
	
}