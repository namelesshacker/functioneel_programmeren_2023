package ai.bets.betlimit;

import ai.AIPostBetLimit;
import scoring.*;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class handles the percentage of the purse to bet for the PostBet.  In the post bet, you
 * already know your hand so it determines based on your hand category what percentage of your hand
 * to bet.
 *
 */

public abstract class AbstractAIPostBetLimit implements AIPostBetLimit{
	private HandValue value;
	
	public AbstractAIPostBetLimit()
	{}
	
	public AbstractAIPostBetLimit(HandValue value)
	{
		this.value=value;
	}
	
	//Every subclass overwrites this
	public final int getPercentage()
	{
		if(value == null)
			return 0;	
		else if(value.getCategory()==HandValue.Category.STRAIGHT_FLUSH)
			return straightFlushPercentage();
		else if(value.getCategory()==HandValue.Category.FOUR_OF_A_KIND)
			return fourOfAKindPercentage();
		else if(value.getCategory()==HandValue.Category.FULL_HOUSE)
			return fullHousePercentage();
		else if(value.getCategory()==HandValue.Category.FLUSH)
			return flushPercentage();
		else if(value.getCategory()==HandValue.Category.STRAIGHT)
			return straightPercentage();
		else if(value.getCategory()==HandValue.Category.THREE_OF_A_KIND)
			return threeOfAKindPercentage();
		else if(value.getCategory()==HandValue.Category.TWO_PAIRS)
			return twoPairsPercentage();
		else if(value.getCategory()==HandValue.Category.PAIR)
			return pairPercentage();

		return highCardPercentage();
	}
	
	protected abstract int straightFlushPercentage();
	protected abstract int fourOfAKindPercentage();
	protected abstract int fullHousePercentage();
	protected abstract int flushPercentage();
	protected abstract int straightPercentage();
	protected abstract int threeOfAKindPercentage();
	protected abstract int twoPairsPercentage();
	protected abstract int pairPercentage();
	protected abstract int highCardPercentage();
	
	public final HandValue getHandValue()
	{
		return value;
	}
	
	public final void setHandValue(HandValue value)
	{
		this.value=value;
	}
	
	public String toString()
	{
		return "AbstractPreBet "+value;
	}
}
