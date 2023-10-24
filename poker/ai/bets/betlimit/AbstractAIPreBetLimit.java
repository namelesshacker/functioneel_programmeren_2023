package ai.bets.betlimit;

import ai.AIPreBetLimit;
import ai.util.DiscardProbability;
import ai.util.AIPreBetHandValuator; 
import util.*;
import scoring.*;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class is the abstraction of the Pre-Bet-Limit.  It determines the upper bound of betting
 * that the player should consider with their hand and possible hands that they might get after card 
 * exchange (using the DiscardProbability class).
 * 
 */

public abstract class AbstractAIPreBetLimit implements AIPreBetLimit{
	
	protected HandValue value;
	protected DiscardProbability probability;
	protected Hand hand;
	protected AIPreBetHandValuator PreBetHandValuator;
	protected int negativePercentage;
	
	public AbstractAIPreBetLimit()
	{
		hand=new Hand();
		probability = new DiscardProbability();
		PreBetHandValuator = new AIPreBetHandValuator();
	}
	
	public final int getPercentage()
	{
		if(value==null || probability == null)
			assert false;
		
		// If it is a bad Probability return the negative percentage
		if(probability.getProbability() < 0)
			return negativePercentage;
		
		// If it is a positive one, get the positive percentage
		else if(probability.getProbability() > 0)
		{
			HandValue.Category category=PreBetHandValuator.evaluateHand(probability, hand);
		
			if(category == HandValue.Category.FOUR_OF_A_KIND)
				return fourOfAKindPercentage();
			else if(category == HandValue.Category.THREE_OF_A_KIND)
				return threeOfAKindPercentage();
			else if(category == HandValue.Category.TWO_PAIRS)
				return twoPairsPercentage();
			else if(category == HandValue.Category.PAIR)
				return pairPercentage();
			else
				return highCardPercentage();
		}
		
		// If it is a made hand, get the madeHand percentage
		return madeHandPercentage();	
	}
	
	private int madeHandPercentage()
	{	
		if(value.getCategory()==HandValue.Category.STRAIGHT_FLUSH)
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
	
	public final void setDiscardProbability(DiscardProbability probability)
	{
		this.probability=probability;
	}
	
	public final void setHandValue(HandValue value)
	{
		this.value = value;
	}
	
	public final void setHand(Hand hand)
	{
		this.hand=hand;
	}
	
	public String toString()
	{
		return "AbstractPreBet "+hand + " " + probability;
	}
}
