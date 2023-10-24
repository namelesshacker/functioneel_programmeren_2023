package ai.bets.betlimit;

import scoring.HandValue;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class represents the upper bound an aggressive player would take during the post bet round.
 * They almost always bet a large portion of their pot.
 *
 */

public class AIAggressivePostBetLimit extends AbstractAIPostBetLimit{

	private final int STRAIGHT_FLUSH = 100;
	private final int FOUR_OF_A_KIND=100;
	private final int FULL_HOUSE = 100;
	private final int FLUSH = 100;
	private final int STRAIGHT = 100;
	private final int THREE_OF_A_KIND = 65;
	private final int TWO_PAIRS = 40;
	private final int PAIR = 15;
	private final int HIGH_CARD = 0;
	
	public AIAggressivePostBetLimit()
	{
		super();
	}
	
	public AIAggressivePostBetLimit(HandValue value)
	{
		super(value);
	}
	
	protected int straightFlushPercentage()
	{
		return STRAIGHT_FLUSH;
	}
	
	protected int fourOfAKindPercentage()
	{
		return FOUR_OF_A_KIND;
	}
	
	protected int fullHousePercentage()
	{
		return FULL_HOUSE;
	}
	
	protected int flushPercentage()
	{
		return FLUSH;
	}
	
	protected int straightPercentage()
	{
		return STRAIGHT;
	}
	
	protected int threeOfAKindPercentage()
	{
		return THREE_OF_A_KIND;
	}
	
	protected int twoPairsPercentage()
	{
		return TWO_PAIRS;
	}
	
	protected int pairPercentage()
	{
		return PAIR;
	}
	
	protected int highCardPercentage()
	{
		return HIGH_CARD;
	}
}
