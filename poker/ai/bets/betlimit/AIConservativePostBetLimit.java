package ai.bets.betlimit;

import scoring.HandValue;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * Tells a conservative ai how to bet with a certain hand in the post bet round
 *
 */

public class AIConservativePostBetLimit extends AbstractAIPostBetLimit{

	private final int STRAIGHT_FLUSH = 100;
	private final int FOUR_OF_A_KIND=100;
	private final int FULL_HOUSE = 100;
	private final int FLUSH = 100;
	private final int STRAIGHT = 75;
	private final int THREE_OF_A_KIND = 35;
	private final int TWO_PAIRS = 18;
	private final int PAIR = 11;
	private final int HIGH_CARD = 0;
	
	public AIConservativePostBetLimit()
	{
		super();
	}
	
	public AIConservativePostBetLimit(HandValue value)
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
