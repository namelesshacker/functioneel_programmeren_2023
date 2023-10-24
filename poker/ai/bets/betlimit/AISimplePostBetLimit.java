package ai.bets.betlimit;

import scoring.HandValue;

public class AISimplePostBetLimit extends AbstractAIPostBetLimit{
	private final int STRAIGHT_FLUSH = 100;
	private final int FOUR_OF_A_KIND=100;
	private final int FULL_HOUSE = 75;
	private final int FLUSH = 100;
	private final int STRAIGHT = 100;
	private final int THREE_OF_A_KIND = 45;
	private final int TWO_PAIRS = 25;
	private final int PAIR = 5;
	private final int HIGH_CARD = 0;
	
	public AISimplePostBetLimit()
	{
		super();
	}
	
	public AISimplePostBetLimit(HandValue value)
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
