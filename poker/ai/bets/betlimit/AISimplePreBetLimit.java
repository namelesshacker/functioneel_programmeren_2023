package ai.bets.betlimit;

public class AISimplePreBetLimit extends AbstractAIPreBetLimit{
	private final int STRAIGHT_FLUSH = 100;
	private final int FOUR_OF_A_KIND=100;
	private final int FULL_HOUSE = 100;
	private final int FLUSH = 100;
	private final int STRAIGHT = 100;
	private final int THREE_OF_A_KIND = 25;
	private final int TWO_PAIRS = 15;
	private final int PAIR = 7;
	private final int HIGH_CARD = 2;	//This means you have a draw for something
	
	public AISimplePreBetLimit()
	{
		super();
		negativePercentage = 0;
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

	public String toString()
	{
		return "Conservative Pre Bet "+ hand + " " + probability;
	}
}
