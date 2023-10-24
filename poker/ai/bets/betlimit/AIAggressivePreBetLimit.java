package ai.bets.betlimit;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This extends the AbstractAIPreBetLimit and makes it aggressive.  As you can see, it bets
 * relatively high, hoping for a great hand, and if it already has a decent hand, bets a high
 * percentage of its pot.
 *
 */

public class AIAggressivePreBetLimit extends AbstractAIPreBetLimit{

	private final int STRAIGHT_FLUSH = 100;
	private final int FOUR_OF_A_KIND=100;
	private final int FULL_HOUSE = 100;
	private final int FLUSH = 100;
	private final int STRAIGHT = 100;
	private final int THREE_OF_A_KIND = 100;
	private final int TWO_PAIRS = 55;
	private final int PAIR = 18;
	private final int HIGH_CARD = 7;	// This means you have a draw for something
	
	public AIAggressivePreBetLimit()
	{
		super();
		negativePercentage = 5;
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
		return "Aggressive "+hand + " " + probability;
	}
}
