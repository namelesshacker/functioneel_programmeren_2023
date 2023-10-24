package ai.bets;

import ai.AIBetDivider;
import ai.util.*;

public class AIParametricBetDivider extends AINumberGenerator implements AIBetDivider {
	
	// How much more to alter the minimum for the pot by
	private int range;
	
	/**
	 * @param range The range of bets you wish to generate 
	 * @param seed The seed for the generator
	 */
	public AIParametricBetDivider(int range, int seed)
	{
		super(seed);

		if(range < 0 || range > 100)
			assert false; 
		
		this.range=range;
	}
	
	/**
	 * @return The next bet Percentage / Raise
	 */
	public final int getNextBet()
	{
		// compute a random change in the allowed range
		int randomChange=(getNextInteger() * 2 * range) / 100;
		
		// ignore decrements
		if(randomChange < range)
			randomChange = 0;

		return randomChange + 1;
	}

	/**
	 * Sets the range of bets this class dishes out for you 
	 */
	public final void setRange(int range)
	{
		this.range=range;
	}
	
	public final int getRange()
	{
		return range;
	}
	
	public String toString()
	{
		return "BET DIVIDER: RANGE (" + range;
	}

}
