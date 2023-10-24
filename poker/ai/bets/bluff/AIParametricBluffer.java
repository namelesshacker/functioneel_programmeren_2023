package ai.bets.bluff;

import ai.AIBluffer;
import ai.util.*;

public class AIParametricBluffer extends AINumberGenerator implements AIBluffer {

	private final int MAX_PERCENTAGE=100;
	
	// Percentage of hands to bluff
	private int bluffPercentage;
	
	// the betting Percentage (from BetLimit)
	private int bettingPercentage;
	
	// amount to bluff by, when bluffing
	private int bluffAmount; 
	
	// If a player is bluffing now (pre bet bluff carries over to post bets)
	private boolean bluff;
	
	public AIParametricBluffer(int bluffPercentage, int bluffAmount, int seed)
	{
		super(seed); 
		
		assert (!(bluffPercentage < 0 || bluffPercentage > MAX_PERCENTAGE));
		assert (!( bluffAmount < 0 || bluffAmount > MAX_PERCENTAGE )); 
		
		this.bluffPercentage = bluffPercentage;
		this.bluffAmount = bluffAmount; 
		
		if(this.bluffAmount > MAX_PERCENTAGE)
			bluffAmount = MAX_PERCENTAGE; 

		bluff=false;
	}
	
	public final int getPreBluff()
	{
		// If the bluff percentage falls in the random number, it bluffs
		int z = getNextInteger(); 
		if( bluffPercentage < z )
		{
			bluff=true;
			return bettingPercentage+bluffAmount;
		}
		
		//Otherwise, it doesnt bluff
		bluff=false;
		return bettingPercentage;
	}
	
	public final int getPostBluff()
	{
		//If it bluffed in the pre bet, continues to do so
		if(bluff)
		{
			bluff=false;
			return bettingPercentage+bluffAmount;
		}
		
		// If not, it tries to bluff in post bet (pretends it got an awesome
		// card exchange
		if( bluffPercentage < getNextInteger() )
		{
			bluff=true;
			return bettingPercentage+bluffAmount;
		}
		
		return bettingPercentage;
	}

	public final void setBluffingPercentage(int bluffingPercentage)
	{
		this.bluffPercentage=bluffingPercentage;
	}
	
	public final int getBluffingPercentage()
	{
		return bluffPercentage;
	}
	
	public final int getBettingPercentage()
	{
		return this.bettingPercentage;	
	}
	
	public final void setBettingPercentage(int bettingPercentage)
	{
		this.bettingPercentage=bettingPercentage;
	}
	
	public final boolean getBluff()
	{
		return bluff;
	}
	
}
