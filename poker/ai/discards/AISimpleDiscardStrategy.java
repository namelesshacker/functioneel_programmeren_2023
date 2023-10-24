package ai.discards;

import ai.util.DiscardProbability;

public class AISimpleDiscardStrategy extends AIStandardDiscardStrategy{
	
	public AISimpleDiscardStrategy()
	{
		super();
	}
	
	/**
	 * This method works like in Standard except that when no clear move is available
	 * for best and second best, the player simply discards the lowest 3
	 */
	protected DiscardProbability getBestDiscard()
	{
		int maxi = -1;
		int smaxi = 0;
		int bd = 15; 
		float avg = (float)0.0;
		float max = (float)0.0;
		float cur;
		
		for(int i = 0; i < NUM_DISCARDS_POSSIBLE-1; i++)
		{
			cur = table[i].getProbability();
			avg += cur;
			
			if(cur > max)
			{
				max = cur;
				if(maxi != -1)
					smaxi = maxi; 
				maxi=i;
			}	
		}
		
		avg /= (NUM_DISCARDS_POSSIBLE-1);
		
		// if all were negative, then return the empty discard
		if(maxi == -1)
		{
			table[(NUM_DISCARDS_POSSIBLE - 1)].setProbability((float)0.0);
			return table[(NUM_DISCARDS_POSSIBLE - 1)];
		}
		
		// don't be too dumb! If only one good hand, he goes for it.
		if( table[maxi].getProbability() > 0 && table[smaxi].getProbability() < 0)
		{
			smaxi = maxi;
			bd = 5; 
		}
		
		// don't be too dumb! If the second hand is good but not that good, keep the better
		else if( table[maxi].getProbability() - table[smaxi].getProbability() > DENY_STDEV)
		{
			smaxi = maxi;
			bd = 5; 
		}
		
		if(getStandardDeviationFromTable(avg) < DENY_STDEV
				&& table[smaxi].getProbability() - avg < DENY_AVG)	
		{
			return table[bd];		// instead of warning about probabilities, return smallest 2,3-card discard 
		}
		
		return table[smaxi];
	}
	
}
