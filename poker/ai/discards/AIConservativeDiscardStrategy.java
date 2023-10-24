package ai.discards;

import scoring.HandValue;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This class represents a conservative player's discard strategy.  This player will
 * make high probability, small risk upgrades worth a lot while high risk, low probability
 * cases punished.  In the case of his hand staying the same, he will reward it (instead of
 * just keeping constant).  In other words, a conservative player wants to discard so that the
 * probabillity of his hand going down is minimal (wants to keep his hand constant/better).
 * 
 * A good example:
 * 
 * The player recevies Js, Jh, Qh, Kh, Ah.  Instead of trying for the high risk, low probability
 * of dropping the Js to try for straight or flush (or royal straight flush), he drops Ah, Kh, Qh
 * and keeps his pair of Jacks.  Therefore, he can only make his hand better (two pairs, three of
 * a kind, full house) or have his hand the same (a pair of Jacks with different kickers).
 *
 */
public class AIConservativeDiscardStrategy extends AIStandardDiscardStrategy {

	public AIConservativeDiscardStrategy()
	{
		super();
	}
	
	protected float scoreFromCategories(HandValue.Category std)
	{
		int t = 0, ord = std.ordinal(), diff, s;  
		float r = (float) 0.0;
		
		// punish bad hands harshly
		for(int i = 0; i < ord; i++)
		{
			t += Categories[i];
			diff = (i - ord) - 3; 
			r += (-1 * diff * diff) * Categories[i]; 
		}
		
		// give small rewards for stability
		t += Categories[ord];
		r += Categories[ord] * 1;
		
		if (ord + 3 > Categories.length )
			s = Categories.length; 
		else
			s = ord + 3; 
		
		// give high rewards for small gains
		for(int i = ord + 1; i < s; i++ )
		{
			t += Categories[i];
			r += Categories[i] * (i - ord) * (i - ord);
		}
		
		// give small rewards for high gains
		for(int i = s; i < Categories.length; i++ )
		{
			t += Categories[i];
			r += Categories[i];
		}
		
		return r/t; 
	}
	
	protected int updateScore(int score)
	{
		// Punishes for the hand going down
		if(score < 0)
		{ 
			score -= 3; 
			return -1*score*score;
		}
		// Big reward for the hand going up for high probabillity cases
		else if(score > 0 && score < 3)
			return score*score;

		// Small Reward if hand is same or difference is greater than 3
		return 1;
	}
}
