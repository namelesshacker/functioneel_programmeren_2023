package ai.discards;

import scoring.HandValue;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class represents the Discard strategy for an aggressive player.  An agressive player
 * will break an already made hand to try for a hand that rewards well (but has low probability)
 * 
 * A good example:
 * 
 * The player recevies Js, Jh, Qh, Kh, Ah.  The aggressive nature forces him to destroy the
 * already made (but low reward) pair of jacks and tries for a higher pair, straight, flush, 
 * straight flush, or royal straight flush.  He does not care about the probability, he only
 * goes for hands that feature high reward.
 *
 */

public class AIAggressiveDiscardStrategy extends AIStandardDiscardStrategy{

	public AIAggressiveDiscardStrategy()
	{
		super();
	}
	
	protected float scoreFromCategories(HandValue.Category std)
	{
		int t = 0, ord = std.ordinal(), s;
		float r = (float)0.0;
		
		// Bad hands are not punished
		for(int i = 0; i < ord; i++)
		{
			t += Categories[i];
			r += Categories[i] * (i - ord);
		}
		
		if(ord + 3 > Categories.length)
			s = Categories.length; 
		else
			s = ord + 3; 
		
		// Medium, Low hands are punished
		for(int i = ord; i < s; i++)
		{
			t += Categories[i]; 
			r -= (i - ord) * Categories[i];
		}
		
		// High cards are rewarded
		for(int i = ord + 3; i < Categories.length; i++)
		{
			t += Categories[i]; 
			r += (i - ord) * (i - ord) * Categories[i];
		}
		
		return r/t; 
	}
}
