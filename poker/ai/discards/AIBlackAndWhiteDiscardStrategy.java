package ai.discards;

import scoring.HandValue;

public class AIBlackAndWhiteDiscardStrategy extends AISimpleDiscardStrategy {

	public AIBlackAndWhiteDiscardStrategy() 
	{
		super();
	}
	
	protected float scoreFromCategories(HandValue.Category std)
	{
		int t = 0, ord = std.ordinal();  
		float r = (float)0.0;
		
		for(int i = 0; i < ord; i++)
		{
			t += Categories[i];
			r -= Categories[i];
		}
		
		for(int i = ord; i < Categories.length; i++)
		{
			t += Categories[i];
			r += Categories[i];
		}
		
		return r/t; 
	}

	
}
