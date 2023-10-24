package ai.discards;

import scoring.HandValue;

public class AIHighHandDiscardStrategy extends AIStandardDiscardStrategy {

	public AIHighHandDiscardStrategy()
	{
		super();
	}
	
	protected float scoreFromCategories(HandValue.Category std)
	{
		int t = 0, diff;  
		float r = (float)0.0;
		
		// if already have something very good, stay
		if(std.ordinal() >= HandValue.Category.STRAIGHT.ordinal())
			return -1; 
		
		// 2 pair, 3 of a kind are strongly favored because most likely to happen
		diff = Categories[HandValue.Category.TWO_PAIRS.ordinal()];
		r += diff * diff;
		t += diff; 
		diff = Categories[HandValue.Category.THREE_OF_A_KIND.ordinal()];
		r += diff * diff * 2;
		t += diff; 
		
		// low hands are strongly discouraged
		diff = Categories[HandValue.Category.HIGH_CARD.ordinal()]; 
		r -= diff * 2; 
		t += diff; 
		diff = Categories[HandValue.Category.PAIR.ordinal()];
		r -= diff; 
		t += diff; 
		
		// higher hands are favored, but not greatly
		for(int i = HandValue.Category.STRAIGHT.ordinal(); i < Categories.length; i++)
		{
			t += Categories[i];
			r += Categories[i];
		}
		
		return r/t; 
	}
	
}
