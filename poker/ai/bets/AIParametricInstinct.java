package ai.bets;

import scoring.HandValue;
import ai.AIInstinct;
import ai.util.*;

public class AIParametricInstinct extends AINumberGenerator implements AIInstinct{

//	 The maximum which no longer satisfies the world
	private int currentMax=0;
	// The hand value which determines the player's fate
	private HandValue value;
	// The Aggression Factor
	private final int AGGRESSION_FACTOR;
	// The small part of you that keeps playing even if you know you have
	// a bad hand
	private final int SPORADIC_FACTOR=2; 
	
	/**
	 * @pre AggressionFactor is a number between 0 and 100
	 * @param AggressionFactor The higher it is, the more likely you are to keep playing
	 * in sticky situations. 
	 */
	public AIParametricInstinct(int AggressionFactor, int Seed)
	{
		super(Seed); 
		assert (AggressionFactor > -1 && AggressionFactor < 101);
		AGGRESSION_FACTOR = AggressionFactor;
	}
	
	/**
	 * Implements Interface
	 */
	public boolean getPostBetFoldPlayDecision()
	{
		if(currentMax < 10)
			return getFoldPlayDecision(0); 
			
		if(currentMax < 25)
			return getFoldPlayDecision(1);
			
		if(currentMax < 50)
			return getFoldPlayDecision(2); 
		
		else
			return getFoldPlayDecision(3); 	
	}
	
	/**
	 * Implements interface
	 */
	public boolean getPreBetFoldPlayDecision()
	{
		// Fold if little money in pot
		if(currentMax < 25)
			return getFoldPlayDecision(0);
			
		if(currentMax < 50)
			return getFoldPlayDecision(1); 
		
		else
			return getFoldPlayDecision(2); 	
	}
	
	/**
	 * If the current hand value is less than or equal to the "bad ordinal"
	 * then only a sporadic decision will make you keep playing. Otherwise
	 * a decision based on your aggression factor is returned.
	 * @return TRUE to keep playing, FALSE to stop playing
	 */
	private boolean getFoldPlayDecision(int BadOrdinal)
	{
		if (value.getCategory().ordinal() <= BadOrdinal)
			return (getNextInteger() < SPORADIC_FACTOR);
		else
			return (getNextInteger() < AGGRESSION_FACTOR); 
	}

	/**
	 * Sets the current max which gets evaluated during decision making
	 */
	public final void setCurrentMax(int currentMax)
	{
		this.currentMax=currentMax;
	}
	
	/**
	 * Sets the hand value which gets evaluated during decision making
	 */
	public final void setHandValue(HandValue value)
	{
		this.value=value;
	}
	
}
