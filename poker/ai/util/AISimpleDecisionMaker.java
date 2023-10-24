package ai.util;

/**
 * 
 * @author david kawrykow and gen kazama.
 * 
 * Is used by AI to make simple yes-no decisions. The divider sets the 
 * proportion of times the decision maker returns "no" or (FALSE).  
 *
 */

public class AISimpleDecisionMaker extends AINumberGenerator {
	
	private int mDivider; 
	
	/**
	 * @pre Divider > 25 && Divider < 75
	 * @param Divider
	 * @param Seed
	 */
	public AISimpleDecisionMaker(int Divider, int Seed)
	{
		super(Seed);
		assert (Divider > -1  && Divider < 101 );
		mDivider = Divider; 
	}
	
	public boolean getDecision()
	{
		return getNextInteger() >= mDivider; 
	}
	
	public int getDivider()
	{
		return mDivider;
	}
}
