package ai.test;

import junit.framework.*;
import ai.bets.*;

public class TestAIParametricBetDivider extends TestCase {

	private AIParametricBetDivider x;
	private int SEED = 2349083;
	private int ExpectedRandom[]; 
	private final int INIT_RANGE = 4;
	
	public void setUp()
	{
		x = new AIParametricBetDivider(INIT_RANGE, SEED);
	}
	
	// this method mimics the code in the class in question 
	public void testSingleNormalBetRaise()
	{
		ExpectedRandom = x.getIntegerSequence(11);
		
		for(int i = 0; i <= 10; i++)
		{
			// compute a random change in the allowed range
			int randomChange=(ExpectedRandom[i] * 2 * INIT_RANGE) / 100;
			
			// ignore decrements
			if(randomChange < INIT_RANGE)
				randomChange = 0;

			assertTrue ( randomChange + 1 == x.getNextBet() );	
		}
	}
}
