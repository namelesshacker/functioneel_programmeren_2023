package ai.test;

import ai.bets.bluff.AIParametricBluffer;
import junit.framework.*; 

public class TestAIParametricBluffer extends TestCase {

	AIParametricBluffer a;
	int seed;
	int ExpectedRandom[];
	
	public TestAIParametricBluffer()
	{
		seed=43593485;
	}
	
	public void setUp()
	{
		a = new AIParametricBluffer(10, 60, seed);
		a.setBettingPercentage(10);
	}
	
	/**
	 * We know the random integers the bluffer produces. So we check whether
	 * it does exactly what we said it should do 
	 */
	public void testPreBluffs()
	{
		// initial bet range is 10%, bluffing 10% of time, we add 60% when bluffing	
		ExpectedRandom = a.getIntegerSequence(100);
		
		for(int i=0; i <100; i++)
			if ( ExpectedRandom[i] > 10 )
				assertTrue(a.getPreBluff() == 70);
			else
				assertTrue(a.getPreBluff() == 10);
	}
	
	public void testPostBluffs()
	{
		// initial bet range is 10%, bluffing 10% of time, we add 60% when bluffing
		ExpectedRandom = a.getIntegerSequence(100);
		
		for(int i=0; i <100; i++)
		{
			
			// if bluffs in pre-bet, we know it bluffs in post bet
			if ( ExpectedRandom[i] > 10 )
			{
				assertTrue(a.getPreBluff() == 70);
				assertTrue(a.getPostBluff() == 70); 
			}
		
			// otherwise it might bluff in post bet
			else
			{
				assertTrue(a.getPreBluff() == 10);
				assertTrue(a.getBluff() == false); 
				
				i++;
				
				// the next random number now says bluff or not
				if( ExpectedRandom[i] > 10 )
					assertTrue(a.getPostBluff() == 70);
				else 
					assertTrue(a.getPostBluff() == 10); 
			}
		}
	} 
	
}
