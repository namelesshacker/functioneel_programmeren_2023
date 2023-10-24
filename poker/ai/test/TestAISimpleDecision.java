package ai.test;

import junit.framework.TestCase;

import ai.util.*;

public class TestAISimpleDecision extends TestCase {

	private AISimpleDecisionMaker a;
	private int[] in;
	
	public void setUp()
	{
		a = new AISimpleDecisionMaker(50, 0);
	}
	
	/**
	 * Test it once
	 *
	 */
	public void testOneTime()
	{
		in=a.getIntegerSequence(1);
		assertTrue( (in[0] >= 50) == a.getDecision());
	}
	
	/**
	 * Test it 500 times
	 *
	 */
	public void testMultipleTimes()
	{
		in = a.getIntegerSequence(500);
		
		for(int i=0; i<in.length; i++)
			assertTrue( (in[i]>=50) == a.getDecision());
	}
}
