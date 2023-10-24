package ai.test;

import junit.framework.*;
import ai.bets.*;
import util.Hand;
import util.test.*;
import scoring.*;

public class TestAIParametricInstinct extends TestCase {

	private AIParametricInstinct x; 
	private int SEED = 12082303;
	private int AGGRESSION_FACTOR = 50; 
	private Hand h1, h2, h3, h4, h5;
	private HandValue hv1, hv2, hv3, hv4, hv5;
	private HandValuator y; 
	
	public TestAIParametricInstinct()
	{
		super(); 
		
		y = new HandValuator();
		
		h1 = new Hand(); 		// HIGH CARD
		h2 = new Hand(); 		// PAIR
		h3 = new Hand(); 		// TWO PAIR
		h4 = new Hand(); 		// THREE OF A KIND
		h5 = new Hand();		// FOUR OF A KIND
		
		h1.add(util.test.AllCards.a2C);
		h2.add(util.test.AllCards.a2C);
		h3.add(util.test.AllCards.a2C);
		h4.add(util.test.AllCards.a2C);
		h5.add(util.test.AllCards.a2C);
		
		h1.add(util.test.AllCards.a3D);
		h2.add(util.test.AllCards.a2D);
		h3.add(util.test.AllCards.a2D);
		h4.add(util.test.AllCards.a2D);
		h5.add(util.test.AllCards.a2D);
		
		h1.add(AllCards.a4C);
		h2.add(AllCards.a4C);
		h3.add(AllCards.a4C);
		h4.add(AllCards.a4C);
		h5.add(util.test.AllCards.a2H);
		
		h1.add(AllCards.a5D);
		h2.add(AllCards.a5D);
		h3.add(AllCards.a4D);
		h4.add(AllCards.a5D);
		h5.add(util.test.AllCards.a2S);
		
		h1.add(AllCards.a7D);
		h2.add(AllCards.a7D);
		h3.add(AllCards.a5D);
		h4.add(AllCards.a2H);
		h5.add(AllCards.a5D);
		
		hv1 = y.valuateHand(h1);
		hv2 = y.valuateHand(h2);
		hv3 = y.valuateHand(h3);
		hv4 = y.valuateHand(h4);
		hv5 = y.valuateHand(h5);
		
	}
	
	public void setUp()
	{
		x = new AIParametricInstinct(AGGRESSION_FACTOR, SEED); 
	}
	
	public void testPreBetDecisionsForLowestMax()
	{	
		/* Check each bet setting with each hand kind */
		x.setCurrentMax(20);
		
		// lowest: we should fold
		x.setHandValue(hv1);
		assertFalse(x.getPreBetFoldPlayDecision());			
		
		// low: we shouldnt fold, but we do twice
		x.setHandValue(hv2);
		assertFalse(x.getPreBetFoldPlayDecision());
		assertFalse(x.getPreBetFoldPlayDecision());
		assertTrue(x.getPreBetFoldPlayDecision());
		
		// medium : play on
		x.setHandValue(hv3);
		assertTrue(x.getPreBetFoldPlayDecision());
		
		// high : play on
		x.setHandValue(hv4);
		assertTrue(x.getPreBetFoldPlayDecision());
		
		// highest : play on
		x.setHandValue(hv5);
		assertTrue(x.getPreBetFoldPlayDecision());	
	}
	
	public void testPreBetDecisionsForMediumMax()
	{	
		/* Check each bet setting with each hand kind */
		x.setCurrentMax(45);
		
		// lowest: we should fold
		x.setHandValue(hv1);
		assertFalse(x.getPreBetFoldPlayDecision());			
		
		// low: we should fold 
		x.setHandValue(hv2);
		assertFalse(x.getPreBetFoldPlayDecision());
		assertFalse(x.getPreBetFoldPlayDecision());
		assertFalse(x.getPreBetFoldPlayDecision());
		
		// medium : play on
		x.setHandValue(hv3);
		assertTrue(x.getPreBetFoldPlayDecision());
		
		// high : play on
		x.setHandValue(hv4);
		assertTrue(x.getPreBetFoldPlayDecision());
		
		// highest : play on
		x.setHandValue(hv5);
		assertTrue(x.getPreBetFoldPlayDecision());	
	}
	
	public void testPreBetDecisionsForHighestMax()
	{	
		/* Check each bet setting with each hand kind */
		x.setCurrentMax(65);
		
		// lowest: we should fold
		x.setHandValue(hv1);
		assertFalse(x.getPreBetFoldPlayDecision());			
		
		// low: we should fold 
		x.setHandValue(hv2);
		assertFalse(x.getPreBetFoldPlayDecision());
		assertFalse(x.getPreBetFoldPlayDecision());
		assertFalse(x.getPreBetFoldPlayDecision());
		
		// medium : should fold
		x.setHandValue(hv3);
		assertFalse(x.getPreBetFoldPlayDecision());
		assertFalse(x.getPreBetFoldPlayDecision());
		assertFalse(x.getPreBetFoldPlayDecision());
		
		// high : play on
		x.setHandValue(hv4);
		assertTrue(x.getPreBetFoldPlayDecision());
		
		// highest : play on
		x.setHandValue(hv5);
		assertTrue(x.getPreBetFoldPlayDecision());	
	}
	
	public void testPostBetDecisionsForLowestMax()
	{	
		/* Check each bet setting with each hand kind */
		x.setCurrentMax(5);
		
		// lowest: we should fold
		x.setHandValue(hv1);
		assertFalse(x.getPostBetFoldPlayDecision());			
		
		// low: we shouldnt fold, but we do twice, as before
		x.setHandValue(hv2);
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		assertTrue(x.getPostBetFoldPlayDecision());
		
		// medium : play on
		x.setHandValue(hv3);
		assertTrue(x.getPostBetFoldPlayDecision());
		
		// high : play on
		x.setHandValue(hv4);
		assertTrue(x.getPostBetFoldPlayDecision());
		
		// highest : play on
		x.setHandValue(hv5);
		assertTrue(x.getPostBetFoldPlayDecision());
	}
	
	public void testPostBetDecisionsForLowMax()
	{	
		/* Check each bet setting with each hand kind */
		x.setCurrentMax(23);
		
		// lowest: we should fold
		x.setHandValue(hv1);
		assertFalse(x.getPostBetFoldPlayDecision());			
		
		// low: we should fold
		x.setHandValue(hv2);
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		
		// medium : play on
		x.setHandValue(hv3);
		assertTrue(x.getPostBetFoldPlayDecision());
		
		// high : play on
		x.setHandValue(hv4);
		assertTrue(x.getPostBetFoldPlayDecision());
		
		// highest : play on
		x.setHandValue(hv5);
		assertTrue(x.getPostBetFoldPlayDecision());
	}
	
	public void testPostBetDecisionsForMediumMax()
	{	
		/* Check each bet setting with each hand kind */
		x.setCurrentMax(43);
		
		// lowest: we should fold
		x.setHandValue(hv1);
		assertFalse(x.getPostBetFoldPlayDecision());			
		
		// low: we should fold
		x.setHandValue(hv2);
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		
		// medium : fold
		x.setHandValue(hv3);
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		
		// high : play on
		x.setHandValue(hv4);
		assertTrue(x.getPostBetFoldPlayDecision());
		
		// highest : play on
		x.setHandValue(hv5);
		assertTrue(x.getPostBetFoldPlayDecision());
	}
	
	public void testPostBetDecisionsForHighMax()
	{	
		/* Check each bet setting with each hand kind */
		x.setCurrentMax(63);
		
		// lowest: we should fold
		x.setHandValue(hv1);
		assertFalse(x.getPostBetFoldPlayDecision());			
		
		// low: we should fold
		x.setHandValue(hv2);
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		
		// medium : fold
		x.setHandValue(hv3);
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		assertFalse(x.getPostBetFoldPlayDecision());
		
		// high : play on
		x.setHandValue(hv4);
		assertFalse(x.getPostBetFoldPlayDecision());
		
		// highest : play on
		x.setHandValue(hv5);
		assertTrue(x.getPostBetFoldPlayDecision());
	}
}
