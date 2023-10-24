package game.test;

import junit.framework.TestCase;
import game.actions.BetAction;
import game.actions.BetAction.betAction;

public class TestBetAction extends TestCase {

	 public void testConstructor()
	 {
		 BetAction ba;
		 
		 try{
			 ba = new BetAction(null, betAction.FOLD, 0);
			 fail();
		 } catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
		 
		 ba = new BetAction("JUNIT TEST", betAction.FOLD, -1);
		 assertTrue(ba.getAmount() == 0);
		 
		 try
		 {
			 ba = new BetAction("JUNIT TEST", betAction.RAISE, -1);
			 fail();
		 } catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
		 
		 ba = new BetAction("GOOD TEST", betAction.RAISE, 50);
		 assertTrue(ba.getAmount() == 50);
	 }
}
