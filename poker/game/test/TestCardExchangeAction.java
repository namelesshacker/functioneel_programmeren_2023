package game.test;

import junit.framework.TestCase;
import game.actions.CardExchangeAction;
import game.actions.CardExchangeAction.cardExchangeAction;

public class TestCardExchangeAction extends TestCase {
	
	public void testConstructor()
	 {
		 CardExchangeAction ba;
		 
		 try{
			 ba = new CardExchangeAction(null, cardExchangeAction.DISCARD, 0);
			 fail();
		 } catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
		 
		 ba = new CardExchangeAction("JUNIT TEST", cardExchangeAction.NO_DISCARD, -1);
		 assertTrue(ba.getAmount() == 0);
		 
		 try
		 {
			 ba = new CardExchangeAction("JUNIT TEST", cardExchangeAction.DISCARD, -1);
			 fail();
		 } catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
		 
		 try
		 {
			 ba = new CardExchangeAction("JUNIT", cardExchangeAction.DISCARD, 50);
			 fail();
		 } catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
		 
		 ba = new CardExchangeAction("JUNIT", cardExchangeAction.DISCARD, 2);
		 assertTrue(ba.getAmount() == 2); 
	 }
	
}
