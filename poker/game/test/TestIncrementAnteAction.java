package game.test;

import junit.framework.TestCase;
import game.actions.IncrementAnteAction;
import game.actions.IncrementAnteAction.incrementAnteAction;

public class TestIncrementAnteAction extends TestCase {

	 public void testConstructor()
	 {
		 IncrementAnteAction iaa;
		 
		 try{
			 iaa = new IncrementAnteAction(null, incrementAnteAction.FAIL, 0);
			 fail();
		 } catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
		 
		 iaa = new IncrementAnteAction("JUNIT TEST", incrementAnteAction.NULL, 0);
		 assertTrue(iaa.getAction() == incrementAnteAction.NULL);
		 
		 try
		 {
			 iaa = new IncrementAnteAction("JUNIT TEST", null, 0);
			 fail();
		 } 
		 
		 catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
	}
}
