package game.test;

import junit.framework.TestCase;
import game.actions.ScoreRevealAction;
import game.actions.ScoreRevealAction.scoreRevealAction;
import util.Hand;

public class TestScoreRevealAction extends TestCase {
	
	public void testConstructor()
	 { 
		 try{
			 ScoreRevealAction ba = new ScoreRevealAction(null, scoreRevealAction.FOLD, new Hand());
			 fail();
		 } catch(IllegalArgumentException e)
		 {
			 /* Good */
		 }
	 }
}
