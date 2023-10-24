package game.test;

import junit.framework.TestCase;
import game.*;
import game.actions.NewMatchAction;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This tests the PlayerGameStats class.  Since all of the inherited methods ahve already
 * been tested in TestGameStats.class, we only need to check if PlayerGameStats 
 * automatically keeps its ArrayList under check.
 * 
 */
public class TestPlayerGameStats extends TestCase {

	private PlayerGameStats p;
	
	public void setUp()
	{
		p=new PlayerGameStats();
	}
	
	/**
	 * Make sure that PlayerGameStats keeps its ArrayList less than 5 elements.
	 *
	 */
	public void testSizeOfPlayerGameStats()
	{
		NewMatchAction nma=new NewMatchAction();
		
		/* with nothing added, this should be 0 length */
		assertTrue(p.getMatchNumber()==0);
		
		p.addNewMatchAction(nma);
		assertTrue(p.getMatchNumber()==1);
		
		p.addNewMatchAction(nma);
		assertTrue(p.getMatchNumber()==2);
		
		p.addNewMatchAction(nma);
		assertTrue(p.getMatchNumber()==3);
		
		p.addNewMatchAction(nma);
		assertTrue(p.getMatchNumber()==4);
		
		/* After 4 matches, the oldest match is removed and the new one added*/
		p.addNewMatchAction(nma);
		assertTrue(p.getMatchNumber()==4);
		
		p.addNewMatchAction(nma);
		assertTrue(p.getMatchNumber()==4);	

		p.addNewMatchAction(nma);
		assertTrue(p.getMatchNumber()==4);	
	}
	
}
