package game.test;

import players.PrototypePlayer;
import junit.framework.TestCase;
import game.*;
import game.actions.Action;
import game.actions.BetAction;
import game.actions.CardExchangeAction;
import game.actions.IncrementAnteAction;
import game.actions.NewMatchAction;
import game.actions.ScoreRevealAction;

/**
 * Test for GameStats
 * 
 * @author Gen Kazama and David Kawrykow
 *
 */
public class TestGameStats extends TestCase {

	private GameStats g;
	
	private PrototypePlayer p1;
	private PrototypePlayer p2;
	private PrototypePlayer p3;
	private PrototypePlayer p4;
	
	public void setUp()
	{
		g=new GameStats();
		p1=new PrototypePlayer("G Toussaint", 100);
		p2=new PrototypePlayer("Rocky Balboa", 100);
		p3=new PrototypePlayer("David Kawrykow", 100);
		p4=new PrototypePlayer("Gen Kazama", 100);
		
		g.attachGameListener(p1);
		g.attachGameListener(p2);
		g.attachGameListener(p3);
		g.attachGameListener(p4);
	}
	
	public void testMockGame()
	{
		BetAction b;
		ScoreRevealAction s;
		IncrementAnteAction i;
		CardExchangeAction c;
		
		g.addNewMatchAction(new NewMatchAction());
		
		b=p1.makePreBetAction(0);
				
		g.addPreExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));

		b=p2.makePreBetAction(5);	
		g.addPreExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));
		
		b=p3.makePreBetAction(5);		
		g.addPreExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));
		
		b=p4.makePreBetAction(5);		
		g.addPreExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));
		
		i=new IncrementAnteAction("GameEngine",IncrementAnteAction.incrementAnteAction.FAIL, 0);
		g.addIncrementAnteAction(i);
		
		c=new CardExchangeAction(p1.toString(), CardExchangeAction.cardExchangeAction.NO_DISCARD, 0);
		
		g.addCardExchangeAction(c);
		assertTrue(p1.getLastReceivedAction() == null);
		assertTrue(p2.getLastReceivedAction() == null);
		assertTrue(p3.getLastReceivedAction() == null);
		assertTrue(p4.getLastReceivedAction() == null);
		
		/*Post bets*/
		b=p1.makePostBetAction(0);		
		g.addPostExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));
		
		b=p2.makePostBetAction(0);	
		g.addPostExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));

		b=p3.makePostBetAction(0);		
		g.addPostExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));

		b=p4.makePostBetAction(0);	
		g.addPostExchangeAction(b);
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));
		
		s=p1.makeScoreRevealAction();		
		g.addScoreRevealAction(s);
	
		s=p2.makeScoreRevealAction();	
		g.addScoreRevealAction(s);
		
		s=p3.makeScoreRevealAction();		
		g.addScoreRevealAction(s);
		
		s=p4.makeScoreRevealAction();
		g.addScoreRevealAction(s);
		
		assertTrue(p1.getLastReceivedAction().equals((Action) b));
		assertTrue(p2.getLastReceivedAction().equals((Action) b));
		assertTrue(p3.getLastReceivedAction().equals((Action) b));
		assertTrue(p4.getLastReceivedAction().equals((Action) b));
	}
}
