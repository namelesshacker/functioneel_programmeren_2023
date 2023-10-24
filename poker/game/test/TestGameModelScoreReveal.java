package game.test;

import players.DriverPlayer;
import game.*;
import game.actions.*;
import junit.framework.TestCase;

public class TestGameModelScoreReveal extends TestCase implements GameListener{
	private GameModel gameModel;
	private DriverPlayer p1, p2, p3, p4;
	private ScoreRevealAction last;
	
	public void setUp()
	{
		gameModel = new GameModel();
		
		
		p1=new DriverPlayer("David Kawrykow", 100);
		p2=new DriverPlayer("G Toussaint", 100);
		p3=new DriverPlayer("Gen Kazama", 100);
		p4=new DriverPlayer("David Hasselhoff", 100);
	}
	
	public void testFoldInScoreReveal()
	{
		p1.setPreBetAction(new BetAction(p1.toString(), BetAction.betAction.CALL, 0));
		p2.setPreBetAction(new BetAction(p2.toString(), BetAction.betAction.CALL, 0));
		p3.setPreBetAction(new BetAction(p3.toString(), BetAction.betAction.CALL, 0));
		p4.setPreBetAction(new BetAction(p4.toString(), BetAction.betAction.CALL, 0));

		p1.setPostBetAction(new BetAction(p1.toString(), BetAction.betAction.CALL, 0));
		p2.setPostBetAction(new BetAction(p2.toString(), BetAction.betAction.CALL, 0));
		p3.setPostBetAction(new BetAction(p3.toString(), BetAction.betAction.CALL, 0));
		p4.setPostBetAction(new BetAction(p4.toString(), BetAction.betAction.CALL, 0));

		p1.setScoreRevealAction(new ScoreRevealAction(p1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.FOLD,
													  	p1.getHand()));
		p2.setScoreRevealAction(new ScoreRevealAction(p2.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														p2.getHand()));
		p3.setScoreRevealAction(new ScoreRevealAction(p3.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														p3.getHand()));
		p4.setScoreRevealAction(new ScoreRevealAction(p4.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														p4.getHand()));
		
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.addPlayerToGame(p4);
			gameModel.attachGameListener(this);
			
			gameModel.ExecuteSingleMatch();
			assertTrue(p2.getPurse().getChips()==115);
		}
		catch(Exception e)
		{
			fail();
		}
		
		// The last person to reveal (p2), should win but with a no show
		assertTrue(last.getAction() == ScoreRevealAction.scoreRevealAction.NO_SHOW);
		assertTrue(last.getActionMaker() == p2.toString());
	}
	
	public void testFoldInPreBet()
	{
		p1.setPreBetAction(new BetAction(p1.toString(), BetAction.betAction.FOLD, 0));
		p2.setPreBetAction(new BetAction(p2.toString(), BetAction.betAction.FOLD, 0));
		p3.setPreBetAction(new BetAction(p3.toString(), BetAction.betAction.FOLD, 0));
		p4.setPreBetAction(new BetAction(p4.toString(), BetAction.betAction.FOLD, 0));

		p1.setPostBetAction(new BetAction(p1.toString(), BetAction.betAction.CALL, 0));
		p2.setPostBetAction(new BetAction(p2.toString(), BetAction.betAction.CALL, 0));
		p3.setPostBetAction(new BetAction(p3.toString(), BetAction.betAction.CALL, 0));
		p4.setPostBetAction(new BetAction(p4.toString(), BetAction.betAction.CALL, 0));

		p1.setScoreRevealAction(new ScoreRevealAction(p1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	p1.getHand()));
		p2.setScoreRevealAction(new ScoreRevealAction(p2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p2.getHand()));
		p3.setScoreRevealAction(new ScoreRevealAction(p3.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p3.getHand()));
		p4.setScoreRevealAction(new ScoreRevealAction(p4.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p4.getHand()));
		
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.addPlayerToGame(p4);
			gameModel.attachGameListener(this);
			
			gameModel.ExecuteSingleMatch();
			assertTrue(p2.getPurse().getChips()==115);
		}
		catch(Exception e)
		{
			fail();
		}
		
		// The last person to reveal (p2), should win but with a no show
		assertTrue(last.getAction() == ScoreRevealAction.scoreRevealAction.NO_SHOW);
		assertTrue(last.getActionMaker() == p2.toString());
	}
	
	
	public void testFoldInPostBet()
	{
		p1.setPreBetAction(new BetAction(p1.toString(), BetAction.betAction.CALL, 0));
		p2.setPreBetAction(new BetAction(p2.toString(), BetAction.betAction.CALL, 0));
		p3.setPreBetAction(new BetAction(p3.toString(), BetAction.betAction.CALL, 0));
		p4.setPreBetAction(new BetAction(p4.toString(), BetAction.betAction.CALL, 0));

		p1.setPostBetAction(new BetAction(p1.toString(), BetAction.betAction.FOLD, 0));
		p2.setPostBetAction(new BetAction(p2.toString(), BetAction.betAction.FOLD, 0));
		p3.setPostBetAction(new BetAction(p3.toString(), BetAction.betAction.FOLD, 0));
		p4.setPostBetAction(new BetAction(p4.toString(), BetAction.betAction.FOLD, 0));

		p1.setScoreRevealAction(new ScoreRevealAction(p1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	p1.getHand()));
		p2.setScoreRevealAction(new ScoreRevealAction(p2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p2.getHand()));
		p3.setScoreRevealAction(new ScoreRevealAction(p3.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p3.getHand()));
		p4.setScoreRevealAction(new ScoreRevealAction(p4.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p4.getHand()));
		
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.addPlayerToGame(p4);
			gameModel.attachGameListener(this);
			
			gameModel.ExecuteSingleMatch();
			assertTrue(p2.getPurse().getChips()==115);
		}
		catch(Exception e)
		{
			fail();
		}
		
		// The last person to reveal (p2), should win but with a no show
		assertTrue(last.getAction() == ScoreRevealAction.scoreRevealAction.NO_SHOW);
		assertTrue(last.getActionMaker() == p2.toString());
	}
	
	public void testFoldInDifferentRounds()
	{
		p1.setPreBetAction(new BetAction(p1.toString(), BetAction.betAction.FOLD, 0));
		p2.setPreBetAction(new BetAction(p2.toString(), BetAction.betAction.CALL, 0));
		p3.setPreBetAction(new BetAction(p3.toString(), BetAction.betAction.CALL, 0));
		p4.setPreBetAction(new BetAction(p4.toString(), BetAction.betAction.CALL, 0));

		p1.setPostBetAction(new BetAction(p1.toString(), BetAction.betAction.CALL, 0)); //already folded
		p2.setPostBetAction(new BetAction(p2.toString(), BetAction.betAction.FOLD, 0));
		p3.setPostBetAction(new BetAction(p3.toString(), BetAction.betAction.CALL, 0));
		p4.setPostBetAction(new BetAction(p4.toString(), BetAction.betAction.CALL, 0));

		p1.setScoreRevealAction(new ScoreRevealAction(p1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	p1.getHand()));
		p2.setScoreRevealAction(new ScoreRevealAction(p2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p2.getHand()));
		p3.setScoreRevealAction(new ScoreRevealAction(p3.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														p3.getHand()));
		p4.setScoreRevealAction(new ScoreRevealAction(p4.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														p4.getHand()));
		
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.addPlayerToGame(p4);
			gameModel.attachGameListener(this);
			
			gameModel.ExecuteSingleMatch();
			assertTrue(p4.getPurse().getChips()==115);
		}
		catch(Exception e)
		{
			fail();
		}
		
		// The last person to reveal (p2), should win but with a no show
		assertTrue(last.getAction() == ScoreRevealAction.scoreRevealAction.NO_SHOW);
		assertTrue(last.getActionMaker() == p4.toString());
	}
	
	public void notify(Action action)
	{
		if(action instanceof ScoreRevealAction)
			last = (ScoreRevealAction) action;
	}
}
