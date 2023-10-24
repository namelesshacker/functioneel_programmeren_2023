package ai.test;

import junit.framework.TestCase;
import ai.*;
import ai.bets.*;
import ai.bets.betlimit.*;
import ai.bets.bluff.*;
import ai.discards.*;
import ai.util.*;
import game.*;
import players.*;
import game.actions.*;

public class TestAIParametricPlayer extends TestCase implements GameListener{

	ConsolePlayer c;
	AIParametricPlayer p;
	AIParametricBrain b;
	PrototypePlayer p1, p2;
	GameModel g;
	final int SEED=0;
	
	public void setUp()
	{
		c = new ConsolePlayer("Gen Kazama", 105);
		p = new AIParametricPlayer("David Kawrykow", 105, SEED);
		p1= new PrototypePlayer("David Hasselhoff", 105);
		p2= new PrototypePlayer("G Toussaint", 105);
		
		b= new AIParametricBrain("David Kawrykow", 105, SEED);
		
		b.setBetDivider(new AIParametricBetDivider(10, SEED));
	
		b.setBluffer(new AIParametricBluffer(10, 20, SEED));
		
		b.setDiscardStrategy(new AIHighHandDiscardStrategy());
		
		b.setBluffFinder(new AIParametricBluffFinder(5, 85, SEED));
		
		b.setInstinct(new AIParametricInstinct(80, SEED));
		
		b.setDecisionMaker(new AISimpleDecisionMaker(0, SEED));	// THE BIGGEST INFLUENCE!! 
	
		b.setPreBetLimit(new AIAggressivePreBetLimit());
		
		b.setPostBetLimit(new AIAggressivePostBetLimit());
		
		p.setBrain(b);
		
		g = new GameModel();
		try
		{
			g.addPlayerToGame(c);
			g.addPlayerToGame(p);
			g.addPlayerToGame(p1);
			g.addPlayerToGame(p2);
			g.attachGameListener(this);
		}
		catch(Exception e)
		{
			e.printStackTrace();
			assertTrue(false);
		}
	}
	
	public void testOneGame()
	{
		while(! g.isGameOver())
		{
			try
			{
				g.ExecuteSingleMatch();
			}
			catch(Exception e)
			{
				e.printStackTrace();
				assertTrue(false);
			}
		}
	}

	
	public void notify(Action action)
	{
		System.out.println(action);
	}
}
