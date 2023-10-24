package ai.test;

import ai.*;
import ai.bets.AIParametricBetDivider;
import ai.bets.AIParametricInstinct;
import ai.bets.betlimit.*;
import ai.bets.bluff.*;
import ai.discards.*;
import ai.util.*;
import junit.framework.TestCase;
import game.actions.*;
import java.util.*;
import game.*;
import players.*;

public class TestAIPlayer extends TestCase implements GameListener{
	private AIParametricPlayer p;
	private AIParametricBrain b;
	private final int SEED=123123123;
	
	private boolean debug;
	
	public void setUp()
	{
		p = new AIParametricPlayer("Gen Kazama", 205, SEED);

		b= new AIParametricBrain("Gen Kazama", 205, SEED);
		
		b.setBetDivider(new AIParametricBetDivider(10, SEED));
	
		b.setBluffer(new AIParametricBluffer(50, 20, SEED));
		
		b.setDiscardStrategy(new AISimpleDiscardStrategy());
		
		b.setBluffFinder(new AIParametricBluffFinder(20, 100, SEED));
		
		b.setInstinct(new AIParametricInstinct(100, SEED));
		
		b.setDecisionMaker(new AISimpleDecisionMaker(70, SEED));	// THE BIGGEST INFLUENCE!! 
	
		b.setPreBetLimit(new AISimplePreBetLimit());
		
		b.setPostBetLimit(new AIAggressivePostBetLimit());
		
		p.setBrain(b);
	}

	public void testDeathMatch()
	{
		debug=true;
		try
		{
			GameModel g = new GameModel();
			
			g.addPlayerToGame(new LeChiffre("", 205, 32984723));
			g.addPlayerToGame(p);
			g.addPlayerToGame(new Robespierre("", 205, 2342311));
			g.addPlayerToGame(new ConsolePlayer("David Kawrykow", 205));
			g.attachGameListener(this);
			Scanner in = new Scanner(System.in);
			while(! g.isGameOver())
			{
				System.out.println("LALALALALA");
				g.ExecuteSingleMatch();
				in.nextLine();
			}
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}
	
	public void notify(Action action)
	{
		//p.notify(action);
		
		if(debug)
			System.out.println(action);
	}
}
