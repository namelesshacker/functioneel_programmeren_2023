package players.test;

import junit.framework.TestCase;
import players.*;
import game.*;
import game.actions.*;

public class TestConsolePlayer extends TestCase implements GameListener{

	ConsolePlayer c;
	PrototypePlayer p1, p2, p3;
	GameModel g;
	
	public void setUp()
	{
		c = new ConsolePlayer("Gen Kazama", 100);
		p1 = new PrototypePlayer("David Kawrykow", 100);
		p2 = new PrototypePlayer("David Hasselhoff", 100);
		p3 = new PrototypePlayer("G Toussaint", 100);
		
		g = new GameModel();
		
		try
		{
			g.addPlayerToGame(c);
			g.addPlayerToGame(p1);
			g.addPlayerToGame(p2);
			g.addPlayerToGame(p3);
			g.attachGameListener(this);
		}
		catch(Exception e)
		{
			//Should not get here
			e.printStackTrace();
			assertTrue(false);
		}
	}

	public void testOneMatch()
	{
		try
		{
			g.ExecuteSingleMatch();
		}
		catch(Exception e)
		{
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
				assertTrue(false);
			}
		}
		assertTrue(g.isGameOver());
	}
	
	public void notify(Action action)
	{
		System.out.println(action);
	}
}
