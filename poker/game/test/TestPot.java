package game.test;

import money.Pot;
import money.Purse;
import junit.framework.TestCase;
import game.exceptions.InvalidMoneyValueException;

public class TestPot extends TestCase {

	private Pot p;
	private Purse[] purses;
	
	
	public void setUp()
	{
		//create a generic pot with 4 players
		p=new Pot(4);
		purses=new Purse[4];
	}
	
	public void testIsFull()
	{
		// check null array returns false
		assertTrue( p.isFull());
		
		// check it works if all numbers are equal
		try{
			purses[0]=new Purse(1);
			purses[1]=new Purse(1);
			purses[2]=new Purse(1);
			purses[3]=new Purse(1);
		
			p=new Pot(purses);
			
			assertTrue(p.isFull());
		}
		catch(Exception e)
		{
			//should not get here
			fail();
		}
		
		// check it returns false
		try{
			purses[0]=new Purse(1);
			purses[1]=new Purse(1);
			purses[2]=new Purse(1);
			purses[3]=new Purse(3);
		
			p=new Pot(purses);
			
			assertTrue(! p.isFull());
		}
		catch(Exception e)
		{
			//should not get here
			fail();
		}
	}
	
	public void testGetAllIn()
	{
		try{
			purses[0]=new Purse(1);
			purses[1]=new Purse(1);
			purses[2]=new Purse(1);
			purses[3]=new Purse(3);
		
			p=new Pot(purses);
			
			assertTrue(p.getAllIn()==3);
		}
		catch(Exception e)
		{
			//should not get here
			fail();
		}
	}
	
	public void testGetPlayerBet()
	{
		//test method
		try{
			purses[0]=new Purse(12);
			purses[1]=new Purse(13);
			purses[2]=new Purse(14);
			purses[3]=new Purse(15);
		
			p=new Pot(purses);
			
			assertTrue(p.getPlayerBet(2)==14);
		}
		catch(Exception e)
		{
			//should not get here
			fail();
		}

		//test exception handling
		try{
			assertTrue(p.getPlayerBet(6)==14);
			fail();
		}
		catch(Exception e)
		{
			//should get here
		}
	}
	
	public void testSetPlayerBet()
	{
		//test the method
		try
		{
			p=new Pot(4);
			p.addPlayerBet(1, 15);
			
			assertTrue(p.getPlayerBet(1)==15);
		}
		catch(Exception e)
		{
			//should not get here
			fail();
		}
	}
	
	public void testAddPlayerBet()
	{
		//test the method
		try{
			purses[0]=new Purse(12);
			purses[1]=new Purse(13);
			purses[2]=new Purse(14);
			purses[3]=new Purse(15);
		
			p=new Pot(purses);
			
			p.addPlayerBet(1, 10);
			
			assertTrue(p.getPlayerBet(1)==23);
		}
		catch(Exception e)
		{
			//should not get here
			fail();
		}
		
		try{
			purses[0]=new Purse(12);
			purses[1]=new Purse(13);
			purses[2]=new Purse(14);
			purses[3]=new Purse(15);
		
			p=new Pot(purses);
			
			p.addPlayerBet(4, 10);
			fail();
		}
		catch(IllegalArgumentException e)
		{
			//should get here
		}
		catch(InvalidMoneyValueException e)
		{
			//should not get here
			fail();
		}
		
		try{
			purses[0]=new Purse(12);
			purses[1]=new Purse(13);
			purses[2]=new Purse(14);
			purses[3]=new Purse(15);
		
			p=new Pot(purses);
			
			p.addPlayerBet(1, -10);
			fail();
		}
		catch(IllegalArgumentException e)
		{
			//should not get here
			fail();
		}
		catch(InvalidMoneyValueException e)
		{
			//should get here
		}
	}
	
	public void testAllIn()
	{
		//first player goes all-in
	try{
			Pot pResult=p.allIn(0, 7);
			
			assertTrue(p.getPlayerBet(0)==7);
			assertTrue(p.getPlayerBet(1)==0);
			assertTrue(p.getPlayerBet(2)==0);
			assertTrue(p.getPlayerBet(3)==0);
			
			assertTrue(pResult.getPlayerBet(0)==0);
			assertTrue(pResult.getPlayerBet(1)==0);
			assertTrue(pResult.getPlayerBet(2)==0);
			assertTrue(pResult.getPlayerBet(3)==0);
		}
		catch(Exception e)
		{
			fail();
		}
		
		//Try a side pot from player 3 with one short bet and one tall
		try{
			p=new Pot(4);
			p.addPlayerBet(0, 3);
			p.addPlayerBet(1, 20);
			Pot pResult=p.allIn(2, 5);
			
			assertTrue(p.getPlayerBet(0)==3);
			assertTrue(p.getPlayerBet(1)==5);
			assertTrue(p.getPlayerBet(2)==5);
			assertTrue(p.getPlayerBet(3)==0);
			
			assertTrue(pResult.getPlayerBet(0)==0);
			assertTrue(pResult.getPlayerBet(1)==15);
			assertTrue(pResult.getPlayerBet(2)==0);
			assertTrue(pResult.getPlayerBet(3)==0);
		}
		catch(Exception e)
		{
			//should not reach here
			fail();
		}
		
		//Try a side pot from player 3 with two tall bets
		try{
			p=new Pot(4);
			p.addPlayerBet(0, 10);
			p.addPlayerBet(1, 20);
			Pot pResult=p.allIn(2, 5);
			
			assertTrue(p.getPlayerBet(0)==5);
			assertTrue(p.getPlayerBet(1)==5);
			assertTrue(p.getPlayerBet(2)==5);
			assertTrue(p.getPlayerBet(3)==0);
			
			assertTrue(pResult.getPlayerBet(0)==5);
			assertTrue(pResult.getPlayerBet(1)==15);
			assertTrue(pResult.getPlayerBet(2)==0);
			assertTrue(pResult.getPlayerBet(3)==0);
		}
		catch(Exception e)
		{
			//should not reach here
			fail();
		}
		
		// Last player (player 4) goes all-in with short stack
		try{
			p=new Pot(4);
			p.addPlayerBet(0, 22);
			p.addPlayerBet(1, 23);
			p.addPlayerBet(2, 24);
			Pot pResult=p.allIn(3, 10);
			
			assertTrue(p.getPlayerBet(0)==10);
			assertTrue(p.getPlayerBet(1)==10);
			assertTrue(p.getPlayerBet(2)==10);
			assertTrue(p.getPlayerBet(3)==10);
			
			assertTrue(pResult.getPlayerBet(0)==12);
			assertTrue(pResult.getPlayerBet(1)==13);
			assertTrue(pResult.getPlayerBet(2)==14);
			assertTrue(pResult.getPlayerBet(3)==0);
		}
		catch(Exception e)
		{
			//Should not get here
			fail();
		}
		
		//another case (last player goes all in over everyone else)
		try{
			p=new Pot(4);
			p.addPlayerBet(0, 3);
			p.addPlayerBet(1, 5);
			p.addPlayerBet(2, 5);
			Pot pResult=p.allIn(3, 7);
			
			assertTrue(p.getPlayerBet(0)==3);
			assertTrue(p.getPlayerBet(1)==5);
			assertTrue(p.getPlayerBet(2)==5);
			assertTrue(p.getPlayerBet(3)==7);
		
			assertTrue(pResult.getPlayerBet(0)==0);
			assertTrue(pResult.getPlayerBet(1)==0);
			assertTrue(pResult.getPlayerBet(2)==0);
			assertTrue(pResult.getPlayerBet(3)==0);
		}
		catch(Exception e)
		{
			fail();
		}
		
		//all players go equally
		try{
			p=new Pot(4);
			p.addPlayerBet(0, 30);
			p.addPlayerBet(1, 30);
			p.addPlayerBet(2, 30);
			Pot pResult=p.allIn(3, 30);
			
			assertTrue(p.getPlayerBet(0)==30);
			assertTrue(p.getPlayerBet(1)==30);
			assertTrue(p.getPlayerBet(2)==30);
			assertTrue(p.getPlayerBet(3)==30);
			
			assertTrue(pResult.getPlayerBet(0)==0);
			assertTrue(pResult.getPlayerBet(1)==0);
			assertTrue(pResult.getPlayerBet(2)==0);
			assertTrue(pResult.getPlayerBet(3)==0);
		}
		catch(Exception e)
		{
			//Should not reach here
			fail();
		}
	}
	
	public void testBubbleIn()
	{
		try{
			p.addPlayerBet(0, 0);
			p.addPlayerBet(1, 30);
			p.addPlayerBet(2, 30);
			p.addPlayerBet(3, 50);
			
			assertTrue(p.bubbleIn(0, 60)==10);
			assertTrue(p.bubbleIn(1, 40)==20);
			assertTrue(p.bubbleIn(2, 50)==30);
		}
		catch(Exception e)
		{
			//should not reach here
			fail();
		}
	}
}
