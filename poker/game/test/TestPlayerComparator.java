package game.test;

import players.PrototypePlayer;
import junit.framework.TestCase;
import game.*;
import util.*;

public class TestPlayerComparator extends TestCase {
	
	PrototypePlayer p1, p2, p3;
	int p1Purse = 100;
	PlayerComparator pc;
	
	public void setUp()
	{
		try
		{
			p1 = new PrototypePlayer("Le Chiffre", p1Purse);
			p2 = new PrototypePlayer("James Bond", p1Purse);
			p3 = new PrototypePlayer("The guy from Lucky You", p1Purse);
			pc = new PlayerComparator();
			pc.addPlayer(p1);
			pc.addPlayer(p2);
		}
		catch (Exception e)
		{
			fail();
		}
	}
	
	public void testIsPlayerValid()
	{
		try
		{
			p1.getPurse().addChips(p1Purse);
			assertFalse(pc.isPlayerValid(p1));
			assertTrue(pc.isPlayerValid(p2));
			
			Hand x = new Hand();
			Card y = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
			x.add(y);
			
			assertFalse(pc.isPlayerValid(p1));
			assertTrue(pc.isPlayerValid(p2));	
			
			
		}
		catch (Exception e)
		{
			fail();
		}
	}
	
	public void testIsPlayerValidWithUpdates()
	{
		p1.getPurse().addChips(p1Purse);
		pc.updatePlayer(p1);
		assertTrue(pc.isPlayerValid(p1));		
		p1.getHand().add(new Card(Card.Rank.Ace, Card.Suit.CLUBS));
		p1.getHand().add(new Card(Card.Rank.Ace, Card.Suit.HEARTS));		
		assertFalse(pc.isPlayerValid(p1));
		pc.updatePlayer(p1);
		assertTrue(pc.isPlayerValid(p1));		
	}
	
}
