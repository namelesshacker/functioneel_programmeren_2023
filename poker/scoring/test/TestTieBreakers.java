package scoring.test;

import junit.framework.TestCase;
import scoring.HandValuator;
import scoring.HandValue;
import util.Card;
import util.Hand;
import util.Card.Rank;
import util.Card.Suit;

public class TestTieBreakers extends TestCase 
{	
	protected Hand myHand, yourHand;
	protected HandValue y, z;
	protected HandValuator x;
	
	protected void setUp() throws Exception 
	{
		myHand = new Hand();
		yourHand = new Hand();
		x = new HandValuator();
	}
	
	// Test some particular aspect of the tie breaking (specify which)
	public void testStraightFlush()
	{
		
		/* Royal Flush Tie Breakers */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.King, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Jack, Suit.HEARTS));
		myHand.add(new Card(Rank.Ten, Suit.HEARTS));
		
		y = x.valuateHand(myHand);
		
		yourHand.add(new Card(Rank.Ace, Suit.SPADES));
		yourHand.add(new Card(Rank.King, Suit.SPADES));
		yourHand.add(new Card(Rank.Queen, Suit.SPADES));
		yourHand.add(new Card(Rank.Jack, Suit.SPADES));
		yourHand.add(new Card(Rank.Ten, Suit.SPADES));
		
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) == 0);
	
		yourHand.clear();
		yourHand.add(new Card(Rank.Nine, Suit.SPADES));
		yourHand.add(new Card(Rank.King, Suit.SPADES));
		yourHand.add(new Card(Rank.Queen, Suit.SPADES));
		yourHand.add(new Card(Rank.Jack, Suit.SPADES));
		yourHand.add(new Card(Rank.Ten, Suit.SPADES));
		
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) > 0);
	}
	
	public void testFourOfAKind()
	{
		
		/* Four of a Kind */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Ace, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Ace, Suit.SPADES));
		myHand.add(new Card(Rank.Six, Suit.SPADES));
		y = x.valuateHand(myHand);

		yourHand.add(new Card(Rank.Six, Suit.SPADES));
		yourHand.add(new Card(Rank.Six, Suit.HEARTS));
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Six, Suit.DIAMONDS));
		yourHand.add(new Card(Rank.Ace, Suit.SPADES));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) > 0);
	}
	
	public void testFullHouse()
	{
	
		/* Full House */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Ace, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Six, Suit.CLUBS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));
		y = x.valuateHand(myHand);

		yourHand.add(new Card(Rank.Six, Suit.SPADES));
		yourHand.add(new Card(Rank.Six, Suit.HEARTS));
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Ace, Suit.DIAMONDS));
		yourHand.add(new Card(Rank.Ace, Suit.SPADES));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) > 0);
	}
	
	public void testFlush()
	{
	
		/* Flush */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		y = x.valuateHand(myHand);
		
		yourHand.add(new Card(Rank.Ace, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.CLUBS));
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Queen, Suit.CLUBS));
		yourHand.add(new Card(Rank.Two, Suit.CLUBS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) == 0);
	
		myHand.clear();
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.CLUBS));
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Queen, Suit.CLUBS));
		yourHand.add(new Card(Rank.Two, Suit.CLUBS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) > 0);
	}
	
	public void testStraight()
	{
		
		/* Straight */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.add(new Card(Rank.Ace, Suit.CLUBS));
		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.Jack, Suit.CLUBS));
		yourHand.add(new Card(Rank.Queen, Suit.CLUBS));
		yourHand.add(new Card(Rank.Ten, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) < 0);
	
		myHand.clear();
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Two, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) < 0);

		myHand.clear();
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Two, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) == 0);
	}
		
	public void testThreeOfAKind()
	{
		/* Three of a kind*/
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Ace, Suit.SPADES));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Six, Suit.DIAMONDS));
		yourHand.add(new Card(Rank.Four, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Six, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) > 0);
	}
	
	public void testTwoPairs()
	{

		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.SPADES));
		y = x.valuateHand(myHand);

		yourHand.add(new Card(Rank.Ace, Suit.CLUBS));
		yourHand.add(new Card(Rank.Ace, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) < 0);
		
		myHand.clear();
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.Ace, Suit.CLUBS));
		yourHand.add(new Card(Rank.Ace, Suit.CLUBS));
		yourHand.add(new Card(Rank.Two, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) == 0);
	}
	
	public void testPair()
	{

		myHand.add(new Card(Rank.Ace, Suit.SPADES));
		myHand.add(new Card(Rank.King, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Two, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.Ace, Suit.CLUBS));
		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.SPADES));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) < 0);
		
		myHand.clear();
		myHand.add(new Card(Rank.King, Suit.SPADES));
		myHand.add(new Card(Rank.King, Suit.CLUBS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Two, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.SPADES));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) < 0);
		
		myHand.clear();
		myHand.add(new Card(Rank.King, Suit.SPADES));
		myHand.add(new Card(Rank.King, Suit.CLUBS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Two, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.SPADES));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) > 0);
	}
	
	public void testHighCard()
	{

		myHand.add(new Card(Rank.King, Suit.SPADES));
		myHand.add(new Card(Rank.Six, Suit.CLUBS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Two, Suit.SPADES));
		y = x.valuateHand(myHand);

		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.Six, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.SPADES));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) < 0);
		
		myHand.clear();
		myHand.add(new Card(Rank.King, Suit.SPADES));
		myHand.add(new Card(Rank.Three, Suit.CLUBS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Two, Suit.SPADES));
		y = x.valuateHand(myHand);
		
		yourHand.clear();
		yourHand.add(new Card(Rank.King, Suit.CLUBS));
		yourHand.add(new Card(Rank.Two, Suit.CLUBS));
		yourHand.add(new Card(Rank.Five, Suit.SPADES));
		yourHand.add(new Card(Rank.Three, Suit.CLUBS));
		yourHand.add(new Card(Rank.Four, Suit.HEARTS));
		z = x.valuateHand(yourHand);
		
		assertTrue(y.compareTo(z) == 0);
	
	}
	
	public void testExceptions()
	{
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.King, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Jack, Suit.HEARTS));
		myHand.add(new Card(Rank.Ten, Suit.HEARTS));
		
		y = x.valuateHand(myHand);
		
		try{
			y.getCard(6);
			fail("WRONG");
		} catch (ArrayIndexOutOfBoundsException e)
		{
			// Success for schlangen
		}
	}
}
