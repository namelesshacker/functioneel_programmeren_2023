package util.test;

import static util.test.AllCards.*;
import util.Card;
import junit.framework.TestCase;

public class TestCard extends TestCase 
{
	public void testToString()
	{
		assertEquals( "ACE of CLUBS", aAC.toString());
		assertEquals( "THREE of CLUBS", a3C.toString());
		assertEquals( "TEN of CLUBS", aTC.toString());
		assertEquals( "JACK of CLUBS", aJC.toString());
		assertEquals( "QUEEN of HEARTS", aQH.toString());
		assertEquals( "KING of SPADES", aKS.toString());
		assertEquals( "QUEEN of DIAMONDS", aQD.toString());
	}
	
	public void testCompareTo()
	{
		assertTrue( aAC.compareTo(a2C) > 0);
		assertTrue( a2C.compareTo(a2C) == 0);
		assertTrue( a2C.compareTo(aAC) < 0);
		assertTrue( a2C.compareTo(a2H) == 0);
		assertTrue( a3C.compareTo(a3H) == 0);
	}
	
	public void testGetSuit()
	{
		assertEquals( Card.Suit.CLUBS, aAC.getSuit() );
		assertEquals( Card.Suit.DIAMONDS, aAD.getSuit() );
		assertEquals( Card.Suit.HEARTS, aAH.getSuit() );
		assertEquals( Card.Suit.SPADES, aAS.getSuit() );
	}
}
