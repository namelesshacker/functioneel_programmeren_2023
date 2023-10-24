package util.test;

import java.util.HashSet;
import java.util.Set;

import util.Card;
import util.Deck;
import junit.framework.TestCase;

public class TestDeck extends TestCase 
{
	public void testdraw()
	{
		Deck lDeck = new Deck();
		lDeck.shuffle();
		Set<Card> lCards = new HashSet<Card>();
		for( int i = 0; i < 52; i++ )
		{
			Card lCard = lDeck.draw();
			assertFalse( lCards.contains(lCard));
			lCards.add( lCard );
			assertEquals( i, 51 - lDeck.size() );
		}
		assertEquals( 0, lDeck.size() );
		
		try
		{
			lDeck.draw();
			fail();
		}
		catch( AssertionError pError )
		{
			// Test passed!
		}
	}
}
