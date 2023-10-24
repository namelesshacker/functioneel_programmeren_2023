package util.test;

import static util.test.AllCards.a4C;
import static util.test.AllCards.a4H;
import static util.test.AllCards.a4S;
import static util.test.AllCards.a5C;
import static util.test.AllCards.a5D;
import static util.test.AllCards.a6H;
import static util.test.AllCards.a7D;
import static util.test.AllCards.a7H;
import static util.test.AllCards.a8H;
import static util.test.AllCards.a9H;
import static util.test.AllCards.aAC;
import static util.test.AllCards.aKC;
import static util.test.AllCards.aKD;

import java.util.Iterator;

import junit.framework.TestCase;
import util.Card;
import util.Hand;

public class TestHand extends TestCase 
{
	public void testAdd()
	{
		Hand lHand = new Hand();
		lHand.add( a5C );
		for( Card lCard : lHand )
		{
			assertEquals( lCard, a5C );
		}
		lHand.add( aKC );
		Iterator<Card> lI = lHand.iterator();
		assertEquals( lI.next(), a5C );
		assertEquals( lI.next(), aKC );
		lHand.add( a6H );
		lHand.add( a7H );
		assertFalse( lHand.isFull() );
		lHand.add( a8H );
		assertTrue( lHand.isFull() );
		try
		{
			lHand.add( a9H );
			fail();
		}
		catch( AssertionError pError )
		{
			// pass
		}
	}
	
	public void testSize()
	{
		Hand lHand = new Hand();
		assertEquals( 0, lHand.size() );
		lHand.add( a5C );
		assertEquals( 1, lHand.size() );
		lHand.add( a5D );
		assertEquals( 2, lHand.size() );
		lHand.add( a5C );
		assertEquals( 3, lHand.size() );
	}
	
	public void testRemove()
	{
		Hand lHand = new Hand();
		lHand.remove( a5C );
		Iterator<Card> lI = lHand.iterator();
		assertFalse( lI.hasNext() );
		lHand.add( a5C );
		lI = lHand.iterator();
		assertEquals( lI.next(), a5C );
		lHand.remove( a4C );
		lI = lHand.iterator();
		assertEquals( lI.next(), a5C );
		lHand.remove( a5C );
		lI = lHand.iterator();
		assertFalse( lI.hasNext() );
	}
	
	public void testSort()
	{
		Hand lHand = new Hand();
		Iterator<Card> lI = lHand.iterator();
		assertFalse( lI.hasNext() );
		lHand.add( aAC );
		lHand.add( a4H );
		lHand.add( aKD );
		lI = lHand.iterator();
		assertEquals( lI.next(), a4H );
		assertEquals( lI.next(), aKD );
		assertEquals( lI.next(), aAC );
		lHand.add( a7D );
		lHand.add( a5C );
		lI = lHand.iterator();
		assertEquals( lI.next(), a4H );
		assertEquals( lI.next(), a5C );
		assertEquals( lI.next(), a7D );
		assertEquals( lI.next(), aKD );
		assertEquals( lI.next(), aAC );
	}
	
	public void testClear()
	{
		Hand lHand = new Hand();
		lHand.clear();
		Iterator<Card> lI = lHand.iterator();
		assertFalse( lI.hasNext() );
		lHand.add( a4S );
		lHand.add( a7D );
		lHand.clear();
		lI = lHand.iterator();
		assertFalse( lI.hasNext() );
	}
	
	public void testGetHighCard()
	{
		Hand lHand = new Hand();
		lHand.add( aAC );
		try
		{
			lHand.getHighCard();
			fail();
		}
		catch( AssertionError pError )
		{
			// passed
		}
		lHand.add( a4H );
		lHand.add( aKD );
		lHand.add( a7D );
		lHand.add( a5C );
		assertEquals( lHand.getHighCard(), aAC );
	}
	
	public void testClone()
	{
		Hand lHand = new Hand();
		Hand lClone = lHand.clone();
		assertFalse( lHand == lClone );
		assertFalse( lHand == null );
		assertEquals( 0, lHand.size() );
		lHand.add( a4H );
		lHand.add( aKD );
		lHand.add( a7D );
		lHand.add( a5C );
		lClone = lHand.clone();
		assertFalse( lHand == lClone );
		assertFalse( lHand == null );
		assertEquals( 4, lHand.size() );
	}
}
