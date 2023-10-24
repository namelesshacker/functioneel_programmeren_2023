package ai.test;

import junit.framework.*;
import ai.util.AIHandValuator;
import util.*;
import scoring.*;

public class TestAIHandValuator extends TestCase {
	
	private AIHandValuator x;
	private Card y[]; 
	private Card c1, c2, c3, c4, c5;
	
	public void setUp()
	{
		x = new AIHandValuator();
		y = new Card[5];
	}
	
	public void testPairDetection_FAST()
	{
		// set up a generic hand
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);

		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		
		// add it to the valuator
		x.setConstantCards(y);
		
		// test lowest pair
		c5 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);
		
		// test nothing in between to make sure pair was forgotten
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
		
		// test low pair
		c5 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);
		
		// test nothing in between to make sure pair was forgotten
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
		
		// test high pair
		c5 = new Card(Card.Rank.Four, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);
		
		// test nothing in between to make sure pair was forgotten
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
		
		// test highest pair
		c5 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);
		
		// test nothing in between to make sure pair was forgotten
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
	}
	
	public void testPairDetection()
	{
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.PAIR);
		
		setUp();
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.PAIR);
		
		setUp();
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.PAIR);
		
		setUp();
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.PAIR);
	}
	
	public void testTwoPairDetection_FAST()
	{
		// set up a generic one pair hand
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Two, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.CLUBS);

		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		
		// add it to the valuator
		x.setConstantCards(y);
		
		// test lowest two pair
		c5 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.TWO_PAIRS);
		
		// test nothing in between to make sure pair is still there
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);
		
		// test high two pair
		c5 = new Card(Card.Rank.Five, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.TWO_PAIRS);
		
		// test nothing in between to make sure pair is still there
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);

	}
	
	public void testTwoPairDetection()
	{
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Three, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.TWO_PAIRS);
		
		setUp();
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Three, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);

		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.TWO_PAIRS);
		
		setUp();
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Three, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);

		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.TWO_PAIRS);
	}
	
	public void testFourOfAKindDetection_FAST()
	{
		// set up a generic four of a kind hand
		c1 = new Card(Card.Rank.Four, Card.Suit.HEARTS);
		c2 = new Card(Card.Rank.Four, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Four, Card.Suit.CLUBS);

		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		
		// add it to the valuator
		x.setConstantCards(y);
		
		// test lowest 4 of a kind
		c5 = new Card(Card.Rank.Five, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FOUR_OF_A_KIND);
		
		// test nothing in between to make sure four of a kind is still there
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FOUR_OF_A_KIND);
		
		// test high four of a kind
		c5 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FOUR_OF_A_KIND);
		
		// test nothing in between to make sure 4 is still there
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FOUR_OF_A_KIND);
		
		// now check that we can catch a four of a kind from a 3 of a kind
		setUp();
		
		// set up a generic three of a kind hand
		c1 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		c2 = new Card(Card.Rank.Four, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Four, Card.Suit.CLUBS);

		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		
		// add it to the valuator
		x.setConstantCards(y);
		
		// test detection of a four a kind
		c5 = new Card(Card.Rank.Four, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FOUR_OF_A_KIND);
		
		// and check that we had 3 of a kind before this
		c5 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.THREE_OF_A_KIND);
		
	}
	
	public void testFourOfAKindDetection()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Ace, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.FOUR_OF_A_KIND);
		
		setUp();
		
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Nine, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Nine, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.FOUR_OF_A_KIND);
	}
	
	public void testHighCardDetection_FAST()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		
		x.setConstantCards(y);
		
		// and check that we had 1 of a kind before this
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
		
		c5 = new Card(Card.Rank.King, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
	}
	
	public void testHighCardDetection()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.HIGH_CARD);
		
		setUp();
		
		c1 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.HIGH_CARD);
		
		setUp();
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.HIGH_CARD);
	}
	
	public void testFullHouseDetection_FAST()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Ten, Card.Suit.SPADES);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		x.setConstantCards(y);
		
		// nothing
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.TWO_PAIRS);
		
		c5 = new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FULL_HOUSE);
		
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.TWO_PAIRS);
		
		c5 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FULL_HOUSE);
	}
	
	public void testFullHouseDetection()
	{
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.FULL_HOUSE);
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Two, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		setUp();
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.FULL_HOUSE);
	}
	
	public void testThreeOfAKindDetection_FAST()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Nine, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Ten, Card.Suit.SPADES);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		x.setConstantCards(y);
		
		// nothing
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.TWO_PAIRS);
		
		c5 = new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.TWO_PAIRS);
		
		c5 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.THREE_OF_A_KIND);
		
		c5 = new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.TWO_PAIRS);
	}
	
	public void testThreeOfAKindDetection()
	{
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Four, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Two, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Two, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.THREE_OF_A_KIND);
		
		setUp();
		
		c1 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Four, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Four, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.THREE_OF_A_KIND);
		
		setUp();
		
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Four, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Four, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.THREE_OF_A_KIND);
	}
	
	public void testStraightDetection_FAST()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.King, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Ten, Card.Suit.SPADES);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		x.setConstantCards(y);
		
		// nothing
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
		
		c5 = new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);
		
		c5 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT);
		
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Five, Card.Suit.SPADES);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		x.setConstantCards(y);
		
		// nothing
		c5 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
		
		c5 = new Card(Card.Rank.Six, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT);
		
		c5 = new Card(Card.Rank.King, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.HIGH_CARD);
		
		c5 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT);
		
	}
	
	public void testStraightDetection()
	{
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Six, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Four, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.STRAIGHT);
		
		setUp();
		
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Four, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.STRAIGHT);
		
		setUp();
		
		c1 = new Card(Card.Rank.King, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.SPADES);
		c5 = new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.STRAIGHT);		
	}
	
	public void testFlushAndOrStraightDetection_FAST()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.King, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		x.setConstantCards(y);
		
		// nothing
		c5 = new Card(Card.Rank.Jack, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT);
		
		c5 = new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.PAIR);
		
		c5 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FLUSH);
		
		c5 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT_FLUSH);
		
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Two, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		x.setConstantCards(y);
		
		// nothing
		c5 = new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.FLUSH);
		
		c5 = new Card(Card.Rank.Six, Card.Suit.HEARTS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT);
		
		c5 = new Card(Card.Rank.Six, Card.Suit.CLUBS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT_FLUSH);
		
		c5 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT);
		
		c5 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		assertTrue(x.getHandValueCategory(c5) == HandValue.Category.STRAIGHT_FLUSH);
	}
	
	public void testFlushDetection()
	{
		c1 = new Card(Card.Rank.King, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Two, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.FLUSH);
	}
	
	public void testStraightFlushDetection()
	{
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Six, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		c5 = new Card(Card.Rank.Two, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.STRAIGHT_FLUSH);
		
		setUp();
		
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		c5 = new Card(Card.Rank.Two, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.STRAIGHT_FLUSH);
		
		setUp();
		
		c1 = new Card(Card.Rank.King, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		assertTrue(x.getHandValueCategory(y) == HandValue.Category.STRAIGHT_FLUSH);
	}
	
	public void testSpeedOfThisGreasyMachine_FAST()
	{
		final int LONGEST_ALLOWABLE_TIME = 100;		// 0.1 secs for 250000 calls to
		
		c1 = new Card(Card.Rank.King, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = null;
		
		x.setConstantCards(y);
		
		c5 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		
		long timeMillis = System.currentTimeMillis();
		
		for(int j = 0; j < 249994; j+=47)
		{
			x.setConstantCards(y);
			
			for(int i = 0; i < 47; i++)
				x.getHandValueCategory(c5);
		}
		
		timeMillis = (System.currentTimeMillis() - timeMillis); 
		
		System.out.println(timeMillis);
		
		assertTrue(timeMillis < LONGEST_ALLOWABLE_TIME);
	}
	
	public void testSpeedOfThisGreasyMachine()
	{
		final int LONGEST_ALLOWABLE_TIME = 500;		// .3 secs for 250000 calls to	
		
		c1 = new Card(Card.Rank.King, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		
		y[0] = c1; y[1] = c2; y[2] = c3; y[3]= c4; y[4] = c5;
		
		long timeMillis = System.currentTimeMillis();
		
		for(int i = 0; i < 249994; i++)
			x.getHandValueCategory(y);
		
		timeMillis = (System.currentTimeMillis() - timeMillis); 
		
		System.out.println(timeMillis);
		
		assertTrue(timeMillis < LONGEST_ALLOWABLE_TIME);
		
	}
}
