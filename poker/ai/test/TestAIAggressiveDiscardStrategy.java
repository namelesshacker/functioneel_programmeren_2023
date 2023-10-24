package ai.test;

import util.*;
import ai.discards.AIAggressiveDiscardStrategy;
import ai.util.DiscardProbability;
import junit.framework.TestCase;

public class TestAIAggressiveDiscardStrategy extends TestCase {
	DiscardProbability result; 
	AIAggressiveDiscardStrategy x; 
	Hand z;
	Card[] resultCards;
	
	Card c1, c2, c3, c4, c5; 
	
	public void setUp()
	{
		x = new AIAggressiveDiscardStrategy();
		z = new Hand();
		resultCards=new Card[3];
	}
	
	/**
	 * In this case, the aggressive player throws away his made hand (Jacks) for an attempt at high
	 * reward hands: straight, flush, straight lfush, royal straight flush.
	 *
	 */
	public void testDetectionOfSingleDiscardsAsBestOption1()
	{
		// Throw the Jack of Hearts and try for ridiculous hands
		c1 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		c3 = new Card(Card.Rank.King, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Jack, Card.Suit.HEARTS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		// Discarded cards should be the Jack of hearts
		resultCards[0]=new Card(Card.Rank.Jack, Card.Suit.HEARTS);
		
		result = x.bestDiscard(z); 
		
		// discarded cards should be Jack of hearts
		assertTrue(equalCardArrays(resultCards, result.getCards()));
		// should not be a bad probability
		assertTrue(x.badProbability() != result.getProbability());
	}
	
	/**
	 * Checks if the AI is not too aggressive (has a flush, shouldn't try for royal straight flush)
	 */
	public void testDetectionOfSingleDiscardsAsBestOption2()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		c3 = new Card(Card.Rank.King, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Nine, Card.Suit.DIAMONDS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		result = x.bestDiscard(z); 
		
		// discarded cards should be null (keep the FLUSH!)
		assertTrue(result.getCards()[0] == null);
	}
	
	/**
	 * Checks if the AI is not too aggressive (has a straight, shouldn't go for the flush/straight flush
	 */
	public void testDetectionOfSingleDiscardsAsBestOption3()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		c3 = new Card(Card.Rank.King, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		result = x.bestDiscard(z); 
		
		// discarded cards should be null (keep the Straight!)
		assertTrue(result.getCards()[0] == null);
	}
	
	/**
	 * Checks if the AI is not too aggressive (has a straight, shouldn't go for the flush/straight flush
	 */
	public void testDetectionOfSingleDiscardsAsBestOption4()
	{
		c1 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Ten, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		result = x.bestDiscard(z); 
		
		// Discarded cards should be the Jack of Diamonds
		resultCards[0]=new Card(Card.Rank.Jack, Card.Suit.DIAMONDS);
		
		// discarded cards should Jack (try for full house!)
		assertTrue(equalCardArrays(resultCards, result.getCards()));
		// should not be a bad probability
		assertTrue(x.badProbability() != result.getProbability());
	}

	/**
	 * Keeps the pair (is not stupid)
	 */
	public void testDetectionOfSingleDiscardsAsBestOption5()
	{
		c1 = new Card(Card.Rank.Ten, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Ten, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Six, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Seven, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		result = x.bestDiscard(z); 
		
		// Discarded cards should be the 6,7,8 of Diamonds (keep the pair!)
		resultCards[0]=new Card(Card.Rank.Six, Card.Suit.DIAMONDS);
		resultCards[1]=new Card(Card.Rank.Seven, Card.Suit.DIAMONDS);
		resultCards[2]=new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		
		// should keep the pair
		assertTrue(equalCardArrays(resultCards, result.getCards()));
		// should not be a bad probability
		assertTrue(x.badProbability() != result.getProbability());
	}
	
	
	/**
	 * Checks if the two card arrays are equal (helper for the other methods)
	 * 
	 * @param c1
	 * @param c2
	 * @return
	 */
	private boolean equalCardArrays(Card[] c1, Card[] c2)
	{
		if(c1.length!=c2.length)
			return false;
		for(int i=0; i<c1.length; i++)
			if((c1[i]!= null && c2[i] != null) &&
			   (c1[i].getSuit() != c2[i].getSuit() || 
			   c1[i].getRank() != c2[i].getRank()))
				return false;
		return true;
	}
}
