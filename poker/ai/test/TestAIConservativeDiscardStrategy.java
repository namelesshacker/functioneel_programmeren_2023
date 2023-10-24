package ai.test;

import util.*;
import ai.discards.AIConservativeDiscardStrategy;
import ai.util.DiscardProbability;
import junit.framework.TestCase;

public class TestAIConservativeDiscardStrategy extends TestCase {
	DiscardProbability result; 
	AIConservativeDiscardStrategy x; 
	Hand z;
	Card[] resultCards;
	
	Card c1, c2, c3, c4, c5; 
	
	public void setUp()
	{
		x = new AIConservativeDiscardStrategy();
		z = new Hand();
		resultCards=new Card[3];
	}

	/**
	 * Test if the conservative player keeps the pair of Jacks and ignores more
	 * tantalizing options (flush, straight, royal straight flush).
	 *
	 */
	public void testDetectionOfSingleDiscardsAsBestOption1()
	{
		// Conservative: keep the current hand (2 jacks)
		// Aggressive: try for the straight, straight flush, flush, etc.
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
		
		// the result should be equal to this array (keep the Jacks)
		resultCards[0]=new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		resultCards[1]=new Card(Card.Rank.King, Card.Suit.DIAMONDS);
		resultCards[2]=new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		
		result = x.bestDiscard(z);
		
		// discarded cards should be Queen, King, Ace of Diamonds
		assertTrue(equalCardArrays(resultCards, result.getCards()));
		// should not be a bad probability
		assertTrue(x.badProbability() != result.getProbability());
	}
	
	/**
	 * Make sure the conservative player still takes the pair (and not the flush,
	 * the open-ended straight draw, or the open-ended straight flush draw)
	 *
	 */
	public void testDetectionOfSingleDiscardsAsBestOption2()
	{	
		//keep pair of 7s
		z  = new Hand();
		c1 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c3 = new Card(Card.Rank.Six, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Seven, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Seven, Card.Suit.HEARTS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
				
		// the result should be equal to this array (keep the 7s)
		resultCards[0]=new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		resultCards[1]=new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		resultCards[2]=new Card(Card.Rank.Six, Card.Suit.DIAMONDS);
		
		result = x.bestDiscard(z); 
	
		// discarded cards should be 4,5,6 of Diamonds
		assertTrue(equalCardArrays(resultCards, result.getCards()));
		// should not be a bad probability
		assertTrue(x.badProbability() != result.getProbability());
	}

	/**
	 * In this case, the player should keep the two highest, running diamonds (5,6)
	 * and throw away the other three cards (to hope for a pair or better).  However,
	 * all discards are very bad, so it should be Bad Probability.
	 *
	 */
	public void testDetectionOfSingleDiscardsAsBestOption3()
	{	
		z  = new Hand();
		c1 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c3 = new Card(Card.Rank.Six, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.King, Card.Suit.HEARTS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
				
		// the result should be equal to this array (keep the two high cards)
		resultCards[0]=new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		resultCards[1]=new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		resultCards[2]=new Card(Card.Rank.Six, Card.Suit.DIAMONDS);
		
		result = x.bestDiscard(z);
		
		// discarded cards should be 4,5,6 of Diamonds
		assertTrue(equalCardArrays(resultCards, result.getCards()));
		// should be a bad probability
		assertTrue(x.badProbability() == result.getProbability());
	}

	/**
	 * Don't be too conservative (should not be content with two pair, should go for full house)
	 *
	 */
	public void testDetectionOfSingleDiscardsAsBestOption4()
	{	
		//keep pair of 7s
		z  = new Hand();
		c1 = new Card(Card.Rank.Four, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Four, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.Six, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Six, Card.Suit.HEARTS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
				
		// the result should be equal to this array (keep the 7s)
		resultCards[0]=new Card(Card.Rank.Five, Card.Suit.DIAMONDS);

		result = x.bestDiscard(z);
		
		// discarded cards should be 4,5,6 of Diamonds
		assertTrue(equalCardArrays(resultCards, result.getCards()));
		// should be a bad probability
		assertTrue(x.badProbability() != result.getProbability());
	}
	
	/**
	 * Don't change the hand!
	 *
	 */
	public void testDetectionOfSingleDiscardsAsBestOption5()
	{
		c1 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Ace, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Ace, Card.Suit.SPADES); 
		c5 = new Card(Card.Rank.Two, Card.Suit.HEARTS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		
		result = x.bestDiscard(z);
		
		// discarded cards should be no discard
		assertTrue(result.getCards()[0] == null);
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
