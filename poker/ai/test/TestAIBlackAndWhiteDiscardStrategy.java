package ai.test;

import scoring.HandValue;
import util.Card;
import util.Hand;
import ai.discards.AIBlackAndWhiteDiscardStrategy;
import ai.util.DiscardProbability;
import junit.framework.TestCase;

public class TestAIBlackAndWhiteDiscardStrategy extends TestCase {

	DiscardProbability best; 
	AIBlackAndWhiteDiscardStrategy x; 
	Hand z; 
	HandValue y; 
	
	Card c1, c2, c3, c4, c5; 
	
	public void setUp()
	{
		x = new AIBlackAndWhiteDiscardStrategy();
		z = new Hand();
	}
	
	public void testDetectionOfBadHandsForBetting()
	{
		
		// This hand will never yield anything good
		z  = new Hand();
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Nine, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Jack, Card.Suit.HEARTS);
		c4 = new Card(Card.Rank.Seven, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z); 
		
		// A noob using AISimpleDiscard should just discard the lowest three and hope.
		// He should not know that this is not optimal. 
		assertTrue(best.getProbability() != x.badProbability());
		assertTrue(best.getCards().length == 3);
		assertTrue(best.getCards()[0] == c1 );
		assertTrue(best.getCards()[1] == c4 );
		assertTrue(best.getCards()[2] == c2 );
	}
	
	public void testDetectionOfGoodHands()
	{
		// This FULL HOUSE should not be exchanged
		z  = new Hand();
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Three, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Jack, Card.Suit.HEARTS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We expect a null to inform us
		assertTrue(best.getCards()[0] == null);
		
		// This FOUR_OF_A_KIND is exchanged by BLACK and WHITE!!!!! 
		z  = new Hand();
		c1 = new Card(Card.Rank.Three, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Jack, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Jack, Card.Suit.HEARTS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We expect a null to inform us
		assert(best.getCards()[0] == c1);
		
	}
	
	public void testDetectionOfSingleDiscards()
	{
		
		// This NOTHING should try for a STRAIGHT
		z  = new Hand();
		c1 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Nine, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Eight, Card.Suit.HEARTS);
		c4 = new Card(Card.Rank.Seven, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Ace, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We expect the ace of clubs with fairly good hope
		assertTrue(best.getCards().length == 3);
		assertTrue(best.getCards()[0] == c4);
		assertTrue(best.getCards()[1] == c3);
		assertTrue(best.getCards()[2] == c2);
		assertTrue(best.getProbability() != x.badProbability());
		
		// This TWO PAIRS needs to be upgraded with a single card exchange
		z  = new Hand();
		c1 = new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Ten, Card.Suit.SPADES);
		c3 = new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Jack, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We expect the queen of clubs
		assertTrue(best.getCards().length == 3);
		assertTrue(best.getCards()[0] == c3);
		assertTrue(best.getCards()[1] == null);
		assertTrue(best.getCards()[2] == null);
		assertTrue(best.getProbability() != x.badProbability());
		
		// This OPEN ENDED STRAIGHT chance should not be wasted in a discard
		z  = new Hand();
		c1 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Five, Card.Suit.CLUBS);
		c3 = new Card(Card.Rank.Six, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Seven, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Seven, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We expect the seven of diamonds (a pair is worthless, right?)
		assertTrue(best.getCards().length == 3);
		assertTrue(best.getCards()[0] == c1);
		assertTrue(best.getCards()[1] == null);
		assertTrue(best.getCards()[2] == null);
		assertTrue(best.getProbability() != x.badProbability()); 
	}
	
	public void testDetectionOfTripleDiscards()
	{
		// This PAIR should be kept, the rest thrown away
		z  = new Hand();
		c1 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Queen, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Six, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Seven, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Seven, Card.Suit.CLUBS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We want to keep the pair with confidence
		assertTrue(best.getCards().length == 3);
		assertTrue(best.getCards()[0] == c1);
		assertTrue(best.getCards()[1] == null);
		assertTrue(best.getCards()[2] == null);
		assertTrue(best.getProbability() != x.badProbability()); 
	}
	
	public void testDetectionOfDoubleDiscards()
	{
		// This PAIR should be kept, the rest thrown away, even for simple
		z  = new Hand();
		c1 = new Card(Card.Rank.Four, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Queen, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Seven, Card.Suit.CLUBS);
		c4 = new Card(Card.Rank.Seven, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Seven, Card.Suit.HEARTS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We want to keep the royal pair with confidence
		assertTrue(best.getCards().length == 3);
		assertTrue(best.getCards()[0] == c1);
		assertTrue(best.getCards()[1] == null);
		assertTrue(best.getCards()[2] == null);
		assertTrue(best.getProbability() != x.badProbability());
		
		// Standard thinks the flush is better in this case,
		// Noob thinks the top two give him better odds
		z  = new Hand();
		c1 = new Card(Card.Rank.Two, Card.Suit.CLUBS);
		c2 = new Card(Card.Rank.Nine, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Six, Card.Suit.HEARTS);
		c4 = new Card(Card.Rank.Eight, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Seven, Card.Suit.SPADES);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		// We want to keep the royal pair with confidence
		assertTrue(best.getCards().length == 3);
		assertTrue(best.getCards()[0] == c1);
		assertTrue(best.getCards()[1] == c3);
		assertTrue(best.getCards()[2] == c5);
		assertTrue(best.getProbability() != x.badProbability());
		
		// We asked someone about this in real life. 
		// They said "discard the Queen" ... 
		// We found how their brain works (see BlackAndWhiteDiscardStrategy code) 
		z = new Hand(); 
		c1 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.King, Card.Suit.DIAMONDS); 
		c5 = new Card(Card.Rank.Ace, Card.Suit.DIAMONDS);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		assertTrue(best.getCards()[0] == c3);
		
		z = new Hand(); 
		c1 = new Card(Card.Rank.Five, Card.Suit.DIAMONDS);
		c2 = new Card(Card.Rank.Five, Card.Suit.HEARTS);
		c3 = new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		c4 = new Card(Card.Rank.King, Card.Suit.CLUBS); 
		c5 = new Card(Card.Rank.Ace, Card.Suit.SPADES);
		
		z.add(c1);
		z.add(c2);
		z.add(c3);
		z.add(c4);
		z.add(c5);
		
		best = x.bestDiscard(z);
		
		assertTrue(best.getCards()[0] == c3);
		
	} 
	
}
