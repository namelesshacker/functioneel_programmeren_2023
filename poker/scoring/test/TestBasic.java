package scoring.test;

import junit.framework.TestCase;
import scoring.HandValuator;
import scoring.HandValue;
import util.Card;
import util.Hand;
import util.Card.Rank;
import util.Card.Suit;

public class TestBasic extends TestCase 
{
	private Hand myHand;
	private HandValuator x;
	
	@Override
	protected void setUp() throws Exception 
	{
		myHand = new Hand();
		x = new HandValuator();
	}
	
	// Test some particular aspect (specify what it is)
	public void testStraightFlush()
	{
		
		/* Royal Flush */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.King, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Jack, Suit.HEARTS));
		myHand.add(new Card(Rank.Ten, Suit.HEARTS));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.STRAIGHT_FLUSH
		);
		
		/* Normal Straight Flush */
		myHand.clear();
		myHand.add(new Card(Rank.Nine, Suit.HEARTS));
		myHand.add(new Card(Rank.King, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Jack, Suit.HEARTS));
		myHand.add(new Card(Rank.Ten, Suit.HEARTS));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.STRAIGHT_FLUSH
		);
		
		/* Low Straight Flush */
		myHand.clear(); 
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		
		assertTrue(
				x.valuateHand(myHand).getCategory() 
			== 	HandValue.Category.STRAIGHT_FLUSH);
	}
	
	public void testFourOfAKind()
	{
		/* Four of a Kind */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Ace, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Ace, Suit.SPADES));
		myHand.add(new Card(Rank.Six, Suit.SPADES));
		
		assertTrue(
				x.valuateHand(myHand).getCategory() 
			== 	HandValue.Category.FOUR_OF_A_KIND);
		myHand.clear();
		myHand.add(new Card(Rank.Six, Suit.SPADES));
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.CLUBS));
		myHand.add(new Card(Rank.Six, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Ace, Suit.SPADES));
		
		assertTrue(
				x.valuateHand(myHand).getCategory() 
			== 	HandValue.Category.FOUR_OF_A_KIND);
	}	
	
	public void testFullHouse()
	{
		/* Higher Full House */
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Ace, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));
		
		assertTrue(
				x.valuateHand(myHand).getCategory() 
			== 	HandValue.Category.FULL_HOUSE);

		/* Lower Full House */
		myHand.clear();
		myHand.add(new Card(Rank.Six, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Ace, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));
		
		assertTrue(
				x.valuateHand(myHand).getCategory() 
			== 	HandValue.Category.FULL_HOUSE);
		
	}
	
	public void testFlush()
	{
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.HEARTS));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.FLUSH
		);
	}

	public void testStraight()
	{
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.King, Suit.CLUBS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Jack, Suit.HEARTS));
		myHand.add(new Card(Rank.Ten, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.STRAIGHT
		);

		myHand.clear();
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.STRAIGHT
		);

		myHand.clear();
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Seven, Suit.CLUBS));
		myHand.add(new Card(Rank.Eight, Suit.HEARTS));
		myHand.add(new Card(Rank.Nine, Suit.HEARTS));
		myHand.add(new Card(Rank.Ten, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.STRAIGHT
		);
	}
	
	public void testThreeOfAKind()
	{
		
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.CLUBS));
		myHand.add(new Card(Rank.Five, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.THREE_OF_A_KIND
		);

		myHand.clear();
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.CLUBS));
		myHand.add(new Card(Rank.Ace, Suit.SPADES));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.THREE_OF_A_KIND
		);

		myHand.clear();
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Three, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.DIAMONDS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.THREE_OF_A_KIND
		);
	}
	
	public void testTwoPairs()
	{
		
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Ace, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.TWO_PAIRS
		);

		myHand.clear();
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.CLUBS));
		myHand.add(new Card(Rank.Six, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Queen, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.TWO_PAIRS
		);

		myHand.clear();
		myHand.add(new Card(Rank.Queen, Suit.HEARTS));
		myHand.add(new Card(Rank.Jack, Suit.CLUBS));
		myHand.add(new Card(Rank.Jack, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.TWO_PAIRS
		);
	}
	
	public void testPair()
	{

		myHand.add(new Card(Rank.King, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.King, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.PAIR
		);

		myHand.clear();
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add (new Card(Rank.Five, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.PAIR
		);

		myHand.clear();
		myHand.add(new Card(Rank.Five, Suit.HEARTS));
		myHand.add (new Card(Rank.Four, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.PAIR
		);		

		myHand.clear();
		myHand.add(new Card(Rank.Two, Suit.HEARTS));
		myHand.add(new Card(Rank.Two, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));

		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.PAIR
		);
	}
	
	public void testHighCard()
	{
		myHand.add(new Card(Rank.Ace, Suit.HEARTS));
		myHand.add(new Card(Rank.Five, Suit.CLUBS));
		myHand.add(new Card(Rank.Three, Suit.HEARTS));
		myHand.add(new Card(Rank.Four, Suit.HEARTS));
		myHand.add(new Card(Rank.Six, Suit.SPADES));
		
		assertTrue(
		x.valuateHand(myHand).getCategory()
		== HandValue.Category.HIGH_CARD
		);
	}
}
