package ai.test;

import junit.framework.TestCase;
import ai.util.AIScoreRevealStrategy;
import util.*;
import game.actions.*;
import scoring.*;

public class TestAIScoreRevealStrategy extends TestCase {

	private AIScoreRevealStrategy s;
	private Hand h1, h2, h3, h4;
	private Card c1, c2, c3, c4, c5;
	private HandValue v;
	
	public void setUp()
	{
		h1=new Hand();
		h2=new Hand();
		h3=new Hand();
		h4=new Hand();
	
		s=new AIScoreRevealStrategy();
	}
	
	/**
	 * If first player, always show
	 *
	 */
	public void testFirstPlayer()
	{
		c1=new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h1.add(c1);
		h1.add(c2);
		h1.add(c3);
		h1.add(c4);
		h1.add(c5);
		v = new HandValuator().valuateHand(h1);
		assertTrue(s.makeScoreRevealAction(v) == ScoreRevealAction.scoreRevealAction.SHOW);
	}
	
	/**
	 * If second player, show only if winning against every hand on the board
	 *
	 */
	public void testSecondPlayer()
	{
		/******First Case******/
		
		// Opponent's hand (straight)
		c1=new Card(Card.Rank.Eight, Card.Suit.HEARTS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h1.add(c1);
		h1.add(c2);
		h1.add(c3);
		h1.add(c4);
		h1.add(c5);
		
		s.notify(new ScoreRevealAction("Gen", ScoreRevealAction.scoreRevealAction.SHOW, h1));
		
		//Your hand (straight flush)
		c1=new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h2.add(c1);
		h2.add(c2);
		h2.add(c3);
		h2.add(c4);
		h2.add(c5);
		v = new HandValuator().valuateHand(h2);
		// Should show as you are winning
		assertTrue(s.makeScoreRevealAction(v) == ScoreRevealAction.scoreRevealAction.SHOW);
		
		/*****Second Case******/
	
		s=new AIScoreRevealStrategy();
		
		// Opponent's hand (straight)
		c1=new Card(Card.Rank.Eight, Card.Suit.HEARTS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h1 = new Hand();
		
		h1.add(c1);
		h1.add(c2);
		h1.add(c3);
		h1.add(c4);
		h1.add(c5);
		
		s.notify(new ScoreRevealAction("Gen", ScoreRevealAction.scoreRevealAction.FOLD, h1));
		
		//Your hand (straight flush)
		c1=new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h2 = new Hand();
		
		h2.add(c1);
		h2.add(c2);
		h2.add(c3);
		h2.add(c4);
		h2.add(c5);
		v = new HandValuator().valuateHand(h2);
		// Should show as opponent folded
		assertTrue(s.makeScoreRevealAction(v) == ScoreRevealAction.scoreRevealAction.SHOW);
		
		/*****Third Case******/
		
		s=new AIScoreRevealStrategy();
		
		// Opponent's hand (straight flush)
		c1=new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h1 = new Hand();
		
		h1.add(c1);
		h1.add(c2);
		h1.add(c3);
		h1.add(c4);
		h1.add(c5);
		
		s.notify(new ScoreRevealAction("Gen", ScoreRevealAction.scoreRevealAction.SHOW, h1));
		
		//Your hand (straight)
		c1=new Card(Card.Rank.Eight, Card.Suit.HEARTS);
		c2=new Card(Card.Rank.Nine, Card.Suit.SPADES);
		c3=new Card(Card.Rank.Ten, Card.Suit.SPADES);
		c4=new Card(Card.Rank.Jack, Card.Suit.SPADES);
		c5=new Card(Card.Rank.Queen, Card.Suit.SPADES);
		
		h2 = new Hand();
		
		h2.add(c1);
		h2.add(c2);
		h2.add(c3);
		h2.add(c4);
		h2.add(c5);
		
		v = new HandValuator().valuateHand(h2);
		
		// Should fold as your opponent is winning now
		assertTrue(s.makeScoreRevealAction(v) == ScoreRevealAction.scoreRevealAction.FOLD);
		
		/*****Fourth Case******/
		
		s=new AIScoreRevealStrategy();
		
		// Opponent's hand (straight flush)
		c1=new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h1 = new Hand();
		
		h1.add(c1);
		h1.add(c2);
		h1.add(c3);
		h1.add(c4);
		h1.add(c5);
		
		s.notify(new ScoreRevealAction("Gen", ScoreRevealAction.scoreRevealAction.SHOW, h1));
		
		//Your hand (straight flush)
		c1=new Card(Card.Rank.Eight, Card.Suit.SPADES);
		c2=new Card(Card.Rank.Nine, Card.Suit.SPADES);
		c3=new Card(Card.Rank.Ten, Card.Suit.SPADES);
		c4=new Card(Card.Rank.Jack, Card.Suit.SPADES);
		c5=new Card(Card.Rank.Queen, Card.Suit.SPADES);
		
		h2 = new Hand();
		
		h2.add(c1);
		h2.add(c2);
		h2.add(c3);
		h2.add(c4);
		h2.add(c5);
		v = new HandValuator().valuateHand(h2);
		// Should show as you are tied with opponent
		assertTrue(s.makeScoreRevealAction(v) == ScoreRevealAction.scoreRevealAction.SHOW);
	}
	
	public void testFourPlayers()
	{
		// Opponent 1 hand (straight to queen)-shows
		c1=new Card(Card.Rank.Eight, Card.Suit.HEARTS);
		c2=new Card(Card.Rank.Nine, Card.Suit.CLUBS);
		c3=new Card(Card.Rank.Ten, Card.Suit.CLUBS);
		c4=new Card(Card.Rank.Jack, Card.Suit.CLUBS);
		c5=new Card(Card.Rank.Queen, Card.Suit.CLUBS);
		
		h1.add(c1);
		h1.add(c2);
		h1.add(c3);
		h1.add(c4);
		h1.add(c5);
		
		s.notify(new ScoreRevealAction("Gen", ScoreRevealAction.scoreRevealAction.SHOW, h1));
		
		//Opponent 2 (straight to queen)-Shows
		c1=new Card(Card.Rank.Eight, Card.Suit.CLUBS);
		c2=new Card(Card.Rank.Nine, Card.Suit.SPADES);
		c3=new Card(Card.Rank.Ten, Card.Suit.SPADES);
		c4=new Card(Card.Rank.Jack, Card.Suit.SPADES);
		c5=new Card(Card.Rank.Queen, Card.Suit.SPADES);
		
		h2.add(c1);
		h2.add(c2);
		h2.add(c3);
		h2.add(c4);
		h2.add(c5);
		
		s.notify(new ScoreRevealAction("Gen", ScoreRevealAction.scoreRevealAction.SHOW, h2));
		
		// Opponent 3 (pair)-folds
		c1=new Card(Card.Rank.Six, Card.Suit.CLUBS);
		c2=new Card(Card.Rank.Six, Card.Suit.SPADES);
		c3=new Card(Card.Rank.Two, Card.Suit.SPADES);
		c4=new Card(Card.Rank.Three, Card.Suit.SPADES);
		c5=new Card(Card.Rank.King, Card.Suit.SPADES);
		
		h3.add(c1);
		h3.add(c2);
		h3.add(c3);
		h3.add(c4);
		h3.add(c5);
		
		s.notify(new ScoreRevealAction("Gen", ScoreRevealAction.scoreRevealAction.FOLD, h3));
		
		//Your hand (straight to queen)
		c1=new Card(Card.Rank.Eight, Card.Suit.DIAMONDS);
		c2=new Card(Card.Rank.Nine, Card.Suit.DIAMONDS);
		c3=new Card(Card.Rank.Ten, Card.Suit.HEARTS);
		c4=new Card(Card.Rank.Jack, Card.Suit.DIAMONDS);
		c5=new Card(Card.Rank.Queen, Card.Suit.DIAMONDS);
		
		
		h4.add(c1);
		h4.add(c2);
		h4.add(c3);
		h4.add(c4);
		h4.add(c5);
		v = new HandValuator().valuateHand(h4);
		
		// Should show as you are tied with opponents
		assertTrue(s.makeScoreRevealAction(v) == ScoreRevealAction.scoreRevealAction.SHOW);
	}
}
