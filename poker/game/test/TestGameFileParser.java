package game.test;

import java.io.FileNotFoundException;
import java.util.ArrayList;

import game.GameFileParser;
import game.actions.*;
import game.exceptions.HandFoldedException;
import scoring.HandValuator;
import scoring.HandValue;
import util.Hand;
import util.test.AllCards;
import junit.framework.TestCase;

/**
 *
 */

public class TestGameFileParser extends TestCase {

	private GameFileParser mGameFileParser;
	private HandValuator mHandValuator;
	private Hand mHand;
	
	public TestGameFileParser()
	{
		mHandValuator   = new HandValuator();
		mGameFileParser = new GameFileParser(" ");
	}
	
	public void setUp()
	{
		mHand = new Hand();
	}
	
	/**
	 * test Hand.toHand(String).toString() returns same data
	 */
	public void testStringToHandToString()
	{
		String input = "{ Two Three Four Five Seven }";
		assertTrue(compareStringWithHand(input));
		
		input = "{ Two Three Four Five Nine }";
		assertTrue(compareStringWithHand(input));
		
		input = "{ Two Three Four Five Jack }";
		assertTrue(compareStringWithHand(input));
		
		input = "{ Two Three Four Five King }";
		assertTrue(compareStringWithHand(input));
		
		input = "{ Two Three Four Five Ace }";
		assertTrue(compareStringWithHand(input));
	}
	
	public void testStringToHandToHandValue()
	{
		String input; 
		
		// Nothing
		
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a4C);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.a7D);
		input = "{ Two Three Four Five Seven }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		// Pair
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.a7D);
		input = "{ Two Three Three Five Seven }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		// Two Pair
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a7C);
		mHand.add(AllCards.a7D);
		input = "{ Two Three Three Seven Seven }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		// Three of a kind
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a7S);
		mHand.add(AllCards.a7C);
		mHand.add(AllCards.a7D);
		input = "{ Two Three Seven Seven Seven }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		// Straight
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a4S);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.a6D);
		input = "{ Two Three Four Five Six }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a4S);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.aAD);
		input = "{ Two Three Four Five Ace }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		// Full house
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a2D);
		mHand.add(AllCards.a2S);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.a5D);
		input = "{ Two Two Two Five Five }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a2D);
		mHand.add(AllCards.a5S);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.a5D);
		input = "{ Two Two Five Five Five }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
		
		// Four of a kind
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a2D);
		mHand.add(AllCards.a2S);
		mHand.add(AllCards.a2H);
		mHand.add(AllCards.a5D);
		input = "{ Two Two Two Two Five }";
		
		assertTrue(compareStringWithHandValue(input, mHandValuator.valuateHand(mHand)));
	}
	
	public void testFlushes()
	{
		String input;
		Hand lHand;
		
		// Flush
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a4C);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.a8C);
		input = "{ Two Three Four Five Eight }";
		lHand = Hand.toHand(input);
		lHand = Hand.makeHandFlushed(lHand);
		
		assertTrue(mHandValuator.valuateHand(lHand).compareTo(mHandValuator.valuateHand(mHand)) == 0);
		
		// Straight Flush
		setUp();
		mHand.add(AllCards.a2C);
		mHand.add(AllCards.a3C);
		mHand.add(AllCards.a4C);
		mHand.add(AllCards.a5C);
		mHand.add(AllCards.aAC);
		input = "{ Two Three Four Five Ace }";
		lHand = Hand.toHand(input);
		lHand = Hand.makeHandFlushed(lHand);
		
		assertTrue(mHandValuator.valuateHand(lHand).compareTo(mHandValuator.valuateHand(mHand)) == 0);		
	}
	
	public void testStringToBetAction()
	{
		// BetAction - All in
		BetAction lAction = new BetAction("David Kawrykow", BetAction.betAction.ALL_IN, 56);
		
		BetAction pAction = (BetAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(lAction.getActionMaker().equals(pAction.getActionMaker()));
		assertTrue(lAction.getAction() == pAction.getAction());
		assertTrue(lAction.getAmount() == pAction.getAmount());
		
		// BetAction - Raise
		lAction = new BetAction("David Kawrykow", BetAction.betAction.RAISE, 56);
		
		pAction = (BetAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(lAction.getActionMaker().equals(pAction.getActionMaker()));
		assertTrue(lAction.getAction() == pAction.getAction());
		assertTrue(lAction.getAmount() == pAction.getAmount());
		
		// BetAction - Call
		lAction = new BetAction("David Kawrykow", BetAction.betAction.CALL, 56);
		
		pAction = (BetAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(lAction.getActionMaker().equals(pAction.getActionMaker()));
		assertTrue(lAction.getAction() == pAction.getAction());
		assertTrue(lAction.getAmount() == pAction.getAmount());
		
		// BetAction - Fold
		lAction = new BetAction("David Kawrykow", BetAction.betAction.FOLD, 56);
		
		pAction = (BetAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(lAction.getActionMaker().equals(pAction.getActionMaker()));
		assertTrue(lAction.getAction() == pAction.getAction());
		assertTrue(lAction.getAmount() == pAction.getAmount());
		
	}
	
	public void testStringtoCardExchangeAction()
	{
		// CardExchangeAction - Discard 
		CardExchangeAction lAction = new CardExchangeAction("David Kawrykow", CardExchangeAction.cardExchangeAction.DISCARD, 2);
		
		CardExchangeAction pAction = (CardExchangeAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(lAction.getActionMaker().equals(pAction.getActionMaker()));
		assertTrue(lAction.getAction() == pAction.getAction());
		assertTrue(lAction.getAmount() == pAction.getAmount());
		
		// CardExchangeAction - Discard 
		lAction = new CardExchangeAction("David Kawrykow", CardExchangeAction.cardExchangeAction.NO_DISCARD, 2);
		
		pAction = (CardExchangeAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(lAction.getActionMaker().equals(pAction.getActionMaker()));
		assertTrue(lAction.getAction() == pAction.getAction());
		assertTrue(lAction.getAmount() == pAction.getAmount());
	}
	
	public void testStringToCardsDealtAction()
	{
		CardsDealtAction lAction = new CardsDealtAction();
		
		CardsDealtAction mAction = (CardsDealtAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(mAction instanceof CardsDealtAction);
	}
	
	public void testStringToGetMoneyAction()
	{
		GetMoneyAction lAction = new GetMoneyAction("David Kawrykow", 1, 56);
		GetMoneyAction mAction = (GetMoneyAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(mAction.getActionMaker().equals(lAction.getActionMaker()));
		assertTrue(mAction.getAmount() == lAction.getAmount());
		assertTrue(mAction.getPot() == lAction.getPot());
	}
	
	public void testStringToIncrementAnteAction()
	{
		// Successful raise of ante
		IncrementAnteAction lAction = new IncrementAnteAction("David Kawrykow", IncrementAnteAction.incrementAnteAction.SUCCEED, 10);
		
		IncrementAnteAction mAction = (IncrementAnteAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(mAction.getActionMaker().equals(lAction.getActionMaker()));
		assertTrue(mAction.getAction() == lAction.getAction());
		assertTrue(mAction.getAmount() == lAction.getAmount());
		
		// Tried but failed to raise ante
		lAction = new IncrementAnteAction("David Kawrykow", IncrementAnteAction.incrementAnteAction.FAIL, 0);
		mAction = (IncrementAnteAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(mAction.getActionMaker().equals(lAction.getActionMaker()));
		assertTrue(mAction.getAction() == lAction.getAction());
		assertTrue(mAction.getAmount() == lAction.getAmount());
		
		// Did not try to raise ante
		lAction = new IncrementAnteAction("David Kawrykow", IncrementAnteAction.incrementAnteAction.SUCCEED, 10);
		mAction = (IncrementAnteAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(mAction.getActionMaker().equals(lAction.getActionMaker()));
		assertTrue(mAction.getAction() == lAction.getAction());
		assertTrue(mAction.getAmount() == lAction.getAmount());
	}
	
	public void testStringToNewGameAction()
	{
		ArrayList<String> lPlayers = new ArrayList<String>();
		lPlayers.add("David Kawrykow");
		lPlayers.add("Gen Kazama");
		lPlayers.add("Martin Robillard");
		lPlayers.add("A+");
		
		ArrayList<Integer> lMoneys = new ArrayList<Integer>();
		lMoneys.add(new Integer(56));
		lMoneys.add(new Integer(23));
		lMoneys.add(new Integer(456));
		lMoneys.add(new Integer(32));
		
		ArrayList<String> lBigIcons = new ArrayList<String>();
		lBigIcons.add("Big 1");
		lBigIcons.add("Big 2");
		lBigIcons.add("Big 3");
		lBigIcons.add("Big 4");
		
		ArrayList<String> lSmallIcons = new ArrayList<String>();
		lSmallIcons.add("Small 1");
		lSmallIcons.add("Small 2");
		lSmallIcons.add("Small 3");
		lSmallIcons.add("Small 4");
		
		NewGameAction lAction = new NewGameAction("e", lPlayers, lMoneys, lBigIcons, lSmallIcons);
		NewGameAction mAction = (NewGameAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(mAction.getNumberOfParticipants() == 4);
		
		// test player names
		assertTrue(mAction.getParticipant(1).equals("David Kawrykow"));
		assertTrue(mAction.getParticipant(2).equals("Gen Kazama"));
		assertTrue(mAction.getParticipant(3).equals("Martin Robillard"));
		assertTrue(mAction.getParticipant(4).equals("A+"));
		
		// test player purses
		assertTrue(mAction.getParticipantMoney(1) == 56);
		assertTrue(mAction.getParticipantMoney(2) == 23);
		assertTrue(mAction.getParticipantMoney(3) == 456);
		assertTrue(mAction.getParticipantMoney(4) == 32);
		
		// test player big icons
		assertTrue(mAction.getParticipantBigIcon(1).equals("Big 1"));
		assertTrue(mAction.getParticipantBigIcon(2).equals("Big 2"));
		assertTrue(mAction.getParticipantBigIcon(3).equals("Big 3"));
		assertTrue(mAction.getParticipantBigIcon(4).equals("Big 4"));
		
		// test player small icons
		assertTrue(mAction.getParticipantSmallIcon(1).equals("Small 1"));
		assertTrue(mAction.getParticipantSmallIcon(2).equals("Small 2"));
		assertTrue(mAction.getParticipantSmallIcon(3).equals("Small 3"));
		assertTrue(mAction.getParticipantSmallIcon(4).equals("Small 4"));
		
	}
	
	public void testStringToNewMatchAction()
	{
		NewMatchAction lAction = new NewMatchAction();
		lAction.setMatchID(3);
		NewMatchAction mAction = (NewMatchAction) mGameFileParser.parseString(lAction.toCode());
		assertEquals(mAction.getMatchID(), 3);
	}
	
	public void testStringToPlayerRemovedAction()
	{
		PlayerRemovedAction lAction = new PlayerRemovedAction("Davie Kawrykow");
		PlayerRemovedAction mAction = (PlayerRemovedAction) mGameFileParser.parseString(lAction.toCode());
		assertTrue(mAction.getActionMaker().equals("Davie Kawrykow"));
	}
	
	public void testStringToScoreAction()
	{
		ScoreAction lAction = new ScoreAction("David Kawrykow", 56);
		ScoreAction mAction = (ScoreAction) mGameFileParser.parseString(lAction.toCode());
		assertTrue(mAction.getActionMaker().equals("David Kawrykow"));
		assertTrue(mAction.getAmount() == 56);
	}
	
	public void testScoreRevealAction()
	{
		
		// Test Show
		Hand lHand = new Hand();
		lHand.add(AllCards.a2C);
		lHand.add(AllCards.a2D);
		lHand.add(AllCards.a2H);
		lHand.add(AllCards.a2S);
		lHand.add(AllCards.a3C);
		ScoreRevealAction lAction = new ScoreRevealAction("David Kawrykow", ScoreRevealAction.scoreRevealAction.SHOW, lHand);
		lAction.setHandValue(mHandValuator.valuateHand(lHand));
		ScoreRevealAction mAction = (ScoreRevealAction) mGameFileParser.parseString(lAction.toCode());
		
		try
		{
			assertTrue(mAction.getActionMaker().equals("David Kawrykow"));
			assertTrue(mAction.getHand().getHighCard().compareTo(AllCards.a3C) == 0);
			assertTrue(mAction.getHandValue().compareTo(mHandValuator.valuateHand(lHand)) == 0);
		}
		catch (HandFoldedException e)
		{
			assertTrue(false);
		}

		// Test FOLD
		lAction = new ScoreRevealAction("David Kawrykow", ScoreRevealAction.scoreRevealAction.FOLD, lHand);
		mAction = (ScoreRevealAction) mGameFileParser.parseString(lAction.toCode());
		
		try
		{
			assertTrue(mAction.getActionMaker().equals("David Kawrykow"));
			assertTrue(mAction.getAction() == ScoreRevealAction.scoreRevealAction.FOLD);
			mAction.getHand();
			assertTrue(false);
		}
		catch (HandFoldedException e)
		{
			try
			{
				mAction.getHandValue();
				assertTrue(false);
			}
			catch (HandFoldedException f)
			{
				
			}
		}
		
		// Test NO-SHOW
		lAction = new ScoreRevealAction("David Kawrykow", ScoreRevealAction.scoreRevealAction.NO_SHOW, lHand);
		mAction = (ScoreRevealAction) mGameFileParser.parseString(lAction.toCode());
		
		try
		{
			assertTrue(mAction.getActionMaker().equals("David Kawrykow"));
			assertTrue(mAction.getAction() == ScoreRevealAction.scoreRevealAction.NO_SHOW);
			mAction.getHand();
			assertTrue(false);
		}
		catch (HandFoldedException e)
		{
			try
			{
				mAction.getHandValue();
				assertTrue(false);
			}
			catch (HandFoldedException f)
			{
				
			}
		}
	}
	
	public void testStringToUserSetHandAction()
	{
		Hand lHand = new Hand();
		lHand.add(AllCards.a2C);
		lHand.add(AllCards.a2D);
		lHand.add(AllCards.a2H);
		lHand.add(AllCards.a2S);
		lHand.add(AllCards.a3C);
		UserSetHandAction lAction = new UserSetHandAction("David Kawrykow", lHand, mHandValuator.valuateHand(lHand));
		UserSetHandAction mAction = (UserSetHandAction) mGameFileParser.parseString(lAction.toCode());
		
		assertTrue(mAction.getActionMaker().equals("David Kawrykow"));
		assertTrue(mAction.getHand().getHighCard().compareTo(AllCards.a3C) == 0);
		assertTrue(mAction.getHandValue().compareTo(mHandValuator.valuateHand(lHand)) == 0);
	}
	
	public void testLoadFile()
	{
		String ss = java.io.File.separator;
		String pathName = "src" + ss + "game" + ss + "test" + ss + "PokerFighter_AutoSave_File.txt";
		GameFileParser tGameFileParser = new GameFileParser(pathName);
		assertTrue(tGameFileParser.isCorrupted() == false);
		assertTrue(tGameFileParser.iterator().hasNext());
	}
	
	private boolean compareStringWithHand(String input)
	{
		return Hand.toHand(input).toString().equals((input));
	}
	
	private boolean compareStringWithHandValue(String input, HandValue m)
	{
		return (m.compareTo(mHandValuator.valuateHand(Hand.toHand(input))) == 0);
	}
}
