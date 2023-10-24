package ai.test;

import game.actions.*;

import java.util.ArrayList;


import util.Hand;
import util.test.AllCards;
import junit.framework.*; 
import ai.bets.bluff.AIParametricBluffFinder;

public class TestAIParametricBluffFinder extends TestCase {
	
	AIParametricBluffFinder L, M, H;  
	
	// parametric information 
	private final int LSize = 15;
	private final int MSize = 35; 
	private final int HSize = 55; 
	private final int Seed = 324098123;
	
	private int ExpectedSeeds[]; 
	private ArrayList<String> ps; 
	private Hand gHand, bHand; 
	private String p1, p2, p3, p4; 
	private final int LOWEST_BETTING_ROUND = 2;
	private final int HIGH_BETTING_ROUND = 65; 
	
	public TestAIParametricBluffFinder()
	{
		super();
		
		// Bluff Finders and their seeds for checking
		L = new AIParametricBluffFinder(5, LSize, Seed);
		M = new AIParametricBluffFinder(5, MSize, Seed);
		H = new AIParametricBluffFinder(5, HSize, Seed); 
		
		ExpectedSeeds = L.getIntegerSequence(5); 
		
		// A good and bad hand
		gHand = new Hand();   
		gHand.add(AllCards.aKS);
		gHand.add(AllCards.aQS); 
		gHand.add(AllCards.aJS); 
		gHand.add(AllCards.aAS); 
		gHand.add(AllCards.aTS);
		bHand = new Hand();
		bHand.add(AllCards.a2C); 
		bHand.add(AllCards.a3D); 
		bHand.add(AllCards.a4H); 
		bHand.add(AllCards.a5S); 
		bHand.add(AllCards.a7C);
		
		// A set of four players
		ps = new ArrayList<String>();
		p1 = "Gen Kazama";
		p2 = "David Kawrykow";
		p3 = "Gottfried Toussaint";
		p4 = "David Hasselhoff";
		ps.add(p1); ps.add(p2); ps.add(p3); ps.add(p4);
	}
	
	public void setUp()
	{
		// Set up a new game for every bluffFinder
		updateBluffFindersWithAction(new NewGameAction("JUNIT", ps)); 
	}
	
	public void testLearnedPlayers()
	{
		assertTrue(L.isPlayerBluffing(p1) == false);
		assertTrue(L.isPlayerBluffing(p2) == false);
		assertTrue(L.isPlayerBluffing(p3) == false);
		assertTrue(L.isPlayerBluffing(p4) == false);
		
		assertTrue(M.isPlayerBluffing(p1) == false);
		assertTrue(M.isPlayerBluffing(p2) == false);
		assertTrue(M.isPlayerBluffing(p3) == false);
		assertTrue(M.isPlayerBluffing(p4) == false);
		
		assertTrue(H.isPlayerBluffing(p1) == false);
		assertTrue(H.isPlayerBluffing(p2) == false);
		assertTrue(H.isPlayerBluffing(p3) == false);
		assertTrue(H.isPlayerBluffing(p4) == false);
	} 
	
	public void testFirstMatchWithBluffs()
	{
		// simulate a game with p1 notably bluffing for all bluffFinders 
		simulateMatchWithBluff(HIGH_BETTING_ROUND);
		
		assertTrue(L.isPlayerBluffing(p1) == false);
		assertTrue(L.isPlayerBluffing(p2) == false);
		assertTrue(L.isPlayerBluffing(p3) == false);
		assertTrue(L.isPlayerBluffing(p4) == false);
		
		assertTrue(M.isPlayerBluffing(p1) == false);
		assertTrue(M.isPlayerBluffing(p2) == false);
		assertTrue(M.isPlayerBluffing(p3) == false);
		assertTrue(M.isPlayerBluffing(p4) == false);
		
		assertTrue(H.isPlayerBluffing(p1) == false);
		assertTrue(H.isPlayerBluffing(p2) == false);
		assertTrue(H.isPlayerBluffing(p3) == false);
		assertTrue(H.isPlayerBluffing(p4) == false);
		
	}
	
	public void testFirstMatchWithoutBluffs()
	{
		// simulate a game with nobody bluffing  
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		
		assertTrue(L.isPlayerBluffing(p1) == false);
		assertTrue(L.isPlayerBluffing(p2) == false);
		assertTrue(L.isPlayerBluffing(p3) == false);
		assertTrue(L.isPlayerBluffing(p4) == false);
		
		assertTrue(M.isPlayerBluffing(p1) == false);
		assertTrue(M.isPlayerBluffing(p2) == false);
		assertTrue(M.isPlayerBluffing(p3) == false);
		assertTrue(M.isPlayerBluffing(p4) == false);
		
		assertTrue(H.isPlayerBluffing(p1) == false);
		assertTrue(H.isPlayerBluffing(p2) == false);
		assertTrue(H.isPlayerBluffing(p3) == false);
		assertTrue(H.isPlayerBluffing(p4) == false);
	}
	
	public void testEffectsOfBluffInLastMatch()
	{
		// simulate a game with p1 obviously bluffing
		simulateMatchWithBluff(HIGH_BETTING_ROUND);
		
		// simulate a partial match 
		updateBluffFindersWithAction(new NewMatchAction("JUNIT"));
		simulateBettingRound(HIGH_BETTING_ROUND);
		
		// We expect all three to reveal many bluffs
		checkCorrectBluffDetection(HIGH_BETTING_ROUND, true, true, true); 
		
		setUp(); 
		
		// simulate a game with p1 obviously bluffing
		simulateMatchWithBluff(HIGH_BETTING_ROUND);
		
		// simulate a partial match
		updateBluffFindersWithAction(new NewMatchAction("JUNIT")); 
		simulateBettingRound(LOWEST_BETTING_ROUND);
		
		// We expect all three to reveal no bluffs
		checkCorrectBluffDetection(LOWEST_BETTING_ROUND, false, false, false);
	}
	
	public void testEffectsOfBluffInHistory()
	{	
		// simulate a game with p1 obviously bluffing
		simulateMatchWithBluff(HIGH_BETTING_ROUND);
		
		// simulate three more games with nobody bluffing
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND); 
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		
		// simulate a partial match 
		updateBluffFindersWithAction(new NewMatchAction("JUNIT"));
		simulateBettingRound(HIGH_BETTING_ROUND);
		
		// We expect all three to reveal many bluffs
		checkCorrectBluffDetection(HIGH_BETTING_ROUND, true, true, true); 
		
		setUp(); 
		
		// simulate a game with p1 obviously bluffing
		simulateMatchWithBluff(HIGH_BETTING_ROUND);
		
		// simulate three more games with nobody bluffing
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND); 
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		
		// simulate a partial match
		updateBluffFindersWithAction(new NewMatchAction("JUNIT")); 
		simulateBettingRound(LOWEST_BETTING_ROUND);
		
		// We expect all three to reveal no bluffs
		checkCorrectBluffDetection(LOWEST_BETTING_ROUND, false, false, false);
	}
	
	public void testErasingBluffFromHistory()
	{
		// simulate a game with p1 obviously bluffing
		simulateMatchWithBluff(HIGH_BETTING_ROUND);
		
		// simulate five more games with nobody bluffing
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND); 
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		
		// simulate a partial match 
		updateBluffFindersWithAction(new NewMatchAction("JUNIT"));
		simulateBettingRound(HIGH_BETTING_ROUND);
		
		// We expect all three to reveal no bluffs 
		checkCorrectBluffDetection(HIGH_BETTING_ROUND, false, false, false);
		
		// simulate a game with p1 obviously bluffing
		simulateMatchWithBluff(HIGH_BETTING_ROUND);
		
		// simulate five more games with nobody bluffing
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND); 
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		simulateMatchWithoutBluff(HIGH_BETTING_ROUND);
		
		// simulate a partial match 
		updateBluffFindersWithAction(new NewMatchAction("JUNIT"));
		simulateBettingRound(LOWEST_BETTING_ROUND);
		
		// We expect all three to reveal no bluffs 
		checkCorrectBluffDetection(LOWEST_BETTING_ROUND, false, false, false);
	}
	
	public void testAMatch()
	{
		updateBluffFindersWithAction(new NewGameAction("GOD", ps));
		updateBluffFindersWithAction(new NewMatchAction());
		updateBluffFindersWithAction(new IncrementAnteAction
		("GOD", IncrementAnteAction.incrementAnteAction.SUCCEED, 2));
		
		updateBluffFindersWithAction(new ScoreAction("Gen Kazama", 100));
		updateBluffFindersWithAction(new ScoreAction("David Hasselhoff", 100));
		updateBluffFindersWithAction(new ScoreAction("David Kawrykow", 100));
		updateBluffFindersWithAction(new ScoreAction("Gottfried Toussaint", 100));
		
		updateBluffFindersWithAction
		(new BetAction("David Hasselhoff", BetAction.betAction.RAISE, 5));

	}
	
	private void checkCorrectBluffDetection(int Stakes, boolean b1, boolean b2, boolean b3)
	{	
		
		int bluffConfidence; 
		
		for(int i = 0; i < ExpectedSeeds.length; i++)
		{			
			if(b1)
				bluffConfidence = LSize + 5; 
			else
				bluffConfidence = 5; 
			
			// Assert correctness of p1 for L
			if ( bluffConfidence >= ExpectedSeeds[i] && Stakes >= 5 )
				assertTrue ( L.isPlayerBluffing(p1) );
			else
				assertFalse (L.isPlayerBluffing(p1) );
			
			if(b2)
				bluffConfidence = MSize + 5; 
			else
				bluffConfidence = 5;
			
			// Assert correctness of p1 for M
			if ( bluffConfidence >= ExpectedSeeds[i] && Stakes >= 5)
				assertTrue ( M.isPlayerBluffing(p1) );
			else
				assertFalse (M.isPlayerBluffing(p1) );

			if(b3)
				bluffConfidence = HSize + 5; 
			else
				bluffConfidence = 5;
			
			// Assert correctness of p1 for H
			if ( bluffConfidence >= ExpectedSeeds[i] && Stakes >= 5)
				assertTrue ( H.isPlayerBluffing(p1) );
			else
				assertFalse (H.isPlayerBluffing(p1) );
		}	
	}

	private void simulateMatchWithBluff(int Stakes)
	{
		ScoreRevealAction.scoreRevealAction a 
		= ScoreRevealAction.scoreRevealAction.SHOW; 
		
		updateBluffFindersWithAction(new NewMatchAction("JUNIT"));
		simulateBettingRound(Stakes);
		updateBluffFindersWithAction(new ScoreRevealAction(p1, a, bHand)); 
		updateBluffFindersWithAction(new ScoreRevealAction(p2, a, gHand));
		updateBluffFindersWithAction(new ScoreRevealAction(p3, a, gHand));
		updateBluffFindersWithAction(new ScoreRevealAction(p4, a, gHand));
		
	}
	
	private void simulateMatchWithoutBluff(int Stakes)
	{
		ScoreRevealAction.scoreRevealAction a 
		= ScoreRevealAction.scoreRevealAction.SHOW; 
		
		updateBluffFindersWithAction(new NewMatchAction("JUNIT"));
		simulateBettingRound(Stakes);
		updateBluffFindersWithAction(new ScoreRevealAction(p1, a, gHand));
		updateBluffFindersWithAction(new ScoreRevealAction(p2, a, gHand));
		updateBluffFindersWithAction(new ScoreRevealAction(p3, a, gHand));
		updateBluffFindersWithAction(new ScoreRevealAction(p4, a, gHand));
	}
	
	
	// Simulates a betting round with each player making the input money
	// as bet
	private void simulateBettingRound(int contributions)
	{
		BetAction action;
		ScoreAction lAction; 
		
		// include the scores (always 100)
		lAction = new ScoreAction(p1, 100);
		updateBluffFindersWithAction(lAction);
		lAction = new ScoreAction(p2, 100);
		updateBluffFindersWithAction(lAction);
		lAction = new ScoreAction(p3, 100);
		updateBluffFindersWithAction(lAction);
		lAction = new ScoreAction(p4, 100);
		updateBluffFindersWithAction(lAction);
		
		// Simulate some new bets
		action = new BetAction(p1, BetAction.betAction.RAISE, contributions - 1);
		updateBluffFindersWithAction(action);
		action = new BetAction(p2, BetAction.betAction.CALL, 0);
		updateBluffFindersWithAction(action);
		action = new BetAction(p3, BetAction.betAction.CALL, 0);
		updateBluffFindersWithAction(action);
		action = new BetAction(p4, BetAction.betAction.CALL, 0);
		updateBluffFindersWithAction(action);
		
		// Add a card exchange action to swap the betting round 
		updateBluffFindersWithAction
		(
			new CardExchangeAction
			(
				p1, 
				CardExchangeAction.cardExchangeAction.NO_DISCARD, 
				0
			)
		); 
		
		// Simulate some new bets
		action = new BetAction(p1, BetAction.betAction.RAISE, 1);
		updateBluffFindersWithAction(action);
		action = new BetAction(p2, BetAction.betAction.CALL, 0);
		updateBluffFindersWithAction(action);
		action = new BetAction(p3, BetAction.betAction.CALL, 0);
		updateBluffFindersWithAction(action);
		action = new BetAction(p4, BetAction.betAction.CALL, 0);
		updateBluffFindersWithAction(action);
	}
	
	private void updateBluffFindersWithAction(Action lAction)
	{
		L.updateGameHistory(lAction);
		M.updateGameHistory(lAction);
		H.updateGameHistory(lAction);
	}
	
}
