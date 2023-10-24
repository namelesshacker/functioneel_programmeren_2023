package game.test;

import money.GamePot;
import junit.framework.TestCase;
import game.exceptions.InvalidMoneyValueException;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class tests the GamePot class.
 *
 */


public class TestGamePot extends TestCase {

	private GamePot g;
	private boolean[] q;
	
	public void setUp()
	{
		// Create a new game pot with 4 participants
		g=new GamePot(4);
		// Create a generic player array for testing
		q=new boolean[4];
		q[1] = q[2] = q[3] = q[0] = true;
		
	}
	
	private boolean playerArraysMatch(boolean[] a1, boolean[] a2) throws NullPointerException
	{
		if(a1.length != a2.length)
			return false;
		
		for(int i = 0; i < a1.length; i++)
		{
			if(a1[i] != a2[i])
				return false;
		}
		
		return true;
	}
	
	public void testSimpleBettingSchemes()
	{
		try
		{
			g.addBettingMoney(0, 5, false);		// Friendly Betting Set //
			g.addBettingMoney(1, 5, false);
			g.addBettingMoney(2, 5, false);
			g.addBettingMoney(3, 5, false);
			
			assertTrue(g.getNumberOfSmallerPots() == 1);
			assertTrue(g.getMoneyInPot(1) == 20);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
		}
		catch (Exception e)
		{
			fail();
		}
		
		try
		{
			setUp();
			
			g.addBettingMoney(0, 5, false);		// Illegal Bet from Second Player //
			g.addBettingMoney(1, 2, false);
			fail();
		}
		catch (Exception e)
		{
			// Good //
		}
		
		try
		{
			setUp();
			
			g.addBettingMoney(0, 5, false);		// Illegal Bet from Fourth Player //
			g.addBettingMoney(1, 5, false);
			g.addBettingMoney(2, 5, false);
			g.addBettingMoney(3, 2, false);
			fail();
		}
		catch (Exception e)
		{
			// Good 
		} 
	}
	
	public void testAnteSingleAllIn()
	{
		try
		{
			// The ante round - one player goes all in
			g.addBettingMoney(0, 11, false);
			g.addBettingMoney(1, 11, true);
			g.addBettingMoney(2, 11, false);
			g.addBettingMoney(3, 11, false);
			
			g.startNewBettingRound();
			
			// The pre bet round - everyone pays 15 except 1
			g.addBettingMoney(0, 15, false);
			g.addBettingMoney(2, 15, false);
			g.addBettingMoney(3, 15, false);
			
			g.startNewBettingRound(); 
			
			// The post bet round - raise, fold, fold
			g.addBettingMoney(0, 4, false);
			g.abortParticipant(2);
			g.abortParticipant(3);
			
			// Test that there are two pots, one with 4x11
			// the second with 3x15 + 4 
			assertTrue(g.getMoneyInPot(1) == 4*11);
			assertTrue(g.getMoneyInPot(2) == 3*15 + 4);	
			
			setUp(); 
			
//			 The ante round - one player goes all in
			g.addBettingMoney(0, 11, false);
			g.addBettingMoney(1, 11, true);
			g.addBettingMoney(2, 11, false);
			g.addBettingMoney(3, 11, false);
			
			g.startNewBettingRound();
			
			// The pre bet round - everyone pays 15 except 1
			g.addBettingMoney(0, 15, false);
			g.addBettingMoney(2, 15, false);
			g.addBettingMoney(3, 15, false);
			
			g.startNewBettingRound(); 
			
			// The post bet round - call, call, raise, call, call 
			g.addBettingMoney(0, 0, false);
			g.addBettingMoney(2, 0, false);
			g.addBettingMoney(3, 4, false);
			g.addBettingMoney(0, 4, false);
			g.addBettingMoney(2, 4, false);
			
			// Test that there are two pots, one with 4x11
			// the second with 3x19 
			assertTrue(g.getMoneyInPot(1) == 4*11);
			assertTrue(g.getMoneyInPot(2) == 3*19);
			assertTrue(g.getNumberOfSmallerPots() == 2); 
		}
		catch (Exception e)
		{
			e.printStackTrace();
			assertTrue(false); 
		}
	}
	
	public void testDoubleAllInPlusRaise()
	{
		try
		{
			g.addBettingMoney(0, 5, false);
			g.addBettingMoney(1, 5, false);
			g.addBettingMoney(2, 5, false);
			g.addBettingMoney(3, 5, false);
			
			g.startNewBettingRound();
			
			g.addBettingMoney(0, 69, true);
			g.addBettingMoney(1, 69, false);
			g.addBettingMoney(2, 69, false);
			g.addBettingMoney(3, 69, true);
			
			g.startNewBettingRound();
			
			g.addBettingMoney(1, 19, false);
			g.addBettingMoney(2, 19, false);
			
			System.out.println(g.getNumberOfSmallerPots());
		}
		catch (Exception e)
		{
			assertTrue(false);
		}
	}
	
	public void testKosherSingleAllIn()
	{		
		try
		{
			g.addBettingMoney(0, 30, false);
			g.addBettingMoney(1, 30, false);
			g.addBettingMoney(2, 10, true);		// Third Player all in with less
			g.addBettingMoney(3, 30, false);
			
			assertTrue(g.getNumberOfSmallerPots() == 2);
			assertTrue(g.getMoneyInPot(1) == 40);
			assertTrue(g.getMoneyInPot(2) == 60);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			assertFalse(g.addBettingMoney(3, 10, false));
			
			q[2] = false;
			
			assertTrue(playerArraysMatch(g.playersInPot(2), q)); 
			
			setUp();
			
			g.addBettingMoney(0, 30, false);		 
			g.addBettingMoney(1, 30, false);
			g.addBettingMoney(2, 30, true);		// Third player All in with same amount 
			g.addBettingMoney(3, 30, false);
			
			assertTrue(g.getNumberOfSmallerPots() == 1);
			assertTrue(g.getMoneyInPot(1) == 120);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			assertFalse(g.addBettingMoney(3, 10, false));
			
			setUp();
			
			g.addBettingMoney(0, 30, false);		 
			g.addBettingMoney(1, 30, false);
			g.addBettingMoney(2, 40, true);		// Third player All in with more
			g.addBettingMoney(3, 40, false);
			g.addBettingMoney(0, 20, false);	// First player raises
			g.addBettingMoney(1, 30, false);	// Second player raises
			g.addBettingMoney(3, 20, false);	// Third player calls			
			g.addBettingMoney(0, 10, false);	// First player calls to complete round
			
			assertTrue(g.getNumberOfSmallerPots() == 2);		
			assertTrue(g.getMoneyInPot(1) == 160);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			q[2] = false;
			
			assertTrue(g.getMoneyInPot(2) == 60);
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			assertFalse(g.addBettingMoney(3, 10, false));
			
			setUp();
			
			g.addBettingMoney(0, 10, true);				// First Player all in  
			g.addBettingMoney(1, 20, false);			// Second Player raises
			g.addBettingMoney(2, 20, false);			// Third Player calls
			g.addBettingMoney(3, 30, false);			// Fourth Player raises	
			g.addBettingMoney(1, 10, false);			// First Player calls
			g.addBettingMoney(2, 10, false);			// Second Player calls
			
			assertTrue(g.getNumberOfSmallerPots() == 2);
			assertTrue(g.getMoneyInPot(1) == 40);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			q[0] = false;
			
			assertTrue(g.getMoneyInPot(2) == 60);
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			assertFalse(g.addBettingMoney(3, 10, false));
						
		} 
		catch (Exception e)
		{
			fail();
		} 	
	}
	
	public void testDoubleAllInFromGUI()
	{
		/*
		 * 
		 * Ante is: 5
			[Mike Tyson-EEEEEEEE, Mike Tyson-XXX, Mike Tyson-OOO, Mike Tyson-iii]
			Score:	Mike Tyson-EEEEEEEE 155
			Score:	Mike Tyson-XXX 435
			Score:	Mike Tyson-OOO 15
			Score:	Mike Tyson-iii 195
			The Cards were dealt out
			Raise 32  
			Score:	Mike Tyson-EEEEEEEE 123
			Call      
			Score:	Mike Tyson-XXX 403
			All in!   
			Score:	Mike Tyson-OOO 0
			All in!   
			Score:	Mike Tyson-iii 0
			Fold      
			Fold      
		 */
		
		try
		{
			g.addBettingMoney(0, 5, false);
			g.addBettingMoney(1, 5, false);
			g.addBettingMoney(2, 5, false);
			g.addBettingMoney(3, 5, false);
			
			g.startNewBettingRound();
			
			g.addBettingMoney(0, 32, false);
			g.addBettingMoney(1, 32, false);
			g.addBettingMoney(2, 15, true);
			g.addBettingMoney(3, 195, true);
			
			assert(g.getNumberOfSmallerPots() == 2);
			
			g.abortParticipant(0);
			g.abortParticipant(1);
			
			assert(g.getNumberOfSmallerPots() == 2);
			assert(g.abortParticipant(3) == 214);
		}
		catch(Exception e)
		{
			e.printStackTrace();
		}
	}
	
	public void testKosherAllInTwo()
	{
		try
		{
			assertTrue(g.addBettingMoney(0, 5, false));
			assertTrue(g.addBettingMoney(1, 5, false));
			assertTrue(g.addBettingMoney(2, 5, false));
			assertTrue(g.addBettingMoney(3, 5, false));
			
			g.startNewBettingRound();
			
			assertTrue(g.addBettingMoney(0, 1,  false));
			assertTrue(g.addBettingMoney(1, 28, false));
			assertTrue(g.addBettingMoney(2, 25, true));
			assertTrue(g.abortParticipant(3) == 0);
			
			setUp();
			
			g.addBettingMoney(0, 5, false);
			g.addBettingMoney(1, 5, false);
			g.addBettingMoney(2, 5, false);
			g.addBettingMoney(3, 5, false);
			
			g.startNewBettingRound();
			
			g.addBettingMoney(0, 0, false);
			g.addBettingMoney(1, 0, false);
			g.addBettingMoney(2, 0, false);			
			g.addBettingMoney(3, 39, false);
			g.addBettingMoney(0, 39, false);
			g.addBettingMoney(1, 54, false);
			assertTrue(g.abortParticipant(2) == 0);
			assertTrue(g.abortParticipant(3) == 0);
			g.addBettingMoney(0, 15, false);
			g.startNewBettingRound();
			g.addBettingMoney(0, 0, false);
			g.addBettingMoney(1, 257, true);
			g.addBettingMoney(0, 133, true);
			
			assertTrue(g.abortParticipant(1) == 124);
			
			
		}
		catch (Exception e)
		{
			e.printStackTrace();
			assertTrue ( false );
		}
	}
	
	public void testKosherMultipleAllIn()
	{		
		try
		{
			g.addBettingMoney(0, 30, false);		 
			g.addBettingMoney(1, 30, true);		// Second Player all in with same
			g.addBettingMoney(2, 30, true);		// Third Player all in with same
			g.addBettingMoney(3, 30, false);
			
			assertTrue(g.getNumberOfSmallerPots() == 1);
			assertTrue(g.getMoneyInPot(1) == 120);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			assertFalse(g.addBettingMoney(3, 10, false));
			
			assertTrue(g.getMinBet(0)==0);
			assertTrue(g.getMinBet(1)==0);
			assertTrue(g.getMinBet(2)==0);
			assertTrue(g.getMinBet(3)==0);
			
			
			setUp();
			
			g.addBettingMoney(0, 20, false);	// First player raises  
			g.addBettingMoney(1, 10, true);		// Second player goes all in with less
			g.addBettingMoney(2, 30, false);	// Third player raises	
			g.addBettingMoney(3, 5, true);		// Fourth player goes all in with even less
			g.addBettingMoney(0, 20, false);	// First player raises
			g.addBettingMoney(2, 5, true);		// Third player throws his last dime
			
			assertTrue(g.getNumberOfSmallerPots() == 4);
			assertTrue(g.getMoneyInPot(1) == 20);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			q[3] = false;
			
			assertTrue(g.getMoneyInPot(2) == 15);
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			
			q[1] = false;
			
			assertTrue(g.getMoneyInPot(3) == 50);
			assertTrue(playerArraysMatch(g.playersInPot(3), q));
			
			q[2] = false;
			
			assertTrue(g.getMoneyInPot(4) == 5);	// Pointless pot but present
			assertTrue(playerArraysMatch(g.playersInPot(4), q));
			
			setUp();
			
			g.addBettingMoney(0, 40, true);		// Everyone all in with decreasing amounts
			g.addBettingMoney(1, 30, true);
			g.addBettingMoney(2, 20, true);
			g.addBettingMoney(3, 10, true);
			
			assertTrue(g.getNumberOfSmallerPots() == 4);   
			assertTrue(g.getMoneyInPot(1) == 40);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			q[3] = false;
			assertTrue(g.getMoneyInPot(2) == 30);
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			q[2] = false;
			assertTrue(g.getMoneyInPot(3) == 20);
			assertTrue(playerArraysMatch(g.playersInPot(3), q));
			q[1] = false;
			assertTrue(g.getMoneyInPot(4) == 10);
			assertTrue(playerArraysMatch(g.playersInPot(4), q));
					
		} 
		catch (InvalidMoneyValueException e)
		{
			fail();
		} 
		
	} 
	
	public void testThreeCallOneRaiseSituation()
	{
		try
		{
			
			g.addBettingMoney(0, 5, false);
			
			g.addBettingMoney(1, 5, false);
			
			g.addBettingMoney(2, 5, false);
			
			g.addBettingMoney(3, 5, false);
			g.startNewBettingRound();
			
			g.addBettingMoney(0, 0, false);
			
			g.addBettingMoney(1, 0, false);
			
			g.addBettingMoney(2, 0, false);
			
			g.addBettingMoney(3, 24, false);
			
			assertTrue(g.getMinBet(0) == 24);
			assertTrue(g.getMinBet(1) == 24);
			assertTrue(g.getMinBet(2) == 24);
			assertTrue(g.getMinBet(3) == 0);
		}
		catch (Exception e)
		{
			assertTrue ( false );
		}
	}
	
	public void testDualFoldSituations()
	{
		try
		{
			g.addBettingMoney(0, 0, false);
			g.addBettingMoney(1, 6, false);
			g.abortParticipant(2);
			g.addBettingMoney(3, 11, false);
			g.abortParticipant(0);
			g.addBettingMoney(1, g.getMinBet(1), false);
			
			assertTrue(g.getMinBet(0) == 0);
			assertTrue(g.getMinBet(1) == 0);
			assertTrue(g.getMinBet(2) == 0);
			assertTrue(g.getMinBet(3) == 0);
		}
		catch (Exception e)
		{
			assertTrue ( false );
		}
	}
	
	public void testComplexSituations()
	{
		
		try{
						
												// Everyone checks
			g.addBettingMoney(2, 5, false);		// Player Three raises to 5
			g.addBettingMoney(3, 10, false);	// Player Four raises to 10
			g.addBettingMoney(0, 15, false);	// Player One raises to 15
			g.addBettingMoney(1, 15, false);	// Player Two calls
			g.addBettingMoney(2, 15, true);		// Player Three raises with all-in to 20
			g.addBettingMoney(3, 5, true);		// Player Four goes all-in with 15
			g.abortParticipant(0);				// Player One folds
			g.addBettingMoney(1, 100, true);	// Player Two goes all-in
			
			assertTrue(g.getNumberOfSmallerPots() == 3);
			assertTrue(g.getMoneyInPot(1) == 60);
			
			q[0] = false;
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			q[3] = false; 
			assertTrue(g.getMoneyInPot(2) == 10);
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			
			q[2] = false;
			assertTrue(g.getMoneyInPot(3) == 95);					
			assertTrue(playerArraysMatch(g.playersInPot(3), q));
			
			assertTrue(g.getMinBet(1)==0);
			
			setUp();
			
												// Everyone checks
			g.addBettingMoney(3, 10, false);	// Player Four raises to 10
			g.addBettingMoney(0, 10, false);	// Player One calls
			g.addBettingMoney(1, 10, false);	// Player Two calls
			g.addBettingMoney(2, 20, false);	// Player Three raises to 20
			g.abortParticipant(3);				// Player Four folds
			g.addBettingMoney(0, 50, true);		// Player One raises to 60 with all-in
			g.abortParticipant(1);				// Player Two folds
			g.addBettingMoney(2, 40, false);	// Player Three calls
			
			// A single pot with 140 to be won by Player One or Three
			assertTrue(g.getNumberOfSmallerPots() == 1);
			assertTrue(g.getMoneyInPot(1) == 140);		
			q[1] = q[3] = false;
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
		}
		catch (Exception e)
		{
			fail();
		} 		
	}
	
	public void testRegularMultipleRounds()
	{
		try{
			
			g.addBettingMoney(2, 5, false);		// Ante Round
			g.addBettingMoney(3, 5, false);		
			g.addBettingMoney(0, 5, false);		
			g.addBettingMoney(1, 5, false);
			
			g.startNewBettingRound();			// Pre-Card-Exchange Betting Round
			
			g.addBettingMoney(2, 10, false);	// Player Three raises to 10
			g.addBettingMoney(3, 15, false);	// Player Four raises to 15
			g.abortParticipant(0);				// Player One folds
			g.addBettingMoney(1, 15, false);	// Player Two calls
			g.addBettingMoney(2, 15, false);	// Player Three raises to 25
			g.addBettingMoney(3, 10, false);	// Player Four calls
			
			g.startNewBettingRound();
			
			g.addBettingMoney(2, 10, false);	// Player Three raises to 10
			g.addBettingMoney(3, 20, false);	// Player Four raises to 20
			g.addBettingMoney(1, 20, false);	// Player Two calls
			g.addBettingMoney(2, 30, false);	// Player Three raises to 40
			g.addBettingMoney(3, 40, false);	// Player Four raises to 60
			g.abortParticipant(1);				// Player Two folds
			g.addBettingMoney(2, 30, false);	// Player Three calls
			
			assertTrue(g.getNumberOfSmallerPots() == 1);
			assertTrue(g.getMoneyInPot(1) == 235);
			
			q[0] = q[1] = false;
			
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			setUp();
			
			g.addBettingMoney(0, 10, false);	// Player One raises to 10
			g.addBettingMoney(1, 20, false);	// Player Two raises to 20
			g.addBettingMoney(2, 50, true);		// Player Three raises to 50 with all-in
			g.addBettingMoney(3, 50, false);	// Player Four calls
			g.addBettingMoney(0, 40, false);	// Player One calls
			g.addBettingMoney(1, 30, false);	// Player Two calls
			
			g.startNewBettingRound();
			
			g.addBettingMoney(0, 20, false);	// Player One raises to 20
			g.addBettingMoney(2, 25, true);		// Player Three raises to 25 with all-in
			g.abortParticipant(3);				// Player Four folds
			g.addBettingMoney(0, 5, false);		// Player One calls
			
			assertTrue(g.getNumberOfSmallerPots() == 2);
			assertTrue(g.getMoneyInPot(1) == 200);
			assertTrue(g.getMoneyInPot(2) == 45);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			q[2] = q[3] = false;
			
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			
		} catch (Exception e) {}
	}
	
	public void testRegularMultipleRoundsEpisode()
	{
		try{
			g.addBettingMoney(2, 10, false);	// Player 2 raises to 10
			g.addBettingMoney(3, 15, false);	// Player 3 raises to 15

			g.addBettingMoney(0, 15, false);		// Player 0 all-in 15
			g.addBettingMoney(1, 20, false);	// Player 1 raises to 20
			g.addBettingMoney(2, 25, true);		// Player 2 raises to 10
			g.addBettingMoney(3, 40, true);		// Player 3 raises to 15
			
			g.addBettingMoney(0, 30, true);		// Player 0 goes all-in 30
			g.addBettingMoney(1, 35, false);	// Player 1 calls all

			assertTrue(g.getNumberOfSmallerPots() == 3);
			assertTrue(g.getMoneyInPot(1) == 140);
		
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			q[2] = false; 
			assertTrue(g.getMoneyInPot(2) == 30);
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			
			q[0] = false;
			assertTrue(g.getMoneyInPot(3) == 20);					
			assertTrue(playerArraysMatch(g.playersInPot(3), q));
			
			//new case
			setUp();
			g.addBettingMoney(1, 10, false);	// Player 1 raises to 10
			g.addBettingMoney(2, 20, false);	// Player 2 raises to 20
			g.addBettingMoney(3, 30, false);	// Player 3 raises to 30

			g.addBettingMoney(0, 30, false);	// Player 0 calls 30
			g.addBettingMoney(1, 50, true);		// Player 1 all-in 50 for total of 60
			g.addBettingMoney(2, 40, false);	// Player 2 calls 60
			g.addBettingMoney(3, 10, true);		// Player 3 all in 10
			
			g.addBettingMoney(0, 70, false);	// Player 0 raises to 100 (+70)
			g.addBettingMoney(2, 40, false);	// Player 1 calls
			
			g.startNewBettingRound();			// start post bet
			
			g.addBettingMoney(0, 10, false);	// Player 0 raises 10
			g.addBettingMoney(2, 15, false);	// Player 2 raises to 15
			g.abortParticipant(0);
			
			q[0]=false;
			assertTrue(g.getNumberOfSmallerPots() == 3);
			assertTrue(g.getMoneyInPot(1) == 160);
			assertTrue(playerArraysMatch(g.playersInPot(1), q));
			
			q[3] = false; 
			assertTrue(g.getMoneyInPot(2) == 60);
			assertTrue(playerArraysMatch(g.playersInPot(2), q));
			
			q[1]=false;
			assertTrue(g.getMoneyInPot(3) == 105);
			assertTrue(playerArraysMatch(g.playersInPot(3), q));
		}
		catch (Exception e)
		{
			fail();
		} 		
	}
	
	public void testIllegalOperations()
	{
		// Invalid participant IDs
		try{
			g.addBettingMoney(-1, 1, false);
			fail();
		} 
		catch (IllegalArgumentException e) {}
		catch (InvalidMoneyValueException e) {fail();}
		
		try{
			g.addBettingMoney(4, 1, false);
			fail();
		} 
		catch (IllegalArgumentException e) {}
		catch (InvalidMoneyValueException e) {fail();}
		
		// Invalid money values
		try{
			g.addBettingMoney(1, -1, false);
			fail();
		} 
		catch (IllegalArgumentException e) {fail();}
		catch (InvalidMoneyValueException e) {}
		
		// Attempting to bet after Folding
		try{
			g.abortParticipant(1);
			g.addBettingMoney(1, 1, false);
			fail();
		} 
		catch (IllegalArgumentException e) {}
		catch (InvalidMoneyValueException e) {fail();}
		
		// Attempting to Fold twice
		try{
			g.abortParticipant(1);
			fail();
		} 
		catch (IllegalArgumentException e) {}
		
		// Attempting to fold all players
		try{
			g.abortParticipant(0);
			g.abortParticipant(2);
			g.abortParticipant(3);		// cannot fold
			fail();
		} 
		catch (IllegalArgumentException e) {}
		
		// Attempting to bet after going all in
		setUp();
		
		try
		{
			g.addBettingMoney(0, 5, true);
			g.addBettingMoney(0, 1, true);
			fail();
		} 
		catch (IllegalArgumentException e) {}
		catch (InvalidMoneyValueException e) {fail();}
		
	} 
}
