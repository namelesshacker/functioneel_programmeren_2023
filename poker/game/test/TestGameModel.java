package game.test;


import java.io.IOException;
import java.util.ArrayList;

import money.GamePot;
import players.DriverPlayer;
import players.Player;
import players.PrototypePlayer;
import junit.framework.TestCase;
import game.*;
import game.actions.*;
import game.exceptions.*;

/**
 * @class This is the test class for GameModel
 * @author david kawrykow and gen kazama
 */
public class TestGameModel extends TestCase implements GameListener{
	private GameModel gameModel;
	private PrototypePlayer p1, p2, p3, p4;
	private int p1Purse, p2Purse, p3Purse, p4Purse;
	private int TotalPurseMoney;
	private int counter;
	private GameStats gs;
	
	/**
	 * Sets up a game model instance with four players attached. 
	 */
	public void setUp()
	{
		gs=new GameStats();
		gameModel = new GameModel();
		counter = 1;
		
		p1Purse = p2Purse = p3Purse = p4Purse = 50;
		
		p1 = new PrototypePlayer("Toussaint", p1Purse);
		p2 = new PrototypePlayer("David Hasselhoff", p2Purse);
		p3 = new PrototypePlayer("KITT", p3Purse);
		p4 = new PrototypePlayer("KARR", p4Purse);

		TotalPurseMoney = p1Purse + p2Purse + p3Purse + p4Purse;
		
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.addPlayerToGame(p4);
			gameModel.attachGameListener(this);
		} 
		catch (Exception e)
		{ fail(); }
	}
	
	/**
	 * This methods tests attaching observers to and detaching observers incorrectly
	 * from GameModel 
	 */
	public void testAttachDetachExceptions()
	{
		gameModel=new GameModel();
		
		p4 = new PrototypePlayer("KARR", gameModel.getAnte() - 1);
		
		// Insufficiently funded players are rejected
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.addPlayerToGame(p4);
			fail();
		}
		catch(InsufficientFundsException e)
		{
			/* Should go in here*/
		}
		catch(Exception e)
		{
			fail();
		}
		
		setUp();
		PrototypePlayer p5 = new PrototypePlayer("The charmer", 50);
		
		// A fifth player is rejected
		try
		{
			gameModel.addPlayerToGame(p5);
			fail();
		}
		catch (GameIsFullException e)
		{
			// Should go in here
		}
		catch (Exception e)
		{
			fail();
		}
		
		// Removing an observer which is not observing is rejected
		try
		{
			gameModel.detachGameListener(p5);
			fail();
		}
		catch (IllegalArgumentException e)
		{
			// Should go here
		}
	}
	
	/**
	 * This methods tests trying to work with a GameModel instance not possessing
	 * the minimum number of players required to support these operations
	 */
	public void testMatchExceptions()
	{
		gameModel = new GameModel();
		
		// checking the dealer with no player 
		// and executing match with one player
		try
		{
			gameModel.getDealer();
			fail();
		}
		catch (NoDealerException e){}
		catch (Exception e){fail();}
		
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.ExecuteSingleMatch();
			fail();
		} 
		catch (GameIsEmptyException e)
		{ }
		catch (Exception e)
		{ fail(); }
		
		try
		{
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.ExecuteSingleMatch();
			fail();
		} 
		catch (GameIsEmptyException e)
		{ }
		catch (Exception e)
		{ fail(); }
	}
	
	/**
	 * Tests if the game doesn't start if the number of players < 4
	 *
	 */
	public void testNumPlayers()
	{
		try
		{
			gameModel.addPlayerToGame(p1);
			gameModel.addPlayerToGame(p2);
			gameModel.addPlayerToGame(p3);
			gameModel.ExecuteSingleMatch();
			fail();
		}
		catch(Exception e)
		{
			//should reach here
		}
	}
	
	/**
	 * This methods tests whether a player is removed in the ante round if
	 * he has insufficient funds during the ante round
	 */
	public void testPlayerRemovedInAnteRound()
	{
		try
		{
			p1.getPurse().removeChips(46);				// can no longer pay ante
			gameModel.ExecuteSingleMatch();				// player will be removed from ante here
			assertTrue(p1.getPurse().getChips() == 4);	// and should still have 4 bucks left
		}

		catch (Exception e )
		{
			e.printStackTrace();
			fail();
		}
	}
	
	/**
	 * This methods tests a single match. It executes the match and checks that
	 * the actual money matches the expected purse money (within 3 because of split
	 * money integer errors) 
	 */
	public void testOneMatch()
	{
		try
		{
			gameModel.ExecuteSingleMatch();
		}
		catch(Exception e)
		{
			fail();
		}
		
		try
		{
			int ActualMoney =   p1.getPurse().getChips() + p2.getPurse().getChips() 
							 +  p3.getPurse().getChips() + p4.getPurse().getChips();
		
			assertTrue(TotalPurseMoney - ActualMoney >= 0);
			assertTrue(TotalPurseMoney - ActualMoney < 4);

			TotalPurseMoney=ActualMoney;
		} 
		catch (PurseIsEmptyException e)
		{
			fail();		// our prototype players can't die in one round when playing together
		}
		
	}
	
	/**
	 * This method runs through an entire game. It checks at the end of each match whether
	 * or not the monies add up between purses and expected total. It also checks that the
	 * winner has money in his purse at the end of the round, and that he does not have less
	 * than anybody else
	 */
	public void testGameUsingPrototypePlayer()
	{
		while(! gameModel.isGameOver())
		{
			testOneMatch();
		}
		try{
			assertTrue(gameModel.getGameWinner().getPurse().getChips() > 0);
			assertTrue(gameModel.getGameWinner().getPurse().getChips() >= p1.getPurse().getChips());
			assertTrue(gameModel.getGameWinner().getPurse().getChips() >= p2.getPurse().getChips());
			assertTrue(gameModel.getGameWinner().getPurse().getChips() >= p3.getPurse().getChips());
			assertTrue(gameModel.getGameWinner().getPurse().getChips() >= p4.getPurse().getChips());
		}
		catch(Exception e)
		{
			fail();
		}
	}
	
	/**
	 * Test a generic match
	 *
	 */
	public void testMatchUsingDriverPlayer1()
	{
		DriverPlayer pp1, pp2, pp3, pp4;
		GameModel g;
		
		pp1=new DriverPlayer("David Kawrykow", 100);
		pp2=new DriverPlayer("G Toussaint", 100);
		pp3=new DriverPlayer("Gen Kazama", 100);
		pp4=new DriverPlayer("David Hasselhoff", 100);
		
		
		pp1.setPreBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPreBetAction(new BetAction(pp2.toString(), BetAction.betAction.CALL, 0));
		pp3.setPreBetAction(new BetAction(pp3.toString(), BetAction.betAction.RAISE, 13));
		pp4.setPreBetAction(new BetAction(pp4.toString(), BetAction.betAction.CALL, 0));

		pp1.setPostBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPostBetAction(new BetAction(pp2.toString(), BetAction.betAction.RAISE, 24));
		pp3.setPostBetAction(new BetAction(pp3.toString(), BetAction.betAction.CALL, 0));
		pp4.setPostBetAction(new BetAction(pp4.toString(), BetAction.betAction.CALL, 0));

		pp1.setScoreRevealAction(new ScoreRevealAction(pp1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	pp1.getHand()));
		pp2.setScoreRevealAction(new ScoreRevealAction(pp2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp2.getHand()));
		pp3.setScoreRevealAction(new ScoreRevealAction(pp3.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp3.getHand()));
		pp4.setScoreRevealAction(new ScoreRevealAction(pp4.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp4.getHand()));

		g=new GameModel();
		
		try
		{
			g.addPlayerToGame(pp1);
			g.addPlayerToGame(pp2);
			g.addPlayerToGame(pp3);
			g.addPlayerToGame(pp4);
			
			g.ExecuteSingleMatch();
			assertTrue(pp4.getPurse().getChips()==58);
		}
		catch(Exception e)
		{
			fail();
		}
	}
	
	/**
	 * All players fold in the pre exchange betting round.
	 *
	 */
	public void testFoldAllPlayers1()
	{
		DriverPlayer pp1, pp2, pp3, pp4;
		GameModel g;
		
		pp1=new DriverPlayer("David Kawrykow", 100);
		pp2=new DriverPlayer("G Toussaint", 100);
		pp3=new DriverPlayer("Gen Kazama", 100);
		pp4=new DriverPlayer("David Hasselhoff", 100);
		
		//Everyone folds
		pp1.setPreBetAction(new BetAction(pp1.toString(), BetAction.betAction.FOLD, 0));
		pp2.setPreBetAction(new BetAction(pp2.toString(), BetAction.betAction.FOLD, 0));
		pp3.setPreBetAction(new BetAction(pp3.toString(), BetAction.betAction.FOLD, 0));
		pp4.setPreBetAction(new BetAction(pp4.toString(), BetAction.betAction.FOLD, 0));

		// The following actions don't matter (since everyone folded).
		pp1.setPostBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPostBetAction(new BetAction(pp2.toString(), BetAction.betAction.CALL, 0));
		pp3.setPostBetAction(new BetAction(pp3.toString(), BetAction.betAction.CALL, 0));
		pp4.setPostBetAction(new BetAction(pp4.toString(), BetAction.betAction.CALL, 0));

		pp1.setScoreRevealAction(new ScoreRevealAction(pp1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	pp1.getHand()));
		pp2.setScoreRevealAction(new ScoreRevealAction(pp2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp2.getHand()));
		pp3.setScoreRevealAction(new ScoreRevealAction(pp3.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp3.getHand()));
		pp4.setScoreRevealAction(new ScoreRevealAction(pp4.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp4.getHand()));

		g=new GameModel();
		
		try
		{
			g.addPlayerToGame(pp1);
			g.addPlayerToGame(pp2);
			g.addPlayerToGame(pp3);
			g.addPlayerToGame(pp4);
			
			g.ExecuteSingleMatch();
			assertTrue(pp2.getPurse().getChips()==115);
		}
		catch(Exception e)
		{
			//should get here
			fail();
		}
	}
	
	/**
	 * All players fold in the post exchange betting round.
	 *
	 */
	public void testFoldAllPlayers2()
	{
		DriverPlayer pp1, pp2, pp3, pp4;
		GameModel g;
		
		pp1=new DriverPlayer("David Kawrykow", 100);
		pp2=new DriverPlayer("G Toussaint", 100);
		pp3=new DriverPlayer("Gen Kazama", 100);
		pp4=new DriverPlayer("David Hasselhoff", 100);
		
		
		pp1.setPreBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPreBetAction(new BetAction(pp2.toString(), BetAction.betAction.CALL, 0));
		pp3.setPreBetAction(new BetAction(pp3.toString(), BetAction.betAction.CALL, 0));
		pp4.setPreBetAction(new BetAction(pp4.toString(), BetAction.betAction.CALL, 0));

		pp1.setPostBetAction(new BetAction(pp1.toString(), BetAction.betAction.FOLD, 0));
		pp2.setPostBetAction(new BetAction(pp2.toString(), BetAction.betAction.FOLD, 0));
		pp3.setPostBetAction(new BetAction(pp3.toString(), BetAction.betAction.FOLD, 0));
		pp4.setPostBetAction(new BetAction(pp4.toString(), BetAction.betAction.FOLD, 0));

		pp1.setScoreRevealAction(new ScoreRevealAction(pp1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	pp1.getHand()));
		pp2.setScoreRevealAction(new ScoreRevealAction(pp2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp2.getHand()));
		pp3.setScoreRevealAction(new ScoreRevealAction(pp3.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp3.getHand()));
		pp4.setScoreRevealAction(new ScoreRevealAction(pp4.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp4.getHand()));

		g=new GameModel();
		
		try
		{
			g.addPlayerToGame(pp1);
			g.addPlayerToGame(pp2);
			g.addPlayerToGame(pp3);
			g.addPlayerToGame(pp4);
			
			g.ExecuteSingleMatch();
			assertTrue(pp2.getPurse().getChips()==115);
		}
		catch(Exception e)
		{
			//should get here
			fail();
		}
	}

	/**
	 * Fold all the players in different rounds and see if the last player is forced
	 * to reveal their hand and win the pot.
	 *
	 */
	public void testFoldAllPlayers3()
	{
		DriverPlayer pp1, pp2, pp3, pp4;
		GameModel g;
		
		pp1=new DriverPlayer("David Kawrykow", 100);
		pp2=new DriverPlayer("G Toussaint", 100);
		pp3=new DriverPlayer("Gen Kazama", 100);
		pp4=new DriverPlayer("David Hasselhoff", 100);
		
		
		pp1.setPreBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPreBetAction(new BetAction(pp2.toString(), BetAction.betAction.CALL, 0));
		pp3.setPreBetAction(new BetAction(pp3.toString(), BetAction.betAction.CALL, 0));
		pp4.setPreBetAction(new BetAction(pp4.toString(), BetAction.betAction.RAISE, 10));

		pp1.setPostBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPostBetAction(new BetAction(pp2.toString(), BetAction.betAction.RAISE, 20));
		pp3.setPostBetAction(new BetAction(pp3.toString(), BetAction.betAction.FOLD, 0));
		pp4.setPostBetAction(new BetAction(pp4.toString(), BetAction.betAction.CALL, 0));

		// Every player folds, so the last one is forced to reveal hand and win the pot
		pp1.setScoreRevealAction(new ScoreRevealAction(pp1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.FOLD,
													  	pp1.getHand()));
		pp2.setScoreRevealAction(new ScoreRevealAction(pp2.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp2.getHand()));
		pp3.setScoreRevealAction(new ScoreRevealAction(pp3.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp3.getHand()));
		pp4.setScoreRevealAction(new ScoreRevealAction(pp4.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp4.getHand()));

		g=new GameModel();
		
		try
		{
			g.addPlayerToGame(pp1);
			g.addPlayerToGame(pp2);
			g.addPlayerToGame(pp3);
			g.addPlayerToGame(pp4);
	
			g.ExecuteSingleMatch();
			System.out.println(pp2.getPurse().getChips());
			assertTrue(pp2.getPurse().getChips()==185);
		}
		catch(Exception e)
		{
			// Should not reach here
			fail();
		}
	}
	
	/**
	 * Fold in multiple rounds and see if last player is forced to reveal hand
	 * and pick up the pot.
	 *
	 */
	public void testFoldAllPlayers4()
	{
		DriverPlayer pp1, pp2, pp3, pp4;
		GameModel g;
		
		pp1=new DriverPlayer("David Kawrykow", 100);
		pp2=new DriverPlayer("G Toussaint", 100);
		pp3=new DriverPlayer("Gen Kazama", 100);
		pp4=new DriverPlayer("David Hasselhoff", 100);
		
		pp1.setPreBetAction(new BetAction(pp1.toString(), BetAction.betAction.FOLD, 0));
		pp2.setPreBetAction(new BetAction(pp2.toString(), BetAction.betAction.CALL, 0));
		pp3.setPreBetAction(new BetAction(pp3.toString(), BetAction.betAction.CALL, 0));
		pp4.setPreBetAction(new BetAction(pp4.toString(), BetAction.betAction.RAISE, 10));

		pp1.setPostBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPostBetAction(new BetAction(pp2.toString(), BetAction.betAction.RAISE, 20));
		pp3.setPostBetAction(new BetAction(pp3.toString(), BetAction.betAction.FOLD, 0));
		pp4.setPostBetAction(new BetAction(pp4.toString(), BetAction.betAction.CALL, 0));

		// Even though player 2 folds, he is the last one to fold so he should get all the money
		pp1.setScoreRevealAction(new ScoreRevealAction(pp1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.FOLD,
													  	pp1.getHand()));
		pp2.setScoreRevealAction(new ScoreRevealAction(pp2.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp2.getHand()));
		pp3.setScoreRevealAction(new ScoreRevealAction(pp3.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp3.getHand()));
		pp4.setScoreRevealAction(new ScoreRevealAction(pp4.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp4.getHand()));

		g=new GameModel();
		
		try{
			g.addPlayerToGame(pp1);
			g.addPlayerToGame(pp2);
			g.addPlayerToGame(pp3);
			g.addPlayerToGame(pp4);
			
			g.ExecuteSingleMatch();
			assertTrue(pp2.getPurse().getChips()==155);
		}
		catch(Exception e)
		{
			//should get here
			fail();
		}
	}
	
	/**
	 * Created at 5:25 am
	 *
	 */
	public void testCrazyMorningScenario()
	{
		DriverPlayer pp1, pp2, pp3, pp4;
		GameModel g;
		
		pp1=new DriverPlayer("David Kawrykow", 35);		//10 left
		pp2=new DriverPlayer("G Toussaint", 30);
		pp3=new DriverPlayer("Gen Kazama", 10);			//0 left
		pp4=new DriverPlayer("David Hasselhoff", 35); 	//10 left
		
		//Everyone folds
		pp1.setPreBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPreBetAction(new BetAction(pp2.toString(), BetAction.betAction.ALL_IN, 25));
		pp3.setPreBetAction(new BetAction(pp3.toString(), BetAction.betAction.ALL_IN, 5));
		pp4.setPreBetAction(new BetAction(pp4.toString(), BetAction.betAction.RAISE, 20));

		// The following actions don't matter (since everyone folded).
		pp1.setPostBetAction(new BetAction(pp1.toString(), BetAction.betAction.FOLD, 0));
		pp2.setPostBetAction(new BetAction(pp2.toString(), BetAction.betAction.FOLD, 0));
		pp3.setPostBetAction(new BetAction(pp3.toString(), BetAction.betAction.CALL, 0));
		pp4.setPostBetAction(new BetAction(pp4.toString(), BetAction.betAction.FOLD, 0));

		pp1.setScoreRevealAction(new ScoreRevealAction(pp1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	pp1.getHand()));
		pp2.setScoreRevealAction(new ScoreRevealAction(pp2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp2.getHand()));
		pp3.setScoreRevealAction(new ScoreRevealAction(pp3.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp3.getHand()));
		pp4.setScoreRevealAction(new ScoreRevealAction(pp4.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp4.getHand()));

		g=new GameModel();
		
		try
		{
			g.addPlayerToGame(pp1);
			g.addPlayerToGame(pp2);
			g.addPlayerToGame(pp3);
			g.addPlayerToGame(pp4);
			
			g.ExecuteSingleMatch();
			assertTrue(pp2.getPurse().getChips()>=60);
		}
		catch(Exception e)
		{
			//should get here
			fail();
		}
	}
	
	
	
	/**
	 * Four side pots are created and the highest stack (Hasselhoff) folds even though no is
	 * facing him in his side pot.  Therefore, he loses 15 chips he put in above everyone else's
	 * bets.
	 *
	 */
	public void testFourSidePots()
	{
		DriverPlayer pp1, pp2, pp3, pp4;
		GameModel g;
		
		pp1=new DriverPlayer("David Kawrykow", 50);
		pp2=new DriverPlayer("G Toussaint", 60);
		pp3=new DriverPlayer("Gen Kazama", 70);
		pp4=new DriverPlayer("David Hasselhoff", 100);
		
		// Player 4 raises over everyone's all-in (not a very smart move, but possible)
		pp1.setPreBetAction(new BetAction(pp1.toString(), BetAction.betAction.ALL_IN, 45));
		pp2.setPreBetAction(new BetAction(pp2.toString(), BetAction.betAction.ALL_IN, 55));
		pp3.setPreBetAction(new BetAction(pp3.toString(), BetAction.betAction.ALL_IN, 65));
		pp4.setPreBetAction(new BetAction(pp4.toString(), BetAction.betAction.RAISE, 15));

		// The following actions dno't matter (since everyone is done).
		pp1.setPostBetAction(new BetAction(pp1.toString(), BetAction.betAction.CALL, 0));
		pp2.setPostBetAction(new BetAction(pp2.toString(), BetAction.betAction.CALL, 0));
		pp3.setPostBetAction(new BetAction(pp3.toString(), BetAction.betAction.CALL, 0));
		pp4.setPostBetAction(new BetAction(pp4.toString(), BetAction.betAction.CALL, 0));

		// Player 4 folds even though he will leave 15 chips in the pot.  Very unintelligent (but possible)
		pp1.setScoreRevealAction(new ScoreRevealAction(pp1.toString(), 
													  	ScoreRevealAction.scoreRevealAction.SHOW,
													  	pp1.getHand()));
		pp2.setScoreRevealAction(new ScoreRevealAction(pp2.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp2.getHand()));
		pp3.setScoreRevealAction(new ScoreRevealAction(pp3.toString(), 
														ScoreRevealAction.scoreRevealAction.SHOW,
														pp3.getHand()));
		pp4.setScoreRevealAction(new ScoreRevealAction(pp4.toString(), 
														ScoreRevealAction.scoreRevealAction.FOLD,
														pp4.getHand()));

		g=new GameModel();
		
		try
		{
			g.addPlayerToGame(pp1);
			g.addPlayerToGame(pp2);
			g.addPlayerToGame(pp3);
			g.addPlayerToGame(pp4);
			
			g.ExecuteSingleMatch();
			//Player 4 has 15 chips left in his purse
			assertTrue(pp4.getPurse().getChips()==30);
		}
		catch(Exception e)
		{
			e.printStackTrace();
			//should not get here
			fail();
		}
	}
	
	public void testLoadFile()
	{
		String ss       = java.io.File.separator;
		String pathName = "src" + ss + "game" + ss + "test" + ss + "PokerFighter_AutoSave_File2.txt";
		
		GameFileParser tGameFileParser = new GameFileParser(pathName);
		
		assertTrue(tGameFileParser.isCorrupted() == false);
		
		try
		{
			GameModel tGameModel = new GameModel(GameModel.LastMatch, tGameFileParser.iterator(), new GamePot(4));
			ArrayList<Player> tGamePlayers = tGameModel.getLoadedPlayers();
			assertTrue(tGamePlayers.size() == 4);
			
			assertTrue(tGamePlayers.get(0).getPurse().getChips() == 301);
			assertTrue(tGamePlayers.get(0).toString().equals("Mike Tyson-n"));
			assertTrue(tGamePlayers.get(1).getPurse().getChips() == 182);
			assertTrue(tGamePlayers.get(1).toString().equals("Mike Tyson-e"));
			assertTrue(tGamePlayers.get(2).getPurse().getChips() == 167);
			assertTrue(tGamePlayers.get(2).toString().equals("Mike Tyson-j"));
			assertTrue(tGamePlayers.get(3).getPurse().getChips() == 150);
			assertTrue(tGamePlayers.get(3).toString().equals("Mike Tyson-w"));
			
			// this is the dealer for the last match to finish
			assertTrue(tGameModel.getDealer().toString().equals("Mike Tyson-n"));
			
			// this should execute without a hitch
			while (tGameModel.isGameOver() == false)
			{
				tGameModel.ExecuteSingleMatch();
			}
			
			Player Winner = tGameModel.getGameWinner();
			
			int i;
			
			for(i = 0; i < tGamePlayers.size(); i++)
			{
				if(Winner.toString().equals(tGamePlayers.get(i).toString()))
					break;
			}
			
			assertTrue(i != tGamePlayers.size());
		}
		catch (PurseIsEmptyException e)
		{
			assertTrue(false);
		}
		catch (IOException e)
		{
			assertTrue(false);
		}
		catch (NoDealerException e)
		{
			assertTrue(false);
		}
		catch (Exception e)
		{
			assertTrue(false);
		}
	}
	
	public void testLoadFile2()
	{
		String ss       = java.io.File.separator;
		String pathName = "src" + ss + "game" + ss + "test" + ss + "PokerFighter_AutoSaver_File3.txt";
		
		GameFileParser tGameFileParser = new GameFileParser(pathName);
		
		assertTrue(tGameFileParser.isCorrupted() == false);
		
		try
		{
			GameModel tGameModel = new GameModel(GameModel.LastMatch, tGameFileParser.iterator(), new GamePot(4));
			ArrayList<Player> tGamePlayers = tGameModel.getLoadedPlayers();
			assertTrue(tGamePlayers.size() == 4);
			
			// marx should be different than in the previous test
			assertTrue(tGamePlayers.get(0).getPurse().getChips() == 302);
			assertTrue(tGamePlayers.get(0).toString().equals("Mike Tyson-n"));
			assertTrue(tGamePlayers.get(1).getPurse().getChips() == 182);
			assertTrue(tGamePlayers.get(1).toString().equals("Mike Tyson-e"));
			assertTrue(tGamePlayers.get(2).getPurse().getChips() == 167);
			assertTrue(tGamePlayers.get(2).toString().equals("Mike Tyson-j"));
			assertTrue(tGamePlayers.get(3).getPurse().getChips() == 150);
			assertTrue(tGamePlayers.get(3).toString().equals("Mike Tyson-w"));
			
			// this is the dealer for the last match to finish
			assertTrue(tGameModel.getDealer().toString().equals("Mike Tyson-e"));
			
			// this should execute without a hitch
			while (tGameModel.isGameOver() == false)
			{
				tGameModel.ExecuteSingleMatch();
				
				System.out.println(tGamePlayers.get(0).getPurse());
				System.out.println(tGamePlayers.get(1).getPurse());
				System.out.println(tGamePlayers.get(2).getPurse());
				System.out.println(tGamePlayers.get(3).getPurse());
			}
			
			Player Winner = tGameModel.getGameWinner();
			
			int i;
			
			for(i = 0; i < tGamePlayers.size(); i++)
			{
				if(Winner.toString().equals(tGamePlayers.get(i).toString()))
					break;
			}
			
			assertTrue(i != tGamePlayers.size());
		}
		catch (PurseIsEmptyException e)
		{
			assertTrue(false);
		}
		catch (IOException e)
		{
			assertTrue(false);
		}
		catch (NoDealerException e)
		{
			assertTrue(false);
		}
		catch (Exception e)
		{
			assertTrue(false);
		}
	}
	
	public void testLoadFile3()
	{
		String ss       = java.io.File.separator;
		String pathName = "src" + ss + "game" + ss + "test" + ss + "PokerFighter_AutoSaver_File3.txt";
		
		GameFileParser tGameFileParser = new GameFileParser(pathName);
		
		assertTrue(tGameFileParser.isCorrupted() == false);
		
		try
		{
			GameModel tGameModel = new GameModel(2, tGameFileParser.iterator(), new GamePot(4));
			ArrayList<Player> tGamePlayers = tGameModel.getLoadedPlayers();
			assertTrue(tGamePlayers.size() == 4);
			
			assertTrue(tGamePlayers.get(0).getPurse().getChips() == 301);
			assertTrue(tGamePlayers.get(0).toString().equals("Mike Tyson-n"));
			assertTrue(tGamePlayers.get(1).getPurse().getChips() == 182);
			assertTrue(tGamePlayers.get(1).toString().equals("Mike Tyson-e"));
			assertTrue(tGamePlayers.get(2).getPurse().getChips() == 167);
			assertTrue(tGamePlayers.get(2).toString().equals("Mike Tyson-j"));
			assertTrue(tGamePlayers.get(3).getPurse().getChips() == 150);
			assertTrue(tGamePlayers.get(3).toString().equals("Mike Tyson-w"));
			
			// this is the dealer for the last match to finish
			assertTrue(tGameModel.getDealer().toString().equals("Mike Tyson-n"));
			
			// this should execute without a hitch
			while (tGameModel.isGameOver() == false)
			{
				tGameModel.ExecuteSingleMatch();
			}
			
			Player Winner = tGameModel.getGameWinner();
			
			int i;
			
			for(i = 0; i < tGamePlayers.size(); i++)
			{
				if(Winner.toString().equals(tGamePlayers.get(i).toString()))
					break;
			}
			
			assertTrue(i != tGamePlayers.size());
		}
		catch (PurseIsEmptyException e)
		{
			assertTrue(false);
		}
		catch (IOException e)
		{
			assertTrue(false);
		}
		catch (NoDealerException e)
		{
			assertTrue(false);
		}
		catch (Exception e)
		{
			assertTrue(false);
		}
	}
	
	/**
	 * This method is used to allow the test case to listen to events spit out by
	 * the GameModel. It verifies that no new matches can be started while a match
	 * is in progress. It verifies too that the action received matches up with the
	 * one expected by the authors.  
	 */
	public void notify(Action action)
	{
		Player actionMaker;
		
		if(action.getActionMaker().equals( "Toussaint"))
			actionMaker = p1;
		else if(action.getActionMaker().equals("David Hasselhoff"))
			actionMaker = p2;
		else if(action.getActionMaker().equals("KITT"))
			actionMaker = p3;
		else if(action.getActionMaker().equals("KARR"))
			actionMaker = p4;
		else
			actionMaker = null;
		
		// check if we can start a new match
		try
		{
			gameModel.ExecuteSingleMatch();		// this should not be allowed here
			fail();
		}
		catch (MatchAlreadyInSessionException e)
		{
			//should get here
		}
		catch (Exception e)
		{
			fail();
		}
		
		try
		{
			if( action instanceof CardExchangeAction)
			{
				assertTrue( ((CardExchangeAction) action).getAmount()==1);
				gs.addCardExchangeAction((CardExchangeAction) action);
			}
			else if (action instanceof IncrementAnteAction)
			{
				if( ( (IncrementAnteAction) action).getAction() 
						== IncrementAnteAction.incrementAnteAction.SUCCEED)
				{	
					if(action.getActionMaker() != "The Reaper")
						assertTrue( (counter % 5) == 0);
				}
				
				gs.addIncrementAnteAction((IncrementAnteAction) action);
			}
			else if(action instanceof BetAction)
			{
				BetAction myAction = (BetAction) action;
				
				if( myAction.getAction() == BetAction.betAction.RAISE )
				{
					assertTrue(myAction.getAmount() == 5);
				}
				else if (myAction.getAction() == BetAction.betAction.CALL)
				{
					assertTrue(myAction.getAmount() == 0);
				}
				else if( myAction.getAction() == BetAction.betAction.ALL_IN)
				{
					assertTrue(myAction.getAmount() == actionMaker.getPurse().getChips());
				}
			}
			else if(action instanceof NewGameAction)
			{
				gs.addNewGameAction((NewGameAction) action);
			}
			else if(action instanceof NewMatchAction)
			{ 
				gs.addNewMatchAction((NewMatchAction) action);
				counter++;	// increment the ante counter to check ante increments
			}
			else if(action instanceof PlayerRemovedAction)
			{
				gs.addPlayerRemovedAction((PlayerRemovedAction) action);
				assertTrue(gameModel.getAnte() > actionMaker.getPurse().getChips());
			}
			
			else if(action instanceof ScoreRevealAction)
			{
				assertTrue( ((ScoreRevealAction) action).getAction() == 
							ScoreRevealAction.scoreRevealAction.SHOW);
				gs.addScoreRevealAction((ScoreRevealAction) action);
			}
			else if(action instanceof GetMoneyAction)
			{
				/**
				 * This cannot be tested because any number of players may tie so 
				 * the amount changes each time. 
				 */
				gs.addGetMoneyAction((GetMoneyAction) action);
			}
			else if(action instanceof ScoreAction)
			{
				assertTrue(actionMaker.getPurse().getChips() == 
							((ScoreAction)action).getAmount());
				gs.addScoreAction((ScoreAction) action);
			}
		} 
		catch (Exception e)
		{fail();} 
	}
}
