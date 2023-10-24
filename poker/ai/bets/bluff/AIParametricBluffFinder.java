package ai.bets.bluff;

import scoring.HandValuator;
import scoring.HandValue;
import game.actions.*;
import game.exceptions.*;

import java.util.ArrayList;

import ai.AIBluffFinder;
import ai.util.*;
import util.*;


public class AIParametricBluffFinder extends AINumberGenerator implements AIBluffFinder {

	// maximum memory size
	private final int MAX_SIZE_MEMORY; 
	
	// maximum bluff certainty
	private final int MAX_BLUFF_CERTAINTY; 
	
	// minimum proportion of purse to assume bluffing 
	private final int MIN_PURSE_PROPORTION;
	
	// counts how many matches have been COMPLETED
	private int matchesCompleted = -1; 
	
	// Some booleans used to remember action types
	private boolean freshScores; 
	
	// Names of the opponents
	private ArrayList<String> myOpponents;
	
	// The opponent bluff history 
	private ArrayList<ArrayList<Boolean>> opponentBluffHistory;
	
	// Keeps tabs on current opponent bets
	private ArrayList<Integer> opponentBets;
	
	// Keeps tabs on opponent purse content at beginning of match
	private ArrayList<Integer> opponentScores;
	
	// Keep tabs on opponent score reveal actions
	private ArrayList<ScoreRevealAction> opponentScoreRevealActions; 
	
	// An int used to remember the pot money 
	private int potMax = 0; 
	
	/**
	 * The constructor for an AIBluffFinder
	 * @param Seed Sets the seed for the random aspect of bluff detection
	 * @param MemorySize Sets the number of rounds the bluff memory can retain 
	 * @param BluffCutoff Sets the maximum certainty that a player is bluffing
	 * regardless of history of that player. (seen as %)
	 * @pre MemorySize > 0 
	 * @pre BluffCutoff [1, 100] 
	 */
	public AIParametricBluffFinder(int MemorySize, int BluffCutoff, int Seed)
	{
		// initialize the random number generator
		super(Seed); 
		
		// Verify correctness of memory and confidence parameters
		assert (MemorySize > 0);
		assert (BluffCutoff > 0); 
		assert (BluffCutoff < getMaximumFeasible() ); 
		
		// Initialize memory and confidence parameters
		MAX_SIZE_MEMORY = MemorySize;
		MAX_BLUFF_CERTAINTY = BluffCutoff; 
		MIN_PURSE_PROPORTION = 5; 				
		
		// Memory Setup
		myOpponents 	= new ArrayList<String>(); 
		opponentBets 	= new ArrayList<Integer>();
		opponentScores 	= new ArrayList<Integer>();
		opponentScoreRevealActions = 
			new ArrayList<ScoreRevealAction>();
		opponentBluffHistory = 
			new ArrayList<ArrayList<Boolean>>();
		
	}

	/**
	 * Keeps tabs on relevant aspect of the game i.e. bets, scores, bluffs
	 */
	public void updateGameHistory(Action action)
	{	
		// New Match
		if(action instanceof NewMatchAction)
		{ 
			freshScores=true;
			potMax = 0;
			matchesCompleted++;
			if(matchesCompleted > 0)
				updateBluffingMatrix();
		}

		// Bets
		else if(action instanceof BetAction)
		{
			BetAction lAction = (BetAction) action;
			freshScores=false;
			if (lAction.getAction() == BetAction.betAction.FOLD)
			{
				setOpponentScoreRevealAction(new ScoreRevealAction(
						lAction.getActionMaker(), 
						ScoreRevealAction.scoreRevealAction.FOLD, 
						new Hand()));
			}
			incrementOpponentWager(lAction);
		}
	
		// Reveal Score
		else if(action instanceof ScoreRevealAction)
		{
			potMax = 0; 
			setOpponentScoreRevealAction((ScoreRevealAction) action);
		}
		
		// Remove player
		else if(action instanceof PlayerRemovedAction)
			removePlayer((PlayerRemovedAction)action);
		
		// Update score
		else if(action instanceof ScoreAction)
		{
			if(freshScores)
				setOpponentMatchPurse((ScoreAction)action);
		}

		// Indicate start of game
		else if(action instanceof NewGameAction)
		{
			refreshComponents(); 
			learnOpponentNames((NewGameAction)action);
			matchesCompleted = -1;
		}
		
		else
			return; 
	}
	
	/**
	 * This method determines whether or not the current player is
	 * bluffing. It returns a nice and easy to understand boolean saying
	 * "yes" or "no"
	 */
	public boolean isPlayerBluffing(String player)
	{
		int z; 
		
		// We have no history yet, so always return false
		if ( matchesCompleted  < 1 )
			return false; 
		
		// Our initial confidence will say he bluffs like he spends
		int bluffConfidence = MIN_PURSE_PROPORTION; 
		
		// Compute the index of the input player
		int i = myOpponents.indexOf(player); 

		// compute the proportion of initial purse that player has already bet 
		int t = (opponentBets.get(i).intValue()*100)/ (opponentScores.get(i).intValue()+1); 
		
		// if he hasn't bet very much, who cares, we will engage him anyway
		if( t < MIN_PURSE_PROPORTION )
			return false;
		
		// otherwise, if he's recently bluffed, he's probably bluffing again 
		if( hasPlayerBluffed(player) )
			bluffConfidence += MAX_BLUFF_CERTAINTY;

		z = getNextInteger(); 
		
		// We feel bluffConfidence confident based on history
		// but our random instinct will override this inversely proportional 
		// to how confident we are
		if( bluffConfidence >= z )
		{
			return true; 
		}
		else
		{
			return false;
		}
	}
		
	private void removePlayer(PlayerRemovedAction action)
	{
		int i = myOpponents.indexOf(action.getActionMaker());
		if(i>=0)
		{
			myOpponents.remove(i);
			opponentBluffHistory.remove(i);
			opponentBets.remove(i);
			opponentScores.remove(i);
		}
	}
	
	/**
	 * This method increments the opponent's current pot contribution
	 */
	private void incrementOpponentWager(BetAction action)
	{
		int a, i = myOpponents.indexOf(action.getActionMaker());
		
		if(action.getAction() == BetAction.betAction.FOLD ||
		   action.getAction() == BetAction.betAction.CHECK)
			return; 
		
		else if (action.getAction() == BetAction.betAction.CALL )
			a = potMax; 
		
		else if (action.getAction() == BetAction.betAction.ALL_IN)
			a = opponentScores.get(i);
		
		else
		{
			a = potMax + action.getAmount();
			potMax += action.getAmount(); 
		}
		
		opponentBets.set(i, new Integer(a));
 
	}
	
	/**
	 * This method sets the input opponent's latest known purse value
	 * for bluff computation
	 */
	private void setOpponentMatchPurse(ScoreAction action)
	{
		int i = myOpponents.indexOf(action.getActionMaker()); 	
		opponentScores.set(i, new Integer(action.getAmount()));
	}
	
	/**
	 * This method sets the input opponent's latest known scoreRevealAction
	 * for bluff computation
	 */
	private void setOpponentScoreRevealAction(ScoreRevealAction action)
	{
		int i = myOpponents.indexOf(action.getActionMaker()); 
		opponentScoreRevealActions.set(i, action); 
	}
	
	/** 
	 * Returns true whether the indicated player has recently bluffed
	 * @pre pName is valid
	 */
	private boolean hasPlayerBluffed(String pName)
	{
		int i = myOpponents.indexOf(pName);
		
		for(int s = 0; s < opponentBluffHistory.get(i).size(); s++)
			if( opponentBluffHistory.get(i).get(s).booleanValue() )
				return true;  
		
		return false;
	}
	
	/**
	 * Forgets the older history and combines recent history with 
	 * the last match. Is called whenever a new match action occurs
	 */
	private void updateBluffingMatrix()
	{
		boolean q; 
		
		// forget the longest-ago bluff stats
		forgetAncientBluffHistory();
		
		// remember player's bluffs from last match
		for(int i = 0; i < myOpponents.size(); i++)
		{
			q = didPlayerBluffLastMatch(myOpponents.get(i));
			opponentBluffHistory.get(i).add( new Boolean(q) );
			opponentBets.set(i, new Integer(0));
			opponentScores.set(i, new Integer(0)); 
		}
			
	}
	
	/**
	 * Shaves off the oldest bluffing values so they no longer interfere
	 * with what we think now and thought more recently
	 */
	private void forgetAncientBluffHistory()
	{ 
		
		if ( opponentBluffHistory.get(0).size() == MAX_SIZE_MEMORY )
			for(int i = 0; i < opponentBluffHistory.size(); i++)
				opponentBluffHistory.get(i).remove(0);
	}
	
	/**
	 * This method computes whether or not the player indicated bluffed in the 
	 * round just expired. It should be called immediately after a newMatchAction
	 */
	private boolean didPlayerBluffLastMatch(String playerName)
	{	
		int i = myOpponents.indexOf(playerName); 
		int bet = 0;
		int init = 1;
		HandValuator x = new HandValuator();
		HandValue.Category y; 
		ScoreRevealAction a;
		
		// Get the total money the player bet
		bet = opponentBets.get(i);		
		
		// Get the money the player started out with that match
		init = opponentScores.get(i);
		init++;
	
		// Determine what the player did with his cards
		a = opponentScoreRevealActions.get(i); 
		
		// Then judge based on this action, the cards, and the amount of money involved
		try
		{
			if(a == null)
				return false;
			if(a.getHand() == null)
				return false;
			
			y = x.valuateHand(a.getHand()).getCategory();  
			if
			( y.compareTo(HandValue.Category.PAIR) <= 0 
			  && (bet*100)/init >= MIN_PURSE_PROPORTION 
			)
			{
				return true;
			}
			else
				return false; 
		} 
		catch (HandFoldedException e)
		{
			return false; 
		}
			
	}
	
	/**
	 * Refreshes the internal array lists to begin a new match
	 */
	private void refreshComponents()
	{
		myOpponents.clear();
		opponentBluffHistory.clear(); 
		opponentScores.clear(); 
		opponentBets.clear();
		opponentScoreRevealActions.clear(); 
	}
	
	/**
	 * This methods handles the NewGameAction. It stores all players names
	 * and sets their bluffing records clean.  
	 */
	private void learnOpponentNames(NewGameAction action)
	{		
		for(int i = 0; i < action.getNumberOfParticipants(); i++)
		{
			myOpponents.add(action.getParticipant(i+1));
			opponentBluffHistory.add(new ArrayList<Boolean>());
			opponentBluffHistory.get(i).add(new Boolean(false));
			opponentScores.add(new Integer(0));
			opponentBets.add(new Integer(0));
			opponentScoreRevealActions.add(null); 
		}
	}
	
}


