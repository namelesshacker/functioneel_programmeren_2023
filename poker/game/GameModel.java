package game;
import game.actions.*;
import game.exceptions.*;

import java.io.IOException;
import java.util.Iterator;
import java.util.ArrayList;

import money.GamePot;

import players.Constants;
import players.Player;
import util.*;
import scoring.*;

/**
 * GameModel 
 * @author David Kawrykow and Gen Kazama
 * 
 * <br>This class models one poker game. 
 * 
 * WHY THIS CLASS IS SO MASSIVE
 * 
 * <br>Consider the following very special cases:
 * 
 * <li> side pots (up to four if each player goes all-in)
 * <li> all players folding before the showdown
 * <li> all players folding before the showdown with side pots present 
 * <li> all players in a side pot folding at any point in the match
 * <li> all players going all in in some order and folding in another
 * 		order at some further point in the game. 
 * <li> players folding during the showdown when they are the last player alive
 * <li> players updating their purses or hands when called to make an action
 * <li> players updating their purses or hands when notified of an action
 * <li> observers attempting to call methods on the current instance of game model
 * 		before a match has ended i.e. in your notification code calling ExecuteSingleMatch
 * <li> players updating their names when control is transferred 
 * <li> players betting in such a way as to not be able to make ante the next round
 * <li> all but one player not being able to make ante 
 * <li> after a player goes all in, that player tries to do something else
 * <li> no more bets possible (say check, all-in, call, in a two player game) before
 * 		cards were exchanged 
 * <li> tie(s) within side pot(s) (up to 3) 
 * <li> the integer remainder when split money does not divide
 * <li> a pot disappears if all players in it FOLD before the showdown, but the last
 * 		FOLDing player gets the pot contents (withdraws)
 * <li> a pot disappears like above but the last FOLDing player does so in the showdown
 * 		and thus gets none of the pot. 
 * <li> players return three cards, but specify the same card three times! 
 * 
 */
public class GameModel implements GameSubject
{
	
	/* The winner of the game . Null to start with */
	private Player GameWinner;
	
	/* A boolean indicating whether or not the Game represented by GameModel 
	 * has terminated.
	 */
	private boolean isGameOver;
	
	/* A boolean indicating whether or not a match is currently in progress */
	private boolean isMatchInProgress;
	
	/* The number of players starting the game */
	private final int NUMBER_OF_PLAYERS = 4;
	
	/* Maximum number of cards to discard */
	private final int NUMBER_OF_DISCARD = 3;
	
	/* Ante increment percentage and how often */
	private final float ANTE_INCREMENT_PERCENTAGE = (float)0.2;
	private final int   ANTE_INCREMENT_ROUND = 5;
	
	/* Hand Size */
	private final int HAND_SIZE = 5;
	
	/* Player seat ordering for entire poker game */
	private GameContainer players;
	
	/* Player seat ordering for current match */
	private ArrayList<PlayerWrapper> MatchPlayers;
	
	/* Current ante size */
	private int ante;
	
	/* The deck */
	private Deck deck;
	
	/* The pot for one match/round */
	private GamePot pot;
	
	/* The scorecard visible to all players, GUI and XML File Writer */
	private GameStats gameStats;
	
	/* The PlayerComparator to check if Players cheat (change their name, purse, etc) */
	private PlayerComparator playerComparator;
	
	/* Number of players folded in a match */
	private int matchNumPlayersFolded;
	
	/* Number of matches played so far */
	private int matchesPlayed;
	
	/* Has the game been initialized to four players */
	private boolean hasReachedFourPlayers = false;
	
	/* Loaded players array list used to get the players found in a loaded file */
	private ArrayList<Player> mLoadedPlayers;
	
	/* History of the loaded game */
	private Action[] mLoadedActions;
	
	/* Load the "Last Match" */
	public static final int LastMatch = -1;
	
	private int tournamentRound =-1;
	private int difficultySetting = 2;
	
	/**
	 * Default constructor for console based versions. Here the pot is simply kept internally. 
	 */
	public GameModel()
	{
		pot     = new GamePot(NUMBER_OF_PLAYERS);
		
		init();
	}
	
	/**
	 * Default gui constructor. 
	 * @param pot Must be accessible by gui to witness changes in detail. 
	 */
	public GameModel(GamePot pot)
	{
		assert (pot != null); 
		
		this.pot = pot;
		
		init();
	}
	
	/**
	 * Loads a game model from a file turned Iterator<Action>. The pot must also come
	 * from an external source. 
	 * @param MatchNumber 	Specifies from which match number in the file we wish to resume playing. 
	 * @param mIterator  	A clean way to traverse the actions in the file using an iterator
	 * @param pot			An external pot to be used by game model. 
	 * @throws IOException	Whenever any flaw in the file is detected. 
	 */
	public GameModel(int MatchNumber, Iterator<Action> mIterator, GamePot pot) throws IOException
	{		
		try
		{
			/* initialize */
			init();
			
			/* Current temporary action */
			Action tAction;
			
			/* Last complete match is stored in here temporarily */
			ArrayList<Action> tCompleteMatch = new ArrayList<Action>();
			
			/* Have we received a new Game Action */
			boolean noNewGameActionYet = true;
			
			while( mIterator.hasNext() )
			{
				tAction = mIterator.next();
				
				/* Adds all the players from game action */
				if(tAction instanceof NewGameAction)
				{
					if(noNewGameActionYet == false)
						throw new IOException();
					
					handleNewGameAction((NewGameAction) tAction, pot);
					gameStats.addAction(tAction);
					noNewGameActionYet = false;
				}
				
				/* Checks if we have reached the target match. 
				 * If yes, break */
				if(tAction instanceof NewMatchAction)
				{
					if(reachedTerminationMatch((NewMatchAction) tAction, MatchNumber))
					{
						tCompleteMatch.add(tAction);
						break;
					}
				}
				
				tCompleteMatch.add(tAction);
				
				/* Check if the game is now over ... (i.e. someone loaded a finished game) */
				if(players.size() == 1)
				{
					win((Player) players.next());
					return;
				}
			}
			
			/* We have run out of strings in the file or reached the target match
			 * Time to use the last complete match to set up the new match
			 */
			loadNextMatchFromLastMatch(tCompleteMatch);
			
		}
		catch (Exception e)
		{
			e.printStackTrace();
			throw new IOException();
		}
		
	}
	
	/**
	 * Returns an array list of the players loaded from an input file
	 * or an empty array if no such file was loaded i.e. we only bother
	 * putting players in this array if these were loaded from a file. Otherwise
	 * the players were added externally and are thus known.  
	 */
	public ArrayList<Player> getLoadedPlayers()
	{
		return mLoadedPlayers;
	}
	
	/**
	 * Swaps the two players. Provides a clean way to switch players mid game. 
	 * Note that the LoadedPlayer array is now cleared and getLoadedPlayers
	 * will return an empty array (because presumably you know who is playing
	 * if you can switch players in and out)
	 */
	public void swapLoadedPlayers(Player RemovePlayer, Player AddPlayer)
	{
		String toRemove = RemovePlayer.toString();
		Player tSwap; 
		
		// attach the added player as a game listener
		this.attachGameListener(AddPlayer);
		
		// remove the removed player as a game listener
		this.detachGameListener(RemovePlayer);
		
		// remove the removed player from the player comparator
		playerComparator.removePlayer(RemovePlayer);
		
		// attach the added player to the player comparator
		playerComparator.addPlayer(AddPlayer);
		
		// swap players in the game container. 
		players.swapPlayers(RemovePlayer, AddPlayer);
	}
	
	/**
	 * Returns the new game action which started the game. This operation is only
	 * supported if the game was loaded from the file. Otherwise GameModel has sent
	 * out the new GameAction already. 
	 * @return null if the game was not loaded from a file. 
	 */
	public NewGameAction getNewGameAction()
	{
		if(mLoadedActions.length > 0)
			return (NewGameAction) mLoadedActions[0];
		
		return null; 
	}
	
	/**
	 * Common constructor code for all constructors. 
	 */
	private void init()
	{
		ante    = 5;
		players = new GameContainer();
		gameStats = new GameStats();
		MatchPlayers = new ArrayList<PlayerWrapper>();
		GameWinner = null;
		isGameOver = false;
		isMatchInProgress = false;
		playerComparator = new PlayerComparator();
		matchNumPlayersFolded = 0;
		matchesPlayed=0;
		mLoadedPlayers=new ArrayList<Player>();
		
		resetDeck();
	}
	
	/**
	 * Checks whether the indicated new match action is the match at which we wish 
	 * to stop (used for the loaded file constructor only)
	 */
	private boolean reachedTerminationMatch(NewMatchAction lAction, int targetMatchID)
	{
		if(targetMatchID == GameModel.LastMatch)
			return false;
		
		if(targetMatchID == lAction.getMatchID())
			return true;
		
		return false;
	}
	
	/**
	 * Loads the next match from the collected game history. 
	 * @param tCompleteMatch contains all previous collected actions. The last
	 * complete match is used to establish seating order in the match to be loaded. 
	 * IncrementAnteActions and PlayerRemovedActions found in the history are used
	 * to remove players as necessary. Used only for the load file constructor. 
	 */
	private void loadNextMatchFromLastMatch(ArrayList<Action> tCompleteMatch) throws IOException
	{
		int nScoreActions = 0;
		Player tPlayer;
		String tName;
		
		int matchesSeenBackwards = 0;
		int beg = 0, end = 0;
		
		Object[] gameHistory;
		Object[] lastMatch;
		
		// find the sub array within the history to work with as the last "official match"
		for(int i = tCompleteMatch.size() - 1; i > -1; i--)
		{
			if(tCompleteMatch.get(i) instanceof NewMatchAction)
			{
				matchesSeenBackwards ++;
				
				// the end counter
				if(matchesSeenBackwards == 1)
					end = i;
				if(matchesSeenBackwards == 2)
					beg = i;
			}
		}
		
		// if we saw nothing useful - exit. The game will start from scratch
		if(beg == 0 && end == 0)
			return;
		
		// this is everything we consider "history"
		gameHistory  = tCompleteMatch.subList(0, beg).toArray();
		
		// this is the last "complete match"
		lastMatch    = tCompleteMatch.subList(beg, end).toArray();
		
		// the joint size of these two elements is the size of the "watchable" game
		mLoadedActions = new Action[gameHistory.length + lastMatch.length];
		
		// increment matches played
		matchesPlayed++;
		
		// run through the history and note every player removed and increment ante
		// action. for every new match played, shift dealer too. add all actions
		// to the watchable history
		for(int i = 0; i < gameHistory.length; i++)
		{
			if (gameHistory[i] instanceof PlayerRemovedAction )
				handlePlayerRemovedAction( (PlayerRemovedAction) gameHistory[i] );
				
			if( gameHistory[i] instanceof IncrementAnteAction )
				ante = ((IncrementAnteAction) gameHistory[i]).getAmount();
			
			if( gameHistory[i] instanceof NewMatchAction )
			{
				setNextDealer();
				
				// Collect the player names in an array
				ArrayList<String> Participants = new ArrayList<String>();
				
				for( int j = 0; i < mLoadedPlayers.size(); i++)
					Participants.add(mLoadedPlayers.get(j).toString()); 
				
				// start a new game pot with same indexes as Loaded Players
				pot.reset(mLoadedPlayers.size(), Participants);
			}
			
			mLoadedActions[i] = (Action) gameHistory[i];
		}
		
		// collect the last valid match in the history. From this set the dealer
		for(int j = 0; j < lastMatch.length; j++)
		{
			if (lastMatch[j] instanceof PlayerRemovedAction )
				handlePlayerRemovedAction( (PlayerRemovedAction) lastMatch[j] );
				
			if( lastMatch[j] instanceof IncrementAnteAction )
			{
				ante = ((IncrementAnteAction) lastMatch[j]).getAmount();
				
				String aMaker  = ((IncrementAnteAction) lastMatch[j]).getActionMaker();
				Player bMaker  = null;
				
				for(int z = 0; z < mLoadedPlayers.size(); z++)
				{
					if(mLoadedPlayers.get(z) != null)
						if(mLoadedPlayers.get(z).toString().equals(aMaker))
							bMaker = mLoadedPlayers.get(z);
				}
				
				try
				{
					/* bMaker could be the Reaper :) */
					if(bMaker != null)
						players.setDealer(bMaker);
				}
				catch (NoDealerException e)
				{
					e.printStackTrace();
					throw new IOException();
				}
			}
			
			if( lastMatch[j] instanceof NewMatchAction )
			{
				// Collect the player names in an array
				ArrayList<String> Participants = new ArrayList<String>();
				
				for( int k = 0; k < mLoadedPlayers.size(); k++)
				{
					if(mLoadedPlayers.get(k) != null)
						Participants.add(mLoadedPlayers.get(k).toString());
				}
				
				// start a new game pot with same indexes as Loaded Players
				pot.reset(Participants.size(), Participants);
				
				// set the matches played
				matchesPlayed = ((NewMatchAction) lastMatch[j]).getMatchID();
			}
			
			mLoadedActions[j + gameHistory.length] = (Action) lastMatch[j];
		}
		
		// we now check whether any players were not removed when they should have been
		// e.g. if the PlayerRemovedAction was lost in the file or something
		ArrayList<Boolean> bPlayers = new ArrayList<Boolean>();
		
		int counter = 0;
		while(players.hasNext() && counter<players.size())
		{
			bPlayers.add(new Boolean(false));
			players.next();
			counter++;
		}
		
		// run through the last valid match backwards and extract the relevant
		// final score actions from the back for playing purposes
		for(int i = lastMatch.length - 1; i > -1; i--)
		{
			if( lastMatch[i] instanceof ScoreAction)
			{
				nScoreActions ++;
				tName = ((ScoreAction) lastMatch[i]).getActionMaker();
				
				int zz = 0;
				
				counter=0;
				while(players.hasNext() && counter<players.size())
				{
					tPlayer = (Player) players.next();
					
					// we have actually updated the player
					if(tPlayer.toString().equals(tName))
					{
						tPlayer.getPurse().setChips( 
								((ScoreAction) lastMatch[i]).getAmount());
							
						playerComparator.updatePlayer(tPlayer);
						bPlayers.set(zz, new Boolean(true));
					}
					
					zz ++;
					counter++;
				}
				
				// updated all player purses to latest
				if (nScoreActions == players.size())
				{
					updateAllPlayers();
					break;
				}
			}
			// as soon as we see a non-score action, break
			else
				break;
		}
		
		// now we check if we didn't see a score action for somebody: this means
		// they weren't part of the game anymore
		int zz = 0;
		while(players.hasNext())
		{
			Player ttPlayer = (Player) players.next();
			if(bPlayers.get(zz).booleanValue() == false)
			{
				players.remove();
				mLoadedPlayers.set(mLoadedPlayers.indexOf(ttPlayer), null);
			}
			
			zz ++;
		}
		
	}
	
	/**
	 * Loads all the players contained in the NewGameAction and saves them. This
	 * function is called in the load file constructor after the file spits out a newGameAction. 
	 * (it is not used anywhere else)
	 */
	private void handleNewGameAction(NewGameAction tAction, GamePot pot)
	{
		/* Temp player */
		Player tPlayer; 
		
		/* Check we have four players - this detects corrupt files */
		int numParticipants = (tAction).getNumberOfParticipants();
		
		assert (numParticipants == 4);
		
		/* Player information extracted from file */
		int    a1, a2, a3, a4;
		String p1, p2, p3, p4;
		String b1, b2, b3, b4;
		String s1, s2, s3, s4;
		
		assert (numParticipants == 4);
		
		p1 = (tAction).getParticipant(1);
		a1 = (tAction).getParticipantMoney(1);
		b1 = (tAction).getParticipantBigIcon(1);
		s1 = (tAction).getParticipantSmallIcon(1);
		
		tPlayer = Constants.getPlayer(p1, a1, b1, s1);
		
		addLoadedPlayerToGame(tPlayer);
		
		p2 = (tAction).getParticipant(2);
		a2 = (tAction).getParticipantMoney(2);
		b2 = (tAction).getParticipantBigIcon(2);
		s2 = (tAction).getParticipantSmallIcon(2);
		
		tPlayer = Constants.getPlayer(p2, a2, b2, s2);
		
		addLoadedPlayerToGame(tPlayer);
		
		p3 = (tAction).getParticipant(3);
		a3 = (tAction).getParticipantMoney(3);
		b3 = (tAction).getParticipantBigIcon(3);
		s3 = (tAction).getParticipantSmallIcon(3);
		
		tPlayer = Constants.getPlayer(p3, a3, b3, s3);
		
		addLoadedPlayerToGame(tPlayer);
		
		p4 = (tAction).getParticipant(4);
		a4 = (tAction).getParticipantMoney(4);
		b4 = (tAction).getParticipantBigIcon(4);
		s4 = (tAction).getParticipantSmallIcon(4);
		
		tPlayer = Constants.getPlayer(p4, a4, b4, s4);
		
		addLoadedPlayerToGame(Constants.getPlayer(p4, a4, b4, s4));
		
		hasReachedFourPlayers = true;
		
		ArrayList<String> names = new ArrayList<String>();
		
		names.add(mLoadedPlayers.get(0).toString());
		names.add(mLoadedPlayers.get(1).toString());
		names.add(mLoadedPlayers.get(2).toString());
		names.add(mLoadedPlayers.get(3).toString());
		
		this.pot = pot;
		this.tournamentRound = tAction.getTournamentRound();
		this.difficultySetting = tAction.getDifficultySetting();
	}
	
	/**
	 * Handles playerRemovedActions when these are seen in the load file constructor.
	 * This function is only called from that constructor. 
	 */
	private void handlePlayerRemovedAction(PlayerRemovedAction tAction)
	{
		String tName = tAction.getActionMaker();
		Player tPlayer;
		
		// remove player from the LoadedPlayers container
		for(int i = 0; i < mLoadedPlayers.size(); i++)
		{
			if(mLoadedPlayers.get(i) != null)
				if(mLoadedPlayers.get(i).toString().equals(tAction.getActionMaker()))
					mLoadedPlayers.set(i, null);
		}
		
		// remove player from the GameContainer
		while(players.hasNext())
		{
			tPlayer = (Player) players.next();
			
			if(tPlayer.toString().equals(tName))
				players.remove();
		}
	}
	
	/**
	 * This method adds the player to the game when the player is loaded from a
	 * file. The player was made from a file which game model produced. We know
	 * the player's information is valid because it was valid when he was first made.
	 * Hence we perform only the relevant labour in this duplicate private function
	 */
	private void addLoadedPlayerToGame(Player p)
	{
		players.addObject(p);
		gameStats.attachGameListener(p);
		playerComparator.addPlayer(p);
		mLoadedPlayers.add(p);
	}
	
	/**
	 * Attaches the GameListener x to the GameModel. 
	 * @throws IllegalArgumentException if the Listener has already been added
	 */
	public void attachGameListener(GameListener x) throws IllegalArgumentException
	{
		gameStats.attachGameListener(x);
	}
	
	/**
	 * Removes the GameListener x from the GameModel
	 * @throws IllegalArgumentException if the Listener was never added
	 */
	public void detachGameListener(GameListener x) throws IllegalArgumentException 
	{
		gameStats.detachGameListener(x);
	}
	
	/**
	 * This method produces no effects. The GameModel plays itself and updates clients
	 * whenever a change occurs. Clients may call it as often as they like, but their actions
	 * will go unnoticed.  
	 */
	public void notifyObservers(Action action)
	{
		gameStats.notifyObservers(action);
	}
	
	/**
	 * Adds the input player as a contestant in the current game and attaches him as
	 * a GameListener at the same time. It is important to remember this latter point. 
	 * @param p The player to be added
	 * @throws IllegalArgumentException Player has already been added
	 * @throws GameIsFullException Obvious
	 * @throws PurseIsEmptyException Thrown when the player's purse is empty
	 * @throws InsufficientFundsException Thrown when the player's finances do not meet ante
	 * @throws MatchAlreadyInSessionException Thrown when a player is added in the middle of
	 * a match. This prevents confusing states from arising. 
	 */
	public void addPlayerToGame(Player p) 
	throws 	IllegalArgumentException, GameIsFullException, PurseIsEmptyException, 
			InsufficientFundsException, MatchAlreadyInSessionException
	{
		
		NewGameAction action; 
		ArrayList<String> tPlayers;
		ArrayList<Integer> tMoneys;
		ArrayList<String> tBigIcons;
		ArrayList<String> tSmallIcons;
		
		if(isMatchInProgress)
			throw new MatchAlreadyInSessionException();
		
		if(players.size() < NUMBER_OF_PLAYERS)
		{
			if(p.getPurse().getChips() < getAnte())
				throw new InsufficientFundsException();
				
			players.addObject(p);
			this.attachGameListener(p);
			
			playerComparator.addPlayer(p);
			
			// tell everyone about it!
			if(players.size() == NUMBER_OF_PLAYERS)
			{
				hasReachedFourPlayers = true;
				tPlayers = new ArrayList<String>();
				tMoneys  = new ArrayList<Integer>();
				tBigIcons = new ArrayList<String>();
				tSmallIcons = new ArrayList<String>();
				
				for(Iterator i = players; i.hasNext(); )
				{
					Player t = (Player) i.next();
					tPlayers.add(t.toString());
					tMoneys.add(t.getPurse().getChips());
					tBigIcons.add(t.getBigIconName());
					tSmallIcons.add(t.getSmallIconName());
				}
					
				action = new NewGameAction("The Egg", tPlayers, tMoneys, tBigIcons, tSmallIcons);
				action.setTournamentRound(tournamentRound);
				action.setDifficultySetting(difficultySetting);
				gameStats.addNewGameAction(action);
			}
		}
		else
			throw new GameIsFullException();
	}
	
	/**
	 * @return the current ante
	 */
	public int getAnte()
	{
		return ante;
	}
	
	/**
	 * @return The current dealer
	 * @throws NoDealerException When there is no dealer
	 */
	public Player getDealer() throws NoDealerException
	{
		return (Player)players.getDealer();
	}
	
	/** 
	 * @return whether a player has won yet
	 */
	public boolean isGameOver()
	{
		return isGameOver;
	}
	
	/**
	 * Executes a single match. To check if the match resulted in a win, use the isGameOver
	 * function. Players must implement the Player Interface to be notified of all changes
	 * occurring during the game. 
	 * @throws GameIsEmptyException When not enough players are currently playing the game
	 * @throws GameIsOverException if a player has already won the current game
	 * @throws MatchAlreadyInSessionException if another match is currently in progress
	 */
	public void ExecuteSingleMatch() 
	throws GameIsEmptyException, GameIsOverException, MatchAlreadyInSessionException, 
	       SteroidPlayerException
	{
		final boolean MATCH_OVER = true;
		
		// Cannot execute another match if a match is already executing
		if(isMatchInProgress)
			throw new MatchAlreadyInSessionException();
			
		// Cannot execute another match if the game is over
		if(isGameOver())
			throw new GameIsOverException();
		
		// Cannot execute a match if one or less players are in game
		if(players.size() <= 1)
			throw new GameIsEmptyException();
		
		// Cannot execute a match if never had four players in game
		if(hasReachedFourPlayers == false)
			throw new GameIsEmptyException();
		
		// protect the current match
		isMatchInProgress = true;
		
		// check that no player cheated before new match invocation
		// inform the world that a new match is beginning
		matchesPlayed++;
		NewMatchAction x = new NewMatchAction();
		x.setMatchID(matchesPlayed);
		gameStats.addNewMatchAction(x);
		validateAllPlayers();
		
		// shift the dealer
		setNextDealer();
		
		// reset the number of players who folded to zero
		matchNumPlayersFolded = 0;
		
		// remove betters who have less than ante
		removeIncompotentBetters();
		
		// collect remaining compotent betters and arrange their seating order 
		setMatchPlayers();
		
		// if all but one player failed, that player wins
		if(MatchPlayers.size() == 1)
		{
			win(MatchPlayers.get(0).getPlayer());
			return;
		}
		
		// Collect the player names in an array
		ArrayList<String> Participants = new ArrayList<String>();
		
		for( int i = 0; i < MatchPlayers.size(); i++)
			Participants.add(MatchPlayers.get(i).getPlayer().toString()); 
		
		// start a new game pot with same indexes as matchPlayers
		pot.reset(MatchPlayers.size(), Participants); 
		
		// set the new ante for this round 
		setAnte();
		
		// collect the antes
		validateAllPlayers();
		collectAntes();
		
		// reset the deck
		resetDeck();
		
		// deal out cards from deck 
		dealOutHands();
		
		// Send out the cards dealt action
		cardsDealtAction();
		
		// collect Pre-Exchange bets from players
		pot.startNewBettingRound();
		
		if(MATCH_OVER != collectPreExchangeBets())
		{
			// discard cards
			discardCards();
			
			// check if there is more than one "still betting" player
			int k = 0; 
			for(int i = 0; i < MatchPlayers.size(); i++)
				if(MatchPlayers.get(i).getBettingStatus() == BettingStatus.STILL_BETTING)
					k++;
			
			// collect Post-Exchange bets from players if more than one betting player
			if(k > 1)
			{
				pot.startNewBettingRound();
				collectPostExchangeBets();
			}
			
			// compare Hands and distribute money (needs to be one function because of side pots)
			competePlayers();
		}
		
		// update player scores
		updatePlayerScores();
		
		// send out the final scores
		finalScoresOfPlayersAlive();
		
		// allow new matches to be evoked
		isMatchInProgress = false;
	}
	
	/**
	 * If a file was used to load some previous history, this function
	 * lets you watch that file match by match. 
	 * @param sIndex should be 0 the first time. 
	 * @return the index of the next match. 
	 */
	public int WatchSingleMatch(int sIndex)
	{
		boolean s = false;
		
		// do not send a new game action
		if(sIndex == 0)
			sIndex = 1;
		
		for(int i = sIndex; i < mLoadedActions.length; i++)
		{
			if(mLoadedActions[i] instanceof NewMatchAction)
			{
				if ( s == true )
					return i;
				if ( s == false )
					s = true;
			}
			
			gameStats.addAction(mLoadedActions[i]);
		}
		
		return mLoadedActions.length;
	}
	
	/**
	 * And this lets you watch everything at once
	 */
	public void WatchEntireGame()
	{
		int i = 0; 
		
		while ( i != mLoadedActions.length )
			i = WatchSingleMatch(i);
	}
	
	/** 
	 * @return The player who won the game 
	 * @throws NoGameWinnerYetException if no player has yet won the game 
	 */
	public Player getGameWinner() throws NoGameWinnerYetException
	{
		if( isGameOver() )
			return GameWinner;
		else
			throw new NoGameWinnerYetException();
	}
	
	/*****************************************************************************/
	
	/**
	 *  Checks if all players have not altered themselves from previous call
	 */
	private void validateAllPlayers() throws SteroidPlayerException
	{
		for(int i = 0; i < MatchPlayers.size(); i++)
			if(!playerComparator.isPlayerValid(MatchPlayers.get(i).getPlayer()))
				throw new SteroidPlayerException();
	}
	
	/**
	 *  Updates players to reflect allowed player modifications
	 */
	private void updateAllPlayers()
	{
		for(int i = 0; i < MatchPlayers.size(); i++)
			playerComparator.updatePlayer(MatchPlayers.get(i).getPlayer());
	}
	
	/**
	 * Sets the next dealer between matches
	 */
	private void setNextDealer() 
	{		
		try
		{
			players.shiftDealer();
		} 
		catch (NoDealerException e) { assert false;}
	}
	
	/**
	 * Removes all betters who cannot pay the ante
	 */
	private void removeIncompotentBetters()
	{
		ArrayList<Player> incompotents = new ArrayList<Player>();
		Player current;
		Iterator i;
	
		int counter=0;
		while(players.hasNext() && counter<players.size())
		{
			current = (Player) players.next();
			
			try
			{
				if(current.getPurse().getChips() < ante)
				{
					players.remove();
					incompotents.add(current);
				}
			}
			catch (PurseIsEmptyException e)
			{
				players.remove();
				incompotents.add(current);
			}
			counter++;
		}
		
		i = incompotents.iterator();
		while(i.hasNext())
		{
			current = (Player) i.next();
			removePlayer(current);	
		}
	}
	
	/**
	 * Sets all players still in game into the struct used to hold them during the match
	 */
	private void setMatchPlayers()
	{
		MatchPlayers = new ArrayList<PlayerWrapper>();
		
		int counter=0;
		while(players.hasNext() && counter<players.size())
		{
			MatchPlayers.add( 
					new PlayerWrapper(
							(Player) players.next(), 
							BettingStatus.STILL_BETTING, 
							MatchPlayers.size()));
		}
	}
	
	/**
	 * Sets the ante according to round number
	 */
	private void setAnte()
	{
		boolean anteCanBeIncremented = false;
		Iterator i;
		Player iPlayer;
		int oldAnte = ante;
		
		if( ! incrementAnte() )
		{
			try{
				gameStats.addAction( 
					new IncrementAnteAction
					(getDealer().toString(), IncrementAnteAction.incrementAnteAction.NULL, ante));
			}
			catch(NoDealerException e)
			{
				assert false;
			}
			return;
		}
		
		updateAnte();
		int counter=0;
		/* Check if at least one player can make new ante */
		for(i=players; i.hasNext() && counter<players.size(); counter++)
		{
			iPlayer = (Player) i.next();
			
			try
			{
				if(iPlayer.getPurse().getChips() >= ante)
					anteCanBeIncremented = true;
			}
			catch (PurseIsEmptyException e){assert false;}
		}
		
		if(anteCanBeIncremented)
		{
			try
			{
				gameStats.addAction( 
						new IncrementAnteAction(getDealer().toString(), 
								IncrementAnteAction.incrementAnteAction.SUCCEED, ante));
			} 
			catch (NoDealerException e)
			{
				assert false;
			}
		}
		
		/* Nobody can make the new ante. Revert to old ante */
		else
		{
			ante = oldAnte;
			try
			{
				gameStats.addAction( 
						new IncrementAnteAction(getDealer().toString(), 
								IncrementAnteAction.incrementAnteAction.FAIL, ante));
			} 
			catch (NoDealerException e)
			{
				assert false;
			}
		}
	}
	
	/** 
	 * @return Whether or not to increment the ante in this round
	 */
	private boolean incrementAnte()
	{
		return ((gameStats.getMatchNumber() + 1) % ANTE_INCREMENT_ROUND == 0); 
	}
	
	/**
	 * increments the ante by 20%
	 */
	private void updateAnte()
	{
		/* Increment the ante by 20% */
		ante += (int) ( ANTE_INCREMENT_PERCENTAGE * (float)ante);
	}
	
	/**
	 * Collects the antes from the players
	 */
	private void collectAntes()
	{	
		final boolean GOING_ALL_IN = true;
		final boolean NOT_GOING_ALL_IN = false;
		Player cp;
		
		for(int j = 0; j < MatchPlayers.size(); j++)
		{
			try
			{
				cp = MatchPlayers.get(j).getPlayer();
				
				if(cp.getPurse().getChips() == ante)
				{
					// Notify the observers of the fact that player is going all in for ante
					gameStats.addAction
						(new BetAction(cp.toString(), BetAction.betAction.ALL_IN, ante));
					
					// add the ante
					pot.addBettingMoney(j,cp.getPurse().removeChips(ante),GOING_ALL_IN);
					
					// inform ourselves that this occurred 
					MatchPlayers.get(j).setBettingStatus(BettingStatus.WENT_ALL_IN);	
				}
				else
					pot.addBettingMoney
						(j, cp.getPurse().removeChips(ante), NOT_GOING_ALL_IN);
				
				
				// update the player scores
				gameStats.addAction
					(new ScoreAction(cp.toString(),cp.getPurse().getChips()));
				
				// update the player's internal
				playerComparator.updatePlayer(cp);
			} 
			catch (Exception e)
			{ assert false;}
		}		
	}
	
	/**
	 * Shuffles the deck and restores old cards
	 */
	private void resetDeck()
	{
		deck = new Deck();
		deck.shuffle();
	}
	
	/**
	 * Deals out hands 
	 */
	private void dealOutHands()
	{
		Hand currentHand;
		Player cp;
		
		for(int i = 0; i < MatchPlayers.size(); i++)
		{
			currentHand = new Hand();
			cp = MatchPlayers.get(i).getPlayer();
			
			for(int j = 0; j < HAND_SIZE; j++)
				currentHand.add(deck.draw());
			
			cp.setHand(currentHand);
			playerComparator.updatePlayer(cp);
		}
	}
	
	private void cardsDealtAction()
	{
		gameStats.addAction(new CardsDealtAction());
	}
	
	/**
	 * Collects the bets pre card exchange
	 * @return Whether or not the match is over
	 */
	private boolean collectPreExchangeBets() throws SteroidPlayerException
	{
		int j = 0;
		int betNumber = 1;
		int numPlayersFolded = matchNumPlayersFolded;
		BetAction action;
		final boolean WENT_ALL_IN = true;
		final boolean NOT_WENT_ALL_IN = false;
		Player currentPlayer;
		
		/* *
		 * The pre Exchange round.  Only terminates if everyone calls and every player
		 * has bet.
		 * */
		while((pot.getMinBet(j) != 0 )  || betNumber <= MatchPlayers.size() )
		{	
			try
			{
				currentPlayer=MatchPlayers.get(j).getPlayer();
				
				/* If the bettor has folded or already gone all in, he cannot make a bet */
				if( MatchPlayers.get(j).getBettingStatus()==BettingStatus.STILL_BETTING)
				{
					if(numPlayersFolded != MatchPlayers.size() - 1)
					{
						action = currentPlayer.makePreBetAction(pot.getMinBet(j));
						if(!playerComparator.isPlayerValid(currentPlayer))
							throw new SteroidPlayerException();
					}
					
					/* the current bettor is the only player not to fold, end the match */
					else
					{
						matchNumPlayersFolded = numPlayersFolded;
						competePlayers();
						return true;
					}
					
					/* If bettor folds, send a FOLD action and abort from all Pots */
					if(action.getAction() == BetAction.betAction.FOLD)
					{
						int returnValue; 
						MatchPlayers.get(j).setBettingStatus(BettingStatus.FOLDED); 
						returnValue = pot.abortParticipant(j);
						numPlayersFolded++;
						if(returnValue != 0)
						{
							try
							{
								currentPlayer.addToPurse(returnValue);
								gameStats.addAction
								(
									new GetMoneyAction(currentPlayer.toString(), pot.getNumberOfSmallerPots(), 
										returnValue)
								);
								validateAllPlayers();
							}
							catch (Exception e)
							{
								assert (false);
							}
						}
						else
						{
							gameStats.addAction(action);
						}
					}
	
					/* If the minBet is 0, then CHECK is allowed.  Else, it polls the player again */
					else if(action.getAction() == BetAction.betAction.CHECK)
					{
						if(pot.getMinBet(j) == 0)
							gameStats.addAction(action);
						else
						{
							j = decrementMatchPlayersIndex(j);
							betNumber--;
						}
					}
					else if(action.getAction() == BetAction.betAction.CALL)
					{
						if(currentPlayer.getPurse().getChips() <= pot.getMinBet(j))
						{
							action = new BetAction(currentPlayer.toString(),
													BetAction.betAction.ALL_IN,
													currentPlayer.getPurse().getChips());
							
							gameStats.addAction(action);
							MatchPlayers.get(j).setBettingStatus(BettingStatus.WENT_ALL_IN);
							pot.addBettingMoney
								(j, currentPlayer.getPurse().removeAllChips(), WENT_ALL_IN);
							
							/* update the player scores */
							gameStats.addAction
							(
								new ScoreAction
									(currentPlayer.toString(), currentPlayer.getPurse().getChips())
							);
						}
						else
						{
							gameStats.addAction(action);
							pot.addBettingMoney
							(	
								j, currentPlayer.getPurse().removeChips(pot.getMinBet(j)), NOT_WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
								(
									currentPlayer.toString(), currentPlayer.getPurse().getChips()
								)
							);
						}
					}
					else if(action.getAction() == BetAction.betAction.RAISE)
					{
						int raiseMoney = pot.getMinBet(j) + action.getAmount();
						
						if(raiseMoney > currentPlayer.getPurse().getChips())
						{
							j = decrementMatchPlayersIndex(j);
							betNumber--; 
						}
						else if(raiseMoney == currentPlayer.getPurse().getChips())
						{
							action = 
								new BetAction(
									currentPlayer.toString(),
									BetAction.betAction.ALL_IN,
									currentPlayer.getPurse().getChips()
									);
							
							gameStats.addAction(action);
							MatchPlayers.get(j).setBettingStatus(BettingStatus.WENT_ALL_IN);
							
							pot.addBettingMoney
							(
								j, currentPlayer.getPurse().removeAllChips(), WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
								(
									currentPlayer.toString(), currentPlayer.getPurse().getChips()
								)
							);
						}
						else
						{
							gameStats.addAction(action);
							pot.addBettingMoney
							(
								j, currentPlayer.getPurse().removeChips(raiseMoney), NOT_WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
								(
									currentPlayer.toString(), currentPlayer.getPurse().getChips()
								)
							);
						}
					}
					else
					{
						if(action.getAmount() != currentPlayer.getPurse().getChips() )
						{
							j = decrementMatchPlayersIndex(j);
							betNumber--;
						}
						else
						{
							gameStats.addAction(action);
							MatchPlayers.get(j).setBettingStatus(BettingStatus.WENT_ALL_IN);
							
							pot.addBettingMoney
							(
								j, currentPlayer.getPurse().removeAllChips(), WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
									(currentPlayer.toString(), currentPlayer.getPurse().getChips())
							);
						}
					}
				}
				playerComparator.updatePlayer(currentPlayer);
			}
			catch(SteroidPlayerException e)
			{
				throw e;
			}
			catch(Exception e)
			{ 
				e.printStackTrace();
				assert false;
			}
			
			j = incrementMatchPlayersIndex(j);
			betNumber++;
		}
		
		matchNumPlayersFolded = numPlayersFolded;
		return false;
	}
	
	private int incrementMatchPlayersIndex(int index)
	{
		return ((index + 1) % MatchPlayers.size());
	}
	
	private int decrementMatchPlayersIndex(int index)
	{
		return ((index + MatchPlayers.size() - 1) % MatchPlayers.size());
	}

	private void discardCards() throws SteroidPlayerException
	{
		int i;
		Player currentPlayer;
		Hand currentHand;
		ArrayList <Card> discardedCards;
		
		// Skip discard round if everyone but one already folded
		if(matchNumPlayersFolded == MatchPlayers.size() - 1)
			return;
		
		for(i = 0; i < MatchPlayers.size(); i++)
		{
			
			if(MatchPlayers.get(i).getBettingStatus() == BettingStatus.FOLDED)
				continue;
			
			currentPlayer = MatchPlayers.get(i).getPlayer();
			currentHand = currentPlayer.getHand();
			discardedCards = currentPlayer.discardCards(NUMBER_OF_DISCARD);
			
			while(discardedCards.size() > NUMBER_OF_DISCARD)
				discardedCards = currentPlayer.discardCards(NUMBER_OF_DISCARD);
			
			if(!assertCorrectnessOfDiscard(discardedCards))
				throw new SteroidPlayerException();
			
			// validate the player (no tricky business)
			if(!playerComparator.isPlayerValid(currentPlayer))
				throw new SteroidPlayerException();
			
			// Set the hand in the player
			for(int j = 0; j < discardedCards.size(); j++)
			{
				currentHand.remove(discardedCards.get(j));
				currentHand.add(deck.draw());
			}
			currentPlayer.setHand(currentHand);
			
			// Notify everyone of the discard
			if(discardedCards.size() > 0)
				gameStats.addAction(
						new CardExchangeAction(currentPlayer.toString(), 
						CardExchangeAction.cardExchangeAction.DISCARD, 
						discardedCards.size()));
			else
				gameStats.addAction(
						new CardExchangeAction(currentPlayer.toString(), 
						CardExchangeAction.cardExchangeAction.NO_DISCARD, 
						discardedCards.size()));
			
			playerComparator.updatePlayer(currentPlayer);
		}
	}
	
	/**
	 * Checks that the discarded Cards from the player are valid
	 */
	private boolean assertCorrectnessOfDiscard(ArrayList<Card> discardedCards)
	{
		if(discardedCards.size() <= 1)
			return true; 
		
		else if(discardedCards.size() == 2)
			return (!compareCards(discardedCards.get(0),discardedCards.get(1)));
		
		else
		{
			boolean result = true;
			result = (result) && (!compareCards(discardedCards.get(0),discardedCards.get(1)));
			result = (result) && (!compareCards(discardedCards.get(1),discardedCards.get(2)));
			result = (result) && (!compareCards(discardedCards.get(0),discardedCards.get(2)));
			return result;
		}
	}
	
	// Quickly compares two cards
	private boolean compareCards(Card One, Card Two)
	{
		return (One.getRank() == Two.getRank() && One.getSuit() == Two.getSuit());
	}
	
	/**
	 * 
	 * @throws SteroidPlayerException
	 */
	private void collectPostExchangeBets() throws SteroidPlayerException
	{
		int j = 0;
		int betNumber = 1;
		int numPlayersFolded = matchNumPlayersFolded;
		BetAction action;
		final boolean WENT_ALL_IN = true;
		final boolean NOT_WENT_ALL_IN = false;
		Player currentPlayer;
		
		/* *
		 * The pre Exchange round.  Only terminates if everyone calls and every player
		 * has bet.
		 * */
		while( 	(pot.getMinBet(j) != 0 /*&& 
		MatchPlayers.get(j).getBettingStatus()==BettingStatus.STILL_BETTING*/)  
			|| betNumber <= MatchPlayers.size()   )
		{	
			try
			{
				currentPlayer=MatchPlayers.get(j).getPlayer();
				
				/* If the bettor has folded or already gone all in, he cannot make a bet */
				if( MatchPlayers.get(j).getBettingStatus()==BettingStatus.STILL_BETTING)
				{
					if(numPlayersFolded != MatchPlayers.size() - 1)
					{
						action = currentPlayer.makePostBetAction(pot.getMinBet(j));
						if(!playerComparator.isPlayerValid(currentPlayer))
							throw new SteroidPlayerException();
					}
					
					/* the current bettor is the only player not to fold, end the match */
					else
					{
						matchNumPlayersFolded = numPlayersFolded;
						return;
					}
					
					/* If bettor folds, send a FOLD action and abort from all Pots */
					if(action.getAction() == BetAction.betAction.FOLD)
					{
						int returnValue; 
						MatchPlayers.get(j).setBettingStatus(BettingStatus.FOLDED); 					
						returnValue = pot.abortParticipant(j);
						numPlayersFolded++;
						if(returnValue != 0)
						{
							try
							{
								currentPlayer.addToPurse(returnValue);
								gameStats.addAction
								(
									new GetMoneyAction(currentPlayer.toString(), pot.getNumberOfSmallerPots(), 
										returnValue)
								);
								validateAllPlayers();
							}
							catch (Exception e)
							{
								assert (false);
							}
						}
						else
						{
							gameStats.addAction(action);
						}
					}
	
					/* If the minBet is 0, then CHECK is allowed.  Else, it polls the player again */
					else if(action.getAction() == BetAction.betAction.CHECK)
					{
						if(pot.getMinBet(j) == 0)
							gameStats.addAction(action);
						else
						{
							j = decrementMatchPlayersIndex(j);
							betNumber--;
						}
					}
					else if(action.getAction() == BetAction.betAction.CALL)
					{	
						if(currentPlayer.getPurse().getChips() <= pot.getMinBet(j))
						{
							action = new BetAction(currentPlayer.toString(),
													BetAction.betAction.ALL_IN,
													currentPlayer.getPurse().getChips());
							
							gameStats.addAction(action);
							MatchPlayers.get(j).setBettingStatus(BettingStatus.WENT_ALL_IN);
							pot.addBettingMoney
								(j, currentPlayer.getPurse().removeAllChips(), WENT_ALL_IN);
							
							/* update the player scores */
							gameStats.addAction
							(
								new ScoreAction
									(currentPlayer.toString(), currentPlayer.getPurse().getChips())
							);
						}
						else
						{
							gameStats.addAction(action);
							pot.addBettingMoney
							(	
								j, currentPlayer.getPurse().removeChips(pot.getMinBet(j)), NOT_WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
								(
									currentPlayer.toString(), currentPlayer.getPurse().getChips()
								)
							);
						}
					}
					else if(action.getAction() == BetAction.betAction.RAISE)
					{
						int raiseMoney = pot.getMinBet(j) + action.getAmount();
						
						if(raiseMoney > currentPlayer.getPurse().getChips())
						{
							j = decrementMatchPlayersIndex(j);
							betNumber--; 
						}
						else if(raiseMoney == currentPlayer.getPurse().getChips())
						{
							action = 
								new BetAction(
									currentPlayer.toString(),
									BetAction.betAction.ALL_IN,
									currentPlayer.getPurse().getChips()
									);
							
							gameStats.addAction(action);
							MatchPlayers.get(j).setBettingStatus(BettingStatus.WENT_ALL_IN);
							
							pot.addBettingMoney
							(
								j, currentPlayer.getPurse().removeAllChips(), WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
								(
									currentPlayer.toString(), currentPlayer.getPurse().getChips()
								)
							);
						}
						else
						{
							gameStats.addAction(action);
							pot.addBettingMoney
							(
								j, currentPlayer.getPurse().removeChips(raiseMoney), NOT_WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
								(
									currentPlayer.toString(), currentPlayer.getPurse().getChips()
								)
							);
						}
					}
					else
					{
						if(action.getAmount() != currentPlayer.getPurse().getChips() )
						{
							j = decrementMatchPlayersIndex(j);
							betNumber--;
						}
						else
						{
							gameStats.addAction(action);
							MatchPlayers.get(j).setBettingStatus(BettingStatus.WENT_ALL_IN);
							
							pot.addBettingMoney
							(
								j, currentPlayer.getPurse().removeAllChips(), WENT_ALL_IN
							);
							
							// update the player scores
							gameStats.addAction
							(
								new ScoreAction
									(currentPlayer.toString(), currentPlayer.getPurse().getChips())
							);
						}
					}
				}
				playerComparator.updatePlayer(currentPlayer);
			}
			catch(SteroidPlayerException e)
			{
				throw e;
			}
			catch(Exception e)
			{ 
				e.printStackTrace();
				assert false;
			}

			j = incrementMatchPlayersIndex(j);
			betNumber++;
		}
		
		matchNumPlayersFolded = numPlayersFolded;
	
	}
	
	private void competePlayers() throws SteroidPlayerException
	{	
		boolean playersInPot[]; 
		ScoreRevealAction action; 
		Hand playerHands[] = new Hand[MatchPlayers.size()];
		HandValuator x = new HandValuator();
		int numFolded = matchNumPlayersFolded;
		int moneyFromFolding = 0;
		Player cp; 
		
		// Collect decisions from players and remove those who folded
		// Keep all those who reveal their cards
		playersInPot = pot.playersInPot(1);
		
		for(int i = 0; i < MatchPlayers.size(); i++)
		{	
			boolean sendGetMoneyAction = false;
			int hp = 0;
			
			cp = MatchPlayers.get(i).getPlayer();
			
			if(playersInPot[i])
			{		
				if(numFolded != MatchPlayers.size()-1)
				{
					action = cp.makeScoreRevealAction();
					if(!playerComparator.isPlayerValid(cp))
						throw new SteroidPlayerException();
				}
				else
					action=
						new ScoreRevealAction(cp.toString(),
								ScoreRevealAction.scoreRevealAction.NO_SHOW, cp.getHand());
				
				if( action.getAction() == ScoreRevealAction.scoreRevealAction.FOLD)
				{
					hp = pot.getHighestPotOfParticipant(i);
					moneyFromFolding = pot.abortParticipant(i);
					
					if(moneyFromFolding > 0)
					{
						MatchPlayers.get(i).getPlayer().addToPurse(moneyFromFolding);
						
						sendGetMoneyAction = true;
						
						try
						{
							gameStats.addAction
							(
								new ScoreAction
								(
									MatchPlayers.get(i).getPlayer().toString(), 
									MatchPlayers.get(i).getPlayer().getPurse().getChips()
								)
							);
						}
						catch (Exception e)
						{
							e.printStackTrace();
							assert false;
						}
						
						this.playerComparator.updatePlayer(MatchPlayers.get(i).getPlayer());
						
					}
					
					try
					{
						MatchPlayers.get(i).setBettingStatus(BettingStatus.FOLDED);
					}
					catch (Exception e) {e.printStackTrace();assert false;} 
					numFolded++;
				}
				else if(action.getAction() == ScoreRevealAction.scoreRevealAction.SHOW)
				{
					try
					{
						playerHands[i] = action.getHand();
					}
					catch(HandFoldedException e)
					{
						e.printStackTrace();
					}
				}
				else
				{
					playerHands[i] = cp.getHand();
				}
				
				try
				{
					if(action.getAction() == ScoreRevealAction.scoreRevealAction.SHOW)
						action.setHandValue(x.valuateHand(action.getHand()));
					
					gameStats.addAction(action);
					validateAllPlayers();
					
					if(sendGetMoneyAction)
					{
						gameStats.addAction(
								new GetMoneyAction(MatchPlayers.get(i).getPlayer().toString(), 
								hp , moneyFromFolding));
					}
				}
				catch(HandFoldedException e)
				{
					e.printStackTrace();
				}
			}
		}
		
		// For every available side pot, compete the players still active in that pot
		// Any-one strictly lower than the highest hand is removed from the current
		// and all subsequent pots. The sharing winners share the money in that pot
		// until they too are defeated in a lower pot
		for(int i = pot.getNumberOfSmallerPots(); i > 0; i--)
		{
			playersInPot = pot.playersInPot(i);
			int MoneyInPot = pot.getMoneyInPot(i);
			int NumPlayersInPot = 0;
			int MoneyRemainder = 0;
			
			for(int j = 0; j < playersInPot.length; j++)
				if(playersInPot[j])
				{
					for(int k = 0; k < playersInPot.length; k++)
						if(playersInPot[k])
						{	
							if(x.valuateHand(playerHands[j]).compareTo(x.valuateHand(playerHands[k])) > 0)
							{
								pot.abortParticipant(k);
								playersInPot[k] = false;
							}
						}
				}
			
			playersInPot = pot.playersInPot(i);
			
			for(int j = 0; j < playersInPot.length; j++)
				if(playersInPot[j])
					NumPlayersInPot++;
			
			if(NumPlayersInPot != 0)
			{
				MoneyInPot += MoneyRemainder;
				MoneyRemainder = MoneyInPot % NumPlayersInPot;
				MoneyInPot = MoneyInPot / NumPlayersInPot;
			}
			else
			{
				continue;
			}
			
			for(int j = 0; j < playersInPot.length; j++)
				if(playersInPot[j])
				{
					MatchPlayers.get(j).getPlayer().addToPurse(MoneyInPot);
					playerComparator.updatePlayer(MatchPlayers.get(j).getPlayer());
					gameStats.addAction(
							new GetMoneyAction(MatchPlayers.get(j).getPlayer().toString(), 
							i, MoneyInPot));
					validateAllPlayers();
					
					try
					{
						// update the player scores
						gameStats.addAction
						(
							new ScoreAction
							(
								MatchPlayers.get(j).getPlayer().toString(), 
								MatchPlayers.get(j).getPlayer().getPurse().getChips()
							)
						);
						validateAllPlayers();
					} 
					catch (PurseIsEmptyException e)
					{ assert false;}
				}
		}
	}
	
	private void updatePlayerScores()
	{
		int amount = 0;
		int numPlayersRemoved = 0;
		Player winningPlayer = MatchPlayers.get(MatchPlayers.size() - 1).getPlayer();
		
		for(int i = 0; i < MatchPlayers.size(); i++)
		{
			try
			{
				amount = MatchPlayers.get(i).getPlayer().getPurse().getChips();
				
				if( amount == 0 )
					throw new PurseIsEmptyException();
				else if (amount < getAnte())
					throw new InsufficientFundsException();
				
				winningPlayer = MatchPlayers.get(i).getPlayer();
			}
			catch (PurseIsEmptyException e)
			{
				removePlayer(MatchPlayers.get(i).getPlayer());
				numPlayersRemoved++;
				
				// If we removed the second last player!, win the remaining player 
				if(numPlayersRemoved == MatchPlayers.size() - 1)
				{
					win(winningPlayer);
				}
			}
			catch (InsufficientFundsException e)
			{
				removePlayer(MatchPlayers.get(i).getPlayer());
				numPlayersRemoved++;
				
				// If we removed the second last player!, win the remaining player 
				if(numPlayersRemoved == MatchPlayers.size() - 1)
					win(winningPlayer);
			}
		}		
	}
	
	/**
	 * Sends out the the absolute final scores of all players still alive
	 */
	private void finalScoresOfPlayersAlive()
	{
		Iterator i = players;
		Player tPlayer;
		
		try
		{
			
			int counter = 0;
			while (i.hasNext()&& counter < players.size())
			{
				tPlayer = (Player) i.next();

				gameStats.addAction(new ScoreAction(tPlayer.toString(), tPlayer.getPurse().getChips()));
				counter++;
			}
		}
		catch (Exception e)
		{
			assert(false);
			e.printStackTrace();
		}
	}
	
	private void removePlayer(Player toRemove)
	{ 
		// Notify remaining players of the removal 
		gameStats.addAction( new PlayerRemovedAction(toRemove.toString()));
		
		// increase the ante, if possible 
		ante += 5;
		gameStats.addAction
		(
			new IncrementAnteAction
				( "The Reaper", IncrementAnteAction.incrementAnteAction.SUCCEED, ante)
		);
		int counter = 0;
		// remove player from the GameContainer
		while(players.hasNext() && counter < players.size())
		{
			if(players.next().toString().equals(toRemove.toString()))
				players.remove();
			counter++;
		}
	}
	
	/** 
	 * The machinery to end the game
	 * @param p , the player who won
	 */
	private void win(Player p) 
	{
		GameWinner = p;
		isGameOver = true;
	}

	public void setTournamentRound(int i)
	{
		tournamentRound = i;
	}
	
	public void setDifficultySetting(int i)
	{
		difficultySetting = i;
	}
	
	public int getDifficultySetting()
	{
		return difficultySetting;
	}
	
	public int getTournamentRound()
	{
		return tournamentRound;
	}
}
