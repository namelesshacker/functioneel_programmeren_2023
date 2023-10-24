package game;
import java.util.ArrayList;

import game.actions.*;
import game.exceptions.*;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class has the stats of the whole game, storing them in an ArrayList of
 * MatchStats (each MatchStats represents one match).  This class also implements the
 * GameSubject Interface.  It notifies all the observers (other players, the GUI, etc.)
 * when its state is changed.
 *
 */

public class GameStats implements GameSubject{
	
	/* The new game action */
	protected NewGameAction mNewGameAction;
	/* The list of MatchStats */
	protected ArrayList<MatchStats> matchStats;
	/* The observers listening in on this object */
	private ArrayList<GameListener> gameListeners;
	
	private boolean keepAtFalse;
	
	public GameStats()
	{
		matchStats=new ArrayList<MatchStats>();
		gameListeners=new ArrayList<GameListener>();
		keepAtFalse=false;
	}
	
	/**
	 * Makes a new match the previous match has finished.
	 * Notify all Observers
	 *
	 */
	public void addNewMatchAction(NewMatchAction action)
	{
		matchStats.add(new MatchStats());
		this.keepAtFalse=true;
		notifyObservers(action);
		keepAtFalse=false;
	}
	
	/**
	 * Adds a generic action to the current match stats
	 * @param action the action to be added
	 */
	public void addAction(Action action)
	{
		this.keepAtFalse=true;
		
		if(action instanceof NewGameAction )
		{
			mNewGameAction = (NewGameAction) action;
			notifyObservers(action);
			this.keepAtFalse=false;
			return;
		}
		
		if(action instanceof NewMatchAction )
		{
			matchStats.add(new MatchStats());
		}
		
	//	matchStats.get(matchStats.size() - 1).addAction(action);
		notifyObservers(action);
		this.keepAtFalse=false;
	}
	
	public void addNewGameAction(NewGameAction action)
	{
		this.keepAtFalse=true;
		notifyObservers(action);
		keepAtFalse=false;
	}
	
	public void addCardsDealtAction(CardsDealtAction action)
	{
		this.keepAtFalse=true;
		notifyObservers(action);
		keepAtFalse=false;
	}
	
	/**
	 * Add a PreExchangeAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addPreExchangeAction(BetAction action)
	{
		try
		{
			matchStats.get(matchStats.size()-1).addPreExchangeBetAction(action);
			keepAtFalse=true;
			notifyObservers(action);
			keepAtFalse=false;
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}

	/**
	 * Add a PostExchangeAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addPostExchangeAction(BetAction action)
	{
		try
		{
			matchStats.get(matchStats.size()-1).addPostExchangeBetAction(action);
			keepAtFalse=true;
			notifyObservers(action);
			keepAtFalse=false;
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}
	
	/**
	 * Add a CardExchangeAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addCardExchangeAction(CardExchangeAction action)
	{
		try
		{
			matchStats.get(matchStats.size()-1).addCardExchangeAction(action);
			keepAtFalse=true;
			notifyObservers(action);
			keepAtFalse=false;
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}
	
	/**
	 * Add a ScoreRevealAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addScoreRevealAction(ScoreRevealAction action)
	{
		try
		{
			matchStats.get(matchStats.size()-1).addScoreRevealAction(action);
			keepAtFalse=true;
			notifyObservers(action);
			keepAtFalse=false;
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}
	 
	/**
	 * Add an AntePlayerRemovedAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addAntePlayerRemovedAction(PlayerRemovedAction action)
	{
		try
		{
			 matchStats.get(matchStats.size()-1).addAntePlayerRemovedAction(action);
			 keepAtFalse=true;
			 notifyObservers(action);
			 keepAtFalse=false;
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}
	
	/**
	 * Add a PlayerRemovedeAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */ 
	public void addPlayerRemovedAction(PlayerRemovedAction action)
	{
		try
		{
			if(matchStats.size()==0)
				matchStats.get(matchStats.size()).addPlayerRemovedAction(action);
			else
				matchStats.get(matchStats.size()-1).addPlayerRemovedAction(action);
				
			keepAtFalse=true;
			notifyObservers(action);
			keepAtFalse=false;
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}

	/**
	 * Add an IncrementAnteAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addIncrementAnteAction(IncrementAnteAction action)
	{
		try
		{
			if(matchStats.size()==0)
				matchStats.get(matchStats.size()).addIncrementAnteAction(action);
			else
				matchStats.get(matchStats.size()-1).addIncrementAnteAction(action);
			keepAtFalse=true;
			notifyObservers(action);
			keepAtFalse=false;
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}

	/**
	 * Add an IncrementAnteAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addGetMoneyAction(GetMoneyAction action)
	{
		if(matchStats.size()==0)	
			matchStats.get(matchStats.size()).addGetMoneyAction(action);
		else
			matchStats.get(matchStats.size()-1).addGetMoneyAction(action);
		keepAtFalse=true;
		notifyObservers(action); 
		keepAtFalse=false;
	}
	
	/**
	 * Add an IncrementAnteAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addScoreAction(ScoreAction action)
	{
		if(matchStats.size()==0)	
			matchStats.get(matchStats.size()).addScoreAction(action);
		else
			matchStats.get(matchStats.size()-1).addScoreAction(action);
		keepAtFalse=true;
		notifyObservers(action); 
		keepAtFalse=false;
	}
	
	/* Get a specific action from a specific match */
	
	public BetAction getPreExchangeAction(int matchNumber, int preExchangeActionNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		if(preExchangeActionNumber < 0 || 
		   preExchangeActionNumber > matchStats.get(matchNumber).getNumPreExchangeBetActions())
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getPreExchangeAction(preExchangeActionNumber);
	}
	
	public BetAction getPostExchangeAction(int matchNumber, int postExchangeActionNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		if(postExchangeActionNumber<0 || 
		   postExchangeActionNumber>matchStats.get(matchNumber).getNumPreExchangeBetActions())
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getPostExchangeAction(postExchangeActionNumber);
	}
	
	public CardExchangeAction getCardExchangeAction(int matchNumber, int cardExchangeActionNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		if(cardExchangeActionNumber<0 || 
		   cardExchangeActionNumber>matchStats.get(matchNumber).getNumPreExchangeBetActions())
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getCardExchangeAction(cardExchangeActionNumber);
	}
	
	public GetMoneyAction getGetMoneyAction(int matchNumber, int getMoneyActionNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		if(getMoneyActionNumber<0 || 
		   getMoneyActionNumber>matchStats.get(matchNumber).getNumPreExchangeBetActions())
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getGetMoneyAction(getMoneyActionNumber);
	}
	
	public IncrementAnteAction getIncrementAnteAction(int matchNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getIncrementAnteAction();
	}
	
	public PlayerRemovedAction getPlayerRemovedAction(int matchNumber, int playerRemovedActionNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		if(playerRemovedActionNumber<0 || 
		   playerRemovedActionNumber>matchStats.get(matchNumber).getNumPreExchangeBetActions())
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getPlayerRemovedAction(playerRemovedActionNumber);
	}
	
	public ScoreRevealAction getScoreRevealAction(int matchNumber, int scoreRevealActionNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		if(scoreRevealActionNumber<0 || 
		   scoreRevealActionNumber>matchStats.get(matchNumber).getNumPreExchangeBetActions())
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getScoreRevealAction(scoreRevealActionNumber);
	}
	
	public ScoreAction getScoreAction(int matchNumber, int scoreActionNumber) throws IllegalArgumentException
	{
		if(matchNumber>=matchStats.size() || matchNumber<0)
			throw new IllegalArgumentException();
		if(scoreActionNumber<0 || 
		   scoreActionNumber>matchStats.get(matchNumber).getNumScoreActions())
			throw new IllegalArgumentException();
		
		return matchStats.get(matchNumber).getScoreAction(scoreActionNumber);
	}
	
	/* Code for getting the number of a certain action for a specific match*/
	
	public int getNumPreExchangeBetActions(int matchNumber)
	{
		return matchStats.get(matchNumber).getNumPreExchangeBetActions();
	}
	 
	public int getNumPostExchangeBetActions(int matchNumber)
	{
		return matchStats.get(matchNumber).getNumPostExchangeBetActions();
	}
	 
	public int getNumCardExchangeActions(int matchNumber)
	{
		return matchStats.get(matchNumber).getNumCardExchangeActions();
	}
	 
	public int getNumGetMoneyActions(int matchNumber)
	{
		return matchStats.get(matchNumber).getNumGetMoneyActions();
	}
	 
	public int getNumPlayerRemovedActions(int matchNumber)
	{
		return matchStats.get(matchNumber).getNumPlayerRemovedActions();
	}
	 
	public int getNumScoreRevealActions(int matchNumber)
	{
		return matchStats.get(matchNumber).getNumScoreRevealActions();
	}
	 
	public int getNumScoreActions(int matchNumber)
	{
		return matchStats.get(matchNumber).getNumScoreActions();
	}
	
	public int getMatchNumber()
	{
		return matchStats.size();
	}
	
	/* Observer design methods (GameListener inherited and notifyObservers)*/
	
	/**
	 * Implemented from the GameSubject class.
	 * Attaches a GameListener
	 */
	public void attachGameListener(GameListener g) throws IllegalArgumentException
	{
		gameListeners.add(g);
	}
	 
	/**
	 * Implemented from the GameSubject class.
	 * Detaches a GameListener
	 */
	public void detachGameListener(GameListener g) throws IllegalArgumentException
	{
		for(int i=0; i<gameListeners.size(); i++)
			if(gameListeners.get(i).equals(g))
			{
				gameListeners.remove(g);
			 	return;
			}
		throw new IllegalArgumentException();
	}
	
	/**
	 * Tells all the observers what has been changed to this obejct.
	 * 
	 * @param action
	 */
	public void notifyObservers(Action action)
	{
		if(keepAtFalse){
			for(int i=0; i<gameListeners.size(); i++){
				gameListeners.get(i).notify(action);
			}
		}
	}
	
	public String toString()
	{
		String result="MatchStats\n";
		for(int i=0; i<matchStats.size(); i++)
			result+=matchStats.get(i);
		
		result+="\nGameListeners\n"+gameListeners;
		
		return result;
	}
}