package game;

import game.actions.BetAction;
import game.actions.CardExchangeAction;
import game.actions.GetMoneyAction;
import game.actions.IncrementAnteAction;
import game.actions.NewMatchAction;
import game.actions.PlayerRemovedAction;
import game.actions.ScoreRevealAction;
import game.exceptions.InvalidUpdateException;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class extends the GameStats class and is almost exactly the same except:
 * <li> The size of the ArrayList of MatchStats is capped at 5 (memory space and to
 * make the playing field relatively equal with a human).
 * <li> It does not notify any Observers when the state is changed.
 *
 */

public class PlayerGameStats extends GameStats{
	
	private final int SIZE_OF_MEMORY=5;
	
	public PlayerGameStats()
	{
		super();
	}
	
	/**
	 * Makes a new match the previous match has finished.
	 * Notify all Observers
	 *
	 */
	public void addNewMatchAction(NewMatchAction action)
	{
		matchStats.add(new MatchStats());
		
		if(matchStats.size() % SIZE_OF_MEMORY==0)
			update();
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
		}
		catch(InvalidUpdateException e)
		{assert false;}
	}

	/**
	 * Add a PostExchangeAction to the current MatchStat
	 * Notify Observers
	 * @param action
	 */
	public void addPostExchangeBetAction(BetAction action)
	{
		try
		{
			matchStats.get(matchStats.size()-1).addPostExchangeBetAction(action);
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
			matchStats.get(matchStats.size()-1).addPlayerRemovedAction(action);
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
	}
	
	public void attachGameListener(GameListener g) throws IllegalArgumentException
	{
		throw new IllegalArgumentException();
	}
	
	public void detachGameListener(GameListener g) throws IllegalArgumentException
	{
		throw new IllegalArgumentException();
	}
	
	private void update()
	{
		matchStats.remove(0);
	}
}
