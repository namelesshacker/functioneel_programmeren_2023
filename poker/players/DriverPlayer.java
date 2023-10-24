package players;

import game.PlayerGameStats;
import game.actions.*;
import game.exceptions.*;
import java.util.ArrayList;

import javax.swing.ImageIcon;

import money.Purse;


import util.Card;
import util.Hand;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class is used for the driver.  It allows users to set its actions for a match and is
 * therefore useful when running the driver.
 * 
 */
public class DriverPlayer implements Player{
	
	private String name;
	private Purse purse;
	private Hand hand;
	
	private BetAction preBetAction;
	private BetAction postBetAction;
	private ScoreRevealAction scoreRevealAction;
	
	private PlayerGameStats myGameStats;
	private boolean postExchange=false;
	
	
	private ImageIcon bigIcon;
	private ImageIcon smallIcon;
	private String smallIconName;
	private String bigIconName;
	
	public DriverPlayer()
	{
		name="Driver Player";
		hand=new Hand();
		myGameStats=new PlayerGameStats();
		
		try
		{
			purse=new Purse(0);
		}
		catch(InvalidMoneyValueException e)
		{ /* Should not get here */ }
		
		initializeActions();
	}
	
	public DriverPlayer(String name, int purse)
	{
		this.name=name;
		hand=new Hand();
		myGameStats=new PlayerGameStats();
		try
		{
			this.purse=new Purse(purse);
		}
		catch(InvalidMoneyValueException e)
		{}
		
		initializeActions();
	}
	
	/**
	 * Initialize the actions to their defaults.
	 *
	 */
	private void initializeActions()
	{
		preBetAction=new BetAction(name, BetAction.betAction.FOLD, 0);
		postBetAction=new BetAction(name, BetAction.betAction.FOLD, 0);
		scoreRevealAction=new ScoreRevealAction(name,
												ScoreRevealAction.scoreRevealAction.FOLD,
												hand);
	}
	
	/**
	 * 
	 * @param minBet
	 * @return User defined pre bet action.
	 */
	public BetAction makePreBetAction(int minBet)
	{
		return preBetAction;
	}
	
	/**
	 * 
	 * @param minBet
	 * @return user defined post bet action
	 */
	public BetAction makePostBetAction(int minBet)
	{		
		return postBetAction;
	}
	
	/**
	 * 
	 * @return User defined Score reveal action
	 */
	public ScoreRevealAction makeScoreRevealAction()
	{
		return new ScoreRevealAction(name, scoreRevealAction.getAction(), hand);
	}
	
	/**
	 * Always swap highest card
	 * @param max
	 * @return
	 */
	public ArrayList<Card> discardCards(int max)
	{
		ArrayList<Card> result=new ArrayList<Card>();
		result.add( hand.getHighCard());
		return result;
	}

	
	/**
	 * Inherited from the GameListener interface
	 */
	public void notify(Action action)
	{
		if( action instanceof CardExchangeAction)
		{
			myGameStats.addCardExchangeAction((CardExchangeAction) action);
			postExchange=true;
		}
		else if (action instanceof IncrementAnteAction)
		{
			myGameStats.addIncrementAnteAction((IncrementAnteAction) action);
		}
		else if(action instanceof BetAction)
		{
			if(postExchange)
				myGameStats.addPostExchangeBetAction((BetAction) action);
			else
				myGameStats.addPreExchangeAction((BetAction) action);
		}
		else if(action instanceof NewMatchAction)
		{
			myGameStats.addNewMatchAction((NewMatchAction) action);
			postExchange=false;
		}
		else if(action instanceof PlayerRemovedAction)
		{
			myGameStats.addPlayerRemovedAction((PlayerRemovedAction) action);
		}
		else if(action instanceof ScoreRevealAction)
		{
			myGameStats.addScoreRevealAction((ScoreRevealAction) action);
			postExchange=false;
		}
		else if(action instanceof GetMoneyAction)
		{
			myGameStats.addGetMoneyAction((GetMoneyAction) action);
		}
		else if(action instanceof ScoreAction)
		{
			myGameStats.addScoreAction((ScoreAction) action);
		}
	}
	
	/* Set and Get funcitons */
	public void setPreBetAction(BetAction ba)
	{
		preBetAction=ba;
	}
	
	public void setPostBetAction(BetAction ba)
	{
		postBetAction=ba;
	}
	
	public void setScoreRevealAction(ScoreRevealAction sra)
	{
		scoreRevealAction=sra;
	}
	
	public void setHand(Hand hand)
	{
		this.hand=hand;
	}
	
	public Hand getHand()
	{
		return hand;
	}
	
	public Purse getPurse()
	{
		return purse;
	}
	
	public void addToPurse(int amount)
	{
		purse.addChips(amount);
	}
	
	public final void setSmallIcon(String s)
	{
		smallIcon = new ImageIcon(s);
		smallIconName = s;
	}
	
	public final void setBigIcon(String s)
	{
		bigIcon = new ImageIcon(s);
		bigIconName = s;
	}
	
	public final ImageIcon getSmallIcon()
	{
		return smallIcon;
	}
	
	public final ImageIcon getBigIcon()
	{
		return bigIcon;
	}
	
	public final String getBigIconName()
	{
		return bigIconName;
	}
	
	public final String getSmallIconName()
	{
		return smallIconName;
	}
	
	public String toString()
	{
		return name;
	}
}
