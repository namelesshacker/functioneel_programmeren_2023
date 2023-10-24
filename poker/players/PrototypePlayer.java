package players;

import game.GameListener;
import game.PlayerGameStats;
import game.actions.Action;
import game.actions.BetAction;
import game.actions.CardExchangeAction;
import game.actions.GetMoneyAction;
import game.actions.IncrementAnteAction;
import game.actions.NewMatchAction;
import game.actions.PlayerRemovedAction;
import game.actions.ScoreAction;
import game.actions.ScoreRevealAction;
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
 * This prototype player was created purely for testing purposes.  It follows a clear cut
 * approach for all desicions in a poker game (ie, always discarding the top card during
 * the discard phase).
 *
 */

public class PrototypePlayer implements Player, GameListener{

	private Purse myPurse;
	private Hand myHand;
	private PlayerGameStats myGameStats;
	private String myName;
	private boolean postExchange;
	
	private BetAction lastAction;
	
	private ImageIcon bigIcon;
	private ImageIcon smallIcon;
	private String smallIconName;
	private String bigIconName;
	
	public PrototypePlayer()
	{
		try{
			myPurse=new Purse(0);
		}
		catch(InvalidMoneyValueException e){}
		myHand=new Hand();
		myGameStats=new PlayerGameStats();
		myName="Adam";
		postExchange=false;
		lastAction = null;
	}

	public PrototypePlayer(String myName, int amount)
	{
		try{
			myPurse=new Purse(amount);
		}
		catch(InvalidMoneyValueException e){}
		this.myName=myName;
		myHand=new Hand();
		myGameStats=new PlayerGameStats();
		postExchange=false;
		lastAction=null;
	}
	
	/**
	 * Sends out a BetAction for the Pre-Exchange round of betting
	 */
	public BetAction makePreBetAction(int minBet)
	{
		try
		{
			/* If this player is the first, it raises 5 (or goes all in if it cannot raise 5*/
			if(lastAction==null)
				if(getPurse().getChips() < minBet + 5)
					return new BetAction(this.toString(), BetAction.betAction.ALL_IN, getPurse().getChips());
				else
					return new BetAction(this.toString(), BetAction.betAction.RAISE, 5);
			/* If this player is not the first, it calls the bet on the table */
			else
			{
				if(getPurse().getChips() < minBet)
					return new BetAction(this.toString(), BetAction.betAction.ALL_IN, getPurse().getChips());
				else
					return new BetAction(this.toString(), BetAction.betAction.CALL, 5);
			}
		}
		catch (Exception e)
		{ }
		return new BetAction(this.toString(), BetAction.betAction.CALL, 0);
	}
	
	/**
	 * Sends out a BetAction for the Pre-Exchange round of betting
	 * (same as makePreBetAction() in the case of this prototype player.
	 */
	public BetAction makePostBetAction(int minBet)
	{
		return makePreBetAction(minBet);
	}

	/**
	 * Sends out an ArrayList of cards to be discarded from the hand
	 * For this prototype, always discards the highest card.
	 */
	public ArrayList<Card> discardCards(int max)
	{
		ArrayList<Card> x = new ArrayList<Card>();
		x.add(myHand.getHighCard());
		return x;
	}
	
	
	/**
	 * Returns a ScoreRevealAction (show the hand or fold) 
	 */
	public ScoreRevealAction makeScoreRevealAction()
	{
		return new ScoreRevealAction(toString(), ScoreRevealAction.scoreRevealAction.SHOW, myHand);
	}
	
	/**
	 * This method takes in an action and adds it to its GameStats accordingly.
	 * It is used by any class implementing GameSubject to notify this object of a change.
	 */
	public void notify(Action action)
	{
		if( action instanceof CardExchangeAction)
		{
			myGameStats.addCardExchangeAction((CardExchangeAction) action);
			postExchange=true;
			lastAction=null;
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
			lastAction=(BetAction) action;
		}
		else if(action instanceof NewMatchAction)
		{
			myGameStats.addNewMatchAction((NewMatchAction) action);
			postExchange=false;
			lastAction=null;
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
	
	public Action getLastReceivedAction()
	{
		return lastAction;
	}
	
	public Purse getPurse()
	{
		return myPurse;
	}
	
	public void addToPurse(int m)
	{
		myPurse.addChips(m);
	}

	public void setHand(Hand hand)
	{
		myHand=hand;
	}
	
	public Hand getHand()
	{
		return myHand;
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
		return myName;
	}
}
