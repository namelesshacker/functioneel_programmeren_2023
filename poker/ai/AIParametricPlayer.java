package ai;

import java.util.ArrayList;

import javax.swing.ImageIcon;

import money.Purse;
import util.*;
import game.*;
import game.exceptions.*;
import game.actions.*;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class represents the Player for AI purposes.  All AI players extend this class.  It 
 * does not serve much purpose except to provide a wrapper for the AIParametricBrain-the Brain 
 * makes all the decisions for the player.
 * 
 * For an AI player to be fully functionable, it extends this class and sets all the Brain's 
 * little helper classes (AIPostBetLimit, AIParametricBetDivider, etc.).
 * 
 */
public class AIParametricPlayer implements AIPlayer {
	
	// The Player's hand
	private Hand myHand;
	// The Player's purse
	protected Purse myPurse;
	// The Player's name
	private final String NAME;
	
	// Informs the player when it is post exchange
	private boolean preBet, postBet;
	
	private int currentAnte;
	
	// THE BRAIN!!!! THE BRAIN!!!
	private AIParametricBrain myBrain;
	
	
	private ImageIcon bigIcon;
	private ImageIcon smallIcon;
	private String smallIconName;
	private String bigIconName;
	
	
	public AIParametricPlayer(String NAME, int purse, int seed)
	{
		this.NAME=NAME;
		myHand = new Hand();
		preBet = false;
		postBet = false;
		currentAnte=0;

		try
		{
			myPurse = new Purse(purse);
		}
		catch(InvalidMoneyValueException e)
		{
			//should not get here
			assert false;
		}
	}
	
	/** The methods for getting Player actions **/
	
	/**
	 * Makes the pre bet action for this player (asks the brain).  It only overrrides the brain
	 * when it sees that the brain has called or raised and the remaining money in the purse is
	 * less than the current ante.  Then, it goes all-in.
	 */
	public final BetAction makePreBetAction(int minBet)
	{
		// If what I have left in my purse is less than ante, go all-in
		try
		{
			if(myPurse.getChips() < currentAnte)
				return new BetAction(NAME, BetAction.betAction.ALL_IN, myPurse.getChips());
		} 
		catch(PurseIsEmptyException e)
		{
			assert false; // should not be asked to bet if purse was already empty
		}
		
		BetAction result = myBrain.getNextPreBet(minBet);
			
		// Return immeadiately if folding (no need to check the amount)
		if(result.getAction() == BetAction.betAction.FOLD)
			return result;
		
		try
		{
			// If the amount left in my purse is less than the ante, just go all-in
			if(myPurse.getChips()-result.getAmount()-minBet < currentAnte)
				return new BetAction(NAME, BetAction.betAction.ALL_IN, myPurse.getChips());
			
			// if your raise is integer deficient (WEIRD!) then just call
			if(BetAction.betAction.RAISE == result.getAction() && result.getAmount() == 0)
				return new BetAction(NAME, BetAction.betAction.CALL, 0);
	
			// If betting an ALL-In amount, dont do raise or call, go all in
			if(result.getAmount() + minBet >= myPurse.getChips())
				return new BetAction(NAME, BetAction.betAction.ALL_IN, myPurse.getChips());
		}
		catch(PurseIsEmptyException e)
		{
			assert false; // should not get here
		}
	
		return result;
	}
	
	/**
	 * Makes the post bet action for this player (asks the brain).  It only overrrides the brain
	 * when it sees that the brain has called or raised and the remaining money in the purse is
	 * less than the current ante.  Then, it goes all-in.
	 */
	public final BetAction makePostBetAction(int minBet)
	{
		//If what I have left in my purse is less than ante, go all-in
		try
		{
			if(myPurse.getChips() < currentAnte)
				return new BetAction(NAME, BetAction.betAction.ALL_IN, myPurse.getChips());
		} 
		catch(PurseIsEmptyException e)
		{
			assert false; // should not be asked to bet if purse was already empty
		}
		
		BetAction result = myBrain.getNextPostBet(minBet);
		
		// Return immeadiately if folding
		if(result.getAction() == BetAction.betAction.FOLD)
			return result;
		
		try
		{
			// If the amount left in my purse is less than the ante, just go all-in
			if(myPurse.getChips()-result.getAmount()-minBet < currentAnte)
				return new BetAction(NAME, BetAction.betAction.ALL_IN, myPurse.getChips());
			
			// if your raise is integer deficient (WEIRD!) then just call
			if(BetAction.betAction.RAISE == result.getAction() && result.getAmount() == 0)
				return new BetAction(NAME, BetAction.betAction.CALL, 0);
	
			// Go ALL_IN if betting the same amount as your purse
			if(result.getAmount() + minBet >= myPurse.getChips())
				return new BetAction(NAME, BetAction.betAction.ALL_IN,myPurse.getChips());
		}
		catch(PurseIsEmptyException e)
		{
			assert false; // should not get here
		}
	
		return result;
	}
	
	/**
	 * Returns the ArrayList of cards to be discarded using the DiscardStrategy from the brain
	 */
	public final ArrayList<Card> discardCards(int max)
	{
		preBet=false;
		postBet=true;
		return myBrain.getDiscardCards();
	}
	
	/**
	 * Makes the score reveal action for this player (from the brain).
	 */
	public final ScoreRevealAction makeScoreRevealAction()
	{
		return new ScoreRevealAction(NAME, myBrain.getScoreRevealAction(), myHand);
	}
	
	/**
	 * Implemented from GameListener
	 */
	public final void notify(Action action)
	{
		// notify the brain of everything going on in the game
		myBrain.notify(action);

		
		if(action instanceof NewGameAction)
		{
			currentAnte=0;
		}
		else if(action instanceof IncrementAnteAction)
		{	
			// Update the current ante
			IncrementAnteAction lAction=((IncrementAnteAction) action);

			if(lAction.getAction() == IncrementAnteAction.incrementAnteAction.SUCCEED)
				currentAnte=lAction.getAmount();
		}
		else if(action instanceof NewMatchAction)
		{
			preBet = true;
			postBet = false;
		}
		else if(action instanceof CardExchangeAction)
		{
			// After discarding your cards, start the postbet initialization
			if(action.getActionMaker().equals(NAME))
				myBrain.startPostBetting();
		}
		else if(action instanceof ScoreAction)
		{
			// update brain's purse when the player's purse is changed
			if(action.getActionMaker().equals(NAME))
				myBrain.setPurse(myPurse);
		}
		else if(action instanceof GetMoneyAction)
		{
			if(action.getActionMaker().equals(NAME))
				myBrain.setPurse(myPurse);
		}
	}
	
	/** Get and Set Methods **/
	
	public final void setBrain(AIParametricBrain brain)
	{
		this.myBrain=brain;
	}
	
	public final void setHand(Hand hand)
	{
		myHand = hand;
		myBrain.setHand(hand);
		
		//After setting the new hand, initialize brain to pre bet
		if(preBet)
			myBrain.startPreBettingRound();
	}
	
	public final Hand getHand()
	{
		return myHand;
	}
	
	public final Purse getPurse()
	{
		return myPurse;
	}
	
	public final void addToPurse(int amount)
	{
		if(amount < 0)
			assert false;
		myPurse.addChips(amount);
		myBrain.setPurse(myPurse);
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
	
	public final String toString()
	{
		return NAME;
	}
}
