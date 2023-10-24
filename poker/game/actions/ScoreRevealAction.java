package game.actions;
import scoring.HandValue;
import util.Hand;
import game.exceptions.HandFoldedException;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This action class is a wrapper class for the enum scoreRevealAction.
 * scoreRevealAction represents the choices the player has at the end of the game:
 * fold or reveal their hands.
 * 
 */


public class ScoreRevealAction implements Action{
	private final String actionMaker;
	private final scoreRevealAction action;
	private Hand hand;
	private HandValue handValue;
	
	public enum scoreRevealAction
	{ FOLD, SHOW, NO_SHOW }
	
	public ScoreRevealAction(String actionMaker, scoreRevealAction action, Hand hand) throws IllegalArgumentException
	{
		if(actionMaker==null)
			throw new IllegalArgumentException();
		
		this.actionMaker=actionMaker;
		this.action=action;
		this.hand=hand;
	}
	
	public void setHandValue(HandValue handValue)
	{
		this.handValue = handValue;
	}
	
	/**
	 * Returns the hand, but if the action is set to folded, obviously 
	 * outside sources cannot have access to the hand.
	 * @return
	 * @throws HandFoldedException
	 */
	public Hand getHand() throws HandFoldedException
	{
		if(action==scoreRevealAction.FOLD || action == scoreRevealAction.NO_SHOW)
			throw new HandFoldedException();
		return hand;
	}
	
	public HandValue getHandValue() throws HandFoldedException
	{
		if(action==scoreRevealAction.FOLD || action == scoreRevealAction.NO_SHOW)
			throw new HandFoldedException();
		return handValue;
	}
	
	public String getActionMaker()
	{
		return actionMaker;
	}
	
	public scoreRevealAction getAction()
	{
		return action;
	}
	
	/**
	 * Regular toString method but does not display hand if folded (for obvious reasons).
	 */
	public String toString()
	{
		return "          ";
	}
	
	public String toCode()
	{
		String mHandString;
		
		if(action != scoreRevealAction.SHOW)
			mHandString = "";
		else
			mHandString = SEP_CHAR + hand.toString() + SEP_CHAR + handValue.toString();
		
		return "ScoreRevealAction"+SEP_CHAR+actionMaker+SEP_CHAR+action+mHandString+END_CHAR+"\n";
	}
}
