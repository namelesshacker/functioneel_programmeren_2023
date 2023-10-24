package game.actions;

import scoring.HandValue;
import util.Hand;

/**
 * This class is used to remember the hand that the user got. It is not sent out by game model.
 */
public class UserSetHandAction implements Action {
	
	private String mActionMaker;
	private Hand   mHand;
	private HandValue mHandValue;
	
	public UserSetHandAction(String user, Hand hand, HandValue value)
	{
		mActionMaker = user;
		mHand        = hand;
		mHandValue   = value;
	}

	public String getActionMaker()
	{
		return mActionMaker;
	}
	
	public Hand getHand()
	{
		return mHand;
	}
	
	public HandValue getHandValue()
	{
		return mHandValue;
	}
	
	public String toString()
	{
		return "                ";
	}
	
	public String toCode()
	{
		return "UserSetHandAction" + SEP_CHAR + mActionMaker + SEP_CHAR + mHand + SEP_CHAR + mHandValue + END_CHAR + "\n";
	}
}
