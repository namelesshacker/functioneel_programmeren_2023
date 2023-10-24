package game.actions;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This action class is a wrapper class for the enum incrementAnteAction.
 * incrementAnteAction represents the choices the game has at the end of each round:
 * increment the ante (succeed) or keep the ante the same (fail).
 * 
 */

public class IncrementAnteAction implements Action{
	private final String actionMaker;
	private final incrementAnteAction action;
	private final int amount;	
	
	public enum incrementAnteAction
	{ SUCCEED, FAIL, NULL }
	
	public IncrementAnteAction(String actionMaker, incrementAnteAction action, int amount ) 
	throws IllegalArgumentException
	{	
		if(actionMaker == null || action == null)
			throw new IllegalArgumentException();
		
		this.actionMaker=actionMaker;
		this.action=action;
		this.amount=amount;
	}
	
	public String getActionMaker()
	{
		return actionMaker;
	}
	
	public incrementAnteAction getAction()
	{
		return action;
	}
	
	public int getAmount()
	{
		return amount;
	}
	
	public String toString()
	{
		return "Ante is: " + amount;
	}
	
	public String toCode()
	{
		return "IncrementAnteAction"+SEP_CHAR+actionMaker+SEP_CHAR+action+SEP_CHAR+amount+END_CHAR + "\n";
	}
}
