package game.actions;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 */

/**
 * 
 * This class represents the actions a player can do when betting:
 * Folding, checking, calling and raising.  The amount variable stores
 * the amount of chips bet in the case of raising.
 *
 */

public class BetAction implements Action {
	
	private final String actionMaker; 
	private final int amount;
	private final betAction action;
	
	public enum betAction
	{ FOLD, CHECK, CALL, RAISE, ALL_IN }
	
	public BetAction( String actionMaker, betAction action, int amount ) throws IllegalArgumentException
	{
		if(actionMaker == null)
			throw new IllegalArgumentException();
		
		if(action != betAction.RAISE && action != betAction.ALL_IN)
			this.amount = 0; 
		else if(amount<=0)
			throw new IllegalArgumentException();
		else
			this.amount=amount;

		
		this.actionMaker = actionMaker; 
		this.action=action;
	}
	
	public String getActionMaker()
	{
		return actionMaker; 
	}
	
	public betAction getAction()
	{
		return action;
	}
	
	public int getAmount()
	{
		return amount;
	}
	
	public String toString()
	{
		String t = "";
		
		if(action == betAction.RAISE)
		{
			t = "Raise " + amount;
			
			if(amount < 10)
				t += "   ";
			else if(amount < 100)
				t += "  ";
			else
				t += " ";
		}
		
		else if(action == betAction.CALL)
			t = "Call      ";
		
		else if(action == betAction.ALL_IN)
			t = "All in!   ";
		
		return t;
	}
	
	public String toCode()
	{
		return "BetAction"+SEP_CHAR+actionMaker+SEP_CHAR+action+SEP_CHAR+amount+END_CHAR + "\n";
	}
}

