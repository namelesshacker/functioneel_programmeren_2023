package game.actions;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This class represents the actions possible when exchanging cards.
 * A player has the option of discarding a number of cards (which is stored in the
 * variable amount), or discarding no cards.  These two options are represented by
 * the enum cardExchangeAction.
 */

public class CardExchangeAction implements Action{
	
	private final String actionMaker; 
	private final int amount;
	private final cardExchangeAction action;
	private final int maxCardsExchanged = 3; 
	
	public enum cardExchangeAction
	{ DISCARD, NO_DISCARD}
	
	public CardExchangeAction(String actionMaker, cardExchangeAction action, int amount ) throws IllegalArgumentException
	{
		if(actionMaker == null)
			throw new IllegalArgumentException();
		
		if(action != cardExchangeAction.DISCARD)
			this.amount = 0; 
		else if(amount<=0 || amount > maxCardsExchanged)
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
	
	public cardExchangeAction getAction()
	{
		return action;
	}
	
	public int getAmount()
	{
		return amount;
	}
	
	public String toString()
	{
		return "Discard " + amount + " "; 
	}
	
	public String toCode()
	{
		return "CardExchangeAction"+SEP_CHAR+actionMaker+SEP_CHAR+action+SEP_CHAR+amount+END_CHAR + "\n";
	}
}
