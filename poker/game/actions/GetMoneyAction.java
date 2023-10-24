package game.actions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This action is used when a player wins a pot in a match (remember,
 * a player can win a sidepot, and not win the match).  It takes in the
 * name of the player, the pot id, the amount and the winning hand.
 *
 */

public class GetMoneyAction implements Action{
	
	private String actionMaker;
	private int pot;
	private int amount;
	
	public GetMoneyAction(String actionMaker, int pot, int amount)
	{
		this.actionMaker=actionMaker;
		this.pot=pot;
		this.amount=amount;
	}
	
	public String getActionMaker()
	{
		return actionMaker;
	}
	
	public int getPot()
	{
		return pot;
	}
	
	public int getAmount()
	{
		return amount;
	}
	
	public String toString()
	{
		return "Money:\t" + actionMaker + " receives " + amount + " from pot " + pot;
	}
	
	public String toCode()
	{
		return "GetMoneyAction"+SEP_CHAR+actionMaker+SEP_CHAR+pot+SEP_CHAR+amount+END_CHAR + "\n";
	}
}
