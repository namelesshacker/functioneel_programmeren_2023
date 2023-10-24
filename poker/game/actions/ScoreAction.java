package game.actions;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This action is used to tell how much money each player has
 * 
 */
public class ScoreAction implements Action{

	int amount;
	String actionMaker;
	
	public ScoreAction(String actionMaker, int amount)
	{
		this.actionMaker=actionMaker;
		this.amount=amount;
	}

	public String getActionMaker()
	{
		return actionMaker;
	}
	
	public int getAmount()
	{
		return amount;
	}

	public String toString()
	{
		return "Score:\t" + actionMaker + " " + amount ;
	}
	
	public String toCode()
	{
		return "ScoreAction"+SEP_CHAR+actionMaker+SEP_CHAR+amount+END_CHAR + "\n";
	}
}
