package game.actions;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This action is used when a new match starts.
 *
 */

public class NewMatchAction implements Action{

	private String actionMaker;
	private int matchID;
	
	public NewMatchAction()
	{
		actionMaker="The Chicken";
		matchID=0;
	}
	
	public NewMatchAction(String actionMaker)
	{
		this.actionMaker=actionMaker;
	}
	
	public String getActionMaker()
	{
		return actionMaker;
	}
	
	public void setMatchID(int ID)
	{
		this.matchID=ID;
	}
	
	public int getMatchID()
	{
		return this.matchID;
	}
	
	public String toString()
	{
		return "\nRound " + matchID + " \nFIGHT!\n";
	}
	
	public String toCode()
	{
		return "NewMatchAction"+SEP_CHAR+matchID+END_CHAR + "\n";
	}
}
