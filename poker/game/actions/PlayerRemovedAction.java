package game.actions;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class is used when a player is removed (loses all of their money or can't pay ante).
 *
 */

public class PlayerRemovedAction implements Action{

	private final String mPlayerRemoved; 
	
	public PlayerRemovedAction(String PlayerName) throws NullPointerException
	{
		if(PlayerName == null)
			throw new NullPointerException();
		
		mPlayerRemoved = PlayerName; 
	}
	
	public String getActionMaker()
	{
		return mPlayerRemoved; 
	}
	
	public String toString()
	{
		return "Removed:" + mPlayerRemoved;
	}
	
	public String toCode()
	{
		return "PlayerRemovedAction"+SEP_CHAR+mPlayerRemoved+END_CHAR + "\n";
	}
}
