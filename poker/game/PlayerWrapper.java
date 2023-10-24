package game;

import players.Player;
import game.exceptions.InvalidUpdateException;

/**
 * @class PlayerWrapper
 * @author david kawrykow and gen kazama
 * A wrapper class used to wrap a Player and a BettingStatus together into a single object. 
 * Its C equivalent is a struct. 
 */

public class PlayerWrapper {
	
	private final Player mPlayer;
	private BettingStatus mBettingStatus;
	private final int mPlayerID;
	private boolean canSetStatus = true;
	
	public PlayerWrapper(Player mPlayer, BettingStatus status, int playerID)
	{
		this.mPlayer = mPlayer;
		this.mBettingStatus = status;
		this.mPlayerID = playerID;
	}
	
	public Player getPlayer()
	{
		return mPlayer;
	}
	
	public BettingStatus getBettingStatus()
	{
		return mBettingStatus;
	}
	
	public void setBettingStatus(BettingStatus newStatus) throws InvalidUpdateException
	{
		if(canSetStatus)
			mBettingStatus = newStatus;
		else
			throw new InvalidUpdateException();
	}
	
	public int getPlayerID()
	{
		return mPlayerID;
	}
	
	public final String toString()
	{
		return mPlayer.toString();
	}
}
