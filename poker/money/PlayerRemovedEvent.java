package money;

public class PlayerRemovedEvent implements GamePotEvent {
	
	private int playerID;
	
	public PlayerRemovedEvent(int PlayerID)
	{
		playerID = PlayerID; 
	}

	public int participantID()
	{
		return playerID; 
	}
}
