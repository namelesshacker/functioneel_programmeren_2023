package money;

public class PlayerAbortedEvent implements GamePotEvent {

	private int ID; 
	private int StillInHigherPots;
	
	public PlayerAbortedEvent(int ID, int StillInHigherPots)
	{
		this.ID = ID; 
		this.StillInHigherPots = StillInHigherPots; 
	}
	
	public int participantID()
	{
		return ID; 
	}
	
	public int getHigherPots()
	{
		return StillInHigherPots; 
	}
}
