package money;

import java.util.ArrayList;

public class SeatingArrangementEvent implements GamePotEvent {
	
	private ArrayList<String> Participants; 
	
	public SeatingArrangementEvent(ArrayList<String> p)
	{
		assert (p != null); 
		
		Participants = new ArrayList<String>(); 
		
		for(int i = 0; i < p.size(); i++)
			Participants.add(p.get(i)); 
	}
	
	public int participantID()
	{
		return 0; 
	}
	
	public ArrayList<String> getParticipants()
	{
		return Participants; 
	}

}
