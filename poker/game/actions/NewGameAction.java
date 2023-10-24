package game.actions;

import java.util.ArrayList;

public class NewGameAction implements Action {
	
	private final String actionMaker; 
	private ArrayList<String>  GameParticipants = new ArrayList<String>();
	private ArrayList<Integer> GameMoneyValues  = new ArrayList<Integer>();
	private ArrayList<String>  GameBigIcons		= new ArrayList<String>();
	private ArrayList<String>  GameSmallIcons	= new ArrayList<String>();
	private int TournamentRound = -1;
	private int DifficultySetting = 2;
	
	/**
	 * Deprecated Constructor
	 */
	public NewGameAction( String actionMaker, ArrayList<String> GameParticipants)
	{
		if(actionMaker == null || GameParticipants.size() == 0)
			throw new IllegalArgumentException();
		
		this.actionMaker = actionMaker;
		
		for(int i = 0; i < GameParticipants.size(); i++)
		{
			this.GameParticipants.add(GameParticipants.get(i));
			this.GameMoneyValues.add(GameMoneyValues.get(i));
		}
	}
	
	public NewGameAction( String actionMaker, ArrayList<String> GameParticipants, ArrayList<Integer> GameMoneyValues,
						  ArrayList<String> bIcons, ArrayList<String> sIcons) 
		throws IllegalArgumentException
	{
		if(actionMaker == null || GameParticipants.size() == 0)
			throw new IllegalArgumentException();
		
		this.actionMaker = actionMaker;
		
		for(int i = 0; i < GameParticipants.size(); i++)
		{
			this.GameParticipants.add(GameParticipants.get(i));
			this.GameMoneyValues.add(GameMoneyValues.get(i));
			this.GameBigIcons.add(bIcons.get(i));
			this.GameSmallIcons.add(sIcons.get(i));
		}
	}
	
	public void setTournamentRound(int Round)
	{
		TournamentRound = Round;
	}
	
	public void setDifficultySetting(int diff)
	{
		DifficultySetting = diff;
	}
	
	public String getActionMaker()
	{
		return actionMaker; 
	}
	
	public int getNumberOfParticipants()
	{
		return this.GameParticipants.size();
	}
	
	/**
	 * @pre participantNumber >= 1 , <= number of players
	 */
	public String getParticipant(int participantNumber) throws IllegalArgumentException
	{
		if(participantNumber <= GameParticipants.size() 
				&& participantNumber > 0)
			return GameParticipants.get(participantNumber-1);
		
		throw new IllegalArgumentException();
	}

	public int getParticipantMoney(int participantNumber) throws IllegalArgumentException
	{
		if(participantNumber <= GameParticipants.size() 
				&& participantNumber > 0)
			return GameMoneyValues.get(participantNumber-1).intValue();
		
		throw new IllegalArgumentException();
	}
	
	public String getParticipantBigIcon(int participantNumber) throws IllegalArgumentException
	{
		if(participantNumber <= GameParticipants.size() 
				&& participantNumber > 0)
			return GameBigIcons.get(participantNumber-1);
		
		throw new IllegalArgumentException();
	}
	
	public String getParticipantSmallIcon(int participantNumber) throws IllegalArgumentException
	{
		if(participantNumber <= GameParticipants.size() 
				&& participantNumber > 0)
			return GameSmallIcons.get(participantNumber-1);
		
		throw new IllegalArgumentException();
	}
	
	public String toString()
	{
		return "New Game created by "+actionMaker;
	}
	
	public int getTournamentRound()
	{
		return TournamentRound;
	}
	
	public int getDifficultySetting()
	{
		return DifficultySetting;
	}
	
	public String toCode()
	{
		String toReturn = "NewGameAction";
		
		for(int i = 0; i < GameParticipants.size(); i++)
			toReturn += SEP_CHAR + GameParticipants.get(i) + SEP_CHAR 
					 +  GameMoneyValues.get(i).toString() + SEP_CHAR
					 +  GameBigIcons.get(i) + SEP_CHAR
					 +	GameSmallIcons.get(i);
		
		toReturn +=  SEP_CHAR + Integer.toString(TournamentRound);
		toReturn +=  SEP_CHAR + Integer.toString(DifficultySetting);
		
		return toReturn + END_CHAR + "\n";
	}
}
