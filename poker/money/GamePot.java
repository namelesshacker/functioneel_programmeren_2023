package money;

import game.BettingStatus;
import game.exceptions.InvalidMoneyValueException;
import java.util.ArrayList;


/**
 * 
 * @author david kawrykow and gen kazama
 * 
 * This class represents a Poker Pot. It asks the user to specify 
 * the number of participating entities. The user can then add money
 * from each entity to the pot as in a poker game. Entities can also be
 * made to go "all-in", in which case this class automatically creates
 * a new side pot for the remaining entities. In the same way, entities
 * can be made to "Fold", in which case they are no longer considered
 * as participants. 
 *
 * This class only asks the user to think of the entities as labeled 
 * 0 to n-1 and to refer to them using these integer identities. 
 *
 */

public class GamePot { 
	
	/* Betting Pot and Betting SidePots */
	private ArrayList<Pot> Pots;
	
	/* Betting Status of participants */
	private BettingStatus participants[];
	
	/* Round Starting Boolean */
	private boolean startNewRound = false;
	
	/* Game Pot Listener: can have one */
	private GamePotListener mListener; 
	
	public GamePot(int participants) throws IllegalArgumentException
	{
		if(participants <= 0)
			throw new IllegalArgumentException();
		
		Pots = new ArrayList<Pot>();
		this.participants = new BettingStatus[participants];
		
		for(int i = 0; i < participants; i++)
			this.participants[i] = BettingStatus.STILL_BETTING;
	}
	
	/**
	 * Equivalent to calling the constructor on the pot
	 * @param participants the number of participants
	 */
	public void reset(int participants, ArrayList<String> Participants)
	{
		if(participants <= 0)
			throw new IllegalArgumentException();
		
		Pots = new ArrayList<Pot>();
		this.participants = new BettingStatus[participants];
		
		for(int i = 0; i < participants; i++)
			this.participants[i] = BettingStatus.STILL_BETTING;
		
		if(mListener != null)
			mListener.notify(new SeatingArrangementEvent(Participants)); 
	}
	
	
	/**
	 * Enables further money to be put into the pots even if they are found to be full
	 */
	public void startNewBettingRound()
	{
		startNewRound = true;
	}
	
	/**
	 * @param mListener adds this is as a listener to game pot
	 */
	public void addGamePotListener(GamePotListener mListener)
	{
		this.mListener = mListener;
	}
	
	/**
	 * @param mListener the listener to be removed
	 */
	public void removeGamePotListener(GamePotListener mListener)
	{
		this.mListener = null; 
	}
	
	/**
	 * Removes the participant from the pot
	 * @param participantID the player to be removed
	 */
	public void removeParticipant(int participantID)
	{
		if(mListener != null)
			mListener.notify(new PlayerRemovedEvent(participantID)); 
	}
	
	/**
	 * Adds money from the specified participant. The flag allIn indicates whether
	 * or not the participant is depositing all of his money
	 * @param participantID
	 * @param money
	 * @return whether or not money was added
	 * @throws InvalidMoneyValueException
	 * @throws IllegalArgumentException If the participantID should not be operated on
	 * for whatever reason.
	 */
	public boolean addBettingMoney(int participantID, int money, boolean allIn) 
		   throws InvalidMoneyValueException, IllegalArgumentException 
	{
		int i = 0;
		int remainingMoney = 0;
		Pot iPot;
	
		validateParticipantID(participantID);
		
		/* Is participant even allowed to make another bet? */
		if(participants[participantID] != BettingStatus.STILL_BETTING)
			throw new IllegalArgumentException();
		
		/* If first time this function is called, create a pot */
		if(Pots.size() == 0)
			addNewPot();
		
		/* Otherwise find lowest empty pot for which player has not paid */
		else
			i = findLowestEmptyPot(participantID);
		
		/* If no more empty pots and not beginning of a betting round
		 * then no money can be added (we are done the round)*/
		if( i == Pots.size() && !startNewRound)
			return false;
		/* Otherwise if we are starting a new round we can add the money to the highest pot
		 * except if that pot was sealed with an all-in */
		else if (i == Pots.size() && startNewRound)
		{
			int k; 
			
			// Check whether any player has gone all in before this
			for(int j = 0; j < participants.length; j++)
			{
				// if someone has gone all in the previous rounds, check which pot
				// that player sealed
				if(participants[j] == BettingStatus.WENT_ALL_IN)
				{
					k = findLowestEmptyPot(j);
					
					// if he sealed the last pot -> we must make a new pot!
					if ( k == Pots.size() )
					{
						addNewPot(); 
						break; 
					}
				}
			}
			
			// Now just add the money to the latest pot -> this could be the top pot from
			// the previous round, or a NEW pot because the previous one was sealed via an all in
			if(!allIn)
				Pots.get(Pots.size() - 1).addPlayerBet(participantID, money);
			else
			{
				Pots.get(Pots.size() - 1).addPlayerBet(participantID, money);
				participants[participantID] = BettingStatus.WENT_ALL_IN;
			}
			
			// if money was actually added! then subsequent bets will now work towards
			// ending the round
			if(money > 0)
				startNewRound = false;
			// else, we consider the round still "new". If we made a new pot because of 
			// all-in sealing, then this time the highest pot will not contain any all-in
			// players, and we wont make new pots. 
			else
				startNewRound = true; 
			
			// notify the listener
			if(mListener != null)
				mListener.notify(new MoneyAddedEvent());
			
			return true;
		}
			
		/* Set the remainingMoney to the initial value to prepare for bubbling */
		remainingMoney = money;
		
		/* We now bubble in the player's money according to whether the player 
		 * is going all-in or just adding money 
		 */
		iPot = Pots.get(i);
		
		/* ALL IN */
		if(allIn)
		{	
			/* Bubble up in lower pots */
			while( remainingMoney + iPot.getPlayerBet(participantID) 
					> iPot.getAllIn() && i != Pots.size() - 1 )
			{
				remainingMoney = iPot.bubbleIn( participantID, money );
				i++; 
				iPot = Pots.get(i);
			}
			
			/* Remaining money meets requirements of last possible pot exactly 
			 * i.e. an empty pot is returned and we ignore this empty pot */
			if(remainingMoney + iPot.getPlayerBet(participantID) == iPot.getAllIn())
			{
				iPot.allIn(participantID, remainingMoney);
			}
			
			/* Otherwise we check if highest pot is already fixed by a previous
			 * all in. */
			
			/* If pot is not fixed or if any input bet splits the current pot */
			else if(!isPotFixed(i+1) || 
					remainingMoney + iPot.getPlayerBet(participantID) < iPot.getAllIn())
			{	
				/* If pot is not fixed treat all-in as a pot-fixing regular
				 * bet */
				if(remainingMoney + iPot.getPlayerBet(participantID) > iPot.getAllIn())
					iPot.addPlayerBet(participantID, remainingMoney);
				
				/* Otherwise treat current all-in as a pot-splitting action */
				else if(i == Pots.size() - 1)
				{
					Pots.add(iPot.allIn(participantID, remainingMoney) );
				}
				else
				{
					Pots.add(i+1, iPot.allIn(participantID, remainingMoney) );
				}
			}
			
			/* If pot is fixed and the current bet exceeds its fixed value,
			 * then it must be the highest pot and then the current all-in 
			 * must first be bubbled in, and the remainder used to define 
			 * the newest highest pot */
			else 
			{
				remainingMoney = iPot.bubbleIn(participantID, remainingMoney);
				addNewPot();
				Pots.get(Pots.size() - 1).addPlayerBet(participantID, remainingMoney);
			}
			
			/* Remove player from subsequent bets */
			participants[participantID] = BettingStatus.WENT_ALL_IN;
		}
		
		/* JUST ADDING MONEY */
		else
		{
			/* If player is not adding enough money */
			if(!isEnough(participantID, remainingMoney))
				throw new InvalidMoneyValueException();
			
			/* Otherwise bubble up within lower pots */
			while( i != Pots.size() - 1 )
			{
				remainingMoney = iPot.bubbleIn( participantID, remainingMoney );
				i++; 
				iPot = Pots.get(i);
			}
			
			/* Once at highest pot check if anyone has fixed the pot's 
			 * all-in value by having gone all-in. If not, then the pot's
			 * all-in value can be increased by adding a fresh bet.*/
			if( !isPotFixed(i+1) || 
				remainingMoney + iPot.getPlayerBet(participantID) == iPot.getAllIn()
			  )
			{
				iPot.addPlayerBet(participantID, remainingMoney);
			}
			
			/* Otherwise adding the current money will cause the inputting player's
			 * money to exceed the fixed all-in. We thus bubble in the money and 
			 * start a new pot initialized to hold only the remaining money. If the 
			 * bubbled up pot was empty (ie. in the case where remainingMoney == 0)
			 * we ignore the pot and simply proceed as above. 
			 */
			else 
			{
				remainingMoney = iPot.bubbleIn(participantID, remainingMoney);
				addNewPot();
				Pots.get(Pots.size() - 1).addPlayerBet(participantID, remainingMoney);
			}
		}
		
		// notify the listener
		if(mListener != null)
			mListener.notify(new MoneyAddedEvent());
		
		return true;
	}
	
	public int getHighestPotOfParticipant(int participantID) throws IllegalArgumentException
	{
		
		if(participantID < 0)
			throw new IllegalArgumentException();
		
		boolean[] pls;
		
		for(int i = getNumberOfSmallerPots(); i > 0; i--)
		{
			pls = playersInPot(i);
			
			if(pls[participantID] == true)
				return i;
		}
		
		return 1;
	}
	
	
	/** 
	 * The input participant is no longer participating in the scheme 
	 * @param participantID
	 * @return Money the participant won because he aborted
	 * @throws IllegalArgumentException When ID is not valid or when trying to FOLD 
	 * for the last player in the pot. 
	 */
	public int abortParticipant(int participantID) throws IllegalArgumentException
	{	
		validateParticipantID(participantID);
		
		int c = 0;
		int returnValue = 0;
		int l;
		int m = 5; 	// records the lowest pot in which player won money
		
		for(int i = 0; i < participants.length; i++)
			if(participants[i] != BettingStatus.FOLDED)
				c++;
		
		if(c < 2)
			throw new IllegalArgumentException(); 
		
		l = findLowestEmptyPot(participantID); 
		
		for(int i = getNumberOfSmallerPots(); i > 0 ; i--)
			if(numPlayersInPot(i) == 1 && l >= i)
			{
				returnValue += getMoneyInPot(i);
				m = Math.min(m, i); 				// keep the lowest pot in which player won money
			}
		
		participants[participantID] = BettingStatus.FOLDED;
		
		// notify the listener of the changes 
		if(mListener != null && returnValue == 0)
			mListener.notify(new PlayerAbortedEvent(participantID, -1));
		else if(mListener != null && returnValue != 0)
			mListener.notify(new PlayerAbortedEvent(participantID,  m));
		
		return returnValue;
	}
	
	/**
	 * @return The number of sub-pots within the game pot
	 */
	public int getNumberOfSmallerPots()
	{
		return Pots.size();
	}
	
	/**
	 * @param potNumber
	 * @return The total value to be won from the specified side pot
	 * @throws IllegalArgumentException When the pot number is not valid
	 */
	public int getMoneyInPot(int potNumber) throws IllegalArgumentException
	{
		if(potNumber < 1 || potNumber > getNumberOfSmallerPots())
			throw new IllegalArgumentException();			

		return Pots.get(potNumber - 1).getMoneyInPot();
	}
	
	/**
	 * Returns a boolean array indicating whether the (index + 1)th player is
	 * in the specified pot or not. Calling the abortPlayer function on a player
	 * will set that player's status to false, regardless of input money 
	 * @param potNumber The pot in question (1, 2, 3, or 4)
	 * @return
	 * @throws IllegalArgumentException
	 */
	public boolean[] playersInPot(int potNumber) throws IllegalArgumentException
	{
		if(potNumber < 1 || potNumber > getNumberOfSmallerPots())
			throw new IllegalArgumentException();
		
		boolean[] arePlayersInPot = new boolean[participants.length];
		Pot x = Pots.get(potNumber - 1);
		
		for(int i = 0; i < arePlayersInPot.length; i++)
		{
			arePlayersInPot[i] = 
				( ((x.getPlayerBet(i) != 0) && (participants[i] != BettingStatus.FOLDED)));
		}
		
		return arePlayersInPot;
	}
	
	private int numPlayersInPot(int potNumber) throws IllegalArgumentException
	{
		boolean[] p = playersInPot(potNumber);
		
		int c = 0;
		
		for(int i = 0; i < p.length; i++)
			if(p[i])
				c++;
		
		return c;
	}
	
	/**
	 * Sets up the main game pot (the one holding antes)
	 * @param participantID The first participant contributing
	 * @param amount The amount contributed
	 */
	private void addNewPot()
			throws InvalidMoneyValueException
	{	
		Pots.add(new Pot(participants.length));
	}
	
	/**
	 * Find the lowest pot still requiring dues from the participant. We assume
	 * that the player is still allowed to make bets
	 * @param participantID
	 * @return
	 */
	private int findLowestEmptyPot(int participantID)
	{
		int i;
		Pot iPot;
		
		for(i = 0; i < Pots.size(); i++)
		{
			iPot = Pots.get(i);
			
			if( !iPot.isFull() )
				if(!iPot.playerMatchedHighestBid(participantID))
					break;		
		}
		
		return i;
	}
	
	/**
	 * Is the input money enough to cover debts in pots still needing
	 * money from the input player.  
	 * @param amount Can this amount be successfully be added
	 * @return Yes or no
	 */
	private boolean isEnough(int playerID, int amount)
	{
		int total = 0;
		Pot iPot;
		
		for(int i = 0; i < Pots.size(); i++)
		{
			iPot = Pots.get(i); 
			
			if(!iPot.isFull())
			{
				total += iPot.getAllIn() - iPot.getPlayerBet(playerID);
			}
		}
		
		if(amount < total)
			return false;
		
		return true;
	}
	
	/**
	 * Checks whether or not an "all-in" player has fixed the input pot
	 */
	private boolean isPotFixed(int potNumber)
	{
		try{
			boolean[] playersInPot = playersInPot(potNumber);
			
			for(int i = 0; i < playersInPot.length; i++)
				if( playersInPot[i] && (participants[i] == BettingStatus.WENT_ALL_IN))
					return true;
		} 
		catch (Exception e)
		{
			assert false;
		}
		
		return false;
	}
	
	/** 
	 * Validates the participantID from external calling methods
	 * @param participantID
	 * @throws IllegalArgumentException
	 */
	private void validateParticipantID(int participantID) throws IllegalArgumentException
	{
		if(participantID < 0 || participantID >= participants.length)
			throw new IllegalArgumentException();
		
		if(participants[participantID] == BettingStatus.FOLDED)
			throw new IllegalArgumentException();
	}
	
	public int getMinBet(int index)
	{
		int result=0;
		
		for(int i=0; i<Pots.size(); i++)
			result+=Pots.get(i).getAllIn()-Pots.get(i).getPlayerBet(index);
		return result;
	}
	
	public String toString()
	{
		String s = "";
		
		for(int i = 0; i < Pots.size(); i++)
			s += Pots.get(i);
		
		return s;
	}

}
