package money;

import game.exceptions.InvalidMoneyValueException;
import game.exceptions.PurseIsEmptyException;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class represents one pot in a game.  It deals with situations of creating new pots (side pots)
 * and also provides a safe way to store players' bets.
 * 
 * The Purse array (named playersBets), represents the bets each player makes in this particular pot.
 * If player 1 goes all in with 10 dollars, and player 2 raises to 20, player's 1 and 2 both have 10
 * dollars stored.  However,  player 2 has an additional 10 dollars stored in the new side pot.
 *
 */

public class Pot {

	/* Each player's bets for this pot (null means they are not currently in this pot) */
	private Purse[] playersBets;
	/* The all-in amount for this pot (if no all-in, it is set to the max amount bet so far) */
	private int allIn;
	
	
	public Pot(int size)
	{
		playersBets=new Purse[size];
		allIn=0;
		try{
			for(int i=0; i<playersBets.length; i++)
				playersBets[i]=new Purse(0);
		}
		catch(Exception e){assert false;}
	}
	
	public Pot(Purse[] playersBets)
	{
		this.playersBets=playersBets;
		
		/* update allIn */
		try{
			for(int i=0; i<this.playersBets.length; i++)
				if(playersBets[i]!=null && playersBets[i].getChips()>this.allIn)
					this.allIn=playersBets[i].getChips();
		}
		catch(PurseIsEmptyException e){assert false;}
	}

	/** 
	 * This method is called whenever a player goes all in.  All in is a special case in that side pots
	 * are always created (sometimes the sidepots are the trivial one player case, but a sidepot is 
	 * definitely created).  This method updates the current pot's bets and returns the new side pot.
	 * 
	 * @param index
	 * @param amount
	 * @return sidepot
	 * @throws IllegalArgumentException
	 * @throws InvalidMoneyValueException
	 */
	public Pot allIn(int index, int amount) throws IllegalArgumentException, InvalidMoneyValueException
	{
		/* Exceptions */
		if(index < 0 || index >= playersBets.length)
			throw new IllegalArgumentException();
		if(amount <= 0)
			throw new InvalidMoneyValueException();
		
		/* Sidepot to be returned */
		Purse[] result=new Purse[playersBets.length];
		
		try{
			// adds the all-in to players current bet and also sets allIn variable
			if(playersBets[index]==null)
				playersBets[index]=new Purse(amount);
			else if(playersBets[index].isEmpty())
				playersBets[index]=new Purse(amount);
			else
				playersBets[index]=new Purse(playersBets[index].getChips() + amount);
			
			allIn=playersBets[index].getChips();

			/* Check if bets made before all-in add money to the new sidepot. */
			for(int i=0; i<result.length; i++){
				
				if(playersBets[i] == null)
					result[i]=new Purse(0);
				else if(playersBets[i].getChips() <= allIn){
					result[i]=new Purse(0);
				}			
				else{
					result[i]=new Purse(playersBets[i].getChips()-allIn);
					playersBets[i]=new Purse(allIn);
				}
			}
		}
		catch(PurseIsEmptyException e)
		{
			assert false;
		}
		return new Pot(result);
	}
	
	
	/**
	 * 
	 * This method is used by the GamePot to bubble-in money into a pot when there are
	 * more than one pots.  In the Pot object, it just takes in an amount, and if greater
	 * than the allIn variable, returns the difference.  If smaller, it is an illegal bet
	 * and an exception is thrown.
	 * 
	 * @param index
	 * @param amount
	 * @return
	 * @throws InvalidMoneyValueException
	 * @throws IllegalArgumentException
	 */
	public int bubbleIn(int index, int amount) throws InvalidMoneyValueException, IllegalArgumentException
	{
		int i = 0;
		
		if(index<0 || index>=playersBets.length)
			throw new IllegalArgumentException();
		
		try{
			if(playersBets[index] != null)
			{
				if(amount+playersBets[index].getChips()<allIn)
					throw new InvalidMoneyValueException();
				else
					i = amount + playersBets[index].getChips() - allIn;
			}
			else
			{
				if(amount < allIn)
					throw new InvalidMoneyValueException();
				else
					i = amount - allIn;
			}
		}
		catch(PurseIsEmptyException e)
		{assert false;}
		
		playersBets[index]= new Purse(allIn);
		return i;
	}

	/**
	 * This method adds a bet to a player's current bet.
	 * 
	 * @param index
	 * @param amount
	 * @throws IllegalArgumentException
	 * @throws InvalidMoneyValueException
	 */
	public void addPlayerBet(int index, int amount) throws IllegalArgumentException, InvalidMoneyValueException
	{
		if(index < 0 || index>=playersBets.length)
			throw new IllegalArgumentException();
		if(amount < 0)
			throw new InvalidMoneyValueException();
		
		try{
			
			if(playersBets[index] == null)
				playersBets[index] = new Purse(amount);
			else
				playersBets[index] = new Purse(playersBets[index].getChips()+amount);
			
			if(playersBets[index].getChips() > allIn)
				allIn=playersBets[index].getChips();
		}
		catch(Exception e)
		{
			assert false;
		}
	}
	
	/**
	 * Gets the bet at the index.
	 * 
	 * @param index
	 * @return
	 * @throws IllegalArgumentException
	 */
	public int getPlayerBet(int index) throws IllegalArgumentException
	{
		if(index<0 || index>=playersBets.length)
			throw new IllegalArgumentException();
		int result=0;
		
		try{
			result=playersBets[index].getChips();
		}
		catch(Exception e){assert false;}
		return result;
	}
	
	public int getAllIn()
	{
		return allIn;
	}

	/**
	 * Checks if the player at index matched the highest bid (allIn)
	 * 
	 * @param index
	 * @return
	 * @throws IllegalArgumentException
	 */
	public boolean playerMatchedHighestBid(int index) throws IllegalArgumentException
	{
		if(index<0 || index>playersBets.length)
			throw new IllegalArgumentException();
		boolean result=false;
		try{
			if(playersBets[index] != null)
				result=(playersBets[index].getChips()==allIn);
			else
				throw new IllegalArgumentException();
		}
		catch(PurseIsEmptyException e){assert false;}
		
		return result;
	}
		
	/**
	 * Checks to see if every player has bet equal to the allin/bet.
	 * 
	 * @return
	 */
	public boolean isFull()
	{
		try{
			for(int i=0; i<playersBets.length;i++)
				if(getAllIn()!=playersBets[i].getChips())
					return false;
		}
		catch(NullPointerException e)
		{
			return false;
		}
		catch(PurseIsEmptyException e)
		{
			assert false;
		}
		return true;
	}
	
	/**
	 * This method is used to find out how much money total is in the pot.
	 * It is used by higher classes to distribute the money.
	 * 
	 * @return
	 */
	public int getMoneyInPot()
	{
		int total = 0;
		
		for(int i = 0; i < playersBets.length; i++)
			try
			{
				if(playersBets[i] != null)
					total += playersBets[i].getChips();
				else
					total += 0;
			}
			catch (PurseIsEmptyException e)
			{
				total += 0;
			}
		
		return total;
	}
	
	public String toString()
	{
		String result="All-in: "+allIn+"\n";
		for(int i=0; i<playersBets.length; i++)
			result+= i + " "+playersBets[i]+"\n";
		return result;
	}
}
