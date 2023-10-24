package money;

import game.exceptions.InsufficientFundsException;
import game.exceptions.InvalidMoneyValueException;
import game.exceptions.PurseIsEmptyException;

/** 
 * @author david kawrykow & gen kazama
 */

/** 
 * This class represents a player's liquid earnings during a poker game. 
 * It is implemented in the same manner as any typical bank account class
 * i.e. players can only withdraw if there is actual money in the purse, 
 * players cannot depost negative money etc. 
 */
public class Purse {
	
	private int chipValue;
	
	public Purse()
	{
		chipValue=0;
	}
	
	public Purse(int chips) throws InvalidMoneyValueException
	{
		if(chips < 0)
			throw new InvalidMoneyValueException();
		else
			chipValue=chips;
	}
	
	public boolean isEmpty()
	{
		return (chipValue <= 0);
	}
	
	public int removeAllChips() throws PurseIsEmptyException
	{
		int result;
		
		if(isEmpty())
			throw new PurseIsEmptyException();
		else
		{
			result = chipValue;
			chipValue=0;
		}
		
		return result;
	}
	
	public int removeChips(int chips) throws IllegalArgumentException, PurseIsEmptyException, InsufficientFundsException
	{
		if(isEmpty())
			throw new PurseIsEmptyException();
		else if(chips < 0)
			throw new IllegalArgumentException();
		else if(chips > chipValue)
			throw new InsufficientFundsException();
		else
			chipValue-=chips;
		
		return chips;
	}
	
	public int getChips() throws PurseIsEmptyException
	{
		if(chipValue<0)
			throw new PurseIsEmptyException();
		return chipValue;
	}
	
	public void addChips( int chips ) throws IllegalArgumentException
	{
		if(chips <= 0)
			throw new IllegalArgumentException();
		else
			chipValue+=chips;
	}
	
	public void setChips(int chips)
	{
		chipValue = chips;
	}
	
	public String toString()
	{
		return "Chips: "+chipValue;
	}
}