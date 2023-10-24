package game.test;

import money.Purse;
import junit.framework.TestCase;
import game.exceptions.InsufficientFundsException;
import game.exceptions.InvalidMoneyValueException;
import game.exceptions.PurseIsEmptyException;

public class TestPurse extends TestCase {

	private Purse p;
	
	@Override
	protected void setUp() throws Exception 
	{	
		try{
			p=new Purse(100);
		}	
		catch( IllegalArgumentException e){}
	}
	
	// Test constructor
	public void testPurseConstructor()
	{	
		try{
			p=new Purse(-1);
			fail();
		}
		catch( InvalidMoneyValueException e)
		{
			// Exception Caught
		}
	}
	
	//Test isEmpty()
	public void testIsEmpty()
	{
		try{
			p=new Purse(0);	
			assertTrue(p.isEmpty());
		}
		catch(InvalidMoneyValueException e)
		{
		}
	}
	
	// Test removeAllChips
	public void testRemoveAllChips()
	{
		try{
			assertTrue(p.removeAllChips()==100);
		}
		catch( PurseIsEmptyException e)
		{}
	}
	
	// Test removeChips
	public void testRemoveChips()
	{
		// Test method first
		try{
			assertTrue(p.removeChips(10)==10);
		}
		catch( IllegalArgumentException e)
		{
		}
		catch( PurseIsEmptyException e)
		{
		}
		catch( InsufficientFundsException e)
		{
		}
		
		// Check that InsufficientFundsException is ok
		try{
			p.removeChips(1000000);
			fail();
		}
		catch( InsufficientFundsException e)
		{
			//Test passed
		}
		catch( IllegalArgumentException e)
		{
		}
		catch( PurseIsEmptyException e)
		{
		}
		
		//Check IllegalArgumentException works 
		try{
			p.removeChips(-1);
			fail();
		}
		catch( IllegalArgumentException e)
		{
			//Test Passed
		}
		catch( PurseIsEmptyException e)
		{
		}
		catch( InsufficientFundsException e)
		{
		}
		
		// Check if Purse is empty in this method
		try{
			p=new Purse();
			p.removeChips(10);
			fail();
		}
		catch( IllegalArgumentException e)
		{
		}
		catch( PurseIsEmptyException e)
		{
			// Test Passed
		}
		catch( InsufficientFundsException e)
		{
		}
	}
	
	public void testGetChips()
	{
		try{
			assertTrue(p.getChips()==100);
		}
		catch(PurseIsEmptyException e)
		{
		}
		
		// Check for exception
		try{
			p=new Purse();
			p.getChips();
		}
		catch(PurseIsEmptyException e)
		{
			// ok!
		}
	}
	
	public void testAddChips() throws IllegalArgumentException
	{
		try{
			p.addChips(1);
			assertTrue(p.getChips()==101);
		}
		catch(PurseIsEmptyException e)
		{
		}
		catch(IllegalArgumentException e)
		{
		}
		
		try{
			p.addChips(-1);
		}
		catch(IllegalArgumentException e)
		{
			//test worked
		}
		
	}
}
