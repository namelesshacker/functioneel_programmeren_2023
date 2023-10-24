package game.test;

import junit.framework.TestCase;
import game.GameContainer;
import java.util.Iterator;

public class TestGameContainer extends TestCase {

	private GameContainer x;
	Object p1, p2, p3, p4;
	
	
	public void setUp()
	{		
		x = new GameContainer();
		p1 = new Object();
		p2 = new Object();
		p3 = new Object();
		p4 = new Object();
		
		x.addObject(p1);
		x.addObject(p2);
		x.addObject(p3);
		x.addObject(p4);
	}
	
	public void testAddIncorrectObject()
	{
		try
		{
			x.addObject(p1);
			fail();
		} catch (IllegalArgumentException e)
		{
			/* Good because p1 already in x */
		}
	}
	
	public void testStandardIterations()
	{
		int counter = 0;
		
		Iterator i = x;
		
		while(i.hasNext())			// single standard
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 4);
		
		while(i.hasNext())			// second standard! 
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 8);
		
		while(i.hasNext())			// second standard
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 12);	// third standard
		
		x = new GameContainer();
		x.addObject(p1);
		
		i = x;
		counter = 0;
		
		while(i.hasNext())
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 1);
		
		x = new GameContainer();
		x.addObject(p1);
		x.addObject(p2);
		
		i = x;
		counter = 0;
		
		while(i.hasNext())
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 2);
	}
	
	public void testShiftingSingleton()
	{
		try{
		x = new GameContainer();
		x.addObject(p1);
		x.shiftDealer();	// expect no difference in dealer
		Iterator i = x;
		int counter = 0;
		
		while(i.hasNext())
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 1);
		assertTrue(x.getDealer() == p1);
		} 
		
		catch(Exception e)
		{
			fail("No exception expected");
		}
	}
	
	public void testShiftDealer()
	{
		try{
		x.shiftDealer();
		assertEquals(x.getDealer(), p2);
		x.shiftDealer();
		assertEquals(x.getDealer(), p3);
		x.shiftDealer();
		assertEquals(x.getDealer(), p4);
		x.shiftDealer();
		assertEquals(x.getDealer(), p1);
		} 
		
		catch(Exception e)
		{
			fail("No exception expected");
		}
	}
	
	public void testOneShiftThenIteration()
	{
		try{
		/* Test shifting dealer, then iterating */
		int counter = 0;
		x.shiftDealer();
		Iterator i = x;
		
		while(i.hasNext())
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 4);
		assertTrue(x.getDealer() == p2);
		
		/* Test shifting dealer, then iterating but with smaller population */
		x = new GameContainer();
		x.addObject(p1);
		x.addObject(p2);
		x.shiftDealer();
		i = x;
		counter = 0;
		
		while(i.hasNext())
		{
			i.next();
			counter++;
		}
		
		assertTrue(counter == 2);
		assertTrue(x.getDealer() == p2);
		}
		
		catch(Exception e)
		{
			fail("No exception expected");
		}
	}
	
	public void testRemoveObjectDuringIteration()
	{
		try{
		x.shiftDealer();
		Iterator i = x;
		Object p; 
		int counter = 0;
		
		while(i.hasNext())
		{
			p = i.next();
			if(p == p3)
				i.remove();
		}
		
		assertEquals(x.getDealer(), p2);		// check dealer still ok
		
		while(i.hasNext())
		{
			i.next();
			counter++;
		}
		
		assertEquals(counter, 3);				// check number still ok
		assertEquals(i.next(), p4);				// check order is still ok
		assertEquals(i.next(), p1);
		assertEquals(i.next(), p2);
		assertEquals(i.hasNext(), false);		// check if still knows how to traverse
		assertTrue(i.hasNext());				// 
		assertTrue(i.hasNext());
		assertEquals(i.next(), p4);				// check if hasNext wasn't messed up
		i.next();
		i.remove();
		i.next();
		i.hasNext();
		assertEquals(i.next(), p4);				// check if second removal was ok
		assertEquals(i.next(), p2);
		i.remove();
		assertEquals(x.getDealer(), p4);		// check if dealer updated
		assertEquals(i.next(), p4);
		assertEquals(i.hasNext(), false);		// check if p4 is only one left
		}
		
		catch(Exception e)
		{
			fail("No exception expected");
		}
		
	}
}
