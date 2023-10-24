package game;

import game.exceptions.NoDealerException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;

import players.Player;

/**
 * @class GameContainer
 * @author david kawrykow and gen kazama
 * 
 * Used to hold any collection of objects in a poker-style seating arrangement. In our
 * context these objects are players so we refer to these objects as such. 
 * Players are retrieved in a clockwise order starting with player after the "dealer". 
 * The dealer can be "shifted". This sets the appropriate player to be dealer, 
 * where appropriate is poker-defined. 
 */

public class GameContainer implements Iterator{
	
	private ArrayList<Object> mObjectContainer;
	private int mCurrentDealer;
	private int mCurrentNext; 
	private int mOldNext;
	private boolean mOldHasNext;
	
	public GameContainer()
	{
		mObjectContainer = new ArrayList<Object>();
		mCurrentDealer = 0;
		mCurrentNext = 0;
		mOldNext = 0;
		mOldHasNext = true;
	}
	
	public void addObject(Object p) throws IllegalArgumentException
	{
		if(mObjectContainer.size() == 1)
		{
			mOldNext = 1;
			mCurrentNext = 1;
		}
		
		mObjectContainer.add(p);
	}
	
	public void swapPlayers(Player out, Player in)
	{
		for(int i = 0; i < mObjectContainer.size(); i++)
		{
			if( ((Player)mObjectContainer.get(i)).toString().equals(in.toString()))
			{
				mObjectContainer.set(i, in);
			}
		}
	}
	
	public int size()
	{
		return mObjectContainer.size();
	}
	
	public void setDealer(Player pDealer) throws NoDealerException
	{
		while( !( (Player)getDealer()).toString().equals(pDealer.toString()))
			shiftDealer();
	}
	
	public void shiftDealer() throws NoDealerException
	{
		if(mObjectContainer.size() == 0)
			throw new NoDealerException();
		
		mCurrentDealer = (mCurrentDealer+1)%mObjectContainer.size();
		mOldNext = (mOldNext+1)%mObjectContainer.size();
		mCurrentNext = (mCurrentNext+1)%mObjectContainer.size();
	}
	
	public Object getDealer() throws NoDealerException
	{
		if(mObjectContainer.size() == 0)
			throw new NoDealerException();
		
		return mObjectContainer.get(mCurrentDealer);
	}
	
	public Object next() throws NoSuchElementException
	{
		if(mObjectContainer.size() == 0)
			throw new NoSuchElementException();
		
		if(mObjectContainer.size() == 1)
		{
			if(mOldHasNext == true)
			{
				mOldHasNext = false;
				return mObjectContainer.get(0);
			}
			else
			{
				throw new NoSuchElementException();
			}
		}
		else if(!hasNext())
		{		
			throw new NoSuchElementException();
		}	
		else
		{
			mOldNext = mCurrentNext;
			mCurrentNext = (mOldNext+1)%mObjectContainer.size();
			return mObjectContainer.get(mOldNext);
		}	
	}
	
	public boolean hasNext() 
	{
		boolean hasNext; 
		
		if(mObjectContainer.size() == 0)
			return false;
		
		if(mObjectContainer.size() == 1)
			return mOldHasNext;
		
		hasNext = (mOldNext != mCurrentDealer);
		
		if(!hasNext)
		{
			mCurrentNext = (mCurrentDealer+1)%mObjectContainer.size();
			mOldNext = mCurrentNext;
		}
		
		return hasNext;
	}
	
	public void remove()
	{
		/* Removing a player before the dealer */
		if(mOldNext < mCurrentDealer)
		{
			mObjectContainer.remove(mOldNext);
			
			if(mOldNext == 0)
				mOldNext = mObjectContainer.size() - 1;
			else
				mOldNext--;
			
			if(mCurrentNext == 0)
				mCurrentNext = mObjectContainer.size() - 1;
			else
				mCurrentNext--;
			
			if(mCurrentDealer == 0)
				mCurrentDealer = mObjectContainer.size() - 1;
			else
				mCurrentDealer--;
		}
		else if(mOldNext == mCurrentDealer)
		{
			mObjectContainer.remove(mOldNext);
		}
		else
		{
			
			mObjectContainer.remove(mOldNext);
			
			/* Very special case: don't shift mOldNext back to dealer */
			if(mOldNext == mCurrentDealer+1)
			{
				if(mCurrentNext > mOldNext)
					mCurrentNext--;
			}
			/* mCurrentNext won't be shifted because of removal */
			else if(mCurrentNext < mOldNext)
			{
				mOldNext--;
			}
			/* Both are shifted by removal */
			else
			{
				mOldNext--;
				mCurrentNext--;
			}
		}	
	}
	
	public String toString()
	{
		return mObjectContainer.toString();
	}
}
