package game;
import java.util.ArrayList;
import java.util.Iterator;

import players.Player;
import game.exceptions.PurseIsEmptyException;
import util.*;

public class PlayerComparator {
	
	private ArrayList<String> mStrings;
	private ArrayList<Integer> mIntegers;
	private ArrayList<Hand> mHands;
	
	public PlayerComparator()
	{
		mStrings = new ArrayList<String>();
		mIntegers = new ArrayList<Integer>();
		mHands = new ArrayList<Hand>();
	}
	
	public void removePlayer(Player player)
	{
		int i = mStrings.indexOf(player.toString());
		
		mStrings.remove(i);
		mHands.remove(i);
		mIntegers.remove(i);
	}
	
	public void addPlayer(Player player) throws IllegalArgumentException
	{
		for(int i = 0; i < mStrings.size(); i++)
		{
			if(player.toString().equals(mStrings.get(i)))
				throw new IllegalArgumentException();
		}
		
		mStrings.add(player.toString());
		Hand x = new Hand();
		
		for(Iterator i = player.getHand().iterator(); i.hasNext(); )
			x.add((Card) i.next());
		
		mHands.add(x);
		
		try
		{
			mIntegers.add(new Integer(player.getPurse().getChips()));
		}
		catch (PurseIsEmptyException e)
		{
			mIntegers.add(new Integer(0));
		}
	}
	
	public void updatePlayer(Player player) throws IllegalArgumentException
	{
		for(int i = 0; i < mStrings.size(); i++)
		{
			if(player.toString().equals(mStrings.get(i)))
			{
				try
				{
					mIntegers.set(i, new Integer(player.getPurse().getChips()));
				}
				catch (PurseIsEmptyException e)
				{
					mIntegers.set(i, new Integer(0));
				}
				
				Hand pHand = new Hand();
				
				for(Iterator j = player.getHand().iterator(); j.hasNext(); )
					pHand.add((Card)j.next());
				
				mHands.set(i, pHand);
				
				return;
			}
		}
				
		throw new IllegalArgumentException();	
	}
	
	public boolean isPlayerValid(Player p1)
	{	
		for(int i = 0; i < mStrings.size(); i++)
		{	
			if( p1.toString().equals(mStrings.get(i)) )
			{
				return isPlayerSame(p1, i);
			}
		}
		
		return false;
	}
	
	private boolean isPlayerSame(Player p1, int i)
	{
		return (compareHands(p1, i) && comparePurses(p1, i));
	}
	
	private boolean compareHands(Player p1, int i)
	{
		Iterator j, k; 
		
		j = p1.getHand().iterator();
		k = mHands.get(i).iterator();
		
		if(p1.getHand().size() != mHands.get(i).size())
			return false;
		
		while(j.hasNext() && k.hasNext())
			if(! ((Card) j.next()).equals((Card) k.next()))
				return false;
		
		return true;
	}
	
	private boolean comparePurses(Player p1, int i)
	{
		int p1PurseAmount, p2PurseAmount;
		
		try
		{
			p1PurseAmount = p1.getPurse().getChips(); 
		} 
		catch (PurseIsEmptyException e)
		{
			p1PurseAmount = 0;
		}
		
		p2PurseAmount = mIntegers.get(i).intValue();
		
		if(p1PurseAmount != p2PurseAmount)
			return false;
		
		return true;
	}
	
	public String toString()
	{
		return mStrings.toString();
	}
}
