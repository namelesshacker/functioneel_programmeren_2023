package gui;

import gui.userpanels.GUIProfileIcon;

import images.Constants;

import java.awt.GridLayout;
import java.util.ArrayList;
import javax.swing.JPanel;

import players.Player;
import money.GamePot;
import money.GamePotEvent;
import money.GamePotListener;
import money.SeatingArrangementEvent;

public class GUIPot extends JPanel implements GamePotListener {
	
	private ArrayList<String> mPlayers;
	private GamePot           mPot; 
	private ArrayList<GUISidePot> mSidePots;
	
	private ArrayList<String> playerNames;
	private ArrayList<String> playerIcons;
	
	public GUIPot(GamePot mPot, ArrayList<Player> players)
	{ 
		super(); 
		GridLayout x = new GridLayout(2, 2);
		x.setHgap(Constants.H_GAP_SIZE);
		x.setVgap(Constants.V_GAP_SIZE);
		this.setLayout(x);
		this.setBackground(Constants.TRANSPARENT);
		
		assert (mPlayers != null);
		assert (mPot     != null); 
		
		this.mPlayers  = new ArrayList<String>();
		this.mPot 	   = mPot; 
		mPot.addGamePotListener(this); 
		this.mSidePots = new ArrayList<GUISidePot>();
		
		playerNames = new ArrayList<String>();
		playerIcons = new ArrayList<String>();
		
		for (int i = 0; i < mPlayers.size(); i++)
			this.mPlayers.add(mPlayers.get(i));
		for(int i = 0; i<players.size(); i++)
		{
			if(players.get(i) != null)
			{
				this.playerNames.add(new String(players.get(i).toString()));
				this.playerIcons.add(new String(players.get(i).getSmallIconName()));
			}
		}
		
		this.updateUI(); 
	}
	
	/**
	 * Just redraws its pot every time
	 */
	public void notify(GamePotEvent e)
	{ 
		// get the latest seating arrangement - a new round has begun
		if(e instanceof SeatingArrangementEvent)
		{
			mPlayers = new ArrayList<String>();
			ArrayList<String> Players = ( (SeatingArrangementEvent) e).getParticipants(); 
			
			for(int i = 0; i < Players.size(); i++)
				mPlayers.add(Players.get(i)); 
			
			// remove all side pots visually
			this.removeAll();
			
			// remove the data itself
			mSidePots.clear(); 
			
			// ipdute
			this.updateUI();
			
			return;
		}
		else
		{
			redraw();
		}
	}
	
	/**
	 * The function for redrawing the pot
	 */
	private void redraw()
	{
		int       moneyInPot; 
		boolean[] playersInPot; 
		
		ArrayList<String> copyPlayers = new ArrayList<String>();
		
		// copy the player names
		for(int i = 0; i < mPlayers.size(); i++)
			copyPlayers.add(mPlayers.get(i)); 
		
		// For each pot
		for( int i = 1; i <= mPot.getNumberOfSmallerPots(); i++ )
		{
			// get the money and the players in the pot
			moneyInPot   = mPot.getMoneyInPot(i); 
			playersInPot = mPot.playersInPot(i);
			
			// kill the strings in the array list
			for( int j = 0; j < playersInPot.length; j++)
				if(playersInPot[j] == false)
					copyPlayers.set(j, null); 
			
			// if we already have an object for this pot
			// check if our current information matches -> if yes, no point
			// remaking it! 
			if( i <= mSidePots.size() )
			{
				mSidePots.get(i-1).update(i, moneyInPot, copyPlayers, getIconList(copyPlayers));
			}
			
			// otherwise create a new side pot with this money
			else	
			{ 
				mSidePots.add(new GUISidePot(mSidePots.size()+1, moneyInPot, copyPlayers, getIconList(copyPlayers)));
				this.add(mSidePots.get(mSidePots.size() - 1));
			}
		} 
		
		this.updateUI();
	}
	
	private ArrayList<String> getIconList(ArrayList<String> names)
	{
		ArrayList<String> result = new ArrayList<String>();
		for(int i=0; i<names.size(); i++)
		{
			result.add(getFilename(names.get(i)));
		}
		return result;
	}
	
	private String getFilename(String name)
	{
		for(int i=0; i<playerNames.size(); i++)
			if(playerNames.get(i).equals(name))
				return playerIcons.get(i);
		return "";
	}
	
	public boolean isPlayerInPot(String Player, int PotNumber)
	{
		return mPot.playersInPot(PotNumber)[mPlayers.indexOf(Player)];
	}
	
	public int getNumberOfSmallerPots()
	{
		return mPot.getNumberOfSmallerPots();
	}
}
