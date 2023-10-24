package gui;

import gui.userpanels.GUIProfileIcon;

import images.Constants;

import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.util.ArrayList;

import javax.swing.JLabel;
import javax.swing.JPanel;

import players.Player;

public class GUISidePot extends JPanel {
	
	// Keeps an ArrayList of players for referencing
	private ArrayList<String> mPlayers; 
	private ArrayList<String> playerIcons;
	
	public GUISidePot(int ID, int Amount, ArrayList<String> Players,ArrayList<String> playerIconz)
	{
		super(new GridLayout(0, 1));
		this.setBackground(Constants.TRANSPARENT);
		this.setBorder(Constants.GUI_BORDER);
		this.setPreferredSize(Constants.GUI_POT_SIZE);
		assert (Players.size() < 5);
		
		mPlayers    = new ArrayList<String>(); 
		playerIcons = new ArrayList<String>();
		
		for(int i = 0; i < Players.size(); i++)
			mPlayers.add(Players.get(i)); 
	
		for(int i=0;i<playerIconz.size();i++)
			this.playerIcons.add(playerIconz.get(i));
		
		update(ID, Amount, Players, this.playerIcons);	
	}
	
	public void update(int ID, int Amount, ArrayList<String> Players, ArrayList<String>playerIconz)
	{
		// remove previous content
		this.removeAll();
		
		JLabel l;
		
		l = new JLabel("Pot: " + Integer.toString(ID));
		l.setHorizontalAlignment(JLabel.CENTER);
		this.add(l);
		
		l = new JLabel(" -- " + Integer.toString(Amount) + " -- ");
		l.setHorizontalAlignment(JLabel.CENTER);
		
		// add money value
		this.add(l); 
		
		// add the players
		for(int i = 0; i < Players.size(); i++)
		{
			// check if player was null and is now non-null -> means we must add him again!
			// this can happen for whatever reason
			if(Players.get(i) != null && mPlayers.get(i) == null)
				mPlayers.set(i, Players.get(i)); 
			
			JPanel p = new JPanel(new FlowLayout());
			p.setBackground(Constants.TRANSPARENT);
			GUIProfileIcon icon = new GUIProfileIcon(playerIconz.get(i));
			l = new JLabel(mPlayers.get(i));
			l.setHorizontalAlignment(JLabel.CENTER);
			
			if(Players.get(i) != null)
				l.setFont(Constants.FONT);
			else
				l.setFont(new Font(null, Font.ITALIC, 8)); 
			
			p.add(icon);
			p.add(l);
			this.add(p);
		}
		
		this.updateUI();
	}
}
