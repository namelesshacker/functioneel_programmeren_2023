package gui;

import images.Constants;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.util.ArrayList;

import game.actions.*;
import game.exceptions.HandFoldedException;
import gui.hands.GUIVerticalDisplayHand;
import gui.userpanels.GUIProfileIcon;

import javax.swing.JLabel;
import javax.swing.JPanel;

import players.Player;
import scoring.HandValue;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class is the panel in the middle of the screen after players have revealed their hands.
 * When revealing, it shows the winning hand for each side pot, and after all hands are revealed,
 * it displays the winner and the amount they won from each pot.  It was created as a nice visual 
 * aid in the end of the game, because viewing player's hands sideways strains the neck.
 *
 */

public class GUIWinningHand extends JPanel{
	
	private final int NUM_POTS_BEGIN = 3;
	private int NUMPOTS = 3;
	
	private ArrayList<ScoreRevealAction> scoreReveals;
	private ArrayList<JPanel> pots;
	private ArrayList<JLabel> infos;
	private ArrayList<GUIVerticalDisplayHand> hands;
	private ArrayList<HandValue> hValues;
	private ArrayList<JLabel> players;
	private ArrayList<GUIProfileIcon> icons;
	private ArrayList<Player> gamePlayers;
	private GUIPot g;
	private final String HUMAN;
	
	public GUIWinningHand(GUIPot g, String human, ArrayList<Player> gamePlayers)
	{
		super();
		
		GridLayout gl = new GridLayout(0,1);
		gl.setVgap(1);
		
		this.setLayout(gl);
		this.setBackground(Constants.TRANSPARENT);
		
		
		HUMAN = human;
		
		this.g=g;
		
		this.gamePlayers = gamePlayers;
		
		scoreReveals = new ArrayList<ScoreRevealAction>();
		pots = new ArrayList<JPanel>();
		infos = new ArrayList<JLabel>();
		hands = new ArrayList<GUIVerticalDisplayHand>();
		hValues = new ArrayList<HandValue>();
		players = new ArrayList<JLabel>();
		icons = new ArrayList<GUIProfileIcon>();
		
		for(int i=0; i<NUM_POTS_BEGIN; i++)
		{
			pots.add(new JPanel(new BorderLayout()));
			pots.get(i).setBackground(Constants.TRANSPARENT);
			pots.get(i).setVisible(false);
			pots.get(i).setBorder(Constants.BORDER);
			
			hands.add(new GUIVerticalDisplayHand());
			hands.get(i).setVisible(false);
			
			JPanel temp = new JPanel(new GridLayout(0,1));
			temp.setBackground(Constants.TRANSPARENT);
			
			infos.add(new JLabel("Pot " + (i+1)+"  "));
			infos.get(i).setFont(Constants.FONT);
	
			icons.add(new GUIProfileIcon("Hasselhoff.gif"));
			icons.get(i).setAlignmentX(JPanel.CENTER_ALIGNMENT);
			temp.add(infos.get(i));
			temp.add(icons.get(i));
	
			players.add(new JLabel(""));
			players.get(i).setPreferredSize(Constants.POT_LABEL_SIZE);
			players.get(i).setFont(Constants.FONT);
			
			pots.get(i).add(temp, BorderLayout.WEST);
			pots.get(i).add(hands.get(i), BorderLayout.CENTER);
			pots.get(i).add(players.get(i), BorderLayout.SOUTH);
			
			hValues.add(null);
			
			this.add(pots.get(i));
		}
	}
	
	public void notify(Action action)
	{
		if(action instanceof NewMatchAction)
		{
			hValues      = new ArrayList<HandValue>();
			scoreReveals = new ArrayList<ScoreRevealAction>();
			
			for(int i =0; i < NUM_POTS_BEGIN; i++)
			{	
				// set all hands invisible
				hands.get(i).setVisible(false);
				
				// clear all hands
				hands.get(i).clearHand();
				
				// set the info text to generic
				infos.get(i).setText("Pot "+i+"  ");
				
				// no player has won any pot yet
				players.get(i).setText("");
				
				// no hand values have been recorded yet
				hValues.add(null);
				
				pots.get(i).setVisible(false);
			}
		}
		else if(action instanceof PlayerRemovedAction)
		{
			NUMPOTS--;
		}
		else if(action instanceof ScoreRevealAction)
		{
			ScoreRevealAction lAction = (ScoreRevealAction)action;
			scoreReveals.add(lAction);
			
			// if the action was a SHOW, update the best hand values in each pot - if applicable
			if(scoreReveals.get(scoreReveals.size()-1).getAction()==ScoreRevealAction.scoreRevealAction.SHOW)
			{
				try
				{
					int numSmallerPots = g.getNumberOfSmallerPots();
					
					if(numSmallerPots > NUMPOTS)
						numSmallerPots = NUMPOTS;
					
					// go through each of the smaller pots
					for(int i=0; i<numSmallerPots;i++)
					{
						// check whether the action maker is in this pot
						if(g.isPlayerInPot(lAction.getActionMaker(), i+1))
						{
							// set the top hand in this pot to either the action maker's
							// or keep it at the value currently in it
							if(hValues.get(i)==null)
							{
								// set the hand value
								hValues.set(i, lAction.getHandValue());
								
								// set the pot to visible (first time)
								pots.get(i).setVisible(true);
								
								// set the hand and reveal it
								hands.get(i).setHand(lAction.getHand());
								hands.get(i).revealAllCards();
								
								// set the curernt leader
								players.get(i).setText(lAction.getActionMaker() + ": " + lAction.getHandValue());
								
								// mark the pot for the human 
								if(g.isPlayerInPot(HUMAN, i+1))
									infos.get(i).setText("Pot "+(i+1)+" +");
								
								// set the hand to visible
								hands.get(i).setVisible(true);
								icons.get(i).setIcon(getFilename(lAction.getActionMaker()));
							}
							else if(hValues.get(i).compareTo(lAction.getHandValue()) == 0)
							{
								// same thing as above but there's a tie, mark both as winning
								hValues.set(i, lAction.getHandValue());
								pots.get(i).setVisible(true);
								hands.get(i).setHand(lAction.getHand());
								hands.get(i).revealAllCards();
								players.get(i).setText(players.get(i).getText()+", "+lAction.getActionMaker() + ": " + lAction.getHandValue());
								icons.get(i).setIcon(getFilename(lAction.getActionMaker()));
							}
							else if(hValues.get(i).compareTo(lAction.getHandValue()) < 0)
							{
								// same thing, but the newest hand is winning. 
								hValues.set(i, lAction.getHandValue());
								pots.get(i).setVisible(true);
								hands.get(i).setHand(lAction.getHand());
								hands.get(i).revealAllCards();
								players.get(i).setText(lAction.getActionMaker() + ": " + lAction.getHandValue());
								icons.get(i).setIcon(getFilename(lAction.getActionMaker()));
							}
						}
					}
				}
				catch(HandFoldedException e)
				{
					e.printStackTrace();
				}
			}
			
			this.updateUI();
		}
		else if(action instanceof GetMoneyAction)
		{
			try
			{
				GetMoneyAction lAction = (GetMoneyAction) action;
				
				int potNumber = lAction.getPot();
				
				if(potNumber > NUM_POTS_BEGIN)
					return;
				
				if(! g.isPlayerInPot(HUMAN, potNumber))
					infos.get(potNumber-1).setText("Pot "+(potNumber)+"  ");
				
				for(int i=0; i<scoreReveals.size(); i++)
				{
					if(lAction.getActionMaker().equals(scoreReveals.get(i).getActionMaker()))
					{
						infos.get(potNumber-1).getText().replace('+', ' ');
						
						if(scoreReveals.get(i).getAction()==ScoreRevealAction.scoreRevealAction.SHOW)
						{
							players.get(potNumber - 1).setText(lAction.getActionMaker()+" won "+lAction.getAmount()+" with " + scoreReveals.get(i).getHandValue());
							icons.get(potNumber-1).setIcon(getFilename(lAction.getActionMaker()));
						}
						else
						{
							// set the pot visible
							pots.get(potNumber-1).setVisible(true);
							
							// make an unrevealed hand and set it to visible
							hands.get(potNumber-1).unrevealedHand();
							hands.get(potNumber-1).setVisible(true);
							
							// set the exact string in the pot panel
							if(scoreReveals.get(i).getAction()==ScoreRevealAction.scoreRevealAction.NO_SHOW)
							{
								players.get(potNumber-1).setText(lAction.getActionMaker()+" won "+lAction.getAmount()+" with a No-Show.");
								icons.get(potNumber-1).setIcon(getFilename(lAction.getActionMaker()));
							}
							else
							{
								players.get(potNumber-1).setText(lAction.getActionMaker()+" won "+lAction.getAmount()+" with a Fold.");
								icons.get(potNumber-1).setIcon(getFilename(lAction.getActionMaker()));
							}
						}
						
						this.updateUI();
						break;
					}		
				}
			} 
			catch(HandFoldedException e)
			{
				e.printStackTrace();
			}
		}
	}
	
	private String getFilename(String s)
	{
		for(int i=0;i<gamePlayers.size();i++)
			if(gamePlayers.get(i)!=null)
				if(gamePlayers.get(i).toString().equals(s))
					return gamePlayers.get(i).getSmallIconName();
		return "";
	}
}
