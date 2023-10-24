package gui.userpanels;

import gui.hands.GUIHand;
import gui.hands.GUIHorizontalHand;

import images.Constants;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;

import javax.swing.JPanel;

public class GUIWestUserPanel extends GUIUserPanel{
	
	public GUIWestUserPanel(String name, String filename, int amount)
	{
		super(name, filename, amount, new BorderLayout());
		addAll();
	}
	
	protected final void addAll()
	{
		this.removeAll();
		
		JPanel p1 = new JPanel(new FlowLayout());
		p1.setBackground(Constants.TRANSPARENT);
		p1.add(hand);
		p1.add(purse);
		
		JPanel profileDealer = new JPanel(new FlowLayout());
		profileDealer.setBackground(Constants.TRANSPARENT);
		profileDealer.add(icon);
		profileDealer.add(profile);
		profileDealer.add(dealer);
		
		this.add(p1, BorderLayout.PAGE_START);
		this.add(profileDealer, BorderLayout.CENTER);
	
		this.updateUI();
	}
	
	public final void setGUIHand(GUIHand hand)
	{
		if(hand instanceof GUIHorizontalHand)
		{
			this.hand = hand;
			addAll();
		}
		else
			assert false;
	}
	
	public final GUIHorizontalHand getGUIHand()
	{
		return (GUIHorizontalHand) hand;
	}
}
