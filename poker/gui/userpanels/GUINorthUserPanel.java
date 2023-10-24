package gui.userpanels;

import gui.hands.GUIHand;
import gui.hands.GUIVerticalHand;

import images.Constants;

import java.awt.FlowLayout;
import java.awt.GridLayout;

import javax.swing.JPanel;

public class GUINorthUserPanel extends GUIUserPanel{
	
	public GUINorthUserPanel(String name, String filename, int amount)
	{
		super(name, filename, amount, new FlowLayout());
		
		addAll();
	}
	
	protected final void addAll()
	{
		this.removeAll();
		
		JPanel profileDealer = new JPanel(new FlowLayout());
		profileDealer.setBackground(Constants.TRANSPARENT);
		profileDealer.add(icon);
		profileDealer.add(profile);
		profileDealer.add(dealer);
		
		JPanel purseProfile = new JPanel(new GridLayout(0,1));
		purseProfile.setBackground(Constants.TRANSPARENT);
		purseProfile.add(profileDealer);
		purseProfile.add(purse);
		
		this.setBackground(Constants.TRANSPARENT);
		
		this.add(hand);
		this.add(purseProfile);
		this.updateUI();
	}

	
	public final void setGUIHand(GUIHand hand)
	{
		if(hand instanceof GUIVerticalHand)
		{
			this.hand = hand;
			addAll();
		}
		else
			assert false;
	}
	
	public final GUIVerticalHand getGUIHand()
	{
		return (GUIVerticalHand) hand;
	}
}
