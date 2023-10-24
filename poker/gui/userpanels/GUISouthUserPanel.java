package gui.userpanels;

import gui.hands.GUIHand;
import gui.hands.GUIVerticalHand;

import images.Constants;

import java.awt.FlowLayout;
import java.awt.GridLayout;

import javax.swing.JPanel;

public class GUISouthUserPanel extends GUIUserPanel {
	private GUIButtonPanel button = new GUIButtonPanel();
	
	public GUISouthUserPanel(String name, String filename, int amount)
	{
		super(name, filename, amount, new GridLayout(0,1));
		
		button = new GUIButtonPanel();
		addAll();
	}
	
	protected final void addAll()
	{
		this.removeAll();
		
		JPanel handPP = new JPanel(new FlowLayout());
		handPP.setBackground(Constants.TRANSPARENT);
		handPP.add(hand);
		
		JPanel profileDealer = new JPanel(new FlowLayout());
		profileDealer.setBackground(Constants.TRANSPARENT);
		profileDealer.add(icon);
		profileDealer.add(profile);
		profileDealer.add(dealer);
		
		JPanel purseProfile = new JPanel(new GridLayout(0,1));
		purseProfile.setBackground(Constants.TRANSPARENT);
		purseProfile.add(profileDealer);
		purseProfile.add(purse);
		
		handPP.add(purseProfile);
		
		this.add(handPP);
		this.add(button);
		this.updateUI();
	}
	
	public final void setGUIButtonPanel(GUIButtonPanel button)
	{
		this.button = button;
		addAll();
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
	
	public GUIVerticalHand getGUIHand()
	{
		return (GUIVerticalHand) hand;
	}
}
