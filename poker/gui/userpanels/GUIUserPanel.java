package gui.userpanels;

import gui.hands.GUIHand;
import gui.hands.GUIHorizontalHand;

import images.Constants;

import java.awt.*;

import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * @author gen kazama and david kawrykow
 * 
 * Aggregates all visual components of a player e.g. hand, purse, name and profile picture. 
 * This abstract class provided the common core for the various tweaked children (e.g. horizontal,
 * vertical, with/without display)
 */

public abstract class GUIUserPanel extends JPanel{
	
	protected GUIHand hand;
	protected GUIPurse purse;
	protected GUIProfilePanel profile;
	protected GUIProfileIcon icon;
	private String NAME;
	protected JPanel dealer;
	private boolean folded;
	
	
	public GUIUserPanel(String name, String filename, int amount, LayoutManager l)
	{
		super(l);
		this.setBackground(Constants.TRANSPARENT);
		hand = new GUIHorizontalHand();
		purse = new GUIHorizontalPurse(amount);
		profile = new GUIProfilePanel(name);
	
		dealer = new JPanel();
		JLabel d = new JLabel("D");
		dealer.add(d);
		dealer.setFont(Constants.FONT);
		dealer.setBackground(Constants.TRANSPARENT);
		dealer.setBorder(Constants.BORDER);
		dealer.setVisible(false);
		
		icon = new GUIProfileIcon(filename);
		
		NAME=name;
		folded = false;
	}

	protected abstract void addAll();
	
	public final void setGUIPurse(GUIPurse purse)
	{
		this.purse = purse;
		addAll();
	}
	
	public final void setPurse(int amount)
	{
		if(this.purse != null)
			this.purse.setPurse(amount);
	}
	
	public final void addToPurse(int amount)
	{
		if(this.purse != null)
			this.purse.addToPurse(amount);
	}
	
	public final int getPurse()
	{
		return purse.getChips();
	}
	
	public final void setGUIProfilePanel(GUIProfilePanel profile)
	{
		this.profile = profile;
		addAll();
	}
	
	public abstract GUIHand getGUIHand();
	
	public abstract void setGUIHand(GUIHand hand);
	
	public final String getName()
	{
		return NAME;
	}
	
	public final void setName(String s)
	{
		NAME=s;
		this.updateUI();
	}
	
	public final void setDealerFalse()
	{
		dealer.setVisible(false);
		dealer.updateUI();
	}
	
	public final void setDealerTrue()
	{
		dealer.setVisible(true);
		dealer.updateUI();
	}
	
	public final boolean isFolded()
	{
		return folded;
	}
	
	public final void foldHand()
	{
		if(! folded)
		{
			hand.foldHand();
			folded = true;
			hand.updateUI();
		}
	}
	
	public final void unfoldHand()
	{
		if(folded)
		{
			hand.unfoldHand();
			folded = false;
			hand.updateUI();
		}
	}
	
	public GUIProfileIcon getProfileIcon()
	{
		return icon;
	}
	
	public void setProfileIcon(GUIProfileIcon icon)
	{
		this.icon = icon;
		addAll();
	}
}
