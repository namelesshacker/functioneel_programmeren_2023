package gui.hands;


import gui.GUICard;

import java.awt.Dimension;
import java.awt.Graphics;
import util.Card;

public class GUIVerticalDisplayCard extends GUICard {
	
	public GUIVerticalDisplayCard(Card card)
	{
		super(card);
		
		this.setPreferredSize(new Dimension(img.getIconWidth(),img.getIconHeight()));
	}

	public void paintComponent(Graphics g)
	{
		super.paintComponent(g);
		
		img.paintIcon(this, g, 0, 0);
	}
}
