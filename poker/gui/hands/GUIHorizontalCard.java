package gui.hands;

import gui.GUICard;

import images.Constants;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;

import util.Card;

public class GUIHorizontalCard extends GUICard{

	public GUIHorizontalCard(Card card)
	{
		super(card);
		this.setBackground(Constants.TRANSPARENT);
		this.setPreferredSize(new Dimension(img.getIconHeight(), img.getIconWidth()));
	}
	
	public void paintComponent(Graphics g)
	{
		if(showCard)
		{
			super.paintComponent(g);
		
			Graphics2D g2d = (Graphics2D)g;
	         
			AffineTransform m_affineTransform = new AffineTransform();
	           
			//rotate with the rotation point as the mid of the image
			m_affineTransform.rotate(Math.toRadians(-90), img.getIconHeight()/2, img.getIconWidth()/2);
	         
			//draw the image using the AffineTransform
			g2d.drawImage(img.getImage(), m_affineTransform, this);
			
			if(folded)
			{
				g2d.setColor(Constants.FOLD_COLOR);
				g2d.fillRect(11, 11, img.getIconHeight(), img.getIconWidth());
				
				g2d.setFont(Constants.FONT);
				g2d.setColor(Color.yellow);
				g2d.drawString(FOLD, img.getIconWidth()/2+10, img.getIconHeight()/2);
			}
		}
		else
		{
			g.drawRect(0, 0, img.getIconHeight()/2, img.getIconWidth()/2);
		}
	}
}
