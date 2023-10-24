package gui.hands;

import gui.GUICard;

import images.Constants;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import util.Card;

public class GUIVerticalCard extends GUICard implements MouseListener{

	private ComponentListener myListener;
	
	public GUIVerticalCard(Card card)
	{
		super(card);
		
		this.addMouseListener(this);
		this.setPreferredSize(new Dimension(img.getIconWidth(),img.getIconHeight()*6/5 + 10));
	}

	public void paintComponent(Graphics g)
	{
		if(showCard == true)
		{
			super.paintComponent(g);
			
			int x = 0;
			int y = 0 + img.getIconHeight()/5;
			
			if(selected && ! fixed && revealed)
			{
				y -= img.getIconHeight()/5;
			}
			
			img.paintIcon(this, g, x, y);
		}
		
		if(folded)
		{
			g.setColor(Constants.FOLD_COLOR);
			g.fillRect(0, img.getIconHeight()/5, img.getIconWidth(), img.getIconHeight());
			
			g.setFont(Constants.FONT);
			g.setColor(Color.yellow);
			g.drawString(FOLD, img.getIconWidth()/2-10, img.getIconHeight()/2 + img.getIconHeight()/5);
		}
	}

	public final void mouseClicked(MouseEvent e)
	{
		if(! fixed)
		{
			selected = ! selected;
			this.repaint();
			myListener.componentMoved(new ComponentEvent(this, 0));
		}
	}
	
	public final void mouseEntered(MouseEvent e)
	{
	}
	
	public final void mousePressed(MouseEvent e)
	{
	}
	
	public final void mouseExited(MouseEvent e)
	{
	}
	
	public final void mouseReleased(MouseEvent e)
	{
	}
	
	public final void addComponentListener(ComponentListener l)
	{
		this.myListener = l;
	}
}
