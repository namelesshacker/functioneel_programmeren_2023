package gui.hands;


import gui.GUICard;

import java.awt.*;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.ArrayList;

import javax.swing.JPanel;
import util.*;

public abstract class GUIHand extends JPanel implements ComponentListener{

	protected GUICard[] cards;
	protected int counter;
	protected final Color TRANSPARENT = new Color(50,205,50);
	protected Hand hand;
	
	public GUIHand(LayoutManager layoutManager)
	{
		super(layoutManager);
		this.setBackground(TRANSPARENT);
		cards = new GUICard[5];
		
		counter = 0;
	}
	
	public abstract void unrevealedHand();
	
	public abstract void setHand(Hand h);
	
	public final void componentResized(ComponentEvent e)
	{}
	
	public final ArrayList<Card> getSelectedCards()
	{
		ArrayList<Card> toReturn = new ArrayList<Card>(); 
		
		for(int i = 0; i < cards.length; i++)
			if(cards[i].isSelected())
				toReturn.add(cards[i].getCard()); 
		
		return toReturn; 
	}
	
	public final void hideSelectedCards()
	{
		for(int i = 0; i < cards.length; i++)
		{
			if(cards[i].isSelected());
			{
				cards[i].hideCard();
				cards[i].updateUI();
			}
		}
	}
	
	public final void componentMoved(ComponentEvent e)
	{
		GUICard x = (GUICard) e.getComponent();
		
		if(x.isSelected())
		{
			counter++;
			if(counter == 3)
			{
				for(int i=0; i<cards.length; i++)
					if(! cards[i].isSelected())
						cards[i].fixCard();
			}
		}
		else
		{
			counter--;
			if(counter == 2)
			{
				for(int i=0; i<cards.length; i++)
					if(! cards[i].isSelected())
						cards[i].unfixCard();
			}
		}
	}
	
	public final void componentHidden(ComponentEvent e)
	{}
	
	public final void componentShown(ComponentEvent e)
	{}
	
	public final void revealAllCards()
	{
		for(int i=0; i<cards.length; i++)
			cards[i].revealCard();
	}
	
	public final void unrevealAllCards()
	{
		for(int i=0; i<cards.length; i++)
			cards[i].unrevealCard();
	}
	
	public final void unfixAllCards()
	{
		for(int i=0; i<cards.length; i++)
			cards[i].unfixCard();
	}
	
	public final void fixAllCards()
	{
		for(int i=0; i<cards.length; i++)
			cards[i].fixCard();
	}
	
	public final Hand getHand()
	{
		return hand;
	}
	
	public final void foldHand()
	{
		for(int i=0; i<cards.length; i++)
			cards[i].foldCard();
	}
	
	public final void unfoldHand()
	{
		for(int i=0; i<cards.length; i++)
			cards[i].unfoldCard();
	}
	
	public final boolean isFolded()
	{
		return cards[0].isFolded();
	}
}