package gui.hands;


import java.awt.Dimension;
import java.awt.GridLayout;
import java.util.Iterator;

import util.Card;
import util.Hand;
import util.test.AllCards;

public class GUIHorizontalHand extends GUIHand{
	
	public GUIHorizontalHand()
	{
		super(new GridLayout(0,1));
		this.setPreferredSize(new Dimension(110, 420));
	}
	
	public final void unrevealedHand()
	{
		for(int i=0; i<cards.length; i++)
		{
			cards[i] = new GUIHorizontalCard(AllCards.a2C);
			this.add(cards[i]);
		}
	}
	
	public final void setHand(Hand h)
	{
		hand = h;
		
		Iterator<Card> i = h.iterator();
		
		int j=0;
		while(i.hasNext())
		{
			cards[j] = new GUIHorizontalCard(i.next());
			cards[j].unrevealCard();
			this.add(cards[j]);
			j++;
		}
		
		this.updateUI();
	}
}
