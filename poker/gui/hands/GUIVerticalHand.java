package gui.hands;


import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.Iterator;

import util.Card;
import util.Hand;
import util.test.AllCards;

public class GUIVerticalHand extends GUIHand{

	public GUIVerticalHand()
	{
		super(new FlowLayout());
		
		this.setPreferredSize(new Dimension(400, 125));
	}
	
	public final void unrevealedHand()
	{
		for(int i=0; i<cards.length; i++)
		{
			cards[i] = new GUIVerticalCard(AllCards.a2C);
			this.add(cards[i]);
		}
	}
	
	public final void setHand(Hand h)
	{
		hand = h;
		
		counter =0;
		Iterator<Card> i = h.iterator();
		
		int j=0;
		
		for(int k = 0; k < cards.length; k++)
			if(cards[k] != null)
				this.remove(cards[k]); 
		
		while(i.hasNext())
		{
			cards[j] = new GUIVerticalCard(i.next());
			this.add(cards[j]);
			cards[j].addComponentListener(this);
			j++;
		}
	}
	
}
