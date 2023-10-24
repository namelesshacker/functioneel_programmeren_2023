package gui.hands;


import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.Iterator;
import util.Card;
import util.Hand;
import util.test.AllCards;

public class GUIVerticalDisplayHand extends GUIHand {

	public GUIVerticalDisplayHand()
	{
		super(new FlowLayout());
		
		this.setPreferredSize(new Dimension(400, 100));
	}
	
	/**
	 * Makes this hand an unrevealed hand (i.e. cards are all backwards)
	 */
	public final void unrevealedHand()
	{
		for(int i=0; i<cards.length; i++)
		{
			cards[i] = new GUIVerticalDisplayCard(AllCards.a2C);
			cards[i].unrevealCard();
			this.add(cards[i]);
		}
	}
	
	public final void clearHand()
	{
		// remove previous cards
		this.removeAll();
	}
	
	public final void setHand(Hand h)
	{
		clearHand();
		
		hand = h;
		
		counter =0;
		Iterator<Card> i = h.iterator();
		
		int j=0;
		
		while(i.hasNext())
		{
			cards[j] = new GUIVerticalDisplayCard(i.next());
			cards[j].unrevealCard();
			this.add(cards[j]);
			j++;
		}
		
		this.updateUI();
	}
	
}
