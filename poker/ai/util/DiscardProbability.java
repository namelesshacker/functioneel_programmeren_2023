package ai.util;

import java.util.ArrayList;
import util.*;

/**
 *
 * @author david kawrykow and gen kazama
 *
 * This class represents which cards to discard during a discard and whether or 
 * not the probability is "good" of getting results, "bad", or just ok. 
 *
 */

public class DiscardProbability {

	private Card[] cards;
	private float probability;
	private int counter;
	
	public DiscardProbability()
	{
		cards=new Card[3];
		probability=(float)0.0;
		counter=0;
	}
	
	public DiscardProbability(Card[] cards, float probability)
	{
		this.cards=cards;
		this.probability=probability;
		counter=0;
	}
	
	public final void addCard(Card card)
	{
		if(counter==cards.length)
			assert false;
		cards[counter]=card;
		counter++;
	}
	
	public final ArrayList<Card> getArrayListCards()
	{
		ArrayList<Card> result=new ArrayList<Card>(3);
		for(int i=0; i < cards.length; i++)
		{
			if(cards[i]==null)
				break;
			result.add(cards[i]);
		}
		return result;
	}
	
	public final boolean isEmptyDiscard()
	{
		return (cards[0]==null && probability == 0.0);
	}
	
	public final void setProbability(float probability)
	{	
		this.probability=probability;
	}
	
	public final void setCards(Card[] cards)
	{
		this.cards=cards;
	}
	
	public final Card[] getCards()
	{
		return cards;
	}
	
	public final float getProbability()
	{
		return probability;
	}
	
	public String toString()
	{
		String result=probability+"\t{ ";
		for(int i=0; i<counter; i++){
			result+=cards[i];
			if(i!=counter-1)	
				result+=", ";
		}
		return result+" }";
	}
}
