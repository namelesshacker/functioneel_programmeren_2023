package util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;

/**
 * Models a hand of five cards. The hand is always sorted.
 * The hand is not a set: adding the same card twice will result in
 * the same card being twice in the hand.
 */
public class Hand implements Iterable<Card>, Cloneable
{
	private static final int HAND_SIZE = 5;
	
	private ArrayList<Card> aCards;
	
	/**
	 * Creates a new, empty hand.
	 */
	public Hand()
	{
		aCards = new ArrayList<Card>(HAND_SIZE);
	}
	
	/**
	 * Adds pCard to the list.
	 * @param pCard The card to add.
	 * @pre !isFull()
	 * @pre pCard != null
	 */
	public void add( Card pCard )
	{
		assert !isFull();
		assert pCard != null;
		aCards.add( pCard );
		Collections.sort( aCards );
	}
	
	/**
	 * Removed pCard from the hand. Does nothing if
	 * pCard is not in the hand.
	 * @param pCard The card to remove.
	 * @pre pCard != null
	 */
	public void remove( Card pCard )
	{
		assert pCard != null;
		aCards.remove( pCard );
	}
	
	/**
	 * @return True if the hand is full.
	 */
	public boolean isFull()
	{
		return aCards.size() == HAND_SIZE;
	}
	
	/**
	 * Removes all the cards from the hand.
	 */
	public void clear()
	{
		aCards.clear();
	}
	
	/**
	 * @see java.lang.Iterable#iterator()
	 * @return An iterator.
	 */
	public Iterator<Card> iterator() 
	{
		return aCards.iterator();
	}
	
	/**
	 * @return The highest card in the hand.
	 * @pre isFull()
	 */
	public Card getHighCard()
	{
		assert isFull();
		return aCards.get( aCards.size() - 1 );
	}
	
	/**
	 * @return The number of cards in the hand.
	 */
	public int size()
	{
		return aCards.size();
	}
	
	/**
	 * @see java.lang.Object#clone()
	 * @return See above
	 */
	public Hand clone()
	{
		Hand lReturn = null;
		try
		{
			lReturn = (Hand) super.clone();
		}
		catch( CloneNotSupportedException lException )
		{
			// Do nothing.
		}
		return lReturn;
	}
	
	/**
	 * @see java.lang.Object#toString()
	 * @return See above
	 */
	public String toString()
	{
		String lReturn = "{ ";
		
		for( Card lCard : aCards )
		{
			lReturn += lCard.toString() + " ";
		}
		return lReturn + "}";
	}
	
	/**
	 * Makes a hand of the string hand
	 */
	public static Hand toHand(String hand)
	{
		int i, j;
		Hand toReturn = new Hand();
		
		i = hand.indexOf(" ");
		j = hand.indexOf(" ", i+1);
		
		// first card
		toReturn.add(Card.toCard(hand.substring(i+1, j), Card.Suit.CLUBS));
		i = j;
		
		// second card
		j = hand.indexOf(" ", i+1);
		toReturn.add(Card.toCard(hand.substring(i+1, j), Card.Suit.CLUBS));
		i = j;
		
		// third card
		j = hand.indexOf(" ", i+1);
		toReturn.add(Card.toCard(hand.substring(i+1, j), Card.Suit.CLUBS));
		i = j;
		
		// fourth card
		j = hand.indexOf(" ", i+1);
		toReturn.add(Card.toCard(hand.substring(i+1, j), Card.Suit.CLUBS));
		i = j;
		
		// fifth card
		j = hand.indexOf(" ", i+1);
		toReturn.add(Card.toCard(hand.substring(i+1, j), Card.Suit.DIAMONDS));
		
		return toReturn;
	}
	
	public static Hand makeHandFlushed(Hand pHand)
	{
		Card r = pHand.getHighCard();
		pHand.remove(r);
		r = new Card(r.getRank(), Card.Suit.CLUBS);
		pHand.add(r);
		return pHand;
	}
}
