package util;

/**
 * An immutable description of a playing card.
 */
public final class Card implements Comparable<Card>
{
	/**
	 * Enum type represening the rank of the card.
	 */
	public enum Rank 
	{ Two, Three, Four, Five, Six,
		Seven, Eight, Nine, Ten, Jack, Queen, King, Ace }
	
	/**
	 * Enum type representing the suit of the card.
	 */
	public enum Suit 
	{ CLUBS, DIAMONDS, HEARTS, SPADES }
	
	private final Rank aRank;
	
	private final Suit aSuit;
	
	/**
	 * Create a new card object. 
	 * @param pRank The rank of the card.
	 * @param pSuit The suit of the card.
	 */
	public Card(Rank pRank, Suit pSuit )
	{
		aRank = pRank;
		aSuit = pSuit;
	}
	
	/**
	 * Obtain the rank of the card.
	 * @return An object representing the rank of the card.
	 */
	public Rank getRank()
	{
		return aRank;
	}
	
	/**
	 * Obtain the suit of the card.
	 * @return An object representing the suit of the card 
	 */
	public Suit getSuit()
	{
		return aSuit;
	}
	
	/**
	 * @see java.lang.Object#toString()
	 * @return See above.
	 */
	public String toString()
	{
		return aRank + "";
	}

	/**
	 * Creates a new card from the current string
	 */
	public static Card toCard(String card, Suit suit)
	{
		return new Card(Rank.valueOf(card), suit);
	}
	
	/**
	 * Compares two cards according to poker rules (ace is high, suits 
	 * don't count).
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 * @param pCard The card to compare to
	 * @return Returns a negative integer, zero, or a positive integer 
	 * as this object is less than, equal to, or greater than pCard
	 */
	public int compareTo(Card pCard)
	{
		return getRank().ordinal() - pCard.getRank().ordinal();
	}
}
