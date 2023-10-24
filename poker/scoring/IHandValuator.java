package scoring;
import util.Hand;

/**
 * Can determine the value of a poker hand.
 * Rules from <A href="http://www.pagat.com/vying/pokerrank.html">http://www.pagat.com/vying/pokerrank.html</A>
 */
public interface IHandValuator 
{
	/**
	 * Determines the value of a poker hand.
	 * @param pHand The hand to valuate.
	 * @return An object representing the value of the hand.
	 * @pre pHand != null
	 * @pre pHand.isFull()
	 * @post return != null
	 */
	public HandValue valuateHand( Hand pHand );
}
