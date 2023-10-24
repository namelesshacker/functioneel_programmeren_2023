package ai;

import scoring.HandValue;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * Tells the ai what the maximum percentage of their purse to bet in the post bet round.
 *
 */

public interface AIPostBetLimit {

	//Get percentage of purse to bet
	int getPercentage();
	
	//Set the hand
	void setHandValue(HandValue value);
	
	//Get the HandValue
	HandValue getHandValue();
}
