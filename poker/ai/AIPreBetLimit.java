package ai;

import util.Hand;
import ai.util.DiscardProbability;
import scoring.HandValue;
/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * Tells an ai the maximum percentage of their purse to bet in the pre bet round
 *
 */

public interface AIPreBetLimit {
	//Get percentage of purse to bet
	int getPercentage();
	
	//Set the hand
	void setHand(Hand hand);
	
	//set the DiscardProbability
	void setDiscardProbability(DiscardProbability probability);
	
	void setHandValue(HandValue value);
}
