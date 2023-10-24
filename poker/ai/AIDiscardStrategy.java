package ai;

import ai.util.DiscardProbability;
import util.*;

/**
 * This interface lays out what an AI character can expect from his discard strategy
 * The function bestDiscard returns a DiscardProbability object which contains a list of
 * cards to discard according to the particular implementation. the probability value 
 * inside the DiscardProbability can be checked against badProbability( ) to see if the discard
 * is worth banking on, or whether it is simply the most optimal for that hand. 
 * @author dkawry
 *
 */
public interface AIDiscardStrategy {
	
	DiscardProbability bestDiscard(Hand hand);
	
	float badProbability();
	float goodProbability(); 
}
