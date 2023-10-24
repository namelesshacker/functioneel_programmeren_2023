package ai;

import scoring.HandValue;

/**
 * 
 * @author david kawrykow and gen kazama
 *
 * Implementations of this interface are used to decide whether to keep playing or to FOLD
 * in the pre and post-bet rounds based on the current proportion of a player's pocket money 
 * in the pot (current max) and on the current HandValue (value). This is called whenever a 
 * player's pocket money has been exceeded in the pot and he needs to decide whether or not 
 * his current hand and the current money spent enable him to continue playing.  
 *
 */
public interface AIInstinct {
	
	// Set the current Max
	void setCurrentMax(int currentMax);
	// Set the hand value 
	void setHandValue(HandValue value);
	// Determine whether to FOLD (false) or to PLAY (true)
	public boolean getPreBetFoldPlayDecision();
	public boolean getPostBetFoldPlayDecision();
	
}
