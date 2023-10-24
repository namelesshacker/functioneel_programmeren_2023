package ai;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This interface is used by the AI to determine if he/she is bluffing and if so, how much more
 * chips to bet (to scare people away using the bluff).
 *
 */
public interface AIBluffer {
	
	// Calculate the bluff
	int getPreBluff();
	int getPostBluff();
	// Determine whether this player is bluffing (so that he remains constant for both betting
	// rounds)
	boolean getBluff();
	
	// Sets/Gets the percentage of times a player bluffs
	void setBluffingPercentage(int bluffingPercentage);
	int getBluffingPercentage();
	
	// Sets/Gets the betting percentage (from betlimit)
	void setBettingPercentage(int bettingPercentage);
	int getBettingPercentage();
}
