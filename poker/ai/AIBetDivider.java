package ai;

/**
 * 
 * @author david kawrykow and gen kazama
 * 
 * This interface is used to get the next RAISE from a player. The Range of the Raise
 * can be set using the setRange function. 
 */
public interface AIBetDivider {
	
	public int getNextBet();
	
	public void setRange(int range);
	public int getRange();
}
