package game.exceptions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * Exception if the maximum number of players are currently participating in the game
 */

public class HandFoldedException extends Exception {

	private final static long serialVersionUID = -1;
	
	public HandFoldedException()
	{
		super();
	}
}
