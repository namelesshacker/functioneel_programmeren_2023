package game.exceptions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * Exception if the maximum number of players are currently participating in the game
 */

public class GameIsFullException extends Exception {

	private final static long serialVersionUID = -1;
	
	public GameIsFullException()
	{
		super();
	}
}
