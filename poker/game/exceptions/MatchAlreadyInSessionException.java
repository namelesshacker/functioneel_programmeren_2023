package game.exceptions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * This exception is thrown when a client of GameModel attempts to 
 * launch a match while another one is already in progress
 */

public class MatchAlreadyInSessionException extends Exception{

	private final static long serialVersionUID = -1;
	
	public MatchAlreadyInSessionException()
	{
		super();
	}
	
}
