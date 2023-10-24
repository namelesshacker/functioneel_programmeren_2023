package game.exceptions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * This exception is used when the obervable recevies a change before it
 * has notified the observers of the previous change.
 *
 */

public class InvalidUpdateException extends Exception{

	private final static long serialVersionUID = -1;
		
	public InvalidUpdateException()
	{
		super();
	}
	
	
}
