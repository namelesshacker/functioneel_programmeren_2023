package game.exceptions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * Exception if Purse is Empty (ie, thrown when a player tries to bet with no
 * money in the purse).
 */

public class PurseIsEmptyException extends Exception {

	private final static long serialVersionUID = -1;
	
	public PurseIsEmptyException()
	{
		super();
	}
}
