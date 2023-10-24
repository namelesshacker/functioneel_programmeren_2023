package game.exceptions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * Exception if No Dealer present (i.e. when trying to operate on dealer when
 * no internal players)
 */

public class NoDealerException extends Exception {

	private final static long serialVersionUID = -1;
	
	public NoDealerException()
	{
		super();
	}
}
