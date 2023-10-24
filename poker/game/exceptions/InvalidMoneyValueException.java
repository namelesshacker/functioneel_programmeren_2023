package game.exceptions;

/**
 * 
 * @author Gen Kazama and DAvid Kawrykow
 * This exception is used when a method expects a money/chip value
 * but recevies an invalid amount (usually a negative amount).
 *
 */

public class InvalidMoneyValueException extends Exception{

	private final static long serialVersionUID = -1;
		
	public InvalidMoneyValueException()
	{
		super();
	}
}