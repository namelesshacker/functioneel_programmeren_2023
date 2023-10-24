package game.exceptions;

/**
 * @class SteroidPlayerException
 * @author david kawrykow and gen kazama
 * Thrown whenever a player changes self during calls from game model
 */

public class SteroidPlayerException extends Exception {

	static final long serialVersionUID = -1;
	
	public SteroidPlayerException()
	{
		super();
	}
	
}
