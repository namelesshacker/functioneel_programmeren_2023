package game.exceptions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * Exception if there is an attempt to remove a player from the pot
 * who has already been removed.
 */

public class ParticipantAlreadyRemovedException extends Exception {

	private final static long serialVersionUID = -1;
	
	public ParticipantAlreadyRemovedException()
	{
		super();
	}
}