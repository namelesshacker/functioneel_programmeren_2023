package game;

import game.actions.Action;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This interface represents the Subject in a Push-Observable system.
 * 
 */
public interface GameSubject {

	/* Attach a listener to this subject */
	public void attachGameListener(GameListener g);
	/* detach a listener to this subject */
	public void detachGameListener(GameListener g);
	
	public void notifyObservers(Action action);
}
