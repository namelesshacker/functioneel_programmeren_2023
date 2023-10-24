package game;

import game.actions.Action;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 * This interface represents the Listener in a push-observable system.
 * 
 */
public interface GameListener {
	
	public void notify(Action action);
}
