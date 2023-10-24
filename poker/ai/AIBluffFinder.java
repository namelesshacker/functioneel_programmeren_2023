package ai;

import game.actions.Action; 

public interface AIBluffFinder {

	boolean isPlayerBluffing(String p);
	
	void updateGameHistory(Action a); 
	
}
