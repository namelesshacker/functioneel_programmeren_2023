package ai;

import javax.swing.ImageIcon;

import players.Player;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This interface is for all AI players.  The only difference between an AI Player and 
 * any other players is that it has an AIParametricBrain.  This brain makes all 
 * decisions for the Player (as a brain normally does).  The only differences between 
 * different AI players are their brains (which contain all the decision making 
 * functions).
 *
 */

public interface AIPlayer extends Player {

	// Set the Brain for the player.
	void setBrain(AIParametricBrain ParametricBrain);
	ImageIcon getSmallIcon();
	ImageIcon getBigIcon();
}
