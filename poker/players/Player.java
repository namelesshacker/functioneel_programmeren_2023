package players;
import util.*;
import game.GameListener;
import game.actions.BetAction;
import game.actions.ScoreRevealAction;

import java.util.ArrayList;

import javax.swing.ImageIcon;

import money.Purse;

/**
 * @interface Player
 * @author david kawrykow and gen kazama
 * 
 * This interface specifies a GameModel-compatible entity
 */

public interface Player extends GameListener{

	/* Money */
	Purse getPurse();
	void addToPurse(int amount);
	
	/* Action Making */
	BetAction makePreBetAction(int minBet);
	BetAction makePostBetAction(int minBet);
	ScoreRevealAction makeScoreRevealAction();
	
	/* Hand Dealing */
	void setHand(Hand hand);
	Hand getHand();
	ArrayList<Card> discardCards(int max);
	
	/* GUI Icons */
	ImageIcon getSmallIcon();
	ImageIcon getBigIcon();
	
	String getSmallIconName();
	String getBigIconName();
	
	/* Immutable identification */
	String toString();
}
