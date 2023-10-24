package players;

import images.Constants;
import ai.*;
import ai.bets.AIParametricBetDivider;
import ai.bets.AIParametricInstinct;
import ai.bets.betlimit.*;
import ai.bets.bluff.*;
import ai.discards.*;
import ai.util.*;

/**
 * Trotsky is an aggressive player. 
 */
public class Trotsky extends AIParametricPlayer {

	public Trotsky(String NAME, int PURSE, int SEED)
	{
		super("Trotsky" + NAME, PURSE, SEED);
		
		AIParametricBrain b = new AIParametricBrain("Trotsky" + NAME, PURSE, SEED);
		
		b.setBetDivider(new AIParametricBetDivider(5, SEED));		
		b.setBluffer(new AIParametricBluffer(100, 100, SEED));		
		b.setDiscardStrategy(new AIHighHandDiscardStrategy());		
		b.setBluffFinder(new AIParametricBluffFinder(10, 100, SEED));		
		b.setInstinct(new AIParametricInstinct(100, SEED));		
		b.setDecisionMaker(new AISimpleDecisionMaker(65, SEED));
		b.setPreBetLimit(new AIAggressivePreBetLimit());		
		b.setPostBetLimit(new AIAggressivePostBetLimit());
		
		super.setBrain(b);
		
		this.setSmallIcon("TrotskyT.gif");
		this.setBigIcon("Trotsky.gif");
	}	
}
