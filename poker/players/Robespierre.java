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
 * Robespierre is a simple Trotsky. He's worse than Prototype players 
 */
public class Robespierre extends AIParametricPlayer {

	public Robespierre(String NAME, int PURSE, int SEED)
	{
		super("Robespierre" + NAME, PURSE, SEED);
		
		AIParametricBrain b = new AIParametricBrain("Robespierre" + NAME, PURSE, SEED);
		
		b.setBetDivider(new AIParametricBetDivider(5, SEED));		
		b.setBluffer(new AIParametricBluffer(100, 100, SEED));		
		b.setDiscardStrategy(new AIBlackAndWhiteDiscardStrategy());		
		b.setBluffFinder(new AIParametricBluffFinder(10, 100, SEED));		
		b.setInstinct(new AIParametricInstinct(100, SEED));		
		b.setDecisionMaker(new AISimpleDecisionMaker(95, SEED));	// THE BIGGEST INFLUENCE!! 	
		b.setPreBetLimit(new AIAggressivePreBetLimit());		
		b.setPostBetLimit(new AIAggressivePostBetLimit());
		
		super.setBrain(b);
		this.setSmallIcon("RobespierreT.gif");
		this.setBigIcon("Robespierre.gif");
	}	
}
