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
 * Rudy Giuliani is an intelligent player who tends to act aggressively at some times
 * and conservatively at others. He discards aggressively: i.e. tries for 5 card hands
 * when these are within reasonable reach. He sets high comfort zones for himself based on
 * the assumption that he will have higher hands often and
 * a strong recovery when a raise forces him to exceed his comfort zone. He CALLS only 10%
 * of the time when given the chance to act offensively. However, his bets in this case 
 * are typically low. He also has almost no bluff detection capabilities i.e. his 
 * Bluff Finder is virtually useless. 
 */
public class Giuliani extends AIParametricPlayer {
	
	public Giuliani(String NAME, int PURSE, int SEED)
	{
		super("Rudy Giuliani" + NAME, PURSE, SEED);

		AIParametricBrain b1 = new AIParametricBrain("Rudy Giuliani" + NAME, PURSE, SEED);
		
		b1.setBetDivider(new AIParametricBetDivider(5, SEED));
		b1.setBluffer(new AIParametricBluffer(20, 25, SEED));
		b1.setDiscardStrategy(new AIAggressiveDiscardStrategy());
		b1.setBluffFinder(new AIParametricBluffFinder(1, 5, SEED));
		b1.setInstinct(new AIParametricInstinct(80, SEED));
		b1.setDecisionMaker(new AISimpleDecisionMaker(10, SEED));	// THE BIGGEST INFLUENCE!! 
		b1.setPreBetLimit(new AIAggressivePreBetLimit());
		b1.setPostBetLimit(new AIAggressivePostBetLimit());
		
		super.setBrain(b1);
		this.setSmallIcon("GiulianiT.gif");
		this.setBigIcon("Giuliani.gif");
	}
}