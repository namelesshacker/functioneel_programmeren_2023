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
 * The IvyLeaguePrepster is your typical run of the mill conservative player.
 * When receiving a hand, the prepster prefers to maximize his odds of achieving
 * smaller, more achievable gains. He would never break a pair to run for a straight
 * or flush, and he considers it bad to have no real option other than five card hands
 * (in which case he is very likely to fold). His bluffs and "comfort zones" are both 
 * small and whenever he can make an offensive move, he is 75% likely to CALL, rather than
 * RAISE. His memory however is extraordinary and he detects bluffs with extreme confidence
 * (given that a bluff was exposed in the past 20 rounds). He is also more likely to fold
 * when exposed to high bets. 
 */
public class IvyLeaguePrepster extends AIParametricPlayer {
	
	public IvyLeaguePrepster(String NAME, int PURSE, int SEED)
	{
		super("Ivy League Prepster"+NAME, PURSE, SEED);

		AIParametricBrain b2 = new AIParametricBrain("Ivy League Prepster"+NAME, PURSE, SEED);
		
		b2.setBetDivider(new AIParametricBetDivider(5, SEED));
		b2.setBluffer(new AIParametricBluffer(2, 25, SEED));
		b2.setDiscardStrategy(new AIConservativeDiscardStrategy());
		b2.setBluffFinder(new AIParametricBluffFinder(20, 95, SEED));
		b2.setInstinct(new AIParametricInstinct(10, SEED));
		b2.setDecisionMaker(new AISimpleDecisionMaker(75, SEED));	// THE BIGGEST INFLUENCE!! 
		b2.setPreBetLimit(new AIConservativePreBetLimit());
		b2.setPostBetLimit(new AIConservativePostBetLimit());
		
		super.setBrain(b2);
		this.setSmallIcon("IvyT.gif");
		this.setBigIcon("Ivy.gif");
	}
}
