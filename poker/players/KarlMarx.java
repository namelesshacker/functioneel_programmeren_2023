package players;

import images.Constants;
import ai.*;
import ai.bets.AIParametricBetDivider;
import ai.bets.AIParametricInstinct;
import ai.bets.betlimit.AIAggressivePostBetLimit;
import ai.bets.betlimit.AIAggressivePreBetLimit;
import ai.bets.bluff.AIParametricBluffFinder;
import ai.bets.bluff.AIParametricBluffer;
import ai.discards.AIConservativeDiscardStrategy;
import ai.util.AISimpleDecisionMaker;


/**
 * 
 * Quasi Aggressive (like a rebel)
 *
 */
public class KarlMarx extends AIParametricPlayer{

	public KarlMarx(String NAME, int PURSE, int SEED)
	{
		super("Karl Marx"+NAME, PURSE, SEED);

		AIParametricBrain b= new AIParametricBrain("Karl Marx"+NAME, PURSE, SEED);	
		b.setBetDivider(new AIParametricBetDivider(10, SEED));
		b.setBluffer(new AIParametricBluffer(10, 20, SEED));
		b.setDiscardStrategy(new AIConservativeDiscardStrategy());
		b.setBluffFinder(new AIParametricBluffFinder(10, 85, SEED));
		b.setInstinct(new AIParametricInstinct(80, SEED));
		b.setDecisionMaker(new AISimpleDecisionMaker(90, SEED));
		b.setPreBetLimit(new AIAggressivePreBetLimit());
		b.setPostBetLimit(new AIAggressivePostBetLimit());
		
		setBrain(b);
		this.setSmallIcon("MarxT.gif");
		this.setBigIcon("Marx.gif");
	}
	
}
