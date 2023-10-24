package players;

import images.Constants;
import ai.*;
import ai.bets.AIParametricBetDivider;
import ai.bets.AIParametricInstinct;
import ai.bets.betlimit.*;
import ai.bets.bluff.*;
import ai.discards.*;
import ai.util.*;

public class ErnestoGuevara extends AIParametricPlayer {
	
	public ErnestoGuevara(String NAME, int PURSE, int SEED)
	{
		super("Ernesto Guevara"+NAME, PURSE, SEED);

		AIParametricBrain b = new AIParametricBrain("Ernesto Guevara"+NAME, PURSE, SEED);
		
		b.setBetDivider(new AIParametricBetDivider(10, SEED));
		b.setBluffer(new AIParametricBluffer(50, 20, SEED));
		b.setDiscardStrategy(new AISimpleDiscardStrategy());
		b.setBluffFinder(new AIParametricBluffFinder(20, 100, SEED));
		b.setInstinct(new AIParametricInstinct(100, SEED));
		b.setDecisionMaker(new AISimpleDecisionMaker(70, SEED));	// THE BIGGEST INFLUENCE!! 
		b.setPreBetLimit(new AISimplePreBetLimit());
		b.setPostBetLimit(new AIAggressivePostBetLimit());
		
		super.setBrain(b);

		this.setSmallIcon("GuevaraT.gif");
		this.setBigIcon("Guevara.gif");
	}
}
