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
 * Hasselhoff was meant to be a standard player but turns out to be quite sub-ordinary.
 * His discard strategy goes for that discard which leads to the biggest hand, most often. 
 * This however tends to overemphasize five card hands somewhat too much. Hasselhoff
 * sets aggressive "comfort zones" for himself through aggressive limits. 
 * When his comfort zone is challenged, he will very often continue
 * playing provided his hand is not too bad. Because he is fairly resistant to high-wages
 * but has an incomplete understanding of how to achieve good hands (2 pair, 3 of a kind)
 * he usually doesn't do that well ... or not too well. Furthermore: he RAISES pretty much
 * 90% of the time when given an offensive chance, which isn't very intelligent. 
 */
public class DavidHasselhoff extends AIParametricPlayer {

	public DavidHasselhoff(String NAME, int PURSE, int SEED)
	{
		super("David Hasselhoff" + NAME, PURSE, SEED);
		
		AIParametricBrain b3 = new AIParametricBrain("David Hasselhoff" + NAME, PURSE, SEED);
		
		b3.setBetDivider(new AIParametricBetDivider(10, SEED));
		b3.setBluffer(new AIParametricBluffer(10, 20, SEED));
		b3.setDiscardStrategy(new AIStandardDiscardStrategy());
		b3.setBluffFinder(new AIParametricBluffFinder(5, 85, SEED));
		b3.setInstinct(new AIParametricInstinct(80, SEED));
		b3.setDecisionMaker(new AISimpleDecisionMaker(10, SEED));	 
		b3.setPreBetLimit(new AIAggressivePreBetLimit());
		b3.setPostBetLimit(new AIAggressivePostBetLimit());
		
		super.setBrain(b3);
		this.setSmallIcon("HasselhoffT.gif");
		this.setBigIcon("Hasselhoff.gif");
	}	
}
