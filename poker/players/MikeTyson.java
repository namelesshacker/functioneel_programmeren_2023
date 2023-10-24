package players;

import images.Constants;
import ai.*;
import ai.bets.AIParametricBetDivider;
import ai.bets.AIParametricInstinct;
import ai.bets.betlimit.AIAggressivePostBetLimit;
import ai.bets.betlimit.AIAggressivePreBetLimit;
import ai.bets.bluff.AIParametricBluffFinder;
import ai.bets.bluff.AIParametricBluffer;
import ai.discards.AIStandardDiscardStrategy;
import ai.util.AISimpleDecisionMaker;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * Aggressive to the max!  Rarely is he missing from the Post-Bet round as he blitzes his opponents in 
 * order to scare them off.  However, he has very little memory and therefore doesn't remember when/how
 * other players bluffed.  He uses all of his strength in offensive and jumps in without really
 * considering his hand nor other player's bets.
 * 
 * His main characteristic is trying to knock out as many players as possible as soon as possible (in
 * the first 5 rounds)
 *
 */
public class MikeTyson extends AIParametricPlayer{

	public MikeTyson(String NAME, int PURSE, int SEED)
	{
		super("Mike Tyson"+NAME, PURSE, SEED);

		AIParametricBrain b= new AIParametricBrain("Mike Tyson"+NAME, 105, SEED);
		
		b.setBetDivider(new AIParametricBetDivider(20, SEED));
		b.setBluffer(new AIParametricBluffer(10, 20, SEED));	
		b.setDiscardStrategy(new AIStandardDiscardStrategy());	
		b.setBluffFinder(new AIParametricBluffFinder(2, 85, SEED));
		b.setInstinct(new AIParametricInstinct(80, SEED));
		b.setDecisionMaker(new AISimpleDecisionMaker(35, SEED));
		b.setPreBetLimit(new AIAggressivePreBetLimit());
		b.setPostBetLimit(new AIAggressivePostBetLimit());
		
		this.setBrain(b);

		this.setSmallIcon("TysonT.gif");
		this.setBigIcon("Tyson.gif");
	}
}
