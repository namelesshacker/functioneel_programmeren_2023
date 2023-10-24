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
 * LeChiffre was meant to be a "simple" player, but he ends up outcompeting Hasselhoff, 
 * his standard "equivalent" too often for this to apply. His discard strategy tends
 * to focus on the second best discard probability, as defined by Hasselhoff's Standard
 * Discard Strategy. However, this seems to produce fairly good results most of the time!
 * He sets himself a high comfort zone and decent recovery when his comfort zone is exceeded. 
 * He raises 50% of the time when given the chance, but never much. His bluff detection suffers
 * from a short memory span however. 
 */
public class LeChiffre extends AIParametricPlayer {

	public LeChiffre(String NAME, int PURSE, int SEED)
	{
		super("Le Chiffre" + NAME, PURSE, SEED);
		
		AIParametricBrain b4 = new AIParametricBrain("Le Chiffre" + NAME, PURSE, SEED);
		
		b4.setBetDivider(new AIParametricBetDivider(5, SEED));
		b4.setBluffer(new AIParametricBluffer(18, 20, SEED));
		b4.setDiscardStrategy(new AISimpleDiscardStrategy());
		b4.setBluffFinder(new AIParametricBluffFinder(2, 85, SEED));
		b4.setInstinct(new AIParametricInstinct(50, SEED));
		b4.setDecisionMaker(new AISimpleDecisionMaker(50, SEED));
		b4.setPreBetLimit(new AISimplePreBetLimit());
		b4.setPostBetLimit(new AISimplePostBetLimit());
		
		super.setBrain(b4);
		this.setSmallIcon("LechiffreT.gif");
		this.setBigIcon("Lechiffre.gif");
	}	
}
