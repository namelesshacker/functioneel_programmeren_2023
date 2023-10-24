package ai;

import ai.util.*;
import game.actions.*;
import util.*;
import scoring.HandValuator;
import java.util.*;

import money.Purse;

import game.*;
import game.exceptions.*;
import scoring.*;

/**
 * @class AIParametricBrain
 * @author david kawrykow and gen kazama
 *
 * This class determines virtually all actions that an AI player makes.
 * It makes use of several smaller utility AI classes to drive these decisions. 
 * Here is the general decision process with a brief description of how one may
 * influence it.
 * 
 * The Brain receives a Hand. It immediately calculates which cards to discard
 * come discard time (if any) and whether or not this discard is likely to succeed. 
 * This is done using an implementation of AIDiscardStrategy, which needs to be set before
 * the brain can operate. The returned information is referred to as a DiscardProbability.
 * There are 6 DiscardStrategies: Standard, Conservative, Aggressive, Simple, B&W, and HighHand.
 * The Standard discard strategy is (in our opinion), the smartest. Conservative and Aggressive
 * are Standard with different emphases on what is considered "good". Simple is sub-par to 
 * Standard and B&W is sub-par to Simple (but still effective). The HighHand ignores 5-card hands
 * and tries mainly for pairs, two pairs and 3 of a kinds - the hands most likely to occur 
 * in the first place.  
 * 
 * This DiscardProbability is then immediately fed into an implementation of the PreBetLimit
 * Interface. The PreBetLimit class selects an initial maximum betting value for this round.
 * The betting value is a proportion of your purse. Here there are again several methods of setting
 * an initial limit for yourself: Simple, Aggressive, and Conservative. The interpretation
 * of these limits is obvious. 
 * 
 * The proportion is then fed into an implementation of the AIBluffer Interface. This 
 * class increments the initial maximum betting value by some smaller factor, i.e. it 
 * allows you to handle larger raises as being "no sweat" (explanation to follow). This class
 * is parametrized: i.e. clients specify roughly how often the limit is extended and by 
 * roughly how much. This gives a sliding scale for Conservative to Aggressive "no sweat"
 * betting. 
 * 
 * Finally the brain takes in an AISimpleDecisionMaker which determines roughly how often
 * you will CALL vs. RAISE under "no sweat" circumstances i.e. when you're still within
 * the bounds of your hand, this class determines whether you keep it cool, or whether
 * you raise. (if you're no longer in your bounds, this class does not apply any longer). 
 * It also takes in an AIBetDivider which determines HOW MUCH you want to raise, given that
 * you want to raise. Both of these classes are parametrized (see descriptions) to allow for
 * a smooth transition from Conservative to Aggressive "offensive betting" styles.  
 * 
 * Now when a bet is demanded from the player, things become interesting. 
 * 
 * The Brain first 
 * checks if anyone has raised. If nobody has raised the AISimpleDecisionMaker is asked
 * to decide whether you CALL ( ~ check) or RAISE. If you RAISE, the AIBetDivider computes
 * how much you want to RAISE (this is a random amount within a range you gave it). 
 * 
 * If a RAISE
 * has been issued the Brain checks whether it is in your "no sweat" range or whether it is not.
 * If it is, then a decision based mostly on your DiscardProbability is made as to whether
 * you FOLD or CONTINUE PLAYING. This decision is also influenced minimally by the Brain's 
 * AIBluffFinder (i.e. whether you think anyone has bluffed in the past or not) and a very 
 * very small amount by how much money is involved (usually this affects your decision by 5% at
 * most). If you CONTINUE PLAYING, another AISimpleDecisionMaker decision is made as to whether
 * you raise or call. If you FOLD, then the consequences are obvious.  
 * 
 * If the RAISE 
 * does not fall in your "no sweat" zone, then a very serious FOLDING decision is taken again. 
 * Here the influence of money is much greater than before and reaches up to 25% of your
 * decision. If you survive the FOLD and continue playing, your AIInstinct implementation is used
 * to see whether 
 */

public class AIParametricBrain {
	
	/**
	 * Final Variables
	 */
	
	/* Random seed for all randomness */
	private final int SEED;
	
	/* Random number generators */
	private final AINumberGenerator RANDOM;
	private final Random rRANDOM;
	
	/* The name of the body this brain resides in */
	private final String NAME;
	
	/**
	 * Neurons
	 */
	
	/* Pre and Post Bet Limits */
	private AIPreBetLimit PreBetLimit; 
	private AIPostBetLimit PostBetLimit; 
	
	/* Bluff */
	private AIBluffer bluffer;
	private AIBluffFinder BluffFinder;
	
	/* Betting helper objects */
	private AIBetDivider betDivider;
	private AIInstinct Instinct; 
	
	/* Discarding cards */
	private AIDiscardStrategy DiscardStrategy;
	
	/* Revealing hands */
	private AIScoreRevealStrategy scoreRevealStrategy;
	
	/* Deciding whether to call or raise */
	private AISimpleDecisionMaker decisionMaker;
	
	private HandValuator handValuator; 
	private AIPreBetHandValuator preBetHandValuator;
	
	private ArrayList<String> myOpponents;
	
	private Purse purse;
	
	/**
	 * Variables for one round
	 */
	
	/* Money in the purse for this round */
	private int rMoneyInPurse;
	
	private int rMaxPercentagePre;
	private int rMaxPercentagePost;
	private int rPercentSpentPre;
	private int rPercentSpentPost;
	private Hand rHand;
	private HandValue rHandValue;
	private DiscardProbability rProbability;
	
	/* Whether someone went all in (for score reveal strategy) */
	private boolean rAllIn;
	
	/* The Last raise for this round */
	private BetAction rLastRaise;
	
	
	public AIParametricBrain(String myName, int purseAmount, int seed)
	{
		handValuator = new HandValuator();
		preBetHandValuator = new AIPreBetHandValuator();
		
		scoreRevealStrategy = new AIScoreRevealStrategy();
		
		rMaxPercentagePre = 0;
		rMaxPercentagePost = 0;
		rPercentSpentPre = 0;
		rPercentSpentPost = 0;
		rAllIn=false;
		
		NAME = myName;
		SEED = seed;
		RANDOM = new AINumberGenerator(SEED);
		rRANDOM = new Random(SEED*-3);
		
		myOpponents = new ArrayList<String>(4);
		
		try
		{
			purse = new Purse(purseAmount);
			rMoneyInPurse=purseAmount;
		}
		catch(InvalidMoneyValueException e)
		{
			assert false;
		}
	}
	
	/**
	 * Starts a new Betting Round. The flavour of the round is largely
	 * determined by the hand. The Brain assumes that a new betting round
	 * begins before any cards have been exchanged 
	 */
	public final void startPreBettingRound()
	{
		// Set the variables to default
		rMaxPercentagePre = 0;
		rPercentSpentPre = 0;
		rPercentSpentPost = 0;
		rPercentSpentPost = 0;
		rLastRaise = null;
		rAllIn = false;
		
		try
		{
			rMoneyInPurse = purse.getChips();
		}
		catch(PurseIsEmptyException e)
		{
			rMoneyInPurse=0;
		}
	
		// Discard probability from the strategy
		this.rProbability = this.DiscardStrategy.bestDiscard(rHand);
	
		// Pre Bet Set up
		this.PreBetLimit.setDiscardProbability(rProbability);
		this.PreBetLimit.setHand(rHand);
		this.PreBetLimit.setHandValue(rHandValue);
		
		// Original upper bound
		rMaxPercentagePre = this.PreBetLimit.getPercentage();
		
		// Compute the next upper bound (with bluffing)
		this.bluffer.setBettingPercentage(rMaxPercentagePre);
		rMaxPercentagePre = this.bluffer.getBettingPercentage();
		
		// Set the Instinct
		this.Instinct.setCurrentMax(rMaxPercentagePre);
		this.Instinct.setHandValue(rHandValue);
	}

	/**
	 * 
	 * @param minBet
	 * @return what betaction the player attempts
	 */
	public final BetAction getNextPreBet(int minBet)
	{
		int preBetRaise;
		// The percentage of your purse needed to remain in the pot
		int minBetPercentage = ((minBet *100) / rMoneyInPurse) + rPercentSpentPre;
		
		
		
		// If the min bet percentage is over 100 or 0, give him a slight chance
		if(minBetPercentage > 100)
			minBetPercentage = 100;
		else if(minBetPercentage < 0)
			minBetPercentage = 1;
				
		BetAction result;
		if( minBetPercentage <= rMaxPercentagePre)
		{
			if(rHandValue.getCategory().ordinal() >= HandValue.Category.STRAIGHT.ordinal())
				result=new BetAction(NAME, BetAction.betAction.RAISE, getPostBetRaise());
			
			// No one has raised yet (it's your move)
			else if(minBet == 0)
			{	
				// Raise (TRUE)
				if(decisionMaker.getDecision())
				{
					// Make sure not to raise by 0
					preBetRaise = getPreBetRaise();
					if(preBetRaise > 0)
						result= new BetAction(NAME, BetAction.betAction.RAISE, preBetRaise);
					else
						result= new BetAction(NAME, BetAction.betAction.CALL, 0);
				}
		
				// Call (FALSE)
				else
					result=new BetAction(NAME, BetAction.betAction.CALL, 0);
			}
			
			// Someone raised before you
			else	
			{	
				// Decide whether to continue playing or fold (Playing in this case is encouraged
				// since you are still within your bounds.)
				if(getPreBetFoldPlayDecision(minBetPercentage/9) ||  
				   rHandValue.getCategory().ordinal() >= HandValue.Category.STRAIGHT.ordinal()) // TRUE
				{
					// Decide to call (FALSE)
					if(!this.decisionMaker.getDecision())	
						result = new BetAction(NAME, BetAction.betAction.CALL,0);
					
					// Raise (TRUE)
					else
					{
						// Make sure not to raise by 0 if raising
						preBetRaise = getPreBetRaise();
						if(preBetRaise > 0)
							result= new BetAction(NAME, BetAction.betAction.RAISE, preBetRaise);
						else
							result= new BetAction(NAME, BetAction.betAction.CALL, 0);
					}
				}	
				
				// Fold (FALSE)
				else
					result= new BetAction(NAME, BetAction.betAction.FOLD, 0);
			}
		}
		// Out of your bounds
		else
		{	
			// Choose between folding and playing (playing in this case)
			if(this.getPreBetFoldPlayDecision(minBetPercentage *2) || 
			   rHandValue.getCategory().ordinal() >= HandValue.Category.STRAIGHT.ordinal())
			{
				// Raise (TRUE)
				if(this.getPreBetCallRaiseDecision())
				{
					// Don't raise by 0!
					preBetRaise = getPreBetRaise();
					if(preBetRaise > 0)
						result= new BetAction(NAME, BetAction.betAction.RAISE, preBetRaise);
					else
						result= new BetAction(NAME, BetAction.betAction.CALL, 0);
				}
				// Call (FALSE)
				else
					result= new BetAction(NAME, BetAction.betAction.CALL,0);
			}	
			// Attempt to Fold
			else
			{
				// If the instinct tells the player to fold (false), fold!
				if(! Instinct.getPreBetFoldPlayDecision())
				{
					result =  new BetAction(NAME, BetAction.betAction.FOLD, 0); 
				}
				// Raise (TRUE)
				else if(this.getPreBetCallRaiseDecision())
				{
					// Don't raise by 0!
					preBetRaise = getPreBetRaise();
					if(preBetRaise > 0)
						result= new BetAction(NAME, BetAction.betAction.RAISE, preBetRaise);
					else
						result= new BetAction(NAME, BetAction.betAction.CALL, 0);
				}
				else
				{
					result= new BetAction(NAME, BetAction.betAction.CALL,0);
				}
			}
		}
		
		// Update rPercentSpent
		if(result.getAction() == BetAction.betAction.CALL)
			rPercentSpentPre = minBetPercentage;
		
		else if(result.getAction() == BetAction.betAction.RAISE)
			rPercentSpentPre = minBetPercentage + (result.getAmount() * 100) / rMoneyInPurse;
		
		// Just in case ... so we dont get CONFUZZED 
		if(rPercentSpentPre > 100)
			rPercentSpentPre = 100;
		
		// If we're spending more than our previous max, update the max! :D 
		if(rPercentSpentPre > rMaxPercentagePre)
			rMaxPercentagePre = rPercentSpentPre;

		return result;
	}
	
	/**
	 * 
	 * @return an integer that represnts the amount (not percentage) to raise by
	 */
	private final int getPreBetRaise()
	{
		// Sets the bet percentage to the next bet from divider
		int betPercentageAmount = this.betDivider.getNextBet();
		
		// adds the ordinal of the current hand
		betPercentageAmount += rHandValue.getCategory().ordinal();
		
		return betPercentageAmount * rMoneyInPurse / 100;
	}
	
	/**
	 * Get a decision on whether to fold or play
	 * @param minBetPercentage
	 * @return whether to fold or play
	 */
	private final boolean getPreBetFoldPlayDecision(int minBetPercentage)
	{
		// Fold or bet
		AISimpleDecisionMaker s;
		
		int y;
		int z;
		int x;
		int sum;
		
		// MinBetPercentage might be larger than 100 so just to make sure
		if(minBetPercentage > 100)
			minBetPercentage = 100;
		
		// Things making you less likely to fold: a high hand, the raising player is
		// bluffing, the money involved is not too great. We are trying to MINIMIZE
		// the value of sum i.e. to minimize the folding percentage
		
		// Standard value with
		y=9;
		
		// Discard Probability if bad =0, if so-so=9, if good > 9, if amazing =16
		if(this.rProbability.getProbability() == DiscardStrategy.badProbability())
			y = 0;
		else if(this.rProbability.getProbability() == DiscardStrategy.goodProbability())
			y = 16;
		else 
		{
			y += preBetHandValuator.evaluateHand(rProbability, rHand).ordinal() + 4;
			
			if(y == 13)		// HIGH CARD DENIED!
				y = 9; 
		}
		
		if(y > 16)
			y = 16; 
		
		// z represents the bluff factor.
		z = 0;
		if(rLastRaise != null)
			if(this.BluffFinder.isPlayerBluffing(this.rLastRaise.getActionMaker()))
				z = 1;
		// If the person in question bluffed then the folding goes down proportionally to
		// the hand ... so bad hand means bluffs still scare you  ... 
		z *= (y / 4);
		
		//  x represents the min bet factor from 0-64 . if its high, then x is low etc. 
		x = 8 - ((minBetPercentage * 8) / 100);
		
		// make sum a percentage from 0-100 (for call/raise vs fold)
		sum = y + z + x;
		sum = 28 - sum;
		
		// we normalize it to be from 0 - 100 
		sum = (sum * 100) / 28;	// 32 is 16 (Hand) + 4 (Bluff) + 8 (Money)
		
		s = new AISimpleDecisionMaker(sum, rRANDOM.nextInt()*rRANDOM.nextInt());
		
		return s.getDecision();
	}
	
	/**
	 * 
	 * @return whether to call or raise
	 */
	private final boolean getPreBetCallRaiseDecision()
	{
		int divider = 0;
		
		// If a good hand, call/raise 35%
		if(this.rProbability.getProbability() > 0)
			divider = 35;
		// If amazing hand, raise 70% of the time
		else if(this.rProbability.getProbability() == 0)
			divider = 70;
		
		// flip divider to comply with interpretation
		divider = 100 - divider; 
		
		AISimpleDecisionMaker s = new AISimpleDecisionMaker(divider, rRANDOM.nextInt());
		
		return s.getDecision();
	}
	
	/**
	 * Resets the betting round with the updated hand. The brain adjusts
	 * to what the hand now captures. 
	 */
	public final void startPostBetting()
	{	
		// Post Bet Limit
		PostBetLimit.setHandValue(rHandValue);
		
		rMaxPercentagePost = PostBetLimit.getPercentage();
		
		// Never just fold
		if(rMaxPercentagePost == 0)
			rMaxPercentagePost = 2;
		
		// Get updated Max with bluffing
		bluffer.setBettingPercentage(rMaxPercentagePost);
		rMaxPercentagePost = bluffer.getPostBluff();
		
		if(rMaxPercentagePost > 100)
			rMaxPercentagePost = 100;
		
		if(rMaxPercentagePost < rPercentSpentPre)
			rMaxPercentagePost = rPercentSpentPre;
		
		rMaxPercentagePost -=  rPercentSpentPre;
		rPercentSpentPost   = 0; 
		
		// Get the betDivider and instinct ready
		this.Instinct.setCurrentMax(rMaxPercentagePost);
		this.Instinct.setHandValue(rHandValue);
				
	}
	
	/**
	 * This function returns the next bet by polling the BetDivider for the
	 * next bet it should make
	 * 
	 * @param minBet (in chips, not percentage)
	 * @return the next bet (in chips, not percentage)
	 */
	public final BetAction getNextPostBet(int minBet)
	{
		// The percentage of your purse needed to remain in the pot
		int minBetPercentage = ((minBet *100) / rMoneyInPurse) + rPercentSpentPost; 
		int postBetRaise;
		
		// If the min bet percentage is over 100 or 0, give him a slight chance
		if(minBetPercentage > 100)
			minBetPercentage = 100;
		else if(minBetPercentage < 0)
			minBetPercentage = 1;
		
		BetAction result;
		// Within your bounds
		if( minBetPercentage <= rMaxPercentagePost)
	
			// No one has raised yet (it's your move)
			if(minBet == 0)
			{	
				if(rHandValue.getCategory().ordinal() >= HandValue.Category.STRAIGHT.ordinal())
					result=new BetAction(NAME, BetAction.betAction.RAISE, getPostBetRaise());			
				else
				{
					// Raise (TRUE)
					if(decisionMaker.getDecision())	
					{
						// Don't raise by 0!
						postBetRaise = getPostBetRaise();
						if(postBetRaise > 0)
							result= new BetAction(NAME, BetAction.betAction.RAISE, postBetRaise);
						else
							result= new BetAction(NAME, BetAction.betAction.CALL, 0);
					}
			
					// Call (FALSE)
					else
						result = new BetAction(NAME, BetAction.betAction.CALL, 0);
				}
			}
			// Someone raised before you
			else	
				
				// Decide whether to continue playing or fold (Play in this case)
				if(this.getPostBetFoldPlayDecision(minBetPercentage/9) ||
				   rHandValue.getCategory().ordinal() >= HandValue.Category.STRAIGHT.ordinal()) // TRUE
				{
					if(rHandValue.getCategory().ordinal() >= HandValue.Category.STRAIGHT.ordinal())
						result=new BetAction(NAME, BetAction.betAction.RAISE, getPostBetRaise());
					
					// Decide to call (FALSE)
					else if(!this.decisionMaker.getDecision())	
						result = new BetAction(NAME, BetAction.betAction.CALL,0);
					
					// Raise (TRUE)
					else			
					{
						// Dont raise by 0!
						postBetRaise = getPostBetRaise();
						if(postBetRaise > 0)
							result= new BetAction(NAME, BetAction.betAction.RAISE, postBetRaise);
						else
							result= new BetAction(NAME, BetAction.betAction.CALL, 0);
					}
					
				}
				// Fold (FALSE)
				else
					result = new BetAction(NAME, BetAction.betAction.FOLD, 0);
				
		// Out of your bounds
		else
			
			// Choose between folding and playing (playing in this case)
			if(this.getPostBetFoldPlayDecision(minBetPercentage *2) ||
			   rHandValue.getCategory().ordinal() >= HandValue.Category.STRAIGHT.ordinal())
		
				// Raise
				if(this.getPostBetCallRaiseDecision())
				{
					// Don't raise
					postBetRaise = getPostBetRaise();
					if(postBetRaise > 0)
						result= new BetAction(NAME, BetAction.betAction.RAISE, postBetRaise);
					else
						result= new BetAction(NAME, BetAction.betAction.CALL, 0);
				}
				
				// Call
				else
					result = new BetAction(NAME, BetAction.betAction.CALL,0);
				
			// Attempt to Fold
			else
				// If the instinct tells the player to fold (false), fold!
				if(! Instinct.getPostBetFoldPlayDecision())
					result = new BetAction(NAME, BetAction.betAction.FOLD, 0);
				
				// The instinct tells the player to keep playing
				else
					// Raise
					if(this.getPostBetCallRaiseDecision())
					{
						// Don't raise by 0!
						postBetRaise = getPostBetRaise();
						if(postBetRaise > 0)
							result= new BetAction(NAME, BetAction.betAction.RAISE, postBetRaise);
						else
							result= new BetAction(NAME, BetAction.betAction.CALL, 0);
					}
					
					// Call
					else
						result = new BetAction(NAME, BetAction.betAction.CALL,0);
	
		// Update rPercentSpent
		if(result.getAction() == BetAction.betAction.CALL)
			rPercentSpentPost = minBetPercentage;
		else if(result.getAction() == BetAction.betAction.RAISE)
			rPercentSpentPost = minBetPercentage + (result.getAmount() * 100) / rMoneyInPurse;
				
		if(rPercentSpentPost + rPercentSpentPre > 100)
			rPercentSpentPost = 100 - rPercentSpentPre;
		
		if(rPercentSpentPost > rMaxPercentagePost)
			rMaxPercentagePost = rPercentSpentPost;
		
		return result;
	}
	
	/**
	 * 
	 * @return an integer that represents the amount to raise by
	 */
	private final int getPostBetRaise()
	{
		// Sets the bet percentage to the next bet from divider
		int betPercentageAmount = this.betDivider.getNextBet();
		
		// adds the ordinal of the current hand
		betPercentageAmount += rHandValue.getCategory().ordinal();
		
		return (betPercentageAmount *  rMoneyInPurse) / 100;
	}
	
	/**
	 * Get a decision on whether to fold or play
	 * @param minBetPercentage
	 * @return whether to fold or play
	 */
	private final boolean getPostBetFoldPlayDecision(int minBetPercentage)
	{
		// Fold or bet
		AISimpleDecisionMaker s;
		
		int y;
		int z;
		int x;
		int sum;
		
		// MinBetPercentage might be larger than 100
		if(minBetPercentage > 100)
			minBetPercentage = 100;
		
		// Here we focus on the exact hand (or AI-exact)
		// High hand means higher chance of going on! 
		y = rHandValue.getCategory().ordinal();
		
		// MAke any hand above high card a lot better
		if(y > 0)
			y += 6;
		
		y *= y;
		
		if(y > 75)
			y = 75; 
		
		// z represents the bluff factor from 0-25 as in pre bet
		z = 0;
		if(rLastRaise != null)
			if(this.BluffFinder.isPlayerBluffing(this.rLastRaise.getActionMaker()))
				z = 1;
		z *= (y / 4);
		
		//  x represents the min bet factor from 0-10 as in pre-bet
		x = 25 - ((minBetPercentage * 25) / 100);
		
		// make sum a percentage from 0-100 (for call/raise vs fold)
		sum = y + z + x;
		
		// normalize it to 100%C
		sum = (sum * 100) / 125;
		
		// reverse to reflect folding vs. playing interpretation
		sum = 100 - sum; 
		
		s = new AISimpleDecisionMaker(sum, rRANDOM.nextInt()*rRANDOM.nextInt());
		
		return s.getDecision();
	}
	
	/**
	 * 
	 * @return whether to call or raise
	 */
	private final boolean getPostBetCallRaiseDecision()
	{
		// set the divider in range 0 - 100
		int divider = (rHandValue.getCategory().ordinal() * 25) / 2;
		
		// flip divider to say FALSE = CALL, and TRUE = RAISE
		divider = 100 - divider; 

		AISimpleDecisionMaker s = new AISimpleDecisionMaker(divider, rRANDOM.nextInt()*rRANDOM.nextInt());
		
		return s.getDecision();
	}
	
	
	/**
	 * Used by the AIPlayer to discard cards
	 * @return The ArrayList of cards to discard
	 */
	public final ArrayList<Card> getDiscardCards()
	{
		rProbability=this.DiscardStrategy.bestDiscard(rHand);
		return rProbability.getArrayListCards();
	}
	
	/**
	 * Used by the AIPlayer to reveal their hand
	 * @return The ScoreRevealAction (SHOW or FOLD) to take based on other players' actions
	 * 		   This is overridden if someone went all-in, then, the player always shows
	 * 		   because there is the chance of smaller side pots that can be won.
	 */
	public final ScoreRevealAction.scoreRevealAction getScoreRevealAction()
	{
		// If someone went all-in (including yourself), show your hand!
		if(this.rAllIn)
			return ScoreRevealAction.scoreRevealAction.SHOW;
		
		return scoreRevealStrategy.makeScoreRevealAction(rHandValue);
	}
	
	public final void notify(Action action)
	{
		BluffFinder.updateGameHistory(action);
		if(action instanceof NewGameAction)
		{
			// Set the arraylist of opponents
			NewGameAction lAction = (NewGameAction) action;
			myOpponents = new ArrayList<String>(lAction.getNumberOfParticipants());
			for(int i=0; i<lAction.getNumberOfParticipants(); i++)
			{
				String temp = lAction.getParticipant(i+1);
				if(! temp.equals(NAME))
					myOpponents.add(temp);
			}
		}
		else if(action instanceof NewMatchAction)
		{
			// reset the score reveal strategy (purge it of the last matche's history
			scoreRevealStrategy.reset();
		}
		else if(action instanceof ScoreRevealAction)
		{
			// Notify the score reveal strategy
			scoreRevealStrategy.notify((ScoreRevealAction) action);
		}
		else if(action instanceof BetAction)
		{
			BetAction lAction = (BetAction) action;
			
			// All-in becomes true (for score reveal action purposes)
			if(lAction.getAction() == BetAction.betAction.ALL_IN)
				rAllIn = true;
			
			// Update the raise
			else if(lAction.getActionMaker() != NAME && lAction.getAction() == BetAction.betAction.RAISE)
				rLastRaise = lAction;
		}
		else if(action instanceof PlayerRemovedAction)
		{
			// update the arraylist of opponents
			myOpponents.remove( ((PlayerRemovedAction) action).getActionMaker());
			if(! action.getActionMaker().equals(NAME))
				makeBrainMoreAggressive();
		}
	}
	
	/**
	 * This is called when a player is removed to speed things up
	 *
	 */
	private final void makeBrainMoreAggressive()
	{
		int newDivider;
		int newRange;
		
		// Set the DesicionMaker to become more aggressive (more raises)
		newDivider = decisionMaker.getDivider() - 20;
		
		if(newDivider < 0)
			newDivider = 0;
		this.decisionMaker = new AISimpleDecisionMaker(newDivider, SEED*SEED);
		
		// Set the range
		newRange = this.betDivider.getRange() + 20;
		
		if(newRange > 100)
			newRange = 100;
		this.betDivider.setRange(newRange);
	}
	
	/** Set + Get **/
	
	
	/**
	 * Sets the DiscardStrategy Object. This object informs the Brain
	 * which cards are to be discarded come discard time (if any). 
	 * The confidence of the discard initiates the betting sequence. 
	 */
	public final void setDiscardStrategy(AIDiscardStrategy DiscardStrategy)
	{
		this.DiscardStrategy = DiscardStrategy; 
	}
	
	/**
	 * Sets the PreBetLimit Object. This is used to 
	 * determine an initial stab at what the player's bet
	 * should be during the pre-bet round
	 */
	public final void setPreBetLimit(AIPreBetLimit PreBetLimit)
	{
		this.PreBetLimit = PreBetLimit; 
	}
	
	/**
	 * Sets the PostBetLimit Object. This is used to 
	 * determine an initial stab at what the player's bet
	 * should be during the post-bet round
	 */
	public final void setPostBetLimit(AIPostBetLimit PostBetLimit)
	{
		this.PostBetLimit = PostBetLimit; 
	}
	
	/**
	 * Sets the Bluffer Object. This is used to raise
	 * the initial pre/post bet by some "bluffing" factor.  
	 * It also aids in keeping the bluff constant
	 */
	public final void setBluffer(AIBluffer bluffer)
	{
		this.bluffer = bluffer;
	}
	
	/**
	 * Sets the BluffFinder Object. This is used to determine
	 * whether other players have been bluffing. This information
	 * is useful for raising/lowering bets during a round.  
	 */
	public final void setBluffFinder(AIBluffFinder BluffFinder)
	{
		this.BluffFinder = BluffFinder; 
	}
	
	/**
	 * Sets the Instinct Object. This object takes over when the 
	 * bound defined by the more reasonable methods can no longer
	 * satisfy the requirements of the game pot. 
	 */
	public final void setInstinct(AIInstinct Instinct)
	{
		this.Instinct = Instinct; 
	}
	
	/**
	 * Sets the Bet Divider Object.  This object makes the next bet for
	 * the brain which has a little randomness thrown in to throw off
	 * humans (and other AI).
	 */
	public final void setBetDivider(AIBetDivider betDivider)
	{
		this.betDivider=betDivider;
	}
	
	/**
	 * Sets the DecisionMaker object.  This object provides the brain with
	 * a solution when it is stuck in a paradox.  Basically, it is a weighted
	 * coin flip for the brain.
	 */
	public final void setDecisionMaker(AISimpleDecisionMaker decisionMaker)
	{
		this.decisionMaker=decisionMaker;
	}
	
	public final void setHand(Hand hand)
	{
		rHand = hand;
		rHandValue = handValuator.valuateHand(rHand);
	}
	
	public final void setPurse(Purse purse)
	{
		this.purse = purse;
	}
	
	/**
	 * 
	 * @return the seed used to initialize all the randomness
	 */
	public final int getSeed()
	{
		return SEED;
	}
	
	public final String toString()
	{
		return "Name of this brain: "+ NAME +"\nSeed: "+SEED;
	}
}
