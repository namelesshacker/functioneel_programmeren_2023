package ai.util;

import game.actions.*;
import game.exceptions.HandFoldedException;

import java.util.*;
import scoring.*;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class is uniform for all AIPlayers when revealing their hands: if a player's hand beats
 * all the hands on the board, it shows.  Otherwise, it folds.
 *
 */
public class AIScoreRevealStrategy {

	private ArrayList<ScoreRevealAction> actions;
	
	public AIScoreRevealStrategy()
	{
		actions=new ArrayList<ScoreRevealAction>();
	}
	
	public final ScoreRevealAction.scoreRevealAction makeScoreRevealAction(HandValue myHandValue)
	{
		int numScoreReveals = actions.size();
		
		// If this player is the first to show, he always shows
		if(numScoreReveals == 0)
			return ScoreRevealAction.scoreRevealAction.SHOW;
		
		// If not the first, check if any of the revealed hands are better, if so fold, if not show
		else
		{
			for(int i=0; i < numScoreReveals; i++)
			{
				ScoreRevealAction s = actions.get(i);
				try
				{
					// If a revealed hand is better, fold!
					if(myHandValue.compareTo((new HandValuator()).valuateHand(s.getHand())) < 0 )
						return ScoreRevealAction.scoreRevealAction.FOLD;
				}
				catch(HandFoldedException e)
				{ 
					//Continue the loop if opponenet's hand was folded
				}
			}
		}
		return ScoreRevealAction.scoreRevealAction.SHOW;
	}
	
	public final void reset()
	{
		actions=new ArrayList<ScoreRevealAction>();
	}
	
	public final void notify(Action action)
	{
		actions.add((ScoreRevealAction) action);
	}
}
