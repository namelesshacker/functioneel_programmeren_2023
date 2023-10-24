package game;

import game.actions.Action;
import game.actions.BetAction;
import game.actions.CardExchangeAction;
import game.actions.GetMoneyAction;
import game.actions.IncrementAnteAction;
import game.actions.PlayerRemovedAction;
import game.actions.ScoreAction;
import game.actions.ScoreRevealAction;
import game.exceptions.InvalidUpdateException;

import java.util.ArrayList;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class contains every action that can occur in one match.
 * GameStats uses this to keep statistics on the whole game.
 *
 */

public class MatchStats{
	
	 private ArrayList <BetAction> preExchangeBets;
	 private ArrayList <PlayerRemovedAction> antePlayersRemoved;
	 private ArrayList <CardExchangeAction> cardExchange;
	 private ArrayList <BetAction> postExchangeBets;
	 private ArrayList <ScoreRevealAction> revealCards;
	 private ArrayList <PlayerRemovedAction> revealPlayersRemoved;
	 private ArrayList <GetMoneyAction> getMoney;
	 private ArrayList <ScoreAction> scoreAction;
	 private IncrementAnteAction anteAction;
	 
	 private ArrayList<Action> matchActions;
	 
	 public MatchStats()
	 {
		 preExchangeBets=new ArrayList <BetAction>();
		 cardExchange=new ArrayList <CardExchangeAction>();
		 postExchangeBets=new ArrayList <BetAction>();
		 revealCards=new ArrayList <ScoreRevealAction>();
		 antePlayersRemoved=new ArrayList <PlayerRemovedAction>();
		 getMoney=new ArrayList<GetMoneyAction>();
		 revealPlayersRemoved = new ArrayList<PlayerRemovedAction>();
		 scoreAction=new ArrayList<ScoreAction>();
		 
		 matchActions = new ArrayList<Action>();
	 }
	 
	 /* Add actions to this MatchStats */
	 
	 public void addAction(Action action)
	 {
		 matchActions.add(action);
	 }
	 
	 public void addPreExchangeBetAction(BetAction action) throws NullPointerException, InvalidUpdateException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 preExchangeBets.add(action);
	 }
	 
	 public void addPostExchangeBetAction(BetAction action) throws NullPointerException, InvalidUpdateException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 
		 postExchangeBets.add(action);
	 }
	 
	 public void addCardExchangeAction(CardExchangeAction action) throws NullPointerException, InvalidUpdateException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 
		 cardExchange.add(action);
	 }
	 
	 public void addScoreRevealAction(ScoreRevealAction action) throws NullPointerException, InvalidUpdateException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 
		 revealCards.add(action);
	 }
	 
	 public void addAntePlayerRemovedAction(PlayerRemovedAction action) throws NullPointerException, InvalidUpdateException
	 {
		 if(action == null)
			 throw new NullPointerException();

		 antePlayersRemoved.add(action);
	 }
	 
	 public void addPlayerRemovedAction(PlayerRemovedAction action) throws NullPointerException, InvalidUpdateException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 
		 revealPlayersRemoved.add(action);
	 }

	 public void addIncrementAnteAction(IncrementAnteAction action) throws NullPointerException, InvalidUpdateException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 
		anteAction=action;
	 }
	 
	 public void addGetMoneyAction(GetMoneyAction action) throws NullPointerException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 
		 getMoney.add(action);
	 }
	 
	 public void addScoreAction(ScoreAction action) throws NullPointerException
	 {
		 if(action == null)
			 throw new NullPointerException();
		 
		 scoreAction.add(action);
	 }
	 
	 /* Get specific actions from this MatchStats */
	 
	 public BetAction getPreExchangeAction(int index) throws IllegalArgumentException
	 {
		 if(index<0 || index>=preExchangeBets.size())
			 throw new IllegalArgumentException();
		 return preExchangeBets.get(index); 
	 }

	 public BetAction getPostExchangeAction(int index) throws IllegalArgumentException
	 {
		 if(index<0 || index>=postExchangeBets.size())
			 throw new IllegalArgumentException();
		 return postExchangeBets.get(index); 
	 }
	 
	 public CardExchangeAction getCardExchangeAction(int index) throws IllegalArgumentException
	 {
		 if(index<0 || index>=cardExchange.size())
			 throw new IllegalArgumentException();
		 return cardExchange.get(index); 
	 }

	 public PlayerRemovedAction getPlayerRemovedAction(int index) throws IllegalArgumentException
	 {
		 if(index<0 || index>=revealPlayersRemoved.size())
			 throw new IllegalArgumentException();
		 return revealPlayersRemoved.get(index); 
	 }
	 
	 public GetMoneyAction getGetMoneyAction(int index) throws IllegalArgumentException
	 {
		 if(index<0 || index>=getMoney.size())
			 throw new IllegalArgumentException();
		 return getMoney.get(index); 
	 }
	 
	 public ScoreRevealAction getScoreRevealAction(int index) throws IllegalArgumentException
	 {
		 if(index<0 || index>=revealCards.size())
			 throw new IllegalArgumentException();
		 return revealCards.get(index); 
	 }
	 
	 public ScoreAction getScoreAction(int index) throws IllegalArgumentException
	 {
		 if(index<0 || index>=scoreAction.size())
			 throw new IllegalArgumentException();
		 return scoreAction.get(index);
	 }
	 
	 public IncrementAnteAction getIncrementAnteAction()
	 {
		 return anteAction;
	 }
	 
	 /* Get the number of a specfic action for this MatchStat */
	 
	 public int getNumPreExchangeBetActions()
	 {
		 return preExchangeBets.size();
	 }
	 
	 public int getNumPostExchangeBetActions()
	 {
		 return postExchangeBets.size();
	 }
	 
	 public int getNumCardExchangeActions()
	 {
		 return cardExchange.size();
	 }
	 
	 public int getNumGetMoneyActions()
	 {
		 return getMoney.size();
	 }
	 
	 public int getNumPlayerRemovedActions()
	 {
		 return revealPlayersRemoved.size();
	 }
	 
	 public int getNumScoreRevealActions()
	 {
		 return revealCards.size();
	 }
	 
	 public int getNumScoreActions()
	 {
		 return scoreAction.size();
	 }
	 public String toString()
	 {
		 return "Ante Action: " + anteAction +
		 		"\nRemoved Ante Players: "+antePlayersRemoved +
		 		"\nPreExchange: " + preExchangeBets +
		 		"\nDiscarded Cards: " + cardExchange +
		 		"\nPostExchange: " + postExchangeBets +
		 		"\nReveal Cards: " + revealCards +
		 		"\nRemoved Players: " + revealPlayersRemoved +
		 		"\nGet Money: " + getMoney+"\n";
	 }
}
