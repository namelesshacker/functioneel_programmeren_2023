package game;

import game.actions.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import scoring.HandValuator;
import util.Card;
import util.Hand;

public class GameFileParser {
	
	private boolean mCorrupted = false;
	private HandValuator mHandValuator;
	private BufferedReader mBufferedReader;
	private ArrayList<Action> mActions;
	
	public GameFileParser(String filename)
	{
		try
		{
			mActions        = new ArrayList<Action>();
			mHandValuator   = new HandValuator();
			mBufferedReader = new BufferedReader(new FileReader(filename));
			parseFile();
		}
		catch(FileNotFoundException e)
		{
			mCorrupted = true;
		}
	}
	
	public Iterator<Action> iterator()
	{
		return mActions.iterator();
	}
	
	public boolean isCorrupted()
	{
		return mCorrupted;
	}
	
	private void parseFile()
	{
		String tAction;
		
		try
		{
			do
			{
				tAction = mBufferedReader.readLine();
				
				if(tAction != null)
				{
					if(tAction.length() < 2)
						continue;
					
					mActions.add(parseString(tAction));
				}
				
			} while(tAction != null);
		}
		catch (IOException e)
		{
			mCorrupted = true;
			return;
		}
	}
	
	/**
	 * 
	 * @param actionString
	 * @return null if the string is empty
	 */
	public Action parseString(String actionString)
	{
		int i, j;
		String actionType;
		String actionMaker;
		
		try
		{	
			i = actionString.indexOf(Action.SEP_CHAR);
			
			actionType = actionString.substring(0, i);
			
			if(actionType.equals("BetAction"))
			{
				BetAction.betAction lAction; 
				int lAmount;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				actionMaker = actionString.substring(i+1, j);

				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				lAction = BetAction.betAction.valueOf(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				
				lAmount = Integer.valueOf(actionString.substring(i+1, j));
				
				return new BetAction(actionMaker, lAction, lAmount);
			}
			
			if(actionType.equals("CardExchangeAction"))
			{
				CardExchangeAction.cardExchangeAction lAction; 
				int lAmount;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				actionMaker = actionString.substring(i+1, j);
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				lAction = CardExchangeAction.cardExchangeAction.valueOf(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				lAmount = Integer.valueOf(actionString.substring(i+1, j));
				
				return new CardExchangeAction(actionMaker, lAction, lAmount);
			}
			
			if(actionType.equals("CardsDealtAction"))
			{
				return new CardsDealtAction();
			}
			
			if(actionType.equals("GetMoneyAction"))
			{
				int lAmount;
				int lPot;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				actionMaker = actionString.substring(i+1, j);
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				lPot = Integer.valueOf(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				lAmount = Integer.valueOf(actionString.substring(i+1, j));
				
				return new GetMoneyAction(actionMaker, lPot, lAmount);
			}
			
			if(actionType.equals("IncrementAnteAction"))
			{
				IncrementAnteAction.incrementAnteAction lAction; 
				int lAmount;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				actionMaker = actionString.substring(i+1, j);
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				lAction = IncrementAnteAction.incrementAnteAction.valueOf(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				lAmount = Integer.valueOf(actionString.substring(i+1, j));
				
				return new IncrementAnteAction(actionMaker, lAction, lAmount);
			}
			
			if(actionType.equals("NewGameAction"))
			{
				
				ArrayList<String> players = new ArrayList<String>();
				ArrayList<Integer> moneys = new ArrayList<Integer>();
				ArrayList<String> bIcons  = new ArrayList<String>();
				ArrayList<String> sIcons  = new ArrayList<String>();
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				players.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				moneys.add(Integer.valueOf(actionString.substring(i+1, j)));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				bIcons.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				sIcons.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				players.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				moneys.add(Integer.valueOf(actionString.substring(i+1, j)));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				bIcons.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				sIcons.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				players.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				moneys.add(Integer.valueOf(actionString.substring(i+1, j)));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				bIcons.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				sIcons.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				players.add(actionString.substring(i+1, j));
			
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				moneys.add(Integer.valueOf(actionString.substring(i+1, j)));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				bIcons.add(actionString.substring(i+1, j));
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				sIcons.add(actionString.substring(i+1, j));
				
				NewGameAction tAction = new NewGameAction("GameFileParser", players, moneys, bIcons, sIcons);
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				tAction.setTournamentRound(Integer.valueOf(actionString.substring(i+1, j)));
				
				i = j;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				tAction.setDifficultySetting(Integer.valueOf(actionString.substring(i+1, j)));
				
				return tAction;
			}
			
			if(actionType.equals("NewMatchAction"))
			{
				int lRound;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				lRound = Integer.valueOf(actionString.substring(i+1, j));
				
				NewMatchAction a = new NewMatchAction();
				
				a.setMatchID(lRound);
				
				return a;
			}
			
			if(actionType.equals("PlayerRemovedAction"))
			{ 
				String lPlayer;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				lPlayer = actionString.substring(i+1, j);
				
				return new PlayerRemovedAction(lPlayer);
			}
			
			if(actionType.equals("ScoreAction"))
			{
				int lAmount;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				actionMaker = actionString.substring(i+1, j);
				
				i = j;
				
				j = actionString.indexOf(Action.END_CHAR, i+1);
				lAmount = Integer.valueOf(actionString.substring(i+1, j));
				
				return new ScoreAction(actionMaker, lAmount);
			}
			
			if(actionType.equals("ScoreRevealAction"))
			{
				ScoreRevealAction.scoreRevealAction lAction; 
				Hand lHand = new Hand();
				ScoreRevealAction lReturn;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				actionMaker = actionString.substring(i+1, j);
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				
				// means not a SHOW
				if(j < 0)
					j = actionString.indexOf(Action.END_CHAR, i+1);
				
				lAction = ScoreRevealAction.scoreRevealAction.valueOf(actionString.substring(i+1, j));
				
				if(lAction == ScoreRevealAction.scoreRevealAction.SHOW)
				{
					i = j;
					
					j = actionString.indexOf(Action.SEP_CHAR, i+1);
					lHand = Hand.toHand(actionString.substring(i+1, j));
					
					i = j;
					j = actionString.indexOf(Action.END_CHAR, i+1);
					
					if(actionString.substring(i+1, j).indexOf("Flush") == -1)
					{
						lHand = Hand.makeHandFlushed(lHand);
					}
				}
				
				lReturn = new ScoreRevealAction(actionMaker, lAction, lHand);
				
				if(lReturn.getAction() == ScoreRevealAction.scoreRevealAction.SHOW)
				{
					lReturn.setHandValue(mHandValuator.valuateHand(lReturn.getHand()));
				}
				
				return lReturn;
			}
			
			if(actionType.equals("UserSetHandAction"))
			{
				Hand lHand;
				UserSetHandAction lReturn;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				actionMaker = actionString.substring(i+1, j);
				
				i = j;
				
				j = actionString.indexOf(Action.SEP_CHAR, i+1);
				lHand = Hand.toHand(actionString.substring(i+1, j));
				
				i = j;
				j = actionString.indexOf(Action.END_CHAR, i+1);
				
				if(actionString.substring(i+1, j).indexOf("Flush") == -1)
				{
					lHand = Hand.makeHandFlushed(lHand);
				}
				
				return new UserSetHandAction(actionMaker, lHand, mHandValuator.valuateHand(lHand));
			}
		}
		catch (Exception e)
		{
			mCorrupted = true;
			e.printStackTrace();
		}
		
		return null;
	}
	
	
	
	
	
}
