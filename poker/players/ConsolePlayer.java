package players;

import game.actions.*;
import util.Hand;
import util.Card;
import java.util.*;

import javax.swing.ImageIcon;

import money.Purse;

public class ConsolePlayer implements Player{
	
	private Hand mHand; 
	private String myName;
	private Purse mPurse; 
	private Scanner myScanner;
	private ImageIcon bigIcon;
	private ImageIcon smallIcon;
	private String smallIconName;
	private String bigIconName;
	
	public ConsolePlayer(String myName, int purse)
	{
		myScanner = new Scanner(System.in);
		this.myName=myName;
		try
		{
			this.mPurse=new Purse(purse);
		}
		catch(Exception e)
		{
			assert false;
		}
		mHand = new Hand();
	}

	public void setHand(Hand yHand)
	{
		System.out.println("");
		System.out.println("Your hand is: " + yHand);
		System.out.println("");
		mHand = yHand; 
	}
	
	/**
	 * Sends out a BetAction for the Pre-Exchange round of betting
	 */
	public BetAction makePreBetAction(int minBet)
	{
		int raiseBy; 
		String reply;
		BetAction.betAction myAction;
		
		System.out.println("");
		System.out.println("Make a bet action. Your minimum bet is: " + minBet);
		System.out.println("Your current hand is: " + mHand);
		System.out.println("Your current funds are:" + mPurse);
		System.out.println("Select one of the following four options: (R) (C) (F) (A) ");
		
		while ( true )
		{
			reply = myScanner.nextLine();
			if(reply.equals("R"))
			{
				myAction = BetAction.betAction.RAISE;
				break;
			}
			
			else if(reply.equals("C"))
			{
				myAction = BetAction.betAction.CALL;
				break;
			}
			
			else if(reply.equals("F"))
			{
				myAction = BetAction.betAction.FOLD;
				break;
			}
			
			else if(reply.equals("A"))
			{
				myAction = BetAction.betAction.ALL_IN;
				break;
			}
			
			else
			{
				System.out.println("You entered an illegible option. Try again: ");
			}
		}
		
		if(reply.equals("R"))
		{
			System.out.println("Enter the amount you wish to raise by: ");
			
			while(true)
			{
				raiseBy = myScanner.nextInt();
				myScanner.nextLine();
				
				if(raiseBy <= 0)
				{
					System.out.println("Cannot raise by a negative number. Try again: ");
				}
				else
				{
					break;
				}
			}
		}
		else if(reply.equals("A"))
		{
			raiseBy=0;
			try
			{
				raiseBy = mPurse.getChips();
			}
			catch(Exception e)
			{}
		}
		else
		{
			raiseBy = 0;
		}
		
		System.out.println("");
		return new BetAction(myName, myAction, raiseBy);
	}
	
	public BetAction makePostBetAction(int minBet)
	{
		return makePreBetAction(minBet);
	}

	public ArrayList<Card> discardCards(int max)
	{
		int discardNum;
		int cardNum; 
		int j;
		Iterator it; 
		ArrayList<Card> toReturn = new ArrayList<Card>(); 
		
		System.out.println("");
		System.out.println("Please discard cards now.");
		System.out.println("Your current hand is " + mHand);
		System.out.println("Your current funds are " + mPurse);
		System.out.println("Please select how many cards you wish to discard (0 - 3) ");
		
		while(true)
		{
			discardNum = myScanner.nextInt();
			myScanner.nextLine();
			
			if(discardNum > 3 || discardNum < 0)
			{
				System.out.println("You entered an illegible option. Try again");
				discardNum = 0;
			}
			else
				break;
		}
		
		for(int i = 0; i < discardNum; i++)
		{
			System.out.println("Please enter the number of the " + (i+1) + "th card.");
			System.out.println("The cards are numbered as (1 , 2 , 3 , 4 , 5) ");
			System.out.println("Your hand is " + mHand);
			
			while(true)
			{
				cardNum = myScanner.nextInt();
				myScanner.nextLine();
				if(cardNum < 1 || cardNum > mHand.size())
				{
					System.out.println("You entered an invalid card number. Try again: ");
				}
				else
					break;
			}
			
			j = 1; 			
			it = mHand.iterator();
			while(it.hasNext())
			{
				if(j == cardNum)
					toReturn.add((Card)it.next());
				else
					it.next();
				j++;
			}
			System.out.println("So far you are discarding: "+toReturn);
		}
		
		System.out.println("");
		return toReturn; 
	}
	
	
	/**
	 * Returns a ScoreRevealAction (show the hand or fold) 
	 */
	public ScoreRevealAction makeScoreRevealAction()
	{
		String reply; 
		ScoreRevealAction.scoreRevealAction myAction;
		
		System.out.println("");
		System.out.println("Time to compete your hand. Your hand is: " + mHand);
		System.out.println("Enter (F) or (S) ");
		
		while(true)
		{
			reply = myScanner.nextLine();
			
			if(reply.equals("F"))
			{
				myAction = ScoreRevealAction.scoreRevealAction.FOLD;
				System.out.println("");
				return new ScoreRevealAction(toString(), myAction, null);
			}	
			else if(reply.equals("S"))
			{
				myAction = ScoreRevealAction.scoreRevealAction.SHOW;
				System.out.println("");
				return new ScoreRevealAction(toString(), myAction, mHand);
			}
			else
				System.out.println("You entered an invalid option. Try again: ");
		}
	}
	
	/**
	 * This method takes in an action and adds it to its GameStats accordingly.
	 * It is used by any class implementing GameSubject to notify this object of a change.
	 */
	public void notify(Action action)
	{
		
	}
	
	public Purse getPurse()
	{
		return mPurse;
	}
	
	public void addToPurse(int m)
	{
		mPurse.addChips(m);
	}
	
	public Hand getHand()
	{
		return mHand;
	}
	
	public final void setSmallIcon(String s)
	{
		smallIcon = new ImageIcon(s);
		smallIconName = s;
	}
	
	public final void setBigIcon(String s)
	{
		bigIcon = new ImageIcon(s);
		bigIconName = s;
	}
	
	public final ImageIcon getSmallIcon()
	{
		return smallIcon;
	}
	
	public final ImageIcon getBigIcon()
	{
		return bigIcon;
	}
	
	public final String getBigIconName()
	{
		return bigIconName;
	}
	
	public final String getSmallIconName()
	{
		return smallIconName;
	}
	
	public String toString()
	{
		return myName;
	}
}


	
	