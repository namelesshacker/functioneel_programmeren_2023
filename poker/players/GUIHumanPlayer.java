package players;

import game.actions.Action;
import game.actions.BetAction;
import game.actions.ScoreRevealAction;
import game.exceptions.InvalidMoneyValueException;
import images.Constants;
import java.util.ArrayList;

import javax.swing.ImageIcon;

import money.Purse;
import util.Card;
import util.Hand;

public class GUIHumanPlayer implements Player{

	private final String NAME;
	private Purse purse;
	private Hand hand;
	
	private ImageIcon bigIcon;
	private ImageIcon smallIcon;
	private String smallIconName;
	private String bigIconName;
	
	public GUIHumanPlayer(String name, int amount, String bigIcon, String smallIcon)
	{
		NAME = name;
		Constants c = new Constants();
		this.bigIcon = c.getImageIconFromString(bigIcon);
		this.smallIcon = c.getImageIconFromString(smallIcon);
		this.smallIconName = smallIcon;
		this.bigIconName = bigIcon;
		
		hand = new Hand();
		
		purse = new Purse();
		purse.setChips(amount);
	}
	
	/* Money */
	public Purse getPurse()
	{
		return purse;
	}
	public void addToPurse(int amount)
	{
		purse.addChips(amount);
	}
	
	/* Action Making */
	public BetAction makePreBetAction(int minBet)
	{
		return null;
	}
	
	public BetAction makePostBetAction(int minBet)
	{
		return null;
	}
	
	public ScoreRevealAction makeScoreRevealAction()
	{
		return new ScoreRevealAction(NAME, ScoreRevealAction.scoreRevealAction.FOLD, hand);
	}
	
	/* Hand Dealing */
	public void setHand(Hand hand)
	{
		this.hand = hand;
	}
	
	public Hand getHand()
	{
		return hand;
	}
	
	public void notify(Action action)
	{
		return;
	}
	public ArrayList<Card> discardCards(int max)
	{
		return new ArrayList<Card>();
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
	
	/* Immutable identification */
	public String toString()
	{
		return NAME;
	}
}
