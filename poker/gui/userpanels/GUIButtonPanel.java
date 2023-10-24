package gui.userpanels;

import images.Constants;
import game.GameListener;
import game.actions.*;
import game.exceptions.HandFoldedException;
import gui.GUICompetePanel;
import javax.swing.JPanel;

/**
 * @author gen kazama and david kawrykow
 *
 * This class houses four distinct panels which can be dynamically loaded at runtime.  
 * These are: a betting panel (providing all betting functionality)
 * 			: a discard panel (providing the "ok" button for the discard phase)
 * 			: a confirmation panel (providing a generic "ok" button plus message)
 * 			: an empty panel (to hide the button panel)
 * 
 * After the panel of choice is loaded. The client class must reset the communication
 * (Comm) action and then poll the class until the Comm action is set by a button click. 
 * This prevents external code from executing until the user has made his decision. 
 */

public class GUIButtonPanel extends JPanel implements GameListener{
	
	private GUIBetPanel lBetPanel;
	private GUIDiscardPanel lDiscardPanel;
	private GUICompetePanel lCompetePanel;
	private GUIConfirmationPanel lConfirmationPanel; 
	private JPanel lPanel;
	private Action commAction;
	
	public GUIButtonPanel()
	{
		super();
		this.setPreferredSize(Constants.BET_PANEL_SIZE);
		
		this.setBackground(Constants.TRANSPARENT);
		
		lBetPanel 		   = new GUIBetPanel();
		lDiscardPanel      = new GUIDiscardPanel();
		lCompetePanel 	   = new GUICompetePanel();
		lConfirmationPanel = new GUIConfirmationPanel("", "");
		lPanel 			   = new JPanel();
		
		lPanel.setPreferredSize(Constants.BET_PANEL_SIZE);
		
		lBetPanel.setBackground(Constants.TRANSPARENT);
		lDiscardPanel.setBackground(Constants.TRANSPARENT);
		lCompetePanel.setBackground(Constants.TRANSPARENT);
		lConfirmationPanel.setBackground(Constants.TRANSPARENT);
		lPanel.setBackground(Constants.TRANSPARENT);
		
		lBetPanel.attachGameListener(this);
		lDiscardPanel.attachGameListener(this);
		lCompetePanel.attachGameListener(this);
		lConfirmationPanel.attachGameListener(this);
		
		this.add(lPanel);
		this.add(lBetPanel);
		this.add(lDiscardPanel);
		this.add(lCompetePanel);
		this.add(lConfirmationPanel);
		
		hideAll();		
	}
	
	public void notify(Action action)
	{
	//	Action result = new NewMatchAction(getActionMaker());
		if(action instanceof BetAction)
		{
			BetAction lAction = (BetAction) action;
			lBetPanel.setVisible(false);
			commAction = new BetAction(getActionMaker(), lAction.getAction(), lAction.getAmount());
		}
		else if(action instanceof CardExchangeAction)
		{
			CardExchangeAction lAction = (CardExchangeAction) action;
			lDiscardPanel.setVisible(false);
			commAction = new CardExchangeAction(getActionMaker(), lAction.getAction(), lAction.getAmount());
		}
		else if(action instanceof ScoreRevealAction)
		{
			ScoreRevealAction lAction = (ScoreRevealAction) action;
			lCompetePanel.setVisible(false);
			
			try
			{
				commAction = new ScoreRevealAction(getActionMaker(), lAction.getAction(), lAction.getHand());
			}
			catch(HandFoldedException e)
			{	
				commAction = new ScoreRevealAction(getActionMaker(), lAction.getAction(), null);
			}
		}
		else if(action instanceof NewMatchAction)
		{
			commAction = new NewMatchAction("GUIButtonPanel"); 
		}
		
		
	/*	if(lListener != null)
			lListener.notify(result);*/
	}
	
	private void hideAll()
	{
		lPanel.setVisible(false);
		lBetPanel.setVisible(false);
		lDiscardPanel.setVisible(false);
		lCompetePanel.setVisible(false);
		lConfirmationPanel.setVisible(false);
	}
	
	public void loadEmptyPanel()
	{
		hideAll();
		lPanel.setVisible(true);
		this.updateUI();
	}
	
	public void loadBetPanel(int min, int max)
	{
		hideAll();
		lBetPanel.setVisible(true);
		lBetPanel.setMinBet(min);
		lBetPanel.setMaxBet(max);
		this.updateUI();
	}
	
	public void loadDiscardPanel()
	{
		hideAll();
		lDiscardPanel.setVisible(true);
		this.updateUI();
		
	}
	
	public void loadCompetePanel()
	{
		hideAll();
		lCompetePanel.setVisible(true);
		this.updateUI(); 
	}
	
	public void loadConfirmationPanel(String Question, String Answer, boolean wOptionToQuit)
	{
		hideAll();
		lConfirmationPanel.setQuestion(Question);
		lConfirmationPanel.setAnswer(Answer);
		lConfirmationPanel.setOptionToQuit(wOptionToQuit);
		lConfirmationPanel.setVisible(true);
		this.updateUI(); 
	}
	
	public void notifyObservers(Action action)
	{
		// nothing
	}
	
	public final Action getCommAction()
	{
		return commAction;
	}
	
	public final boolean wasQuitSelected()
	{
		return lConfirmationPanel.wasQuitSelected();
	}
	
	public final void resetCommAction()
	{
		commAction = null;
	}
	
	public final String getActionMaker()
	{
		return "";
	}
}
