package gui.userpanels;

import images.Constants;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import game.actions.*;
import game.GameListener;
import game.GameSubject;

public class GUIDiscardPanel extends JPanel implements ActionListener, GameSubject, KeyListener{

	private final JButton OKButton;
	private GameListener lListener;
	private final CardExchangeAction.cardExchangeAction lAction;
	private final CardExchangeAction mAction;
	
	public GUIDiscardPanel()
	{
		super();
		this.setPreferredSize(Constants.BET_PANEL_SIZE);
		this.setBackground(Constants.TRANSPARENT);
		
		OKButton = new JButton("Discard");
		OKButton.addActionListener(this);
		OKButton.addKeyListener(this);
		OKButton.setBackground(Constants.TRANSPARENT);
		OKButton.setFont(Constants.FONT);
		OKButton.setPreferredSize(Constants.BUTTON_SIZE);
		lAction = CardExchangeAction.cardExchangeAction.NO_DISCARD;
		mAction = new CardExchangeAction("GUIDiscardPanel", lAction, 0);
		this.add(OKButton);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		if(lListener != null)
		{
			lListener.notify(mAction);
		}	
	}
	
	public void keyPressed(KeyEvent e)
	{
		
	}
	
	public void attachGameListener(GameListener l)
	{
		lListener = l;
	}
	
	public void detachGameListener(GameListener l)
	{
		lListener = null;
	}
	
	public void notifyObservers(Action action)
	{
		// nothing
	}
	
	public void keyTyped(KeyEvent e)
	{
		if(e.getKeyCode() == KeyEvent.VK_ENTER)
		{
			if(lListener != null)
			{
				lListener.notify(mAction);
			}
		}
	}
	
	public void keyReleased(KeyEvent e)
	{
		// nothing
	}
}
