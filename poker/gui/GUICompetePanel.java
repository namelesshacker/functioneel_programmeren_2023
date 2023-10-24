package gui;

import game.GameListener;
import game.GameSubject;
import game.actions.Action;
import game.actions.ScoreRevealAction;

import images.Constants;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class GUICompetePanel extends JPanel implements ActionListener, GameSubject, KeyListener {
	
	private final JButton SHOWButton;
	private final JButton FOLDButton;
	private final ScoreRevealAction ShowAction;
	private final ScoreRevealAction FoldAction;
	
	private GameListener lListener;
	
	public GUICompetePanel()
	{
		super();
		this.setPreferredSize(Constants.BET_PANEL_SIZE);
		this.setBackground(Constants.TRANSPARENT);
		
		SHOWButton = new JButton("Show");
		SHOWButton.setBackground(Constants.TRANSPARENT);
		SHOWButton.setFont(Constants.FONT);
		SHOWButton.setPreferredSize(Constants.BUTTON_SIZE);
		FOLDButton = new JButton("Fold");
		FOLDButton.setBackground(Constants.TRANSPARENT);
		FOLDButton.setFont(Constants.FONT);
		FOLDButton.setPreferredSize(Constants.BUTTON_SIZE);
		
		ShowAction = 
		new ScoreRevealAction
		(
			this.getClass().getName(), 
			ScoreRevealAction.scoreRevealAction.SHOW,
			null
		);
		
		FoldAction = 
		new ScoreRevealAction
		(
			this.getClass().getName(),
			ScoreRevealAction.scoreRevealAction.FOLD,
			null
		);
		
		SHOWButton.addActionListener(this);
		FOLDButton.addActionListener(this);
		SHOWButton.addKeyListener(this);
		FOLDButton.addKeyListener(this);
		
		this.add(SHOWButton);
		this.add(FOLDButton);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		if((JButton)e.getSource() == SHOWButton)
		{
			if(lListener != null)
				lListener.notify(ShowAction);
		}
		if((JButton)e.getSource() == FOLDButton)
		{
			if(lListener != null)
				lListener.notify(FoldAction);
		}
		
	}
	
	public void keyPressed(KeyEvent e)
	{
		if(e.getKeyChar() == 's')
		{
			if(lListener != null)
				lListener.notify(ShowAction);
		}
		if(e.getKeyChar() == 'f')
		{
			if(lListener != null)
				lListener.notify(FoldAction);
		}	
	}
	
	public void attachGameListener(GameListener e)
	{
		lListener = e;
	}

	public void detachGameListener(GameListener e)
	{
		lListener = null;
	}
	
	public void notifyObservers(Action e)
	{
		// nothing
	}
	
	public void keyTyped(KeyEvent e)
	{
		
	}
	
	public void keyReleased(KeyEvent e)
	{
		
	}
}
