package gui.userpanels;

import game.GameListener;
import game.GameSubject;
import game.actions.*;
import gui.GUIBetException;

import images.Constants;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.*;

public class GUIBetPanel extends JPanel implements ActionListener, KeyListener, GameSubject{

	private final JButton FOLD, RAISE, CALL, ALL_IN, OK;
	private final JTextField TEXT;
	private final BetAction CALL_ACTION, FOLD_ACTION;
	private final JLabel MESSAGE;
	private int minBet;
	private int maxBet;
	private GameListener g;
	
	public GUIBetPanel()
	{
		super(new GridLayout(0,1));
		
		this.setBackground(Constants.TRANSPARENT);
		this.setPreferredSize(Constants.BET_PANEL_SIZE);
		
		minBet=0;
		maxBet=0;
		
		JPanel panel1 = new JPanel(new FlowLayout());
		JPanel panel2 = new JPanel(new FlowLayout());
		
		CALL = new JButton("Call");
		CALL.setPreferredSize(Constants.BUTTON_SIZE);
		CALL.addActionListener(this);
		CALL.addKeyListener(this);
		CALL.setBackground(Constants.TRANSPARENT);
		CALL.setFont(Constants.FONT);
		panel1.add(CALL);
		
		RAISE = new JButton("Raise");
		RAISE.setPreferredSize(Constants.BUTTON_SIZE);
		RAISE.addActionListener(this);
		RAISE.addKeyListener(this);
		RAISE.setBackground(Constants.TRANSPARENT);
		RAISE.setFont(Constants.FONT);
		panel1.add(RAISE);
		
		ALL_IN = new JButton("All In");
		ALL_IN.setPreferredSize(Constants.BUTTON_SIZE);
		ALL_IN.addActionListener(this);
		ALL_IN.setBackground(Constants.TRANSPARENT);
		ALL_IN.setFont(Constants.FONT);
		panel1.add(ALL_IN);
		
		FOLD = new JButton("Fold");
		FOLD.setPreferredSize(Constants.BUTTON_SIZE);
		FOLD.addActionListener(this);
		FOLD.setBackground(Constants.TRANSPARENT);
		FOLD.setFont(Constants.FONT);
		panel1.add(FOLD);
		
		TEXT = new JTextField(5);
		TEXT.setVisible(false);
		TEXT.setFont(Constants.FONT);
		TEXT.addKeyListener(this);
		panel2.add(TEXT);
	
		OK = new JButton("Bet!");
		OK.setPreferredSize(Constants.BUTTON_SIZE);
		OK.addActionListener(this);
		OK.setBackground(Constants.TRANSPARENT);
		OK.setVisible(false);
		OK.setFont(Constants.FONT);
		panel2.add(OK);
		OK.addKeyListener(this);
		
		MESSAGE = new JLabel("Place a bet.   ");
		MESSAGE.addKeyListener(this);
		MESSAGE.setFont(Constants.FONT);
		panel2.add(MESSAGE);
		
		panel1.setBackground(Constants.TRANSPARENT);
		panel2.setBackground(Constants.TRANSPARENT);
		
		this.add(panel1);
		this.add(panel2);
		
		CALL_ACTION = new BetAction("GUIBetPanel", BetAction.betAction.CALL, 0);

		FOLD_ACTION = new BetAction("GUIBetPanel", BetAction.betAction.FOLD, 0);
	}
	
	public final void actionPerformed(ActionEvent e)
	{
		JButton temp = (JButton) e.getSource();
		
		if( temp.equals(RAISE))
		{
			TEXT.setVisible(true);
			TEXT.grabFocus();
			OK.setVisible(true);
		}
		else
		{
			if(temp.equals(OK))
			{
				String input = TEXT.getText();
	
				try{
					int result = new Integer(input).intValue();
					 
					if(result <= 0)
						 throw new NumberFormatException();
					 else if(result > maxBet)
						 throw new GUIBetException();
					 
					notifyObservers(new BetAction("GUIBetPanel",
									BetAction.betAction.RAISE, result));
					MESSAGE.setText("               ");
					TEXT.setVisible(false);
					OK.setVisible(false);
				}
				catch(NumberFormatException ex)
				{
					MESSAGE.setText("Invalid Number.");
					TEXT.setText("");
				}
				catch(GUIBetException ex)
				{
					 MESSAGE.setText("FOOL!! ("+maxBet+")");
					 TEXT.setText("");
				}
			}
			else
			{
				if(temp.equals(CALL))
					notifyObservers(CALL_ACTION);
				else if(temp.equals(FOLD))
					notifyObservers(FOLD_ACTION);
				else if(temp.equals(ALL_IN))
					notifyObservers(new BetAction("GUIBetPenel",BetAction.betAction.ALL_IN, maxBet + minBet));
				
				TEXT.setText("");
				MESSAGE.setText("               ");
				TEXT.setVisible(false);
				OK.setVisible(false);
			}
		}
	}
	
	public final void keyPressed(KeyEvent e)
	{
		if(TEXT.isVisible())
		{
			if(e.getKeyCode() == KeyEvent.VK_ENTER)
			{
				 String input = TEXT.getText();
				
				 try
				 {
					 int result = new Integer(input).intValue();
					 
					 if(result <= 0)
						 throw new NumberFormatException();
					 else if(result > maxBet)
						 throw new GUIBetException();
					 
					 notifyObservers(new BetAction("GUIBetPanel",
								BetAction.betAction.RAISE, result));
					 MESSAGE.setText("               ");
					 TEXT.setVisible(false);
					 OK.setVisible(false);
				 }
				 catch(NumberFormatException ex)
				 {
					 MESSAGE.setText("Invalid Number.");
					 TEXT.setText("");
				 }
				 catch(GUIBetException ex)
				 {
					 MESSAGE.setText("FOOL!! ("+maxBet+")");
					 TEXT.setText("");
				 }
			 }
		}
		else
		{
			if(e.getKeyChar() == 'r')
			{
				 TEXT.setVisible(true);
				 TEXT.grabFocus();
				 OK.setVisible(true);
			}
			else
			{
				if(e.getKeyChar() == 'a')
					notifyObservers(new BetAction("GUIBetPenel",BetAction.betAction.ALL_IN, maxBet + minBet));
		
				else if(e.getKeyChar() == 'c')
					notifyObservers(CALL_ACTION);
				
				else if(e.getKeyChar() == 'f')
					notifyObservers(FOLD_ACTION);
				
				TEXT.setText("");
				TEXT.setVisible(false);
				MESSAGE.setText("               ");
				OK.setVisible(false);
			}
		}
	}
	
	public final void keyTyped(KeyEvent e)
	{}
	
	public final void keyReleased(KeyEvent e)
	{}
	
	public final void notifyObservers(game.actions.Action action)
	{
		if(g !=null)
		{
			g.notify(action);
		}
	}
	
	public final void attachGameListener(GameListener g)
	{
		this.g = g;
	}
	
	public void detachGameListener(GameListener g)
	{
		this.g =null;
	}
	
	public final void setMinBet(int minBet)
	{
		this.minBet = minBet;
		CALL.setText("Call " + minBet);
		MESSAGE.setText("Place a bet.    ");
	}
	
	public final void setMaxBet(int maxBet)
	{
		this.maxBet = maxBet;
	}
		
	public final String toString()
	{
		return "minBet "+minBet+" maxBet "+maxBet;
	}
}
