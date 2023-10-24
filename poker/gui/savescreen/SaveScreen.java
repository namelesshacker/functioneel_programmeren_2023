package gui.savescreen;

import gui.GUIGameScreen;

import images.Constants;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import ai.AIPlayer;

import players.DavidHasselhoff;
import players.ErnestoGuevara;
import players.Giuliani;
import players.IvyLeaguePrepster;
import players.KarlMarx;
import players.LeChiffre;
import players.MikeTyson;
import players.Player;
import players.Robespierre;
import players.Trotsky;

public class SaveScreen extends JPanel implements ActionListener, KeyListener{

	// Save buttons
	private JRadioButton laButton;
	private JRadioButton lbButton;
	private JRadioButton lcButton;
	private JRadioButton lnButton;
	private JRadioButton ltButton;
	
	
	// The Button panel
	private JPanel ButtonPanel;
	
	// the button group
	private ButtonGroup  group;
	
	// the message
	private final String PROMPT = "Select slot in which to save progress";
	
	// the JLabel with prompt
	private JLabel PromptLabel;
	
	// The button to exit the screen
	private JButton ok;
	
	// the "finito" marker
	private boolean done;
	
	// the file to which we want to save our progress
	private int file;
	
	public SaveScreen(boolean tournament)
	{
		super(new BorderLayout());
		this.setPreferredSize(Constants.GAME_SIZE);
		this.setBackground(Constants.TRANSPARENT);
		
		// Initialize variables
		PromptLabel = new JLabel(PROMPT);
		PromptLabel.setFont(Constants.FONT);
		PromptLabel.setAlignmentX(JLabel.CENTER_ALIGNMENT);
		PromptLabel.setAlignmentY(JLabel.CENTER_ALIGNMENT);
		PromptLabel.setBackground(Constants.TRANSPARENT);
		
		laButton = new JRadioButton("Arcade Match 1");
	    laButton.setFont(Constants.FONT);
	    laButton.setBackground(Constants.TRANSPARENT);
	    laButton.setActionCommand("a");
	    laButton.setSelected(false);
	    laButton.setVisible(! tournament);
	    
	    lbButton = new JRadioButton("Arcade Match 2");
	    lbButton.setFont(Constants.FONT);
	    lbButton.setBackground(Constants.TRANSPARENT);
	    lbButton.setActionCommand("b");
	    lbButton.setSelected(false);
	    lbButton.setVisible(! tournament);

	    lcButton = new JRadioButton("Arcade Match 3");
	    lcButton.setFont(Constants.FONT);
	    lcButton.setBackground(Constants.TRANSPARENT);
	    lcButton.setActionCommand("c");
	    lcButton.setSelected(false);
	    lcButton.setVisible(! tournament);
	    
	    ltButton = new JRadioButton("Tournament Match");
	    ltButton.setFont(Constants.FONT);
	    ltButton.setBackground(Constants.TRANSPARENT);
	    ltButton.setActionCommand("t");
	    ltButton.setSelected(false);
	    ltButton.setVisible(tournament);
	    
	    lnButton = new JRadioButton("Don't bother");
	    lnButton.setFont(Constants.FONT);
	    lnButton.setBackground(Constants.TRANSPARENT);
	    lnButton.setActionCommand("n");
	    lnButton.setSelected(true);
	   
	    
	    //Group the radio buttons.
	    group = new ButtonGroup();
	    group.add(laButton);
	    group.add(lbButton);
	    group.add(lcButton);
	    group.add(ltButton);
	    group.add(lnButton);

	    laButton.addActionListener(this);
	    lbButton.addActionListener(this);
	    lcButton.addActionListener(this);
	    ltButton.addActionListener(this);
	    lnButton.addActionListener(this);
		
	    ok = new JButton("OK");
		ok.setBackground(Constants.TRANSPARENT);
		ok.setFont(Constants.FONT);
		ok.addActionListener(this);
		ok.setActionCommand("OK");
		ok.grabFocus();
		ok.addKeyListener(this);
	    
		done = false;
		file = GUIGameScreen.NO_SAVE;
		
		//Center panel
		PromptLabel.setHorizontalAlignment(PromptLabel.CENTER);
		laButton.setHorizontalAlignment(laButton.CENTER);
		lbButton.setHorizontalAlignment(lbButton.CENTER);
		lcButton.setHorizontalAlignment(lcButton.CENTER);
		ltButton.setHorizontalAlignment(ltButton.CENTER);
		lnButton.setHorizontalAlignment(lnButton.CENTER);
		
	    JPanel ButtonPanel = new JPanel(new GridLayout(0,1));
	    ButtonPanel.setBorder(Constants.BORDER);
	    ButtonPanel.add(PromptLabel);
	    if(! tournament)
	    {
		    ButtonPanel.add(laButton);
		    ButtonPanel.add(lbButton);
		    ButtonPanel.add(lcButton);
	    }
	    if(tournament)
	    	ButtonPanel.add(ltButton);
	    ButtonPanel.add(lnButton);
	    ButtonPanel.add(ok);
	    ButtonPanel.setBackground(Constants.TRANSPARENT);
		
		//south
		JPanel south = new JPanel();
		south.setBackground(Constants.TRANSPARENT);
		south.setPreferredSize(new Dimension(100, 300));

		JPanel north = new JPanel();
		north.setBackground(Constants.TRANSPARENT);
		north.setPreferredSize(new Dimension(100, 300));
		
		JPanel east = new JPanel();
		east.setBackground(Constants.TRANSPARENT);
		east.setPreferredSize(new Dimension(400, 100));
		
		JPanel west = new JPanel();
		west.setBackground(Constants.TRANSPARENT);
		west.setPreferredSize(new Dimension(400, 100));
		
		this.add(north, BorderLayout.NORTH);
		this.add(east, BorderLayout.EAST);
		this.add(ButtonPanel, BorderLayout.CENTER);
		this.add(west, BorderLayout.WEST);
		this.add(south, BorderLayout.SOUTH);
	}
	
	public final void actionPerformed(ActionEvent e)
	{
	
		if(e.getActionCommand().equals(ok.getActionCommand()))
		{
			done = true;
		}
		if(e.getActionCommand().equals(laButton.getActionCommand()))
		{
			file = GUIGameScreen.SLOT_ONE;
		}
		if(e.getActionCommand().equals(lbButton.getActionCommand()))
		{
			file = GUIGameScreen.SLOT_TWO;
		}
		if(e.getActionCommand().equals(lcButton.getActionCommand()))
		{
			file = GUIGameScreen.SLOT_THREE;
		}
		if(e.getActionCommand().equals(ltButton.getActionCommand()))
		{
			file = GUIGameScreen.SLOT_FOUR;
		}
		if(e.getActionCommand().equals(lnButton.getActionCommand()))
		{
			file = GUIGameScreen.NO_SAVE;
		}
	}
	
	public final void keyPressed(KeyEvent e)
	{
		if(e.getKeyCode() == KeyEvent.VK_ENTER)
		{
			done = true;
		}
	}
	
	public final void keyTyped(KeyEvent e)
	{	
	}
	
	public final void keyReleased(KeyEvent e)
	{	
	}

	public final boolean isDone()
	{
		return done;
	}
	
	public final int getSaveType()
	{
		return GUIGameScreen.NO_SAVE;
	}
}
