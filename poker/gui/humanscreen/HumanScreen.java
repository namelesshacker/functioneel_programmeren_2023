package gui.humanscreen;


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
import players.Robespierre;
import players.Trotsky;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * Represents the code for the JPanel when the user is choosing their character.  The character's are
 * selected by custom radio buttons (CharRadioButton.java).
 *
 */

public class HumanScreen extends JPanel implements ActionListener, KeyListener{
	private final String H = "Hasselhoff";	
	private final String G = "Giuliani";
	private final String I = "Ivy";
	private final String E = "Guevara";
	private final String L = "Lechiffre";
	private final String K = "Marx";
	private final String M = "Tyson";
	private final String R = "Robespierre";
	private final String T = "Trotsky";

	private final int EASY = 300;
	private final int MED  = 200;
	private final int HARD = 100;
	
	
	private difficulty d;
	private gameType g;
	
	public enum difficulty
	{Easy, Medium, Difficult}
	
	public enum gameType
	{Arcade, Tournament}
	
	private ArrayList<PlayerPanel> players;
	private ArrayList<String> actionCmds;
	private ArrayList<AIPlayer> ps;
	private ArrayList<CharRadioButton> radioButtons;
	private BigIconPanel bigIcon;
	
	private int[] difficultyChips;
	
	private AIPlayer p;
	
	private JButton ok;
	
	private boolean done;
	
	public HumanScreen(String name)
	{
		super(new BorderLayout());
		this.setPreferredSize(Constants.GAME_SIZE);
		this.setBackground(Constants.TRANSPARENT);
		
		// Initialize variables
		players = new ArrayList<PlayerPanel>();
		actionCmds = new ArrayList<String>();
		ps = new ArrayList<AIPlayer>();
		radioButtons = new ArrayList<CharRadioButton>();
		
		bigIcon = new BigIconPanel(H+".gif");
		
		difficultyChips = new int[3];
		difficultyChips[0]=EASY;
		difficultyChips[1]=MED;
		difficultyChips[2]=HARD;
		
		done = false;
	
		// Create radio buttons for Players
		
		int counter = 0;
		
		JPanel chars = new JPanel(new GridLayout(3,3));
		chars.setBackground(Constants.TRANSPARENT);
		ButtonGroup b = new ButtonGroup();
	
		// Hasselhoff
		p = new DavidHasselhoff("", 200, 1);
		players.add(new PlayerPanel(H, "HasselhoffT.gif"));
		actionCmds.add(H);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(actionCmds.get(counter));
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setSelected(true);
		radioButtons.get(counter).setFont(Constants.FONT);
	
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new DavidHasselhoff("", 200, 1));
		bigIcon.setFilename(H+".gif");
		counter ++;
		
		// LeChiffre
		players.add(new PlayerPanel(L, "LechiffreT.gif"));
		actionCmds.add(L);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(L);
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new LeChiffre("", 200, 1));
		counter++;
		
		// Robespierre
		players.add(new PlayerPanel(R, "RobespierreT.gif"));
		actionCmds.add(R);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(R);
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new Robespierre("", 200, 1));
		counter++;
		
		// Karl Marx
		players.add(new PlayerPanel(K, "MarxT.gif"));
		actionCmds.add(K);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(K);
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new KarlMarx("", 200, 1));
		counter++;
		
		
		// Trotsky
		players.add(new PlayerPanel(T, "TrotskyT.gif"));
		actionCmds.add(T);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).setActionCommand(actionCmds.get(counter));
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new Trotsky("", 100, 1));
		counter++;
		
		// Giuliani
		players.add(new PlayerPanel(G, "GiulianiT.gif"));
		actionCmds.add(G);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(actionCmds.get(counter));
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));	
		ps.add(new Giuliani("", 100, 1));
		counter ++;

		// Tyson
		players.add(new PlayerPanel(M, "TysonT.gif"));
		actionCmds.add(M);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(actionCmds.get(counter));
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new MikeTyson("", 100, 1));
		counter ++;
	
		// Ivy League Prepster
		players.add(new PlayerPanel(I, "IvyT.gif"));
		actionCmds.add(I);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(actionCmds.get(counter));
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new IvyLeaguePrepster("", 100, 1));
		counter ++;
		
		// Ernesto Guevara
		players.add(new PlayerPanel(E, "GuevaraT.gif"));
		actionCmds.add(E);
		
		radioButtons.add(new CharRadioButton(players.get(counter).getName(), players.get(counter).getImg()));
		radioButtons.get(counter).setActionCommand(actionCmds.get(counter));
		radioButtons.get(counter).setBackground(Constants.TRANSPARENT);
		radioButtons.get(counter).addActionListener(this);
		radioButtons.get(counter).setFont(Constants.FONT);
		
		chars.add(radioButtons.get(counter));
		b.add(radioButtons.get(counter));
		ps.add(new ErnestoGuevara("", 100, 1));
		counter ++;

		JPanel charSelect = new JPanel(new BorderLayout());
		charSelect.setBackground(Constants.TRANSPARENT);
		
		charSelect.add(new JLabel("Select your character: "), BorderLayout.NORTH);
		charSelect.add(chars, BorderLayout.CENTER);
		
		
		// Game type
		g = gameType.Arcade;
		
		ButtonGroup bg = new ButtonGroup();
		
		JPanel gameTypeSelect = new JPanel(new GridLayout(0,1));
		gameTypeSelect.setBackground(Constants.TRANSPARENT);
		
		JRadioButton arcade = new JRadioButton(gameType.Arcade.toString()+"    ");
		arcade.setFont(Constants.FONT);
		arcade.setActionCommand(gameType.Arcade.toString());
		arcade.setBackground(Constants.TRANSPARENT);
		arcade.addActionListener(this);
		bg.add(arcade);
		
		JRadioButton tourney = new JRadioButton(gameType.Tournament.toString());
		tourney.setActionCommand(gameType.Tournament.toString());
		tourney.setFont(Constants.FONT);
		tourney.setBackground(Constants.TRANSPARENT);
		tourney.addActionListener(this);
		bg.add(tourney);
		
		arcade.setSelected(true);
		
		JLabel gameTypeLabel = new JLabel("Select Game Type:");
		gameTypeLabel.setHorizontalAlignment(JLabel.CENTER);
		arcade.setHorizontalAlignment(arcade.CENTER);
		tourney.setHorizontalAlignment(tourney.CENTER);
		
		gameTypeSelect.add(gameTypeLabel);
		gameTypeSelect.add(arcade);
		gameTypeSelect.add(tourney);
		
		//Difficulty
		d = difficulty.Medium;
		
		ButtonGroup bd = new ButtonGroup();
		JPanel diffSelect = new JPanel(new GridLayout(0,1));
		diffSelect.setBackground(Constants.TRANSPARENT);
		
		JRadioButton easy = new JRadioButton(difficulty.Easy.toString()+"     ");
		easy.setActionCommand(difficulty.Easy.toString());
		easy.setFont(Constants.FONT);
		easy.setBackground(Constants.TRANSPARENT);
		easy.addActionListener(this);
		bd.add(easy);
		
		JRadioButton med = new JRadioButton(difficulty.Medium.toString()+"   ");
		med.setActionCommand(difficulty.Medium.toString());
		med.setFont(Constants.FONT);
		med.setBackground(Constants.TRANSPARENT);
		med.addActionListener(this);
		bd.add(med);
		
		JRadioButton difficult = new JRadioButton(difficulty.Difficult.toString());
		difficult.setActionCommand(difficulty.Difficult.toString());
		difficult.setFont(Constants.FONT);
		difficult.setBackground(Constants.TRANSPARENT);
		difficult.addActionListener(this);
		bd.add(difficult);
		
		med.setSelected(true);
		
		JLabel diffLabel = new JLabel("Select Difficulty:");
		diffLabel.setHorizontalAlignment(JLabel.CENTER);
		easy.setHorizontalAlignment(easy.CENTER);
		med.setHorizontalAlignment(med.CENTER);
		difficult.setHorizontalAlignment(difficult.CENTER);
		
		diffSelect.add(diffLabel);
		diffSelect.add(easy);
		diffSelect.add(med);
		diffSelect.add(difficult);
		
		ok = new JButton("OK");
		ok.setBackground(Constants.TRANSPARENT);
		ok.setFont(Constants.FONT);
		ok.addActionListener(this);
		ok.setActionCommand("OK");
		ok.grabFocus();
		ok.addKeyListener(this);
		
		//set up panel layouts
		
		diffSelect.setBorder(Constants.BORDER);
		gameTypeSelect.setBorder(Constants.BORDER);
		charSelect.setBorder(Constants.BORDER);
		
		JPanel big = new JPanel(new BorderLayout());
		
		JLabel playerName = new JLabel("Player Name: "+name);
		playerName.setFont(Constants.FONT);
		playerName.setPreferredSize(new Dimension(370, 50));
		playerName.setHorizontalAlignment(JLabel.CENTER);
		//playerName.setVerticalAlignment(JLabel.CENTER);
		big.setBackground(Constants.TRANSPARENT);
		big.add(bigIcon, BorderLayout.CENTER);
		big.add(playerName, BorderLayout.SOUTH);
		
		//charSelect and BigIcon
		JPanel charsBig = new JPanel(new GridLayout(0,1));
		charsBig.setBackground(Constants.TRANSPARENT);
		charsBig.add(charSelect);
		charsBig.add(big);
		
		//Center panel
		JPanel c = new JPanel(new BorderLayout());
		c.setBackground(Constants.TRANSPARENT);
		c.add(diffSelect, BorderLayout.SOUTH);
		c.add(charsBig, BorderLayout.CENTER);
		c.add(gameTypeSelect, BorderLayout.NORTH);
		
		
		JPanel center = new JPanel(new FlowLayout());
		center.setBackground(Constants.TRANSPARENT);
		
		JPanel spacer1 = new JPanel();
		spacer1.setBackground(Constants.TRANSPARENT);
		spacer1.setPreferredSize(new Dimension(100, this.getSize().height));
		
		JPanel spacer2 = new JPanel();
		spacer2.setBackground(Constants.TRANSPARENT);
		spacer2.setPreferredSize(new Dimension(100, this.getSize().height));
		
		center.add(spacer1);
		center.add(c);
		center.add(spacer2);
		
		
		//south
		JPanel south = new JPanel(new FlowLayout());
		south.setBackground(Constants.TRANSPARENT);
		
		JPanel spacer3 = new JPanel();
		spacer3.setBackground(Constants.TRANSPARENT);
		
		JPanel spacer4 = new JPanel();
		spacer4.setBackground(Constants.TRANSPARENT);
		
		south.add(spacer3);
		south.add(ok);
		south.add(spacer4);
		
		//Add everything

		this.add(center, BorderLayout.CENTER);
		this.add(south, BorderLayout.SOUTH);
	}
	
	public final void actionPerformed(ActionEvent e)
	{
		if(e.getActionCommand().equals(ok.getActionCommand()))
		{
			done = true;
		}
		if(e.getActionCommand().equals(gameType.Arcade.toString()))
		{
			g = gameType.Arcade;
		}
		else if(e.getActionCommand().equals(gameType.Tournament.toString()))
		{
			g = gameType.Tournament;
		}
		else if(e.getActionCommand().equals(difficulty.Easy.toString()))
		{
			d = difficulty.Easy;
			p.getPurse().setChips(EASY);
		}
		else if(e.getActionCommand().equals(difficulty.Medium.toString()))
		{
			d = difficulty.Medium;
			p.getPurse().setChips(MED);
		}
		else if(e.getActionCommand().equals(difficulty.Difficult.toString()))
		{
			d = difficulty.Difficult;
			p.getPurse().setChips(HARD);
		}
		else
		{
			for(int i =0; i< actionCmds.size(); i++)
			{
				if(actionCmds.get(i).equals(e.getActionCommand()))
				{
					p = ps.get(i);
					p.getPurse().setChips(difficultyChips[d.ordinal()]);
					bigIcon.setFilename(players.get(i).getName()+".gif");
				}
			}
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
	
	public gameType getGameType()
	{
		return g;
	}
	
	public final difficulty getDifficulty()
	{
		return d;
	}
	
	public final AIPlayer getPlayer()
	{
		return p;
	}

	public final boolean isDone()
	{
		return done;
	}
}
