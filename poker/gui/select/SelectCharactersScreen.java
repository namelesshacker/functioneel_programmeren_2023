package gui.select;

import game.exceptions.PurseIsEmptyException;

import images.Constants;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import players.DavidHasselhoff;
import players.ErnestoGuevara;
import players.GUIHumanPlayer;
import players.Giuliani;
import players.IvyLeaguePrepster;
import players.KarlMarx;
import players.LeChiffre;
import players.MikeTyson;
import players.Player;
import players.Robespierre;
import players.Trotsky;

public class SelectCharactersScreen extends JPanel implements ActionListener{

	private final int MAX = 3;
	
	private final String H = "Hasselhoff.gif";
	private final String G = "Giuliani.gif";
	private final String E = "Guevara.gif";
	private final String I = "Ivy.gif";
	private final String L = "Lechiffre.gif";
	private final String M = "Tyson.gif";
	private final String R = "Robespierre.gif";
	private final String K = "Marx.gif";
	private final String T = "Trotsky.gif";
	
	
	private ArrayList<SelectCharPanel> charPanels;
	private SelectBigIconPanel bigIcon;
	private JButton OK;
	private JLabel msg;
	
	private ArrayList<Player> players;
	private int counter;
	private boolean increment;
	private boolean done;
	
	public SelectCharactersScreen(GUIHumanPlayer p)
	{
		super(new BorderLayout());	
		this.setBackground(Constants.TRANSPARENT);
		this.setPreferredSize(Constants.GAME_SIZE);
		
		players = new ArrayList<Player>();
		try
		{
			players.add(new GUIHumanPlayer(p.toString()+"-s",
										   p.getPurse().getChips(),
										   p.getBigIconName(),
										   p.getSmallIconName()));
		}
		catch(PurseIsEmptyException es)
		{
			es.printStackTrace();
			assert false;
		}
		String s = java.io.File.separator;
		
		counter = 0;
		increment = true;
		done = false;
		
		bigIcon = new SelectBigIconPanel();
		bigIcon.addIcon(p.getBigIconName());
		
		GridLayout g = new GridLayout(3, 3);
		g.setHgap(15);
		g.setVgap(5);
		
		JPanel chars = new JPanel();
		chars.setBackground(Constants.TRANSPARENT);
		chars.setLayout(g);
		
		charPanels = new ArrayList<SelectCharPanel>();
		
		// Hasselhoff, lechiffre, robspierre
		charPanels.add(new SelectCharPanel("David Hasselhoff", "HasselhoffT.gif", H));
		charPanels.add(new SelectCharPanel("Karl Marx", "MarxT.gif", K));
		charPanels.add(new SelectCharPanel("Mike Tyson", "TysonT.gif", M));
		
		//marx, trotsky, jekyll
		charPanels.add(new SelectCharPanel("Le Chiffre","LechiffreT.gif", L));
		charPanels.add(new SelectCharPanel("Trotsy", "TrotskyT.gif", T));
		charPanels.add(new SelectCharPanel("Ivy League Prepster","IvyT.gif", I));
		
		//tyson, guevara, ivy
		charPanels.add(new SelectCharPanel("Robespierre", "RobespierreT.gif", R));
		charPanels.add(new SelectCharPanel("Rudy Giuliani", "GiulianiT.gif", G));
		charPanels.add(new SelectCharPanel("Ernesto Guevara", "GuevaraT.gif", E));
		
		for(int i = 0; i<charPanels.size(); i++)
		{
			charPanels.get(i).addActionListener(this);
			chars.add(charPanels.get(i));
		}
		
		OK = new JButton("Play Game!");
		OK.setFont(Constants.FONT);
		OK.setBackground(Constants.TRANSPARENT);
		OK.setBorder(BorderFactory.createLineBorder(Color.red));
		OK.setPreferredSize(new Dimension(100, 30));
		OK.addActionListener(this);
		OK.setVisible(false);
		
		msg = new JLabel("Choose 3 characters to fight");
		msg.setFont(Constants.FONT);
		
		JPanel south = new JPanel(new GridLayout(0,1));
		south.setBackground(Constants.TRANSPARENT);
		
		JPanel ok = new JPanel(new FlowLayout());
		ok.setBackground(Constants.TRANSPARENT);
		
		JPanel sp1 = new JPanel();
		sp1.setBackground(Constants.TRANSPARENT);
		sp1.setPreferredSize(new Dimension(100, 30));
		
		JPanel sp2 = new JPanel();
		sp2.setBackground(Constants.TRANSPARENT);
		sp2.setPreferredSize(new Dimension(100, 30));
		
		JPanel m = new JPanel(new FlowLayout());
		m.setBackground(Constants.TRANSPARENT);
		
		JPanel sp3 = new JPanel();
		sp3.setBackground(Constants.TRANSPARENT);
		sp3.setPreferredSize(new Dimension(100, 30));
		
		JPanel sp4 = new JPanel();
		sp4.setBackground(Constants.TRANSPARENT);
		sp4.setPreferredSize(new Dimension(100, 30));
		
		m.add(sp3);
		m.add(msg);
		m.add(sp4);
		
		ok.add(sp1);
		ok.add(OK);
		ok.add(sp2);
		
		south.add(m);
		south.add(ok);
		
		JPanel east = new JPanel();
		east.setBackground(Constants.TRANSPARENT);
		east.setPreferredSize(new Dimension(150, this.getHeight()));
		
		JPanel west = new JPanel();
		west.setBackground(Constants.TRANSPARENT);
		west.setPreferredSize(new Dimension(150, this.getHeight()));
		
		JPanel north = new JPanel();
		north.setBackground(Constants.TRANSPARENT);
		north.setPreferredSize(new Dimension(this.getWidth(), 95));
		
		JPanel sou = new JPanel();
		south.setBackground(Constants.TRANSPARENT);
		south.setPreferredSize(new Dimension(this.getWidth(), 95));
		
		BorderLayout b = new BorderLayout();
		b.setVgap(20);
		JPanel center = new JPanel(b);
		center.setBackground(Constants.TRANSPARENT);
		center.add(chars, BorderLayout.NORTH);
		center.add(bigIcon, BorderLayout.CENTER);
		center.add(south, BorderLayout.SOUTH);
		
		this.add(north, BorderLayout.NORTH);
		this.add(west, BorderLayout.WEST);
		this.add(center, BorderLayout.CENTER);
		this.add(east, BorderLayout.EAST);
		this.add(sou, BorderLayout.SOUTH);
	
		System.out.println(bigIcon.getPreferredSize());
	}
	
	public final void actionPerformed(ActionEvent e)
	{	
		if( ((JButton) e.getSource()).equals(OK))
		{
			ArrayList<String> filenames = bigIcon.getFilenames();
			ArrayList<String> dir = new ArrayList<String>();
			
			dir.add("-w");
			dir.add("-n");
			dir.add("-e");
			
			for(int i=1; i<filenames.size(); i++)
			{
				String filename = filenames.get(i);
				if(filename.equals(H))
					players.add(new DavidHasselhoff(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(G))
					players.add(new Giuliani(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(E))
					players.add(new ErnestoGuevara(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(I))
					players.add(new IvyLeaguePrepster(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(L))
					players.add(new LeChiffre(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(M))
					players.add(new MikeTyson(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(R))
					players.add(new Robespierre(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(T))
					players.add(new Trotsky(dir.get(i-1), 200, (int)System.currentTimeMillis()));
				else if(filename.equals(K))
					players.add(new KarlMarx(dir.get(i-1), 200, (int)System.currentTimeMillis()));
			}
			
			System.out.println(players);
			done = true;
			return;
		}
		
		
		SelectCharPanel temp = (SelectCharPanel) e.getSource();
		
		for(int i=0; i < charPanels.size(); i++)
		{
			if(charPanels.get(i).equals(temp))
			{
				if(increment)
				{
					msg.setText("Choose 3 characters to fight");
					bigIcon.addIcon(charPanels.get(i).getBigFilename());
					charPanels.get(i).incrementCircles();
					counter ++;
					if(counter >= MAX)
					{
						counter = MAX;
						increment = false;
					}
				}
				else
				{
					if(bigIcon.removeIcon(charPanels.get(i).getBigFilename()))
					{	
						msg.setText("Deselect all characters.");
						counter --;
						charPanels.get(i).decrementCircles();
					}
					if(counter <= 0)
					{
						msg.setText("Choose 3 characters to fight");
						counter = 0;
						increment = true;
					}
				}
				
				if(counter == 3)
				{
					OK.setVisible(true);
					msg.setText("Play Game or deselect all characters");
				}
				else
					OK.setVisible(false);	
				
				this.updateUI();
			}
		}
	}
	
	public final boolean isDone()
	{
		return done;
	}
	
	public final ArrayList<Player> getPlayers()
	{
		return players;
	}
}
