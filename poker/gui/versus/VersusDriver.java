package gui.versus;

import java.util.ArrayList;

import gui.start.StartDriver;
import gui.start.StartScreen;

import javax.swing.JFrame;

import players.DavidHasselhoff;
import players.Player;

public class VersusDriver extends JFrame {
	public static void main(String[] args)
	{
		VersusDriver g = new VersusDriver();
		
		ArrayList<Player> p = new ArrayList<Player>();
		p.add(new DavidHasselhoff("", 100, 1));
		p.add(new DavidHasselhoff("", 100, 1));
		p.add(new DavidHasselhoff("", 100, 1));
		p.add(new DavidHasselhoff("", 100, 1));
		
		VersusScreen v = new VersusScreen(p);
		
		g.setTitle("Gen and David");
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(v);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);		
	}
	
	public VersusDriver()
	{
		super();
	}
}
