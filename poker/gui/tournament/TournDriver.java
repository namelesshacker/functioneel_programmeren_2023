package gui.tournament;

import images.Constants;

import java.awt.Color;

import gui.start.StartDriver;
import gui.start.StartScreen;

import javax.swing.JFrame;

public class TournDriver extends JFrame{
	public static void main(String[] args)
	{
	//	TournRightPanel tr = new TournRightPanel("src/images/player/TysonT.gif", 0);
		TournScreen s = new TournScreen("src/images/player/TysonT.gif");
	
		TournDriver t = new TournDriver();
		t.setTitle("Gen and David");
		t.setBackground(Constants.TRANSPARENT);
		t.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		t.add(s);
		t.pack();
		t.setLocationRelativeTo(null);
		t.setVisible(true);		
		
		s.drawRound1End();
		s.updateUI();
		s.drawRound2End();
		s.updateUI();
		s.drawRound3End();
	}
	
	public TournDriver()
	{
		super();
	}
}
