package gui;

import game.DriverListener;
import game.GameModel;
import gui.hands.GUIHorizontalCard;

import images.Constants;

import java.awt.*;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

import javax.swing.*;

import money.GamePot;

import players.*;
import util.test.*;
import util.*;


public class GUIGameScreenDriver extends JFrame{

	public static void main(String [] args)
	{
	
		ArrayList<Player> players =new ArrayList<Player>(3);
		
		try
		{
			players.add(new GUIHumanPlayer("david-s", 100,"Hasselhoff.gif", "HasselhoffT.gif"));
			players.add(new MikeTyson("-XXX", 100, 2314234));
			players.add(new MikeTyson("-OOO", 100, 11233423));
			players.add(new MikeTyson("-dsfds", 100, 32432));
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		
		
/*		GUIButtonPanel b = new GUIButtonPanel();
		b.loadBetPanel();
		
	//	u.setGUIButtonPanel(b);
		u.setGUIPurse(p);
		u.setGUIHand(h); */

		GUIGameScreen gs = null;
		
		try
		{
		//	gs = new GUIGameScreen(players, GUIGameScreen.RECOVERY_FILE);
		//	gs = new GUIGameScreen(GameModel.LastMatch, GUIGameScreen.SLOT_ONE, GUIGameScreen.RECOVERY_FILE);
			gs.beginGame();
		//	gs.watchSingleMatch(3);
		} 
		catch (Exception e)
		{
			e.printStackTrace();
		} 
				
	//	GUIHorizontalCard c = new GUIHorizontalCard(AllCards.a2C);
		
		GUIGameScreenDriver g = new GUIGameScreenDriver();
		g.setTitle("Gen and David");
	//	g.setPreferredSize(new Dimension(160, 70));
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(gs);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);

		while(! gs.isGameOver())
		{
			gs.executeSingleMatch();
		}
		
		System.out.println("Game was won by: " + gs.getGameWinner());
		System.exit(0);
		
	}
	
	
	public GUIGameScreenDriver()
	{
		super();
	}
}