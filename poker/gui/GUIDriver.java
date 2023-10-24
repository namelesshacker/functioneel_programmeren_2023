package gui;

import game.DriverListener;
import game.GameModel;
import gui.hands.GUIHorizontalCard;

import java.awt.*;
import java.util.ArrayList;
import java.util.Scanner;

import javax.swing.*;

import money.GamePot;

import players.*;
import util.test.*;
import util.*;


public class GUIDriver extends JFrame{

	public static void main(String [] args)
	{
/*		GUIUserPanel u =new GUIHorizontalUserPanel("Gen&David", "src/images/ac.gif", 400);
	
		GUIPurse p = new GUIPurse(400);
		p.setPurse(100);
		*/
/*		GUIHand h = new GUIHorizontalHand();
		Hand hand = new Hand();
		hand.add(AllCards.a2C);
		hand.add(AllCards.a2H);
		hand.add(AllCards.a2S);
		hand.add(AllCards.a2D);
		hand.add(AllCards.aKH);
		h.setHand(hand);
		h.revealAllCards();
		h.unfixAllCards();
		
		ArrayList<Player> players =new ArrayList<Player>(3);
		
		try
		{
			players.add(new MikeTyson("-EEEEEEEE", 100, 11233423));
			players.add(new MikeTyson("-XXX", 100, 11233423));
			players.add(new MikeTyson("-OOO", 100, 11233423));
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		
		
		GUIButtonPanel b = new GUIButtonPanel();
		b.loadBetPanel();
		
	//	u.setGUIButtonPanel(b);
		u.setGUIPurse(p);
		u.setGUIHand(h);

		GUIGameScreen gs = new GUIGameScreen(new MikeTyson("-iii", 100, 11233423), players); 
		gs.beginGame();
		
		GUIHorizontalCard c = new GUIHorizontalCard(AllCards.a2C);
		
		GUIDriver g = new GUIDriver();
		g.setTitle("Gen and David");
	//	g.setPreferredSize(new Dimension(160, 70));
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(gs);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);

		System.out.println(gs.getSize());
		
		while(! gs.isGameOver())
		{
			gs.executeSingleMatch();
		}
		
		System.out.println("Game was won by: " + gs.getGameWinner());
		System.exit(0);*/
		
		GUIGame g = new GUIGame();
		
	}
	
	
	public GUIDriver()
	{
		super();
	}
}

