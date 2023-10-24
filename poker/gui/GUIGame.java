package gui;

import images.Constants;

import java.awt.Dimension;
import java.io.IOException;
import java.util.ArrayList;


import javax.swing.JFrame;

import ai.AIPlayer;

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

import game.exceptions.GameIsOverException;
import game.exceptions.PurseIsEmptyException;
import gui.error.ErrorScreen;
import gui.humanscreen.HumanScreen;
import gui.savescreen.SaveScreen;
import gui.select.SelectCharactersScreen;
import gui.start.StartScreen;
import gui.tournament.TournScreen;
import gui.versus.VersusScreen;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * The game in its GUI representation.  This class is the game and linearly makes the player
 * go through a series of steps to play arcade and tournament style formats.
 *
 */
public class GUIGame extends JFrame{

	private StartScreen ss;
	private SelectCharactersScreen scs;
	private HumanScreen hs;
	private SaveScreen sas;
	private GUIGameScreen gs;
	
	private GUIHumanPlayer human;
	private HumanScreen.gameType game;
	
	private HumanScreen.difficulty diff;
	
	
	public GUIGame()
	{
		super();
		this.setPreferredSize(new Dimension(Constants.GAME_SIZE.width+50, Constants.GAME_SIZE.height+50));
		this.setBackground(Constants.TRANSPARENT);
		this.setTitle(Constants.TITLE+" - Type your name.");
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setLocationRelativeTo(null);
		setUp();
	}
	
	private void setUp()
	{
		// set up the start screen - type your name here
		ss = new StartScreen();
		this.add(ss);
		this.pack();
		this.setVisible(true);

		// Waits for Start Screen to finish and get's player's name or which file to load from
		while(! ss.isDone());
		String humanName="";
		int loadType  = ss.getFile();
		this.remove(ss);
		
		// we collect the name if there was no loaded file specified
		if(loadType == GUIGameScreen.NO_LOAD)
		{
			humanName = ss.getName();
		
			
			// Waits for Human Screen to finish and get's icon, difficulty and game type
			hs = new HumanScreen(humanName);
			this.setTitle(Constants.TITLE+" - Choose your options");
			this.add(hs);
			this.pack();
			while(! hs.isDone());
			try
			{
				AIPlayer p = hs.getPlayer();
				human = new GUIHumanPlayer(humanName, p.getPurse().getChips(), p.getBigIconName(), p.getSmallIconName());
				game = hs.getGameType();
			}
			catch(PurseIsEmptyException e)
			{
				e.printStackTrace();
				assert false;
			}
			
			diff = hs.getDifficulty();
			this.remove(hs);
		}
		
		// Wait for Save Screen to finish and get the file to which we would like to save
		sas = new SaveScreen(game==HumanScreen.gameType.Tournament || loadType==GUIGameScreen.SLOT_FOUR);
		this.setTitle(Constants.TITLE + " - Select your save options.");
		
		//Load the save screen
		this.pack();
		int saveType = sas.getSaveType();
		this.remove(sas);
		
		if(loadType == GUIGameScreen.SLOT_FOUR)
		{
			playTournamentGame(loadType, saveType);
		}
		
		// if we loaded a game, just play an arcade game now. 
		if(loadType != GUIGameScreen.NO_LOAD)
		{
			playArcadeGame(loadType, saveType);
		}
		
		if(game==HumanScreen.gameType.Arcade)
		{
			playArcadeGame(loadType, saveType);
		}
		else if(game == HumanScreen.gameType.Tournament)
		{
			playTournamentGame(loadType, saveType);
		}		
	}
	
	private void playArcadeGame(int LoadGame, int SaveGame)
	{
		// set up a whole new arcade game
		if(LoadGame == GUIGameScreen.NO_LOAD)
		{
			// create the character selection screen inside the main frame
			this.setTitle(Constants.TITLE+" - Choose your opponents");
			scs = new SelectCharactersScreen(human);
			this.add(scs);
			this.pack();
			
			//wait until the character selection is done
			while(! scs.isDone());
			
			this.remove(scs);
			
			try
			{
				gs = new GUIGameScreen(scs.getPlayers(), SaveGame, -1);
				gs.setDifficultySetting(diff.ordinal()+1);
				VersusScreen vs = new VersusScreen(scs.getPlayers());
				this.add(vs);
				this.pack();
				Thread.sleep(2000);
				this.remove(vs);
			}
			catch (IOException e)
			{
				e.printStackTrace();
				
				ErrorScreen es = new ErrorScreen("Could not load file.");
				this.setTitle(Constants.TITLE+" - Error");
				this.add(es);
				this.pack();
				while(! es.isDone());
				this.remove(es);
				setUp();
			}
			catch(InterruptedException e)
			{
				e.printStackTrace();
			}
		}
		
		// otherwise play a loaded game
		else
		{
			try
			{
				gs = new GUIGameScreen(LoadGame, SaveGame);
				gs.setDifficultySetting(diff.ordinal()+1);
			}
			catch (IOException e)
			{
				e.printStackTrace();
				ErrorScreen es = new ErrorScreen("Could not load file.");
				this.add(es);
				this.setTitle(Constants.TITLE+" - Error");
				this.pack();
				while(! es.isDone());
				this.remove(es);
				setUp();
			}
			catch (GameIsOverException e)
			{
				e.printStackTrace();
			}
		}
		
		this.setTitle(Constants.TITLE+" - Play the game!");
		gs.beginGame();
		gs.setVisible(true);
		this.add(gs);
		this.pack();
		
		//play until the game is over
		while(! gs.isGameOver())
		{
			gs.executeSingleMatch();
		}
		
		System.out.println("Game was won by: " + gs.getGameWinner());
		
		remove(gs);
		setUp();
	}
	
	private void playTournamentGame(int loadType, int saveType)
	{
		// First round
		ArrayList<Player> p = new ArrayList<Player>();
		int purseAmount = 0;
		
		TournScreen ts = new TournScreen("Hasselhoff.gif");
		
		if(loadType==GUIGameScreen.NO_LOAD)
		{
			try
			{
				purseAmount = human.getPurse().getChips();
				human = new GUIHumanPlayer(human.toString()+"-s", purseAmount, human.getBigIconName(), human.getSmallIconName());
			}
			catch(PurseIsEmptyException e)
			{
				e.printStackTrace();
			}
			p.add(human);
			p.add(new DavidHasselhoff("-w", 200, (int)System.currentTimeMillis()));
			p.add(new LeChiffre("-n", 200, (int)System.currentTimeMillis()));
			p.add(new Robespierre("-e", 200, (int)System.currentTimeMillis()));
			
			ts = new TournScreen(human.getSmallIconName());
			
			//show the user what the tournament tree looks like
			this.setTitle(Constants.TITLE+" - Tournament (Round 1)!");
			this.add(ts);
			this.pack();
			while(!ts.isDone());
			this.remove(ts);
			
			//Add the VS screen!
			VersusScreen vs = new VersusScreen(p);
			this.add(vs);
			this.pack();
			try
			{
				Thread.sleep(2000);
			}
			catch(InterruptedException e)
			{
				e.printStackTrace();
			}
			this.remove(vs);
			
			
			try
			{
				gs = new GUIGameScreen(p, saveType, 1);
				gs.setDifficultySetting(diff.ordinal()+1);
			}
			catch (IOException e)
			{
				e.printStackTrace();
				
				ErrorScreen es = new ErrorScreen("Could not load file.");
				this.add(es);
				this.setTitle(Constants.TITLE+" - Error");
				this.pack();
				while(! es.isDone());
				this.remove(es);
				setUp();
			}
			
			this.setTitle(Constants.TITLE+" - Play the game (Tournament Round 1)!");
		}
		else
		{	
			// Loaded tournament
			try
			{
				gs = new GUIGameScreen(loadType, saveType);
				if(gs.getDifficultySetting()==0)
					diff = HumanScreen.difficulty.Easy;
				else if(gs.getDifficultySetting()==1)
					diff = HumanScreen.difficulty.Medium;
				else if(gs.getDifficultySetting()==2)
					diff = HumanScreen.difficulty.Difficult;
			}
			catch(GameIsOverException e)
			{
				e.printStackTrace();
			}
			catch(IOException e)
			{
				e.printStackTrace();
				
				// Brings you to error screen, exiting out of tournament
				ErrorScreen es = new ErrorScreen("Could not load file.");
				this.add(es);
				this.setTitle(Constants.TITLE+" - Error");
				this.pack();
				while(! es.isDone());
				this.remove(es);
				setUp();
			}
			this.setTitle(Constants.TITLE+" - Play the game (Tournament Round "+gs.getTournamentRound()+")!");
		}

		gs.beginGame();
		gs.setVisible(true);
		this.add(gs);
		this.pack();
		
		//play until the game is over
		while(! gs.isGameOver())
		{
			gs.executeSingleMatch();
		}
		
		if(gs.getGameWinner().equals(""))
		{
			//load a losing screen maybe?????
			this.remove(gs);
			setUp();
		}
		if(gs.getTournamentRound()==1)
		{
			//Show the tournament bracket again (updated)
			ts.resetDone();
			this.remove(gs);
			this.add(ts);
			this.setTitle(Constants.TITLE+" - Tournament (Round 2)");
			this.pack();
			ts.drawRound1End();
			ts.updateUI();
			while(! ts.isDone());
			
			this.remove(ts);
			p = new ArrayList<Player>();
			
			if(diff == HumanScreen.difficulty.Easy)
				purseAmount = 300;
			else if(diff == HumanScreen.difficulty.Medium)
				purseAmount = 200;
			else if(diff == HumanScreen.difficulty.Difficult)
				purseAmount = 100;
			
			human = new GUIHumanPlayer(human.toString(), purseAmount, human.getBigIconName(), human.getSmallIconName());
			
			p.add(human);
			p.add(new Trotsky("-w", 200, (int)System.currentTimeMillis()));
			p.add(new KarlMarx("-n", 200, (int)System.currentTimeMillis()));
			p.add(new Giuliani("-e", 200, (int)System.currentTimeMillis()));
			
			// Add the VS screen!
			VersusScreen vs = new VersusScreen(p);
			this.add(vs);
			this.pack();
			try
			{
				Thread.sleep(2000);
			}
			catch(InterruptedException e)
			{
				e.printStackTrace();
			}
			this.remove(vs);
			
			// Start the actual game
			try
			{
				gs = new GUIGameScreen(p, saveType, 2);
			}
			catch (IOException e)
			{
				e.printStackTrace();
				
				ErrorScreen es = new ErrorScreen("Could not load file.");
				this.add(es);
				this.setTitle(Constants.TITLE+" - Error");
				this.pack();
				while(! es.isDone());
				this.remove(es);
				setUp();
			}
			
			gs.setTournamentRound(2);
			
			this.setTitle(Constants.TITLE+" - Play the game (Tournament Round 2)!");
		
			gs.beginGame();
			gs.setVisible(true);
			this.add(gs);
			this.pack();
			
			while(! gs.isGameOver())
			{
				gs.executeSingleMatch();
			}
			
			System.out.println("Tournament Round 2 was won by " + gs.getGameWinner());
		}	
		if(gs.getGameWinner().equals(""))
		{
			//load a losing screen maybe?????
			this.remove(gs);
			setUp();
		}
		if(gs.getTournamentRound()==2)
		{
			//Show the tournament bracket again (updated)
			ts.resetDone();
			ts.drawRound1End();
			this.remove(gs);
			this.add(ts);
			this.setTitle(Constants.TITLE+" - Tournament (Round 3)");
			this.pack();
			ts.drawRound2End();
			ts.updateUI();
			while(! ts.isDone());
			
			this.remove(ts);
			p = new ArrayList<Player>();
			
			if(diff == HumanScreen.difficulty.Easy)
				purseAmount = 300;
			else if(diff == HumanScreen.difficulty.Medium)
				purseAmount = 200;
			else if(diff == HumanScreen.difficulty.Difficult)
				purseAmount = 100;
			
			human = new GUIHumanPlayer(human.toString(), purseAmount, human.getBigIconName(), human.getSmallIconName());
			
			p.add(human);
			p.add(new MikeTyson("-w", 200, (int)System.currentTimeMillis()));
			p.add(new IvyLeaguePrepster("-n", 200, (int)System.currentTimeMillis()));
			p.add(new ErnestoGuevara("-e", 200, (int)System.currentTimeMillis()));
			
			//	Add the VS screen!
			VersusScreen vs = new VersusScreen(p);
			this.add(vs);
			this.pack();
			try
			{
				Thread.sleep(2000);
			}
			catch(InterruptedException e)
			{
				e.printStackTrace();
			}
			this.remove(vs);
			
			try
			{
				gs = new GUIGameScreen(p, saveType, 3);
			}
			catch (IOException e)
			{
				e.printStackTrace();
				
				ErrorScreen es = new ErrorScreen("Could not load file.");
				this.add(es);
				this.setTitle(Constants.TITLE+" - Error");
				this.pack();
				while(! es.isDone());
				this.remove(es);
				setUp();
			}
			
			gs.setTournamentRound(3);
			
			this.setTitle(Constants.TITLE+" - Play the game (Tournament Round 3)!");
			gs.beginGame();
			gs.setVisible(true);
			this.add(gs);
			this.pack();
			
			while(! gs.isGameOver())
			{
				gs.executeSingleMatch();
			}
			
			System.out.println("Tournament Round 3 was won by " + gs.getGameWinner());
			if(gs.getGameWinner().equals(""))
			{
				//load a losing screen maybe?????
				this.remove(gs);
				setUp();
			}
			else
			{
				ts.resetDone();
				ts.drawRound1End();
				ts.drawRound2End();
				this.remove(gs);
				this.add(ts);
				this.setTitle(Constants.TITLE+" - Tournament WON!!!");
				this.pack();

				ts.drawRound3End();
				ts.updateUI();
				while(! ts.isDone());
				setUp();
			}
			setUp();
		}
	}
}
