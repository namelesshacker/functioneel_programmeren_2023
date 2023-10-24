package gui;

import game.GameFileParser;
import game.GameModel;
import game.actions.*;
import game.exceptions.GameIsOverException;
import game.exceptions.HandFoldedException;
import game.exceptions.PurseIsEmptyException;
import gui.exceptions.GUIGameEndedException;
import gui.hands.*;
import gui.userpanels.*;

import images.Constants;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import java.io.File;
import java.io.IOException;

import money.GamePot;
import money.Purse;

import players.GUIHumanPlayer;
import players.Player;
import scoring.HandValuator;
import util.Hand;
import util.Card;

public class GUIGameScreen extends JPanel implements Player{
	
	// a boolean indicating the game is over
	private boolean mIsGameOver = false;
	
	// a boolean indicating which of the two constructors we called i.e. if we loaded a file
	// or if we started from scratch 
	private boolean mPlayingNormally;
	
	// a string indicating the game winner (set when mIsGameOver = true and the human won)
	private String mPlayerWinner = "";
	
	// a hack enum for determining whether to clear the AI message panels during game play. 
	private enum clearState
	{
		CLEAR_SCREEN, PRE_BET_FIRST_TIME, CARD_FIRST_TIME, POST_BET_FIRST_TIME, NO_CLEAR
	}
	
	// the hack enum's current state
	private clearState mState;
	
	// our player components 
	private final String 		mUserName;
	private       Purse  		mUserPurse;
	private		  Hand   		mUserHand; 
	private       HandValuator  mHandValuator;
	private 	  ImageIcon 	bigIcon;
	private 	  ImageIcon 	smallIcon;
	private 	  String 		smallIconName;
	private 	  String 		bigIconName;
	
	// our game components 
	private int       mTotalMoney; 
	private GameModel mGameModel;
	private GamePot   mGamePot; 
	private int       mNumPlayers;
	
	// some constants
	private final int NS_USER_WIDTH = 700; 
	private final int N_USER_HEIGHT = 140;
	private final int S_USER_HEIGHT = 250;
	private final int EW_USER_WIDTH = 220; 	
	private final int EW_USER_HEIGHT = 480; 	
	
	// panels and components
	private ArrayList<GUIUserPanel> mAIPanels;
	private ArrayList<String> 		mAINames;
	private GUIUserPanel	  		mUserPanel;
	
	private GUIButtonPanel			mButtonPanel;
	private GUIVerticalHand			mGUIUserHand;
	private GUIPurse				mGUIUserPurse;
	
	private GUIWestUserPanel west;
	private GUIEastUserPanel east;
	private GUINorthUserPanel north;
	
	private GUIPot mGUIPot;
	
	private JPanel c;
	private GUICentrePanel mCentre;
	private GUIWinningHand mWinningHand;
	
	// game autosaver tools
	private String mAutoSaveFileName;
	private GUIGameSaver mAutoSaver;
	
	// game file reading tool
	private GameFileParser mGameFileParser;
	
	// public file slots you can save to and from where to load from
	public static final int NO_LOAD			= -2;
	public static final int NO_SAVE			= -1;
	public static final int SLOT_ONE 		= 1;
	public static final int SLOT_TWO		= 2;
	public static final int SLOT_THREE		= 3;
	public static final int SLOT_FOUR       = 4;
	public static final int LAST_MATCH		= GameModel.LastMatch;
	
	// the corresponding file names for the slots
	private String ss 				= File.separator;
	private String mSlotOneFile 	= new String("src" + ss + "savedgames" + ss + "PokerFighter_AutoSave_File1.txt");
	private String mSlotTwoFile 	= new String("src" + ss + "savedgames" + ss + "PokerFighter_AutoSave_File2.txt");
	private String mSlotThreeFile 	= new String("src" + ss + "savedgames" + ss + "PokerFighter_AutoSave_File3.txt");
	private String mSlotFourFile 	= new String("src" + ss + "savedgames" + ss + "PokerFighter_AutoSave_File4.txt");
	
	private int am9=0;
	/**
	 * Loads the game from the input file. 
	 * @param TargetMatch The match at which we wish to start. In client code this is always set 
	 * to LAST_MATCH but we support any match which has previously been played and is in the file. 
	 * @param filename The file containing the game to load
	 * @param saveName The file under which we wish to save, if at all
	 * @throws IOException whenever the load file is corrupted. 
	 */
	public GUIGameScreen(int loadFile, int saveFile) throws IOException, GameIsOverException
	{
		super(new BorderLayout());
		
		// set the way in which we were set up. we use this boolean to determine whether
		// or not we should add ourselves to the game during the "beginGame" function call. 
		mPlayingNormally = false;
		
		// construct our game pot
		mGamePot   = new GamePot(4);
		
		// construct the game file parser
		String filename = ""; 
		switch(loadFile)
		{
			case SLOT_ONE:
				filename = mSlotOneFile;
				break;
			case SLOT_TWO:
				filename = mSlotTwoFile;
				break;
			case SLOT_THREE:
				filename = mSlotThreeFile;
				break;
			case SLOT_FOUR:
				filename = mSlotFourFile;
				break;
			default:
				throw new IOException();
		}
		
		try
		{
			mGameFileParser = new GameFileParser(filename);
		}
		catch (Exception e)
		{
			throw new IOException("Failure reading the file");
		}
		
		// check whether the file could be loaded
		if(mGameFileParser.isCorrupted())
			throw new IOException("Failure reading the file");
		
		// create the game model we had previously
		mGameModel = new GameModel(GameModel.LastMatch, mGameFileParser.iterator(), mGamePot);
		
		// Collect the players in an array. The original constructor requires the
		// that the user player be the first entry in the array, so we move the user
		// to the front. 
		Player tPlayer;
		ArrayList<Player> Players = mGameModel.getLoadedPlayers();
		
		if(Players.size() == 1)
		{
			throw new GameIsOverException();
		}

		// put human in front
		for(int i = 0; i < Players.size(); i++)
		{
			if(Players.get(i) instanceof GUIHumanPlayer)
			{
				tPlayer = Players.get(0);
				Players.set(0, Players.get(i));
				Players.set(i, tPlayer);
				break;
			}
		}
		
		// the way we load the players requires to switch these two
		Player zPlayer = Players.get(1);
		Players.set(1, Players.get(3));
		Players.set(3, zPlayer);
		
		// initialize some player components
		mUserName     = Players.get(0).toString();
		mUserHand 	  = new Hand();
		smallIconName = Players.get(0).getSmallIconName();
		bigIconName   = Players.get(0).getBigIconName();
		
		try
		{
			mUserPurse = new Purse(Players.get(0).getPurse().getChips());
		} 
		catch (Exception e)
		{
			e.printStackTrace();
			assert false; 
		}
		
		// Initialize the AI names
		mAINames = new ArrayList<String>();
		for(int i = 1; i < Players.size(); i++)
			if(Players.get(i) != null)
				mAINames.add(Players.get(i).toString());
			else
				mAINames.add(null);
		
		// set up the game saver
		// collect the file to save to
		switch(saveFile)
		{
			case NO_SAVE:
				mAutoSaveFileName = "";
				break;
			case SLOT_ONE:
				mAutoSaveFileName = mSlotOneFile;
				break;
			case SLOT_TWO:
				mAutoSaveFileName = mSlotTwoFile;
				break;
			case SLOT_THREE:
				mAutoSaveFileName = mSlotThreeFile;
				break;
			case SLOT_FOUR:
				mAutoSaveFileName = mSlotFourFile;
				break;
		}
		
		if(!mAutoSaveFileName.equals(""))
		{
			mAutoSaver = new GUIGameSaver(mAutoSaveFileName);
		
			// if anything at all went wrong, we do not bother saving. 
			if(!mAutoSaver.isCorrupted())
				mGameModel.attachGameListener(mAutoSaver);
			else
				throw new IOException();
		}
		
		// send the new game action to remember it in the file
		// if the action is null then the file was invalid. 
		if(mGameModel.getNewGameAction() == null)
			throw new IOException();
		
		if(mAutoSaver != null)
			mAutoSaver.notify(mGameModel.getNewGameAction());
		
		// swap the GUIUserPlayer with the GUIGameScreen
		mGameModel.swapLoadedPlayers(Players.get(0),this);
		
		// Call the common constructor code
		init(Players);
	}
	
	public GUIGameScreen(ArrayList<Player> Players, int saveFile, int TournamentRound) throws IOException
	{
		super(new BorderLayout());
	
		// determine how we were set up
		mPlayingNormally = true;
		
		// Initialize some of our player components
		mUserName = Players.get(0).toString();
		mUserHand = new Hand();
		smallIconName = Players.get(0).getSmallIconName();
		bigIconName   = Players.get(0).getBigIconName();
		
		try
		{
			mUserPurse = new Purse(Players.get(0).getPurse().getChips());
		} 
		catch (Exception e)
		{
			e.printStackTrace();
			assert false; 
		}
	
		// Initialize the AI names
		mAINames = new ArrayList<String>();
		for(int i = 1; i < Players.size(); i++)
			mAINames.add(Players.get(i).toString());
		
		// Initialize the GamePot
		mGamePot = new GamePot(1 + mAINames.size()); 
		
		// Initialize the GameModel
		mGameModel = new GameModel(mGamePot);
		mGameModel.setTournamentRound(TournamentRound);
		
		// Initialize GameModel with the AIPlayers and ourselves as the human player
		// Notice that we cannot see the AI cards at any point during execution
		try
		{
			mGameModel.addPlayerToGame(Players.get(1));
			mGameModel.addPlayerToGame(Players.get(2));
			mGameModel.addPlayerToGame(Players.get(3));
		}
		catch (Exception e)
		{
			e.printStackTrace();
			this.setBackground(Color.BLACK);
		}

		// set up the game saver
		// collect the file to save to
		switch(saveFile)
		{
			case NO_SAVE:
				mAutoSaveFileName = "";
				break;
			case SLOT_ONE:
				mAutoSaveFileName = mSlotOneFile;
				break;
			case SLOT_TWO:
				mAutoSaveFileName = mSlotTwoFile;
				break;
			case SLOT_THREE:
				mAutoSaveFileName = mSlotThreeFile;
				break;
			case SLOT_FOUR:
				mAutoSaveFileName = mSlotFourFile;
				break;
		}
		
		System.out.println("-->" + mAutoSaveFileName);
		
		if(!mAutoSaveFileName.equals(""))
		{
			mAutoSaver = new GUIGameSaver(mAutoSaveFileName);
		
			// if anything at all went wrong, we do not bother continuing. 
			if(!mAutoSaver.isCorrupted())
				mGameModel.attachGameListener(mAutoSaver);
			else
				throw new IOException();
		}
		
		init(Players);
	}
	
	/**
	 * Common constructor code. 
	 * @param Players The first index in this array list is the user information. The panels
	 * and everything else player related is set up assuming the player information is valid. 
	 * This function does not add the players to the GameModel. 
	 */
	private void init(ArrayList<Player> Players)
	{
		// initialize the images for all cards (to avoid slow disk access)
		// we didnt think of this, so dont credit us for saving all the cards
		
		new Constants().initializeAllCards();
		
		mNumPlayers = Players.size();
		mHandValuator = new HandValuator();
		
		// Initialize the total money in the game
		try
		{
			mTotalMoney = mUserPurse.getChips();
			
			for(int i = 1; i < Players.size(); i++)
				if(Players.get(i) != null)
					mTotalMoney += Players.get(i).getPurse().getChips();
		}
		catch (Exception e)
		{
			e.printStackTrace(); 
			assert (false); 
		}
		
		// Initialize the user panel
		mUserPanel = new GUISouthUserPanel(mUserName, Players.get(0).getSmallIconName(), mTotalMoney);
		mUserPanel.setGUIHand(new GUIVerticalHand());
		
		// set the gui hand
		mGUIUserHand = new GUIVerticalHand();
		mUserPanel.setGUIHand(mGUIUserHand);
		
		try
		{
			mGUIUserPurse = new GUIHorizontalPurse(mTotalMoney);
			mGUIUserPurse.addToPurse(mUserPurse.getChips()); 
			mUserPanel.setGUIPurse(mGUIUserPurse);
		}
		catch (Exception e)
		{
			e.printStackTrace();
			assert (false); 
		}
		
		// Initialize the ai panels
		mAIPanels = new ArrayList<GUIUserPanel>();
		
		// Initialize 1/3 - WEST
		GUIHand mAIHand = new GUIHorizontalHand();
		String mAIName  = "";
		String mIcName  = "";
		int    mAIPurse = 0;
		mAIHand.unrevealedHand();
		boolean makeVisible;
		
		if(mAINames.get(0) == null)
		{
			mAIName = "";
			mIcName = "";
			mAIPurse = 0;
			mNumPlayers --;
			makeVisible = false;
		}
		else
		{
			mAIName = mAINames.get(0);
			mIcName = Players.get(1).getSmallIconName();
			makeVisible = true;
			
			try
			{
				mAIPurse = Players.get(1).getPurse().getChips();
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}
		
		west = new GUIWestUserPanel(mAIName, mIcName, mTotalMoney);
		west.setVisible(makeVisible);
		mAIPanels.add(west);
		mAIPanels.get(0).setGUIHand(mAIHand);
		
		GUIPurse t = new GUIVerticalPurse(mTotalMoney);
		t.addToPurse(mAIPurse); 
		mAIPanels.get(0).setGUIPurse(t);
		
		// Initialize 2/3 - NORTH
		mAIHand = new GUIVerticalHand();
		mAIHand.unrevealedHand();
		
		if(mAINames.get(1) == null)
		{
			mAIName = "";
			mIcName = "";
			mAIPurse = 0;
			makeVisible = false;
			mNumPlayers --;
		}
		else
		{
			mAIName = mAINames.get(1);
			mIcName = Players.get(2).getSmallIconName();
			makeVisible = true;
			
			try
			{
				mAIPurse = Players.get(2).getPurse().getChips();
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}
		
		north = new GUINorthUserPanel(mAIName, mIcName, mTotalMoney);
		north.setVisible(makeVisible);
		mAIPanels.add(north);
		mAIPanels.get(1).setGUIHand(mAIHand);
		
		t = new GUIHorizontalPurse(mTotalMoney);
		t.addToPurse(mAIPurse); 
		mAIPanels.get(1).setGUIPurse(t);
		
		// Initialize 3/3 - EAST
		mAIHand = new GUIHorizontalHand();
		mAIName  = "";
		mIcName  = "";
		mAIPurse = 0;
		mAIHand.unrevealedHand();
		
		if(mAINames.get(2) == null)
		{
			mAIName = "";
			mIcName = "";
			mAIPurse = 0;
			makeVisible = false;
			mNumPlayers --;
		}
		else
		{
			mAIName = mAINames.get(2);
			mIcName = Players.get(3).getSmallIconName();
			makeVisible = true;
			
			try
			{
				mAIPurse = Players.get(3).getPurse().getChips();
			}
			catch (Exception e)
			{
				e.printStackTrace();
			}
		}
		
		east = new GUIEastUserPanel(mAIName, mIcName, mTotalMoney);
		east.setVisible(makeVisible);
		mAIPanels.add(east);
		mAIPanels.get(2).setGUIHand(mAIHand);
		
		t = new GUIVerticalPurse(mTotalMoney);
		t.addToPurse(mAIPurse); 
		mAIPanels.get(2).setGUIPurse(t);
		
		// Create anonymous "spacer" elements
		JPanel sp1, sp2, sp3, sp4;
		
		sp1 = new JPanel();
		sp1.setPreferredSize(new Dimension(EW_USER_WIDTH, N_USER_HEIGHT));
		sp1.setBackground(Constants.TRANSPARENT);

		sp2 = new JPanel();
		sp2.setBackground(Constants.TRANSPARENT);
		sp2.setPreferredSize(new Dimension(EW_USER_WIDTH, N_USER_HEIGHT));

		sp3 = new JPanel();
		sp3.setBackground(Constants.TRANSPARENT);
		sp3.setPreferredSize(new Dimension(EW_USER_WIDTH, S_USER_HEIGHT));

		sp4 = new JPanel(new BorderLayout());
		sp4.setBackground(Constants.TRANSPARENT);
		sp4.setPreferredSize(new Dimension(EW_USER_WIDTH, S_USER_HEIGHT));
		
		// Create the north set of panels i.e. spacer AI-North spacer
		JPanel n = new JPanel();
		n.setBackground(Constants.TRANSPARENT);
		n.setPreferredSize(new Dimension(NS_USER_WIDTH, N_USER_HEIGHT));
		n.add(mAIPanels.get(1)); 
		
		JPanel north_1 = new JPanel(new FlowLayout());
		north_1.setBackground(Constants.TRANSPARENT);
		north_1.add(sp1);
		north_1.add(n);
		north_1.add(sp2);
		
		// Create the south set of panels i.e. spacer User spacer
		JPanel s = new JPanel();
		s.setBackground(Constants.TRANSPARENT);
		s.setPreferredSize(new Dimension(NS_USER_WIDTH, S_USER_HEIGHT));
		s.add(mUserPanel); 
		
		JPanel south_1 = new JPanel(new FlowLayout());
		south_1.setBackground(Constants.TRANSPARENT);
		south_1.add(sp3);
		south_1.add(s);
		south_1.add(sp4);
		
		// Create the east, west, and centre panels
		JPanel w = new JPanel();
		w.setBackground(Constants.TRANSPARENT);
		w.setPreferredSize(new Dimension(EW_USER_WIDTH, EW_USER_HEIGHT));
		w.add(mAIPanels.get(0));
		
		JPanel e = new JPanel();
		e.setBackground(Constants.TRANSPARENT);
		e.setPreferredSize(new Dimension(EW_USER_WIDTH, EW_USER_HEIGHT));
		e.add(mAIPanels.get(2));
			
		c = new JPanel();
		c.setBackground(Constants.TRANSPARENT);
		
		for(int i = 0; i < Players.size(); i++)
			if(Players.get(i) == null)
				Players.remove(i);
		
		mGUIPot = new GUIPot(mGamePot, Players);
		mWinningHand = new GUIWinningHand(mGUIPot, mUserName, Players);
		mCentre = new GUICentrePanel(mGUIPot, mWinningHand, NS_USER_WIDTH, EW_USER_HEIGHT);
		
		c.add(mCentre);
		// Create the user button panel
		mButtonPanel = new GUIButtonPanel();
		
		// Add the button panel to the user panel
		((GUISouthUserPanel)mUserPanel).setGUIButtonPanel(mButtonPanel); 
		
		rearrangePanels();
		
		// Add the whole kaboozle to ourselves graphically
		this.setBackground(Constants.TRANSPARENT);		
		this.add(north_1, BorderLayout.PAGE_START);
		this.add(south_1, BorderLayout.PAGE_END);
		this.add(e, BorderLayout.LINE_START);
		this.add(w, BorderLayout.LINE_END);
		this.add(c, BorderLayout.CENTER);
	}
	
	/**
	 * This function allows games to be played after the constructor was called. 
	 */
	public void beginGame()
	{
		try
		{
			if(mPlayingNormally)
				mGameModel.addPlayerToGame(this);
		}
		catch (Exception e)
		{
			e.printStackTrace(); 
			this.setBackground(Color.CYAN); 
		}
	}
	
	/**
	 * Executes a single match from GameModel. The user is prompted whether he/she is ready
	 * for the next match or if they would like to quit. If quit, the game is set to over
	 * but no winner is declared. 
	 *
	 */
	public final void executeSingleMatch()
	{
		try
		{ 
			// if he quit, append a new match action to the end of the file we're saving to
			// unless of course its not being saved. 
			// then exit
			if(mIsGameOver == true)
			{
				if(mAutoSaver != null)
				{
					// set the match ID to anything. the file only cares about the
					// conclusion of the previous match. 
					NewMatchAction lAction = new NewMatchAction();
					lAction.setMatchID(84);
					mAutoSaver.notify(new NewMatchAction());
				}
				
				// exit
				return;
			}
			
			if(mIsGameOver == false)
				// check if user is ready for the next match with option to quit
				confirmUserReady("Ready?", "Yes", am9!=0);
			am9++;
			
			if(mIsGameOver == false)
				mGameModel.ExecuteSingleMatch();
		}
		catch(Exception e)
		{
			e.printStackTrace();
			return;
		}
	}
	
	/**
	 * @return whether or not the user has won or lost
	 */
	public boolean isGameOver()
	{
		return mIsGameOver ;
	}
	
	/**
	 * @return the user name if the user won, an empty string otherwise
	 */
	public String getGameWinner()
	{
		return mPlayerWinner;
	}
	
	/**
	 * see @interface Player
	 */
	public Purse getPurse()
	{
		return mUserPurse; 
	}
	
	/**
	 * see @interface Player
	 */
	public void addToPurse(int amount)
	{
		try
		{
			mUserPurse.addChips(amount); 
		}
		catch (Exception e)
		{
			e.printStackTrace();
			assert false; 
		}
	}
	
	/**
	 * See @Interface Player 
	 */
	public void setHand(Hand hand)
	{
		mUserHand = hand;
		mGUIUserHand.setHand(hand);
		mGUIUserHand.fixAllCards();
		mGUIUserHand.revealAllCards();
		
		if(mUserPanel.isFolded())
			mUserPanel.unfoldHand();
		
		mUserPanel.updateUI();
		
		if(mAutoSaver != null)
			if(mAutoSaver.isCorrupted() == false)
				mAutoSaver.notify(new UserSetHandAction(mUserName, mUserHand, mHandValuator.valuateHand(mUserHand)));
	}
	
	/**
	 * See @interface Player
	 */
	public Hand getHand()
	{
		return mUserHand; 
	}
	
	/**
	 * See @Interface Player
	 */
	public BetAction makePreBetAction(int min)
	{
		BetAction result = null;
		
		if(mState == clearState.CLEAR_SCREEN)
			mCentre.resetAll();
		
		mState = clearState.CARD_FIRST_TIME;
		
		mCentre.resetSouth();
		
		try
		{
			int maxBet = this.mUserPurse.getChips() - min;
			mButtonPanel.resetCommAction();
			mButtonPanel.loadBetPanel(min, maxBet);
			while(mButtonPanel.getCommAction() == null);
			mButtonPanel.loadEmptyPanel();
			
			BetAction comm = (BetAction) mButtonPanel.getCommAction();
			
			result = new BetAction(mUserName, comm.getAction(), comm.getAmount());
		}
		catch(PurseIsEmptyException e)
		{
			e.printStackTrace();
			assert false;
		}
		
		return result; 
	}
	
	/**
	 * See @interface player
	 */
	public BetAction makePostBetAction(int min)
	{
		if(mState == clearState.POST_BET_FIRST_TIME)
			mCentre.resetAll();
		
		mState = clearState.CARD_FIRST_TIME;
		
		return makePreBetAction(min); 
	}
	
	/**
	 * See @interface player
	 */
	public ArrayList<Card> discardCards(int max)
	{
		ArrayList<Card> DiscardedCards = null;
	
		if(mState == clearState.CARD_FIRST_TIME)
			mCentre.resetAll();
		
		mState = clearState.POST_BET_FIRST_TIME;
		
		// Let user select cards now
		mGUIUserHand.unfixAllCards(); 
		
		// Wait for the "ok" from the user
		mButtonPanel.resetCommAction();
		mButtonPanel.loadDiscardPanel();
		mButtonPanel.grabFocus();
		while(mButtonPanel.getCommAction() == null); 
		mButtonPanel.loadEmptyPanel(); 
		this.updateUI(); 
		
		// Once the user said "ok", the cards currently raised
		// will be discarded. Get them from the GUIUserHand 
		DiscardedCards = mGUIUserHand.getSelectedCards();
		
		// Set the selected cards as invisible
		mGUIUserHand.hideSelectedCards(); 
		
		// Update the user interface
		this.updateUI();
		
		return DiscardedCards; 
	}
	
	/**
	 * See @interface Player
	 */
	public ScoreRevealAction makeScoreRevealAction()
	{	
		if(mState != clearState.NO_CLEAR)
			mCentre.resetAll();
		
		mState = clearState.CLEAR_SCREEN;
		
		ScoreRevealAction toReturn = null; 

		mButtonPanel.resetCommAction();
		mButtonPanel.loadCompetePanel();
		mButtonPanel.grabFocus();
		while(mButtonPanel.getCommAction() == null);
		mButtonPanel.loadEmptyPanel();
		this.updateUI();
		
		ScoreRevealAction comm = (ScoreRevealAction) mButtonPanel.getCommAction();
		
		toReturn = new ScoreRevealAction(mUserName, comm.getAction(), getHand());
		
		return toReturn;
	}
	
	/**
	 * See @interface Player
	 */
	public void notify(Action action)
	{
		if(action instanceof CardsDealtAction)
		{
			mState = clearState.CLEAR_SCREEN;
			
			handleGeneralAction(action);
		}
		else if(action instanceof CardExchangeAction)
		{
			if(mState == clearState.CARD_FIRST_TIME)
				mState = clearState.CLEAR_SCREEN;
			
			handleGeneralAction(action);
			
			mState = clearState.POST_BET_FIRST_TIME;
		}
		else if (action instanceof IncrementAnteAction)
		{
			handleGeneralAction(action);
			handleIncrementAnteAction((IncrementAnteAction) action);
		}
		else if(action instanceof BetAction)
		{
			if(mState == clearState.POST_BET_FIRST_TIME)
			   mState = clearState.CLEAR_SCREEN;
			
			BetAction lAction = (BetAction) action;
			
			handleBetAction(lAction);
			
			mState=clearState.CARD_FIRST_TIME;
			
			if(lAction.getAction()==BetAction.betAction.FOLD)
				handleFold(lAction);
		}
		else if(action instanceof NewMatchAction)
		{
			mWinningHand.notify(action);
			reset();
		}
		else if(action instanceof NewGameAction)
		{
			
		}
		else if(action instanceof PlayerRemovedAction)
		{
			try
			{
				handlePlayerRemovedAction((PlayerRemovedAction) action); 
				mWinningHand.notify(action);
			} 
			catch (GUIGameEndedException e)
			{
				return;
			}
		}
		else if(action instanceof ScoreRevealAction)
		{
			if(mState == clearState.CARD_FIRST_TIME 
			|| mState == clearState.POST_BET_FIRST_TIME)
				mState = clearState.CLEAR_SCREEN;
			
			mWinningHand.notify(action);
			
			ScoreRevealAction lAction = (ScoreRevealAction) action;
			
			handleScoreRevealAction(lAction);
			
			if(lAction.getAction()==ScoreRevealAction.scoreRevealAction.FOLD)
				handleFold(action);
			
			mState = clearState.NO_CLEAR;
		}
		else if(action instanceof GetMoneyAction)
		{
			mWinningHand.notify(action);
			handleGetMoneyAction((GetMoneyAction) action);
			displayPotWinner((GetMoneyAction) action);
		}
		else if(action instanceof ScoreAction)
		{
			handleScoreAction((ScoreAction) action); 
		}
	}
	
	/**
	 * See @interface player
	 */
	public final String toString()
	{
		return this.mUserName;
	}
	
	/**
	 * Called between rounds to reset the gui
	 */
	private void reset()
	{
		north.getGUIHand().fixAllCards();
		north.getGUIHand().unrevealAllCards();
		
		east.getGUIHand().fixAllCards();
		east.getGUIHand().unrevealAllCards();

		west.getGUIHand().fixAllCards();
		west.getGUIHand().unrevealAllCards();

		mCentre.resetAll();
		mCentre.setVisible(true);
		mWinningHand.setVisible(false);
		
		mUserPanel.setDealerFalse();
		west.setDealerFalse();
		east.setDealerFalse();
		north.setDealerFalse();
		
		if(north.isFolded())
			north.unfoldHand();
		if(east.isFolded())
			east.unfoldHand();
		if(west.isFolded())
			west.unfoldHand();
		
		mCentre.setPotAndWinningHand(true, false);

		north.updateUI();
		west.updateUI();
		east.updateUI();
	}
	
	/**
	 * Takes care of displaying the "you lost" screen to the user
	 */
	private void handleUserLostPrematurely()
	{
		this.removeAll();
		GUIButtonPanel x = new GUIButtonPanel();
		x.loadConfirmationPanel("You just lost.", "So what?", false);
		this.add(x, BorderLayout.CENTER);
		x.resetCommAction();
		this.updateUI();
		System.out.println("bfore while loop");
		while(x.getCommAction() == null);
		System.out.println("aftr while loop");
	//	this.removeAll();
		this.updateUI();
		mIsGameOver   = true;
		mPlayerWinner = "";
	}
	
	/**
	 * Handles displaying the "you won" screen to the user
	 */
	private void handleUserWonGame()
	{
		this.removeAll();
		GUIButtonPanel x = new GUIButtonPanel();
		x.loadConfirmationPanel("You just won!", "I know.", false);
		this.add(x, BorderLayout.CENTER);
		x.resetCommAction();
		this.updateUI();
		while(x.getCommAction() == null);
		this.removeAll();
		this.updateUI();
		mIsGameOver   = true;
		mPlayerWinner = mUserName;
	}
	
	/**
	 * When there are only two players left, this function makes sure the north panel is
	 * occupied by a player. 
	 */
	private void rearrangePanels()
	{
		if(mNumPlayers == 2)
		{
			if(north.isVisible())
			{
				return;
			}
			else if(east.isVisible())
			{
				east.setVisible(false);
				north.setName(east.getName());
				north.setPurse(east.getPurse());
				mAIPanels.set(1, mAIPanels.get(2));
				north.setGUIProfilePanel(new GUIProfilePanel(north.getName()));
				north.setProfileIcon(east.getProfileIcon());
				north.setVisible(true);
				east.setName("");
			}
			else if(west.isVisible())
			{
				west.setVisible(false);
				north.setName(west.getName());
				north.setPurse(west.getPurse());
				mAIPanels.set(1, mAIPanels.get(0));
				north.setGUIProfilePanel(new GUIProfilePanel(north.getName()));
				north.setProfileIcon(west.getProfileIcon());
				north.setVisible(true);
				north.updateUI();
				west.setName("");
			}
		}
		
		this.updateUI();
	}
	
	/**
	 * Handles GameModel telling us someone was removed. 
	 * @param action
	 * @throws GUIGameEndedException thrown when the user was removed or when the user won
	 */
	private void handlePlayerRemovedAction(PlayerRemovedAction action) throws GUIGameEndedException
	{
		if(action.getActionMaker().equals(mUserName))
		{
			if(mAutoSaver != null)
				mAutoSaver.closeFile();
			handleUserLostPrematurely();
			System.out.println("Handle died");
			throw new GUIGameEndedException("");
		}
		
		confirmUserReady( (action).getActionMaker() + " was just KO'd.", 
		"So what?", false);
	
		if(action.getActionMaker().equals(west.getName()))
			west.setVisible(false);
		
		else if(action.getActionMaker().equals(north.getName()))
			north.setVisible(false);
		
		else if(action.getActionMaker().equals(east.getName()))
			east.setVisible(false);

		mNumPlayers--;
		
		// Game ended and the user won
		if(mNumPlayers == 1)
		{
			if(mAutoSaver != null)
				mAutoSaver.closeFile();
			handleUserWonGame();
			throw new GUIGameEndedException(mPlayerWinner);
		}
		
		// otherwise arrange the panels in a s-n fashion
		rearrangePanels();
	}
	
	/**
	 * Displays the action in the "speech" panels for the AI or user
	 */
	private void handleGeneralAction(Action action)
	{
		if(mState == clearState.CLEAR_SCREEN)
			mCentre.resetAll();
		
		String actionMaker = action.getActionMaker();
		
		if(actionMaker.equals(mUserName))
			mCentre.setSouthString(action.toString());
			
		else if(actionMaker.equals(west.getName()))
			mCentre.setEastString(action.toString());
		
		else if(actionMaker.equals(north.getName()))
			mCentre.setNorthString(action.toString());
		
		else if(actionMaker.equals(east.getName()))
			mCentre.setWestString(action.toString());
		
	}
	
	/**
	 * Displays the bet in the "speech panels" for the user or AI
	 */
	private void handleBetAction(BetAction lAction)
	{
		if(mState == clearState.CLEAR_SCREEN)
			mCentre.resetAll();
		
		String actionMaker = lAction.getActionMaker();
		
		if(actionMaker.equals(mUserName))
			mCentre.setSouthPanel(lAction.toString(), getBetActionColor(lAction));
			
		else if(actionMaker.equals(west.getName()))
			mCentre.setEastPanel(lAction.toString(), getBetActionColor(lAction));
		
		else if(actionMaker.equals(north.getName()))
			mCentre.setNorthPanel(lAction.toString(), getBetActionColor(lAction));
		
		else if(actionMaker.equals(east.getName()))
			mCentre.setWestPanel(lAction.toString(), getBetActionColor(lAction));
	}
	
	/**
	 * Displays the Fold appropriately
	 */
	private void handleFold(Action action)
	{
		if(action.getActionMaker().equals(mUserName))
		{
			mUserPanel.foldHand();
			mCentre.resetSouth();
		}
		else if(action.getActionMaker().equals(north.getName()))
		{
			north.foldHand();
			mCentre.resetNorth();
		}	
		else if(action.getActionMaker().equals(east.getName()))
		{
			east.foldHand();
			mCentre.resetWest();		
		}
		else if(action.getActionMaker().equals(west.getName()))
		{
			west.foldHand();
			mCentre.resetEast();
		}
	}
	
	/**
	 * Returns the border color of the bet action (raise, all-in : red, rest clear)
	 */
	private Color getBetActionColor(BetAction lAction)
	{
		BetAction.betAction mAction = lAction.getAction();
		
		if(mAction == BetAction.betAction.RAISE)
			return Color.RED;
		else if(mAction == BetAction.betAction.ALL_IN)
			return Color.RED;	
		else
			return Constants.TRANSPARENT;
	}
	
	/**
	 * As always
	 */
	private void handleGetMoneyAction(GetMoneyAction action)
	{
		int amount = action.getAmount();;
		String playerID = action.getActionMaker();
		
		// update the user purse
		if(playerID.equals(mUserName))
			mUserPanel.addToPurse(amount); 
		
		// update an AI purse
		else
		{
			int i;
			
			// scan for which purse
			for(i = 0; i < mAINames.size(); i++)
				if(mAINames.get(i) != null)
					if(mAINames.get(i).equals(playerID))
						break; 
			
			mAIPanels.get(i).addToPurse(amount);  
		}
	}
	
	/**
	 * As always
	 */
	private void handleScoreAction(ScoreAction action)
	{
		int amount = action.getAmount();;
		String playerID = action.getActionMaker();
		
		// update the user purse
		if(playerID.equals(mUserName))
			mUserPanel.setPurse(amount); 
		
		// update an AI purse
		else
		{
			if(east.getName().equals(playerID))
				east.setPurse(amount);
			
			if(north.getName().equals(playerID))
				north.setPurse(amount);
			
			if(west.getName().equals(playerID))
				west.setPurse(amount);
		}
	}
	
	/**
	 * Ditto
	 */
	private void handleIncrementAnteAction(IncrementAnteAction action)
	{
		if(action.getActionMaker().equals(mUserName))
			this.mUserPanel.setDealerTrue();
		else if(action.getActionMaker().equals(west.getName()))
			west.setDealerTrue();
		else if(action.getActionMaker().equals(east.getName()))
			east.setDealerTrue();
		else if(action.getActionMaker().equals(north.getName()))
			north.setDealerTrue();
	}
	
	/**
	 * Yep.
	 */
	private void handleScoreRevealAction(ScoreRevealAction action)
	{
		try
		{
			if(mState == clearState.CLEAR_SCREEN)
				mCentre.resetAll();
			
			mCentre.setPotAndWinningHand(false, true);
			
			Hand temp = action.getHand();
			
			if(action.getActionMaker().equals(north.getName()))
			{
				GUIVerticalHand h = new GUIVerticalHand();
				h.setHand(temp);
				h.fixAllCards();
				h.revealAllCards();
				north.setGUIHand(h);
				mCentre.resetNorth();
			}
			else
			{
				GUIHorizontalHand h = new GUIHorizontalHand();
				h.setHand(temp);
				h.revealAllCards();
				h.fixAllCards();
				
				if(action.getActionMaker().equals(west.getName()))
				{
					west.setGUIHand(h);
					mCentre.resetEast();
				}
				else if(action.getActionMaker().equals(east.getName()))
				{
					east.setGUIHand(h);
					mCentre.resetWest();
				}
			}
		}
		catch(HandFoldedException e)
		{
			handleGeneralAction(action);
		}
		catch(Exception e)
		{
			e.printStackTrace();
			assert false;
		}
	}
	
	/**
	 * Puts up a button and message waiting for user to click on button. If wOpToQuit = true
	 * another button giving "quit" option is also presented. This button will quit the game
	 * if pressed. It is only presented before matches to avoid GameModel confusion. 
	 */
	private void confirmUserReady(String Question, String Message, boolean wOpToQuit)
	{
		mButtonPanel.resetCommAction();
		mButtonPanel.loadConfirmationPanel(Question, Message, wOpToQuit);
		mButtonPanel.grabFocus();
		while(mButtonPanel.getCommAction() == null);
		mButtonPanel.loadEmptyPanel();
		
		if(wOpToQuit)
		{
			if(mButtonPanel.wasQuitSelected())
				mIsGameOver = true;
		}
	}
	
	/**
	 * Displays the winner of the pot as embodied by the action
	 */
	private void displayPotWinner(GetMoneyAction action)
	{
		mCentre.setPotAndWinningHand(false, true);
		
		if(action.getPot() == 1)
		{
			mButtonPanel.resetCommAction();
			mButtonPanel.loadConfirmationPanel("", "Ok!", false);
			mButtonPanel.grabFocus();
			while(mButtonPanel.getCommAction() == null);
			mButtonPanel.loadEmptyPanel();
		}
	}
	
	public final void setSmallIcon(String s)
	{
		smallIcon = new ImageIcon(s);
		smallIconName = s;
	}
	
	public final void setBigIcon(String s)
	{
		bigIcon = new ImageIcon(s);
		bigIconName = s;
	}
	
	public ImageIcon getSmallIcon()
	{
		return smallIcon;
	}
	
	public ImageIcon getBigIcon()
	{
		return bigIcon;
	}
	
	public String getBigIconName()
	{
		return bigIconName;
	}
	
	public String getSmallIconName()
	{
		return smallIconName;
	}
	
	public void setTournamentRound(int i)
	{
		mGameModel.setTournamentRound(i);
	}
	
	public void setDifficultySetting(int i)
	{
		mGameModel.setDifficultySetting(i);
	}
	
	public int getTournamentRound()
	{
		return mGameModel.getTournamentRound();
	}
	
	public int getDifficultySetting()
	{
		return mGameModel.getDifficultySetting();
	}
}
