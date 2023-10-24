package images;

import gui.GUIGameScreen;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.util.ArrayList;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.border.Border;

import util.Card;

public class Constants {
	
	
	public static ImageIcon Hasselhoff;
	public static ImageIcon HasselhoffT;
	public static ImageIcon Tyson;
	public static ImageIcon TysonT;
	public static ImageIcon Robespierre;
	public static ImageIcon RobespierreT;
	public static ImageIcon Giuliani;
	public static ImageIcon GiulianiT;
	public static ImageIcon Lechiffre;
	public static ImageIcon LechiffreT;
	public static ImageIcon Marx;
	public static ImageIcon MarxT;
	public static ImageIcon Trotsky;
	public static ImageIcon TrotskyT;
	public static ImageIcon Guevara;
	public static ImageIcon GuevaraT;
	public static ImageIcon Ivy;
	public static ImageIcon IvyT;
	
	public Constants()
	{
		
	}
	
	private static ArrayList<ImageIcon> mCards     = new ArrayList<ImageIcon>();
	private static String mString = new String("src"+java.io.File.separator+"images"+java.io.File.separator);
	
	public static final String TITLE = "Poker Fighter";
	
	public static final String SRC = "src"+java.io.File.separator+"images"+java.io.File.separator+"player"+java.io.File.separator;
	
	public static final Color TRANSPARENT   = new Color(50, 205, 50);
	
	public static final Color FOLD_COLOR    = new Color(50, 50, 50, 85);
	
	public static final Border BORDER       = BorderFactory.createEtchedBorder();
	
	public static final Border GUI_BORDER   = BorderFactory.createLineBorder(new Color(145, 145, 145, 88), 1);
	
	public static final Border EMPTY_BORDER = BorderFactory.createEmptyBorder();
	
	public static final Dimension BUTTON_SIZE = new Dimension(92, 25);
	
	public static final Dimension POT_LABEL_SIZE = new Dimension(400, 40);
	
	public static final Font FONT = new Font("Monospaced", Font.BOLD, 12);
	
	public static final Dimension BET_PANEL_SIZE = new Dimension(400, 70);
	
	public static final Dimension SPACER_SIZE = new Dimension(34, 34);
	
	public static final Dimension GUI_POT_SIZE = new Dimension(188, 176);
	
	public static final int H_GAP_SIZE = 11;
	
	public static final int V_GAP_SIZE = 11; 
	
	public static final String EMPTY_STRING = "          ";
	
	public static final Dimension GAME_SIZE = new Dimension(1120, 900);
	
	public ImageIcon getImageIconFromString(String t)
	{
		String s = java.io.File.separator;
		String FileString = "images"+s+"player"+s+t;
		
		ImageIcon i= new ImageIcon(Constants.class.getClassLoader().getResource(FileString));

		return i;
	}
	
	public ImageIcon getCardFromString(String t)
	{
		String s = java.io.File.separator;
		String FileString = "images"+t;
		
		ImageIcon i= new ImageIcon(Constants.class.getClassLoader().getResource(FileString));

		return i;
	}
	
	public final void initializeAllCards()
	{
		String RankString = "23456789tjqka";
		String SuitString = "cdhs";
		String FileString = "";
		
		for (int i = 0; i < RankString.length(); i++)
		{
			for (int j = 0; j < SuitString.length(); j++)
			{
				FileString = "";
				FileString += RankString.substring(i, i+1) + SuitString.substring(j, j+1);
				java.net.URL img = getClass().getResource(FileString + ".gif");
				mCards.add(new ImageIcon(img));
			}
		}
		
		java.net.URL img = getClass().getResource("b.gif");
		mCards.add(new ImageIcon(img));
	}
	
	public static ImageIcon getBackImageIcon()
	{
		return mCards.get(mCards.size() - 1);
	}
	
	public static ImageIcon getImageIcon(Card pCard)
	{
		return mCards.get(pCard.getRank().ordinal()*4 + pCard.getSuit().ordinal());
	}
	
	public static int NFile = GUIGameScreen.NO_SAVE;
	public static int AFile = GUIGameScreen.SLOT_ONE;
	public static int BFile = GUIGameScreen.SLOT_TWO;
	public static int CFile = GUIGameScreen.SLOT_THREE;
}
