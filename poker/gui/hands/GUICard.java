package gui.hands;


import images.Constants;

import javax.swing.*;
import java.awt.*;
import util.*;


public abstract class GUICard extends JPanel{

	protected final String FOLD = "Fold";
	
	protected boolean revealed;
	protected boolean fixed;
	protected boolean selected;
	protected boolean folded;
	protected Card card;
	private String filename;
	
	protected ImageIcon img;
	protected JPanel    emptyPanel;
	
	protected boolean showCard;
	
	private Constants c = new Constants();
	
	public GUICard(Card card)
	{
		super();
		this.setBackground(Constants.TRANSPARENT);
		this.card = card;
		revealed = false;
		selected = false;
		String s=java.io.File.separator;

		
		filename = getFilename();
		img = c.getCardFromString("b.gif");
		
		emptyPanel = new JPanel();
		emptyPanel.setBackground(Constants.TRANSPARENT);
		
		showCard = true;
		
		// not folded
		folded = false;
	}
	
	private String getFilename()
	{
		int rank = card.getRank().ordinal()+2;
		int suit = card.getSuit().ordinal();
		
		String result = "";
		switch(rank)
		{
		case 2:
		case 3:
		case 4:
		case 5:
		case 6:
		case 7:
		case 8:
		case 9:
			result += Integer.toString(rank);
			break;
		case 10:
			result+="t";
			break;
		case 11:
			result+="j";
			break;
		case 12:
			result+="q";
			break;
		case 13:
			result+="k";
			break;
		case 14:
			result+="a";
			break;
		default:
			assert false;
		}
		
		switch(suit)
		{
		case 0:
			result += "c";
			break;
		case 1:
			result += "d";
			break;
		case 2:
			result += "h";
			break;
		case 3:
			result += "s";
			break;
		default:
			assert false;
		}
		result +=".gif";
		return result;
	}
	
	/**
	 * Only method that is overwritten
	 */
	public void paintComponent(Graphics g)
	{
		super.paintComponent(g);
	}

	public final void hideCard()
	{
		showCard = false; 
		
	}
	
	public final void showCard()
	{
		showCard = true; 
	}
	
	public final void revealCard()
	{
		this.revealed = true;
		img = c.getCardFromString(filename);
	}
	
	public final void unrevealCard()
	{
		this.revealed = false;
		img = c.getCardFromString("b.gif");
	}
	
	public final boolean isSelected()
	{
		return selected;
	}

	public final void fixCard()
	{
		fixed = true;
	}
	
	public final void unfixCard()
	{
		fixed = false;
	}
	
	public final Card getCard()
	{
		return card; 
	}
	
	public final void foldCard()
	{
		folded = true;
	}
	
	public final void unfoldCard()
	{
		folded = false;
	}
	
	public final boolean isFolded()
	{
		return folded;
	}
}
