package gui;

import images.Constants;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class GUICentrePanel extends JPanel {
	
	private JPanel mSpacerNorth, mSpacerSouth;
	private JPanel mSpacerWest, mSpacerEast;
	private JLabel mWest, mNorth, mEast, mSouth;
	private JPanel mCentre;
	private JPanel mPot;
	private JPanel mWinningHand;
	private BorderLayout x;
	
	public GUICentrePanel(GUIPot pot, GUIWinningHand gwh, int x, int y)
	{
		super();
		
		this.x = new BorderLayout();
		this.x.setHgap(2);
		this.x.setVgap(2);
		
		this.setLayout(this.x);
		
		this.setPreferredSize(new Dimension(x, y));

		JPanel centre = new JPanel();
		centre.setBackground(Constants.TRANSPARENT);
		
		mWest   = new JLabel(Constants.EMPTY_STRING, JLabel.CENTER);
		mEast   = new JLabel(Constants.EMPTY_STRING, JLabel.CENTER); 
		mNorth  = new JLabel(Constants.EMPTY_STRING, JLabel.CENTER);
		mSouth  = new JLabel(Constants.EMPTY_STRING, JLabel.CENTER);
		mCentre = new JPanel(new BorderLayout());
		mSpacerNorth = new JPanel();
		mSpacerSouth = new JPanel();
		mSpacerWest = new JPanel();
		mSpacerEast = new JPanel();
		
		mPot = pot;
		mWinningHand = gwh;
		
		mCentre.setVisible(true);
		mWinningHand.setVisible(false);
		
		mCentre.add(mSpacerNorth, BorderLayout.NORTH);
		mCentre.add(mPot, BorderLayout.CENTER);
		mCentre.add(mSpacerSouth, BorderLayout.SOUTH);
		mCentre.add(mSpacerWest, BorderLayout.WEST);
		mCentre.add(mSpacerEast, BorderLayout.EAST);
		
		mWest.setBackground(Constants.TRANSPARENT);
		mNorth.setPreferredSize(new Dimension(500, 30));
		mEast.setBackground(Constants.TRANSPARENT);
		mNorth.setBackground(Constants.TRANSPARENT);
		mSouth.setBackground(Constants.TRANSPARENT);
		mCentre.setBackground(Constants.TRANSPARENT);
		mSpacerNorth.setBackground(Constants.TRANSPARENT);
		mSpacerNorth.setPreferredSize(Constants.SPACER_SIZE);
		mSpacerSouth.setBackground(Constants.TRANSPARENT);
		mSpacerSouth.setPreferredSize(Constants.SPACER_SIZE);
		mSpacerEast.setBackground(Constants.TRANSPARENT);
		mSpacerWest.setBackground(Constants.TRANSPARENT);
		mSpacerEast.setPreferredSize(Constants.SPACER_SIZE);
		mSpacerWest.setPreferredSize(Constants.SPACER_SIZE);
		
		mCentre.setAlignmentX(JPanel.CENTER_ALIGNMENT);
		mCentre.setAlignmentY(JPanel.CENTER_ALIGNMENT);
		
		mWest.setFont(Constants.FONT);
		mEast.setFont(Constants.FONT);
		mSouth.setFont(Constants.FONT);
		mNorth.setFont(Constants.FONT);
		
		mWest.setPreferredSize(Constants.BUTTON_SIZE);
		mEast.setPreferredSize(Constants.BUTTON_SIZE);
		mSouth.setPreferredSize(Constants.BUTTON_SIZE);
		mNorth.setPreferredSize(Constants.BUTTON_SIZE);
		
		centre.add(mWinningHand);
		centre.add(mCentre);

		centre.setPreferredSize(new Dimension(600, 450));
		
		this.add(mWest,  BorderLayout.WEST); 
		this.add(mEast,  BorderLayout.EAST);
		this.add(mNorth, BorderLayout.NORTH);
		this.add(mSouth, BorderLayout.SOUTH);
		this.add(centre, BorderLayout.CENTER); 
		this.setBackground(Constants.TRANSPARENT);
		
		this.updateUI();
	}
	
	public void setPotAndWinningHand(boolean Pot, boolean WH)
	{
		mCentre.setVisible(Pot);
		mWinningHand.setVisible(WH);
	}
	
	public void setEastString(String message)
	{
		mEast.setBorder(Constants.BORDER);
		mEast.setText(message);
		mEast.updateUI();
	}
	
	public void setEastPanel(String message, Color color)
	{
		mEast.setBackground(color);
		setEastString(message);
	}
	
	public void setWestString(String message)
	{
		mWest.setBorder(Constants.BORDER);
		mWest.setText(message);
		mWest.updateUI();
	}
	
	public void setWestPanel(String message, Color color)
	{
		mWest.setBackground(color);
		setWestString(message);
	}
	
	public void setNorthString(String message)
	{
		mNorth.setBorder(Constants.BORDER);
		mNorth.setText(message);
		mNorth.updateUI();
	}
	
	public void setNorthPanel(String message, Color color)
	{
		mNorth.setBackground(color);
		setNorthString(message);
	}
	
	public void setSouthString(String message)
	{
		mSouth.setBorder(Constants.BORDER);
		mSouth.setText(message);
		mSouth.updateUI();
	}
	
	public void setSouthPanel(String message, Color color)
	{
		mSouth.setBackground(color);
		setSouthString(message);
	}
	
	public void resetSouth()
	{
		mSouth.setText(Constants.EMPTY_STRING);
		
		if(!mSouth.getText().contains("All in!"))
		{
			mSouth.setBorder(Constants.EMPTY_BORDER);
			mSouth.setBackground(Constants.TRANSPARENT);
		}
		
		mSouth.updateUI();
	}
	
	public void resetEast()
	{
		mEast.setText(Constants.EMPTY_STRING);
		
		if(!mEast.getText().toLowerCase().contains("all in"))
		{
			mEast.setBorder(Constants.EMPTY_BORDER);
			mEast.setBackground(Constants.TRANSPARENT);
		}
		
		mEast.updateUI();
	}
	
	public void resetNorth()
	{
		mNorth.setText(Constants.EMPTY_STRING);
		
		if(!mNorth.getText().toLowerCase().contains("all in"))
		{
			mNorth.setBorder(Constants.EMPTY_BORDER);
			mNorth.setBackground(Constants.TRANSPARENT);
		}
		
		mNorth.updateUI();
	}
	
	public void resetWest()
	{
		mWest.setText(Constants.EMPTY_STRING);
		
		if(!mWest.getText().toLowerCase().contains("all in"))
		{
			mWest.setBorder(Constants.EMPTY_BORDER);
			mWest.setBackground(Constants.TRANSPARENT);
		}
		
		mWest.updateUI();
	}
	
	public void resetAll()
	{
		resetSouth();
		resetNorth();
		resetWest();
		resetEast();
	}
}
