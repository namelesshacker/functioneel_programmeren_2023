package gui.userpanels;

import game.GameListener;
import game.GameSubject;
import game.actions.Action;
import game.actions.NewMatchAction;
import images.Constants;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class GUIConfirmationPanel extends JPanel implements ActionListener, GameSubject {
	
	private final JLabel  OKQuestion; 
	private final JButton OKButton;
	private final JButton mQuitButton;
	private GameListener lListener;
	private final Action lAction; 
	private boolean wasQuitSelected = false;
	
	public GUIConfirmationPanel(String Question, String Answer)
	{
		super();
		this.setPreferredSize(Constants.BET_PANEL_SIZE);
	//	this.setBackground(Constants.TRANSPARENT);
		
		OKQuestion = new JLabel(Question);
		OKQuestion.setFont(Constants.FONT);
		OKButton = new JButton(Answer);
		OKButton.setFont(Constants.FONT);
		OKButton.setPreferredSize(Constants.BUTTON_SIZE);
		OKButton.setBackground(Constants.TRANSPARENT);
		OKButton.addActionListener(this);
		OKButton.setActionCommand("Ok");
		
		mQuitButton = new JButton();
		mQuitButton.setText("Quit");
		mQuitButton.setBackground(Constants.TRANSPARENT);
		mQuitButton.setFont(Constants.FONT);
		mQuitButton.setPreferredSize(Constants.BUTTON_SIZE);
		mQuitButton.addActionListener(this);
		mQuitButton.setActionCommand("Quit");
		mQuitButton.setVisible(false);
		
		lAction = new NewMatchAction("GUIConfirmationPanel");
		this.add(OKQuestion); 
		this.add(OKButton);
		this.add(mQuitButton);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		if(e.getActionCommand().equals(mQuitButton.getActionCommand()))
		{
			wasQuitSelected = true;
		}
		
		if(lListener != null)
		{
			lListener.notify(lAction);
		}	
	}
	
	public void attachGameListener(GameListener l)
	{
		lListener = l;
	}
	
	public void detachGameListener(GameListener l)
	{
		lListener = null;
	}
	
	public void notifyObservers(Action action)
	{
		// nothing
	}
	
	public void setQuestion(String Question)
	{
		OKQuestion.setText(Question);
	}
	
	public void setAnswer(String Answer)
	{
		OKButton.setText(Answer);
	}

	public void setOptionToQuit(boolean opToQuit)
	{
		mQuitButton.setVisible(opToQuit);
	}
	
	public boolean wasQuitSelected()
	{
		if(!mQuitButton.isVisible())
			return false;
		else
			return wasQuitSelected;
	}
}
