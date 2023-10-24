package gui.userpanels;

import images.Constants;

import java.awt.LayoutManager;
import javax.swing.*;

public abstract class GUIPurse extends JPanel{

	protected JProgressBar bar;
	protected JLabel label;
	protected final int START;
	private int moneyInPurse;
	
	public GUIPurse(int startingAmount, LayoutManager l)
	{
		super(l);
		this.setBackground(Constants.TRANSPARENT); 
		START = startingAmount;
		moneyInPurse=0;
		label = new JLabel("0");
		label.setFont(Constants.FONT);
		
		constructorHelper();
	}
	
	protected abstract void constructorHelper();

	public final void setPurse(int i)
	{
		moneyInPurse = i;
		bar.setValue(moneyInPurse);
		if(moneyInPurse < 10)
			label.setText(moneyInPurse+"  ");
		else if(moneyInPurse < 100)
			label.setText(moneyInPurse+" ");
		else
			label.setText(moneyInPurse+"");
			
		this.updateUI();
	}
	
	public final void addToPurse(int i)
	{
		moneyInPurse += i;
		bar.setValue(moneyInPurse);
		
		if(moneyInPurse < 10)
			label.setText(moneyInPurse+"  ");
		else if(moneyInPurse < 100)
			label.setText(moneyInPurse+" ");
		else
			label.setText(moneyInPurse+"");
		
		this.updateUI();
	}
	
	public final int getChips()
	{
		return moneyInPurse;
	}
}
