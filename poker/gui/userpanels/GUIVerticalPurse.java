package gui.userpanels;


import java.awt.BorderLayout;
import java.awt.Dimension;

import javax.swing.JProgressBar;

public class GUIVerticalPurse extends GUIPurse{
	
	public GUIVerticalPurse(int startingAmount)
	{
		super(startingAmount, new BorderLayout());
	}
	
	public final void constructorHelper()
	{	
		bar = new JProgressBar(JProgressBar.VERTICAL, 0, START);
		bar.setPreferredSize(new Dimension(20, 190));
		
		this.add(label, BorderLayout.PAGE_START);
		this.add(bar, BorderLayout.CENTER);
	}

}
