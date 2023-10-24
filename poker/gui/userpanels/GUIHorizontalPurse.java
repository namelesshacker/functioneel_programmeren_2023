package gui.userpanels;


import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;

import javax.swing.JProgressBar;

public class GUIHorizontalPurse extends GUIPurse{
	public GUIHorizontalPurse(int startingAmount)
	{
		super(startingAmount, new FlowLayout());	
	}
	
	public final void constructorHelper()
	{	
		bar = new JProgressBar(JProgressBar.HORIZONTAL,0, START);
		bar.setPreferredSize(new Dimension(190, 20));
		this.add(bar);
		this.add(label);
	}
}
