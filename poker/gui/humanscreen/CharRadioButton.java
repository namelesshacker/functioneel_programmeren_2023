package gui.humanscreen;

import images.Constants;

import javax.swing.*;
import java.awt.*;


public class CharRadioButton extends JRadioButton{

	public CharRadioButton(String name, Icon icon)
	{
		super(name, icon);
	}
	
	public void paintComponent(Graphics g)
	{
		super.paintComponent(g);
		
		if(this.isSelected())
		{
			g.setColor(Constants.FOLD_COLOR);
			g.fillRect(0, 0, this.getSize().width, this.getSize().height);
		}
	}
	
	
}
