package gui.start;

import images.Constants;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;


import javax.swing.ImageIcon;
import javax.swing.JPanel;

public class GenAndDave extends JPanel{

	ImageIcon gen, dave;
	
	public GenAndDave()
	{
		super();
		this.setPreferredSize(new Dimension(604, 225));
		
		Constants c = new Constants();
		
		gen = c.getImageIconFromString("gen.jpg");
		dave = c.getImageIconFromString("dave.jpg");
	}
	
	public void paintComponent(Graphics g)
	{
		Graphics2D g2 = (Graphics2D) g;
		g2.scale(0.5, 0.5);
		
		gen.paintIcon(this, g, 0, 0);
		dave.paintIcon(this, g, gen.getIconWidth(), 0);
	}
}
