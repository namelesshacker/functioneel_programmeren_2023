package gui.userpanels;

import images.Constants;

import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.ImageIcon;
import javax.swing.JPanel;

public class GUIProfileIcon extends JPanel{

	private ImageIcon img;
	private Constants c = new Constants();
	
	public GUIProfileIcon(String filename)
	{
		super();
		this.setPreferredSize(new Dimension(27,27));
		img = c.getImageIconFromString(filename);
	}
	
	public final void setIcon(String s)
	{
		img = c.getImageIconFromString(s);
	}
	
	public final void paintComponent(Graphics g)
	{		
		Graphics2D g2 = (Graphics2D) g;
		g2.scale(0.25, 0.25);
		img.paintIcon(this, g, 0, 0);
	}
}
