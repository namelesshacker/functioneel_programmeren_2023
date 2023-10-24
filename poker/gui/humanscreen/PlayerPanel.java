package gui.humanscreen;

import images.Constants;

import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.ImageIcon;
import javax.swing.JPanel;

public class PlayerPanel extends JPanel{
	
	private ImageIcon img;
	private String name;
	private Constants c = new Constants();
	
	public PlayerPanel(String name, String filename)
	{
		super();
		this.setPreferredSize(new Dimension(100,100));
	
		this.name = name;
		
		img = c.getImageIconFromString(filename);
	}
	
	public final void paintComponent(Graphics g)
	{
		img.paintIcon(this, g, 0, 0);
	}
	
	public final String getName()
	{
		return name;
	}
	
	public final ImageIcon getImg()
	{
		return img;
	}
}
