package gui.humanscreen;

import images.Constants;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.ImageIcon;
import javax.swing.JPanel;


public class BigIconPanel extends JPanel{
	
	private ImageIcon img;
	private Constants c = new Constants();
	
	public BigIconPanel(String filename)
	{
		super();
		this.setPreferredSize(new Dimension(350, 273));
		
		img = c.getImageIconFromString(filename);
	}
	
	public final void paintComponent(Graphics g)
	{
		g.setColor(Constants.TRANSPARENT);
		g.fillRect(0, 0, this.getSize().width, this.getSize().height);
		img.paintIcon(this, g, (this.getSize().width-img.getIconWidth())/2, 0);
	}
	
	public final void setFilename(String s)
	{
		System.out.println("Set filename: "+s);
		img = c.getImageIconFromString(s);
		this.updateUI();
	}
	
}
