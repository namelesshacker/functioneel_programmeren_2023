package gui.tournament;

import images.Constants;

import java.awt.Dimension;
import java.awt.Graphics;


import javax.swing.ImageIcon;
import javax.swing.JPanel;

public class TournCupPanel extends JPanel{
	private ImageIcon img;
	private Constants c = new Constants();
	public TournCupPanel()
	{
		super();
		this.setPreferredSize(new Dimension(200, 270));
		img = c.getImageIconFromString("Cup.gif");
	}
	
	public final void paintComponent(Graphics g)
	{
		img.paintIcon(this, g, (this.getSize().width-img.getIconWidth())/2, (this.getSize().height-img.getIconHeight())/2);
	}
}
