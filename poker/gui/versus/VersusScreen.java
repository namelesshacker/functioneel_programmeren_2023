package gui.versus;


import images.Constants;

import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;

import players.Player;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 */
public class VersusScreen extends JPanel{

	private ArrayList<ImageIcon> imgs;
	private Constants c = new Constants();
	
	public VersusScreen(ArrayList<Player> p)
	{
		super();
		this.setPreferredSize(Constants.GAME_SIZE);
		imgs = new ArrayList<ImageIcon>();
		for(int i=0; i<p.size(); i++)
			imgs.add(c.getImageIconFromString(p.get(i).getBigIconName()));
	}
	
	public void paintComponent(Graphics g)
	{
		// set background
		g.setColor(Constants.TRANSPARENT);
		g.fillRect(0, 0, this.getPreferredSize().width+100, this.getPreferredSize().height+100);
		
		//draw the images
		imgs.get(0).paintIcon(this, g, 20, 20);
		imgs.get(1).paintIcon(this, g, this.getPreferredSize().width-imgs.get(1).getIconWidth()-20,20);
		imgs.get(2).paintIcon(this, g, 20, this.getPreferredSize().height-imgs.get(2).getIconHeight()-20);
		imgs.get(3).paintIcon(this, g, 
							  this.getPreferredSize().width-imgs.get(3).getIconWidth()-20, 
							  this.getPreferredSize().height-imgs.get(3).getIconHeight()-20);
		
		//draw the "VS" in the center
		g.setFont(new Font("Monospaced", Font.BOLD, 200));
		g.setColor(Color.yellow);
		g.drawString("VS", this.getPreferredSize().width/2-100, this.getPreferredSize().height/2+100);
	}
	
}
