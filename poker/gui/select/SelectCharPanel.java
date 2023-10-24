package gui.select;


import images.Constants;

import java.awt.Color;
import java.awt.Graphics;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 *
 */

public class SelectCharPanel extends JButton{
	
	private final int MAX_CHARS=3;
	
	private String bigFilename;
	private String name;
	private ImageIcon img;
	private boolean isSelected;
	private boolean increment;
	private int counter;
	private Constants c = new Constants();
	public SelectCharPanel(String name, String thumbFilename, String bigFilename)
	{
		super();
		this.setBackground(Constants.TRANSPARENT);
		this.setPreferredSize(SelectConstants.CHAR_SIZE);
		
		this.bigFilename = bigFilename;
		
		this.name = name;
		
		img = c.getImageIconFromString(thumbFilename);
		isSelected = false;
		increment = true;
		counter =0;
	}

	
	public void paintComponent(Graphics g)
	{
		// Draw the icon 
		g.setColor(Constants.TRANSPARENT);
		g.fillRect(0, 0, this.getSize().width, this.getSize().height);
		img.paintIcon(this, g, 0, 0);
		
		// Draw the gray box if selected
		if(isSelected)
		{
			g.setColor(SelectConstants.SELECT);
			g.drawRect(0, 0, img.getIconWidth(), img.getIconHeight());
		}
		
		// Draw the name
		g.setColor(Color.black);
		g.setFont(Constants.FONT);
		g.drawString(name, img.getIconWidth()+10, img.getIconHeight()*2/5);
		
		// Draw the circles
/*		for(int i = 0, x = img.getIconWidth()+10, y = img.getIconHeight()*2/3; i<MAX_CHARS; i++, x+=30)
		{
			g.drawOval(x, y, 10, 10);
		}
	*/	for(int i = 0, x = img.getIconWidth() + 10, y = img.getIconHeight()*3/5; i < counter; i++, x+=30)
		{
			g.fillOval(x, y, 10, 10);
		}
	}
	
	public final boolean isSelected()
	{
		return isSelected;
	}
	
	public final void incrementCircles()
	{
		counter++;
	}
	
	public final void decrementCircles()
	{
		counter--;
	}
	
	public final boolean isIncrementing()
	{
		return increment;
	}
	
	public final void unselectThis()
	{
		isSelected = false;
	}
	
	public final String getBigFilename()
	{
		return bigFilename;
	}
}
