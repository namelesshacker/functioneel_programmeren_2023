package gui.select;

import images.Constants;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;

import javax.swing.ImageIcon;
import javax.swing.JPanel;

public class SelectBigIconPanel extends JPanel{

	private final int MAX = 4;
	private final int INCREMENTER = 165;
	
	private ArrayList<ImageIcon> imgs;
	private ArrayList<String> filenames;
	private Constants c = new Constants();
	
	public SelectBigIconPanel()
	{
		super();
		this.setPreferredSize(SelectConstants.BIG_SIZE);
		
		imgs = new ArrayList<ImageIcon>();
		filenames = new ArrayList<String>();
	}
	
	public final void addIcon(String filename)
	{
		if(imgs.size() >= MAX)
			assert false;
		else
		{
			ImageIcon img = c.getImageIconFromString(filename);
			imgs.add(img);
			filenames.add(filename);
		}
		this.updateUI();
	}
	
	public final boolean removeIcon(String filename)
	{
		boolean result = false;
		
		// the first icon is always the user
		for(int i = 1; i < filenames.size(); i++)
			if(filenames.get(i).equals(filename))
			{
				filenames.remove(i);
				imgs.remove(i);
				result = true;
				break;
			}
		this.updateUI();
		return result;
	}
	
	public final void paintComponent(Graphics g)
	{	
		for(int i=0, x=45; i < imgs.size(); i+=2, x+=INCREMENTER*2)
			imgs.get(i).paintIcon(this, g, x, 0);
		
		for(int i=1, x=45+INCREMENTER; i< imgs.size(); i+=2, x+=INCREMENTER*2)
			imgs.get(i).paintIcon(this, g, x, 0);
	}
	
	public final ArrayList<String> getFilenames()
	{
		return filenames;
	}
}
