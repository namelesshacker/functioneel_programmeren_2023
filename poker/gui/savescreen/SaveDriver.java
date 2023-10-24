package gui.savescreen;

import gui.GUIDriver;

import javax.swing.JFrame;

public class SaveDriver extends JFrame{

	
	public static void main(String [] args)
	{
		SaveScreen h = new SaveScreen(false);
		
		SaveDriver g = new SaveDriver();
		g.setTitle("Gen and David");
	//	g.setPreferredSize(new Dimension(160, 70));
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(h);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);

	}
	
	public SaveDriver()
	{
		super();
	}
}
