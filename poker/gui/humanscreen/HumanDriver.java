package gui.humanscreen;

import gui.GUIDriver;

import javax.swing.JFrame;

public class HumanDriver extends JFrame{

	
	public static void main(String [] args)
	{
		HumanScreen h = new HumanScreen("MEMEMEME");
		BigIconPanel b = new BigIconPanel("Hasselhoff.gif");
		
		HumanDriver g = new HumanDriver();
		g.setTitle("Gen and David");
	//	g.setPreferredSize(new Dimension(160, 70));
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(h);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);

	}
	
	public HumanDriver()
	{
		super();
	}
}
