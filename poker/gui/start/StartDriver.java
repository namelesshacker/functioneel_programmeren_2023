package gui.start;


import javax.swing.JFrame;

public class StartDriver extends JFrame{

	public static void main(String[] args)
	{
		StartScreen s = new StartScreen();
		
		StartDriver g = new StartDriver();
		g.setTitle("Gen and David");
	//	g.setPreferredSize(new Dimension(160, 70));
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(s);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);		
	}
	
	public StartDriver()
	{
		super();
	}
}
