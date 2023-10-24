package gui.error;

import javax.swing.JFrame;

public class ErrorDriver extends JFrame{
	public static void main(String [] args)
	{
		ErrorScreen e = new ErrorScreen("GEn's error");
		ErrorDriver g = new ErrorDriver();
		g.setTitle("Gen and David");
	//	g.setPreferredSize(new Dimension(160, 70));
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(e);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);

	}
	
	public ErrorDriver()
	{
		super();
	}
}
