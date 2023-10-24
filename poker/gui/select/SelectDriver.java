package gui.select;

import javax.swing.JFrame;

import players.GUIHumanPlayer;

public class SelectDriver extends JFrame {

	public static void main(String[]args)
	{
	//	SelectCharPanel s= new SelectCharPanel("src/images/player/GuevaraT.gif", "src/images/player/Guevara.gif");
		
		SelectBigIconPanel b = new SelectBigIconPanel();
		b.addIcon("src/images/player/Guevara.gif");
		b.addIcon("src/images/player/Lechiffre.gif");
		b.addIcon("src/images/player/Hasselhoff.gif");
		b.addIcon("src/images/player/Trotsky.gif");

		SelectCharactersScreen sc = new SelectCharactersScreen(new GUIHumanPlayer("sdf", 100, "src/images/player/Guevara.gif", "src/images/player/GuevaraT.gif"));
		
		
		
		
		SelectDriver g = new SelectDriver();
		g.setTitle("Gen and David");
	//	g.setPreferredSize(new Dimension(160, 70));
		g.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		g.add(sc);
		g.pack();
		g.setLocationRelativeTo(null);
		g.setVisible(true);
	}
	
	public SelectDriver()
	{
		super();
	}
}
