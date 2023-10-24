package gui.userpanels;


import images.Constants;

import java.awt.GridLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class GUIProfilePanel extends JPanel {
	
	private final JLabel lLabel;
	public GUIProfilePanel(String name)
	{
		super();
		this.setBackground(Constants.TRANSPARENT);
		lLabel = new JLabel(name);
		lLabel.setFont(Constants.FONT);
		this.add(lLabel);
	}
}
