package gui.start;

import images.Constants;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import gui.GUIGameScreen;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

public class StartScreen extends JPanel implements ActionListener, KeyListener{

	private final int MAX_NAME_SIZE = 19;
	
	private JLabel in;
	private JLabel load;
	private JLabel msg;
	private JLabel lmsg;
	private JButton ok;
	private JButton okLoad;
	private JTextField input;

	private JRadioButton laButton;
	private JRadioButton lbButton;
	private JRadioButton lcButton;
	private JRadioButton ltButton;
	private ButtonGroup group;
	
	private String 	name;
	private final int NOT_SELECTED = GUIGameScreen.NO_LOAD;
	private int 	file = NOT_SELECTED;
	
	private boolean done;
	
	public StartScreen()
	{
		super(new BorderLayout());
		this.setBackground(Constants.TRANSPARENT);
		this.setPreferredSize(Constants.GAME_SIZE);
		
		done = false;
		
		in = new JLabel("Enter your Name:");
		in.setFont(Constants.FONT);
		in.setHorizontalAlignment(JLabel.CENTER);
		
		load = new JLabel("or Load a Game:");
		load.setFont(Constants.FONT);
		load.setHorizontalAlignment(JLabel.CENTER);
		
		msg = new JLabel("Enter a valid name!");
		msg.setFont(Constants.FONT);
		msg.setVisible(false);
		msg.setHorizontalAlignment(JLabel.CENTER);
		
		lmsg = new JLabel("Select a file to load!");
		lmsg.setFont(Constants.FONT);
		lmsg.setVisible(false);
		lmsg.setHorizontalAlignment(JLabel.CENTER);
		
		ok = new JButton("Play!");
		ok.setFont(Constants.FONT);
		ok.setBackground(Constants.TRANSPARENT);
		ok.addActionListener(this);
		ok.setActionCommand("OK");
		
		okLoad = new JButton("Load!");
		okLoad.setFont(Constants.FONT);
		okLoad.setBackground(Constants.TRANSPARENT);
		okLoad.addActionListener(this);
		okLoad.setActionCommand("LOAD");
		okLoad.setPreferredSize(Constants.BUTTON_SIZE);
		
		input = new JTextField(10);
		input.setFont(Constants.FONT);
		input.addKeyListener(this);
		
	    laButton = new JRadioButton("Arcade Match 1");
	    laButton.setFont(Constants.FONT);
	    laButton.setBackground(Constants.TRANSPARENT);
	    laButton.setActionCommand("a");
	    laButton.setSelected(false);

	    lbButton = new JRadioButton("Arcade Match 2");
	    lbButton.setFont(Constants.FONT);
	    lbButton.setBackground(Constants.TRANSPARENT);
	    lbButton.setActionCommand("b");
	    lbButton.setSelected(false);

	    lcButton = new JRadioButton("Arcade Match 3");
	    lcButton.setFont(Constants.FONT);
	    lcButton.setBackground(Constants.TRANSPARENT);
	    lcButton.setActionCommand("c");
	    lcButton.setSelected(false);
	    
	    ltButton = new JRadioButton("Tournament Match 1");
	    ltButton.setFont(Constants.FONT);
	    ltButton.setBackground(Constants.TRANSPARENT);
	    ltButton.setActionCommand("t");
	    ltButton.setSelected(false);

	    //Group the radio buttons.
	    group = new ButtonGroup();
	    group.add(laButton);
	    group.add(lbButton);
	    group.add(lcButton);
	    group.add(ltButton);

	    //Register a listener for the radio buttons.
	    laButton.addActionListener(this);
	    lbButton.addActionListener(this);
	    lcButton.addActionListener(this);
	    ltButton.addActionListener(this);
	    
		
	    JPanel ButtonPanel = new JPanel(new GridLayout(0,1));
	    ButtonPanel.add(laButton);
	    ButtonPanel.add(lbButton);
	    ButtonPanel.add(lcButton);
	    ButtonPanel.add(ltButton);
	    ButtonPanel.setBackground(Constants.TRANSPARENT);
	    
		JPanel center = new JPanel(new GridLayout(0,1));
		center.setBackground(Constants.TRANSPARENT);
		center.setPreferredSize(Constants.BET_PANEL_SIZE);
		
		JPanel inputOk = new JPanel(new FlowLayout());
		inputOk.setBackground(Constants.TRANSPARENT);
		inputOk.add(input);
		inputOk.add(ok);
		//inputOk.setBorder(Constants.BORDER);
		
		JPanel name = new JPanel(new GridLayout(0,1));
		name.setBorder(Constants.BORDER);
		name.setBackground(Constants.TRANSPARENT);
	    name.add(in);
	    name.add(inputOk);
	    name.add(msg);
		
		JPanel outputOK = new JPanel();
		outputOK.setBackground(Constants.TRANSPARENT);
		outputOK.add(ButtonPanel);
		outputOK.add(okLoad);
		
		center.add(name);
		
		JLabel title1 = new JLabel("Gen Kazama & David Kawrykow's");
		title1.setFont(new Font("Monospaced", Font.BOLD, 50));
		title1.setVerticalAlignment(title1.CENTER);
		title1.setHorizontalAlignment(JLabel.CENTER);
		
		JLabel title2 = new JLabel("Poker Fighter");
		title2.setFont(new Font("Monospaced", Font.BOLD, 120));
		title2.setVerticalAlignment(title2.CENTER);
		title2.setHorizontalAlignment(JLabel.CENTER);
		title2.setForeground(Color.red);
		
		JPanel north = new JPanel(new GridLayout(0,1));
		north.setBackground(Constants.TRANSPARENT);
		north.setPreferredSize(new Dimension(this.getSize().width, 300));
		north.add(title1);
		north.add(title2);
		
			
		JPanel south = new JPanel();
		south.setBackground(Constants.TRANSPARENT);
		south.setPreferredSize(new Dimension(this.getSize().width, 300));
		south.add(new GenAndDave());
		
		this.add(north, BorderLayout.NORTH);
		this.add(center, BorderLayout.CENTER);
		this.add(south, BorderLayout.SOUTH);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		if(e.getActionCommand().equals(ok.getActionCommand()))
		{
			name = input.getText();
			if(name.equals(""))
			{
				msg.setText("Enter a valid name!");
				msg.setVisible(true);
				input.setText("");
				input.grabFocus();
			}
			else if( name.length() > MAX_NAME_SIZE)
			{
				msg.setText("Enter a name less than 20 characters.");
				msg.setVisible(true);
				input.setText("");
				input.grabFocus();
			}
			else
			{
				file = GUIGameScreen.NO_LOAD;
				done = true;
			}
		}
		
		if(e.getActionCommand().equals(okLoad.getActionCommand()))
		{
			if(file == NOT_SELECTED)
			{
				lmsg.setText("Select a file to load!");
				lmsg.setVisible(true);
			}
			else
			{
				done = true;
			}
		}
		
		if(e.getActionCommand().equals(laButton.getActionCommand()))
			file = GUIGameScreen.SLOT_ONE;
		
		if(e.getActionCommand().equals(lbButton.getActionCommand()))
			file = GUIGameScreen.SLOT_TWO;
		
		if(e.getActionCommand().equals(lcButton.getActionCommand()))
			file = GUIGameScreen.SLOT_THREE;
		
		if(e.getActionCommand().equals(ltButton.getActionCommand()))
			file = GUIGameScreen.SLOT_FOUR;
		
	}
	
	public final void keyPressed(KeyEvent e)
	{
		if(e.getKeyCode() == KeyEvent.VK_ENTER)
		{
			name = input.getText();
			if(name.equals(""))
			{
				msg.setText("Enter a valid name!");
				msg.setVisible(true);
				input.setText("");
			}
			else if( name.length() > MAX_NAME_SIZE)
			{
				msg.setText("Enter a name less than 20 characters.");
				msg.setVisible(true);
				input.setText("");
			}
			else
			{
				done = true;
			}
		}	
	}
	
	public final void keyTyped(KeyEvent e)
	{	
	}
	
	public final void keyReleased(KeyEvent e)
	{	
	}
	
	public final boolean isDone()
	{
		return done;
	}
	
	public final String getName()
	{
		return name;
	}
	
	public int getFile()
	{
		return GUIGameScreen.NO_LOAD;
	}
}
