package gui.tournament;


import images.Constants;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * The whole tournament bracket.  Updates the bracket when drawRound1End, drawRound2End,
 * drawRound3End are called.
 *
 */
public class TournScreen extends JPanel implements ActionListener{

	private boolean done;
	private JButton ok;
	private JLabel text;
	private TournLeftPanel tl;
	private TournRightPanel tr;
	
	public TournScreen(String user)
	{
		super(new BorderLayout());
		this.setPreferredSize(Constants.GAME_SIZE);
		this.setBackground(Constants.TRANSPARENT);
		
		done =false;
		
		JPanel c = new JPanel(new BorderLayout());
		c.setBackground(Constants.TRANSPARENT);
		
		JPanel center = new JPanel(new BorderLayout());
		center.setBackground(Constants.TRANSPARENT);
		
		TournCupPanel cup = new TournCupPanel();
		cup.setAlignmentY(JPanel.CENTER_ALIGNMENT);
		cup.setAlignmentY(JPanel.CENTER_ALIGNMENT);
		
		ok = new JButton("Play!");
		ok.setBackground(Constants.TRANSPARENT);
		ok.setPreferredSize(Constants.BUTTON_SIZE);
		ok.setFont(Constants.FONT);
		ok.addActionListener(this);
		ok.setVerticalAlignment(JButton.CENTER);
		
		text = new JLabel("Round 1");
		text.setFont(new Font("Monospaced", Font.BOLD, 40));
		text.setHorizontalAlignment(JLabel.CENTER);
		text.setForeground(Color.MAGENTA);
		
		center.add(text, BorderLayout.NORTH);
		center.add(cup, BorderLayout.CENTER);
		center.add(ok, BorderLayout.SOUTH);
		
		//spacer
		JPanel north = new JPanel();
		north.setPreferredSize(new Dimension(100, 200));
		north.setBackground(Constants.TRANSPARENT);
		
		//spacer
		JPanel south = new JPanel();
		south.setPreferredSize(new Dimension(100, 150));
		south.setBackground(Constants.TRANSPARENT);
		
		c.add(north, BorderLayout.NORTH);
		c.add(center, BorderLayout.CENTER);
		c.add(south, BorderLayout.SOUTH);
		
		tl  = new TournLeftPanel(user, (int)System.currentTimeMillis());
		tr = new TournRightPanel(user, (int)System.currentTimeMillis());
		
		this.add(tl, BorderLayout.WEST);
		this.add(c, BorderLayout.CENTER);
		this.add(tr, BorderLayout.EAST);
	}
	
	public void actionPerformed(ActionEvent e)
	{
		if(e.getSource().equals(ok))
		{
			done = true;
		}
	}
	
	public boolean isDone()
	{
		return done;
	}
	
	public void drawRound1End()
	{
		tl.drawRound1End();
		tr.drawRound1End();
		text.setText("Round 1");
		ok.setText("Next Round");
	}
	
	public void drawRound2End()
	{
		tl.drawRound2End();
		tr.drawRound2End();
		text.setText("Round 2");
	}
	
	public void drawRound3End()
	{
		tl.drawRound3End();
		text.setText("You WIN!!!");
		ok.setText("New Game");
	}
	
	public void resetDone()
	{
		done = false;
	}
}
