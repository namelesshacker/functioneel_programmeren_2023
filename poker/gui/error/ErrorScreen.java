package gui.error;

import images.Constants;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * The error screen.  a message and a confirmation button.  simple.
 *
 */
public class ErrorScreen extends JPanel implements ActionListener{

	private boolean done;
	
	public ErrorScreen(String msg)
	{
		super(new BorderLayout());
		this.setPreferredSize(Constants.GAME_SIZE);
		this.setBackground(Constants.TRANSPARENT);
		
		JLabel error = new JLabel("Error! "+msg);
		error.setFont(Constants.FONT);
		error.setHorizontalAlignment(JLabel.CENTER);
		error.setForeground(Color.red);
		
		JButton ok = new JButton("BACK");
		ok.setFont(Constants.FONT);
		ok.setBackground(Constants.TRANSPARENT);
		ok.addActionListener(this);
		ok.setActionCommand("ok");
		ok.setPreferredSize(Constants.BUTTON_SIZE);
		
		JPanel button = new JPanel(new FlowLayout());
		button.setBackground(Constants.TRANSPARENT);
		
		JPanel sp1 = new JPanel();
		sp1.setBackground(Constants.TRANSPARENT);
		sp1.setPreferredSize(new Dimension(10, 10));
		
		JPanel sp2 = new JPanel();
		sp2.setBackground(Constants.TRANSPARENT);
		sp2.setPreferredSize(new Dimension(10, 10));
		
		button.add(sp1);
		button.add(ok);
		button.add(sp2);
		
		JPanel center = new JPanel(new GridLayout(0,1));
		center.setBackground(Constants.TRANSPARENT);
		center.add(error);
		center.add(button);
		
		JPanel n = new JPanel();
		n.setBackground(Constants.TRANSPARENT);
		n.setPreferredSize(new Dimension(100, 300));
		
		JPanel s = new JPanel();
		s.setBackground(Constants.TRANSPARENT);
		s.setPreferredSize(new Dimension(100, 300));
		
		JPanel e = new JPanel();
		e.setBackground(Constants.TRANSPARENT);
		e.setPreferredSize(new Dimension(100, 300));
		
		JPanel w = new JPanel();
		w.setBackground(Constants.TRANSPARENT);
		w.setPreferredSize(new Dimension(100, 300));
		
		done = false;
		
		this.add(n, BorderLayout.NORTH);
		this.add(e, BorderLayout.EAST);
		this.add(center, BorderLayout.CENTER);
		this.add(w, BorderLayout.WEST);
		this.add(s, BorderLayout.SOUTH);	
	}
	
	public void actionPerformed(ActionEvent e)
	{
		if(e.getActionCommand().equals("ok"))
		{
			done = true;
		}
	}
	
	public boolean isDone()
	{
		return done;
	}
}
