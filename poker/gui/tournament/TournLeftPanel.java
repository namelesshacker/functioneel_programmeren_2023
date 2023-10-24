package gui.tournament;


import images.Constants;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.JPanel;


/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * Represents the left side of the Tournament bracket (where the user is).  Complex due to animations.
 *
 */
public class TournLeftPanel extends JPanel{

	private ArrayList<TournIcon> icons;
	private boolean round1, round2, round3;
	private int aniX0, aniX1, aniX2;
	private int paintProgress1, paintProgress2, paintProgress3;
	
	
	public TournLeftPanel(String user, int seed)
	{
		super();
		this.setPreferredSize(TournConstants.TOURN_SIZE);
		this.setBackground(Constants.TRANSPARENT);
		
		ArrayList<String>temp = new ArrayList<String>();
		temp.add("HasselhoffT.gif");
		temp.add("RobespierreT.gif");
		temp.add("TysonT.gif");
		temp.add("LechiffreT.gif");
		temp.add("IvyT.gif");
		temp.add("GuevaraT.gif");
		temp.add("GiulianiT.gif");
		temp.add("MarxT.gif");
		temp.add("TrotskyT.gif");
		
		
		icons = new ArrayList<TournIcon>();
		
		paintProgress1 = 0;
		paintProgress2 = 0;
		paintProgress3 = 0;
		
		round1 = false;
		round2 = false;
		round3 = false;
		
		Random rand = new Random(seed);
		
		final int x0, x1, x2;
		int y0, y1, y2;
		
		
		for(int i =0; i<32; i++)
			icons.add(new TournIcon( temp.get(rand.nextInt(temp.size()))));
		
		icons.set(0,  new TournIcon(user));
		icons.get(0).winRound3();
		icons.set(1,  new TournIcon("HasselhoffT.gif"));
		icons.set(2,  new TournIcon("LechiffreT.gif"));
		icons.set(3,  new TournIcon("RobespierreT.gif"));
		icons.set(4,  new TournIcon("MarxT.gif"));
		icons.get(4).winRound1();
		icons.set(8,  new TournIcon("TrotskyT.gif"));
		icons.get(8).winRound1();
		icons.set(12, new TournIcon("GiulianiT.gif"));
		icons.get(12).winRound1();
		icons.set(16, new TournIcon("GuevaraT.gif"));
		icons.get(16).winRound2();
		
		
		for(int i = 4; i<32; i+=4)
			icons.get(i).winRound1();
		x0 = TournConstants.BUF + TournConstants.W;
		x1 = x0 + TournConstants.FWD;
		x2 = x1;
		
		y0 = TournConstants.BUF + TournConstants.H/2;
		y1 = y0;
		
		for(int i = 0; i < 32; i++)
		{
			if(i%4==0)
			{
				y2 = y1 + TournConstants.H*3/2 + TournConstants.VGAP*3/2;
				icons.get(i).setX0R2(x0 + TournConstants.FWD);
				icons.get(i).setX1R2(x0 + TournConstants.FWD*2);
				icons.get(i).setX2R2(x0 + TournConstants.FWD*2);
				
				icons.get(i).setY0R2(y2);
				icons.get(i).setY1R2(y2);
				icons.get(i).setY2R2(y2 + TournConstants.W*6 + TournConstants.SEP*3/2 + TournConstants.VGAP*7);
				
				if(i==12 || i==28)
				{
					icons.get(i).setY0R2(y2);
					icons.get(i).setY1R2(y2);
					icons.get(i).setY2R2(y2 - TournConstants.W*6 - TournConstants.SEP*3/2 - TournConstants.VGAP*6);
				}
			}
			else if(i%4==1)
				y2 = y1 + TournConstants.H/2 + TournConstants.VGAP/2;
			else if(i%4==2)
				y2 = y1 - TournConstants.H/2 - TournConstants.VGAP/2;
			else
				y2 = y1 - TournConstants.H*3/2 - TournConstants.VGAP*3/2;
				
			icons.get(i).setX0R1(x0);
			icons.get(i).setX1R1(x1);
			icons.get(i).setX2R1(x2);
			
			icons.get(i).setY0R1(y0);
			icons.get(i).setY1R1(y1);
			icons.get(i).setY2R1(y2);
			
			y0 += TournConstants.VGAP + TournConstants.H;
			if((i+1)%4==0)
				y0 += TournConstants.SEP;
			y1 = y0;
		}
	}
	
	public final void paintComponent(Graphics g)
	{
		Graphics2D g2 = (Graphics2D) g;
		
		g2.scale(0.25, 0.25);

		for(int i=0, x = TournConstants.BUF, y=TournConstants.BUF; 
			i < icons.size(); 
			i++, y+=TournConstants.VGAP + TournConstants.H)
		{
			if(i==0)
			{
				g.setColor(Color.yellow);
				g.fillRect(x-4, y-4, TournConstants.W+8, TournConstants.H+8);
			}
			
			g.setColor(Color.black);
			icons.get(i).getImageIcon().paintIcon(this, g, x, y);
			
			g.drawLine(icons.get(i).getX0R1(), icons.get(i).getY0R1(),
					   icons.get(i).getX1R1(), icons.get(i).getY1R1());
			
			if(i%4 == 0)
			{
				g.drawLine(x+TournConstants.W+TournConstants.FWD, y+TournConstants.H/2,
						   x+TournConstants.W+TournConstants.FWD, y+TournConstants.H/2+TournConstants.H*3+TournConstants.VGAP*3);
			
				g.drawLine(icons.get(i).getX0R2(), icons.get(i).getY0R2(),
						   icons.get(i).getX1R2(), icons.get(i).getY1R2());
				
				if(i%16==0 || (i==12 || i==28))
				{
					g.drawLine(icons.get(i).getX1R2(), icons.get(i).getY1R2(),
							   icons.get(i).getX2R2(), icons.get(i).getY2R2());
				
					if(i%16==0)
					{
						g.drawLine(x+TournConstants.W+TournConstants.FWD*2, y+8*TournConstants.H+7*TournConstants.VGAP+TournConstants.SEP*3/2, 
								   x+TournConstants.W+TournConstants.FWD*3, y+8*TournConstants.H+7*TournConstants.VGAP+TournConstants.SEP*3/2);
					}
				}
			}
			
			//Draw the animations
			if(round1 && icons.get(i).round1Result())
			{
				g.setColor(Color.red);
				if(paintProgress1 == 1)
					g.drawLine(icons.get(i).getX0R1(), icons.get(i).getY0R1(), aniX0, icons.get(i).getY1R1());
				else if(paintProgress1 == 2)
				{
					g.drawLine(icons.get(i).getX0R1(), icons.get(i).getY0R1(), 
							icons.get(i).getX1R1(), icons.get(i).getY1R1());
					
					g.drawLine(icons.get(i).getX1R1(), icons.get(i).getY1R1(),
							   icons.get(i).getX2R1(), icons.get(i).getNextYR1());
				}
				
				else if(paintProgress1 == 3)
				{
					g.drawLine(icons.get(i).getX0R1(), icons.get(i).getY0R1(), 
							icons.get(i).getX1R1(), icons.get(i).getY1R1());
					
					g.drawLine(icons.get(i).getX1R1(), icons.get(i).getY1R1(),
							   icons.get(i).getX2R1(), icons.get(i).getY2R1());
				}
				
				if(round2)
				{
					if(paintProgress2 == 1)
					{
						if(i%4==0)
							g.drawLine(icons.get(i).getX0R2(), icons.get(i).getY0R2(),
									   aniX1, icons.get(i).getY1R2());
					}
					else if(paintProgress2 == 2)
					{
						if(i%4==0)
							g.drawLine(icons.get(i).getX0R2(), icons.get(i).getY0R2(),
									   icons.get(i).getX1R2(), icons.get(i).getY1R2());
						if(i%16==0)
							g.drawLine(icons.get(i).getX1R2(), icons.get(i).getY1R2(),
									   icons.get(i).getX2R2(), icons.get(i).getNextYR2());
					}
					else if(paintProgress2 == 3)
					{
						if(i%4==0)
							g.drawLine(icons.get(i).getX0R2(), icons.get(i).getY0R2(),
									   icons.get(i).getX1R2(), icons.get(i).getY1R2());
						if(i%16==0)
							g.drawLine(icons.get(i).getX1R2(), icons.get(i).getY1R2(),
									   icons.get(i).getX2R2(), icons.get(i).getY2R2());
					}
					
					if(round3)
					{
						if(i==0)
						{
							if(paintProgress3==1)
								g.drawLine(x+TournConstants.W+TournConstants.FWD*2, y+8*TournConstants.H+7*TournConstants.VGAP+TournConstants.SEP*3/2, 
										aniX0, y+8*TournConstants.H+7*TournConstants.VGAP+TournConstants.SEP*3/2);
						}
					}
				}
				
				
				
				
				
				g.setColor(Color.black);
			}
			if((i+1)%4 == 0)
				y+=TournConstants.SEP;
		}
	}
	
	public final void drawRound1End()
	{
		round1 = true;
		paintProgress1 = 1;
		for(aniX1 = TournConstants.BUF + TournConstants.W + TournConstants.FWD; 
			aniX1 < TournConstants.BUF + TournConstants.W + TournConstants.FWD*2; 
			aniX1 +=2)
		{
			try
			{
				Thread.sleep(1);
				this.updateUI();
			}
			catch(InterruptedException e)
			{}
		}
		paintProgress1++;
		for(int i=0; i < 120; i+=2)
		{
			try
			{
				Thread.sleep(1);
				this.updateUI();
			}
			catch(InterruptedException e)
			{}
		}
		paintProgress1++;
		this.updateUI();
	}
	
	public final void drawRound2End()
	{
		round1 = true;
		round2 = true;
		
		paintProgress2 = 1;
		
		for(aniX0 = TournConstants.BUF + TournConstants.W + TournConstants.FWD;
			aniX0 < TournConstants.BUF + TournConstants.W + TournConstants.FWD*2;
			aniX0+=2)
		{
			try
			{
				Thread.sleep(1);
				this.updateUI();
			}
			catch(InterruptedException e)
			{
			}
		}
		
		paintProgress2++;
		
		for(int i=0; i < 100; i+=2)
		{
			try
			{
				Thread.sleep(1);
				this.updateUI();
			}
			catch(InterruptedException e)
			{}
		}
		
		paintProgress2++;
	}
	
	public final void drawRound3End()
	{
		round1 = true;
		round2 = true;
		round3 = true;
		
		paintProgress3 = 1;
		
		for(aniX0 = TournConstants.BUF + TournConstants.W + TournConstants.FWD*2;
			aniX0 < TournConstants.BUF + TournConstants.W + TournConstants.FWD*3;
			aniX0+=2)
		{
			try
			{
				Thread.sleep(1);
				this.updateUI();
			}
			catch(InterruptedException e)
			{
			}
		}
	}
}
