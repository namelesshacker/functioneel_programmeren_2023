package gui.tournament;


import images.Constants;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.Random;

import javax.swing.JPanel;

public class TournRightPanel extends JPanel{
	private ArrayList<TournIcon> icons;
	private boolean round1, round2, round3;
	private int aniX0, aniX1, aniX2;
	private int paintProgress1, paintProgress2, paintProgress3;
	
	
	public TournRightPanel(String user, int seed)
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
		
		icons.set(0,  new TournIcon("TysonT.gif"));
		icons.get(0).winRound2();
		icons.set(16, new TournIcon("IvyT.gif"));
		icons.get(16).winRound2();
		
		
		for(int i = 4; i<32; i+=4)
			icons.get(i).winRound1();
		x0 = TournConstants.WIDTH*4-TournConstants.BUF - TournConstants.W;
		x1 = x0 - TournConstants.FWD;
		x2 = x1;
		
		y0 = TournConstants.BUF + TournConstants.H/2;
		y1 = y0;
		
		for(int i = 0; i < 32; i++)
		{
			if(i%4==0)
			{
				y2 = y1 + TournConstants.H*3/2 + TournConstants.VGAP*3/2;
				icons.get(i).setX0R2(x0 - TournConstants.FWD);
				icons.get(i).setX1R2(x0 - TournConstants.FWD*2);
				icons.get(i).setX2R2(x0 - TournConstants.FWD*2);
				
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
		
		for(int i=0, x = TournConstants.WIDTH*4-TournConstants.BUF, y=TournConstants.BUF; 
			i < icons.size(); 
			i++, y+=TournConstants.VGAP + TournConstants.H)
		{
			
			g.setColor(Color.black);
			
			icons.get(i).getImageIcon().paintIcon(this, g, x-TournConstants.W, y);
			
			g.drawLine(icons.get(i).getX0R1(), icons.get(i).getY0R1(),
					   icons.get(i).getX1R1(), icons.get(i).getY1R1());

			/*System.out.println(icons.get(i).getX0R1() +" "+ icons.get(i).getY0R1()+" "+
					   icons.get(i).getX1R1()+" "+ icons.get(i).getY1R1());
			*/
			if(i%4 == 0)
			{
				g.drawLine(x-TournConstants.W-TournConstants.FWD, y+TournConstants.H/2,
						   x-TournConstants.W-TournConstants.FWD, y+TournConstants.H/2+TournConstants.H*3+TournConstants.VGAP*3);
			
				g.drawLine(icons.get(i).getX0R2(), icons.get(i).getY0R2(),
						   icons.get(i).getX1R2(), icons.get(i).getY1R2());
				
				if(i%16==0 || (i==12 || i==28))
				{
					g.drawLine(icons.get(i).getX1R2(), icons.get(i).getY1R2(),
							   icons.get(i).getX2R2(), icons.get(i).getY2R2());
				
					if(i%16==0)
					{
						g.drawLine(x-TournConstants.W-TournConstants.FWD*2, y+8*TournConstants.H+7*TournConstants.VGAP+TournConstants.SEP*3/2, 
								   x-TournConstants.W-TournConstants.FWD*3, y+8*TournConstants.H+7*TournConstants.VGAP+TournConstants.SEP*3/2);
					}
				}
			}
			
			if(round1)
			{
				g.setColor(Color.red);
				if(i%4==0)
				{
					g.drawLine(icons.get(i).getX0R1(), icons.get(i).getY0R1(), 
						       icons.get(i).getX1R1(), icons.get(i).getY1R1());
		
					g.drawLine(icons.get(i).getX1R1(), icons.get(i).getY1R1(),
						       icons.get(i).getX2R1(), icons.get(i).getY2R1());
				}
			}
			if(round2)
			{
				if(i%4==0)
					g.drawLine(icons.get(i).getX0R2(), icons.get(i).getY0R2(),
							   icons.get(i).getX1R2(), icons.get(i).getY1R2());
				if(i%16==0)
					g.drawLine(icons.get(i).getX1R2(), icons.get(i).getY1R2(),
							   icons.get(i).getX2R2(), icons.get(i).getY2R2());
			}
					
			g.setColor(Color.black);
		
			if((i+1)%4 == 0)
				y+=TournConstants.SEP;
		}
	}
	
	public final void drawRound1End()
	{
		round1 = true;
	}
	
	public final void drawRound2End()
	{
		round1 = true;
		round2 = true;
	}
	
	public final void drawRound3End()
	{
		round1 = true;
		round2 = true;
		round3 = true;
	}
}
