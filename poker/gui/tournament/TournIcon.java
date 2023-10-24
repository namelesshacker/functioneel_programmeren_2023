package gui.tournament;

import images.Constants;

import javax.swing.ImageIcon;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This class represents one icon in the Tournament bracket.
 *
 */
public class TournIcon {
	
	private ImageIcon img;
	private boolean wonRound1, wonRound2, wonRound3;
	private int x0r1, y0r1, x1r1, y1r1, x2r1, y2r1;
	private int yir1;
	
	private int x0r2, y0r2, x1r2, y1r2, x2r2, y2r2;
	private int yir2;
	
	private Constants c = new Constants();
	
	public TournIcon(String s)
	{
		img = c.getImageIconFromString(s);
		wonRound1 = false;
		wonRound2 = false;
		wonRound3 = false;
	}

	public void winRound1()
	{
		wonRound1 = true;
	}
	
	public void winRound2()
	{
		wonRound1 = true;
		wonRound2 = true;
	}
	
	public void winRound3()
	{
		wonRound1 = true;
		wonRound2 = true;
		wonRound3 = true;
	}
	
	public ImageIcon getImageIcon()
	{
		return img;
	}
	
	public boolean round1Result()
	{
		return wonRound1;
	}
	
	public boolean round2Result()
	{
		return wonRound2;
	}
	
	public boolean round3Result()
	{
		return wonRound3;
	}
	
	public void setX0R1(int x)
	{
		x0r1=x;
	}
	
	public void setX1R1(int x)
	{
		x1r1=x;
	}
	
	public void setX2R1(int x)
	{
		x2r1=x;
	}
	
	public void setY0R1(int y)
	{
		y0r1 = y;
	}
	
	public void setY1R1(int y)
	{
		y1r1 = y;
		yir1 = y;
	}
	
	public void setY2R1(int y)
	{
		y2r1 = y;
	}
	
	public int getX0R1()
	{
		return x0r1;
	}
	
	public int getX1R1()
	{
		return x1r1;
	}
	
	public int getX2R1()
	{
		return x2r1;
	}
	
	public int getY0R1()
	{
		return y0r1;
	}
	
	public int getY1R1()
	{
		return y1r1;
	}
	
	public int getY2R1()
	{
		return y2r1;
	}
	
	public int getNextYR1()
	{
		yir1+=2;
		if(yir1>=y2r1)
			yir1=y1r1;
		return yir1;
	}
	
	public int getYI1()
	{
		return yir1;
	}
	
	
	
	//Round 2 things
	public void setX0R2(int x)
	{
		x0r2=x;
	}
	
	public void setX1R2(int x)
	{
		x1r2=x;
	}
	
	public void setX2R2(int x)
	{
		x2r2=x;
	}
	
	public void setY0R2(int y)
	{
		y0r2 = y;
	}
	
	public void setY1R2(int y)
	{
		y1r2 = y;
		yir2 = y;
	}
	
	public void setY2R2(int y)
	{
		y2r2 = y;
	}
	
	public int getX0R2()
	{
		return x0r2;
	}
	
	public int getX1R2()
	{
		return x1r2;
	}
	
	public int getX2R2()
	{
		return x2r2;
	}
	
	public int getY0R2()
	{
		return y0r2;
	}
	
	public int getY1R2()
	{
		return y1r2;
	}
	
	public int getY2R2()
	{
		return y2r2;
	}
	
	public int getNextYR2()
	{
		yir2+=2;
		if(yir2>=y2r2)
			yir2=y1r2;
		return yir2;
	}
	
	public int getYI2()
	{
		return yir2;
	}
}
