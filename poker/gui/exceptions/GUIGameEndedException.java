package gui.exceptions;

public class GUIGameEndedException extends Exception {
	
	private String mWinner;
	
	/**
	 * This class terminates a game prematurely if the user lost
	 */
	public GUIGameEndedException(String GameWinner)
	{
		super();
		
		mWinner = GameWinner;
	}

}
