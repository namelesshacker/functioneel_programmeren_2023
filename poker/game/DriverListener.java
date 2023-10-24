package game;

import game.actions.*;

public class DriverListener implements GameListener{

	public DriverListener()
	{}
	
	public void notify(Action action)
	{
		System.out.println(action);
	}
}
