package gui;

import game.GameListener;
import game.actions.Action;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class GUIGameSaver implements GameListener {
	
	/**
	 * Recalls whether an IOException occurred
	 */
	boolean mCorrupted = false;
	
	/**
	 * The writer itself
	 */
	BufferedWriter mBufferedWriter;
	
	/**
	 * IOExceptions are remembered. The writer is deemed corrupt when any exception is thrown.
	 */
	GUIGameSaver(String filename)
	{
		try {
	        mBufferedWriter = new BufferedWriter(new FileWriter(filename));
	        mBufferedWriter.flush();
	    } 
		catch (IOException e) 
		{
			mCorrupted = true;
	    }
	}
	
	/**
	 * Answers whether an IOException occurred at any point
	 */
	public boolean isCorrupted()
	{
		return mCorrupted;
	}
	
	/**
	 * See the interface
	 */
	public void notify(Action action)
	{
		try
		{
			mBufferedWriter.write(action.toCode());
			mBufferedWriter.flush();
		}
		catch (IOException e)
		{
			mCorrupted = true;
		}
	}
	
	/**
	 * Ends the transmission
	 */
	public void closeFile()
	{
		try
		{
			mBufferedWriter.close();
		}
		catch (IOException e)
		{
			mCorrupted = true;
		}
	}
}
