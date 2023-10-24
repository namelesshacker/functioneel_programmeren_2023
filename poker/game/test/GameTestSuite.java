package game.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class GameTestSuite extends TestSuite{
	
	public static Test suite()
	{
      TestSuite lSuite = new TestSuite( "Test suite for the game package" );
      lSuite.addTestSuite( TestBetAction.class );
      lSuite.addTestSuite( TestCardExchangeAction.class );
      lSuite.addTestSuite( TestScoreRevealAction.class);
      lSuite.addTestSuite( TestIncrementAnteAction.class);
      
      lSuite.addTestSuite( TestPurse.class);
      lSuite.addTestSuite( TestGameContainer.class);
      
      lSuite.addTestSuite( TestPot.class);
      lSuite.addTestSuite( TestGamePot.class);
      
      lSuite.addTestSuite( TestGameStats.class);
      lSuite.addTestSuite( TestPlayerGameStats.class);
      
      lSuite.addTestSuite( TestGameModel.class);
      lSuite.addTestSuite( TestGameModelScoreReveal.class);
      lSuite.addTestSuite( TestPlayerComparator.class);
	  return lSuite;
  	}	
}