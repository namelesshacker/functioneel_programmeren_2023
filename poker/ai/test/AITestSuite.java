package ai.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AITestSuite extends TestSuite{
	
	public static Test suite()
	{
      TestSuite lSuite = new TestSuite( "Test suite for the ai package" );

      // Discard Strategies
      lSuite.addTestSuite( TestAIAggressiveDiscardStrategy.class);
      lSuite.addTestSuite( TestAIConservativeDiscardStrategy.class);
      lSuite.addTestSuite( TestAIBlackAndWhiteDiscardStrategy.class);
      
      //Score Reveal Strategy
      lSuite.addTestSuite( TestAIScoreRevealStrategy.class);
      
      // Utility classes
      lSuite.addTestSuite( TestAIHandValuator.class);
      lSuite.addTestSuite( TestAISimpleDecision.class);
      
      // Bet classes
      lSuite.addTestSuite( TestAIParametricBetDivider.class);
      lSuite.addTestSuite( TestAIParametricInstinct.class);
      
      // Bluff classes
      lSuite.addTestSuite( TestAIParametricBluffFinder.class); 
      lSuite.addTestSuite( TestAIParametricBluffer.class);
      
      return lSuite;
  	}	
}