package util.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class UtilTestSuite extends TestSuite
{
	/**
	 * @return A test suite for the concern model package.
	 */
	public static Test suite()
	{
      TestSuite lSuite = new TestSuite( "Test suite for the util package" );
      lSuite.addTestSuite( TestCard.class );
      lSuite.addTestSuite( TestDeck.class );
      lSuite.addTestSuite( TestHand.class );
	  return lSuite;
  	}
}

