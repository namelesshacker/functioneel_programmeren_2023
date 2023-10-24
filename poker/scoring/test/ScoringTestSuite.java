package scoring.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class ScoringTestSuite extends TestSuite
{
	/**
	 * @return A test suite for the concern model package.
	 */
	public static Test suite()
	{
      TestSuite lSuite = new TestSuite( "Test suite for the scoring package" );
      lSuite.addTestSuite( TestBasic.class );
      lSuite.addTestSuite( TestTieBreakers.class );
	  return lSuite;
  	}
}

