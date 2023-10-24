package players;

public class Constants {

	public static Player getPlayer(String name, int money, String bIcon, String sIcon)
	{
		int i = name.indexOf("-");
		String ending;
		
		if(i < 0)
			return null;
		
		ending = name.substring(i);
		
		// The user player
		if(name.indexOf("-s") >= 0)
		{
			return new GUIHumanPlayer(name, money, bIcon, sIcon);
		}
		
		// The AI players
		if(name.indexOf("Hasselhoff") >= 0)
		{
			return new DavidHasselhoff(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Guevara") >= 0)
		{
			return new ErnestoGuevara(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Giuliani") >= 0)
		{
			return new Giuliani(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Ivy League Prepster") >= 0)
		{
			return new IvyLeaguePrepster(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Marx") >= 0)
		{
			return new KarlMarx(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Chiffre") >= 0)
		{
			return new LeChiffre(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Tyson") >= 0)
		{
			return new MikeTyson(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Robespierre") >= 0)
		{
			return new Robespierre(ending, money, (int)System.currentTimeMillis());
		}
		
		if(name.indexOf("Trotsky") >= 0)
		{
			return new Trotsky(ending, money, (int)System.currentTimeMillis());
		}
		
		return null;
	}
	
}
