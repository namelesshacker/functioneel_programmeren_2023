package game.actions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * This action tells everyone that the cards were dealt out.
 *
 */
public class CardsDealtAction implements Action{

	private String actionMaker;
	
	public CardsDealtAction()
	{
		actionMaker = "The Dealer";
	}
	
	public String getActionMaker()
	{
		return actionMaker;
	}
	
	public String toString()
	{
		return "The Cards were dealt out";
	}
	
	public String toCode()
	{
		return "CardsDealtAction"+SEP_CHAR + END_CHAR + "\n";
	}
}
