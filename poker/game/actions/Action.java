package game.actions;

/**
 * 
 * @author Gen Kazama and David Kawrykow
 * 
 * Interface for all actions
 * 
 */
public interface Action {

	public String getActionMaker();
	
	public String toCode();
	
	public static final String SEP_CHAR = new String(",");
	public static final String END_CHAR = new String(";");
}
