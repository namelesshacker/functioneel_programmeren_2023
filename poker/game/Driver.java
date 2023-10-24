package game;

import players.*;
import java.util.*;


public class Driver{
	
	public static void main(String[] args)
	{
		Scanner in = new Scanner(System.in);
		// Set up the initial objects and add them to the game 
		DriverListener dl = new DriverListener();	// a trivial game listener used to print game progress
		GameModel g =new GameModel();				// an instance of the game model
		Player p[] = new Player[4];
	
		
		for(int i=0; i< 3; i++)
		{
			System.out.println("Please choose one of the following AI players to play: \n");
			System.out.println("1.  David Hasselhoff\t(Mediocre Aggressive)");
			System.out.println("2.  Dr Jekyll\t\t(Smart Aggressive)");
			System.out.println("3.  Ivy League Prepster\t(Smart Conservative)");
			System.out.println("4.  Karl Marx\t\t(Expert Conservative)");
			System.out.println("5.  Le Chiffre\t\t(Mediocre)");
			System.out.println("6.  Mike Tyson\t\t(Aggressive)");
			System.out.println("7.  Robespierre\t\t(Beginner)");
			System.out.println("8.  Trotsky\t\t(Mediocre Conservative)");
			System.out.println("9.  Mussolini\t\t(Beginner)");
			System.out.println("10. Ernesto Guevara\t(Tactical Expert)\n");
			System.out.print("Player "+(i+1)+" is: ");
			
			int choice = in.nextInt();
			
			switch (choice)
			{
			case 1:
				p[i] = new DavidHasselhoff("-"+i, 205, 123413431);
				break;
			case 2:
				p[i] = new Giuliani("-"+i, 205, 123123333);
				break;
			case 3:
				p[i] = new IvyLeaguePrepster("-"+i, 205, 234234343);
				break;
			case 4:
				p[i] = new KarlMarx("-"+i, 205, 1998919);
				break;
			case 5:
				p[i] = new LeChiffre("-"+i, 205, 90866161);
				break;
			case 6:
				p[i] = new MikeTyson("-"+i, 205, 78934751);
				break;
			case 7:
				p[i] = new Robespierre("-"+i, 205, 9898231);
				break;
			case 8:
				p[i] = new Trotsky("-"+i, 205, 234234345);
				break;
			case 9:
				p[i] = new PrototypePlayer("Mussolini", 205);
				break;
			case 10:
				p[i] = new ErnestoGuevara("-"+i, 205, 12341321);
				break;
			default:
				System.out.println("You entered in a wrong input.  Creating a defult character");
				p[i] = new PrototypePlayer("Default-"+i, 205);
			}
			
			System.out.println("The Player you just created is "+p[i]+"\n");
			
			System.out.println();
			in.nextLine();
		}
			
			
		p[3] = new ConsolePlayer("User", 205);
		
		System.out.println("-----------------------------------------------");
		System.out.println("You will now battle the 3 AI players you chose.\n\n");
		System.out.println("-----------------------------------------------");
		
		try{
			g.addPlayerToGame(p[0]);
			g.addPlayerToGame(p[1]);
			g.addPlayerToGame(p[2]);
			g.addPlayerToGame(p[3]);
			g.attachGameListener(dl);
		}
		catch(Exception e)
		{
			assert false;
		}
		
		// Execute until a game winner is determined (or a pre-defined number of times if you wish)
		try
		{
			while(!g.isGameOver())
			{
				g.ExecuteSingleMatch();
			
				in.nextLine();
			}
			// Once the condition is met
			System.out.println("\n\nWINNER: " + g.getGameWinner());
		}
		catch(Exception e)
		{
			assert false;
		}
		
		System.out.println("END\n\n\n\n"); 
	}
	
	
}
