public class n01
{
    public static void main(String[] args)
    {
	int x = 3, y = 15;
	while (y < 100)
	{
	    x += y + 5;
	    if (x < 50)
	    {	
		y += x + 5;
	    }	
	    y += 10;

	    System.out.printf("At while end: x = %d, y = %d\n", x, y);
	}
	System.out.printf("END: x = %d, y = %d\n", x, y);
    }
}
