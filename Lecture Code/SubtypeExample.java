import java.util.*;

class Point 
{
    public int x;
    public int y;

    public void print() 
    {
	System.out.println("x = "+x+"; y = "+y);
    }

}

class ColorPoint extends Point 
{
    public int color;

    public void print() 
    {
	System.out.println("x = "+x+"; y = "+y+"; color = "+color);
    }

}

class SubtypeExample 
{
    public static void main(String args[]) 
    {
	Point p = new Point();
	p.x = 0; p.y = 0; 
	p.print();

	ColorPoint cp = new ColorPoint();
	cp.x = 10; cp.y = 1; 
	cp.color = 77;
	cp.print();
	
	Point p2 = cp;
	p2.print();

    }

}

