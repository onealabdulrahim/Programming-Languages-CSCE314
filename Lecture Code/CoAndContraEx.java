import java.util.*;

class Point 
{
	public int x;
	public int y;

	public void print() {
		System.out.println("x = "+x+"; y = "+y);
	}	

}

class ColorPoint extends Point
{
	public int color;

	public void print() {
		System.out.println("x = "+x+"; y = "+y+"; color = "+color);
	}	

	public void updateColor(int c) {
		color = c;
	}
}

class AlphaColorPoint extends ColorPoint
{
	public int alpha;

	public void print() {
		System.out.println("x = "+x+"; y = "+y+"; color = "+color+"; alpha = "+alpha);
	}	

	public void updateColor(int c, int a) {
		color = c;
		alpha = a;
	}

}

class BasicExample 
{
	public ColorPoint doStuffWithPt(ColorPoint pt) {
		System.out.println("Doing stuff with a point.");
		if (pt.color == 0)
		{	
			System.out.println("This point is black.");
			return pt;
		}
		return null;
	}
}

class ExtendedExample  extends BasicExample
{
	//public AlphaColorPoint doStuffWithPt(Point pt) { // Co- + Contra-variance
	//public ColorPoint doStuffWithPt(Point pt) { // Contra-variance -- java treats this as a new name
	public AlphaColorPoint doStuffWithPt(ColorPoint pt) { // Co-variance -- java treats this as the same name, i.e., overloads
		System.out.println("Doing extended stuff with point.");
		if (pt.x < pt.y)
		{
			System.out.println("Yes");
			AlphaColorPoint ret = new AlphaColorPoint();	
			ret.x = pt.x;
			ret.y = pt.y;
			ret.updateColor(45, 69);
			return ret;
		}
		System.out.println("No");
		return null;
	}

}

class MyPair<S, T> {
	public S fst;
	public T snd;
}

class CoAndContraEx
{
	public static void oldMain(String args[]) {

		BasicExample be = new ExtendedExample();

		ColorPoint cp = new AlphaColorPoint();		

		cp.x = 7;
		cp.y = 67;
		cp.updateColor(0);
		cp.print();
			
		// This compiles fine
		ColorPoint cp2 = 
				be.doStuffWithPt(cp);
		// This doesn't compile
		//AlphaColorPoint cp2 = 
		//		be.doStuffWithPt(cp);
		
		//ExtendedExample ee = new ExtendedExample();

		//AlphaColorPoint cp3 = 
		//		ee.doStuffWithPt(cp);

		cp2.print();

		System.out.println("cp2 = "+cp2);
	}

	//Could also do: private static void workPair(MyPair<? extends Point,? super ColorPoint> p) {
	//		
	// This will work with: MyPair<Point, ColorPoint>
	//                      MyPair<ColorPoint, Point>
	//                      MyPair<AlphaColorPoint, Object>
	//	                       (and the variations inbetween)
	
	private static void workPair(MyPair<? extends Point,? extends Point> p) {
		if (p == null)
		{
			System.out.println("You gave me a null pair");
			return;
		}
		if ((p.fst == null) || (p.snd == null))
		{
			System.out.println("An element is empty");
			return;
		}
		System.out.println("Things check out");
		System.out.print("First: ");
		p.fst.print();
		System.out.print("Second: ");
		p.snd.print();
	}

	public static void main(String args[]) {


		ColorPoint cp = new AlphaColorPoint();		

		cp.x = 7;
		cp.y = 67;
		cp.updateColor(0);
		cp.print();
			
		workPair(null);
	
		MyPair<ColorPoint, ColorPoint> pr1 = new MyPair<ColorPoint, ColorPoint>();	
		pr1.fst = cp;
		pr1.snd = cp;
		workPair(pr1);
		

		Point p = cp;
		MyPair<Point, ColorPoint> pr2 = new MyPair<Point, ColorPoint>();	
		pr2.fst = p;
		pr2.snd = null;
		workPair(pr2);
	}

}

