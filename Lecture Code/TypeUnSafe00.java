import java.util.*;

public class TypeUnSafe00
{
    public static void main(String args[]) 
    {
	List    l = new LinkedList();
	l.add(new Integer(0));
	Integer x = l.iterator().next(); 
    }

    /*
      TypeUnSafe00.java:9: error: incompatible types: Object cannot be
      converted to Integer
	Integer x = l.iterator().next(); 
	                             ^
      Note: TypeUnSafe00.java uses unchecked or unsafe operations.
      Note: Recompile with -Xlint:unchecked for details.
      1 error
    */

    /*
      Possible fix is type cast:
    */
    // Integer x = (Integer) l.iterator().next(); // need type cast

    // Or!!!
    // as we see in the next example using Generics
}


