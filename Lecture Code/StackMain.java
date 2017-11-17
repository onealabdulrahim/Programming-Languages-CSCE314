//
// Main Class that applies Stack
// 
class StackMain {

    public static void main (String args[])
    {
	Stack s = new Stack();
	if (s.empty())
	{
	    System.out.println( "\nStack s is initially empty" ); 
	}

	s.push(1);
	System.out.println( Integer.toString( s.top() ) );

	s.push(3);
	System.out.println( Integer.toString( s.top() ) );

	s.pop();
	System.out.println( Integer.toString( s.top() ) );

	s.pop();
	if (s.empty())
	{
	    System.out.println( "Stack s is empty again" ); 
	}

    }
}
