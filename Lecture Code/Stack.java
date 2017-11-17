public class Stack {
    protected class Node {
	int data;
	Node next;

	// Method Node data structure
	Node (int v, Node n) { data = v; next = n; }
    }

    // Initialization
    public Stack() { stk = null; }

    // Is the stack empty
    public boolean empty() { return stk == null; }

    // Get the top element, point to the next, and return the top element
    public int pop() {
	int result = stk.data;
	stk = stk.next;
	return result;
    }

    // Show me the top element
    public int top () { return stk.data; }

    // Add an element to the top of the stack
    public void push (int i) { stk = new Node (i, stk); }

    // Make stk private
    private Node stk; // state variable, properly encapsulated
}
