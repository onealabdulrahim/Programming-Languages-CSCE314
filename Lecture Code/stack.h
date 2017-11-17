// stack.h

struct node
{
  int data;
  struct node *next;
};

typedef struct node* STACK;

int empty(STACK s);

STACK newstack();

int pop(STACK* s);

void push(STACK* s, int x);

int top(STACK s);

