// stack.c

#include "stack.h"
#include <stdlib.h>
#include <stdio.h>

int empty(STACK s) { return s == NULL; }

STACK newstack()   { return (STACK) NULL; }

int pop(STACK* s)
{
  STACK tmp;
  int res = (*s)->data;
  tmp = *s;
  *s = (*s)->next;
  free(tmp);

  return res;
}

void push(STACK* s, int x)
{
  STACK tmp;
  tmp = (STACK)malloc(sizeof(struct node));
  tmp->data = x;
  tmp->next = *s;
  *s = tmp;
}

int top(STACK s) { return s->data; }

int main(void)
{
  STACK s = newstack();

  if (empty(s))
    {
      printf( "\nStack s is initially empty" ); 
    }

/*
  To be completed - not entirely correct ...

  push(s, 1);
  if (empty(s))
    {
      printf( "\nStack s is initially empty" ); 
    }

  printf( "s = %d\n", top(s) );
  
  push(s, 3);
  printf( "s = %d\n", top(s) );
  
  pop(s);
  printf( "s = %d\n", top(s) );
  
  pop(s);
  if (empty(s))
    {
      printf( "Stack s is empty again" ); 
    }

*/  
  exit(0);
}


