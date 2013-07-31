#include <stdio.h>
#include <stdlib.h>

typedef int val_t;

typedef struct cvm_stack_s {
   val_t val;
   struct cvm_stack_s *next;
} cvm_stack_t;

cvm_stack_t *stack = NULL;

void push(val_t val) {
   cvm_stack_t *s = (cvm_stack_t *)calloc(1, sizeof(cvm_stack_t));
   s->val = val;
   s->next = stack;
   stack = s;
}

val_t pop(val_t val) {
   if (!stack) { fprintf(stderr, "Error: stack is empty\n"); exit(1); }
   else {
   val_t res = stack->val;
   cvm_stack_t *temp = stack;
   stack = stack->next;
   free(temp);
   return res;
   }
}

int main(int argc, char const *argv[])
{
   /* code */
   return 0;
}