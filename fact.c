#include <stdio.h>

unsigned fact(unsigned i) {
   return i <= 1 ? i : i * fact(i-1);
}

unsigned fib (unsigned i) {
   return i <= 2 ? i : fib(i-1) + fib(i-2);
}

int main(int argc, char const *argv[])
{
   printf("%d %d\n", fact(10), fib(40));
   return 0;
}