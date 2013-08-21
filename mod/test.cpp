#include <iostream>

int main(int argc, char const *argv[])
{
   std::string *s = new std::string('x');
   std::cout << *s << std::endl;
   return 0;
}