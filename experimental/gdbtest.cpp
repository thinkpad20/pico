#include <iostream>
#include <vector>
#include <string>

void foo(int i) {
   for (int j = 0; j <i; ++j) {
      std::cout << j << std::endl;
   }
fuzzy:
   std::cout << "hey now brown cow" << std::endl;
}

int main(int argc, char const *argv[])
{
   foo(10);
   std::vector<std::string> v;
   for (int i = 1; i < argc; ++i) {
      v.push_back(std::string(argv[i]));
   }
   std::cout << "You said: ";
   for (int i = 0; i < v.size(); ++i) {
      std::cout << v[i] << " ";
   }
   std::cout << std::endl;
   return 0;
}