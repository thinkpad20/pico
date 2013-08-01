#include <iostream>
#include <map>
#include <vector>
#include "../include/ast.h"

using namespace std;

int main(int argc, char const *argv[])
{
   cout << "Map is size: " << sizeof(map<void *, void *>) << endl;
   cout << "Vector is size: " << sizeof(vector<void *>) << endl;
   return 0;
}