#include "../include/picoScanner.h"
#include "../include/ast.h"

namespace pico {
   class Parser {
      private:
         pico::FlexScanner scanner;
         pico::BisonParser parser;
      public:
         Parser(): parser(scanner) {}
      
         int parse() {
            return parser.parse();
         }
         void set_debug_level(int level) {
            parser.set_debug_level(level);
         }
   };
}

using namespace pico;
using namespace std;

// Entry Point
int main(int argc, char * argv[]) {
	Parser parser;
   Expression::init();
   if (argc > 1 && !strcmp(argv[1], "debug"))
      parser.set_debug_level(1);
	parser.parse();
   cout << parsed_expressions << endl;
   return 0;
}

