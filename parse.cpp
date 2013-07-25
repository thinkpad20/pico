#include "foobarScanner.h"
#include "expression.h"

namespace foobar {
   class Parser {
      public:
         Parser() : parser(scanner) {}
      
         int parse() {
            return parser.parse();
         }
         void set_debug_level(int level) {
            parser.set_debug_level(level);
         }
      private:
         foobar::FlexScanner scanner;
         foobar::BisonParser parser;
   };
}

// Entry Point
int main(int argc, char * argv[]) {
	foobar::Parser parser;
   foobar::Initialize();
   if (argc > 1 && !strcmp(argv[1], "debug"))
      parser.set_debug_level(1);
	return parser.parse();
}

