#include "picoScanner.h"
#include "ast-mod.h"

namespace pico {
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
         pico::FlexScanner scanner;
         pico::BisonParser parser;
   };
}

// Entry Point
int main(int argc, char * argv[]) {
	pico::Parser parser;
   pico::Initialize();
   if (argc > 1 && !strcmp(argv[1], "debug"))
      parser.set_debug_level(1);
	return parser.parse();
}

