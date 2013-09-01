#include "picoScanner.h"
#include "ast.h"

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
   // compile_init();
   // try { compile((*parsed_expressions)[0]); }
   // catch (string s) { cout << "Exception: " << s << endl; }
   // std::deque<Assignment> asns = get_assignments(parsed_expressions);
   // cout << "Found these assignments: " << endl;
   // for (int i = 0; i < asns.size(); ++i) {
   //    cout << asns[i] << endl;
   // }
   return 0;
}

