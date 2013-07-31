var: lower opt_lower_or_uppers
typename: upper opt_lower_or_uppers
int_literal: digits;
float_literal: opt_digits '.' digits ;
string_literal: '"' not_quotes '"'

opt_lower_or_uppers:| lower_or_uppers;
lower_or_uppers: lower_or_upper | lower_or_uppers lower_or_upper ;
lower_or_upper: lower | upper ;

lower: 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' |
       'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' |
       'w' | 'x' | 'y' | 'z' | '_' ;

upper: 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' |
       'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' |
       'W' | 'X' | 'Y' | 'Z' ;

opt_digits:| digits ;
digits: digit | digits digit ;
digit: '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ;

ws: ' ' | '\t' | '\r' | '\n' ;

not_quotes: not_quote | not_quotes not_quote ;
not_quote: lower | upper | digits | op | unary_op | '\''