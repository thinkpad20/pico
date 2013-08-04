/* A Bison parser, made by GNU Bison 2.7.12-4996.  */

/* Skeleton implementation for Bison LALR(1) parsers in C++
   
      Copyright (C) 2002-2013 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */


/* First part of user declarations.  */
/* Line 283 of lalr1.cc  */
#line 1 "src/pico.y"


#include "../include/ast.h"
#include "../include/compile.h"
#define YYDEBUG 1
int yydebug = 1;
extern int yylineno;
extern pico::ExpressionList *pico::parsed_expressions;


/* Line 283 of lalr1.cc  */
#line 49 "pico.tab.c"


#include "pico.tab.h"

/* User implementation prologue.  */

/* Line 289 of lalr1.cc  */
#line 57 "pico.tab.c"
/* Unqualified %code blocks.  */
/* Line 290 of lalr1.cc  */
#line 32 "src/pico.y"

	// Prototype for the yylex function
	static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner);


/* Line 290 of lalr1.cc  */
#line 67 "pico.tab.c"


# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* FIXME: INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (/*CONSTCOND*/ false)
# endif


/* Suppress unused-variable warnings by "using" E.  */
#define YYUSE(e) ((void) (e))

/* Enable debugging if requested.  */
#if YYDEBUG

/* A pseudo ostream that takes yydebug_ into account.  */
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)	\
do {							\
  if (yydebug_)						\
    {							\
      *yycdebug_ << Title << ' ';			\
      yy_symbol_print_ ((Type), (Value), (Location));	\
      *yycdebug_ << std::endl;				\
    }							\
} while (false)

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug_)				\
    yy_reduce_print_ (Rule);		\
} while (false)

# define YY_STACK_PRINT()		\
do {					\
  if (yydebug_)				\
    yystack_print_ ();			\
} while (false)

#else /* !YYDEBUG */

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Type, Value, Location) YYUSE(Type)
# define YY_REDUCE_PRINT(Rule)        static_cast<void>(0)
# define YY_STACK_PRINT()             static_cast<void>(0)

#endif /* !YYDEBUG */

#define yyerrok		(yyerrstatus_ = 0)
#define yyclearin	(yychar = yyempty_)

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)

/* Line 357 of lalr1.cc  */
#line 14 "src/pico.y"
namespace pico {
/* Line 357 of lalr1.cc  */
#line 163 "pico.tab.c"

  /* Return YYSTR after stripping away unnecessary quotes and
     backslashes, so that it's suitable for yyerror.  The heuristic is
     that double-quoting is unnecessary unless the string contains an
     apostrophe, a comma, or backslash (other than backslash-backslash).
     YYSTR is taken from yytname.  */
  std::string
  BisonParser::yytnamerr_ (const char *yystr)
  {
    if (*yystr == '"')
      {
        std::string yyr = "";
        char const *yyp = yystr;

        for (;;)
          switch (*++yyp)
            {
            case '\'':
            case ',':
              goto do_not_strip_quotes;

            case '\\':
              if (*++yyp != '\\')
                goto do_not_strip_quotes;
              /* Fall through.  */
            default:
              yyr += *yyp;
              break;

            case '"':
              return yyr;
            }
      do_not_strip_quotes: ;
      }

    return yystr;
  }


  /// Build a parser object.
  BisonParser::BisonParser (pico::FlexScanner &scanner_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      scanner (scanner_yyarg)
  {
  }

  BisonParser::~BisonParser ()
  {
  }

#if YYDEBUG
  /*--------------------------------.
  | Print this symbol on YYOUTPUT.  |
  `--------------------------------*/

  inline void
  BisonParser::yy_symbol_value_print_ (int yytype,
			   const semantic_type* yyvaluep, const location_type* yylocationp)
  {
    YYUSE (yylocationp);
    YYUSE (yyvaluep);
    std::ostream& yyo = debug_stream ();
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    YYUSE (yytype);
  }


  void
  BisonParser::yy_symbol_print_ (int yytype,
			   const semantic_type* yyvaluep, const location_type* yylocationp)
  {
    *yycdebug_ << (yytype < yyntokens_ ? "token" : "nterm")
	       << ' ' << yytname_[yytype] << " ("
	       << *yylocationp << ": ";
    yy_symbol_value_print_ (yytype, yyvaluep, yylocationp);
    *yycdebug_ << ')';
  }
#endif

  void
  BisonParser::yydestruct_ (const char* yymsg,
			   int yytype, semantic_type* yyvaluep, location_type* yylocationp)
  {
    YYUSE (yylocationp);
    YYUSE (yymsg);
    YYUSE (yyvaluep);

    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

    YYUSE (yytype);
  }

  void
  BisonParser::yypop_ (unsigned int n)
  {
    yystate_stack_.pop (n);
    yysemantic_stack_.pop (n);
    yylocation_stack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  BisonParser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  BisonParser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  BisonParser::debug_level_type
  BisonParser::debug_level () const
  {
    return yydebug_;
  }

  void
  BisonParser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif

  inline bool
  BisonParser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  inline bool
  BisonParser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  BisonParser::parse ()
  {
    /// Lookahead and lookahead in internal form.
    int yychar = yyempty_;
    int yytoken = 0;

    // State.
    int yyn;
    int yylen = 0;
    int yystate = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// Semantic value of the lookahead.
    static semantic_type yyval_default;
    semantic_type yylval = yyval_default;
    /// Location of the lookahead.
    location_type yylloc;
    /// The locations where the error started and ended.
    location_type yyerror_range[3];

    /// $$.
    semantic_type yyval;
    /// @$.
    location_type yyloc;

    int yyresult;

    // FIXME: This shoud be completely indented.  It is not yet to
    // avoid gratuitous conflicts when merging into the master branch.
    try
      {
    YYCDEBUG << "Starting parse" << std::endl;


    /* Initialize the stacks.  The initial state will be pushed in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystate_stack_.clear ();
    yysemantic_stack_.clear ();
    yylocation_stack_.clear ();
    yysemantic_stack_.push (yylval);
    yylocation_stack_.push (yylloc);

    /* New state.  */
  yynewstate:
    yystate_stack_.push (yystate);
    YYCDEBUG << "Entering state " << yystate << std::endl;

    /* Accept?  */
    if (yystate == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    /* Backup.  */
  yybackup:

    /* Try to take a decision without lookahead.  */
    yyn = yypact_[yystate];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    /* Read a lookahead token.  */
    if (yychar == yyempty_)
      {
        YYCDEBUG << "Reading a token: ";
        yychar = yylex (&yylval, scanner);
      }

    /* Convert token to internal form.  */
    if (yychar <= yyeof_)
      {
	yychar = yytoken = yyeof_;
	YYCDEBUG << "Now at end of input." << std::endl;
      }
    else
      {
	yytoken = yytranslate_ (yychar);
	YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
      }

    /* If the proper action on seeing token YYTOKEN is to reduce or to
       detect an error, take that action.  */
    yyn += yytoken;
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yytoken)
      goto yydefault;

    /* Reduce or error.  */
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
	if (yy_table_value_is_error_ (yyn))
	  goto yyerrlab;
	yyn = -yyn;
	goto yyreduce;
      }

    /* Shift the lookahead token.  */
    YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

    /* Discard the token being shifted.  */
    yychar = yyempty_;

    yysemantic_stack_.push (yylval);
    yylocation_stack_.push (yylloc);

    /* Count tokens shifted since error; after three, turn off error
       status.  */
    if (yyerrstatus_)
      --yyerrstatus_;

    yystate = yyn;
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystate];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    /* If YYLEN is nonzero, implement the default value of the action:
       `$$ = $1'.  Otherwise, use the top of the stack.

       Otherwise, the following line sets YYVAL to garbage.
       This behavior is undocumented and Bison
       users should not rely upon it.  */
    if (yylen)
      yyval = yysemantic_stack_[yylen - 1];
    else
      yyval = yysemantic_stack_[0];

    // Compute the default @$.
    {
      slice<location_type, location_stack_type> slice (yylocation_stack_, yylen);
      YYLLOC_DEFAULT (yyloc, slice, yylen);
    }

    // Perform the reduction.
    YY_REDUCE_PRINT (yyn);
    switch (yyn)
      {
          case 2:
/* Line 664 of lalr1.cc  */
#line 73 "src/pico.y"
    { (yyval.expr_list) = (yysemantic_stack_[(1) - (1)].expr_list); }
    break;

  case 3:
/* Line 664 of lalr1.cc  */
#line 75 "src/pico.y"
    { parsed_expressions = (yyval.expr_list) = new ExpressionList(); 
                       (yyval.expr_list)->push_back((yysemantic_stack_[(2) - (1)].expr)); }
    break;

  case 4:
/* Line 664 of lalr1.cc  */
#line 77 "src/pico.y"
    { (yysemantic_stack_[(3) - (1)].expr_list)->push_back((yysemantic_stack_[(3) - (2)].expr)); parsed_expressions = (yyval.expr_list) = (yysemantic_stack_[(3) - (1)].expr_list); }
    break;

  case 6:
/* Line 664 of lalr1.cc  */
#line 81 "src/pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(5) - (1)].strval), (yysemantic_stack_[(5) - (3)].expr), (yysemantic_stack_[(5) - (5)].expr)); }
    break;

  case 7:
/* Line 664 of lalr1.cc  */
#line 82 "src/pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(6) - (2)].expr), (yysemantic_stack_[(6) - (4)].expr), (yysemantic_stack_[(6) - (6)].expr)); }
    break;

  case 11:
/* Line 664 of lalr1.cc  */
#line 91 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("<", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 12:
/* Line 664 of lalr1.cc  */
#line 92 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call(">", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 13:
/* Line 664 of lalr1.cc  */
#line 93 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("<=", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 14:
/* Line 664 of lalr1.cc  */
#line 94 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call(">=", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 15:
/* Line 664 of lalr1.cc  */
#line 95 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("==", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 16:
/* Line 664 of lalr1.cc  */
#line 96 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("!=", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 17:
/* Line 664 of lalr1.cc  */
#line 97 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("&&", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 18:
/* Line 664 of lalr1.cc  */
#line 98 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("||", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 19:
/* Line 664 of lalr1.cc  */
#line 99 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("+", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 20:
/* Line 664 of lalr1.cc  */
#line 100 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("-", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 21:
/* Line 664 of lalr1.cc  */
#line 101 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("*", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 22:
/* Line 664 of lalr1.cc  */
#line 102 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("/", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 23:
/* Line 664 of lalr1.cc  */
#line 103 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("%", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 24:
/* Line 664 of lalr1.cc  */
#line 104 "src/pico.y"
    { (yyval.expr) = Expression::make_binary_sym_call("^", (yysemantic_stack_[(3) - (1)].expr), (yysemantic_stack_[(3) - (3)].expr)); }
    break;

  case 25:
/* Line 664 of lalr1.cc  */
#line 105 "src/pico.y"
    { (yyval.expr) = Expression::make_unary_sym_call("!", (yysemantic_stack_[(2) - (2)].expr)); }
    break;

  case 26:
/* Line 664 of lalr1.cc  */
#line 106 "src/pico.y"
    { (yyval.expr) = Expression::make_unary_sym_call("-", (yysemantic_stack_[(2) - (2)].expr)); }
    break;

  case 27:
/* Line 664 of lalr1.cc  */
#line 110 "src/pico.y"
    { (yyval.expr) = Expression::make_0ary_call((yysemantic_stack_[(1) - (1)].expr));}
    break;

  case 28:
/* Line 664 of lalr1.cc  */
#line 111 "src/pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(4) - (1)].expr), (yysemantic_stack_[(4) - (3)].expr_list)); }
    break;

  case 29:
/* Line 664 of lalr1.cc  */
#line 115 "src/pico.y"
    { (yyval.expr_list) = new ExpressionList(); (yyval.expr_list)->push_back((yysemantic_stack_[(1) - (1)].expr)); }
    break;

  case 30:
/* Line 664 of lalr1.cc  */
#line 116 "src/pico.y"
    { (yysemantic_stack_[(3) - (1)].expr_list)->push_back((yysemantic_stack_[(3) - (3)].expr)); (yyval.expr_list) = (yysemantic_stack_[(3) - (1)].expr_list); }
    break;

  case 32:
/* Line 664 of lalr1.cc  */
#line 121 "src/pico.y"
    { (yyval.expr) = Expression::BLANK_EXPR(); }
    break;

  case 35:
/* Line 664 of lalr1.cc  */
#line 127 "src/pico.y"
    { (yyval.expr) = (yysemantic_stack_[(3) - (2)].expr); }
    break;

  case 36:
/* Line 664 of lalr1.cc  */
#line 130 "src/pico.y"
    { (yyval.expr) = Expression::make_var((yysemantic_stack_[(1) - (1)].strval)); }
    break;

  case 37:
/* Line 664 of lalr1.cc  */
#line 131 "src/pico.y"
    { (yyval.expr) = Expression::make_unbound_var((yysemantic_stack_[(2) - (1)].strval), (yysemantic_stack_[(2) - (2)].strval)); }
    break;

  case 38:
/* Line 664 of lalr1.cc  */
#line 134 "src/pico.y"
    { (yyval.strval) = strdup("Any"); }
    break;

  case 39:
/* Line 664 of lalr1.cc  */
#line 135 "src/pico.y"
    { (yyval.strval) = strdup("Int"); }
    break;

  case 40:
/* Line 664 of lalr1.cc  */
#line 136 "src/pico.y"
    { (yyval.strval) = strdup("Float"); }
    break;

  case 41:
/* Line 664 of lalr1.cc  */
#line 137 "src/pico.y"
    { (yyval.strval) = strdup("String"); }
    break;

  case 42:
/* Line 664 of lalr1.cc  */
#line 138 "src/pico.y"
    { (yyval.strval) = strdup("Array"); }
    break;

  case 43:
/* Line 664 of lalr1.cc  */
#line 139 "src/pico.y"
    { (yyval.strval) = strdup("List"); }
    break;

  case 44:
/* Line 664 of lalr1.cc  */
#line 140 "src/pico.y"
    { (yyval.strval) = strdup("Table"); }
    break;

  case 46:
/* Line 664 of lalr1.cc  */
#line 145 "src/pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(1) - (1)].ival)); }
    break;

  case 47:
/* Line 664 of lalr1.cc  */
#line 146 "src/pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(1) - (1)].fval)); }
    break;

  case 48:
/* Line 664 of lalr1.cc  */
#line 147 "src/pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(1) - (1)].strval)); }
    break;

  case 49:
/* Line 664 of lalr1.cc  */
#line 148 "src/pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(1) - (1)].cval)); }
    break;

  case 50:
/* Line 664 of lalr1.cc  */
#line 149 "src/pico.y"
    { (yyval.expr) = new Expression(true); }
    break;

  case 51:
/* Line 664 of lalr1.cc  */
#line 150 "src/pico.y"
    { (yyval.expr) = new Expression(false); }
    break;


/* Line 664 of lalr1.cc  */
#line 718 "pico.tab.c"
      default:
        break;
      }

    /* User semantic actions sometimes alter yychar, and that requires
       that yytoken be updated with the new translation.  We take the
       approach of translating immediately before every use of yytoken.
       One alternative is translating here after every semantic action,
       but that translation would be missed if the semantic action
       invokes YYABORT, YYACCEPT, or YYERROR immediately after altering
       yychar.  In the case of YYABORT or YYACCEPT, an incorrect
       destructor might then be invoked immediately.  In the case of
       YYERROR, subsequent parser actions might lead to an incorrect
       destructor call or verbose syntax error message before the
       lookahead is translated.  */
    YY_SYMBOL_PRINT ("-> $$ =", yyr1_[yyn], &yyval, &yyloc);

    yypop_ (yylen);
    yylen = 0;
    YY_STACK_PRINT ();

    yysemantic_stack_.push (yyval);
    yylocation_stack_.push (yyloc);

    /* Shift the result of the reduction.  */
    yyn = yyr1_[yyn];
    yystate = yypgoto_[yyn - yyntokens_] + yystate_stack_[0];
    if (0 <= yystate && yystate <= yylast_
	&& yycheck_[yystate] == yystate_stack_[0])
      yystate = yytable_[yystate];
    else
      yystate = yydefgoto_[yyn - yyntokens_];
    goto yynewstate;

  /*------------------------------------.
  | yyerrlab -- here on detecting error |
  `------------------------------------*/
  yyerrlab:
    /* Make sure we have latest lookahead translation.  See comments at
       user semantic actions for why this is necessary.  */
    yytoken = yytranslate_ (yychar);

    /* If not already recovering from an error, report this error.  */
    if (!yyerrstatus_)
      {
	++yynerrs_;
	if (yychar == yyempty_)
	  yytoken = yyempty_;
	error (yylloc, yysyntax_error_ (yystate, yytoken));
      }

    yyerror_range[1] = yylloc;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */
        if (yychar <= yyeof_)
          {
            /* Return failure if at end of input.  */
            if (yychar == yyeof_)
              YYABORT;
          }
        else
          {
            yydestruct_ ("Error: discarding", yytoken, &yylval, &yylloc);
            yychar = yyempty_;
          }
      }

    /* Else will try to reuse lookahead token after shifting the error
       token.  */
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;

    yyerror_range[1] = yylocation_stack_[yylen - 1];
    /* Do not reclaim the symbols of the rule which action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    yystate = yystate_stack_[0];
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;	/* Each real token shifted decrements this.  */

    for (;;)
      {
	yyn = yypact_[yystate];
	if (!yy_pact_value_is_default_ (yyn))
	{
	  yyn += yyterror_;
	  if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
	    {
	      yyn = yytable_[yyn];
	      if (0 < yyn)
		break;
	    }
	}

	/* Pop the current state because it cannot handle the error token.  */
	if (yystate_stack_.height () == 1)
	  YYABORT;

	yyerror_range[1] = yylocation_stack_[0];
	yydestruct_ ("Error: popping",
		     yystos_[yystate],
		     &yysemantic_stack_[0], &yylocation_stack_[0]);
	yypop_ ();
	yystate = yystate_stack_[0];
	YY_STACK_PRINT ();
      }

    yyerror_range[2] = yylloc;
    // Using YYLLOC is tempting, but would change the location of
    // the lookahead.  YYLOC is available though.
    YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
    yysemantic_stack_.push (yylval);
    yylocation_stack_.push (yyloc);

    /* Shift the error token.  */
    YY_SYMBOL_PRINT ("Shifting", yystos_[yyn],
		     &yysemantic_stack_[0], &yylocation_stack_[0]);

    yystate = yyn;
    goto yynewstate;

    /* Accept.  */
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    /* Abort.  */
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (yychar != yyempty_)
      {
        /* Make sure we have latest lookahead translation.  See comments
           at user semantic actions for why this is necessary.  */
        yytoken = yytranslate_ (yychar);
        yydestruct_ ("Cleanup: discarding lookahead", yytoken, &yylval,
                     &yylloc);
      }

    /* Do not reclaim the symbols of the rule which action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystate_stack_.height ())
      {
        yydestruct_ ("Cleanup: popping",
                     yystos_[yystate_stack_[0]],
                     &yysemantic_stack_[0],
                     &yylocation_stack_[0]);
        yypop_ ();
      }

    return yyresult;
    }
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack"
                 << std::endl;
        // Do not try to display the values of the reclaimed symbols,
        // as their printer might throw an exception.
        if (yychar != yyempty_)
          {
            /* Make sure we have latest lookahead translation.  See
               comments at user semantic actions for why this is
               necessary.  */
            yytoken = yytranslate_ (yychar);
            yydestruct_ (YY_NULL, yytoken, &yylval, &yylloc);
          }

        while (1 < yystate_stack_.height ())
          {
            yydestruct_ (YY_NULL,
                         yystos_[yystate_stack_[0]],
                         &yysemantic_stack_[0],
                         &yylocation_stack_[0]);
            yypop_ ();
          }
        throw;
      }
  }

  // Generate an error message.
  std::string
  BisonParser::yysyntax_error_ (int yystate, int yytoken)
  {
    std::string yyres;
    // Number of reported tokens (one for the "unexpected", one per
    // "expected").
    size_t yycount = 0;
    // Its maximum.
    enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
    // Arguments of yyformat.
    char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];

    /* There are many possibilities here to consider:
       - If this state is a consistent state with a default action, then
         the only way this function was invoked is if the default action
         is an error action.  In that case, don't check for expected
         tokens because there are none.
       - The only way there can be no lookahead present (in yytoken) is
         if this state is a consistent state with a default action.
         Thus, detecting the absence of a lookahead is sufficient to
         determine that there is no unexpected or expected token to
         report.  In that case, just report a simple "syntax error".
       - Don't assume there isn't a lookahead just because this state is
         a consistent state with a default action.  There might have
         been a previous inconsistent state, consistent state with a
         non-default action, or user semantic action that manipulated
         yychar.
       - Of course, the expected token list depends on states to have
         correct lookahead information, and it depends on the parser not
         to perform extra reductions after fetching a lookahead from the
         scanner and before detecting a syntax error.  Thus, state
         merging (from LALR or IELR) and default reductions corrupt the
         expected token list.  However, the list is correct for
         canonical LR with one exception: it will still contain any
         token that will not be accepted due to an error action in a
         later state.
    */
    if (yytoken != yyempty_)
      {
        yyarg[yycount++] = yytname_[yytoken];
        int yyn = yypact_[yystate];
        if (!yy_pact_value_is_default_ (yyn))
          {
            /* Start YYX at -YYN if negative to avoid negative indexes in
               YYCHECK.  In other words, skip the first -YYN actions for
               this state because they are default actions.  */
            int yyxbegin = yyn < 0 ? -yyn : 0;
            /* Stay within bounds of both yycheck and yytname.  */
            int yychecklim = yylast_ - yyn + 1;
            int yyxend = yychecklim < yyntokens_ ? yychecklim : yyntokens_;
            for (int yyx = yyxbegin; yyx < yyxend; ++yyx)
              if (yycheck_[yyx + yyn] == yyx && yyx != yyterror_
                  && !yy_table_value_is_error_ (yytable_[yyx + yyn]))
                {
                  if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                    {
                      yycount = 1;
                      break;
                    }
                  else
                    yyarg[yycount++] = yytname_[yyx];
                }
          }
      }

    char const* yyformat = YY_NULL;
    switch (yycount)
      {
#define YYCASE_(N, S)                         \
        case N:                               \
          yyformat = S;                       \
        break
        YYCASE_(0, YY_("syntax error"));
        YYCASE_(1, YY_("syntax error, unexpected %s"));
        YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
        YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
        YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
        YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
      }

    // Argument number.
    size_t yyi = 0;
    for (char const* yyp = yyformat; *yyp; ++yyp)
      if (yyp[0] == '%' && yyp[1] == 's' && yyi < yycount)
        {
          yyres += yytnamerr_ (yyarg[yyi++]);
          ++yyp;
        }
      else
        yyres += *yyp;
    return yyres;
  }


  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
  const signed char BisonParser::yypact_ninf_ = -53;
  const short int
  BisonParser::yypact_[] =
  {
        93,   -53,   -53,   -53,   -53,   -53,   133,   -53,   -53,   133,
     -53,   -53,   -53,   -53,   -53,   -53,   -53,   -53,   133,    93,
      53,   -53,   -37,   193,   -44,   -53,   -53,   -17,   -53,   -28,
     166,   -53,   -53,   -53,   -31,   -53,   -24,   -53,   133,   133,
     133,   133,   133,   133,   133,   133,   133,   133,   133,   133,
     133,   133,   133,   -53,    93,   133,   -53,   -53,   199,   199,
     199,   199,   -40,   -40,   199,   199,   -26,   -26,   -21,   -21,
     -21,   -53,   193,   -43,   -53,   -52,   157,   133,   -53,   -53,
     -53,    93,    93,   -53,   -53,   -53
  };

  /* YYDEFACT[S] -- default reduction number in state S.  Performed when
     YYTABLE doesn't specify something else to do.  Zero means the
     default is an error.  */
  const unsigned char
  BisonParser::yydefact_[] =
  {
         0,    39,    40,    41,    42,    44,     0,    38,    43,     0,
      50,    51,    52,    45,    48,    47,    46,    49,     0,     0,
       0,     2,     0,     5,    10,    27,    34,     0,    33,    36,
       0,    36,    25,    26,     0,     1,     0,     3,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    32,    37,     0,     0,    35,     4,    13,    14,
      15,    16,    18,    17,    11,    12,    19,    20,    21,    22,
      23,    24,    31,     0,    29,     0,     0,    32,    28,     8,
       9,     0,     0,    30,     6,     7
  };

  /* YYPGOTO[NTERM-NUM].  */
  const signed char
  BisonParser::yypgoto_[] =
  {
       -53,   -53,   -53,    -3,   -53,    -5,   -53,   -53,   -47,   -53,
     -53,   -53,   -53,     0
  };

  /* YYDEFGOTO[NTERM-NUM].  */
  const signed char
  BisonParser::yydefgoto_[] =
  {
        -1,    20,    21,    22,    81,    23,    24,    73,    74,    25,
      26,    27,    28,    31
  };

  /* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule which
     number is the opposite.  If YYTABLE_NINF_, syntax error.  */
  const signed char BisonParser::yytable_ninf_ = -1;
  const unsigned char
  BisonParser::yytable_[] =
  {
        29,    30,    79,    80,    32,    46,    47,    48,    49,    50,
      51,    77,    52,    33,    78,    37,    34,    36,    12,    29,
      29,    48,    49,    50,    51,    54,    56,    53,    57,    51,
      83,     0,     0,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,     0,     0,
      76,    75,     0,    35,    29,     0,     0,     1,     2,     0,
       3,     4,     5,     0,     0,     0,     6,     0,     7,     0,
       0,     0,    72,     0,     8,     0,     0,     0,    84,    85,
       0,    29,    29,     9,     0,     0,    10,    11,    12,     0,
      13,    14,    15,    16,    17,     0,     0,     1,     2,    18,
       3,     4,     5,     0,     0,     0,     6,     0,     7,    19,
       0,     0,     0,     0,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     9,     0,     0,    10,    11,    12,     0,
      13,    14,    15,    16,    17,     0,     0,     1,     2,    18,
       3,     4,     5,     0,     0,     0,     0,     0,     7,    19,
       0,     0,     0,     0,     8,     0,     0,     0,     0,     0,
       0,     0,     0,     9,     0,     0,    10,    11,    12,     0,
      13,    14,    15,    16,    17,     0,     0,     0,     0,    18,
      55,    38,    39,    40,    41,    42,    43,     0,    82,    19,
      38,    39,    40,    41,    42,    43,     0,     0,     0,     0,
      44,    45,    46,    47,    48,    49,    50,    51,     0,    44,
      45,    46,    47,    48,    49,    50,    51,    38,    39,    40,
      41,    42,    43,     0,     0,     0,     0,    42,    43,     0,
       0,     0,     0,     0,     0,     0,    44,    45,    46,    47,
      48,    49,    50,    51,    46,    47,    48,    49,    50,    51
  };

  /* YYCHECK.  */
  const signed char
  BisonParser::yycheck_[] =
  {
         0,     6,    54,    55,     9,    45,    46,    47,    48,    49,
      50,    54,    56,    18,    57,    52,    19,    20,    35,    19,
      20,    47,    48,    49,    50,    53,    57,    27,    52,    50,
      77,    -1,    -1,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    54,    -1,     0,    54,    -1,    -1,     4,     5,    -1,
       7,     8,     9,    -1,    -1,    -1,    13,    -1,    15,    -1,
      -1,    -1,    77,    -1,    21,    -1,    -1,    -1,    81,    82,
      -1,    81,    82,    30,    -1,    -1,    33,    34,    35,    -1,
      37,    38,    39,    40,    41,    -1,    -1,     4,     5,    46,
       7,     8,     9,    -1,    -1,    -1,    13,    -1,    15,    56,
      -1,    -1,    -1,    -1,    21,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    30,    -1,    -1,    33,    34,    35,    -1,
      37,    38,    39,    40,    41,    -1,    -1,     4,     5,    46,
       7,     8,     9,    -1,    -1,    -1,    -1,    -1,    15,    56,
      -1,    -1,    -1,    -1,    21,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    30,    -1,    -1,    33,    34,    35,    -1,
      37,    38,    39,    40,    41,    -1,    -1,    -1,    -1,    46,
      14,    24,    25,    26,    27,    28,    29,    -1,    31,    56,
      24,    25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
      43,    44,    45,    46,    47,    48,    49,    50,    -1,    43,
      44,    45,    46,    47,    48,    49,    50,    24,    25,    26,
      27,    28,    29,    -1,    -1,    -1,    -1,    28,    29,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    43,    44,    45,    46,
      47,    48,    49,    50,    45,    46,    47,    48,    49,    50
  };

  /* STOS_[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
  const unsigned char
  BisonParser::yystos_[] =
  {
         0,     4,     5,     7,     8,     9,    13,    15,    21,    30,
      33,    34,    35,    37,    38,    39,    40,    41,    46,    56,
      59,    60,    61,    63,    64,    67,    68,    69,    70,    71,
      63,    71,    63,    63,    61,     0,    61,    52,    24,    25,
      26,    27,    28,    29,    43,    44,    45,    46,    47,    48,
      49,    50,    56,    71,    53,    14,    57,    52,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    65,    66,    61,    63,    54,    57,    54,
      55,    62,    31,    66,    61,    61
  };

#if YYDEBUG
  /* TOKEN_NUMBER_[YYLEX-NUM] -- Internal symbol number corresponding
     to YYLEX-NUM.  */
  const unsigned short int
  BisonParser::yytoken_number_[] =
  {
         0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,    60,    62,    43,    45,    42,    47,    37,
      94,   298,    46,    61,    44,    10,    40,    41
  };
#endif

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
  const unsigned char
  BisonParser::yyr1_[] =
  {
         0,    58,    59,    60,    60,    61,    61,    61,    62,    62,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    64,    64,    65,
      65,    66,    66,    67,    67,    67,    68,    68,    69,    69,
      69,    69,    69,    69,    69,    69,    70,    70,    70,    70,
      70,    70,    71
  };

  /* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
  const unsigned char
  BisonParser::yyr2_[] =
  {
         0,     2,     1,     2,     3,     1,     5,     6,     1,     1,
       1,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     2,     2,     1,     4,     1,
       3,     1,     0,     1,     1,     3,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1
  };


  /* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
     First, the terminals, then, starting at \a yyntokens_, nonterminals.  */
  const char*
  const BisonParser::yytname_[] =
  {
    "$end", "error", "$undefined", "FN", "INT", "FLOAT", "CHAR", "STRING",
  "ARRAY", "TABLE", "ALG", "WHEN", "END", "IF", "THEN", "ANY", "RETURN",
  "IS", "DO", "IO", "IOFN", "LIST", "SL", "SR", "LEQ", "GEQ", "EQ", "NEQ",
  "OR", "AND", "NOT", "ELSE", "BOOL", "TRUE", "FALSE", "ID", "SYMBOL",
  "TYPENAME", "STRING_LITERAL", "FLOAT_LITERAL", "INT_LITERAL",
  "CHAR_LITERAL", "INTEGER", "'<'", "'>'", "'+'", "'-'", "'*'", "'/'",
  "'%'", "'^'", "UNARY", "'.'", "'='", "','", "'\\n'", "'('", "')'",
  "$accept", "pico", "exprs", "expr", "comma_or_newline", "term",
  "invocation", "expr_list", "opt_term", "primary", "var", "type_name",
  "literal", "var_name", YY_NULL
  };

#if YYDEBUG
  /* YYRHS -- A `-1'-separated list of the rules' RHS.  */
  const BisonParser::rhs_number_type
  BisonParser::yyrhs_[] =
  {
        59,     0,    -1,    60,    -1,    61,    52,    -1,    59,    61,
      52,    -1,    63,    -1,    71,    53,    61,    62,    61,    -1,
      13,    63,    14,    63,    31,    61,    -1,    54,    -1,    55,
      -1,    64,    -1,    63,    43,    63,    -1,    63,    44,    63,
      -1,    63,    24,    63,    -1,    63,    25,    63,    -1,    63,
      26,    63,    -1,    63,    27,    63,    -1,    63,    29,    63,
      -1,    63,    28,    63,    -1,    63,    45,    63,    -1,    63,
      46,    63,    -1,    63,    47,    63,    -1,    63,    48,    63,
      -1,    63,    49,    63,    -1,    63,    50,    63,    -1,    30,
      63,    -1,    46,    63,    -1,    67,    -1,    64,    56,    65,
      57,    -1,    66,    -1,    65,    54,    66,    -1,    63,    -1,
      -1,    70,    -1,    68,    -1,    56,    61,    57,    -1,    71,
      -1,    69,    71,    -1,    15,    -1,     4,    -1,     5,    -1,
       7,    -1,     8,    -1,    21,    -1,     9,    -1,    37,    -1,
      40,    -1,    39,    -1,    38,    -1,    41,    -1,    33,    -1,
      34,    -1,    35,    -1
  };

  /* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
     YYRHS.  */
  const unsigned char
  BisonParser::yyprhs_[] =
  {
         0,     0,     3,     5,     8,    12,    14,    20,    27,    29,
      31,    33,    37,    41,    45,    49,    53,    57,    61,    65,
      69,    73,    77,    81,    85,    89,    92,    95,    97,   102,
     104,   108,   110,   111,   113,   115,   119,   121,   124,   126,
     128,   130,   132,   134,   136,   138,   140,   142,   144,   146,
     148,   150,   152
  };

  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
  const unsigned char
  BisonParser::yyrline_[] =
  {
         0,    73,    73,    75,    77,    80,    81,    82,    86,    86,
      90,    91,    92,    93,    94,    95,    96,    97,    98,    99,
     100,   101,   102,   103,   104,   105,   106,   110,   111,   115,
     116,   120,   121,   125,   126,   127,   130,   131,   134,   135,
     136,   137,   138,   139,   140,   141,   145,   146,   147,   148,
     149,   150,   153
  };

  // Print the state stack on the debug stream.
  void
  BisonParser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (state_stack_type::const_iterator i = yystate_stack_.begin ();
	 i != yystate_stack_.end (); ++i)
      *yycdebug_ << ' ' << *i;
    *yycdebug_ << std::endl;
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  BisonParser::yy_reduce_print_ (int yyrule)
  {
    unsigned int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    /* Print the symbols being reduced, and their result.  */
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
	       << " (line " << yylno << "):" << std::endl;
    /* The symbols being reduced.  */
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
		       yyrhs_[yyprhs_[yyrule] + yyi],
		       &(yysemantic_stack_[(yynrhs) - (yyi + 1)]),
		       &(yylocation_stack_[(yynrhs) - (yyi + 1)]));
  }
#endif // YYDEBUG

  /* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
  BisonParser::token_number_type
  BisonParser::yytranslate_ (int t)
  {
    static
    const token_number_type
    translate_table[] =
    {
           0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      55,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    49,     2,     2,
      56,    57,    47,    45,    54,    46,    52,    48,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      43,    53,    44,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    50,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    51
    };
    if ((unsigned int) t <= yyuser_token_number_max_)
      return translate_table[t];
    else
      return yyundef_token_;
  }

  const int BisonParser::yyeof_ = 0;
  const int BisonParser::yylast_ = 249;
  const int BisonParser::yynnts_ = 14;
  const int BisonParser::yyempty_ = -2;
  const int BisonParser::yyfinal_ = 35;
  const int BisonParser::yyterror_ = 1;
  const int BisonParser::yyerrcode_ = 256;
  const int BisonParser::yyntokens_ = 58;

  const unsigned int BisonParser::yyuser_token_number_max_ = 298;
  const BisonParser::token_number_type BisonParser::yyundef_token_ = 2;

/* Line 1135 of lalr1.cc  */
#line 14 "src/pico.y"
} // pico
/* Line 1135 of lalr1.cc  */
#line 1342 "pico.tab.c"
/* Line 1136 of lalr1.cc  */
#line 155 "src/pico.y"


// We have to implement the error function
void pico::BisonParser::error(const pico::BisonParser::location_type &loc, const std::string &msg) {
	std::cerr << "Error: " << msg << ", line " << yylineno << std::endl;
}

// Now that we have the Parser declared, we can declare the Scanner and implement
// the yylex function
#include "../include/picoScanner.h"
static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner) {
	return scanner.yylex(yylval);
}
