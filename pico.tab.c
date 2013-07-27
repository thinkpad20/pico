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
#line 1 "pico.y"


#include "expression.h"
#define YYDEBUG 1
int yydebug = 1;


/* Line 283 of lalr1.cc  */
#line 46 "pico.tab.c"


#include "pico.tab.h"

/* User implementation prologue.  */

/* Line 289 of lalr1.cc  */
#line 54 "pico.tab.c"
/* Unqualified %code blocks.  */
/* Line 290 of lalr1.cc  */
#line 32 "pico.y"

	// Prototype for the yylex function
	static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner);


/* Line 290 of lalr1.cc  */
#line 64 "pico.tab.c"


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
#line 11 "pico.y"
namespace pico {
/* Line 357 of lalr1.cc  */
#line 160 "pico.tab.c"

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
#line 78 "pico.y"
    { (yyval.expr_list) = (yysemantic_stack_[(1) - (1)].expr_list); (yyval.expr_list)->print(); }
    break;

  case 3:
/* Line 664 of lalr1.cc  */
#line 80 "pico.y"
    { (yyval.expr_list) = new ExpressionList(); (yyval.expr_list)->push_back((yysemantic_stack_[(2) - (1)].expr)); }
    break;

  case 4:
/* Line 664 of lalr1.cc  */
#line 81 "pico.y"
    { (yysemantic_stack_[(3) - (1)].expr_list)->push_back((yysemantic_stack_[(3) - (2)].expr)); (yyval.expr_list) = (yysemantic_stack_[(3) - (1)].expr_list); }
    break;

  case 5:
/* Line 664 of lalr1.cc  */
#line 84 "pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(1) - (1)].term)); }
    break;

  case 6:
/* Line 664 of lalr1.cc  */
#line 85 "pico.y"
    { (yyval.expr) = new Expression(new Var((yysemantic_stack_[(5) - (1)].strval)), (yysemantic_stack_[(5) - (3)].term), (yysemantic_stack_[(5) - (5)].expr)); }
    break;

  case 7:
/* Line 664 of lalr1.cc  */
#line 86 "pico.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(7) - (2)].term), (yysemantic_stack_[(7) - (4)].term), (yysemantic_stack_[(7) - (7)].expr)); }
    break;

  case 9:
/* Line 664 of lalr1.cc  */
#line 91 "pico.y"
    { (yyval.term) = make_lt((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 10:
/* Line 664 of lalr1.cc  */
#line 92 "pico.y"
    { (yyval.term) = make_gt((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 11:
/* Line 664 of lalr1.cc  */
#line 93 "pico.y"
    { (yyval.term) = make_leq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 12:
/* Line 664 of lalr1.cc  */
#line 94 "pico.y"
    { (yyval.term) = make_leq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 13:
/* Line 664 of lalr1.cc  */
#line 95 "pico.y"
    { (yyval.term) = make_eq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 14:
/* Line 664 of lalr1.cc  */
#line 96 "pico.y"
    { (yyval.term) = make_neq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 15:
/* Line 664 of lalr1.cc  */
#line 97 "pico.y"
    { (yyval.term) = make_log_and((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 16:
/* Line 664 of lalr1.cc  */
#line 98 "pico.y"
    { (yyval.term) = make_log_or((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 17:
/* Line 664 of lalr1.cc  */
#line 99 "pico.y"
    { (yyval.term) = make_add((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 18:
/* Line 664 of lalr1.cc  */
#line 100 "pico.y"
    { (yyval.term) = make_sub((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 19:
/* Line 664 of lalr1.cc  */
#line 101 "pico.y"
    { (yyval.term) = make_mult((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 20:
/* Line 664 of lalr1.cc  */
#line 102 "pico.y"
    { (yyval.term) = make_div((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 21:
/* Line 664 of lalr1.cc  */
#line 103 "pico.y"
    { (yyval.term) = make_mod((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 22:
/* Line 664 of lalr1.cc  */
#line 104 "pico.y"
    { (yyval.term) = make_log_not((yysemantic_stack_[(2) - (2)].term)); }
    break;

  case 23:
/* Line 664 of lalr1.cc  */
#line 105 "pico.y"
    { (yyval.term) = make_neg((yysemantic_stack_[(2) - (2)].term)); }
    break;

  case 25:
/* Line 664 of lalr1.cc  */
#line 110 "pico.y"
    { (yyval.term) = new Term((yysemantic_stack_[(4) - (1)].term), (yysemantic_stack_[(4) - (3)].term_list)); }
    break;

  case 26:
/* Line 664 of lalr1.cc  */
#line 114 "pico.y"
    { (yyval.term_list) = new TermList(); (yyval.term_list)->push_back((yysemantic_stack_[(1) - (1)].term)); }
    break;

  case 27:
/* Line 664 of lalr1.cc  */
#line 115 "pico.y"
    { (yysemantic_stack_[(3) - (1)].term_list)->push_back((yysemantic_stack_[(3) - (3)].term)); (yyval.term_list) = (yysemantic_stack_[(3) - (1)].term_list); }
    break;

  case 29:
/* Line 664 of lalr1.cc  */
#line 120 "pico.y"
    { (yyval.term) = new Term(); }
    break;

  case 31:
/* Line 664 of lalr1.cc  */
#line 125 "pico.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].var));}
    break;

  case 32:
/* Line 664 of lalr1.cc  */
#line 126 "pico.y"
    { (yyval.term) = new Term((yysemantic_stack_[(3) - (2)].expr)); }
    break;

  case 33:
/* Line 664 of lalr1.cc  */
#line 129 "pico.y"
    { (yyval.var) = Var::lookup((yysemantic_stack_[(1) - (1)].strval)); }
    break;

  case 34:
/* Line 664 of lalr1.cc  */
#line 130 "pico.y"
    { (yyval.var) = new Var((yysemantic_stack_[(2) - (2)].strval), (yysemantic_stack_[(2) - (1)].strval)); }
    break;

  case 35:
/* Line 664 of lalr1.cc  */
#line 133 "pico.y"
    { (yyval.strval) = new std::string("Any"); }
    break;

  case 36:
/* Line 664 of lalr1.cc  */
#line 134 "pico.y"
    { (yyval.strval) = new std::string("Int"); }
    break;

  case 37:
/* Line 664 of lalr1.cc  */
#line 135 "pico.y"
    { (yyval.strval) = new std::string("Float"); }
    break;

  case 38:
/* Line 664 of lalr1.cc  */
#line 136 "pico.y"
    { (yyval.strval) = new std::string("String"); }
    break;

  case 39:
/* Line 664 of lalr1.cc  */
#line 137 "pico.y"
    { (yyval.strval) = new std::string("Array"); }
    break;

  case 40:
/* Line 664 of lalr1.cc  */
#line 138 "pico.y"
    { (yyval.strval) = new std::string("List"); }
    break;

  case 41:
/* Line 664 of lalr1.cc  */
#line 139 "pico.y"
    { (yyval.strval) = new std::string("Table"); }
    break;

  case 43:
/* Line 664 of lalr1.cc  */
#line 144 "pico.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].ival)); }
    break;

  case 44:
/* Line 664 of lalr1.cc  */
#line 145 "pico.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].fval)); }
    break;

  case 45:
/* Line 664 of lalr1.cc  */
#line 146 "pico.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].strval)); }
    break;

  case 46:
/* Line 664 of lalr1.cc  */
#line 147 "pico.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].cval)); }
    break;

  case 47:
/* Line 664 of lalr1.cc  */
#line 148 "pico.y"
    { (yyval.term) = new Term(true); }
    break;

  case 48:
/* Line 664 of lalr1.cc  */
#line 149 "pico.y"
    { (yyval.term) = new Term(false); }
    break;


/* Line 664 of lalr1.cc  */
#line 714 "pico.tab.c"
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
  const signed char BisonParser::yypact_ninf_ = -62;
  const short int
  BisonParser::yypact_[] =
  {
        71,   -62,   -62,   -62,   -62,   -62,   111,   -62,   -62,   111,
     -62,   -62,   -62,   -62,   -62,   -62,   -62,   -62,   111,    71,
      17,   -62,   -45,   223,   -46,   -62,   -62,   -26,   -62,   -42,
     150,   -62,   -62,   -62,   -44,   -62,   -39,   -62,   151,   151,
     151,   151,   151,   151,   151,   151,   151,   151,   151,   151,
     151,   111,   -62,   111,   111,   -62,   -62,   -46,   -46,   -46,
     -46,   -46,   -46,   -46,   -46,   -46,   -46,   -46,   -46,   -46,
     223,   -48,   -62,   182,   193,   111,   -62,    71,   -16,   -62,
     -62,    71,   -62
  };

  /* YYDEFACT[S] -- default reduction number in state S.  Performed when
     YYTABLE doesn't specify something else to do.  Zero means the
     default is an error.  */
  const unsigned char
  BisonParser::yydefact_[] =
  {
         0,    36,    37,    38,    39,    41,     0,    35,    40,     0,
      47,    48,    49,    42,    45,    44,    43,    46,     0,     0,
       0,     2,     0,     5,     8,    24,    31,     0,    30,    33,
       0,    33,    22,    23,     0,     1,     0,     3,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    29,    34,     0,     0,    32,     4,    11,    12,    13,
      14,    16,    15,     9,    10,    17,    18,    19,    20,    21,
      28,     0,    26,     0,     0,    29,    25,     0,     0,    27,
       6,     0,     7
  };

  /* YYPGOTO[NTERM-NUM].  */
  const short int
  BisonParser::yypgoto_[] =
  {
       -62,   -62,   -62,   -17,    -5,   215,   -62,   -61,   -62,   -62,
     -62,   -62,     0
  };

  /* YYDEFGOTO[NTERM-NUM].  */
  const signed char
  BisonParser::yydefgoto_[] =
  {
        -1,    20,    21,    22,    23,    24,    71,    72,    25,    26,
      27,    28,    31
  };

  /* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule which
     number is the opposite.  If YYTABLE_NINF_, syntax error.  */
  const signed char BisonParser::yytable_ninf_ = -1;
  const unsigned char
  BisonParser::yytable_[] =
  {
        29,    30,    34,    36,    32,    75,    37,    76,    51,    12,
      53,    55,    56,    33,    79,    81,     0,    35,     0,    29,
      29,     1,     2,     0,     3,     4,     5,    52,     0,     0,
       6,     0,     7,     0,     0,     0,     0,     0,     8,     0,
       0,     0,     0,     0,     0,     0,    70,     9,    73,    74,
      10,    11,    12,     0,    13,    14,    15,    16,    17,     0,
      80,     0,     0,    18,    82,     0,     0,     0,     0,     0,
      70,    19,     0,     0,     0,     1,     2,    29,     3,     4,
       5,    29,     0,     0,     6,     0,     7,     0,     0,     0,
       0,     0,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     9,     0,     0,    10,    11,    12,     0,    13,    14,
      15,    16,    17,     0,     0,     1,     2,    18,     3,     4,
       5,     0,     0,     0,     0,    19,     7,     0,     0,     0,
       0,     0,     8,     0,     0,     0,     0,     0,     0,     0,
       0,     9,     0,     0,    10,    11,    12,     0,    13,    14,
      15,    16,    17,     0,     0,     1,     2,    18,     3,     4,
       5,     0,     0,     0,    54,    19,     7,     0,     0,     0,
       0,     0,     8,     0,    38,    39,    40,    41,    42,    43,
       0,     0,     0,     0,    10,    11,    12,     0,    13,    14,
      15,    16,    17,    44,    45,    46,    47,    48,    49,    50,
       0,     0,     0,     0,     0,    19,    38,    39,    40,    41,
      42,    43,     0,     0,     0,     0,     0,    38,    39,    40,
      41,    42,    43,     0,     0,    44,    45,    46,    47,    48,
      49,    50,     0,     0,     0,    77,    44,    45,    46,    47,
      48,    49,    50,     0,     0,     0,    78,    38,    39,    40,
      41,    42,    43,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    44,    45,    46,    47,
      48,    49,    50
  };

  /* YYCHECK.  */
  const signed char
  BisonParser::yycheck_[] =
  {
         0,     6,    19,    20,     9,    53,    51,    55,    54,    35,
      52,    55,    51,    18,    75,    31,    -1,     0,    -1,    19,
      20,     4,     5,    -1,     7,     8,     9,    27,    -1,    -1,
      13,    -1,    15,    -1,    -1,    -1,    -1,    -1,    21,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    30,    53,    54,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,    -1,
      77,    -1,    -1,    46,    81,    -1,    -1,    -1,    -1,    -1,
      75,    54,    -1,    -1,    -1,     4,     5,    77,     7,     8,
       9,    81,    -1,    -1,    13,    -1,    15,    -1,    -1,    -1,
      -1,    -1,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    -1,    -1,     4,     5,    46,     7,     8,
       9,    -1,    -1,    -1,    -1,    54,    15,    -1,    -1,    -1,
      -1,    -1,    21,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    30,    -1,    -1,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    -1,    -1,     4,     5,    46,     7,     8,
       9,    -1,    -1,    -1,    14,    54,    15,    -1,    -1,    -1,
      -1,    -1,    21,    -1,    24,    25,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    43,    44,    45,    46,    47,    48,    49,
      -1,    -1,    -1,    -1,    -1,    54,    24,    25,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    -1,    24,    25,    26,
      27,    28,    29,    -1,    -1,    43,    44,    45,    46,    47,
      48,    49,    -1,    -1,    -1,    53,    43,    44,    45,    46,
      47,    48,    49,    -1,    -1,    -1,    53,    24,    25,    26,
      27,    28,    29,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    43,    44,    45,    46,
      47,    48,    49
  };

  /* STOS_[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
  const unsigned char
  BisonParser::yystos_[] =
  {
         0,     4,     5,     7,     8,     9,    13,    15,    21,    30,
      33,    34,    35,    37,    38,    39,    40,    41,    46,    54,
      57,    58,    59,    60,    61,    64,    65,    66,    67,    68,
      60,    68,    60,    60,    59,     0,    59,    51,    24,    25,
      26,    27,    28,    29,    43,    44,    45,    46,    47,    48,
      49,    54,    68,    52,    14,    55,    51,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      60,    62,    63,    60,    60,    53,    55,    53,    53,    63,
      59,    31,    59
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
     298,    46,    61,    44,    40,    41
  };
#endif

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
  const unsigned char
  BisonParser::yyr1_[] =
  {
         0,    56,    57,    58,    58,    59,    59,    59,    60,    60,
      60,    60,    60,    60,    60,    60,    60,    60,    60,    60,
      60,    60,    60,    60,    61,    61,    62,    62,    63,    63,
      64,    64,    64,    65,    65,    66,    66,    66,    66,    66,
      66,    66,    66,    67,    67,    67,    67,    67,    67,    68
  };

  /* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
  const unsigned char
  BisonParser::yyr2_[] =
  {
         0,     2,     1,     2,     3,     1,     5,     7,     1,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     1,     4,     1,     3,     1,     0,
       1,     1,     3,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1
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
  "'%'", "UNARY", "'.'", "'='", "','", "'('", "')'", "$accept", "pico",
  "exprs", "expr", "term", "invocation", "term_list", "opt_term",
  "primary", "var", "type_name", "literal", "var_name", YY_NULL
  };

#if YYDEBUG
  /* YYRHS -- A `-1'-separated list of the rules' RHS.  */
  const BisonParser::rhs_number_type
  BisonParser::yyrhs_[] =
  {
        57,     0,    -1,    58,    -1,    59,    51,    -1,    57,    59,
      51,    -1,    60,    -1,    68,    52,    60,    53,    59,    -1,
      13,    60,    14,    60,    53,    31,    59,    -1,    61,    -1,
      60,    43,    61,    -1,    60,    44,    61,    -1,    60,    24,
      61,    -1,    60,    25,    61,    -1,    60,    26,    61,    -1,
      60,    27,    61,    -1,    60,    29,    61,    -1,    60,    28,
      61,    -1,    60,    45,    61,    -1,    60,    46,    61,    -1,
      60,    47,    61,    -1,    60,    48,    61,    -1,    60,    49,
      61,    -1,    30,    60,    -1,    46,    60,    -1,    64,    -1,
      61,    54,    62,    55,    -1,    63,    -1,    62,    53,    63,
      -1,    60,    -1,    -1,    67,    -1,    65,    -1,    54,    59,
      55,    -1,    68,    -1,    66,    68,    -1,    15,    -1,     4,
      -1,     5,    -1,     7,    -1,     8,    -1,    21,    -1,     9,
      -1,    37,    -1,    40,    -1,    39,    -1,    38,    -1,    41,
      -1,    33,    -1,    34,    -1,    35,    -1
  };

  /* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
     YYRHS.  */
  const unsigned char
  BisonParser::yyprhs_[] =
  {
         0,     0,     3,     5,     8,    12,    14,    20,    28,    30,
      34,    38,    42,    46,    50,    54,    58,    62,    66,    70,
      74,    78,    82,    85,    88,    90,    95,    97,   101,   103,
     104,   106,   108,   112,   114,   117,   119,   121,   123,   125,
     127,   129,   131,   133,   135,   137,   139,   141,   143,   145
  };

  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
  const unsigned char
  BisonParser::yyrline_[] =
  {
         0,    78,    78,    80,    81,    84,    85,    86,    90,    91,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     102,   103,   104,   105,   109,   110,   114,   115,   119,   120,
     124,   125,   126,   129,   130,   133,   134,   135,   136,   137,
     138,   139,   140,   144,   145,   146,   147,   148,   149,   152
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,    49,     2,     2,
      54,    55,    47,    45,    53,    46,    51,    48,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      43,    52,    44,     2,     2,     2,     2,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    50
    };
    if ((unsigned int) t <= yyuser_token_number_max_)
      return translate_table[t];
    else
      return yyundef_token_;
  }

  const int BisonParser::yyeof_ = 0;
  const int BisonParser::yylast_ = 272;
  const int BisonParser::yynnts_ = 13;
  const int BisonParser::yyempty_ = -2;
  const int BisonParser::yyfinal_ = 35;
  const int BisonParser::yyterror_ = 1;
  const int BisonParser::yyerrcode_ = 256;
  const int BisonParser::yyntokens_ = 56;

  const unsigned int BisonParser::yyuser_token_number_max_ = 298;
  const BisonParser::token_number_type BisonParser::yyundef_token_ = 2;

/* Line 1135 of lalr1.cc  */
#line 11 "pico.y"
} // pico
/* Line 1135 of lalr1.cc  */
#line 1338 "pico.tab.c"
/* Line 1136 of lalr1.cc  */
#line 154 "pico.y"


// We have to implement the error function
void pico::BisonParser::error(const pico::BisonParser::location_type &loc, const std::string &msg) {
	std::cerr << "Error: " << msg << std::endl;
}

// Now that we have the Parser declared, we can declare the Scanner and implement
// the yylex function
#include "picoScanner.h"
static int yylex(pico::BisonParser::semantic_type * yylval, pico::FlexScanner &scanner) {
	return scanner.yylex(yylval);
}
