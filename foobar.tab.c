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
#line 1 "foobar.y"


#include "expression.h"
#define YYDEBUG 1
int yydebug = 1;


/* Line 283 of lalr1.cc  */
#line 46 "foobar.tab.c"


#include "foobar.tab.h"

/* User implementation prologue.  */

/* Line 289 of lalr1.cc  */
#line 54 "foobar.tab.c"
/* Unqualified %code blocks.  */
/* Line 290 of lalr1.cc  */
#line 32 "foobar.y"

	// Prototype for the yylex function
	static int yylex(foobar::BisonParser::semantic_type * yylval, foobar::FlexScanner &scanner);


/* Line 290 of lalr1.cc  */
#line 64 "foobar.tab.c"


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
#line 11 "foobar.y"
namespace foobar {
/* Line 357 of lalr1.cc  */
#line 160 "foobar.tab.c"

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
  BisonParser::BisonParser (foobar::FlexScanner &scanner_yyarg)
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
          case 4:
/* Line 664 of lalr1.cc  */
#line 83 "foobar.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(2) - (1)].term)); }
    break;

  case 5:
/* Line 664 of lalr1.cc  */
#line 84 "foobar.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(2) - (1)].assign), (yysemantic_stack_[(2) - (2)].expr)); }
    break;

  case 6:
/* Line 664 of lalr1.cc  */
#line 85 "foobar.y"
    { (yyval.expr) = new Expression((yysemantic_stack_[(2) - (1)].if_s), (yysemantic_stack_[(2) - (2)].expr)); }
    break;

  case 8:
/* Line 664 of lalr1.cc  */
#line 90 "foobar.y"
    { (yyval.term) = make_log_or((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 10:
/* Line 664 of lalr1.cc  */
#line 95 "foobar.y"
    { (yyval.term) = make_log_and((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); }
    break;

  case 12:
/* Line 664 of lalr1.cc  */
#line 101 "foobar.y"
    {
         switch ((yysemantic_stack_[(3) - (2)].ival)) {
            case 0: (yyval.term) = make_eq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); break;
            case 1: (yyval.term) = make_lt((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); break;
            case 2: (yyval.term) = make_gt((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); break;
            case 3: (yyval.term) = make_leq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); break;
            case 4: (yyval.term) = make_neq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); break;
            case 5: (yyval.term) = make_geq((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); break;
         }
      }
    break;

  case 13:
/* Line 664 of lalr1.cc  */
#line 113 "foobar.y"
    {(yyval.ival) = 0;}
    break;

  case 14:
/* Line 664 of lalr1.cc  */
#line 113 "foobar.y"
    {(yyval.ival) = 1;}
    break;

  case 15:
/* Line 664 of lalr1.cc  */
#line 113 "foobar.y"
    {(yyval.ival) = 2;}
    break;

  case 16:
/* Line 664 of lalr1.cc  */
#line 114 "foobar.y"
    {(yyval.ival) = 3;}
    break;

  case 17:
/* Line 664 of lalr1.cc  */
#line 114 "foobar.y"
    {(yyval.ival) = 4;}
    break;

  case 18:
/* Line 664 of lalr1.cc  */
#line 114 "foobar.y"
    {(yyval.ival) = 5;}
    break;

  case 20:
/* Line 664 of lalr1.cc  */
#line 119 "foobar.y"
    { (yyval.term) = make_add((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); printf("Found an addition\n"); }
    break;

  case 21:
/* Line 664 of lalr1.cc  */
#line 120 "foobar.y"
    { (yyval.term) = make_sub((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); printf("Found a subtraction\n"); }
    break;

  case 23:
/* Line 664 of lalr1.cc  */
#line 125 "foobar.y"
    { (yyval.term) = make_mult((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); printf("Found a multiplication\n"); }
    break;

  case 24:
/* Line 664 of lalr1.cc  */
#line 126 "foobar.y"
    { (yyval.term) = make_div((yysemantic_stack_[(3) - (1)].term), (yysemantic_stack_[(3) - (3)].term)); printf("Found a division\n"); }
    break;

  case 26:
/* Line 664 of lalr1.cc  */
#line 131 "foobar.y"
    { (yyval.term) = make_neg((yysemantic_stack_[(2) - (2)].term)); }
    break;

  case 27:
/* Line 664 of lalr1.cc  */
#line 132 "foobar.y"
    { (yyval.term) = make_log_not((yysemantic_stack_[(2) - (2)].term)); }
    break;

  case 29:
/* Line 664 of lalr1.cc  */
#line 138 "foobar.y"
    { 
         (yyval.term) = new Term(new Invoke((yysemantic_stack_[(4) - (1)].term), (yysemantic_stack_[(4) - (3)].expr))); 
      }
    break;

  case 31:
/* Line 664 of lalr1.cc  */
#line 145 "foobar.y"
    { (yysemantic_stack_[(3) - (1)].expr)->append((yysemantic_stack_[(3) - (3)].expr)); (yyval.expr) = (yysemantic_stack_[(3) - (1)].expr); }
    break;

  case 33:
/* Line 664 of lalr1.cc  */
#line 150 "foobar.y"
    {(yyval.expr) = new Expression(); }
    break;

  case 35:
/* Line 664 of lalr1.cc  */
#line 155 "foobar.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].var)); }
    break;

  case 36:
/* Line 664 of lalr1.cc  */
#line 156 "foobar.y"
    { (yyval.term) = make_parens((yysemantic_stack_[(3) - (2)].expr)); }
    break;

  case 37:
/* Line 664 of lalr1.cc  */
#line 157 "foobar.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].var)); }
    break;

  case 38:
/* Line 664 of lalr1.cc  */
#line 160 "foobar.y"
    { (yyval.var) = (yysemantic_stack_[(2) - (2)].var); }
    break;

  case 46:
/* Line 664 of lalr1.cc  */
#line 165 "foobar.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].ival)); }
    break;

  case 47:
/* Line 664 of lalr1.cc  */
#line 166 "foobar.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].fval)); }
    break;

  case 48:
/* Line 664 of lalr1.cc  */
#line 167 "foobar.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].strval)); }
    break;

  case 49:
/* Line 664 of lalr1.cc  */
#line 168 "foobar.y"
    { (yyval.term) = new Term((yysemantic_stack_[(1) - (1)].cval)); }
    break;

  case 50:
/* Line 664 of lalr1.cc  */
#line 171 "foobar.y"
    { (yyval.var) = new Var((yysemantic_stack_[(1) - (1)].strval)); printf("saw var %s\n", (yysemantic_stack_[(1) - (1)].strval)->c_str()); }
    break;

  case 51:
/* Line 664 of lalr1.cc  */
#line 175 "foobar.y"
    { 
         (yyval.assign) = new Assign((yysemantic_stack_[(3) - (1)].var), (yysemantic_stack_[(3) - (3)].expr));
         (yyval.assign)->print();
      }
    break;

  case 52:
/* Line 664 of lalr1.cc  */
#line 181 "foobar.y"
    { (yyval.if_s) = new If((yysemantic_stack_[(5) - (2)].term), (yysemantic_stack_[(5) - (4)].expr)); }
    break;


/* Line 664 of lalr1.cc  */
#line 668 "foobar.tab.c"
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
  const signed char BisonParser::yypact_ninf_ = -47;
  const signed char
  BisonParser::yypact_[] =
  {
        80,   -47,   -47,   -47,   -47,   -47,   104,   -47,   104,   -47,
     -47,   -47,   -47,   -47,   -47,   104,    80,    42,   -47,   -27,
      -9,   -20,   -47,   -37,   -30,   -46,   -47,   -47,    -8,   -47,
     -21,    80,    80,    -6,   -47,   -47,   -47,   -18,   -47,   -47,
     104,   -47,   104,   -47,   -47,   -47,   -47,   -47,   -47,   104,
     104,   104,   104,   104,    80,   -47,    80,   -47,   -47,    80,
     -47,    -9,   -20,   -47,   -47,   -47,   -47,   -47,   -47,   -31,
     -47,   -47,     8,   -47,    80,   -47,   -47
  };

  /* YYDEFACT[S] -- default reduction number in state S.  Performed when
     YYTABLE doesn't specify something else to do.  Zero means the
     default is an error.  */
  const unsigned char
  BisonParser::yydefact_[] =
  {
         0,    39,    40,    41,    42,    44,     0,    43,     0,    50,
      45,    48,    47,    46,    49,     0,     0,     0,     2,     0,
       7,     9,    11,    19,    22,    25,    28,    37,     0,    34,
      35,     0,     0,     0,    35,    27,    26,     0,     1,     3,
       0,     4,     0,    16,    18,    13,    17,    14,    15,     0,
       0,     0,     0,     0,    33,    38,     0,     5,     6,     0,
      36,     8,    10,    12,    20,    21,    23,    24,    32,     0,
      30,    51,     0,    29,    33,    52,    31
  };

  /* YYPGOTO[NTERM-NUM].  */
  const signed char
  BisonParser::yypgoto_[] =
  {
       -47,   -47,     2,    34,     1,     3,   -47,   -38,   -17,    -5,
     -47,   -47,   -26,   -47,   -47,   -47,   -47,     0,   -47,   -47
  };

  /* YYDEFGOTO[NTERM-NUM].  */
  const signed char
  BisonParser::yydefgoto_[] =
  {
        -1,    17,    68,    19,    20,    21,    49,    22,    23,    24,
      25,    69,    70,    26,    27,    28,    29,    34,    31,    32
  };

  /* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule which
     number is the opposite.  If YYTABLE_NINF_, syntax error.  */
  const signed char BisonParser::yytable_ninf_ = -1;
  const unsigned char
  BisonParser::yytable_[] =
  {
        30,    40,    18,    35,    43,    44,    45,    46,    59,    54,
      36,    63,    64,    65,    50,    51,    30,    30,    37,    39,
      42,    41,    40,    52,    53,    73,    74,     9,    55,    47,
      48,    30,    30,    57,    58,    66,    67,    56,    60,    75,
      33,    61,    38,     0,     0,    62,     1,     2,    76,     3,
       4,     5,     0,     0,    30,     6,    30,     0,    71,    30,
       0,    72,     0,     7,     0,     0,     0,     0,     0,     0,
       0,     0,     8,     0,    30,     0,     0,     9,     0,    10,
      11,    12,    13,    14,     1,     2,     0,     3,     4,     5,
       0,     0,     0,     6,    15,     0,     0,    16,     0,     0,
       0,     7,     0,     0,     0,     0,     0,     0,     1,     2,
       8,     3,     4,     5,     0,     9,     0,    10,    11,    12,
      13,    14,     0,     0,     0,     7,     0,     0,     0,     0,
       0,     0,    15,     0,     8,    16,     0,     0,     0,     9,
       0,    10,    11,    12,    13,    14,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    15,     0,     0,    16
  };

  /* YYCHECK.  */
  const signed char
  BisonParser::yycheck_[] =
  {
         0,    28,     0,     8,    24,    25,    26,    27,    14,    55,
      15,    49,    50,    51,    51,    52,    16,    17,    16,    17,
      29,    48,    28,    53,    54,    56,    57,    35,    28,    49,
      50,    31,    32,    31,    32,    52,    53,    58,    56,    31,
       6,    40,     0,    -1,    -1,    42,     4,     5,    74,     7,
       8,     9,    -1,    -1,    54,    13,    56,    -1,    56,    59,
      -1,    59,    -1,    21,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    30,    -1,    74,    -1,    -1,    35,    -1,    37,
      38,    39,    40,    41,     4,     5,    -1,     7,     8,     9,
      -1,    -1,    -1,    13,    52,    -1,    -1,    55,    -1,    -1,
      -1,    21,    -1,    -1,    -1,    -1,    -1,    -1,     4,     5,
      30,     7,     8,     9,    -1,    35,    -1,    37,    38,    39,
      40,    41,    -1,    -1,    -1,    21,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    -1,    30,    55,    -1,    -1,    -1,    35,
      -1,    37,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,    -1,    55
  };

  /* STOS_[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
  const unsigned char
  BisonParser::yystos_[] =
  {
         0,     4,     5,     7,     8,     9,    13,    21,    30,    35,
      37,    38,    39,    40,    41,    52,    55,    60,    61,    62,
      63,    64,    66,    67,    68,    69,    72,    73,    74,    75,
      76,    77,    78,    62,    76,    68,    68,    61,     0,    61,
      28,    48,    29,    24,    25,    26,    27,    49,    50,    65,
      51,    52,    53,    54,    55,    76,    58,    61,    61,    14,
      56,    63,    64,    66,    66,    66,    67,    67,    61,    70,
      71,    61,    61,    56,    57,    31,    71
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
     295,   296,   297,   298,   299,   300,   301,   302,    46,    60,
      62,    43,    45,    42,    47,    40,    41,    44,    61
  };
#endif

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
  const unsigned char
  BisonParser::yyr1_[] =
  {
         0,    59,    60,    60,    61,    61,    61,    62,    62,    63,
      63,    64,    64,    65,    65,    65,    65,    65,    65,    66,
      66,    66,    67,    67,    67,    68,    68,    68,    69,    69,
      70,    70,    71,    71,    72,    72,    72,    72,    73,    74,
      74,    74,    74,    74,    74,    74,    75,    75,    75,    75,
      76,    77,    78
  };

  /* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
  const unsigned char
  BisonParser::yyr2_[] =
  {
         0,     2,     1,     2,     2,     2,     2,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     1,     3,     3,     1,     2,     2,     1,     4,
       1,     3,     1,     0,     1,     1,     3,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     5
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
  "CHAR_LITERAL", "INTEGER", "COMP", "LOGICAL", "ADDITIVE", "MULTI",
  "UNARY", "'.'", "'<'", "'>'", "'+'", "'-'", "'*'", "'/'", "'('", "')'",
  "','", "'='", "$accept", "exprs", "expr", "term", "and_term",
  "comp_term", "comp", "add_term", "mult_term", "unary_term", "invocation",
  "expr_list", "opt_expr", "primary", "undef_var", "typename", "literal",
  "var", "assign", "if", YY_NULL
  };

#if YYDEBUG
  /* YYRHS -- A `-1'-separated list of the rules' RHS.  */
  const BisonParser::rhs_number_type
  BisonParser::yyrhs_[] =
  {
        60,     0,    -1,    61,    -1,    60,    61,    -1,    62,    48,
      -1,    77,    61,    -1,    78,    61,    -1,    63,    -1,    62,
      28,    63,    -1,    64,    -1,    63,    29,    64,    -1,    66,
      -1,    64,    65,    66,    -1,    26,    -1,    49,    -1,    50,
      -1,    24,    -1,    27,    -1,    25,    -1,    67,    -1,    67,
      51,    66,    -1,    67,    52,    66,    -1,    68,    -1,    68,
      53,    67,    -1,    68,    54,    67,    -1,    69,    -1,    52,
      68,    -1,    30,    68,    -1,    72,    -1,    69,    55,    70,
      56,    -1,    71,    -1,    70,    57,    71,    -1,    61,    -1,
      -1,    75,    -1,    76,    -1,    55,    61,    56,    -1,    73,
      -1,    74,    76,    -1,     4,    -1,     5,    -1,     7,    -1,
       8,    -1,    21,    -1,     9,    -1,    37,    -1,    40,    -1,
      39,    -1,    38,    -1,    41,    -1,    35,    -1,    76,    58,
      61,    -1,    13,    62,    14,    61,    31,    -1
  };

  /* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
     YYRHS.  */
  const unsigned char
  BisonParser::yyprhs_[] =
  {
         0,     0,     3,     5,     8,    11,    14,    17,    19,    23,
      25,    29,    31,    35,    37,    39,    41,    43,    45,    47,
      49,    53,    57,    59,    63,    67,    69,    72,    75,    77,
      82,    84,    88,    90,    91,    93,    95,    99,   101,   104,
     106,   108,   110,   112,   114,   116,   118,   120,   122,   124,
     126,   128,   132
  };

  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
  const unsigned char
  BisonParser::yyrline_[] =
  {
         0,    80,    80,    80,    83,    84,    85,    89,    90,    94,
      95,    99,   100,   113,   113,   113,   114,   114,   114,   118,
     119,   120,   124,   125,   126,   130,   131,   132,   136,   137,
     144,   145,   149,   150,   154,   155,   156,   157,   160,   162,
     162,   162,   162,   162,   162,   162,   165,   166,   167,   168,
     171,   174,   181
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
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      55,    56,    53,    51,    57,    52,    48,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      49,    58,    50,     2,     2,     2,     2,     2,     2,     2,
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
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47
    };
    if ((unsigned int) t <= yyuser_token_number_max_)
      return translate_table[t];
    else
      return yyundef_token_;
  }

  const int BisonParser::yyeof_ = 0;
  const int BisonParser::yylast_ = 159;
  const int BisonParser::yynnts_ = 20;
  const int BisonParser::yyempty_ = -2;
  const int BisonParser::yyfinal_ = 38;
  const int BisonParser::yyterror_ = 1;
  const int BisonParser::yyerrcode_ = 256;
  const int BisonParser::yyntokens_ = 59;

  const unsigned int BisonParser::yyuser_token_number_max_ = 302;
  const BisonParser::token_number_type BisonParser::yyundef_token_ = 2;

/* Line 1135 of lalr1.cc  */
#line 11 "foobar.y"
} // foobar
/* Line 1135 of lalr1.cc  */
#line 1271 "foobar.tab.c"
/* Line 1136 of lalr1.cc  */
#line 183 "foobar.y"


// We have to implement the error function
void foobar::BisonParser::error(const foobar::BisonParser::location_type &loc, const std::string &msg) {
	std::cerr << "Error: " << msg << std::endl;
}

// Now that we have the Parser declared, we can declare the Scanner and implement
// the yylex function
#include "foobarScanner.h"
static int yylex(foobar::BisonParser::semantic_type * yylval, foobar::FlexScanner &scanner) {
	return scanner.yylex(yylval);
}
