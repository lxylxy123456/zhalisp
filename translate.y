%{
// 
// zhalisp - A "zha" Clisp implementation
// Copyright (C) 2019  lxylxy123456
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// 
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.
// 
#include <ctype.h>
#include <stdio.h>

#include "translate.h"

#define YYSTYPE Sexp*

#define NEWLST(CAR, CDR) new List(CAR, CDR)
#define NEWLST2(CAR1, CAR2) NEWLST(CAR1, PTR<List>(NEWLST(CAR2, Nil::lisp_nil)))
#define DCL dynamic_cast<List*>
#define DCS dynamic_cast<Symbol*>
#define DCN dynamic_cast<Number*>
#define PTRS PTR<Sexp>
#define PTRL PTR<List>
#define PTRN PTR<Number>
%}

%token ID
%token NUM

%%

sexps:sexp sexps			{ $$ = NEWLST(PTRS($1), PTRL(DCL($2))); }
	 |						{ $$ = new Nil; }
	 ;
sexp :'(' exps ')'			{ $$ = $2; }
	 |'\'' sexp				{ $$ = NEWLST2(Symbol::lisp_quote, PTRS($2)); }
	 |'#' '\'' sexp			{ $$ = NEWLST2(Symbol::lisp_function, PTRS($3)); }
	 |NUM					{ $$ = $1; }
	 |ID					{ $$ = $1; }
	 |'#' ID '(' NUM NUM ')'{ $$ = reduced_complex(DCN($4), PTRN(DCN($5))); 
									assert(DCS($2)->get_value() == "C");  }
	 ;
exps :sexp exps				{ $$ = NEWLST(PTRS($1), PTRL(DCL($2))); }
	 |sexp '.' sexp			{ $$ = NEWLST(PTRS($1), PTRL(DCL($3))); }
	 |						{ $$ = new Nil; }
	 ;

%%

#include "lex.yy.c"

// TODO: write a function to create PTR, in order to replace use of new Bool and new Nil. This function checks the pointer value and decides whether to use existing PTR object. 

SyntaxError::SyntaxError(std::string d): desc(d) {}

int yyerror(const char *msg) {
	throw SyntaxError(msg);
	return printf("%s\n", msg); 
}

std::shared_ptr<List> parse(std::string s) {
	yy_scan_string(s.c_str());
	assert(!yyparse());
	return std::shared_ptr<List>(dynamic_cast<List*>(yyval));
}

