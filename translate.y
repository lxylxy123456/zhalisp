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

#define YYSTYPE std::shared_ptr<Sexp>

#define NEWLIST(CAR, CDR) static_cast<std::shared_ptr<List>>(new List(CAR, CDR))
#define NEWLIST2(CAR1, CAR2) NEWLIST(CAR1, NEWLIST(CAR2, Nil::lisp_nil))
#define NEWTYPE(TYPE, A,B,C) static_cast<std::shared_ptr<TYPE>>(new TYPE(A,B,C))
#define SCSPL static_cast<std::shared_ptr<List>>
#define DPCL std::dynamic_pointer_cast<List>
#define DPCS std::dynamic_pointer_cast<Symbol>
#define DPCN std::dynamic_pointer_cast<Number>
%}

%token ID
%token NUM

%%


sexps	: sexp sexps			{ $$ = NEWLIST($1, DPCL($2)); }
		| 						{ $$ = Nil::lisp_nil; }
		;
sexp	: '(' exps ')'			{ $$ = $2; }
		| '\'' sexp				{ $$ = NEWLIST2(Symbol::lisp_quote, $2); }
		| '#' '\'' sexp			{ $$ = NEWLIST2(Symbol::lisp_function, $3); }
		| NUM					{ $$ = $1; }
		| ID					{ $$ = $1; }
		| '#' ID '(' NUM NUM ')'{ $$ = reduced_complex(DPCN($4), DPCN($5)); 
									assert(DPCS($2)->get_value() == "C");}
		;
exps	: sexp exps				{ $$ = NEWLIST($1, DPCL($2)); }
		| sexp '.' exps			{ $$ = NEWLIST($1, DPCL($3)); }
		|						{ $$ = Nil::lisp_nil; }
		;

%%

#include "lex.yy.c"

SyntaxError::SyntaxError(std::string d): desc(d) {}

int yyerror(const char *msg) {
	throw SyntaxError(msg);
	return printf("%s\n", msg); 
}

std::shared_ptr<List> parse(std::string s) {
	yy_scan_string(s.c_str());
	assert(!yyparse());
	return std::dynamic_pointer_cast<List>(yyval);
}

