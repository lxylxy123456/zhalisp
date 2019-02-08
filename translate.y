%{
#include <ctype.h>
#include <stdio.h>

#include "structs.cpp"
#include <memory>
#define YYSTYPE std::shared_ptr<Sexp>

#define NEWLIST(CAR, CDR) static_cast<std::shared_ptr<List>>(new List(CAR, CDR))
#define NEWLIST2(CAR1, CAR2) NEWLIST(CAR1, NEWLIST(CAR2, Nil::lisp_nil))
#define SCSPL static_cast<std::shared_ptr<List>>
#define DPCL std::dynamic_pointer_cast<List>
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
		| '#''c''(' NUM NUM ')'	{ printf("NOT IMPLEMENTED\n"); $$ = nullptr; }
		;
exps	: sexp exps				{ $$ = NEWLIST($1, DPCL($2)); }
		| sexp '.' exps			{ $$ = NEWLIST($1, DPCL($3)); }
		|						{ $$ = Nil::lisp_nil; }
		;

%%

#include "lex.yy.c"
#include <iostream>

int yyerror(const char *msg) {
	throw SyntaxError(msg);
	return printf("%s\n", msg); 
}

std::shared_ptr<Sexp> parse(std::string s) {
	yy_scan_string(s.c_str());
	assert(!yyparse());
	return yyval;
}

int main() {
	yy_scan_string("(1 2 3 a 1/2 3.4) (4 a b) jkl");
	assert(!yyparse());
	std::cout << (yyval->str()) << std::endl;
}

// lex lex.l && yacc translate.y && gcc y.tab.c && ./a.out

