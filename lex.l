%{
#include "structs.h"
#define NEWTYP(TYPE, ARG) static_cast<std::shared_ptr<TYPE>>(new TYPE(ARG))
%}

id	[A-Za-z0-9\+\-\*\/\_\?\>\<\=\!\.]+

%%

[ \n]	{ /* skip blanks */ }
;.+		{ /* skip comments */ }
{id}	{
			try {
				yylval = NEWTYP(Bool, yytext);
				return ID;
			} catch (std::invalid_argument& e) {}
			try {
				mpz_class z;
				z = yytext;
				yylval = NEWTYP(Integer, z);
				return NUM;
			} catch (std::invalid_argument& e) {}
			try {
				mpf_class f;
				f = yytext;
				yylval = NEWTYP(Float, f);
				return NUM;
			} catch (std::invalid_argument& e) {}
			try {
				mpq_class q;
				q = yytext;
				q.canonicalize();
				yylval = reduced_rational(q);
				return NUM;
			} catch (std::invalid_argument& e) {}
			yylval = NEWTYP(Symbol, yytext); 
			return ID; 
		}
.		{ return yytext[0]; }

%%

int yywrap(void) {
	return 1;
}

