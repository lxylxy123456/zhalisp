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
#include "structs.h"
#define NEWTYP(TYPE, ARG) static_cast<std::shared_ptr<TYPE>>(new TYPE(ARG))
%}

id	[A-Za-z0-9\+\-\*\/\_\?\>\<\=\!\.]+

%%

[ \n]	{ /* skip blanks */ }
;.+		{ /* skip comments */ }
{id}	{
			if ((yytext[0] == 't' || yytext[0] == 'T') && yytext[1] == '\0') {
				yylval = Bool::lisp_t;
				return ID;
			}
			if ((yytext[0] == 'n' || yytext[0] == 'N') &&
				(yytext[1] == 'i' || yytext[1] == 'I') &&
				(yytext[2] == 'l' || yytext[2] == 'L') && yytext[3] == '\0') {
				yylval = Nil::lisp_nil;
				return ID;
			}
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

