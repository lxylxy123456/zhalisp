%{
#include "structs.cpp"
%}

id	[A-Za-z0-9\+\-\*\/\_\?\>\<\=\!\.]+

%%

[ \n]	{ /* skip blanks */ }
;.+		{ /* skip comments */ }
{id}	{
			yylval = static_cast<std::shared_ptr<Symbol>>(new Symbol(yytext)); 
			return ID; }
.		{ return yytext[0]; }

%%

int yywrap(void) {
	return 1;
}

