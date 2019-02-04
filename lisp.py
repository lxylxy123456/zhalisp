'''
	https://www.mediawiki.org/wiki/Help:Extension:ParserFunctions##expr
	https://meta.wikimedia.org/wiki/Help:Calculation
	https://meta.wikimedia.org/wiki/Special:ExpandTemplates
'''

from fractions import Fraction
# import operator, math

tokens = (
	'ID', 'NUMBER', 'LPAREN', 'RPAREN', 'DOT', 'QUOTE', 'HASH', 
	)

t_LPAREN = '\('
t_RPAREN = '\)'
t_QUOTE = '\''
t_HASH = '\#'

def t_ID(t) :
	'[A-Za-z0-9\+\-\*\/\_\?\>\<\=\!\.]+'
	if t.value == '.' :
		t.type = 'DOT'
		return t
	# https://www.tutorialspoint.com/lisp/lisp_numbers.htm
	for f in (int, float, Fraction) :
		try :
			t.value = f(t.value)
			t.type = 'NUMBER'
			return t
		except ValueError :
			pass
	return t

t_ignore = " \t\n"

def t_newline(t):
	r'\n+'
	t.lexer.lineno += t.value.count('\n')

def t_error(t):
	print("Illegal character '%s'" % t.value[0])
	t.lexer.skip(1)
	0/0

import ply.lex as lex
lexer = lex.lex(optimize=1)

def p_sexps(p) :
	'sexps : sexps sexp'

def p_sexps_empty(p) :
	'sexps : '

def p_sexp_list(p) :
	'sexp : LPAREN exps RPAREN'

def p_sexp_dot(p) :
	'sexp : LPAREN exps DOT sexp RPAREN'

def p_sexp_quote(p) :
	'sexp : QUOTE sexp'

def p_sexp_function(p) :
	'sexp : HASH QUOTE sexp'
	# https://stackoverflow.com/questions/4873810/what-does-mean-in-lisp

def p_sexp_number(p) :
	'sexp : NUMBER'

def p_sexp_id(p) :
	'sexp : ID'

def p_sexp_complex(p) :
	'sexp : HASH LPAREN NUMBER NUMBER RPAREN'
	# https://www.tutorialspoint.com/lisp/lisp_numbers.htm

def p_exps_recu(p) :
	'exps : sexp exps'

def p_exps_empty(p) :
	'exps : '

def p_error(p) :
	print("Syntax error at '%s'" % p.value)
	0/0

import ply.yacc as yacc
parser = yacc.yacc(debug=False)

evaluate = parser.parse

if __name__ == '__main__' :
	print(parser.parse('(123.67e-5 + e / Pi - 45 (1 2 . (3 4)))'))

