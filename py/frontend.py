# 
# zhalisp - A "zha" Clisp implementation
# Copyright (C) 2019  lxylxy123456
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
# 

'''
	Read Lisp syntax
	Reference
		https://www.dabeaz.com/ply/ply.html
		https://stackoverflow.com/questions/4873810/what-does-mean-in-lisp
		https://www.tutorialspoint.com/lisp/lisp_numbers.htm
'''

from fractions import Fraction
from structs import Number, Symbol, List, Bool

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
	for f in (int, float, Fraction) :
		try :
			t.value = f(t.value)
			t.type = 'NUMBER'
			return t
		except ValueError :
			pass
	return t

def t_COMMENT(t):
	r'\;.*'

t_ignore = " \t\n"

def t_newline(t):
	r'\n+'
	t.lexer.lineno += t.value.count('\n')

def t_error(t):
	raise SyntaxError("Illegal character '%s'" % t.value[0])
	t.lexer.skip(1)

import ply.lex as lex
lexer = lex.lex(optimize=0)

def p_sexps(p) :
	'sexps : sexps sexp'
	p[0] = p[1].copy()
	p[0].append(p[2])

def p_sexps_empty(p) :
	'sexps : '
	p[0] = []

def p_sexp_list(p) :
	'sexp : LPAREN exps RPAREN'
	p[0] = p[2]

def p_sexp_quote(p) :
	'sexp : QUOTE sexp'
	p[0] = List(Symbol('quote'), List(p[2], List()))

def p_sexp_function(p) :
	'sexp : HASH QUOTE sexp'
	p[0] = List(Symbol('function'), List(p[3], List()))

def p_sexp_number(p) :
	'sexp : NUMBER'
	p[0] = Number(p[1])

def p_sexp_id(p) :
	'sexp : ID'
	if p[1].upper() == 'NIL' :
		p[0] =  List()
	elif p[1].upper() == 'T' :
		p[0] = Bool()
	else :
		p[0] = Symbol(p[1])

def p_sexp_complex(p) :
	'sexp : HASH ID LPAREN NUMBER NUMBER RPAREN'
	assert p[2].upper() == 'C'
	p[0] = Number(complex(p[3].value, p[4].value))

def p_exps_recu(p) :
	'exps : sexp exps'
	p[0] = List(p[1], p[2])

def p_exps_dot(p) :
	'exps : sexp DOT sexp'
	p[0] = List(p[1], p[3])

def p_exps_empty(p) :
	'exps : '
	p[0] = List()

def p_error(p) :
	if p :
		raise SyntaxError("Syntax error at '%s'" % p.value)
	else :
		raise SyntaxError

import ply.yacc as yacc
parser = yacc.yacc(debug=False)

build_tree = parser.parse

