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
	builtin functions in lisp
	LIMIT: complex + fraction will have different behavior than Clisp
	LIMIT: eq have different behavior than Clisp (currently same as eql)
'''

import functools, operator, re, math

from structs import Env, Atom, Number, Symbol, List, Bool, Dot
from itertools import repeat

# Helper functions

T = Bool()

Nil = List()

def eval_params(exps, env) :
	'((+ 1 2) (- 3 4)) -> [3, -1]'
	return map(evaluate, exps, repeat(env))

def call_func(func, args, env) :
	'func: a Func object; args: a List'
	if type(func) == Symbol :
		func = find_func(func.value, env)
	return func(args, env)

def find_func(name, env) :
	'Find function from builtin and environments'
	if name in builtin_functions :
		return builtin_functions[name]
	matched = re.fullmatch('C([AD]{1,4})R', name)
	if matched :
		return caordr(matched.groups()[0])
	for i in reversed(env) :
		if i.has_fun(name) :
			return i.get_fun(name)
	raise NameError('Cannot find function: %s' % name)

def find_var(name, env) :
	'Find variable from environments'
	for i in reversed(env) :
		if i.has_var(name) :
			return i.get_var(name)
	raise NameError('Cannot find variable: %s' % name)

def to_bool(value) :
	if value == True :
		return T
	elif value == False :
		return Nil
	else :
		raise ValueError('Expect Python bool value, but get %s' % repr(value))

def is_true(value) :
	return not(type(value) == List and value.nil())

def build_list(*l) :
	ans = List()	# Cannot be replaced with Nil
	ptr = None
	for i in l :
		for j in i :
			if ptr == None :
				ans = List(j, ans)
				ptr = ans
			else :
				ptr.cdr = List(j, ptr.cdr)
				ptr = ptr.cdr
	return ans

quoter = lambda x: List(Symbol('quote'), List(x, Nil))

arg1 = lambda x: List(quoter(x), Nil)

arg2 = lambda x, y: List(quoter(x), List(quoter(y), Nil))

builtin_functions = {}

def lisp_builtin(symbol) :
	assert type(symbol) == str and symbol == symbol.upper()
	def f(func) :
		builtin_functions[symbol] = func
		return func
	return f

# Arithmetics

@lisp_builtin('+')
def plus(exps, env) :
	'(+ 1 2 3)'
	nums = eval_params(exps, env)
	return functools.reduce(operator.add, nums)

@lisp_builtin('-')
def minus(exps, env) :
	'(- 1 2) OR (- 1)'
	nums = tuple(eval_params(exps, env))
	if len(nums) == 1 :
		a, = nums
		assert type(a) == Number
		return operator.neg(a)
	elif len(nums) == 2 :
		a, b = nums
		assert type(a) == Number and type(b) == Number
		return operator.sub(a, b)
	else :
		raise Exception('Invalid variable numbers for - function')

@lisp_builtin('*')
def mul(exps, env) :
	'(* 1 2 3)'
	nums = eval_params(exps, env)
	return functools.reduce(operator.mul, nums)

@lisp_builtin('/')
def div(exps, env) :
	'(/ 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return operator.truediv(a, b)

@lisp_builtin('1+')
def one_plus(exps, env) :
	'(1+ 2)'
	a, = eval_params(exps, env)
	assert type(a) == Number
	return a + Number(1)

@lisp_builtin('1-')
def one_minus(exps, env) :
	'(1- 2)'
	a, = eval_params(exps, env)
	assert type(a) == Number
	return a - Number(1)

@lisp_builtin('=')
def eq_(exps, env) :
	'(= 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(a.equal(b))

@lisp_builtin('<')
def lt(exps, env) :
	'(< 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.lt(a, b))

@lisp_builtin('<=')
def le(exps, env) :
	'(<= 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.le(a, b))

@lisp_builtin('>')
def gt(exps, env) :
	'(> 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.gt(a, b))

@lisp_builtin('>=')
def ge(exps, env) :
	'(>= 1 2)'
	a, b = eval_params(exps, env)
	assert type(a) == Number and type(b) == Number
	return to_bool(operator.ge(a, b))

@lisp_builtin('SQRT')
def sqrt(exps, env) :
	value, = eval_params(exps, env)
	assert type(value) == Number
	return Number(math.sqrt(value.value))

# Unary Predicates

@lisp_builtin('ATOM')
def atom(exps, env) :
	'(atom NIL) -> T'
	value, = eval_params(exps, env)
	return to_bool(issubclass(type(value), Atom) or 
					type(value) == List and value.nil())

@lisp_builtin('LISTP')
def listp(exps, env) :
	'(listp ()) -> T'
	value, = eval_params(exps, env)
	return to_bool(type(value) == List)

@lisp_builtin('NULL')
def null(exps, env) :
	'(null NIL) -> T'
	value, = eval_params(exps, env)
	return to_bool(type(value) == List and value.nil())

@lisp_builtin('NUMBERP')
def numberp(exps, env) :
	'(numberp 0) -> T; (numberp ()) -> NIL'
	value, = eval_params(exps, env)
	return to_bool(type(value) == Number)

@lisp_builtin('TYPEP')
def typep(exps, env) :
	"(typep 0 'number) -> T"
	val, typ = eval_params(exps, env)
	assert type(typ) == Symbol
	func = {
		'ATOM': atom, 
		'LIST': listp, 
		'NUMBER': numberp, 
		'SYMBOL': symbolp, 
	}[typ.value]
	return func(arg1(val), env)

@lisp_builtin('SYMBOLP')
def symbolp(exps, env) :
	"(symbolp 'a) -> T"
	value, = eval_params(exps, env)
	return to_bool(type(value) == Symbol)

@lisp_builtin('ZEROP')
def zerop(exps, env) :
	'(zerop ()) -> T'
	value, = eval_params(exps, env)
	assert type(value) == Number
	return to_bool(value.value == 0)

@lisp_builtin('EVENP')
def evenp(exps, env) :
	'(evenp 0) -> T'
	value, = eval_params(exps, env)
	assert type(value) == Number and type(value.value) == int
	return to_bool(value.value % 2 == 0)

@lisp_builtin('ODDP')
def oddp(exps, env) :
	'(oddp 0) -> NIL'
	value, = eval_params(exps, env)
	assert type(value) == Number and type(value.value) == int
	return to_bool(value.value % 2 == 1)

# Binary Predicates

@lisp_builtin('EQ')
def eq(exps, env) :
	'(eq 1 2)'
	return eql(exps, env)

@lisp_builtin('EQL')
def eql(exps, env) :
	'(eql 1 2)'
	a, b = eval_params(exps, env)
	ta, tb = type(a), type(b)
	if ta != tb :
		return to_bool(False)
	if ta == Number :
		return to_bool(a.equal(b))
	elif ta == List :
		return to_bool(id(a) == id(b) or a.nil() and b.nil())
	elif ta == Dot :
		return to_bool(True)
	elif ta == Symbol :
		return to_bool(a.value == b.value)
	elif ta == Bool :
		return to_bool(True)
	else :
		raise ValueError('Unexpected type: %s' % repr(ta))

@lisp_builtin('EQUAL')
def equal(exps, env) :
	"(equal '(1) '(1))"
	a, b = eval_params(exps, env)
	ta, tb = type(a), type(b)
	if ta != tb :
		return to_bool(False)
	if type(a) == Number :
		return to_bool(a.equal(b))
	elif type(a) == List :
		if a.nil() :
			return to_bool(b.nil())
		if b.nil() :
			return to_bool(False)
		if not is_true(equal(arg2(a.car, b.car), env)) :
			return to_bool(False)
		return equal(arg2(a.cdr, b.cdr), env)
	elif type(a) == Dot :
		return to_bool(True)
	elif type(a) == Symbol :
		return to_bool(a.value == b.value)
	elif type(a) == Bool :
		return to_bool(True)
	else :
		raise ValueError('Unexpected type: %s' % repr(ta))

# Logic

@lisp_builtin('AND')
def and_(exps, env) :
	'(and 1 2 3)'
	for value in exps :
		if not is_true(evaluate(value, env)) :
			return to_bool(False)
	return to_bool(True)

@lisp_builtin('OR')
def or_(exps, env) :
	'(or 1 2 3)'
	for value in exps :
		if is_true(evaluate(value, env)) :
			return to_bool(True)
	return to_bool(False)

@lisp_builtin('NOT')
def not_(exps, env) :
	'(not 1)'
	value, = eval_params(exps, env)
	return to_bool(not is_true(value))

# List operations

@lisp_builtin('CAR')
def car(exps, env) :
	"(car '(1 2 3)) -> 1"
	l, = eval_params(exps, env)
	assert type(l) == List
	return l.get_car()

@lisp_builtin('CDR')
def cdr(exps, env) :
	"(cdr '(1 2 3)) -> (2 3)"
	l, = eval_params(exps, env)
	assert type(l) == List
	return l.get_cdr()

def caordr(func_name) :
	"(cadddr '(1 2 3 4 5)) -> 4"
	def answer(exps, env) :
		l, = eval_params(exps, env)
		for i in order :
			assert type(l) == List
			if i == 'A' :
				l = l.get_car()
			else :
				l = l.get_cdr()
		return l

	order = list(reversed(func_name))
	return answer

@lisp_builtin('CONS')
def cons(exps, env) :
	"(cons '1 '(2 3)) -> (1 2 3); (cons 1 2) -> (1 . 2)"
	a, d = eval_params(exps, env)
	return List(a, d)

@lisp_builtin('LIST')
def list_(exps, env) :
	"(list '1 'a) -> (1 A)"
	params = eval_params(exps, env)
	return build_list(params)

@lisp_builtin('MEMBER')
def member(exps, env) :
	"(member '1 '(0 1 2)) -> (1 2)"
	a, l = eval_params(exps, env)
	assert type(l) == List
	while not l.nil() :
		if is_true(eql(arg2(a, l.car), env)) :
			return l
		else :
			l = l.cdr
	return Nil

# High-Order Functions

@lisp_builtin('MAPCAR')
def mapcar(exps, env) :
	"(mapcar #'+ '(1 2 3) '(10 20 30))"
	args = eval_params(exps, env)
	func = next(args)
	params = list(args)
	assert all(map(lambda x: type(x) == List, params))
	answer = []
	while not any(map(lambda x: x.nil(), params)) :
		arg = build_list(map(lambda x: quoter(x.car), params))
		answer.append(call_func(func, arg, env))
		params = list(map(lambda x: x.cdr, params))
	return build_list(answer)

@lisp_builtin('MAPC')
def mapc(exps, env) :
	"(mapc #'+ '(1 2 3) '(10 20 30)) -> '(1 2 3)"
	args = eval_params(exps, env)
	func = next(args)
	params = list(args)
	ret = params[0]
	assert all(map(lambda x: type(x) == List, params))
	while not any(map(lambda x: x.nil(), params)) :
		arg = build_list(map(lambda x: quoter(x.car), params))
		call_func(func, arg, env)
		params = list(map(lambda x: x.cdr, params))
	return ret

@lisp_builtin('MAPLIST')
def maplist(exps, env) :
	"(maplist #'cons '(2 3) '(20 30)) -> (((2 3) 20 30) ((3) 30))"
	args = eval_params(exps, env)
	func = next(args)
	params = list(args)
	assert all(map(lambda x: type(x) == List, params))
	answer = []
	while not any(map(lambda x: x.nil(), params)) :
		arg = build_list(map(quoter, params))
		answer.append(call_func(func, arg, env))
		params = list(map(lambda x: x.cdr, params))
	return build_list(answer)

@lisp_builtin('APPEND')
def append(exps, env) :
	"(append '(1 2 3) '(4 5 6) '(7 8 9))"
	return build_list(*eval_params(exps, env))

# Functions

@lisp_builtin('DEFUN')
def defun(exps, env) :
	'(defun f (x) (* x x))'

	def result(exps_, env_) :
		new_env = Env('DEFUN')
		new_envs = env + [new_env]		# determines lexical / static scoping
		actual_args = eval_params(exps_, env_)
		for f, a in zip(formal_args, actual_args) :
			new_env.set_var(f.value, a)
		for stmt in f_stmt :
			ans = evaluate(stmt, new_envs)
		return ans

	f_name = exps.car
	assert type(f_name) == Symbol
	f_args = exps.cdr.car
	f_stmt = exps.cdr.cdr
	formal_args = list(f_args)
	assert all(map(lambda x: type(x) == Symbol, formal_args))
	env[0].set_fun(f_name.value, result)
	return f_name

@lisp_builtin('APPLY')
def apply(exps, env) :
	"(apply #'+ '(1 2 3))"
	apply_args = tuple(eval_params(exps, env))
	func = apply_args[0]
	args = apply_args[1:-1] + tuple(apply_args[-1])
	arg_list = build_list(map(quoter, args))
	return call_func(func, arg_list, env)

@lisp_builtin('FUNCALL')
def funcall(exps, env) :
	"(funcall #'+ 1 2 3)"
	func = evaluate(exps.car, env)
	args = exps.cdr
	return call_func(func, args, env)

@lisp_builtin('FUNCTION')
def function(exps, env) :
	"(function +) OR #'+"
	name, = exps
	assert type(name) == Symbol
	return find_func(name.value, env)

@lisp_builtin('QUOTE')
def quote(exps, env) :
	"(quote (1 2 3)) OR '(1 2 3)"
	assert exps.cdr.nil()
	return exps.car

@lisp_builtin('EVAL')
def eval_(exps, env) :
	'(eval (+ 1 2 3))'
	return evaluate(exps.car, env)

# Variables

@lisp_builtin('LET')
def let(exps, env) :
	'(let ((x 10)) (* x 2))'
	new_env = Env('LET')
	new_envs = env + [new_env]
	assert type(exps.car) == List
	for i in exps.car :
		assert type(i) == List
		assert i.cdr.cdr.nil()
		new_env.set_var(i.car.value, evaluate(i.cdr.car, env))
	ans = Nil
	for i in exps.cdr :
		ans = evaluate(i, new_envs)
	return ans

@lisp_builtin('LET*')
def let_star(exps, env) :
	'(let* ((x 10) (y (1+ x))) (* y 2)) -> 22'
	new_env = Env('LET*')
	new_envs = env + [new_env]
	assert type(exps.car) == List
	for i in exps.car :
		assert type(i) == List
		assert i.cdr.cdr.nil()
		new_env.set_var(i.car.value, evaluate(i.cdr.car, new_envs))
	ans = Nil
	for i in exps.cdr :
		ans = evaluate(i, new_envs)
	return ans

@lisp_builtin('SETQ')
def setq(exps, env) :
	'(setq x 10) -> 10'
	assert exps.cdr.cdr.nil()
	k = exps.car.value
	v = evaluate(exps.cdr.car, env)
	for e in reversed(env) :
		if e.has_var(k) :
			break
	e.set_var(exps.car.value, evaluate(exps.cdr.car, env))
	return v

@lisp_builtin('SET')
def set_(exps, env) :
	"(set 'x 'y) -> y; x -> y"
	k, v = eval_params(exps, env)
	for e in reversed(env) :
		if e.has_var(k.value) :
			break
	e.set_var(k.value, v)
	return v

# Conditions

@lisp_builtin('COND')
def cond(exp, env) :
	'(cond ((> 1 2) 1) ((< 1 2) 2))'
	for test in exp :
		assert type(test) == List
		tests = iter(test)
		ans = evaluate(next(tests), env)
		if is_true(ans) :
			for stmt in tests :
				ans = evaluate(stmt, env)
			return ans
	return Nil

@lisp_builtin('IF')
def if_(exps, env) :
	'(if (> 1 2) 1 2) -> 2'
	test, true, false = exps
	if is_true(evaluate(test, env)) :
		return evaluate(true, env)
	else :
		return evaluate(false, env)

# Iteration

@lisp_builtin('DO')
def do(exps, env) :
	'(do () (t 1)) -> 1'
	new_env = Env('DO')
	new_envs = env + [new_env]
	params = iter(exps)
	var_list = []	# ("var symbol", increment s-exp)
	for i in next(params) :	# initialize variables
		l = tuple(i)
		if len(l) == 2 :
			symbol, init = l
			incr = None
		else :
			symbol, init, incr = l
		assert type(symbol) == Symbol
		new_env.set_var(symbol.value, evaluate(init, env))
		if incr != None :
			var_list.append((symbol.value, incr))
	exit_clause = iter(next(params))
	exit_test = next(exit_clause)
	forms = None
	while True :
		if is_true(evaluate(exit_test, new_envs)) :
			break
		if forms == None :
			forms = list(params)
		for i in forms :
			evaluate(i, new_envs)
		new_values = []
		for s, i in var_list :
			new_values.append((s, evaluate(i, new_envs)))
		for s, i in new_values :
			new_env.set_var(s, i)
	ans = Nil
	for i in exit_clause :
		ans = evaluate(i, new_envs)
	return ans

class ProgInterrupt(Exception) :
	def __init__(self, env, code, value) :
		super().__init__()
		self.env = env
		self.code = code
		self.value = value

# TODO: set_var should accept Symbol as argument

@lisp_builtin('PROG')
def prog(exps, env) :
	'(prog ((x 1)) (return x)) -> 1'
	new_env = Env('PROG')
	new_envs = env + [new_env]
	params = iter(exps)
	for i in next(params) :	# initialize variables
		symbol, init = i
		assert type(symbol) == Symbol
		new_env.set_var(symbol.value, evaluate(init, env))
	sequence = []
	labels = {}
	for i in params :
		if type(i) == List :
			sequence.append(i)
		else :
			labels[i.value] = len(sequence)
	cur = 0
	while cur < len(sequence) :
		try :
			evaluate(sequence[cur], new_envs)
		except ProgInterrupt as e :
			if e.env != new_env :
				raise e
			if e.code == 'return' :
				return e.value
			else :
				assert e.code == 'go'
				cur = labels[e.value.value]
				continue
		cur += 1
	return Nil

@lisp_builtin('GO')
def go(exps, env) :
	'Used by PROG'
	value, = exps
	raise ProgInterrupt(env[-1], 'go', value)

@lisp_builtin('RETURN')
def return_(exps, env) :
	'Used by PROG'
	value, = eval_params(exps, env)
	raise ProgInterrupt(env[-1], 'return', value)

# Evaluate

def evaluate(exp, env) :
	'exp is (+ 2 3) OR 2 OR jkl'
	te = type(exp)
	if te == List :
		if exp.nil() :
			return exp
		assert type(exp.car) == Symbol
		func = find_func(exp.car.value, env)
		return call_func(func, exp.cdr, env)
	elif te == Number :
		return exp
	elif te == Symbol :
		return find_var(exp.value, env)
	elif te == Bool :
		return exp
	else :
		assert isinstance(exp, Atom)
		raise ValueError('Unexpected type: %s' % repr(te))

