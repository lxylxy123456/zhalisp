'''
	builtin functions in lisp
	LIMIT: complex + fraction will have different behavior than Clisp
'''

import functools, operator

from structs import Env, Atom, Number, Symbol, List, Bool
from itertools import repeat

# Helper functions

def eval_params(exps, env) :
	'((+ 1 2) (- 3 4)) -> [3, -1]'
	return tuple(map(evaluate, exps, repeat(env)))

def call_func(func, args, env) :
	# func: a Func object; args: a List
	if type(func) == Symbol :
		func = find_func(func.value, env)
	return func(args, env)

def find_func(name, env) :
	'Find function from builtin and environments'
	if name in functions :
		return functions[name]
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
		return Bool()
	elif value == False :
		return List()
	else :
		raise ValueError('Expect Python bool value, but get %s' % repr(value))

def is_true(value) :
	return not(type(value) == List and value.nil())

def build_list(l) :
	ans = List()
	for i in reversed(l) :
		ans = List(i, ans)
	return ans

# Arithmetics

def plus(exps, env) :
	'(+ 1 2 3)'
	nums = eval_params(exps, env)
	return functools.reduce(operator.add, nums)

def minus(exps, env) :
	'(- 1 2)'
	a, b = eval_params(exps, env)
	return operator.sub(a, b)

def mul(exps, env) :
	'(* 1 2 3)'
	nums = eval_params(exps, env)
	return functools.reduce(operator.mul, nums)

def div(exps, env) :
	'(/ 1 2)'
	a, b = eval_params(exps, env)
	return operator.truediv(a, b)

def lt(exps, env) :
	'(< 1 2)'
	a, b = eval_params(exps, env)
	return to_bool(operator.lt(a, b))

def le(exps, env) :
	'(<= 1 2)'
	a, b = eval_params(exps, env)
	return to_bool(operator.le(a, b))

def gt(exps, env) :
	'(> 1 2)'
	a, b = eval_params(exps, env)
	return to_bool(operator.gt(a, b))

def ge(exps, env) :
	'(>= 1 2)'
	a, b = eval_params(exps, env)
	return to_bool(operator.ge(a, b))

# Predicates

def null(exps, env) :
	'(null NIL) -> T'
	value, = eval_params(exps, env)
	return to_bool(type(value) == List and value.nil())

# List operations

def car(exps, env) :
	"(car '(1 2 3)) -> 1"
	l, = eval_params(exps, env)
	assert type(l) == List
	return l.car

def cdr(exps, env) :
	"(cdr '(1 2 3)) -> (2 3)"
	l, = eval_params(exps, env)
	assert type(l) == List
	return l.cdr

def cons(exps, env) :
	"(cons '1 '(2 3)) -> (1 2 3)"
	a, d = eval_params(exps, env)
	assert type(d) == List
	return List(a, d)

def list_(exps, env) :
	"(list '1 'a) -> (1 A)"
	params = eval_params(exps, env)
	return build_list(params)

# Functions

def defun(exps, env) :
	'(defun f (x) (* x x))'

	def result(exps_, env_) :
		new_env = Env()
		new_envs = env_ + [new_env]
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

def apply(exps, env) :
	"(apply #'+ '(1 2 3))"
	apply_args = eval_params(exps, env)
	func = apply_args[0]
	args = apply_args[1:-1] + tuple(apply_args[-1])
	quoter = lambda x: List(Symbol('quote'), List(x, List()))
	arg_list = build_list(list(map(quoter, args)))
	return call_func(func, arg_list, env)

def funcall(exps, env) :
	"(funcall #'+ 1 2 3)"
	func = evaluate(exps.car, env)
	args = exps.cdr
	return call_func(func, args, env)

def function(exps, env) :
	"(function +) OR #'+"
	name, = exps
	assert type(name) == Symbol
	return find_func(name.value, env)

def quote(exps, env) :
	"(quote (1 2 3)) OR '(1 2 3)"
	assert exps.cdr.nil()
	return exps.car

def eval_(exps, env) :
	'(eval (+ 1 2 3))'
	return evaluate(exps.car, env)

# Variables

def let(exps, env) :
	'(let ((x 10)) (* x 2))'
	new_env = Env()
	new_envs = env + [new_env]
	assert type(exps.car) == List
	for i in exps.car :
		assert type(i) == List
		assert i.cdr.cdr.nil()
		new_env.set_var(i.car.value, evaluate(i.cdr.car, env))
	ans = List()
	for i in exps.cdr :
		ans = evaluate(i, new_envs)
	return ans

def setq(exps, env) :
	'(setq x 10) -> X'
	0/0

# Conditions

def cond(exp, env) :
	'(cond ((> 1 2) 1) ((< 1 2) 2))'
	for test in exp :
		assert type(test) == List
		if is_true(evaluate(test.car, env)) :
			for stmt in test.cdr :
				ans = evaluate(stmt, env)
			return ans
	return List()

# Evaluate

def evaluate(exp, env) :
	'exp is (+ 2 3) OR 2 OR jkl'
	if type(exp) == List :
		assert type(exp.car) == Symbol
		func = find_func(exp.car.value, env)
		return call_func(func, exp.cdr, env)
	elif type(exp) == Number :
		return exp
	elif type(exp) == Symbol :
		return find_var(exp.value, env)
	else :
		assert isinstance(exp, Atom)

# Index

functions = {
	'+': plus, 				# Arithmetics
	'-': minus, 
	'*': mul, 
	'/': div, 
	'<': lt, 
	'<=': le, 
	'>': gt, 
	'>=': ge, 
	'NULL': null, 			# Predicates
	'CAR': car, 			# List operations
	'CDR': cdr, 
	'CONS': cons, 
	'LIST': list_, 
	'DEFUN': defun, 		# Functions
	'APPLY': apply, 
	'FUNCALL': funcall, 
	'FUNCTION': function, 
	'QUOTE': quote, 
	'EVAL': eval_, 
	'LET': let, 			# Variables
	'SETQ': setq, 
	'COND': cond, 			# Conditions
}

